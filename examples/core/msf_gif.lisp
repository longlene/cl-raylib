;;;; msf_gif - GIF encoder (version 2.2)
;;;;
;;;; HOW TO USE:
;;;;
;;;;     (load (merge-pathnames "msf_gif.lisp" *load-truename*))
;;;;
;;;; USAGE EXAMPLE:
;;;;
;;;;     (let ((width 480) (height 320) (centiseconds-per-frame 5) (bit-depth 16)
;;;;           (gif-state (make-msf-gif-state)))
;;;;       ;; (setf *msf-gif-bgra-flag* t)        ; optionally, set this flag if your pixels are in BGRA format instead of RGBA
;;;;       ;; (setf *msf-gif-alpha-threshold* 128) ; optionally, enable transparency (see function documentation below for details)
;;;;       (msf-gif-begin gif-state width height)
;;;;       (msf-gif-frame gif-state ... centiseconds-per-frame bit-depth (* width 4)) ; frame 1
;;;;       (msf-gif-frame gif-state ... centiseconds-per-frame bit-depth (* width 4)) ; frame 2
;;;;       (msf-gif-frame gif-state ... centiseconds-per-frame bit-depth (* width 4)) ; frame 3, etc...
;;;;       (let ((result (msf-gif-end gif-state)))
;;;;         (when (msf-gif-result-data result)
;;;;           (with-open-file (fp "MyGif.gif" :direction :output :element-type '(unsigned-byte 8))
;;;;             (write-sequence (msf-gif-result-data result) fp :end (msf-gif-result-data-size result))))
;;;;         (msf-gif-free result)))
;;;;
;;;; ERROR HANDLING:
;;;;
;;;;     If one function call fails, the library will free all of its allocations,
;;;;     and all subsequent calls will safely no-op and return 0 until the next call to `msf-gif-begin`.
;;;;     Therefore, it's safe to check only the return value of `msf-gif-end`.
;;;;
;;;; NOTE: Memory is managed by the Lisp GC, the C MSF_GIF_MALLOC/REALLOC/FREE macros and
;;;; customAllocatorContext have no equivalent; msf-gif-free is kept for API symmetry
;;;;
;;;; LICENSE: MIT License or Public Domain (www.unlicense.org), choose whichever you prefer
;;;;
;;;; Copyright (c) 2021 Miles Fogle
;;;;
;;;; Common Lisp port of raylib/examples/core/msf_gif.h

(require :cl-raylib)

(defpackage #:msf-gif
  (:use #:cl)
  (:export #:msf-gif-state #:make-msf-gif-state
           #:msf-gif-result #:make-msf-gif-result #:msf-gif-result-data #:msf-gif-result-data-size
           #:msf-gif-begin #:msf-gif-frame #:msf-gif-end #:msf-gif-free
           #:*msf-gif-alpha-threshold* #:*msf-gif-bgra-flag*
           #:msf-gif-begin-to-file #:msf-gif-frame-to-file #:msf-gif-end-to-file))
(in-package #:msf-gif)

;;----------------------------------------------------------------------------------
;; HEADER
;;----------------------------------------------------------------------------------

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct msf-gif-result
  (data nil :type (or null octets))
  (data-size 0 :type fixnum)

  (alloc-size 0 :type fixnum))          ; internal use

(defstruct msf-cooked-frame               ; internal use
  (pixels nil :type (or null (simple-array (unsigned-byte 32) (*))))
  (depth 0 :type fixnum)
  (count 0 :type fixnum)
  (rbits 0 :type fixnum)
  (gbits 0 :type fixnum)
  (bbits 0 :type fixnum))

;; NOTE: C keeps a linked list of MsfGifBuffer nodes (next, size, data[]), here a list of
;; byte vectors holding exactly SIZE bytes each
(defstruct msf-gif-buffer
  (size 0 :type fixnum)
  (data nil :type (or null octets)))

;; NOTE: fileWriteFunc/fileWriteData (fwrite() and a FILE *) are a binary output stream here
(defstruct msf-gif-state
  (file-write-stream nil)
  (previous-frame (make-msf-cooked-frame) :type msf-cooked-frame)
  (current-frame (make-msf-cooked-frame) :type msf-cooked-frame)
  (lzw-mem nil :type (or null (simple-array (signed-byte 16) (*))))
  (list-head nil :type list)            ; Buffers, in order (C listHead)
  (list-tail nil :type list)            ; Last cons of list-head (C listTail)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (frames-submitted 0 :type fixnum))    ; needed for transparency to work correctly (because we reach into the previous frame)

;; The gif format only supports 1-bit transparency, meaning a pixel will either be fully transparent or fully opaque.
;; Pixels with an alpha value less than the alpha threshold will be treated as transparent.
;; To enable exporting transparent gifs, set it to a value between 1 and 255 (inclusive) before calling msf-gif-frame.
;; Setting it to 0 causes the alpha channel to be ignored. Its initial value is 0.
(defvar *msf-gif-alpha-threshold* 0)

;; Set *msf-gif-bgra-flag* to true before calling msf-gif-frame if your pixels are in BGRA byte order instead of RBGA.
(defvar *msf-gif-bgra-flag* nil)

;;----------------------------------------------------------------------------------
;; IMPLEMENTATION
;;----------------------------------------------------------------------------------

(declaim (inline msf-bit-log msf-imin msf-imax))
(defun msf-bit-log (i) (integer-length i)) ; 32 - __builtin_clz(i)
(defun msf-imin (a b) (if (< a b) a b))
(defun msf-imax (a b) (if (< b a) a b))

;;----------------------------------------------------------------------------------
;; Frame Cooking
;;----------------------------------------------------------------------------------

;; bit depth for each channel
(defparameter +rdepths-array+ #(0 0 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
(defparameter +gdepths-array+ #(0 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6))
(defparameter +bdepths-array+ #(0 0 0 1 1 1 2 2 2 3 3 3 4 4 4 5 5))

(defparameter +dither-kernel+
  (map '(simple-array fixnum (16)) (lambda (v) (ash v 12))
       #(0 8 2 10
         12 4 14 6
         3 11 1 9
         15 7 13 5)))

;; NOTE: C has an SSE2 path computing 4 pixels at a time, its results are identical to the scalar loop
(defun msf-cook-frame (frame raw raw-start used width height pitch depth)
  (declare (type msf-cooked-frame frame) (type octets raw used)
           (type fixnum raw-start width height pitch depth)
           (optimize speed))
  (let* ((rdepths (if *msf-gif-bgra-flag* +bdepths-array+ +rdepths-array+))
         (gdepths +gdepths-array+)
         (bdepths (if *msf-gif-bgra-flag* +rdepths-array+ +bdepths-array+))
         (dither-kernel +dither-kernel+)
         (alpha-threshold *msf-gif-alpha-threshold*)
         (cooked (msf-cooked-frame-pixels frame))
         (count 0))
    (declare (type (simple-array (unsigned-byte 32) (*)) cooked)
             (type (simple-array fixnum (16)) dither-kernel)
             (type fixnum count alpha-threshold))
    (loop
      (let* ((rbits (svref rdepths depth)) (gbits (svref gdepths depth)) (bbits (svref bdepths depth))
             (palette-size (1+ (ash 1 (+ rbits gbits bbits)))))
        (declare (type (integer 0 6) rbits gbits bbits) (type fixnum palette-size))
        (fill used 0 :end palette-size)

        ;; TODO: document what this math does and why it's correct
        (let* ((rdiff (1- (ash 1 (- 8 rbits))))
               (gdiff (1- (ash 1 (- 8 gbits))))
               (bdiff (1- (ash 1 (- 8 bbits))))
               (rmul (truncate (* (/ (- 255.0 rdiff) 255.0) 257)))
               (gmul (truncate (* (/ (- 255.0 gdiff) 255.0) 257)))
               (bmul (truncate (* (/ (- 255.0 bdiff) 255.0) 257)))

               (gmask (ash (1- (ash 1 gbits)) rbits))
               (bmask (ash (ash (1- (ash 1 bbits)) rbits) gbits)))
          (declare (type fixnum rmul gmul bmul gmask bmask))

          (dotimes (y height)
            ;; scalar cleanup loop
            (dotimes (x width)
              (let ((p (+ raw-start (* y pitch) (* x 4))))
                (declare (type fixnum p))
                ;; transparent pixel if alpha is low
                (if (< (aref raw (+ p 3)) alpha-threshold)
                    (setf (aref cooked (+ (* y width) x)) (1- palette-size))
                    (let* ((dx (logand x 3)) (dy (logand y 3))
                           (k (aref dither-kernel (+ (* dy 4) dx))))
                      (setf (aref cooked (+ (* y width) x))
                            (logior (logand (ash (msf-imin 65535 (+ (* (aref raw (+ p 2)) bmul) (ash k (- bbits))))
                                                 (- (- 16 rbits gbits bbits)))
                                            bmask)
                                    (logand (ash (msf-imin 65535 (+ (* (aref raw (+ p 1)) gmul) (ash k (- gbits))))
                                                 (- (- 16 rbits gbits)))
                                            gmask)
                                    (ash (msf-imin 65535 (+ (* (aref raw p) rmul) (ash k (- rbits))))
                                         (- (- 16 rbits)))))))))))

        (setf count 0)
        (dotimes (i (* width height))
          (setf (aref used (aref cooked i)) 1))

        ;; count used colors, transparent is ignored
        (dotimes (j (1- palette-size))
          (incf count (aref used j))))

      (unless (and (>= count 256) (/= (decf depth) 0)) (return)))

    (setf (msf-cooked-frame-pixels frame) cooked
          (msf-cooked-frame-depth frame) depth
          (msf-cooked-frame-count frame) count
          (msf-cooked-frame-rbits frame) (svref rdepths depth)
          (msf-cooked-frame-gbits frame) (svref gdepths depth)
          (msf-cooked-frame-bbits frame) (svref bdepths depth))
    frame))

;;----------------------------------------------------------------------------------
;; Frame Compression
;;----------------------------------------------------------------------------------

;; NOTE: C passes uint8_t **writeHead and uint32_t *blockBits, here the buffer, the write head index
;; and the block bits are passed and the updated (values write-head block-bits) returned
(declaim (inline msf-put-code))
(defun msf-put-code (buffer write-head block-bits len code)
  (declare (type octets buffer) (type fixnum write-head block-bits len code))
  ;; insert new code into block buffer
  (let ((idx (floor block-bits 8))
        (bit (mod block-bits 8)))
    (setf (aref buffer (+ write-head idx 0)) (logand #xFF (logior (aref buffer (+ write-head idx 0)) (ash code bit))))
    (setf (aref buffer (+ write-head idx 1)) (logand #xFF (logior (aref buffer (+ write-head idx 1)) (ash code (- (- 8 bit))))))
    (setf (aref buffer (+ write-head idx 2)) (logand #xFF (logior (aref buffer (+ write-head idx 2)) (ash code (- (- 16 bit))))))
    (incf block-bits len)

    ;; prep the next block buffer if the current one is full
    (when (>= block-bits (* 256 8))
      (decf block-bits (* 255 8))
      (incf write-head 256)
      (setf (aref buffer (+ write-head 2)) (aref buffer (+ write-head 1)))
      (setf (aref buffer (+ write-head 1)) (aref buffer write-head))
      (setf (aref buffer write-head) 255)
      (fill buffer 0 :start (+ write-head 4) :end (+ write-head 4 256)))
    (values write-head block-bits)))

;; MsfStridedList: data is the state lzw-mem
(defstruct msf-strided-list
  (data nil :type (or null (simple-array (signed-byte 16) (*))))
  (len 0 :type fixnum)
  (stride 0 :type fixnum))

(defun msf-lzw-reset (lzw table-size stride)
  (fill (msf-strided-list-data lzw) -1 :end (* 4096 stride))
  (setf (msf-strided-list-len lzw) (+ table-size 2))
  (setf (msf-strided-list-stride lzw) stride))

(defun msf-compress-frame (width height centi-seconds frame handle used lzw-mem)
  (declare (type fixnum width height centi-seconds) (type msf-cooked-frame frame)
           (type msf-gif-state handle) (type octets used)
           (type (simple-array (signed-byte 16) (*)) lzw-mem)
           (optimize speed))
  ;; NOTE: we reserve enough memory for theoretical the worst case upfront because it's a reasonable amount,
  ;;       and prevents us from ever having to check size or realloc during compression
  ;; NOTE: +260 bytes of slack so the next block buffer prep never runs past the end
  (let* ((max-buf-size (+ 32 (* 256 3) (floor (* width height 3) 2))) ; headers + color table + data
         (buffer (make-array (+ max-buf-size 260) :element-type '(unsigned-byte 8) :initial-element 0))
         (write-head 0)
         (lzw (make-msf-strided-list :data lzw-mem))

         ;; allocate tlb
         (total-bits (+ (msf-cooked-frame-rbits frame) (msf-cooked-frame-gbits frame) (msf-cooked-frame-bbits frame)))
         (tlb-size (1+ (ash 1 total-bits)))
         (tlb (make-array (1+ (ash 1 16)) :element-type '(unsigned-byte 8) :initial-element 0))

         ;; generate palette
         (table (make-array (* 256 3) :element-type '(unsigned-byte 8) :initial-element 0)) ; Color3 table[256]
         (table-idx 1)                  ; we start counting at 1 because 0 is the transparent color
         (pixels (msf-cooked-frame-pixels frame)))
    (declare (type fixnum write-head table-idx tlb-size)
             (type (simple-array (unsigned-byte 32) (*)) pixels))
    ;; transparent is always last in the table
    (setf (aref tlb (1- tlb-size)) 0)
    (dotimes (i (1- tlb-size))
      (when (/= (aref used i) 0)
        (setf (aref tlb i) table-idx)
        (let* ((rbits (msf-cooked-frame-rbits frame))
               (gbits (msf-cooked-frame-gbits frame))
               (bbits (msf-cooked-frame-bbits frame))
               (rmask (1- (ash 1 rbits)))
               (gmask (1- (ash 1 gbits)))
               ;; isolate components
               (r (logand i rmask))
               (g (logand (ash i (- rbits)) gmask))
               (b (ash i (- (+ rbits gbits)))))
          ;; shift into highest bits
          (setf r (ash r (- 8 rbits)))
          (setf g (ash g (- 8 gbits)))
          (setf b (ash b (- 8 bbits)))
          (flet ((spread (c bits)
                   (logand #xFF (logior c (ash c (- bits)) (ash c (- (* bits 2))) (ash c (- (* bits 3)))))))
            (setf (aref table (* table-idx 3)) (spread r rbits)
                  (aref table (+ (* table-idx 3) 1)) (spread g gbits)
                  (aref table (+ (* table-idx 3) 2)) (spread b bbits)))
          (when *msf-gif-bgra-flag*
            (rotatef (aref table (* table-idx 3)) (aref table (+ (* table-idx 3) 2))))
          (incf table-idx))))

    (let* ((has-transparent-pixels (/= (aref used (1- tlb-size)) 0))

           ;; SPEC: "Because of some algorithmic constraints however, black & white images which have one color bit
           ;;       must be indicated as having a code size of 2."
           (table-bits (msf-imax 2 (msf-bit-log (1- table-idx))))
           (table-size (ash 1 table-bits))
           ;; NOTE: we don't just compare `depth` field here because it will be wrong for the first frame and we will segfault
           (previous (msf-gif-state-previous-frame handle))
           (previous-pixels (msf-cooked-frame-pixels previous))
           (has-same-pal (and (= (msf-cooked-frame-rbits frame) (msf-cooked-frame-rbits previous))
                              (= (msf-cooked-frame-gbits frame) (msf-cooked-frame-gbits previous))
                              (= (msf-cooked-frame-bbits frame) (msf-cooked-frame-bbits previous))))
           (frames-compatible (and has-same-pal (not has-transparent-pixels)))

           (header-bytes (make-array 18 :element-type '(unsigned-byte 8)
                                        :initial-contents '(#x21 #xF9 #x04 #x05 0 0 0 0
                                                            #x2C 0 0 0 0 0 0 0 0 #x80)))
           (block-bits 8))                ; relative to block.head
      (declare (type fixnum table-bits table-size block-bits)
               (type (simple-array (unsigned-byte 32) (*)) previous-pixels))
      ;; NOTE: we need to check the frame number because if we reach into the buffer prior to the first frame,
      ;;       we'll just clobber the file header instead, which is a bug
      (when (and has-transparent-pixels (> (msf-gif-state-frames-submitted handle) 0))
        ;; set the previous frame's disposal to background, so transparency is possible
        (setf (aref (msf-gif-buffer-data (car (msf-gif-state-list-tail handle))) 3) #x09))
      (setf (aref header-bytes 4) (ldb (byte 8 0) centi-seconds)
            (aref header-bytes 5) (ldb (byte 8 8) centi-seconds))
      (setf (aref header-bytes 13) (ldb (byte 8 0) width)
            (aref header-bytes 14) (ldb (byte 8 8) width))
      (setf (aref header-bytes 15) (ldb (byte 8 0) height)
            (aref header-bytes 16) (ldb (byte 8 8) height))
      (setf (aref header-bytes 17) (logior (aref header-bytes 17) (1- table-bits)))
      (replace buffer header-bytes :start1 write-head)
      (incf write-head 18)

      ;; local color table
      (replace buffer table :start1 write-head :end2 (* table-size 3))
      (incf write-head (* table-size 3))
      (setf (aref buffer write-head) table-bits)
      (incf write-head)

      ;; prep block
      (fill buffer 0 :start write-head :end (+ write-head 260))
      (setf (aref buffer write-head) 255)

      ;; SPEC: "Encoders should output a Clear code as the first code of each image data stream."
      (msf-lzw-reset lzw table-size table-idx)
      (multiple-value-setq (write-head block-bits)
        (msf-put-code buffer write-head block-bits (msf-bit-log (1- (msf-strided-list-len lzw))) table-size))

      (let ((last-code (if (and frames-compatible (= (aref pixels 0) (aref previous-pixels 0)))
                           0
                           (aref tlb (aref pixels 0))))
            (lzw-len (msf-strided-list-len lzw))
            (stride table-idx))
        (declare (type fixnum last-code lzw-len stride))
        (loop for i of-type fixnum from 1 below (* width height)
              do ;; PERF: branching vs. branchless version of this line is observed to have no discernable impact on speed
                 (let* ((color (if (and frames-compatible (= (aref pixels i) (aref previous-pixels i)))
                                   0
                                   (aref tlb (aref pixels i))))
                        (code (aref lzw-mem (+ (* last-code stride) color))))
                   (declare (type fixnum color code))
                   (if (< code 0)
                       ;; write to code stream
                       (let ((code-bits (msf-bit-log (1- lzw-len))))
                         (multiple-value-setq (write-head block-bits)
                           (msf-put-code buffer write-head block-bits code-bits last-code))

                         (if (> lzw-len 4095)
                             (progn
                               ;; reset buffer code table
                               (multiple-value-setq (write-head block-bits)
                                 (msf-put-code buffer write-head block-bits code-bits table-size))
                               (msf-lzw-reset lzw table-size table-idx)
                               (setf lzw-len (msf-strided-list-len lzw)))
                             (progn
                               (setf (aref lzw-mem (+ (* last-code stride) color)) lzw-len)
                               (incf lzw-len)))

                         (setf last-code color))
                       (setf last-code code))))

        ;; write code for leftover index buffer contents, then the end code
        (multiple-value-setq (write-head block-bits)
          (msf-put-code buffer write-head block-bits (msf-imin 12 (msf-bit-log (1- lzw-len))) last-code))
        (multiple-value-setq (write-head block-bits)
          (msf-put-code buffer write-head block-bits (msf-imin 12 (msf-bit-log lzw-len)) (1+ table-size))))

      ;; flush remaining data
      (when (> block-bits 8)
        (let ((bytes (floor (+ block-bits 7) 8))) ; round up
          (setf (aref buffer write-head) (1- bytes))
          (incf write-head bytes)))
      (setf (aref buffer write-head) 0) ; terminating block
      (incf write-head)

      ;; fill in buffer header and shrink buffer to fit data
      (make-msf-gif-buffer :size write-head :data (subseq buffer 0 write-head)))))

;;----------------------------------------------------------------------------------
;; To-memory API
;;----------------------------------------------------------------------------------

(defconstant +lzw-alloc-size+ (* 4096 256)) ; int16_t elements

(defun msf-free-gif-state (handle)
  (setf (msf-cooked-frame-pixels (msf-gif-state-previous-frame handle)) nil
        (msf-cooked-frame-pixels (msf-gif-state-current-frame handle)) nil
        (msf-gif-state-lzw-mem handle) nil)
  (setf (msf-gif-state-list-head handle) nil ; this implicitly marks the handle as invalid until the next msf-gif-begin call
        (msf-gif-state-list-tail handle) nil))

(defun msf-gif-begin (handle width height)
  "Begin a gif of WIDTH x HEIGHT pixels: returns non-zero on success, 0 on error"
  (setf (msf-gif-state-previous-frame handle) (make-msf-cooked-frame))
  (setf (msf-gif-state-current-frame handle) (make-msf-cooked-frame))
  (setf (msf-gif-state-width handle) width)
  (setf (msf-gif-state-height handle) height)
  (setf (msf-gif-state-frames-submitted handle) 0)

  ;; allocate memory for LZW buffer
  (setf (msf-gif-state-lzw-mem handle) (make-array +lzw-alloc-size+ :element-type '(signed-byte 16) :initial-element 0))
  (setf (msf-cooked-frame-pixels (msf-gif-state-previous-frame handle))
        (make-array (* width height) :element-type '(unsigned-byte 32) :initial-element 0))
  (setf (msf-cooked-frame-pixels (msf-gif-state-current-frame handle))
        (make-array (* width height) :element-type '(unsigned-byte 32) :initial-element 0))

  ;; setup header buffer header (lol)
  (let ((header-bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace header-bytes (map 'vector #'char-code "GIF89a"))
    (setf (aref header-bytes 6) (ldb (byte 8 0) width)
          (aref header-bytes 7) (ldb (byte 8 8) width)
          (aref header-bytes 8) (ldb (byte 8 0) height)
          (aref header-bytes 9) (ldb (byte 8 8) height)
          (aref header-bytes 10) #x70)
    (replace header-bytes #(#x21 #xFF #x0B) :start1 13)
    (replace header-bytes (map 'vector #'char-code "NETSCAPE2.0") :start1 16)
    (replace header-bytes #(#x03 #x01 0 0 0) :start1 27)
    (setf (msf-gif-state-list-head handle) (list (make-msf-gif-buffer :size 32 :data header-bytes)))
    (setf (msf-gif-state-list-tail handle) (msf-gif-state-list-head handle)))
  1)

(defun msf-gif-frame (handle pixel-data centi-seconds-per-fame max-bit-depth pitch-in-bytes)
  "Add a frame: PIXEL-DATA is a byte vector of RGBA8 (or BGRA8 with *msf-gif-bgra-flag*) rows,
PITCH-IN-BYTES the distance between rows (negative to flip the image), returns non-zero on success, 0 on error"
  (unless (msf-gif-state-list-head handle) (return-from msf-gif-frame 0))

  (setf max-bit-depth (msf-imax 1 (msf-imin 16 max-bit-depth)))
  (when (= pitch-in-bytes 0) (setf pitch-in-bytes (* (msf-gif-state-width handle) 4)))
  (let ((pixel-start (if (< pitch-in-bytes 0) (- (* pitch-in-bytes (1- (msf-gif-state-height handle)))) 0))
        (used (make-array (1+ (ash 1 16)) :element-type '(unsigned-byte 8) :initial-element 0))
        (previous (msf-gif-state-previous-frame handle)))
    (msf-cook-frame (msf-gif-state-current-frame handle) pixel-data pixel-start used
                    (msf-gif-state-width handle) (msf-gif-state-height handle) pitch-in-bytes
                    (msf-imin max-bit-depth (+ (msf-cooked-frame-depth previous)
                                               (floor 160 (msf-imax 1 (msf-cooked-frame-count previous))))))

    (let ((buffer (msf-compress-frame (msf-gif-state-width handle) (msf-gif-state-height handle)
                                      centi-seconds-per-fame (msf-gif-state-current-frame handle) handle used
                                      (msf-gif-state-lzw-mem handle))))
      (setf (cdr (msf-gif-state-list-tail handle)) (list buffer))
      (setf (msf-gif-state-list-tail handle) (cdr (msf-gif-state-list-tail handle)))))

  ;; swap current and previous frames
  (rotatef (msf-gif-state-previous-frame handle) (msf-gif-state-current-frame handle))

  (incf (msf-gif-state-frames-submitted handle))
  1)

(defun msf-gif-end (handle)
  "Finish the gif: returns a msf-gif-result with the gif file data, data is NIL on error"
  (unless (msf-gif-state-list-head handle) (return-from msf-gif-end (make-msf-gif-result)))

  ;; first pass: determine total size
  (let ((total 1))                      ; 1 byte for trailing marker
    (dolist (node (msf-gif-state-list-head handle))
      (incf total (msf-gif-buffer-size node)))

    ;; second pass: write data
    (let ((buffer (make-array total :element-type '(unsigned-byte 8)))
          (write-head 0))
      (dolist (node (msf-gif-state-list-head handle))
        (replace buffer (msf-gif-buffer-data node) :start1 write-head :end2 (msf-gif-buffer-size node))
        (incf write-head (msf-gif-buffer-size node)))
      (setf (aref buffer write-head) #x3B)

      ;; third pass: free buffers
      (msf-free-gif-state handle)

      (make-msf-gif-result :data buffer :data-size total :alloc-size total))))

(defun msf-gif-free (result)
  "Free the gif data of RESULT (memory is managed by the GC, it only drops the reference)"
  (setf (msf-gif-result-data result) nil))

;;----------------------------------------------------------------------------------
;; To-file API
;;----------------------------------------------------------------------------------
;; These functions are equivalent to the ones above, but they write results to a binary output STREAM
;; incrementally, instead of building a buffer in memory

(defun msf-gif-begin-to-file (handle width height stream)
  (setf (msf-gif-state-file-write-stream handle) stream)
  (msf-gif-begin handle width height))

(defun msf-gif-frame-to-file (handle pixel-data centi-seconds-per-fame max-bit-depth pitch-in-bytes)
  (when (= (msf-gif-frame handle pixel-data centi-seconds-per-fame max-bit-depth pitch-in-bytes) 0)
    (return-from msf-gif-frame-to-file 0))

  ;; NOTE: this is a somewhat hacky implementation which is not perfectly efficient, but it's good enough for now
  (let ((head (car (msf-gif-state-list-head handle))))
    (write-sequence (msf-gif-buffer-data head) (msf-gif-state-file-write-stream handle) :end (msf-gif-buffer-size head))
    (pop (msf-gif-state-list-head handle))
    1))

(defun msf-gif-end-to-file (handle)
  ;; NOTE: this is a somewhat hacky implementation which is not perfectly efficient, but it's good enough for now
  (let ((result (msf-gif-end handle)))
    (if (msf-gif-result-data result)
        (progn
          (write-sequence (msf-gif-result-data result) (msf-gif-state-file-write-stream handle)
                          :end (msf-gif-result-data-size result))
          (msf-gif-free result)
          1)
        0)))
