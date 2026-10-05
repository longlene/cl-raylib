(in-package #:cl-raylib)

;;;===================================================================================
;;; stb_vorbis - Ogg Vorbis audio decoder
;;; Port of raylib/src/external/stb_vorbis.c (v1.22) pull API, used by raudio
;;;
;;; Ported: memory decoding (stb_vorbis_open_memory), stream length, frame decoding,
;;; seeking (stb_vorbis_seek_frame, stb_vorbis_seek_start) and short interleaved output
;;; NOTE: Push data API, stdio functions and floor type 0 (not supported by stb_vorbis
;;; either) are not ported, files are decoded from memory
;;; NOTE: Configuration matches stb_vorbis defaults: STB_VORBIS_FAST_HUFFMAN_LENGTH 10,
;;; deferred floor, fast scaled float to int conversion
;;;===================================================================================

(defconstant +stbv-max-channels+ 16)
(defconstant +stbv-fast-huffman-length+ 10)
(defconstant +stbv-fast-huffman-table-size+ (ash 1 +stbv-fast-huffman-length+))
(defconstant +stbv-fast-huffman-table-mask+ (- +stbv-fast-huffman-table-size+ 1))
(defconstant +stbv-no-code+ 255)
(defconstant +stbv-eop+ -1)
(defconstant +stbv-invalid-bits+ -1)
(defconstant +stbv-sample-unknown+ #xffffffff)

(defconstant +stbv-pageflag-continued-packet+ 1)
(defconstant +stbv-pageflag-first-page+ 2)
(defconstant +stbv-pageflag-last-page+ 4)

(defconstant +stbv-packet-id+ 1)
(defconstant +stbv-packet-comment+ 3)
(defconstant +stbv-packet-setup+ 5)

(deftype %f32-array () '(simple-array single-float (*)))

(defstruct (stbv-codebook (:conc-name cb-))
  (dimensions 0 :type fixnum)
  (entries 0 :type fixnum)
  (codeword-lengths nil)                ; (unsigned-byte 8) array
  (minimum-value 0f0 :type single-float)
  (delta-value 0f0 :type single-float)
  (value-bits 0)
  (lookup-type 0)
  (sequence-p 0)
  (sparse 0)
  (lookup-values 0)
  (multiplicands nil)                   ; single-float array
  (codewords nil)                       ; (unsigned-byte 32) array
  (fast-huffman (make-array +stbv-fast-huffman-table-size+ :element-type '(signed-byte 16) :initial-element -1)
   :type (simple-array (signed-byte 16) (*)))
  (sorted-codewords nil)                ; (unsigned-byte 32) array
  (sorted-values nil)                   ; fixnum array, index offset by 1 so that sorted_values[-1] = -1
  (sorted-entries 0 :type fixnum))

(defstruct (stbv-floor1 (:conc-name fl-))
  (partitions 0)
  (partition-class-list (make-array 32 :initial-element 0))
  (class-dimensions (make-array 16 :initial-element 0))
  (class-subclasses (make-array 16 :initial-element 0))
  (class-masterbooks (make-array 16 :initial-element 0))
  (subclass-books (make-array '(16 8) :initial-element 0))
  (xlist (make-array (+ (* 31 8) 2) :initial-element 0))
  (sorted-order (make-array (+ (* 31 8) 2) :initial-element 0))
  (neighbors (make-array (list (+ (* 31 8) 2) 2) :initial-element 0))
  (floor1-multiplier 0)
  (rangebits 0)
  (values 0))

(defstruct (stbv-residue (:conc-name rs-))
  (begin 0) (end 0)
  (part-size 0)
  (classifications 0)
  (classbook 0)
  (classdata nil)                       ; Vector of (unsigned-byte 8) vectors
  (residue-books nil))                  ; 2D array [classifications][8]

(defstruct (stbv-mapping (:conc-name mp-))
  (coupling-steps 0)
  (chan-magnitude nil)
  (chan-angle nil)
  (chan-mux nil)
  (submaps 0)
  (submap-floor (make-array 15 :initial-element 0))
  (submap-residue (make-array 15 :initial-element 0)))

(defstruct (stbv-mode (:conc-name md-))
  (blockflag 0) (mapping 0) (windowtype 0) (transformtype 0))

(defstruct (stbv-probed-page (:conc-name pp-))
  (page-start 0) (page-end 0) (last-decoded-sample 0))

(defstruct (stb-vorbis (:conc-name vb-) (:constructor %make-stb-vorbis))
  (sample-rate 0)
  (channels 0)
  (vendor "")
  (comment-list nil)
  ;; Memory stream
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (stream 0 :type fixnum)                       ; Current position
  (stream-end 0 :type fixnum)
  (stream-len 0 :type fixnum)
  (first-audio-page-offset 0)
  (p-first (make-stbv-probed-page))
  (p-last (make-stbv-probed-page))
  (eof nil)
  (error 0)
  ;; Header info
  (blocksize (make-array 2 :initial-element 0))
  (blocksize-0 0 :type fixnum)
  (blocksize-1 0 :type fixnum)
  (codebook-count 0)
  (codebooks nil)
  (floor-count 0)
  (floor-types (make-array 64 :initial-element 0))
  (floor-config nil)
  (residue-count 0)
  (residue-types (make-array 64 :initial-element 0))
  (residue-config nil)
  (mapping-count 0)
  (mapping nil)
  (mode-count 0)
  (mode-config (make-array 64 :initial-element nil))
  (total-samples 0)
  ;; Decode buffer
  (channel-buffers (make-array +stbv-max-channels+ :initial-element nil))
  (previous-window (make-array +stbv-max-channels+ :initial-element nil))
  (previous-length 0 :type fixnum)
  (final-y (make-array +stbv-max-channels+ :initial-element nil))
  (current-loc 0 :type (unsigned-byte 32))      ; Sample location of next frame to decode
  (current-loc-valid nil)
  ;; Per-blocksize precomputed data
  (a (make-array 2 :initial-element nil))
  (b (make-array 2 :initial-element nil))
  (c (make-array 2 :initial-element nil))
  (window (make-array 2 :initial-element nil))
  (bit-reverse (make-array 2 :initial-element nil))
  (imdct-buf nil)                               ; Temp buffer for inverse_mdct()
  ;; Current page/packet/segment streaming info
  (serial 0)
  (last-page 0)
  (segment-count 0 :type fixnum)
  (segments (make-array 255 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (255)))
  (page-flag 0 :type fixnum)
  (bytes-in-seg 0 :type fixnum)
  (first-decode nil)
  (next-seg 0 :type fixnum)
  (last-seg nil)                                ; Flag that we're on the last segment
  (last-seg-which 0 :type fixnum)               ; What was the segment number of the last seg?
  (acc 0 :type (unsigned-byte 32))
  (valid-bits 0 :type fixnum)
  (packet-bytes 0 :type fixnum)
  (end-seg-with-known-loc 0 :type fixnum)
  (known-loc-for-packet 0 :type (unsigned-byte 32))
  (discard-samples-deferred 0 :type fixnum)
  (samples-output 0)
  ;; Sample-access
  (channel-buffer-start 0 :type fixnum)
  (channel-buffer-end 0 :type fixnum))

;; STBVorbisError codes
(defconstant +vorbis-no-error+ 0)
(defconstant +vorbis-need-more-data+ 1)
(defconstant +vorbis-invalid-api-mixing+ 2)
(defconstant +vorbis-outofmem+ 3)
(defconstant +vorbis-feature-not-supported+ 4)
(defconstant +vorbis-too-many-channels+ 5)
(defconstant +vorbis-file-open-failure+ 6)
(defconstant +vorbis-seek-without-length+ 7)
(defconstant +vorbis-unexpected-eof+ 10)
(defconstant +vorbis-seek-invalid+ 11)
(defconstant +vorbis-invalid-setup+ 20)
(defconstant +vorbis-invalid-stream+ 21)
(defconstant +vorbis-missing-capture-pattern+ 30)
(defconstant +vorbis-invalid-stream-structure-version+ 31)
(defconstant +vorbis-continued-packet-flag-invalid+ 32)
(defconstant +vorbis-incorrect-stream-serial-number+ 33)
(defconstant +vorbis-invalid-first-page+ 34)
(defconstant +vorbis-bad-packet-type+ 35)
(defconstant +vorbis-cant-find-last-page+ 36)
(defconstant +vorbis-seek-failed+ 37)
(defconstant +vorbis-ogg-skeleton-not-supported+ 38)

(defun %stbv-error (f e)
  (setf (vb-error f) e)
  nil)

;;;----------------------------------------------------------------------------------
;;; Utility functions
;;;----------------------------------------------------------------------------------

(defvar *stbv-crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (i 256 table)
      (let ((s (ash i 24)))
        (dotimes (j 8)
          (setf s (logxor (logand (ash s 1) #xffffffff) (if (>= s #x80000000) #x04c11db7 0))))
        (setf (aref table i) s)))))

(declaim (inline %stbv-crc32-update))
(defun %stbv-crc32-update (crc byte)
  (logxor (logand (ash crc 8) #xffffffff) (aref *stbv-crc-table* (logxor byte (ash crc -24)))))

(declaim (inline %stbv-bit-reverse))
(defun %stbv-bit-reverse (n)
  (declare (type (unsigned-byte 32) n))
  (setf n (logior (ash (logand n #xAAAAAAAA) -1) (logand (ash (logand n #x55555555) 1) #xffffffff)))
  (setf n (logior (ash (logand n #xCCCCCCCC) -2) (logand (ash (logand n #x33333333) 2) #xffffffff)))
  (setf n (logior (ash (logand n #xF0F0F0F0) -4) (logand (ash (logand n #x0F0F0F0F) 4) #xffffffff)))
  (setf n (logior (ash (logand n #xFF00FF00) -8) (logand (ash (logand n #x00FF00FF) 8) #xffffffff)))
  (logior (ash n -16) (logand (ash n 16) #xffffffff)))

(defun %stbv-ilog (n)
  (cond ((< n 0) 0)                     ; signed n returns 0
        (t (integer-length n))))

(defun %stbv-float32-unpack (x)
  (let* ((mantissa (logand x #x1fffff))
         (sign (logand x #x80000000))
         (exp (ash (logand x #x7fe00000) -21))
         (res (if (/= sign 0) (- (float mantissa 1d0)) (float mantissa 1d0))))
    ;; ldexp((float)res, (int)exp-788)
    (coerce (scale-float (float (coerce res 'single-float) 1d0) (- exp 788)) 'single-float)))

(defun %stbv-add-entry (c huff-code symbol count len values)
  (if (= (cb-sparse c) 0)
      (setf (aref (cb-codewords c) symbol) huff-code)
      (setf (aref (cb-codewords c) count) huff-code
            (aref (cb-codeword-lengths c) count) len
            (aref values count) symbol)))

(defun %stbv-compute-codewords (c len n values)
  (let ((available (make-array 32 :element-type '(unsigned-byte 32) :initial-element 0))
        (m 0)
        (k (or (position-if (lambda (l) (< l +stbv-no-code+)) len :end n) n)))
    ;; Find the first entry
    (when (= k n)
      (return-from %stbv-compute-codewords t))
    ;; Add to the list
    (%stbv-add-entry c 0 k m (aref len k) values)
    (incf m)
    ;; Add all available leaves
    (loop for i from 1 to (aref len k)
          do (setf (aref available i) (ash 1 (- 32 i))))
    ;; Note that the above code treats the first case specially,
    ;; but it's really the same as the following code
    (loop for i from (+ k 1) below n
          do (let ((z (aref len i)))
               (unless (= z +stbv-no-code+)
                 ;; Find lowest available leaf (should always be earliest, which is what spec requires)
                 (loop while (and (> z 0) (= (aref available z) 0)) do (decf z))
                 (when (= z 0) (return-from %stbv-compute-codewords nil))
                 (let ((res (aref available z)))
                   (setf (aref available z) 0)
                   (%stbv-add-entry c (%stbv-bit-reverse res) i m (aref len i) values)
                   (incf m)
                   ;; Propagate availability up the tree
                   (when (/= z (aref len i))
                     (loop for y from (aref len i) above z
                           do (setf (aref available y) (logand (+ res (ash 1 (- 32 y))) #xffffffff))))))))
    t))

;; Accelerated huffman table allows fast O(1) match of all symbols of length <= STB_VORBIS_FAST_HUFFMAN_LENGTH
(defun %stbv-compute-accelerated-huffman (c)
  (let ((fast (cb-fast-huffman c))
        (len (min (if (/= (cb-sparse c) 0) (cb-sorted-entries c) (cb-entries c)) 32767)))
    (fill fast -1)
    (dotimes (i len)
      (when (<= (aref (cb-codeword-lengths c) i) +stbv-fast-huffman-length+)
        (let ((z (if (/= (cb-sparse c) 0)
                     (%stbv-bit-reverse (aref (cb-sorted-codewords c) i))
                     (aref (cb-codewords c) i))))
          ;; Set table entries for all bit combinations in the higher bits
          (loop while (< z +stbv-fast-huffman-table-size+)
                do (setf (aref fast z) i)
                   (incf z (ash 1 (aref (cb-codeword-lengths c) i)))))))))

(defun %stbv-include-in-sort (c len)
  (cond ((/= (cb-sparse c) 0) t)
        ((= len +stbv-no-code+) nil)
        ((> len +stbv-fast-huffman-length+) t)
        (t nil)))

;; If the fast table above doesn't work, we want to binary search them
(defun %stbv-compute-sorted-huffman (c lengths values)
  (let ((sorted (cb-sorted-codewords c)))
    ;; Build a list of all the entries
    (if (= (cb-sparse c) 0)
        (let ((k 0))
          (dotimes (i (cb-entries c))
            (when (%stbv-include-in-sort c (aref lengths i))
              (setf (aref sorted k) (%stbv-bit-reverse (aref (cb-codewords c) i)))
              (incf k))))
        (dotimes (i (cb-sorted-entries c))
          (setf (aref sorted i) (%stbv-bit-reverse (aref (cb-codewords c) i)))))
    (setf (subseq sorted 0 (cb-sorted-entries c)) (sort (subseq sorted 0 (cb-sorted-entries c)) #'<))
    (setf (aref sorted (cb-sorted-entries c)) #xffffffff)
    (let ((len (if (/= (cb-sparse c) 0) (cb-sorted-entries c) (cb-entries c))))
      ;; Now we need to indicate how they correspond; we could either
      ;;   #1: sort a different data structure that says who they correspond to
      ;;   #2: for each sorted entry, search the original list to find who corresponds
      ;;   #3: for each original entry, find the sorted entry
      (dotimes (i len)
        (let ((huff-len (if (/= (cb-sparse c) 0) (aref lengths (aref values i)) (aref lengths i))))
          (when (%stbv-include-in-sort c huff-len)
            (let ((code (%stbv-bit-reverse (aref (cb-codewords c) i)))
                  (x 0)
                  (n (cb-sorted-entries c)))
              (loop while (> n 1)
                    do ;; Invariant: sc[x] <= code < sc[x+n]
                       (let ((m (+ x (ash n -1))))
                         (if (<= (aref sorted m) code)
                             (setf x m n (- n (ash n -1)))
                             (setf n (ash n -1)))))
              (if (/= (cb-sparse c) 0)
                  (setf (aref (cb-sorted-values c) (+ x 1)) (aref values i)
                        (aref (cb-codeword-lengths c) x) huff-len)
                  (setf (aref (cb-sorted-values c) (+ x 1)) i)))))))))

(defun %stbv-lookup1-values (entries dim)
  (let ((r (floor (exp (coerce (/ (coerce (log (float (coerce entries 'single-float) 1d0)) 'single-float)
                                  (coerce dim 'single-float))
                               'double-float)))))
    (when (<= (floor (expt (float (+ (coerce r 'single-float) 1f0) 1d0) dim)) entries)
      (incf r))
    (cond ((<= (expt (float (+ (coerce r 'single-float) 1f0) 1d0) dim) entries) -1)
          ((> (floor (expt (float (coerce r 'single-float) 1d0) dim)) entries) -1)
          (t r))))

;; Only run while parsing the header (3 times)
(defun %stbv-compute-twiddle-factors (n a b c)
  (let ((n4 (ash n -2)) (n8 (ash n -3)))
    (loop for k from 0 below n4
          for k2 from 0 by 2
          do (setf (aref a k2) (coerce (cos (/ (* 4 k pi) n)) 'single-float)
                   (aref a (+ k2 1)) (coerce (- (sin (/ (* 4 k pi) n))) 'single-float)
                   (aref b k2) (* (coerce (cos (/ (/ (* (+ k2 1) pi) n) 2)) 'single-float) 0.5f0)
                   (aref b (+ k2 1)) (* (coerce (sin (/ (/ (* (+ k2 1) pi) n) 2)) 'single-float) 0.5f0)))
    (loop for k from 0 below n8
          for k2 from 0 by 2
          do (setf (aref c k2) (coerce (cos (/ (* 2 (+ k2 1) pi) n)) 'single-float)
                   (aref c (+ k2 1)) (coerce (- (sin (/ (* 2 (+ k2 1) pi) n))) 'single-float)))))

(defun %stbv-compute-window (n window)
  (let ((n2 (ash n -1)))
    (dotimes (i n2)
      (let* ((s (coerce (sin (* (/ (+ i 0.5d0) n2) 0.5d0 pi)) 'single-float))
             (sq (* s s)))
        (setf (aref window i) (coerce (sin (* 0.5d0 pi sq)) 'single-float))))))

(defun %stbv-compute-bitreverse (n rev)
  (let ((ld (- (%stbv-ilog n) 1))      ; ilog is off-by-one from normal definitions
        (n8 (ash n -3)))
    (dotimes (i n8)
      (setf (aref rev i) (ash (ash (%stbv-bit-reverse i) (- (+ (- 32 ld) 3))) 2)))))

(defun %stbv-init-blocksize (f b n)
  (let ((n2 (ash n -1)) (n4 (ash n -2)) (n8 (ash n -3)))
    (setf (aref (vb-a f) b) (make-array n2 :element-type 'single-float :initial-element 0f0)
          (aref (vb-b f) b) (make-array n2 :element-type 'single-float :initial-element 0f0)
          (aref (vb-c f) b) (make-array n4 :element-type 'single-float :initial-element 0f0))
    (%stbv-compute-twiddle-factors n (aref (vb-a f) b) (aref (vb-b f) b) (aref (vb-c f) b))
    (setf (aref (vb-window f) b) (make-array n2 :element-type 'single-float :initial-element 0f0))
    (%stbv-compute-window n (aref (vb-window f) b))
    (setf (aref (vb-bit-reverse f) b) (make-array n8 :element-type '(unsigned-byte 16) :initial-element 0))
    (%stbv-compute-bitreverse n (aref (vb-bit-reverse f) b))
    t))

(defun %stbv-neighbors (x n)
  (let ((low -1) (high 65536) (plow 0) (phigh 0))
    (dotimes (i n)
      (when (and (> (aref x i) low) (< (aref x i) (aref x n))) (setf plow i low (aref x i)))
      (when (and (< (aref x i) high) (> (aref x i) (aref x n))) (setf phigh i high (aref x i))))
    (values plow phigh)))

;;;----------------------------------------------------------------------------------
;;; Memory stream
;;;----------------------------------------------------------------------------------

(declaim (inline %stbv-get8))
(defun %stbv-get8 (z)
  (if (>= (vb-stream z) (vb-stream-end z))
      (progn (setf (vb-eof z) t) 0)
      (prog1 (aref (vb-data z) (vb-stream z))
        (incf (vb-stream z)))))

(defun %stbv-get32 (f)
  (let ((x (%stbv-get8 f)))
    (incf x (ash (%stbv-get8 f) 8))
    (incf x (ash (%stbv-get8 f) 16))
    (logand (+ x (ash (%stbv-get8 f) 24)) #xffffffff)))

(defun %stbv-getn (z data n)
  (if (> (+ (vb-stream z) n) (vb-stream-end z))
      (progn (setf (vb-eof z) t) nil)
      (progn (replace data (vb-data z) :start2 (vb-stream z) :end2 (+ (vb-stream z) n))
             (incf (vb-stream z) n)
             t)))

(defun %stbv-skip (z n)
  (incf (vb-stream z) n)
  (when (>= (vb-stream z) (vb-stream-end z))
    (setf (vb-eof z) t)))

(defun %stbv-set-file-offset (f loc)
  (setf (vb-eof f) nil)
  (if (or (>= loc (vb-stream-end f)) (< loc 0))
      (progn (setf (vb-stream f) (vb-stream-end f)
                   (vb-eof f) t)
             nil)
      (progn (setf (vb-stream f) loc) t)))

(defun stb-vorbis-get-file-offset (f)
  (vb-stream f))

(defun %stbv-capture-pattern (f)
  (and (= #x4f (%stbv-get8 f)) (= #x67 (%stbv-get8 f)) (= #x67 (%stbv-get8 f)) (= #x53 (%stbv-get8 f))))

(defun %stbv-start-page-no-capturepattern (f)
  (when (vb-first-decode f)
    (setf (pp-page-start (vb-p-first f)) (- (stb-vorbis-get-file-offset f) 4)))
  ;; Stream structure version
  (unless (= 0 (%stbv-get8 f)) (return-from %stbv-start-page-no-capturepattern
                                 (%stbv-error f +vorbis-invalid-stream-structure-version+)))
  ;; Header flag
  (setf (vb-page-flag f) (%stbv-get8 f))
  ;; Absolute granule position
  (let ((loc0 (%stbv-get32 f))
        (loc1 (%stbv-get32 f)))
    ;; Stream serial number -- vorbis doesn't interleave, so discard
    (%stbv-get32 f)
    ;; Page sequence number
    (setf (vb-last-page f) (%stbv-get32 f))
    ;; CRC32
    (%stbv-get32 f)
    ;; Page_segments
    (setf (vb-segment-count f) (%stbv-get8 f))
    (unless (%stbv-getn f (vb-segments f) (vb-segment-count f))
      (return-from %stbv-start-page-no-capturepattern (%stbv-error f +vorbis-unexpected-eof+)))
    ;; Assume we _don't_ know any the sample position of any segments
    (setf (vb-end-seg-with-known-loc f) -2)
    (when (or (/= loc0 #xffffffff) (/= loc1 #xffffffff))
      ;; Determine which packet is the last one that will complete
      (let ((i (loop for i from (- (vb-segment-count f) 1) downto 0
                     when (< (aref (vb-segments f) i) 255) return i
                     finally (return -1))))
        ;; 'i' is now the index of the _last_ segment of a packet that ends
        (when (>= i 0)
          (setf (vb-end-seg-with-known-loc f) i
                (vb-known-loc-for-packet f) loc0))))
    (when (vb-first-decode f)
      (let ((len (+ (loop for i from 0 below (vb-segment-count f) sum (aref (vb-segments f) i))
                    27 (vb-segment-count f))))
        (setf (pp-page-end (vb-p-first f)) (+ (pp-page-start (vb-p-first f)) len)
              (pp-last-decoded-sample (vb-p-first f)) loc0)))
    (setf (vb-next-seg f) 0)
    t))

(defun %stbv-start-page (f)
  (if (not (%stbv-capture-pattern f))
      (%stbv-error f +vorbis-missing-capture-pattern+)
      (%stbv-start-page-no-capturepattern f)))

(defun %stbv-start-packet (f)
  (loop while (= (vb-next-seg f) -1)
        do (unless (%stbv-start-page f) (return-from %stbv-start-packet nil))
           (when (logtest (vb-page-flag f) +stbv-pageflag-continued-packet+)
             (return-from %stbv-start-packet (%stbv-error f +vorbis-continued-packet-flag-invalid+))))
  (setf (vb-last-seg f) nil
        (vb-valid-bits f) 0
        (vb-packet-bytes f) 0
        (vb-bytes-in-seg f) 0)
  ;; f->next_seg is now valid
  t)

(defun %stbv-maybe-start-packet (f)
  (when (= (vb-next-seg f) -1)
    (let ((x (%stbv-get8 f)))
      (when (vb-eof f) (return-from %stbv-maybe-start-packet nil))   ; EOF at page boundary is not an error!
      (unless (and (= x #x4f) (= (%stbv-get8 f) #x67) (= (%stbv-get8 f) #x67) (= (%stbv-get8 f) #x53))
        (return-from %stbv-maybe-start-packet (%stbv-error f +vorbis-missing-capture-pattern+)))
      (unless (%stbv-start-page-no-capturepattern f)
        (return-from %stbv-maybe-start-packet nil))
      (when (logtest (vb-page-flag f) +stbv-pageflag-continued-packet+)
        ;; Set up enough state that we can read this packet if we want, e.g. during recovery
        (setf (vb-last-seg f) nil
              (vb-bytes-in-seg f) 0)
        (return-from %stbv-maybe-start-packet (%stbv-error f +vorbis-continued-packet-flag-invalid+)))))
  (%stbv-start-packet f))

(defun %stbv-next-segment (f)
  (when (vb-last-seg f) (return-from %stbv-next-segment 0))
  (when (= (vb-next-seg f) -1)
    (setf (vb-last-seg-which f) (- (vb-segment-count f) 1))   ; In case start_page fails
    (unless (%stbv-start-page f)
      (setf (vb-last-seg f) t)
      (return-from %stbv-next-segment 0))
    (unless (logtest (vb-page-flag f) +stbv-pageflag-continued-packet+)
      (%stbv-error f +vorbis-continued-packet-flag-invalid+)
      (return-from %stbv-next-segment 0)))
  (let ((len (aref (vb-segments f) (vb-next-seg f))))
    (incf (vb-next-seg f))
    (when (< len 255)
      (setf (vb-last-seg f) t
            (vb-last-seg-which f) (- (vb-next-seg f) 1)))
    (when (>= (vb-next-seg f) (vb-segment-count f))
      (setf (vb-next-seg f) -1))
    (setf (vb-bytes-in-seg f) len)
    len))

(defun %stbv-get8-packet-raw (f)
  (when (= (vb-bytes-in-seg f) 0)
    (cond ((vb-last-seg f) (return-from %stbv-get8-packet-raw +stbv-eop+))
          ((= (%stbv-next-segment f) 0) (return-from %stbv-get8-packet-raw +stbv-eop+))))
  (decf (vb-bytes-in-seg f))
  (incf (vb-packet-bytes f))
  (%stbv-get8 f))

(defun %stbv-get8-packet (f)
  (prog1 (%stbv-get8-packet-raw f)
    (setf (vb-valid-bits f) 0)))

(defun %stbv-get32-packet (f)
  ;; NOTE: EOP (-1) bytes are added as in C, the result is truncated to 32 bits
  (let ((x (%stbv-get8-packet f)))
    (setf x (+ x (ash (%stbv-get8-packet f) 8)))
    (setf x (+ x (ash (%stbv-get8-packet f) 16)))
    (%i32 (+ x (ash (%stbv-get8-packet f) 24)))))

(defun %stbv-flush-packet (f)
  (loop until (= (%stbv-get8-packet-raw f) +stbv-eop+)))

;; @OPTIMIZE: this is the secondary bit decoder, so it's probably not as important
;; as the huffman decoder?
(defun %stbv-get-bits (f n)
  (when (< (vb-valid-bits f) 0) (return-from %stbv-get-bits 0))
  (when (< (vb-valid-bits f) n)
    (when (> n 24)
      ;; The accumulator technique below would not work correctly in this case
      (let ((z (%stbv-get-bits f 24)))
        (return-from %stbv-get-bits (logand (+ z (ash (%stbv-get-bits f (- n 24)) 24)) #xffffffff))))
    (when (= (vb-valid-bits f) 0) (setf (vb-acc f) 0))
    (loop while (< (vb-valid-bits f) n)
          do (let ((z (%stbv-get8-packet-raw f)))
               (when (= z +stbv-eop+)
                 (setf (vb-valid-bits f) +stbv-invalid-bits+)
                 (return-from %stbv-get-bits 0))
               (setf (vb-acc f) (logand (+ (vb-acc f) (ash z (vb-valid-bits f))) #xffffffff))
               (incf (vb-valid-bits f) 8))))
  (let ((z (logand (vb-acc f) (- (ash 1 n) 1))))
    (setf (vb-acc f) (ash (vb-acc f) (- n)))
    (decf (vb-valid-bits f) n)
    z))

;; @OPTIMIZE: primary accumulator for huffman
;; expand the buffer to as many bits as possible without reading off end of packet
(defun %stbv-prep-huffman (f)
  (when (<= (vb-valid-bits f) 24)
    (when (= (vb-valid-bits f) 0) (setf (vb-acc f) 0))
    (loop
      (when (and (vb-last-seg f) (= (vb-bytes-in-seg f) 0)) (return))
      (let ((z (%stbv-get8-packet-raw f)))
        ;; If we run out of bits in the packet, pad with zeros
        (when (= z +stbv-eop+) (return))
        (setf (vb-acc f) (logand (+ (vb-acc f) (ash z (vb-valid-bits f))) #xffffffff))
        (incf (vb-valid-bits f) 8))
      (unless (<= (vb-valid-bits f) 24) (return)))))

;;;----------------------------------------------------------------------------------
;;; Huffman decoding
;;;----------------------------------------------------------------------------------

(defun %stbv-codebook-decode-scalar-raw (f c)
  (%stbv-prep-huffman f)
  (when (and (null (cb-codewords c)) (null (cb-sorted-codewords c)))
    (return-from %stbv-codebook-decode-scalar-raw -1))
  ;; Cases to use binary search: sorted_codewords && !c->codewords
  ;;                             sorted_codewords && c->entries > 8
  (when (if (> (cb-entries c) 8) (cb-sorted-codewords c) (null (cb-codewords c)))
    ;; Binary search
    (let ((code (%stbv-bit-reverse (vb-acc f)))
          (x 0)
          (n (cb-sorted-entries c))
          (sorted (cb-sorted-codewords c)))
      (loop while (> n 1)
            do ;; Invariant: sc[x] <= code < sc[x+n]
               (let ((m (+ x (ash n -1))))
                 (if (<= (aref sorted m) code)
                     (setf x m n (- n (ash n -1)))
                     (setf n (ash n -1)))))
      ;; x is now the sorted index
      (when (= (cb-sparse c) 0) (setf x (aref (cb-sorted-values c) (+ x 1))))
      ;; x is now sorted index if sparse, or symbol otherwise
      (let ((len (aref (cb-codeword-lengths c) x)))
        (when (>= (vb-valid-bits f) len)
          (setf (vb-acc f) (ash (vb-acc f) (- len)))
          (decf (vb-valid-bits f) len)
          (return-from %stbv-codebook-decode-scalar-raw x))
        (setf (vb-valid-bits f) 0)
        (return-from %stbv-codebook-decode-scalar-raw -1))))
  ;; If small, linear search
  (dotimes (i (cb-entries c))
    (let ((len (aref (cb-codeword-lengths c) i)))
      (unless (= len +stbv-no-code+)
        (when (= (aref (cb-codewords c) i) (logand (vb-acc f) (- (ash 1 len) 1)))
          (when (>= (vb-valid-bits f) len)
            (setf (vb-acc f) (ash (vb-acc f) (- len)))
            (decf (vb-valid-bits f) len)
            (return-from %stbv-codebook-decode-scalar-raw i))
          (setf (vb-valid-bits f) 0)
          (return-from %stbv-codebook-decode-scalar-raw -1)))))
  (%stbv-error f +vorbis-invalid-stream+)
  (setf (vb-valid-bits f) 0)
  -1)

;; DECODE_RAW
(declaim (inline %stbv-decode-raw))
(defun %stbv-decode-raw (f c)
  (when (< (vb-valid-bits f) +stbv-fast-huffman-length+)
    (%stbv-prep-huffman f))
  (let ((var (aref (cb-fast-huffman c) (logand (vb-acc f) +stbv-fast-huffman-table-mask+))))
    (if (>= var 0)
        (let ((n (aref (cb-codeword-lengths c) var)))
          (setf (vb-acc f) (ash (vb-acc f) (- n)))
          (decf (vb-valid-bits f) n)
          (when (< (vb-valid-bits f) 0)
            (setf (vb-valid-bits f) 0 var -1))
          var)
        (%stbv-codebook-decode-scalar-raw f c))))

;; DECODE
(defun %stbv-decode (f c)
  (let ((var (%stbv-decode-raw f c)))
    (if (/= (cb-sparse c) 0)
        (aref (cb-sorted-values c) (+ var 1))
        var)))

(defun %stbv-codebook-decode-start (f c)
  (let ((z -1))
    ;; Type 0 is only legal in a scalar context
    (if (= (cb-lookup-type c) 0)
        (%stbv-error f +vorbis-invalid-stream+)
        (progn
          (setf z (%stbv-decode-raw f c))
          (when (< z 0)                 ; Check for EOP
            (when (and (= (vb-bytes-in-seg f) 0) (vb-last-seg f))
              (return-from %stbv-codebook-decode-start z))
            (%stbv-error f +vorbis-invalid-stream+))))
    z))

(defun %stbv-codebook-decode (f c output offset len)
  (declare (type %f32-array output) (type fixnum offset len))
  (let ((z (%stbv-codebook-decode-start f c))
        (mult (cb-multiplicands c)))
    (declare (type %f32-array mult))
    (when (< z 0) (return-from %stbv-codebook-decode nil))
    (when (> len (cb-dimensions c)) (setf len (cb-dimensions c)))
    (setf z (* z (cb-dimensions c)))
    (if (/= (cb-sequence-p c) 0)
        (let ((last 0f0))
          (declare (type single-float last))
          (dotimes (i len)
            (let ((val (+ (aref mult (+ z i)) last)))
              (setf (aref output (+ offset i)) (+ (aref output (+ offset i)) val))
              (setf last (+ val (cb-minimum-value c))))))
        (let ((last 0f0))
          (dotimes (i len)
            (setf (aref output (+ offset i)) (+ (aref output (+ offset i)) (+ (aref mult (+ z i)) last))))))
    t))

(defun %stbv-codebook-decode-step (f c output offset len step)
  (declare (type %f32-array output) (type fixnum offset len step))
  (let ((z (%stbv-codebook-decode-start f c))
        (last 0f0)
        (mult (cb-multiplicands c)))
    (declare (type single-float last) (type %f32-array mult))
    (when (< z 0) (return-from %stbv-codebook-decode-step nil))
    (when (> len (cb-dimensions c)) (setf len (cb-dimensions c)))
    (setf z (* z (cb-dimensions c)))
    (dotimes (i len)
      (let ((val (+ (aref mult (+ z i)) last)))
        (setf (aref output (+ offset (* i step))) (+ (aref output (+ offset (* i step))) val))
        (when (/= (cb-sequence-p c) 0) (setf last val))))
    t))

;; Returns (values ok c-inter p-inter)
(defun %stbv-codebook-decode-deinterleave-repeat (f c outputs ch c-inter p-inter len total-decode)
  (declare (type fixnum ch c-inter p-inter len total-decode))
  (let ((effective (cb-dimensions c))
        (mult (cb-multiplicands c)))
    (declare (type fixnum effective))
    ;; Type 0 is only legal in a scalar context
    (when (= (cb-lookup-type c) 0)
      (return-from %stbv-codebook-decode-deinterleave-repeat
        (values (%stbv-error f +vorbis-invalid-stream+) c-inter p-inter)))
    (loop while (> total-decode 0)
          do (let ((last 0f0)
                   (z (%stbv-decode-raw f c)))
               (declare (type single-float last) (type fixnum z))
               (when (< z 0)
                 (when (and (= (vb-bytes-in-seg f) 0) (vb-last-seg f))
                   (return-from %stbv-codebook-decode-deinterleave-repeat (values nil c-inter p-inter)))
                 (return-from %stbv-codebook-decode-deinterleave-repeat
                   (values (%stbv-error f +vorbis-invalid-stream+) c-inter p-inter)))
               ;; If this will take us off the end of the buffers, stop short!
               ;; We check by computing the length of the virtual interleaved
               ;; buffer (len*ch), our current offset within it (p_inter*ch)+(c_inter),
               ;; and the length we'll be using (effective)
               (when (> (+ c-inter (* p-inter ch) effective) (* len ch))
                 (setf effective (- (* len ch) (- (* p-inter ch) c-inter))))
               (setf z (* z (cb-dimensions c)))
               (dotimes (i effective)
                 (let ((val (+ (aref mult (+ z i)) last))
                       (out (svref outputs c-inter)))
                   (declare (type single-float val))
                   (when out
                     (setf (aref out p-inter) (+ (aref out p-inter) val)))
                   (when (= (incf c-inter) ch) (setf c-inter 0) (incf p-inter))
                   (when (/= (cb-sequence-p c) 0) (setf last val))))
               (decf total-decode effective)))
    (values t c-inter p-inter)))

(defun %stbv-predict-point (x x0 x1 y0 y1)
  (let* ((dy (- y1 y0))
         (adx (- x1 x0))
         ;; @OPTIMIZE: force int division to round in the right direction... is this necessary on x86?
         (err (* (abs dy) (- x x0)))
         (off (truncate err adx)))
    (if (< dy 0) (- y0 off) (+ y0 off))))

;; The following table is block-copied from the specification
(alexandria:define-constant +stbv-inverse-db-table+
  (make-array 256 :element-type 'single-float :initial-contents '(
    1.0649863f-07 1.1341951f-07 1.2079015f-07 1.2863978f-07 1.3699951f-07 1.4590251f-07 1.5538408f-07 
    1.6548181f-07 1.7623575f-07 1.8768855f-07 1.9988561f-07 2.1287530f-07 2.2670913f-07 2.4144197f-07 
    2.5713223f-07 2.7384213f-07 2.9163793f-07 3.1059021f-07 3.3077411f-07 3.5226968f-07 3.7516214f-07 
    3.9954229f-07 4.2550680f-07 4.5315863f-07 4.8260743f-07 5.1396998f-07 5.4737065f-07 5.8294187f-07 
    6.2082472f-07 6.6116941f-07 7.0413592f-07 7.4989464f-07 7.9862701f-07 8.5052630f-07 9.0579828f-07 
    9.6466216f-07 1.0273513f-06 1.0941144f-06 1.1652161f-06 1.2409384f-06 1.3215816f-06 1.4074654f-06 
    1.4989305f-06 1.5963394f-06 1.7000785f-06 1.8105592f-06 1.9282195f-06 2.0535261f-06 2.1869758f-06 
    2.3290978f-06 2.4804557f-06 2.6416497f-06 2.8133190f-06 2.9961443f-06 3.1908506f-06 3.3982101f-06 
    3.6190449f-06 3.8542308f-06 4.1047004f-06 4.3714470f-06 4.6555282f-06 4.9580707f-06 5.2802740f-06 
    5.6234160f-06 5.9888572f-06 6.3780469f-06 6.7925283f-06 7.2339451f-06 7.7040476f-06 8.2047000f-06 
    8.7378876f-06 9.3057248f-06 9.9104632f-06 1.0554501f-05 1.1240392f-05 1.1970856f-05 1.2748789f-05 
    1.3577278f-05 1.4459606f-05 1.5399272f-05 1.6400004f-05 1.7465768f-05 1.8600792f-05 1.9809576f-05 
    2.1096914f-05 2.2467911f-05 2.3928002f-05 2.5482978f-05 2.7139006f-05 2.8902651f-05 3.0780908f-05 
    3.2781225f-05 3.4911534f-05 3.7180282f-05 3.9596466f-05 4.2169667f-05 4.4910090f-05 4.7828601f-05 
    5.0936773f-05 5.4246931f-05 5.7772202f-05 6.1526565f-05 6.5524908f-05 6.9783085f-05 7.4317983f-05 
    7.9147585f-05 8.4291040f-05 8.9768747f-05 9.5602426f-05 0.00010181521f0 0.00010843174f0 
    0.00011547824f0 0.00012298267f0 0.00013097477f0 0.00013948625f0 0.00014855085f0 0.00015820453f0 
    0.00016848555f0 0.00017943469f0 0.00019109536f0 0.00020351382f0 0.00021673929f0 0.00023082423f0 
    0.00024582449f0 0.00026179955f0 0.00027881276f0 0.00029693158f0 0.00031622787f0 0.00033677814f0 
    0.00035866388f0 0.00038197188f0 0.00040679456f0 0.00043323036f0 0.00046138411f0 0.00049136745f0 
    0.00052329927f0 0.00055730621f0 0.00059352311f0 0.00063209358f0 0.00067317058f0 0.00071691700f0 
    0.00076350630f0 0.00081312324f0 0.00086596457f0 0.00092223983f0 0.00098217216f0 0.0010459992f0 
    0.0011139742f0 0.0011863665f0 0.0012634633f0 0.0013455702f0 0.0014330129f0 0.0015261382f0 
    0.0016253153f0 0.0017309374f0 0.0018434235f0 0.0019632195f0 0.0020908006f0 0.0022266726f0 
    0.0023713743f0 0.0025254795f0 0.0026895994f0 0.0028643847f0 0.0030505286f0 0.0032487691f0 
    0.0034598925f0 0.0036847358f0 0.0039241906f0 0.0041792066f0 0.0044507950f0 0.0047400328f0 
    0.0050480668f0 0.0053761186f0 0.0057254891f0 0.0060975636f0 0.0064938176f0 0.0069158225f0 
    0.0073652516f0 0.0078438871f0 0.0083536271f0 0.0088964928f0 0.009474637f0 0.010090352f0 
    0.010746080f0 0.011444421f0 0.012188144f0 0.012980198f0 0.013823725f0 0.014722068f0 0.015678791f0 
    0.016697687f0 0.017782797f0 0.018938423f0 0.020169149f0 0.021479854f0 0.022875735f0 0.024362330f0 
    0.025945531f0 0.027631618f0 0.029427276f0 0.031339626f0 0.033376252f0 0.035545228f0 0.037855157f0 
    0.040315199f0 0.042935108f0 0.045725273f0 0.048696758f0 0.051861348f0 0.055231591f0 0.058820850f0 
    0.062643361f0 0.066714279f0 0.071049749f0 0.075666962f0 0.080584227f0 0.085821044f0 0.091398179f0 
    0.097337747f0 0.10366330f0 0.11039993f0 0.11757434f0 0.12521498f0 0.13335215f0 0.14201813f0 
    0.15124727f0 0.16107617f0 0.17154380f0 0.18269168f0 0.19456402f0 0.20720788f0 0.22067342f0 
    0.23501402f0 0.25028656f0 0.26655159f0 0.28387361f0 0.30232132f0 0.32196786f0 0.34289114f0 
    0.36517414f0 0.38890521f0 0.41417847f0 0.44109412f0 0.46975890f0 0.50028648f0 0.53279791f0 
    0.56742212f0 0.60429640f0 0.64356699f0 0.68538959f0 0.72993007f0 0.77736504f0 0.82788260f0 
    0.88168307f0 0.9389798f0 1.0f0))
  :test #'equalp)

;; Deferred floor: LINE_OP(a,b) a *= b
(defun %stbv-draw-line (output x0 y0 x1 y1 n)
  (declare (type %f32-array output) (type fixnum x0 y0 x1 y1 n))
  (let* ((dy (- y1 y0))
         (adx (- x1 x0))
         (ady (abs dy))
         (base (truncate dy adx))
         (sy (if (< dy 0) (- base 1) (+ base 1)))
         (x x0) (y y0) (err 0)
         (table +stbv-inverse-db-table+))
    (declare (type fixnum dy adx ady base sy x y err)
             (type %f32-array table))
    (setf ady (- ady (* (abs base) adx)))
    (when (> x1 n) (setf x1 n))
    (when (< x x1)
      (setf (aref output x) (* (aref output x) (aref table (logand y 255))))
      (loop for xx from (+ x 1) below x1
            do (incf err ady)
               (if (>= err adx)
                   (progn (decf err adx) (incf y sy))
                   (incf y base))
               (setf (aref output xx) (* (aref output xx) (aref table (logand y 255))))))))

(defun %stbv-residue-decode (f book target offset n rtype)
  (if (= rtype 0)
      (let ((step (truncate n (cb-dimensions book))))
        (dotimes (k step)
          (unless (%stbv-codebook-decode-step f book target (+ offset k) (- n offset k) step)
            (return-from %stbv-residue-decode nil))))
      (let ((k 0))
        (loop while (< k n)
              do (unless (%stbv-codebook-decode f book target offset (- n k))
                   (return-from %stbv-residue-decode nil))
                 (incf k (cb-dimensions book))
                 (incf offset (cb-dimensions book)))))
  t)

;; n is 1/2 of the blocksize --
;; specification: "Correct per-vector decode length is [n]/2"
(defun %stbv-decode-residue (f residue-buffers ch n rn do-not-decode)
  (let* ((r (aref (vb-residue-config f) rn))
         (rtype (aref (vb-residue-types f) rn))
         (classbook (aref (vb-codebooks f) (rs-classbook r)))
         (classwords (cb-dimensions classbook))
         (actual-size (if (= rtype 2) (* n 2) n))
         (limit-r-begin (min (rs-begin r) actual-size))
         (limit-r-end (min (rs-end r) actual-size))
         (n-read (- limit-r-end limit-r-begin))
         (part-read (truncate n-read (rs-part-size r)))
         (part-classdata (let ((v (make-array (vb-channels f))))
                           (dotimes (i (vb-channels f) v)
                             (setf (svref v i) (make-array (max part-read 0) :initial-element nil))))))
    (dotimes (i ch)
      (unless (aref do-not-decode i)
        (fill (svref residue-buffers i) 0f0 :end n)))
    (block done
      (when (and (= rtype 2) (/= ch 1))
        (unless (position nil do-not-decode :end ch)
          (return-from done))
        (dotimes (pass 8)
          (let ((pcount 0) (class-set 0))
            (loop while (< pcount part-read)
                  do (let* ((z (+ (rs-begin r) (* pcount (rs-part-size r))))
                            (c-inter (if (= ch 2) (logand z 1) (rem z ch)))
                            (p-inter (if (= ch 2) (ash z -1) (truncate z ch))))
                       (when (= pass 0)
                         (let ((q (%stbv-decode f classbook)))
                           (when (= q +stbv-eop+) (return-from done))
                           (setf (aref (svref part-classdata 0) class-set) (aref (rs-classdata r) q))))
                       (loop for i from 0
                             while (and (< i classwords) (< pcount part-read))
                             do (let* ((z (+ (rs-begin r) (* pcount (rs-part-size r))))
                                       (c (aref (aref (svref part-classdata 0) class-set) i))
                                       (b (aref (rs-residue-books r) c pass)))
                                  (if (>= b 0)
                                      (multiple-value-bind (ok ci pi2)
                                          (%stbv-codebook-decode-deinterleave-repeat
                                           f (aref (vb-codebooks f) b) residue-buffers ch c-inter p-inter n (rs-part-size r))
                                        (setf c-inter ci p-inter pi2)
                                        (unless ok (return-from done)))
                                      (progn
                                        (incf z (rs-part-size r))
                                        (setf c-inter (if (= ch 2) (logand z 1) (rem z ch))
                                              p-inter (if (= ch 2) (ash z -1) (truncate z ch))))))
                                (incf pcount))
                       (incf class-set)))))
        (return-from done))
      (dotimes (pass 8)
        (let ((pcount 0) (class-set 0))
          (loop while (< pcount part-read)
                do (when (= pass 0)
                     (dotimes (j ch)
                       (unless (aref do-not-decode j)
                         (let ((temp (%stbv-decode f classbook)))
                           (when (= temp +stbv-eop+) (return-from done))
                           (setf (aref (svref part-classdata j) class-set) (aref (rs-classdata r) temp))))))
                   (loop for i from 0
                         while (and (< i classwords) (< pcount part-read))
                         do (dotimes (j ch)
                              (unless (aref do-not-decode j)
                                (let* ((c (aref (aref (svref part-classdata j) class-set) i))
                                       (b (aref (rs-residue-books r) c pass)))
                                  (when (>= b 0)
                                    (unless (%stbv-residue-decode f (aref (vb-codebooks f) b) (svref residue-buffers j)
                                                                  (+ (rs-begin r) (* pcount (rs-part-size r)))
                                                                  (rs-part-size r) rtype)
                                      (return-from done))))))
                            (incf pcount))
                   (incf class-set)))))))

;;;----------------------------------------------------------------------------------
;;; Inverse MDCT
;;;----------------------------------------------------------------------------------

;; imdct_step3_iter0_loop() and imdct_step3_inner_r_loop(): COUNT iterations of 4 butterflies,
;; A advances by ASTEP after each butterfly
(defun %stbv-imdct-step3-r-loop (count e d0 k-off a astep)
  (declare (type %f32-array e a) (type fixnum count d0 k-off astep)
           (optimize speed (safety 0)))
  (let ((e0 d0)
        (e2 (+ d0 k-off))
        (ai 0))
    (declare (type fixnum e0 e2 ai))
    (loop repeat count
          do (loop for m of-type fixnum from 0 below 8 by 2
                   do (let ((k00-20 (- (aref e (- e0 m)) (aref e (- e2 m))))
                            (k01-21 (- (aref e (- e0 m 1)) (aref e (- e2 m 1)))))
                        (setf (aref e (- e0 m)) (+ (aref e (- e0 m)) (aref e (- e2 m)))
                              (aref e (- e0 m 1)) (+ (aref e (- e0 m 1)) (aref e (- e2 m 1))))
                        (setf (aref e (- e2 m)) (- (* k00-20 (aref a ai)) (* k01-21 (aref a (+ ai 1))))
                              (aref e (- e2 m 1)) (+ (* k01-21 (aref a ai)) (* k00-20 (aref a (+ ai 1)))))
                        (incf ai astep)))
             (decf e0 8)
             (decf e2 8))))

(defun %stbv-imdct-step3-inner-s-loop (n e i-off k-off a a-start a-off k0)
  (declare (type %f32-array e a) (type fixnum n i-off k-off a-start a-off k0)
           (optimize speed (safety 0)))
  (let ((ee0 i-off)
        (ee2 (+ i-off k-off))
        (aa (make-array 8 :element-type 'single-float)))
    (declare (type fixnum ee0 ee2) (type (simple-array single-float (8)) aa))
    (setf (aref aa 0) (aref a a-start)
          (aref aa 1) (aref a (+ a-start 1))
          (aref aa 2) (aref a (+ a-start a-off))
          (aref aa 3) (aref a (+ a-start a-off 1))
          (aref aa 4) (aref a (+ a-start (* a-off 2)))
          (aref aa 5) (aref a (+ a-start (* a-off 2) 1))
          (aref aa 6) (aref a (+ a-start (* a-off 3)))
          (aref aa 7) (aref a (+ a-start (* a-off 3) 1)))
    (loop repeat n
          do (loop for m of-type fixnum from 0 below 8 by 2
                   do (let ((k00 (- (aref e (- ee0 m)) (aref e (- ee2 m))))
                            (k11 (- (aref e (- ee0 m 1)) (aref e (- ee2 m 1)))))
                        (setf (aref e (- ee0 m)) (+ (aref e (- ee0 m)) (aref e (- ee2 m)))
                              (aref e (- ee0 m 1)) (+ (aref e (- ee0 m 1)) (aref e (- ee2 m 1))))
                        (setf (aref e (- ee2 m)) (- (* k00 (aref aa m)) (* k11 (aref aa (+ m 1))))
                              (aref e (- ee2 m 1)) (+ (* k11 (aref aa m)) (* k00 (aref aa (+ m 1)))))))
             (decf ee0 k0)
             (decf ee2 k0))))

(declaim (inline %stbv-iter-54))
(defun %stbv-iter-54 (e z)
  (declare (type %f32-array e) (type fixnum z) (optimize speed (safety 0)))
  (let* ((k00 (- (aref e z) (aref e (- z 4))))
         (y0 (+ (aref e z) (aref e (- z 4))))
         (y2 (+ (aref e (- z 2)) (aref e (- z 6))))
         (k22 (- (aref e (- z 2)) (aref e (- z 6)))))
    (setf (aref e z) (+ y0 y2)
          (aref e (- z 2)) (- y0 y2))
    (let ((k33 (- (aref e (- z 3)) (aref e (- z 7)))))
      (setf (aref e (- z 4)) (+ k00 k33)
            (aref e (- z 6)) (- k00 k33)))
    (let ((k11 (- (aref e (- z 1)) (aref e (- z 5))))
          (y1 (+ (aref e (- z 1)) (aref e (- z 5))))
          (y3 (+ (aref e (- z 3)) (aref e (- z 7)))))
      (setf (aref e (- z 1)) (+ y1 y3)
            (aref e (- z 3)) (- y1 y3)
            (aref e (- z 5)) (- k11 k22)
            (aref e (- z 7)) (+ k11 k22)))))

(defun %stbv-imdct-step3-inner-s-loop-ld654 (n e i-off a base-n)
  (declare (type %f32-array e a) (type fixnum n i-off base-n) (optimize speed (safety 0)))
  (let* ((a-off (ash base-n -3))
         (a2 (aref a a-off))
         (z i-off)
         (base (- z (* 16 n))))
    (declare (type fixnum a-off z base) (type single-float a2))
    (loop while (> z base)
          do (let ((k00 (- (aref e z) (aref e (- z 8))))
                   (k11 (- (aref e (- z 1)) (aref e (- z 9))))
                   (l00 (- (aref e (- z 2)) (aref e (- z 10))))
                   (l11 (- (aref e (- z 3)) (aref e (- z 11)))))
               (setf (aref e z) (+ (aref e z) (aref e (- z 8)))
                     (aref e (- z 1)) (+ (aref e (- z 1)) (aref e (- z 9)))
                     (aref e (- z 2)) (+ (aref e (- z 2)) (aref e (- z 10)))
                     (aref e (- z 3)) (+ (aref e (- z 3)) (aref e (- z 11))))
               (setf (aref e (- z 8)) k00
                     (aref e (- z 9)) k11
                     (aref e (- z 10)) (* (+ l00 l11) a2)
                     (aref e (- z 11)) (* (- l11 l00) a2)))
             (let ((k00 (- (aref e (- z 4)) (aref e (- z 12))))
                   (k11 (- (aref e (- z 5)) (aref e (- z 13))))
                   (l00 (- (aref e (- z 6)) (aref e (- z 14))))
                   (l11 (- (aref e (- z 7)) (aref e (- z 15)))))
               (setf (aref e (- z 4)) (+ (aref e (- z 4)) (aref e (- z 12)))
                     (aref e (- z 5)) (+ (aref e (- z 5)) (aref e (- z 13)))
                     (aref e (- z 6)) (+ (aref e (- z 6)) (aref e (- z 14)))
                     (aref e (- z 7)) (+ (aref e (- z 7)) (aref e (- z 15))))
               (setf (aref e (- z 12)) k11
                     (aref e (- z 13)) (- k00)
                     (aref e (- z 14)) (* (- l11 l00) a2)
                     (aref e (- z 15)) (* (+ l00 l11) (- a2))))
             (%stbv-iter-54 e z)
             (%stbv-iter-54 e (- z 8))
             (decf z 16))))

(defun %stbv-inverse-mdct (buffer n f blocktype)
  (declare (type %f32-array buffer) (type fixnum n) (optimize speed (safety 0)))
  (let* ((n2 (ash n -1)) (n4 (ash n -2)) (n8 (ash n -3))
         (buf2 (vb-imdct-buf f))
         (a (aref (vb-a f) blocktype))
         (u buffer)
         (v buf2))
    (declare (type fixnum n2 n4 n8) (type %f32-array buf2 a u v))
    ;; IMDCT algorithm from "The use of multirate filter banks for coding of high quality digital audio"
    ;; by Th. Sporer, K. Brandenburg and B. Edler, collectively hereafter "the paper"
    ;; kernel from paper
    ;; merged:
    ;;   copy and reflect spectral data
    ;;   step 0
    (let ((d (- n2 2)) (aa 0) (e 0))
      (declare (type fixnum d aa e))
      (loop until (= e n2)
            do (setf (aref buf2 (+ d 1)) (- (* (aref buffer e) (aref a aa)) (* (aref buffer (+ e 2)) (aref a (+ aa 1))))
                     (aref buf2 d) (+ (* (aref buffer e) (aref a (+ aa 1))) (* (aref buffer (+ e 2)) (aref a aa))))
               (decf d 2) (incf aa 2) (incf e 4))
      (setf e (- n2 3))
      (loop while (>= d 0)
            do (setf (aref buf2 (+ d 1)) (- (* (- (aref buffer (+ e 2))) (aref a aa)) (* (- (aref buffer e)) (aref a (+ aa 1))))
                     (aref buf2 d) (+ (* (- (aref buffer (+ e 2))) (aref a (+ aa 1))) (* (- (aref buffer e)) (aref a aa))))
               (decf d 2) (incf aa 2) (decf e 4)))
    ;; step 2
    (let ((aa (- n2 8)) (e0 n4) (e1 0) (d0 n4) (d1 0))
      (declare (type fixnum aa e0 e1 d0 d1))
      (loop while (>= aa 0)
            do (let ((v41-21 (- (aref v (+ e0 1)) (aref v (+ e1 1))))
                     (v40-20 (- (aref v e0) (aref v e1))))
                 (setf (aref u (+ d0 1)) (+ (aref v (+ e0 1)) (aref v (+ e1 1)))
                       (aref u d0) (+ (aref v e0) (aref v e1)))
                 (setf (aref u (+ d1 1)) (- (* v41-21 (aref a (+ aa 4))) (* v40-20 (aref a (+ aa 5))))
                       (aref u d1) (+ (* v40-20 (aref a (+ aa 4))) (* v41-21 (aref a (+ aa 5))))))
               (let ((v41-21 (- (aref v (+ e0 3)) (aref v (+ e1 3))))
                     (v40-20 (- (aref v (+ e0 2)) (aref v (+ e1 2)))))
                 (setf (aref u (+ d0 3)) (+ (aref v (+ e0 3)) (aref v (+ e1 3)))
                       (aref u (+ d0 2)) (+ (aref v (+ e0 2)) (aref v (+ e1 2))))
                 (setf (aref u (+ d1 3)) (- (* v41-21 (aref a aa)) (* v40-20 (aref a (+ aa 1))))
                       (aref u (+ d1 2)) (+ (* v40-20 (aref a aa)) (* v41-21 (aref a (+ aa 1))))))
               (decf aa 8)
               (incf d0 4) (incf d1 4) (incf e0 4) (incf e1 4)))
    ;; step 3
    (let ((ld (- (%stbv-ilog n) 1)))   ; ilog is off-by-one from normal definitions
      (declare (type fixnum ld))
      ;; optimized step 3:
      ;; the original step3 loop can be nested r inside s or s inside r;
      ;; it's written originally as s inside r, but this is dumb when r
      ;; iterates many times, and s few. So I have two copies of it and
      ;; switch between them halfway.
      ;; this is iteration 0 of step 3
      (%stbv-imdct-step3-r-loop (ash (ash n -4) -2) u (- n2 1 (* n4 0)) (- (ash n -3)) a 8)
      (%stbv-imdct-step3-r-loop (ash (ash n -4) -2) u (- n2 1 (* n4 1)) (- (ash n -3)) a 8)
      ;; this is iteration 1 of step 3
      (%stbv-imdct-step3-r-loop (ash (ash n -5) -2) u (- n2 1 (* n8 0)) (- (ash n -4)) a 16)
      (%stbv-imdct-step3-r-loop (ash (ash n -5) -2) u (- n2 1 (* n8 1)) (- (ash n -4)) a 16)
      (%stbv-imdct-step3-r-loop (ash (ash n -5) -2) u (- n2 1 (* n8 2)) (- (ash n -4)) a 16)
      (%stbv-imdct-step3-r-loop (ash (ash n -5) -2) u (- n2 1 (* n8 3)) (- (ash n -4)) a 16)
      (let ((l 2))
        (declare (type fixnum l))
        (loop while (< l (ash (- ld 3) -1))
              do (let* ((k0 (ash n (- (+ l 2))))
                        (k0-2 (ash k0 -1))
                        (lim (ash 1 (+ l 1))))
                   (dotimes (i lim)
                     (%stbv-imdct-step3-r-loop (ash (ash n (- (+ l 4))) -2) u (- n2 1 (* k0 i)) (- k0-2) a (ash 1 (+ l 3)))))
                 (incf l))
        (loop while (< l (- ld 6))
              do (let* ((k0 (ash n (- (+ l 2))))
                        (k1 (ash 1 (+ l 3)))
                        (k0-2 (ash k0 -1))
                        (rlim (ash n (- (+ l 6))))
                        (lim (ash 1 (+ l 1)))
                        (a0 0)
                        (i-off (- n2 1)))
                   (declare (type fixnum a0 i-off))
                   (loop repeat rlim
                         do (%stbv-imdct-step3-inner-s-loop lim u i-off (- k0-2) a a0 k1 k0)
                            (incf a0 (* k1 4))
                            (decf i-off 8)))
                 (incf l)))
      ;; iterations with count:
      ;;   ld-6,-5,-4 all interleaved together
      ;;       the big win comes from getting rid of needless flops
      ;;         due to the constants on pass 5 & 4 being all 1 and 0;
      ;;       combining them to be simultaneous to improve cache made little difference
      (%stbv-imdct-step3-inner-s-loop-ld654 (ash n -5) u (- n2 1) a n))
    ;; output is u
    ;; step 4, 5, and 6
    ;; cannot be in-place because of step 5
    (let ((bitrev (aref (vb-bit-reverse f) blocktype))
          (br 0)
          (d0 (- n4 4))
          (d1 (- n2 4)))
      (declare (type (simple-array (unsigned-byte 16) (*)) bitrev) (type fixnum br d0 d1))
      (loop while (>= d0 0)
            do (let ((k4 (aref bitrev br)))
                 (setf (aref v (+ d1 3)) (aref u k4)
                       (aref v (+ d1 2)) (aref u (+ k4 1))
                       (aref v (+ d0 3)) (aref u (+ k4 2))
                       (aref v (+ d0 2)) (aref u (+ k4 3))))
               (let ((k4 (aref bitrev (+ br 1))))
                 (setf (aref v (+ d1 1)) (aref u k4)
                       (aref v d1) (aref u (+ k4 1))
                       (aref v (+ d0 1)) (aref u (+ k4 2))
                       (aref v d0) (aref u (+ k4 3))))
               (decf d0 4) (decf d1 4) (incf br 2)))
    ;; (paper output is u, now v)
    ;; step 7   (paper output is v, now v)
    ;; this is now in place
    (let ((c (aref (vb-c f) blocktype))
          (ci 0) (d 0) (e (- n2 4)))
      (declare (type %f32-array c) (type fixnum ci d e))
      (loop while (< d e)
            do (let* ((a02 (- (aref v d) (aref v (+ e 2))))
                      (a11 (+ (aref v (+ d 1)) (aref v (+ e 3))))
                      (b0 (+ (* (aref c (+ ci 1)) a02) (* (aref c ci) a11)))
                      (b1 (- (* (aref c (+ ci 1)) a11) (* (aref c ci) a02)))
                      (b2 (+ (aref v d) (aref v (+ e 2))))
                      (b3 (- (aref v (+ d 1)) (aref v (+ e 3)))))
                 (setf (aref v d) (+ b2 b0)
                       (aref v (+ d 1)) (+ b3 b1)
                       (aref v (+ e 2)) (- b2 b0)
                       (aref v (+ e 3)) (- b1 b3)))
               (let* ((a02 (- (aref v (+ d 2)) (aref v e)))
                      (a11 (+ (aref v (+ d 3)) (aref v (+ e 1))))
                      (b0 (+ (* (aref c (+ ci 3)) a02) (* (aref c (+ ci 2)) a11)))
                      (b1 (- (* (aref c (+ ci 3)) a11) (* (aref c (+ ci 2)) a02)))
                      (b2 (+ (aref v (+ d 2)) (aref v e)))
                      (b3 (- (aref v (+ d 3)) (aref v (+ e 1)))))
                 (setf (aref v (+ d 2)) (+ b2 b0)
                       (aref v (+ d 3)) (+ b3 b1)
                       (aref v e) (- b2 b0)
                       (aref v (+ e 1)) (- b1 b3)))
               (incf ci 4) (incf d 4) (decf e 4)))
    ;; data must be in buf2
    ;; step 8+decode   (paper output is X, now buffer)
    ;; this generates pairs of data a la 8 and pushes them directly through
    ;; the decode kernel (pushing rather than pulling) to avoid having
    ;; to make another pass later
    (let ((b (aref (vb-b f) blocktype))
          (bi (- n2 8))
          (e (- n2 8))
          (d0 0) (d1 (- n2 4)) (d2 n2) (d3 (- n 4)))
      (declare (type %f32-array b) (type fixnum bi e d0 d1 d2 d3))
      (loop while (>= e 0)
            do (let ((p3 (- (* (aref buf2 (+ e 6)) (aref b (+ bi 7))) (* (aref buf2 (+ e 7)) (aref b (+ bi 6)))))
                     (p2 (- (* (- (aref buf2 (+ e 6))) (aref b (+ bi 6))) (* (aref buf2 (+ e 7)) (aref b (+ bi 7))))))
                 (setf (aref buffer d0) p3
                       (aref buffer (+ d1 3)) (- p3)
                       (aref buffer d2) p2
                       (aref buffer (+ d3 3)) p2))
               (let ((p1 (- (* (aref buf2 (+ e 4)) (aref b (+ bi 5))) (* (aref buf2 (+ e 5)) (aref b (+ bi 4)))))
                     (p0 (- (* (- (aref buf2 (+ e 4))) (aref b (+ bi 4))) (* (aref buf2 (+ e 5)) (aref b (+ bi 5))))))
                 (setf (aref buffer (+ d0 1)) p1
                       (aref buffer (+ d1 2)) (- p1)
                       (aref buffer (+ d2 1)) p0
                       (aref buffer (+ d3 2)) p0))
               (let ((p3 (- (* (aref buf2 (+ e 2)) (aref b (+ bi 3))) (* (aref buf2 (+ e 3)) (aref b (+ bi 2)))))
                     (p2 (- (* (- (aref buf2 (+ e 2))) (aref b (+ bi 2))) (* (aref buf2 (+ e 3)) (aref b (+ bi 3))))))
                 (setf (aref buffer (+ d0 2)) p3
                       (aref buffer (+ d1 1)) (- p3)
                       (aref buffer (+ d2 2)) p2
                       (aref buffer (+ d3 1)) p2))
               (let ((p1 (- (* (aref buf2 e) (aref b (+ bi 1))) (* (aref buf2 (+ e 1)) (aref b bi))))
                     (p0 (- (* (- (aref buf2 e)) (aref b bi)) (* (aref buf2 (+ e 1)) (aref b (+ bi 1))))))
                 (setf (aref buffer (+ d0 3)) p1
                       (aref buffer d1) (- p1)
                       (aref buffer (+ d2 3)) p0
                       (aref buffer d3) p0))
               (decf bi 8) (decf e 8)
               (incf d0 4) (incf d2 4) (decf d1 4) (decf d3 4)))))

;;;----------------------------------------------------------------------------------
;;; Packet decoding
;;;----------------------------------------------------------------------------------

(defun %stbv-get-window (f len)
  (setf len (ash len 1))
  (cond ((= len (vb-blocksize-0 f)) (aref (vb-window f) 0))
        ((= len (vb-blocksize-1 f)) (aref (vb-window f) 1))
        (t nil)))

(defun %stbv-do-floor (f map i n target final-y)
  (let* ((n2 (ash n -1))
         (s (aref (mp-chan-mux map) i))
         (floor (aref (mp-submap-floor map) s)))
    (if (= (aref (vb-floor-types f) floor) 0)
        (%stbv-error f +vorbis-invalid-stream+)
        (let* ((g (aref (vb-floor-config f) floor))
               (lx 0)
               (ly (* (aref final-y 0) (fl-floor1-multiplier g))))
          (loop for q from 1 below (fl-values g)
                do (let ((j (aref (fl-sorted-order g) q)))
                     (when (>= (aref final-y j) 0)
                       (let ((hy (* (aref final-y j) (fl-floor1-multiplier g)))
                             (hx (aref (fl-xlist g) j)))
                         (when (/= lx hx)
                           (%stbv-draw-line target lx ly hx hy n2))
                         (setf lx hx ly hy)))))
          (when (< lx n2)
            ;; Optimization of: draw_line(target, lx,ly, n,ly);
            (loop for j from lx below n2
                  do (setf (aref target j) (* (aref target j) (aref +stbv-inverse-db-table+ ly)))))
          t))))

;; Returns (values ok left-start left-end right-start right-end mode)
(defun %stbv-vorbis-decode-initial (f)
  (setf (vb-channel-buffer-start f) 0
        (vb-channel-buffer-end f) 0)
  (loop
    (when (vb-eof f) (return-from %stbv-vorbis-decode-initial nil))
    (unless (%stbv-maybe-start-packet f)
      (return-from %stbv-vorbis-decode-initial nil))
    ;; Check packet type
    (if (/= (%stbv-get-bits f 1) 0)
        (loop until (= +stbv-eop+ (%stbv-get8-packet f)))
        (return)))
  (let ((i (%stbv-get-bits f (%stbv-ilog (- (vb-mode-count f) 1)))))
    (when (>= i (vb-mode-count f)) (return-from %stbv-vorbis-decode-initial nil))
    (let* ((m (aref (vb-mode-config f) i))
           (blockflag (/= (md-blockflag m) 0))
           (n (if blockflag (vb-blocksize-1 f) (vb-blocksize-0 f)))
           (prev 0) (next 0))
      (when blockflag
        (setf prev (%stbv-get-bits f 1)
              next (%stbv-get-bits f 1)))
      (let ((window-center (ash n -1))
            (b0 (vb-blocksize-0 f)))
        (multiple-value-bind (left-start left-end)
            (if (and blockflag (= prev 0))
                (values (ash (- n b0) -2) (ash (+ n b0) -2))
                (values 0 window-center))
          (multiple-value-bind (right-start right-end)
              (if (and blockflag (= next 0))
                  (values (ash (- (* n 3) b0) -2) (ash (+ (* n 3) b0) -2))
                  (values window-center n))
            (values t left-start left-end right-start right-end i)))))))

;; Returns (values ok len left-start)
(defun %stbv-vorbis-decode-packet-rest (f m left-start left-end right-start right-end)
  (declare (ignore left-end))
  (let* ((n (aref (vb-blocksize f) (md-blockflag m)))
         (map (aref (vb-mapping f) (md-mapping m)))
         (n2 (ash n -1))
         (channels (vb-channels f))
         (zero-channel (make-array 256 :initial-element nil))
         (really-zero-channel (make-array 256 :initial-element nil)))
    ;; FLOORS
    (dotimes (i channels)
      (let* ((s (aref (mp-chan-mux map) i))
             (floor (aref (mp-submap-floor map) s)))
        (setf (aref zero-channel i) nil)
        (if (= (aref (vb-floor-types f) floor) 0)
            (return-from %stbv-vorbis-decode-packet-rest (%stbv-error f +vorbis-invalid-stream+))
            (let ((g (aref (vb-floor-config f) floor)))
              (block floor1
                (if (/= (%stbv-get-bits f 1) 0)
                    (let* ((final-y (aref (vb-final-y f) i))
                           (step2-flag (make-array 256 :initial-element nil))
                           (range (svref #(256 128 86 64) (- (fl-floor1-multiplier g) 1)))
                           (offset 2))
                      (setf (aref final-y 0) (%i16 (%stbv-get-bits f (- (%stbv-ilog range) 1)))
                            (aref final-y 1) (%i16 (%stbv-get-bits f (- (%stbv-ilog range) 1))))
                      (dotimes (j (fl-partitions g))
                        (let* ((pclass (aref (fl-partition-class-list g) j))
                               (cdim (aref (fl-class-dimensions g) pclass))
                               (cbits (aref (fl-class-subclasses g) pclass))
                               (csub (- (ash 1 cbits) 1))
                               (cval 0))
                          (when (/= cbits 0)
                            (setf cval (%stbv-decode f (aref (vb-codebooks f) (aref (fl-class-masterbooks g) pclass)))))
                          (dotimes (k cdim)
                            (let ((book (aref (fl-subclass-books g) pclass (logand cval csub))))
                              (setf cval (ash cval (- cbits)))
                              (if (>= book 0)
                                  (setf (aref final-y offset) (%i16 (%stbv-decode f (aref (vb-codebooks f) book))))
                                  (setf (aref final-y offset) 0))
                              (incf offset)))))
                      (when (= (vb-valid-bits f) +stbv-invalid-bits+)   ; Behavior according to spec
                        (setf (aref zero-channel i) t)
                        (return-from floor1))
                      (setf (aref step2-flag 0) t (aref step2-flag 1) t)
                      (loop for j from 2 below (fl-values g)
                            do (let* ((low (aref (fl-neighbors g) j 0))
                                      (high (aref (fl-neighbors g) j 1))
                                      (xl (fl-xlist g))
                                      (pred (%stbv-predict-point (aref xl j) (aref xl low) (aref xl high)
                                                                 (aref final-y low) (aref final-y high)))
                                      (val (aref final-y j))
                                      (highroom (- range pred))
                                      (lowroom pred)
                                      (room (if (< highroom lowroom) (* highroom 2) (* lowroom 2))))
                                 (if (/= val 0)
                                     (progn
                                       (setf (aref step2-flag low) t (aref step2-flag high) t (aref step2-flag j) t)
                                       (setf (aref final-y j)
                                             (%i16 (if (>= val room)
                                                       (if (> highroom lowroom)
                                                           (+ (- val lowroom) pred)
                                                           (+ (- pred val) highroom -1))
                                                       (if (logtest val 1)
                                                           (- pred (ash (+ val 1) -1))
                                                           (+ pred (ash val -1)))))))
                                     (setf (aref step2-flag j) nil
                                           (aref final-y j) (%i16 pred)))))
                      ;; Defer final floor computation until _after_ residue
                      (dotimes (j (fl-values g))
                        (unless (aref step2-flag j)
                          (setf (aref final-y j) -1))))
                    (setf (aref zero-channel i) t)))))))
    ;; NOTE: Residue decoding
    (replace really-zero-channel zero-channel :end2 channels)
    (dotimes (i (mp-coupling-steps map))
      (let ((mag (aref (mp-chan-magnitude map) i))
            (ang (aref (mp-chan-angle map) i)))
        (when (or (not (aref zero-channel mag)) (not (aref zero-channel ang)))
          (setf (aref zero-channel mag) nil
                (aref zero-channel ang) nil))))
    (dotimes (i (mp-submaps map))
      (let ((residue-buffers (make-array +stbv-max-channels+ :initial-element nil))
            (do-not-decode (make-array 256 :initial-element nil))
            (ch 0))
        (dotimes (j channels)
          (when (= (aref (mp-chan-mux map) j) i)
            (if (aref zero-channel j)
                (setf (aref do-not-decode ch) t
                      (svref residue-buffers ch) nil)
                (setf (aref do-not-decode ch) nil
                      (svref residue-buffers ch) (aref (vb-channel-buffers f) j)))
            (incf ch)))
        (%stbv-decode-residue f residue-buffers ch n2 (aref (mp-submap-residue map) i) do-not-decode)))
    ;; INVERSE COUPLING
    (loop for i from (- (mp-coupling-steps map) 1) downto 0
          do (let ((mm (aref (vb-channel-buffers f) (aref (mp-chan-magnitude map) i)))
                   (aa (aref (vb-channel-buffers f) (aref (mp-chan-angle map) i))))
               (declare (type %f32-array mm aa))
               (dotimes (j n2)
                 (let ((mj (aref mm j)) (aj (aref aa j)) a2 m2)
                   (if (> mj 0)
                       (if (> aj 0)
                           (setf m2 mj a2 (- mj aj))
                           (setf a2 mj m2 (+ mj aj)))
                       (if (> aj 0)
                           (setf m2 mj a2 (+ mj aj))
                           (setf a2 mj m2 (- mj aj))))
                   (setf (aref mm j) m2
                         (aref aa j) a2)))))
    ;; Finish decoding the floors
    (dotimes (i channels)
      (if (aref really-zero-channel i)
          (fill (aref (vb-channel-buffers f) i) 0f0 :end n2)
          (%stbv-do-floor f map i n (aref (vb-channel-buffers f) i) (aref (vb-final-y f) i))))
    ;; INVERSE MDCT
    (dotimes (i channels)
      (%stbv-inverse-mdct (aref (vb-channel-buffers f) i) n f (md-blockflag m)))
    ;; This shouldn't be necessary, unless we exited on an error
    ;; and want to flush to get to the next packet
    (%stbv-flush-packet f)
    (cond
      ((vb-first-decode f)
       ;; Assume we start so first non-discarded sample is sample 0
       ;; this isn't to spec, but spec would require us to read ahead
       ;; and decode the size of all current frames--could be done,
       ;; but presumably it's not a commonly used feature
       (setf (vb-current-loc f) (%u32 (- n2))   ; Start of first frame is positioned for discard
             ;; We might have to discard samples "from" the next frame too,
             ;; if we're lapping a large block then a small at the start?
             (vb-discard-samples-deferred f) (- n right-end)
             (vb-current-loc-valid f) t
             (vb-first-decode f) nil))
      ((/= (vb-discard-samples-deferred f) 0)
       (if (>= (vb-discard-samples-deferred f) (- right-start left-start))
           (progn
             (decf (vb-discard-samples-deferred f) (- right-start left-start))
             (setf left-start right-start))
           (progn
             (incf left-start (vb-discard-samples-deferred f))
             (setf (vb-discard-samples-deferred f) 0)))))
    ;; Check if we have ogg information about the sample # for this packet
    (when (= (vb-last-seg-which f) (vb-end-seg-with-known-loc f))
      ;; If we have a valid current loc, and this is final:
      (when (and (vb-current-loc-valid f) (logtest (vb-page-flag f) +stbv-pageflag-last-page+))
        (let ((current-end (vb-known-loc-for-packet f)))
          ;; Then let's infer the size of the (probably) short final frame
          (when (< current-end (%u32 (+ (vb-current-loc f) (- right-end left-start))))
            (let ((len (if (< current-end (vb-current-loc f))
                           0      ; Negative truncation, that's impossible!
                           (%i32 (- current-end (vb-current-loc f))))))
              (incf len left-start)   ; This doesn't seem right, but has no ill effect on my test files
              (when (> len right-end) (setf len right-end))   ; This should never happen
              (setf (vb-current-loc f) (%u32 (+ (vb-current-loc f) len)))
              (return-from %stbv-vorbis-decode-packet-rest (values t len left-start))))))
      ;; Otherwise, just set our sample loc
      ;; Guess that the ogg granule pos refers to the _middle_ of the
      ;; last frame?
      ;; Set f->current_loc to the position of left_start
      (setf (vb-current-loc f) (%u32 (- (vb-known-loc-for-packet f) (- n2 left-start)))
            (vb-current-loc-valid f) t))
    (when (vb-current-loc-valid f)
      (setf (vb-current-loc f) (%u32 (+ (vb-current-loc f) (- right-start left-start)))))
    ;; Ignore samples after the window goes to 0
    (values t right-end left-start)))

;; Returns (values ok len left right)
(defun %stbv-vorbis-decode-packet (f)
  (multiple-value-bind (ok left-start left-end right-start right-end mode) (%stbv-vorbis-decode-initial f)
    (if (not ok)
        nil
        (multiple-value-bind (ok2 len left)
            (%stbv-vorbis-decode-packet-rest f (aref (vb-mode-config f) mode) left-start left-end right-start right-end)
          (values ok2 len left right-start)))))

(defun %stbv-vorbis-finish-frame (f len left right)
  ;; We use right&left (the start of the right- and left-window sin()-regions)
  ;; to determine how much to return, rather than inferring from the rules
  ;; (same result, clearer code); 'left' indicates where our sin() window
  ;; starts, therefore where the previous window's right edge starts, and
  ;; therefore where to start mixing from the previous buffer. 'right'
  ;; indicates where our sin() ending-window starts, therefore that's where
  ;; we start saving, and where our returned-data ends.
  ;; Mixin from previous window
  (when (/= (vb-previous-length f) 0)
    (let* ((n (vb-previous-length f))
           (w (%stbv-get-window f n)))
      (when (null w) (return-from %stbv-vorbis-finish-frame 0))
      (dotimes (i (vb-channels f))
        (let ((cb (aref (vb-channel-buffers f) i))
              (pw (aref (vb-previous-window f) i)))
          (declare (type %f32-array cb pw w))
          (dotimes (j n)
            (setf (aref cb (+ left j)) (+ (* (aref cb (+ left j)) (aref w j))
                                          (* (aref pw j) (aref w (- n 1 j))))))))))
  (let ((prev (vb-previous-length f)))
    ;; Last half of this data becomes previous window
    (setf (vb-previous-length f) (- len right))
    ;; @OPTIMIZE: could avoid this copy by double-buffering the
    ;; output (flipping previous_window with channel_buffers), but
    ;; then previous_window would have to be 2x as large, and
    ;; channel_buffers couldn't be temp mem (although they're NOT
    ;; currently temp mem, they could be (unless we want to level
    ;; performance by spreading out the computation))
    (dotimes (i (vb-channels f))
      (replace (aref (vb-previous-window f) i) (aref (vb-channel-buffers f) i) :start2 right :end2 (max right len)))
    (if (= prev 0)
        ;; There was no previous packet, so this data isn't valid...
        ;; this isn't entirely true, only the would-have-overlapped data
        ;; isn't valid, but this seems to be what the spec requires
        0
        (progn
          ;; Truncate a short frame
          (when (< len right) (setf right len))
          (incf (vb-samples-output f) (- right left))
          (- right left)))))

(defun %stbv-vorbis-pump-first-frame (f)
  (multiple-value-bind (res len left right) (%stbv-vorbis-decode-packet f)
    (when res
      (%stbv-vorbis-finish-frame f len left right))
    res))

;;;----------------------------------------------------------------------------------
;;; Header decoding
;;;----------------------------------------------------------------------------------

(defun %stbv-vorbis-validate (data)
  (every #'= data (map 'vector #'char-code "vorbis")))

(defun %stbv-start-decoder (f)
  (let ((header (make-array 6 :element-type '(unsigned-byte 8) :initial-element 0))
        (longest-floorlist 0))
    (macrolet ((fail (e) `(return-from %stbv-start-decoder (%stbv-error f ,e))))
      (setf (vb-first-decode f) t)
      ;; First page, first packet
      (unless (%stbv-start-page f) (return-from %stbv-start-decoder nil))
      ;; Validate page flag
      (unless (logtest (vb-page-flag f) +stbv-pageflag-first-page+) (fail +vorbis-invalid-first-page+))
      (when (logtest (vb-page-flag f) +stbv-pageflag-last-page+) (fail +vorbis-invalid-first-page+))
      (when (logtest (vb-page-flag f) +stbv-pageflag-continued-packet+) (fail +vorbis-invalid-first-page+))
      ;; Check for expected packet length
      (unless (= (vb-segment-count f) 1) (fail +vorbis-invalid-first-page+))
      (unless (= (aref (vb-segments f) 0) 30)
        ;; Check for the Ogg skeleton fishead identifying header to refine our error
        (if (and (= (aref (vb-segments f) 0) 64)
                 (%stbv-getn f header 6)
                 (equalp header (map 'vector #'char-code "fishea"))
                 (= (%stbv-get8 f) (char-code #\d))
                 (= (%stbv-get8 f) 0))
            (fail +vorbis-ogg-skeleton-not-supported+)
            (fail +vorbis-invalid-first-page+)))
      ;; Read packet
      ;; Check packet header
      (unless (= (%stbv-get8 f) +stbv-packet-id+) (fail +vorbis-invalid-first-page+))
      (unless (%stbv-getn f header 6) (fail +vorbis-unexpected-eof+))
      (unless (%stbv-vorbis-validate header) (fail +vorbis-invalid-first-page+))
      ;; vorbis_version
      (unless (= (%stbv-get32 f) 0) (fail +vorbis-invalid-first-page+))
      (setf (vb-channels f) (%stbv-get8 f))
      (when (= (vb-channels f) 0) (fail +vorbis-invalid-first-page+))
      (when (> (vb-channels f) +stbv-max-channels+) (fail +vorbis-too-many-channels+))
      (setf (vb-sample-rate f) (%stbv-get32 f))
      (when (= (vb-sample-rate f) 0) (fail +vorbis-invalid-first-page+))
      (%stbv-get32 f)                   ; bitrate_maximum
      (%stbv-get32 f)                   ; bitrate_nominal
      (%stbv-get32 f)                   ; bitrate_minimum
      (let* ((x (%stbv-get8 f))
             (log0 (logand x 15))
             (log1 (ash x -4)))
        (setf (vb-blocksize-0 f) (ash 1 log0)
              (vb-blocksize-1 f) (ash 1 log1))
        (when (or (< log0 6) (> log0 13)) (fail +vorbis-invalid-setup+))
        (when (or (< log1 6) (> log1 13)) (fail +vorbis-invalid-setup+))
        (when (> log0 log1) (fail +vorbis-invalid-setup+)))
      ;; Framing_flag
      (unless (logtest (%stbv-get8 f) 1) (fail +vorbis-invalid-first-page+))
      ;; Second packet!
      (unless (%stbv-start-page f) (return-from %stbv-start-decoder nil))
      (unless (%stbv-start-packet f) (return-from %stbv-start-decoder nil))
      (when (= (%stbv-next-segment f) 0) (return-from %stbv-start-decoder nil))
      (unless (= (%stbv-get8-packet f) +stbv-packet-comment+) (fail +vorbis-invalid-setup+))
      (dotimes (i 6) (setf (aref header i) (logand (%stbv-get8-packet f) #xff)))
      (unless (%stbv-vorbis-validate header) (fail +vorbis-invalid-setup+))
      ;; Comments
      (flet ((read-string ()
               (let* ((len (%stbv-get32-packet f))
                      (s (make-array (max len 0) :element-type '(unsigned-byte 8))))
                 (dotimes (i len) (setf (aref s i) (logand (%stbv-get8-packet f) #xff)))
                 (babel:octets-to-string s :encoding :utf-8 :errorp nil))))
        (setf (vb-vendor f) (read-string))
        (let ((comment-list-length (%stbv-get32-packet f)))
          (setf (vb-comment-list f) (loop repeat (max comment-list-length 0) collect (read-string)))))
      ;; Framing_flag
      (unless (logtest (%stbv-get8-packet f) 1) (fail +vorbis-invalid-setup+))
      (%stbv-skip f (vb-bytes-in-seg f))
      (setf (vb-bytes-in-seg f) 0)
      (loop (let ((len (%stbv-next-segment f)))
              (%stbv-skip f len)
              (setf (vb-bytes-in-seg f) 0)
              (when (= len 0) (return))))
      ;; Third packet!
      (unless (%stbv-start-packet f) (return-from %stbv-start-decoder nil))
      (unless (= (%stbv-get8-packet f) +stbv-packet-setup+) (fail +vorbis-invalid-setup+))
      (dotimes (i 6) (setf (aref header i) (logand (%stbv-get8-packet f) #xff)))
      (unless (%stbv-vorbis-validate header) (fail +vorbis-invalid-setup+))
      ;; Codebooks
      (setf (vb-codebook-count f) (+ (%stbv-get-bits f 8) 1)
            (vb-codebooks f) (make-array (vb-codebook-count f)))
      (dotimes (i (vb-codebook-count f))
        (let ((c (make-stbv-codebook))
              (total 0)
              (lengths nil)
              (values nil))
          (setf (aref (vb-codebooks f) i) c)
          (unless (= (%stbv-get-bits f 8) #x42) (fail +vorbis-invalid-setup+))
          (unless (= (%stbv-get-bits f 8) #x43) (fail +vorbis-invalid-setup+))
          (unless (= (%stbv-get-bits f 8) #x56) (fail +vorbis-invalid-setup+))
          (let ((x (%stbv-get-bits f 8)))
            (setf (cb-dimensions c) (+ (ash (%stbv-get-bits f 8) 8) x)))
          (let* ((x (%stbv-get-bits f 8))
                 (y (%stbv-get-bits f 8)))
            (setf (cb-entries c) (+ (ash (%stbv-get-bits f 8) 16) (ash y 8) x)))
          (let ((ordered (%stbv-get-bits f 1)))
            (setf (cb-sparse c) (if (/= ordered 0) 0 (%stbv-get-bits f 1)))
            (when (and (= (cb-dimensions c) 0) (/= (cb-entries c) 0)) (fail +vorbis-invalid-setup+))
            (setf lengths (make-array (cb-entries c) :element-type '(unsigned-byte 8) :initial-element 0))
            (when (= (cb-sparse c) 0) (setf (cb-codeword-lengths c) lengths))
            (if (/= ordered 0)
                (let ((current-entry 0)
                      (current-length (+ (%stbv-get-bits f 5) 1)))
                  (loop while (< current-entry (cb-entries c))
                        do (let* ((limit (- (cb-entries c) current-entry))
                                  (n (%stbv-get-bits f (%stbv-ilog limit))))
                             (when (>= current-length 32) (fail +vorbis-invalid-setup+))
                             (when (> (+ current-entry n) (cb-entries c)) (fail +vorbis-invalid-setup+))
                             (fill lengths current-length :start current-entry :end (+ current-entry n))
                             (incf current-entry n)
                             (incf current-length))))
                (dotimes (j (cb-entries c))
                  (let ((present (if (/= (cb-sparse c) 0) (%stbv-get-bits f 1) 1)))
                    (if (/= present 0)
                        (progn
                          (setf (aref lengths j) (+ (%stbv-get-bits f 5) 1))
                          (incf total)
                          (when (= (aref lengths j) 32) (fail +vorbis-invalid-setup+)))
                        (setf (aref lengths j) +stbv-no-code+))))))
          (when (and (/= (cb-sparse c) 0) (>= total (ash (cb-entries c) -2)))
            ;; Convert sparse items to non-sparse!
            (setf (cb-codeword-lengths c) lengths
                  (cb-sparse c) 0))
          ;; Compute the size of the sorted tables
          (let ((sorted-count (if (/= (cb-sparse c) 0)
                                  total
                                  (count-if (lambda (l) (and (> l +stbv-fast-huffman-length+) (/= l +stbv-no-code+)))
                                            lengths))))
            (setf (cb-sorted-entries c) sorted-count))
          (if (= (cb-sparse c) 0)
              (setf (cb-codewords c) (make-array (cb-entries c) :element-type '(unsigned-byte 32) :initial-element 0))
              (when (/= (cb-sorted-entries c) 0)
                (setf (cb-codeword-lengths c) (make-array (cb-sorted-entries c) :element-type '(unsigned-byte 8) :initial-element 0)
                      (cb-codewords c) (make-array (cb-sorted-entries c) :element-type '(unsigned-byte 32) :initial-element 0)
                      values (make-array (cb-sorted-entries c) :element-type '(unsigned-byte 32) :initial-element 0))))
          (unless (%stbv-compute-codewords c lengths (cb-entries c) values)
            (fail +vorbis-invalid-setup+))
          (when (/= (cb-sorted-entries c) 0)
            ;; Allocate an extra slot for sentinels
            (setf (cb-sorted-codewords c) (make-array (+ (cb-sorted-entries c) 1) :element-type '(unsigned-byte 32) :initial-element 0)
                  ;; Allocate an extra slot at the front so that c->sorted_values[-1] is defined
                  ;; so that we can catch that case without an extra if
                  (cb-sorted-values c) (make-array (+ (cb-sorted-entries c) 2) :element-type 'fixnum :initial-element 0))
            (setf (aref (cb-sorted-values c) 0) -1)
            (%stbv-compute-sorted-huffman c lengths values))
          (when (/= (cb-sparse c) 0)
            (setf (cb-codewords c) nil))
          (%stbv-compute-accelerated-huffman c)
          (setf (cb-lookup-type c) (%stbv-get-bits f 4))
          (when (> (cb-lookup-type c) 2) (fail +vorbis-invalid-setup+))
          (when (> (cb-lookup-type c) 0)
            (setf (cb-minimum-value c) (%stbv-float32-unpack (%stbv-get-bits f 32))
                  (cb-delta-value c) (%stbv-float32-unpack (%stbv-get-bits f 32))
                  (cb-value-bits c) (+ (%stbv-get-bits f 4) 1)
                  (cb-sequence-p c) (%stbv-get-bits f 1))
            (if (= (cb-lookup-type c) 1)
                (let ((values (%stbv-lookup1-values (cb-entries c) (cb-dimensions c))))
                  (when (< values 0) (fail +vorbis-invalid-setup+))
                  (setf (cb-lookup-values c) values))
                (setf (cb-lookup-values c) (* (cb-entries c) (cb-dimensions c))))
            (when (= (cb-lookup-values c) 0) (fail +vorbis-invalid-setup+))
            (let ((mults (make-array (cb-lookup-values c) :initial-element 0)))
              (dotimes (j (cb-lookup-values c))
                (setf (aref mults j) (%stbv-get-bits f (cb-value-bits c))))
              (block skip
                (if (= (cb-lookup-type c) 1)
                    (let* ((sparse (/= (cb-sparse c) 0))
                           (last 0f0)
                           (len (if sparse (cb-sorted-entries c) (cb-entries c))))
                      (when (and sparse (= (cb-sorted-entries c) 0)) (return-from skip))
                      (setf (cb-multiplicands c) (make-array (* len (cb-dimensions c)) :element-type 'single-float
                                                                                       :initial-element 0f0))
                      ;; Pre-expand the lookup1-style multiplicands, to avoid a divide in the inner loop
                      (dotimes (j len)
                        (let ((z (if sparse (aref (cb-sorted-values c) (+ j 1)) j))
                              (div 1))
                          (dotimes (k (cb-dimensions c))
                            (let* ((off (mod (floor z div) (cb-lookup-values c)))
                                   (val (+ (+ (* (float (aref mults off) 1f0) (cb-delta-value c)) (cb-minimum-value c)) last)))
                              (setf (aref (cb-multiplicands c) (+ (* j (cb-dimensions c)) k)) val)
                              (when (/= (cb-sequence-p c) 0) (setf last val))
                              (when (< (+ k 1) (cb-dimensions c))
                                (when (> div (floor #xffffffff (cb-lookup-values c)))
                                  (fail +vorbis-invalid-setup+))
                                (setf div (* div (cb-lookup-values c))))))))
                      (setf (cb-lookup-type c) 2))
                    (let ((last 0f0))
                      (setf (cb-multiplicands c) (make-array (cb-lookup-values c) :element-type 'single-float
                                                                                  :initial-element 0f0))
                      (dotimes (j (cb-lookup-values c))
                        (let ((val (+ (+ (* (float (aref mults j) 1f0) (cb-delta-value c)) (cb-minimum-value c)) last)))
                          (setf (aref (cb-multiplicands c) j) val)
                          (when (/= (cb-sequence-p c) 0) (setf last val)))))))))))
      ;; Time domain transfers (notused)
      (let ((x (+ (%stbv-get-bits f 6) 1)))
        (dotimes (i x)
          (unless (= (%stbv-get-bits f 16) 0) (fail +vorbis-invalid-setup+))))
      ;; Floors
      (setf (vb-floor-count f) (+ (%stbv-get-bits f 6) 1)
            (vb-floor-config f) (make-array (vb-floor-count f) :initial-element nil))
      (dotimes (i (vb-floor-count f))
        (setf (aref (vb-floor-types f) i) (%stbv-get-bits f 16))
        (when (> (aref (vb-floor-types f) i) 1) (fail +vorbis-invalid-setup+))
        (if (= (aref (vb-floor-types f) i) 0)
            (progn
              ;; Floor 0 is not supported by stb_vorbis
              (%stbv-get-bits f 8) (%stbv-get-bits f 16) (%stbv-get-bits f 16) (%stbv-get-bits f 6) (%stbv-get-bits f 8)
              (let ((number-of-books (+ (%stbv-get-bits f 4) 1)))
                (dotimes (j number-of-books) (%stbv-get-bits f 8)))
              (fail +vorbis-feature-not-supported+))
            (let ((g (make-stbv-floor1))
                  (max-class -1))
              (setf (aref (vb-floor-config f) i) g)
              (setf (fl-partitions g) (%stbv-get-bits f 5))
              (dotimes (j (fl-partitions g))
                (setf (aref (fl-partition-class-list g) j) (%stbv-get-bits f 4))
                (setf max-class (max max-class (aref (fl-partition-class-list g) j))))
              (loop for j from 0 to max-class
                    do (setf (aref (fl-class-dimensions g) j) (+ (%stbv-get-bits f 3) 1)
                             (aref (fl-class-subclasses g) j) (%stbv-get-bits f 2))
                       (when (/= (aref (fl-class-subclasses g) j) 0)
                         (setf (aref (fl-class-masterbooks g) j) (%stbv-get-bits f 8))
                         (when (>= (aref (fl-class-masterbooks g) j) (vb-codebook-count f)) (fail +vorbis-invalid-setup+)))
                       (dotimes (k (ash 1 (aref (fl-class-subclasses g) j)))
                         (setf (aref (fl-subclass-books g) j k) (- (%stbv-get-bits f 8) 1))
                         (when (>= (aref (fl-subclass-books g) j k) (vb-codebook-count f)) (fail +vorbis-invalid-setup+))))
              (setf (fl-floor1-multiplier g) (+ (%stbv-get-bits f 2) 1)
                    (fl-rangebits g) (%stbv-get-bits f 4))
              (setf (aref (fl-xlist g) 0) 0
                    (aref (fl-xlist g) 1) (ash 1 (fl-rangebits g))
                    (fl-values g) 2)
              (dotimes (j (fl-partitions g))
                (let ((c (aref (fl-partition-class-list g) j)))
                  (dotimes (k (aref (fl-class-dimensions g) c))
                    (setf (aref (fl-xlist g) (fl-values g)) (%stbv-get-bits f (fl-rangebits g)))
                    (incf (fl-values g)))))
              ;; Precompute the sorting
              (let ((p (sort (loop for j from 0 below (fl-values g) collect (cons (aref (fl-xlist g) j) j))
                             #'< :key #'car)))
                (loop for (a b) on p
                      when (and b (= (car a) (car b))) do (fail +vorbis-invalid-setup+))
                (loop for (nil . id) in p
                      for j from 0
                      do (setf (aref (fl-sorted-order g) j) id)))
              ;; Precompute the neighbors
              (loop for j from 2 below (fl-values g)
                    do (multiple-value-bind (low hi) (%stbv-neighbors (fl-xlist g) j)
                         (setf (aref (fl-neighbors g) j 0) low
                               (aref (fl-neighbors g) j 1) hi)))
              (setf longest-floorlist (max longest-floorlist (fl-values g))))))
      ;; Residue
      (setf (vb-residue-count f) (+ (%stbv-get-bits f 6) 1)
            (vb-residue-config f) (make-array (vb-residue-count f) :initial-element nil))
      (dotimes (i (vb-residue-count f))
        (let ((residue-cascade (make-array 64 :initial-element 0))
              (r (make-stbv-residue)))
          (setf (aref (vb-residue-config f) i) r)
          (setf (aref (vb-residue-types f) i) (%stbv-get-bits f 16))
          (when (> (aref (vb-residue-types f) i) 2) (fail +vorbis-invalid-setup+))
          (setf (rs-begin r) (%stbv-get-bits f 24)
                (rs-end r) (%stbv-get-bits f 24))
          (when (< (rs-end r) (rs-begin r)) (fail +vorbis-invalid-setup+))
          (setf (rs-part-size r) (+ (%stbv-get-bits f 24) 1)
                (rs-classifications r) (+ (%stbv-get-bits f 6) 1)
                (rs-classbook r) (%stbv-get-bits f 8))
          (when (>= (rs-classbook r) (vb-codebook-count f)) (fail +vorbis-invalid-setup+))
          (dotimes (j (rs-classifications r))
            (let ((high-bits 0)
                  (low-bits (%stbv-get-bits f 3)))
              (when (/= (%stbv-get-bits f 1) 0)
                (setf high-bits (%stbv-get-bits f 5)))
              (setf (aref residue-cascade j) (logand (+ (* high-bits 8) low-bits) #xff))))
          (setf (rs-residue-books r) (make-array (list (rs-classifications r) 8) :initial-element 0))
          (dotimes (j (rs-classifications r))
            (dotimes (k 8)
              (if (logbitp k (aref residue-cascade j))
                  (progn
                    (setf (aref (rs-residue-books r) j k) (%stbv-get-bits f 8))
                    (when (>= (aref (rs-residue-books r) j k) (vb-codebook-count f)) (fail +vorbis-invalid-setup+)))
                  (setf (aref (rs-residue-books r) j k) -1))))
          ;; Precompute the classifications[] array to avoid inner-loop mod/divide
          ;; call it 'classdata' since we already have r->classifications
          (let* ((classbook (aref (vb-codebooks f) (rs-classbook r)))
                 (entries (cb-entries classbook))
                 (classwords (cb-dimensions classbook)))
            (setf (rs-classdata r) (make-array entries))
            (dotimes (j entries)
              (let ((temp j)
                    (cd (make-array classwords :element-type '(unsigned-byte 8) :initial-element 0)))
                (setf (aref (rs-classdata r) j) cd)
                (loop for k from (- classwords 1) downto 0
                      do (setf (aref cd k) (mod temp (rs-classifications r))
                               temp (floor temp (rs-classifications r)))))))))
      ;; Mappings
      (setf (vb-mapping-count f) (+ (%stbv-get-bits f 6) 1)
            (vb-mapping f) (make-array (vb-mapping-count f) :initial-element nil))
      (dotimes (i (vb-mapping-count f))
        (let ((m (make-stbv-mapping))
              (channels (vb-channels f)))
          (setf (aref (vb-mapping f) i) m)
          (unless (= (%stbv-get-bits f 16) 0) (fail +vorbis-invalid-setup+))
          (setf (mp-chan-magnitude m) (make-array channels :initial-element 0)
                (mp-chan-angle m) (make-array channels :initial-element 0)
                (mp-chan-mux m) (make-array channels :initial-element 0))
          (setf (mp-submaps m) (if (/= (%stbv-get-bits f 1) 0) (+ (%stbv-get-bits f 4) 1) 1))
          (if (/= (%stbv-get-bits f 1) 0)
              (progn
                (setf (mp-coupling-steps m) (+ (%stbv-get-bits f 8) 1))
                (when (> (mp-coupling-steps m) channels) (fail +vorbis-invalid-setup+))
                (dotimes (k (mp-coupling-steps m))
                  (setf (aref (mp-chan-magnitude m) k) (%stbv-get-bits f (%stbv-ilog (- channels 1)))
                        (aref (mp-chan-angle m) k) (%stbv-get-bits f (%stbv-ilog (- channels 1))))
                  (when (>= (aref (mp-chan-magnitude m) k) channels) (fail +vorbis-invalid-setup+))
                  (when (>= (aref (mp-chan-angle m) k) channels) (fail +vorbis-invalid-setup+))
                  (when (= (aref (mp-chan-magnitude m) k) (aref (mp-chan-angle m) k)) (fail +vorbis-invalid-setup+))))
              (setf (mp-coupling-steps m) 0))
          ;; Reserved field
          (unless (= (%stbv-get-bits f 2) 0) (fail +vorbis-invalid-setup+))
          (if (> (mp-submaps m) 1)
              (dotimes (j channels)
                (setf (aref (mp-chan-mux m) j) (%stbv-get-bits f 4))
                (when (>= (aref (mp-chan-mux m) j) (mp-submaps m)) (fail +vorbis-invalid-setup+)))
              ;; @SPECIFICATION: this case is missing from the spec
              (fill (mp-chan-mux m) 0))
          (dotimes (j (mp-submaps m))
            (%stbv-get-bits f 8)        ; Discard
            (setf (aref (mp-submap-floor m) j) (%stbv-get-bits f 8)
                  (aref (mp-submap-residue m) j) (%stbv-get-bits f 8))
            (when (>= (aref (mp-submap-floor m) j) (vb-floor-count f)) (fail +vorbis-invalid-setup+))
            (when (>= (aref (mp-submap-residue m) j) (vb-residue-count f)) (fail +vorbis-invalid-setup+)))))
      ;; Modes
      (setf (vb-mode-count f) (+ (%stbv-get-bits f 6) 1))
      (dotimes (i (vb-mode-count f))
        (let ((m (make-stbv-mode)))
          (setf (aref (vb-mode-config f) i) m)
          (setf (md-blockflag m) (%stbv-get-bits f 1)
                (md-windowtype m) (%stbv-get-bits f 16)
                (md-transformtype m) (%stbv-get-bits f 16)
                (md-mapping m) (%stbv-get-bits f 8))
          (unless (= (md-windowtype m) 0) (fail +vorbis-invalid-setup+))
          (unless (= (md-transformtype m) 0) (fail +vorbis-invalid-setup+))
          (when (>= (md-mapping m) (vb-mapping-count f)) (fail +vorbis-invalid-setup+))))
      (%stbv-flush-packet f)
      (setf (vb-previous-length f) 0)
      (dotimes (i (vb-channels f))
        (setf (aref (vb-channel-buffers f) i) (make-array (vb-blocksize-1 f) :element-type 'single-float :initial-element 0f0)
              (aref (vb-previous-window f) i) (make-array (floor (vb-blocksize-1 f) 2) :element-type 'single-float
                                                                                       :initial-element 0f0)
              (aref (vb-final-y f) i) (make-array longest-floorlist :element-type 'fixnum :initial-element 0)))
      (%stbv-init-blocksize f 0 (vb-blocksize-0 f))
      (%stbv-init-blocksize f 1 (vb-blocksize-1 f))
      (setf (aref (vb-blocksize f) 0) (vb-blocksize-0 f)
            (aref (vb-blocksize f) 1) (vb-blocksize-1 f)
            (vb-imdct-buf f) (make-array (floor (vb-blocksize-1 f) 2) :element-type 'single-float :initial-element 0f0))
      (setf (vb-first-audio-page-offset f) (if (= (vb-next-seg f) -1) (stb-vorbis-get-file-offset f) 0))
      t)))

;;;----------------------------------------------------------------------------------
;;; Pull API
;;;----------------------------------------------------------------------------------

(defun stb-vorbis-get-info (f)
  "Returns (values channels sample-rate max-frame-size)"
  (values (vb-channels f) (vb-sample-rate f) (ash (vb-blocksize-1 f) -1)))

(defun stb-vorbis-get-error (f)
  (prog1 (vb-error f) (setf (vb-error f) +vorbis-no-error+)))

;; Returns (values found end last)
(defun %stbv-vorbis-find-page (f)
  (loop
    (when (vb-eof f) (return nil))
    (let ((n (%stbv-get8 f)))
      (when (= n #x4f)                  ; Page header candidate
        (let ((retry-loc (stb-vorbis-get-file-offset f)))
          ;; Check if we're off the end of a file_section stream
          (when (> (%u32 (- retry-loc 25)) (vb-stream-len f))
            (return nil))
          ;; Check the rest of the header
          (let ((i (loop for i from 1 below 4
                         unless (= (%stbv-get8 f) (aref #(#x4f #x67 #x67 #x53) i)) return i
                         finally (return 4))))
            (when (vb-eof f) (return nil))
            (block invalid
              (when (= i 4)
                (let ((header (make-array 27 :element-type '(unsigned-byte 8) :initial-element 0))
                      (crc 0) (goal 0) (len 0))
                  (replace header #(#x4f #x67 #x67 #x53))
                  (loop for k from 4 below 27 do (setf (aref header k) (%stbv-get8 f)))
                  (when (vb-eof f) (return-from %stbv-vorbis-find-page nil))
                  (unless (= (aref header 4) 0) (return-from invalid))
                  (setf goal (logior (aref header 22) (ash (aref header 23) 8) (ash (aref header 24) 16)
                                     (ash (aref header 25) 24)))
                  (loop for k from 22 below 26 do (setf (aref header k) 0))
                  (dotimes (k 27) (setf crc (%stbv-crc32-update crc (aref header k))))
                  (dotimes (k (aref header 26))
                    (let ((s (%stbv-get8 f)))
                      (setf crc (%stbv-crc32-update crc s))
                      (incf len s)))
                  (when (and (/= len 0) (vb-eof f)) (return-from %stbv-vorbis-find-page nil))
                  (dotimes (k len)
                    (setf crc (%stbv-crc32-update crc (%stbv-get8 f))))
                  ;; Finished parsing probable page
                  (when (= crc goal)
                    ;; We could now check that it's either got the last
                    ;; page flag set, OR it's followed by the capture
                    ;; pattern, but I guess TECHNICALLY you could have
                    ;; a file with garbage between each ogg page and recover
                    ;; from it automatically? So even though that paranoia
                    ;; might decrease the chance of an invalid decode by
                    ;; another 2^32, not worth it since it would hose those
                    ;; invalid-but-useful files?
                    (let ((end (stb-vorbis-get-file-offset f))
                          (last (if (logtest (aref header 5) #x04) 1 0)))
                      (%stbv-set-file-offset f (- retry-loc 1))
                      (return-from %stbv-vorbis-find-page (values t end last)))))))
            (%stbv-set-file-offset f retry-loc)))))))

(defun %stbv-get-seek-page-info (f z)
  (let ((header (make-array 27 :element-type '(unsigned-byte 8) :initial-element 0))
        (lacing (make-array 255 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Record where the page starts
    (setf (pp-page-start z) (stb-vorbis-get-file-offset f))
    ;; Parse the header
    (%stbv-getn f header 27)
    (unless (and (= (aref header 0) (char-code #\O)) (= (aref header 1) (char-code #\g))
                 (= (aref header 2) (char-code #\g)) (= (aref header 3) (char-code #\S)))
      (return-from %stbv-get-seek-page-info nil))
    (%stbv-getn f lacing (aref header 26))
    ;; Determine the length of the payload
    (let ((len (loop for i from 0 below (aref header 26) sum (aref lacing i))))
      ;; This implies where the page ends
      (setf (pp-page-end z) (+ (pp-page-start z) 27 (aref header 26) len)
            ;; Read the last-decoded sample out of the data
            (pp-last-decoded-sample z) (logand (+ (aref header 6) (ash (aref header 7) 8) (ash (aref header 8) 16)
                                                  (ash (aref header 9) 24))
                                               #xffffffff)))
    ;; Restore file state to where we were
    (%stbv-set-file-offset f (pp-page-start z))
    t))

;; Rarely used function to seek back to the preceding page while finding the
;; start of a packet
(defun %stbv-go-to-page-before (f limit-offset)
  (let ((previous-safe (if (and (>= limit-offset 65536) (>= (- limit-offset 65536) (vb-first-audio-page-offset f)))
                           (- limit-offset 65536)
                           (vb-first-audio-page-offset f))))
    (%stbv-set-file-offset f previous-safe)
    (loop
      (multiple-value-bind (found end) (%stbv-vorbis-find-page f)
        (unless found (return nil))
        (when (and (>= end limit-offset) (< (stb-vorbis-get-file-offset f) limit-offset))
          (return t))
        (%stbv-set-file-offset f end)))))

;; Implements the search logic for finding a page and starting decoding. If
;; the function succeeds, current_loc_valid will be true and current_loc will
;; be less than or equal to the provided sample number (the closer the
;; better).
(defun %stbv-seek-to-sample-coarse (f sample-number)
  (let* ((stream-length (stb-vorbis-stream-length-in-samples f))
         (padding 0) (last-sample-limit 0)
         (offset 0d0) (bytes-per-sample 0d0)
         (probe 0)
         (left nil) (right nil) (mid (make-stbv-probed-page)))
    (macrolet ((fail-error ()
                 `(progn (stb-vorbis-seek-start f)
                         (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-failed+)))))
      ;; Find the last page and validate the target sample
      (when (= stream-length 0) (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-without-length+)))
      (when (> sample-number stream-length) (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-invalid+)))
      ;; This is the maximum difference between the window-center (which is the
      ;; actual granule position value), and the right-start (which the spec
      ;; indicates should be the granule position (give or take one)).
      (setf padding (ash (- (vb-blocksize-1 f) (vb-blocksize-0 f)) -2)
            last-sample-limit (if (< sample-number padding) 0 (- sample-number padding)))
      (setf left (copy-stbv-probed-page (vb-p-first f)))
      (loop while (= (pp-last-decoded-sample left) #xffffffff)
            do ;; (untested) the first page does not have a 'last_decoded_sample'
               (%stbv-set-file-offset f (pp-page-end left))
               (unless (%stbv-get-seek-page-info f left) (fail-error)))
      (setf right (copy-stbv-probed-page (vb-p-last f)))
      ;; If we've already passed the target sample, seek to the start
      (when (<= last-sample-limit (pp-last-decoded-sample left))
        (if (stb-vorbis-seek-start f)
            (progn
              (when (> (vb-current-loc f) sample-number)
                (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-failed+)))
              (return-from %stbv-seek-to-sample-coarse t))
            (return-from %stbv-seek-to-sample-coarse nil)))
      (loop while (/= (pp-page-end left) (pp-page-start right))
            do (let ((delta (- (pp-page-start right) (pp-page-end left))))
                 (if (<= delta 65536)
                     ;; There's only 64K left to search - handle it linearly
                     (%stbv-set-file-offset f (pp-page-end left))
                     (progn
                       (if (< probe 2)
                           (progn
                             (if (= probe 0)
                                 ;; First probe (interpolate)
                                 (let ((data-bytes (float (- (pp-page-end right) (pp-page-start left)) 1d0)))
                                   (setf bytes-per-sample (/ data-bytes (pp-last-decoded-sample right))
                                         offset (+ (pp-page-start left)
                                                   (* bytes-per-sample (%u32 (- last-sample-limit (pp-last-decoded-sample left)))))))
                                 ;; Second probe (try to bound the other side)
                                 (let ((err (* (- (float last-sample-limit 1d0) (pp-last-decoded-sample mid)) bytes-per-sample)))
                                   (when (and (>= err 0) (< err 8000)) (setf err 8000d0))
                                   (when (and (< err 0) (> err -8000)) (setf err -8000d0))
                                   (incf offset (* err 2))))
                             ;; Ensure the offset is valid
                             (when (< offset (pp-page-end left)) (setf offset (float (pp-page-end left) 1d0)))
                             (when (> offset (%u32 (- (pp-page-start right) 65536)))
                               (setf offset (float (%u32 (- (pp-page-start right) 65536)) 1d0)))
                             (%stbv-set-file-offset f (%u32 (truncate offset))))
                           ;; Binary search for large ranges (offset by 32K to ensure
                           ;; we don't hit the right page)
                           (%stbv-set-file-offset f (- (+ (pp-page-end left) (floor delta 2)) 32768)))
                       (unless (%stbv-vorbis-find-page f) (fail-error))))
                 (loop
                   (unless (%stbv-get-seek-page-info f mid) (fail-error))
                   (unless (= (pp-last-decoded-sample mid) #xffffffff) (return))
                   ;; (untested) no frames end on this page
                   (%stbv-set-file-offset f (pp-page-end mid)))
                 ;; If we've just found the last page again then we're in a tricky file,
                 ;; and we're close enough (if it wasn't an interpolation probe).
                 (if (= (pp-page-start mid) (pp-page-start right))
                     (when (or (>= probe 2) (<= delta 65536))
                       (return))
                     (if (< last-sample-limit (pp-last-decoded-sample mid))
                         (setf right (copy-stbv-probed-page mid))
                         (setf left (copy-stbv-probed-page mid))))
                 (incf probe)))
      ;; Seek back to start of the last packet
      (let ((page-start (pp-page-start left))
            (end-pos 0) (start-seg-with-known-loc 0))
        (%stbv-set-file-offset f page-start)
        (unless (%stbv-start-page f) (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-failed+)))
        (setf end-pos (vb-end-seg-with-known-loc f))
        ;; Prefer to fail here
        (loop
          (let ((i (loop for i from end-pos above 0
                         unless (= (aref (vb-segments f) (- i 1)) 255) return i
                         finally (return 0))))
            (setf start-seg-with-known-loc i)
            (when (or (> start-seg-with-known-loc 0)
                      (not (logtest (vb-page-flag f) +stbv-pageflag-continued-packet+)))
              (return))
            ;; (untested) the final packet begins on an earlier page
            (unless (%stbv-go-to-page-before f page-start) (fail-error))
            (setf page-start (stb-vorbis-get-file-offset f))
            (unless (%stbv-start-page f) (fail-error))
            (setf end-pos (- (vb-segment-count f) 1))))
        ;; Prepare to start decoding
        (setf (vb-current-loc-valid f) nil
              (vb-last-seg f) nil
              (vb-valid-bits f) 0
              (vb-packet-bytes f) 0
              (vb-bytes-in-seg f) 0
              (vb-previous-length f) 0
              (vb-next-seg f) start-seg-with-known-loc)
        (dotimes (i start-seg-with-known-loc)
          (%stbv-skip f (aref (vb-segments f) i)))
        ;; Start decoding (optimizable - this frame is generally discarded)
        (unless (%stbv-vorbis-pump-first-frame f)
          (return-from %stbv-seek-to-sample-coarse nil))
        (when (> (vb-current-loc f) sample-number)
          (return-from %stbv-seek-to-sample-coarse (%stbv-error f +vorbis-seek-failed+)))
        t))))

;; The same as vorbis_decode_initial, but without advancing
(defun %stbv-peek-decode-initial (f)
  (multiple-value-bind (ok left-start left-end right-start right-end mode) (%stbv-vorbis-decode-initial f)
    (unless ok (return-from %stbv-peek-decode-initial nil))
    ;; Either 1 or 2 bytes were read, figure out which so we can rewind
    (let* ((bits-read (+ 1 (%stbv-ilog (- (vb-mode-count f) 1))
                         (if (/= (md-blockflag (aref (vb-mode-config f) mode)) 0) 2 0)))
           (bytes-read (floor (+ bits-read 7) 8)))
      (incf (vb-bytes-in-seg f) bytes-read)
      (decf (vb-packet-bytes f) bytes-read)
      (%stbv-skip f (- bytes-read))
      (if (= (vb-next-seg f) -1)
          (setf (vb-next-seg f) (- (vb-segment-count f) 1))
          (decf (vb-next-seg f)))
      (setf (vb-valid-bits f) 0)
      (values t left-start left-end right-start right-end mode))))

(defun stb-vorbis-seek-frame (f sample-number)
  "Seek to the frame containing SAMPLE-NUMBER"
  (unless (%stbv-seek-to-sample-coarse f sample-number)
    (return-from stb-vorbis-seek-frame nil))
  (let ((max-frame-samples (ash (- (* (vb-blocksize-1 f) 3) (vb-blocksize-0 f)) -2)))
    (loop while (< (vb-current-loc f) sample-number)
          do (multiple-value-bind (ok left-start left-end right-start) (%stbv-peek-decode-initial f)
               (declare (ignore left-end))
               (unless ok (return-from stb-vorbis-seek-frame (%stbv-error f +vorbis-seek-failed+)))
               ;; Calculate the number of samples returned by the next frame
               (let ((frame-samples (- right-start left-start)))
                 (cond ((> (+ (vb-current-loc f) frame-samples) sample-number)
                        (return-from stb-vorbis-seek-frame t))   ; The next frame will contain the sample
                       ((> (+ (vb-current-loc f) frame-samples max-frame-samples) sample-number)
                        ;; There's a chance the frame after this could contain the sample
                        (%stbv-vorbis-pump-first-frame f))
                       (t
                        ;; This frame is too early to be relevant
                        (setf (vb-current-loc f) (%u32 (+ (vb-current-loc f) frame-samples))
                              (vb-previous-length f) 0)
                        (%stbv-maybe-start-packet f)
                        (%stbv-flush-packet f))))))
    ;; The next frame should start with the sample
    (if (/= (vb-current-loc f) sample-number)
        (%stbv-error f +vorbis-seek-failed+)
        t)))

(defun stb-vorbis-seek-start (f)
  "Seek to the start of the stream"
  (%stbv-set-file-offset f (vb-first-audio-page-offset f))
  (setf (vb-previous-length f) 0
        (vb-first-decode f) t
        (vb-next-seg f) -1)
  (%stbv-vorbis-pump-first-frame f))

(defun stb-vorbis-stream-length-in-samples (f)
  "Get the total stream length in samples (per channel)"
  (when (= (vb-total-samples f) 0)
    (let ((restore-offset (stb-vorbis-get-file-offset f))
          (previous-safe 0) (end 0) (last 0) (last-page-loc 0))
      (block done
        ;; We need to find the last page, so search backwards from the end
        ;; (this is the "conservative" approach, any file could have been concatenated)
        (setf previous-safe (if (and (>= (vb-stream-len f) 65536)
                                     (>= (- (vb-stream-len f) 65536) (vb-first-audio-page-offset f)))
                                (- (vb-stream-len f) 65536)
                                (vb-first-audio-page-offset f)))
        (%stbv-set-file-offset f previous-safe)
        ;; previous_safe is now our candidate 'earliest known place that seeking
        ;; to will lead to the final page'
        (multiple-value-bind (found e l) (%stbv-vorbis-find-page f)
          (unless found
            (setf (vb-error f) +vorbis-cant-find-last-page+
                  (vb-total-samples f) #xffffffff)
            (return-from done))
          (setf end e last l))
        ;; Check if there are more pages
        (setf last-page-loc (stb-vorbis-get-file-offset f))
        ;; Stop when the last_page flag is set, not when we reach eof;
        ;; this allows us to stop short of a 'file_section' end without
        ;; explicitly checking the length of the section
        (loop while (= last 0)
              do (%stbv-set-file-offset f end)
                 (multiple-value-bind (found e l) (%stbv-vorbis-find-page f)
                   ;; The last page we found didn't have the 'last page' flag
                   ;; set. Whoops!
                   (unless found (return))
                   (setf end e last l))
                 (setf last-page-loc (stb-vorbis-get-file-offset f)))
        (%stbv-set-file-offset f last-page-loc)
        ;; Parse the header
        (let ((header (make-array 6 :element-type '(unsigned-byte 8))))
          (%stbv-getn f header 6))
        ;; Extract the absolute granule position
        (let ((lo (%stbv-get32 f))
              (hi (%stbv-get32 f)))
          (when (and (= lo #xffffffff) (= hi #xffffffff))
            (setf (vb-error f) +vorbis-cant-find-last-page+
                  (vb-total-samples f) +stbv-sample-unknown+)
            (return-from done))
          (when (/= hi 0) (setf lo #xfffffffe))   ; Saturate
          (setf (vb-total-samples f) lo
                (pp-page-start (vb-p-last f)) last-page-loc
                (pp-page-end (vb-p-last f)) end
                (pp-last-decoded-sample (vb-p-last f)) lo)))
      (%stbv-set-file-offset f restore-offset)))
  (if (= (vb-total-samples f) +stbv-sample-unknown+) 0 (vb-total-samples f)))

(defun stb-vorbis-stream-length-in-seconds (f)
  (/ (float (stb-vorbis-stream-length-in-samples f) 1f0) (float (vb-sample-rate f) 1f0)))

(defun stb-vorbis-get-frame-float (f)
  "Decode the next frame, returns (values samples channels outputs-start), samples is 0 at the end"
  (multiple-value-bind (ok len left right) (%stbv-vorbis-decode-packet f)
    (if (not ok)
        (progn (setf (vb-channel-buffer-start f) 0
                     (vb-channel-buffer-end f) 0)
               0)
        (let ((len (%stbv-vorbis-finish-frame f len left right)))
          (setf (vb-channel-buffer-start f) left
                (vb-channel-buffer-end f) (+ left len))
          (values len (vb-channels f) left)))))

(defun stb-vorbis-open-memory (data &optional (len (length data)))
  "Open an Ogg Vorbis stream from memory, returns (values decoder error)"
  (if (null data)
      (values nil +vorbis-unexpected-eof+)
      (let ((f (%make-stb-vorbis :data data :stream 0 :stream-end len :stream-len len)))
        (if (handler-case (%stbv-start-decoder f)
              ;; Reading out of bounds on corrupted data
              (error () (%stbv-error f +vorbis-invalid-setup+)))
            (progn
              (%stbv-vorbis-pump-first-frame f)
              (values f +vorbis-no-error+))
            (values nil (vb-error f))))))

(defun stb-vorbis-open-filename (file-name)
  "Open an Ogg Vorbis file, the whole file is loaded to memory"
  (multiple-value-bind (data size) (load-file-data file-name)
    (if data
        (stb-vorbis-open-memory data size)
        (values nil +vorbis-file-open-failure+))))

(defun stb-vorbis-close (f)
  (when f (setf (vb-data f) nil)))

;;;----------------------------------------------------------------------------------
;;; Integer conversion
;;;----------------------------------------------------------------------------------

;; FAST_SCALED_FLOAT_TO_INT(temp, x, 15): float addition of a magic number and
;; reinterpretation of the float bits as an integer
(declaim (inline %stbv-fast-scaled-float-to-int))
(defun %stbv-fast-scaled-float-to-int (x)
  (declare (type single-float x))
  (let* ((magic (+ (* 1.5f0 (ash 1 (- 23 15))) (/ 0.5f0 (ash 1 15))))
         (temp (+ x magic))
         (addend (+ (ash (- 150 15) 23) (ash 1 22)))
         (v (- (%f32->sbits temp) addend)))
    (if (> (%u32 (+ v 32768)) 65535)
        (if (< v 0) -32768 32767)
        v)))

(defun %stbv-convert-channels-short-interleaved (buf-c buffer b-offset data-c data d-offset len)
  (let ((limit (min buf-c data-c))
        (p b-offset))
    ;; NOTE: Channel mixing (buf_c != data_c with buf_c <= 2) is not ported, raudio always requests
    ;; the stream channel count
    (dotimes (j len)
      (dotimes (i limit)
        (setf (aref buffer p) (%stbv-fast-scaled-float-to-int (aref (aref data i) (+ d-offset j))))
        (incf p))
      (loop for i from limit below buf-c
            do (setf (aref buffer p) 0)
               (incf p)))))

(defun stb-vorbis-get-samples-short-interleaved (f channels buffer num-shorts &optional (buffer-start 0))
  "Decode NUM-SHORTS samples as interleaved s16 into BUFFER, returns the frames decoded"
  (let ((len (floor num-shorts channels))
        (n 0)
        (pos buffer-start))
    (loop while (< n len)
          do (let ((k (- (vb-channel-buffer-end f) (vb-channel-buffer-start f))))
               (when (>= (+ n k) len) (setf k (- len n)))
               (when (/= k 0)
                 (%stbv-convert-channels-short-interleaved channels buffer pos (vb-channels f) (vb-channel-buffers f)
                                                           (vb-channel-buffer-start f) k))
               (incf pos (* k channels))
               (incf n k)
               (incf (vb-channel-buffer-start f) k)
               (when (= n len) (return))
               (when (= (stb-vorbis-get-frame-float f) 0) (return))))
    n))
