(in-package #:cl-raylib)

;;;===================================================================================
;;; FLAC audio decoder
;;; Replaces the subset of raylib/src/external/dr_flac.h used by raudio
;;;
;;; Decodes native FLAC streams from memory: STREAMINFO, frames with CONSTANT, VERBATIM,
;;; FIXED and LPC subframes, partitioned Rice residuals and stereo decorrelation
;;; Output conversion to s16 follows dr_flac: samples are left-justified to 32 bits and
;;; the top 16 bits are taken
;;; NOTE: Ogg FLAC streams are not supported, frame CRCs are not verified
;;;===================================================================================

(define-condition %flac-end-of-data (error) ())

(defstruct (drflac (:constructor %make-drflac))
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (data-size 0 :type fixnum)
  (bit-pos 0 :type fixnum)                      ; Current read position in bits
  (first-frame-pos 0 :type fixnum)              ; Byte position of the first FLAC frame
  (sample-rate 0)
  (channels 0)
  (bits-per-sample 0)
  (max-block-size 0)
  (total-pcm-frame-count 0)
  (current-pcm-frame 0)
  ;; Current FLAC frame decoded samples (one array per channel, final values after decorrelation)
  (frame-samples nil)
  (frame-block-size 0)
  (frame-channels 0)
  (pcm-frames-remaining 0))

;;;----------------------------------------------------------------------------------
;;; Bit reader
;;;----------------------------------------------------------------------------------

(defun %flac-read-bits (flac n)
  "Read N bits as an unsigned integer (MSB first)"
  (declare (type fixnum n))
  (let ((data (drflac-data flac))
        (pos (drflac-bit-pos flac))
        (result 0))
    (declare (type fixnum pos))
    (when (> (+ pos n) (* (drflac-data-size flac) 8))
      (error '%flac-end-of-data))
    (loop while (> n 0)
          do (let* ((bit-off (logand pos 7))
                    (avail (- 8 bit-off))
                    (take (min avail n))
                    (bits (ldb (byte take (- avail take)) (aref data (ash pos -3)))))
               (setf result (logior (ash result take) bits))
               (incf pos take)
               (decf n take)))
    (setf (drflac-bit-pos flac) pos)
    result))

(defun %flac-read-signed (flac n)
  (if (= n 0)
      0
      (let ((x (%flac-read-bits flac n)))
        (if (logbitp (- n 1) x) (- x (ash 1 n)) x))))

(defun %flac-read-unary (flac)
  "Count zero bits up to the next one bit"
  (let ((data (drflac-data flac))
        (pos (drflac-bit-pos flac))
        (limit (* (drflac-data-size flac) 8))
        (count 0))
    (declare (type fixnum pos count))
    (loop
      (when (>= pos limit) (error '%flac-end-of-data))
      (let ((byte (aref data (ash pos -3)))
            (bit-off (logand pos 7)))
        (if (and (= bit-off 0) (= byte 0))
            (progn (incf count 8) (incf pos 8))
            (progn
              (incf pos)
              (if (logbitp (- 7 bit-off) byte)
                  (return)
                  (incf count))))))
    (setf (drflac-bit-pos flac) pos)
    count))

(defun %flac-align-to-byte (flac)
  (setf (drflac-bit-pos flac) (* (ceiling (drflac-bit-pos flac) 8) 8)))

;;;----------------------------------------------------------------------------------
;;; Opening
;;;----------------------------------------------------------------------------------

(defun drflac-open-memory (data &optional (data-size (length data)))
  "Open a FLAC stream from memory, returns NIL on failure"
  (let ((flac (%make-drflac :data data :data-size data-size)))
    (handler-case
        (progn
          ;; Skip ID3v2 tags
          (loop while (and (>= (- data-size (floor (drflac-bit-pos flac) 8)) 10)
                           (let ((p (floor (drflac-bit-pos flac) 8)))
                             (and (= (aref data p) (char-code #\I)) (= (aref data (+ p 1)) (char-code #\D))
                                  (= (aref data (+ p 2)) (char-code #\3)))))
                do (let* ((p (floor (drflac-bit-pos flac) 8))
                          (flags (aref data (+ p 5)))
                          (size (logior (ash (logand (aref data (+ p 6)) #x7f) 21) (ash (logand (aref data (+ p 7)) #x7f) 14)
                                        (ash (logand (aref data (+ p 8)) #x7f) 7) (logand (aref data (+ p 9)) #x7f))))
                     (when (logbitp 4 flags) (incf size 10))   ; Footer present
                     (setf (drflac-bit-pos flac) (* (+ p 10 size) 8))))
          (unless (= (%flac-read-bits flac 32) #x664c6143)   ; "fLaC"
            (return-from drflac-open-memory nil))
          ;; Metadata blocks, STREAMINFO must be the first one
          (let ((found-streaminfo nil))
            (loop
              (let* ((last-p (= (%flac-read-bits flac 1) 1))
                     (block-type (%flac-read-bits flac 7))
                     (block-size (%flac-read-bits flac 24))
                     (block-start (drflac-bit-pos flac)))
                (when (= block-type 0)
                  (%flac-read-bits flac 16)   ; Min block size
                  (setf (drflac-max-block-size flac) (%flac-read-bits flac 16))
                  (%flac-read-bits flac 24)   ; Min frame size
                  (%flac-read-bits flac 24)   ; Max frame size
                  (setf (drflac-sample-rate flac) (%flac-read-bits flac 20)
                        (drflac-channels flac) (+ (%flac-read-bits flac 3) 1)
                        (drflac-bits-per-sample flac) (+ (%flac-read-bits flac 5) 1)
                        (drflac-total-pcm-frame-count flac) (%flac-read-bits flac 36)
                        found-streaminfo t))
                (setf (drflac-bit-pos flac) (+ block-start (* block-size 8)))
                (when last-p (return))))
            (unless found-streaminfo
              (return-from drflac-open-memory nil)))
          (setf (drflac-first-frame-pos flac) (floor (drflac-bit-pos flac) 8))
          flac)
      (error () nil))))

(defun drflac-close (flac)
  (setf (drflac-data flac) nil))

;;;----------------------------------------------------------------------------------
;;; Frame decoding
;;;----------------------------------------------------------------------------------

;; Read the UTF-8 like coded frame/sample number
(defun %flac-read-utf8-number (flac)
  (let* ((first (%flac-read-bits flac 8))
         (extra (cond ((< first #x80) 0) ((< first #xe0) 1) ((< first #xf0) 2)
                      ((< first #xf8) 3) ((< first #xfc) 4) ((< first #xfe) 5) (t 6)))
         (value (if (= extra 0) first (logand first (- (ash 1 (- 6 extra)) 1)))))
    (dotimes (i extra value)
      (setf value (logior (ash value 6) (logand (%flac-read-bits flac 8) #x3f))))))

;; Decode the residual into SAMPLES after the warm-up samples
(defun %flac-decode-residual (flac samples block-size order)
  (let* ((method (%flac-read-bits flac 2))
         (param-bits (if (= method 0) 4 5))
         (escape (if (= method 0) 15 31))
         (partition-order (%flac-read-bits flac 4))
         (partitions (ash 1 partition-order))
         (pos order))
    (when (> method 1) (error '%flac-end-of-data))
    (dotimes (p partitions)
      (let ((count (if (= p 0)
                       (- (ash block-size (- partition-order)) order)
                       (ash block-size (- partition-order))))
            (param (%flac-read-bits flac param-bits)))
        (if (= param escape)
            (let ((bits (%flac-read-bits flac 5)))
              (dotimes (i count)
                (setf (aref samples pos) (%flac-read-signed flac bits))
                (incf pos)))
            (dotimes (i count)
              (let* ((q (%flac-read-unary flac))
                     (u (logior (ash q param) (%flac-read-bits flac param))))
                ;; Zigzag decoding
                (setf (aref samples pos) (if (logbitp 0 u) (- (ash (+ u 1) -1)) (ash u -1)))
                (incf pos))))))))

(defun %flac-decode-subframe (flac block-size bps)
  "Decode a subframe, returns the samples (wasted bits restored)"
  (let ((samples (make-array block-size :initial-element 0)))
    (unless (= (%flac-read-bits flac 1) 0) (error '%flac-end-of-data))   ; Zero padding bit
    (let* ((type (%flac-read-bits flac 6))
           (wasted (if (= (%flac-read-bits flac 1) 1) (+ (%flac-read-unary flac) 1) 0))
           (bps (- bps wasted)))
      (cond
        ;; CONSTANT
        ((= type 0)
         (fill samples (%flac-read-signed flac bps)))
        ;; VERBATIM
        ((= type 1)
         (dotimes (i block-size)
           (setf (aref samples i) (%flac-read-signed flac bps))))
        ;; FIXED
        ((<= 8 type 12)
         (let ((order (- type 8)))
           (dotimes (i order)
             (setf (aref samples i) (%flac-read-signed flac bps)))
           (%flac-decode-residual flac samples block-size order)
           (loop for i from order below block-size
                 do (incf (aref samples i)
                          (ecase order
                            (0 0)
                            (1 (aref samples (- i 1)))
                            (2 (- (* 2 (aref samples (- i 1))) (aref samples (- i 2))))
                            (3 (+ (- (* 3 (aref samples (- i 1))) (* 3 (aref samples (- i 2)))) (aref samples (- i 3))))
                            (4 (+ (- (* 4 (aref samples (- i 1))) (* 6 (aref samples (- i 2))))
                                  (- (* 4 (aref samples (- i 3))) (aref samples (- i 4))))))))))
        ;; LPC
        ((>= type 32)
         (let ((order (+ (- type 32) 1)))
           (dotimes (i order)
             (setf (aref samples i) (%flac-read-signed flac bps)))
           (let* ((precision (+ (%flac-read-bits flac 4) 1))
                  (shift (%flac-read-signed flac 5))
                  (coefficients (make-array order)))
             (when (= precision 16) (error '%flac-end-of-data))   ; Invalid precision
             (dotimes (i order)
               (setf (aref coefficients i) (%flac-read-signed flac precision)))
             (%flac-decode-residual flac samples block-size order)
             (loop for i from order below block-size
                   do (let ((sum 0))
                        (dotimes (j order)
                          (incf sum (* (aref coefficients j) (aref samples (- i j 1)))))
                        (incf (aref samples i) (ash sum (- shift))))))))
        (t (error '%flac-end-of-data)))
      (unless (= wasted 0)
        (dotimes (i block-size)
          (setf (aref samples i) (ash (aref samples i) wasted))))
      samples)))

;; drflac__read_and_decode_next_flac_frame()
(defun %flac-read-and-decode-next-frame (flac)
  (handler-case
      (progn
        (%flac-align-to-byte flac)
        ;; Find the frame sync code
        (loop
          (when (>= (+ (floor (drflac-bit-pos flac) 8) 2) (drflac-data-size flac))
            (return-from %flac-read-and-decode-next-frame nil))
          (let ((p (floor (drflac-bit-pos flac) 8))
                (data (drflac-data flac)))
            (if (and (= (aref data p) #xff) (= (logand (aref data (+ p 1)) #xfe) #xf8))
                (return)
                (incf (drflac-bit-pos flac) 8))))
        (%flac-read-bits flac 15)               ; Sync code and reserved bit
        (%flac-read-bits flac 1)                ; Blocking strategy
        (let* ((block-size-code (%flac-read-bits flac 4))
               (sample-rate-code (%flac-read-bits flac 4))
               (channel-assignment (%flac-read-bits flac 4))
               (sample-size-code (%flac-read-bits flac 3))
               (block-size 0)
               (bps 0))
          (%flac-read-bits flac 1)              ; Reserved
          (%flac-read-utf8-number flac)         ; Frame/sample number
          (setf block-size
                (cond ((= block-size-code 1) 192)
                      ((<= 2 block-size-code 5) (* 576 (ash 1 (- block-size-code 2))))
                      ((= block-size-code 6) (+ (%flac-read-bits flac 8) 1))
                      ((= block-size-code 7) (+ (%flac-read-bits flac 16) 1))
                      ((<= 8 block-size-code 15) (* 256 (ash 1 (- block-size-code 8))))
                      (t (error '%flac-end-of-data))))
          (case sample-rate-code
            (12 (%flac-read-bits flac 8))
            ((13 14) (%flac-read-bits flac 16)))
          (setf bps (case sample-size-code
                      (0 (drflac-bits-per-sample flac))
                      (1 8) (2 12) (4 16) (5 20) (6 24) (7 32)
                      (t (error '%flac-end-of-data))))
          (%flac-read-bits flac 8)              ; CRC-8
          (let* ((channels (if (< channel-assignment 8) (+ channel-assignment 1) 2))
                 (subframes (make-array channels)))
            (when (> channel-assignment 10) (error '%flac-end-of-data))
            (dotimes (c channels)
              ;; Side channels have one extra bit of precision
              (let ((side-p (or (and (= channel-assignment 8) (= c 1))
                                (and (= channel-assignment 9) (= c 0))
                                (and (= channel-assignment 10) (= c 1)))))
                (setf (svref subframes c) (%flac-decode-subframe flac block-size (if side-p (+ bps 1) bps)))))
            ;; Stereo decorrelation
            (let ((s0 (when (>= channels 2) (svref subframes 0)))
                  (s1 (when (>= channels 2) (svref subframes 1))))
              (case channel-assignment
                (8 (dotimes (i block-size) (setf (aref s1 i) (- (aref s0 i) (aref s1 i)))))   ; Left/side
                (9 (dotimes (i block-size) (setf (aref s0 i) (+ (aref s0 i) (aref s1 i)))))   ; Right/side
                (10 (dotimes (i block-size)                                                   ; Mid/side
                      (let* ((side (aref s1 i))
                             (mid (logior (ash (aref s0 i) 1) (logand side 1))))
                        (setf (aref s0 i) (ash (+ mid side) -1)
                              (aref s1 i) (ash (- mid side) -1)))))))
            (%flac-align-to-byte flac)
            (%flac-read-bits flac 16)           ; CRC-16
            (setf (drflac-frame-samples flac) subframes
                  (drflac-frame-block-size flac) block-size
                  (drflac-frame-channels flac) channels
                  (drflac-pcm-frames-remaining flac) block-size)
            t)))
    (error () nil)))

;;;----------------------------------------------------------------------------------
;;; Reading and seeking
;;;----------------------------------------------------------------------------------

(defun drflac-read-pcm-frames-s16 (flac frames-to-read out &optional (out-pos 0))
  "Read FRAMES-TO-READ frames as s16 into OUT (a (signed-byte 16) array, or NIL to skip)"
  (let ((frames-read 0)
        (unused-bits-per-sample (- 32 (drflac-bits-per-sample flac))))
    (loop while (> frames-to-read 0)
          do (if (= (drflac-pcm-frames-remaining flac) 0)
                 (unless (%flac-read-and-decode-next-frame flac)
                   (return))
                 (let* ((channels (drflac-frame-channels flac))
                        (first-frame (- (drflac-frame-block-size flac) (drflac-pcm-frames-remaining flac)))
                        (count (min frames-to-read (drflac-pcm-frames-remaining flac)))
                        (subframes (drflac-frame-samples flac)))
                   (when out
                     (dotimes (i count)
                       (dotimes (j channels)
                         (setf (aref out (+ out-pos (* i channels) j))
                               (ash (%i32 (ash (aref (svref subframes j) (+ first-frame i)) unused-bits-per-sample)) -16)))))
                   (incf frames-read count)
                   (incf out-pos (* count channels))
                   (decf frames-to-read count)
                   (incf (drflac-current-pcm-frame flac) count)
                   (decf (drflac-pcm-frames-remaining flac) count))))
    frames-read))

(defun drflac-seek-to-first-frame (flac)
  (setf (drflac-bit-pos flac) (* (drflac-first-frame-pos flac) 8)
        (drflac-current-pcm-frame flac) 0
        (drflac-pcm-frames-remaining flac) 0)
  t)

(defun drflac-seek-to-pcm-frame (flac pcm-frame-index)
  "Seek decoding forward from the closest position"
  (when (and (> (drflac-total-pcm-frame-count flac) 0)
             (> pcm-frame-index (drflac-total-pcm-frame-count flac)))
    (setf pcm-frame-index (drflac-total-pcm-frame-count flac)))
  (when (< pcm-frame-index (drflac-current-pcm-frame flac))
    (drflac-seek-to-first-frame flac))
  (let ((offset (- pcm-frame-index (drflac-current-pcm-frame flac))))
    (= (drflac-read-pcm-frames-s16 flac offset nil) offset)))

(defun drflac-open-memory-and-read-pcm-frames-s16 (data data-size)
  "Decode a whole FLAC stream, returns (values samples channels sample-rate total-frame-count) or NIL"
  (let ((flac (drflac-open-memory data data-size)))
    (when flac
      (let ((total (drflac-total-pcm-frame-count flac))
            (channels (drflac-channels flac)))
        (if (> total 0)
            (let* ((samples (make-array (* total channels) :element-type '(signed-byte 16) :initial-element 0))
                   (frames-read (drflac-read-pcm-frames-s16 flac total samples 0)))
              (values samples channels (drflac-sample-rate flac) frames-read))
            ;; Unknown length, read in chunks
            (let ((chunks '()) (frames-read 0) (chunk-frames 4096))
              (loop (let* ((chunk (make-array (* chunk-frames channels) :element-type '(signed-byte 16) :initial-element 0))
                           (n (drflac-read-pcm-frames-s16 flac chunk-frames chunk 0)))
                      (when (= n 0) (return))
                      (push (subseq chunk 0 (* n channels)) chunks)
                      (incf frames-read n)))
              (values (apply #'concatenate '(simple-array (signed-byte 16) (*)) (nreverse chunks))
                      channels (drflac-sample-rate flac) frames-read)))))))
