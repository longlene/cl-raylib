(in-package #:cl-raylib)

;;;===================================================================================
;;; QOA - The "Quite OK Audio" format for fast, lossy audio compression
;;; Port of raylib/src/external/qoa.h and raylib/src/external/qoaplay.c
;;;
;;; NOTE: qoaplay always reads from memory, qoaplay_open() loads the whole file
;;;===================================================================================

(defconstant +qoa-min-filesize+ 16)
(defconstant +qoa-max-channels+ 8)
(defconstant +qoa-slice-len+ 20)
(defconstant +qoa-slices-per-frame+ 256)
(defconstant +qoa-frame-len+ (* +qoa-slices-per-frame+ +qoa-slice-len+))
(defconstant +qoa-lms-len+ 4)
(defconstant +qoa-magic+ #x716f6166)    ; 'qoaf'

(defun qoa-frame-size (channels slices)
  (+ 8 (* +qoa-lms-len+ 4 channels) (* 8 slices channels)))

(defstruct (qoa-lms (:copier %copy-qoa-lms))
  (history (make-array +qoa-lms-len+ :element-type '(signed-byte 32) :initial-element 0)
   :type (simple-array (signed-byte 32) (4)))
  (weights (make-array +qoa-lms-len+ :element-type '(signed-byte 32) :initial-element 0)
   :type (simple-array (signed-byte 32) (4))))

(defun copy-qoa-lms (lms)
  (make-qoa-lms :history (copy-seq (qoa-lms-history lms)) :weights (copy-seq (qoa-lms-weights lms))))

(defstruct qoa-desc
  (channels 0)
  (samplerate 0)
  (samples 0)
  (lms (let ((v (make-array +qoa-max-channels+)))
         (dotimes (i +qoa-max-channels+ v) (setf (svref v i) (make-qoa-lms))))))

;; The quant_tab provides an index into the dequant_tab for residuals in the range of -8 .. 8
(alexandria:define-constant +qoa-quant-tab+
  #(7 7 7 5 5 3 3 1 0 0 2 2 4 4 6 6 6) :test #'equalp)

(alexandria:define-constant +qoa-scalefactor-tab+
  #(1 7 21 45 84 138 211 304 421 562 731 928 1157 1419 1715 2048) :test #'equalp)

;; Rounded reciprocals 1/scalefactor in .16 fixed point
(alexandria:define-constant +qoa-reciprocal-tab+
  #(65536 9363 3121 1457 781 475 311 216 156 117 90 71 57 47 39 32) :test #'equalp)

(alexandria:define-constant +qoa-dequant-tab+
  #2A((   1    -1     3    -3     5    -5      7     -7)
      (   5    -5    18   -18    32   -32     49    -49)
      (  16   -16    53   -53    95   -95    147   -147)
      (  34   -34   113  -113   203  -203    315   -315)
      (  63   -63   210  -210   378  -378    588   -588)
      ( 104  -104   345  -345   621  -621    966   -966)
      ( 158  -158   528  -528   950  -950   1477  -1477)
      ( 228  -228   760  -760  1368 -1368   2128  -2128)
      ( 316  -316  1053 -1053  1895 -1895   2947  -2947)
      ( 422  -422  1405 -1405  2529 -2529   3934  -3934)
      ( 548  -548  1828 -1828  3290 -3290   5117  -5117)
      ( 696  -696  2320 -2320  4176 -4176   6496  -6496)
      ( 868  -868  2893 -2893  5207 -5207   8099  -8099)
      (1064 -1064  3548 -3548  6386 -6386   9933  -9933)
      (1286 -1286  4288 -4288  7718 -7718  12005 -12005)
      (1536 -1536  5120 -5120  9216 -9216  14336 -14336))
  :test #'equalp)

;; The Least Mean Squares Filter predicts the next sample based on the previous 4 reconstructed samples
(declaim (inline qoa-lms-predict qoa-lms-update qoa-div qoa-clamp qoa-clamp-s16))
(defun qoa-lms-predict (lms)
  (let ((prediction 0)
        (history (qoa-lms-history lms))
        (weights (qoa-lms-weights lms)))
    (dotimes (i +qoa-lms-len+)
      (setf prediction (%i32 (+ prediction (* (aref weights i) (aref history i))))))
    (ash prediction -13)))

(defun qoa-lms-update (lms sample residual)
  (let ((delta (ash residual -4))
        (history (qoa-lms-history lms))
        (weights (qoa-lms-weights lms)))
    (dotimes (i +qoa-lms-len+)
      (setf (aref weights i) (%i32 (+ (aref weights i) (if (< (aref history i) 0) (- delta) delta)))))
    (dotimes (i (- +qoa-lms-len+ 1))
      (setf (aref history i) (aref history (+ i 1))))
    (setf (aref history (- +qoa-lms-len+ 1)) sample)))

;; Rounding division that avoids rounding to zero for small numbers
(defun qoa-div (v scalefactor)
  (let* ((reciprocal (svref +qoa-reciprocal-tab+ scalefactor))
         (n (ash (%i32 (+ (%i32 (* v reciprocal)) (ash 1 15))) -16)))
    (+ n (- (signum v) (signum n)))))    ; Round away from 0

(defun qoa-clamp (v min max)
  (cond ((< v min) min) ((> v max) max) (t v)))

(defun qoa-clamp-s16 (v)
  (cond ((< v -32768) -32768) ((> v 32767) 32767) (t v)))

(defun %qoa-read-u64 (bytes p)
  (let ((v 0))
    (dotimes (i 8 v)
      (setf v (logior (ash v 8) (aref bytes (+ p i)))))))

(defun %qoa-write-u64 (v bytes p)
  (dotimes (i 8)
    (setf (aref bytes (+ p i)) (ldb (byte 8 (* (- 7 i) 8)) v))))

;;;----------------------------------------------------------------------------------
;;; Encoder
;;;----------------------------------------------------------------------------------

(defun qoa-encode-header (qoa bytes p)
  (%qoa-write-u64 (logior (ash +qoa-magic+ 32) (qoa-desc-samples qoa)) bytes p)
  8)

;; Encode FRAME-LEN frames of SAMPLE-DATA starting at sample SAMPLE-START, returns the bytes written
(defun qoa-encode-frame (sample-data sample-start qoa frame-len bytes start)
  (let* ((channels (qoa-desc-channels qoa))
         (p start)
         (slices (floor (+ frame-len +qoa-slice-len+ -1) +qoa-slice-len+))
         (frame-size (qoa-frame-size channels slices))
         (prev-scalefactor (make-array +qoa-max-channels+ :initial-element 0)))
    ;; Write the frame header
    (%qoa-write-u64 (logior (ash channels 56) (ash (qoa-desc-samplerate qoa) 32) (ash frame-len 16) frame-size) bytes p)
    (incf p 8)
    ;; Write the current LMS state
    (dotimes (c channels)
      (let ((weights 0) (history 0) (lms (svref (qoa-desc-lms qoa) c)))
        (dotimes (i +qoa-lms-len+)
          (setf history (logior (ash history 16) (logand (aref (qoa-lms-history lms) i) #xffff))
                weights (logior (ash weights 16) (logand (aref (qoa-lms-weights lms) i) #xffff))))
        (%qoa-write-u64 history bytes p) (incf p 8)
        (%qoa-write-u64 weights bytes p) (incf p 8)))
    ;; Encode all samples with the channels interleaved on a slice level
    (loop for sample-index from 0 below frame-len by +qoa-slice-len+
          do (dotimes (c channels)
               (let* ((slice-len (qoa-clamp +qoa-slice-len+ 0 (- frame-len sample-index)))
                      (slice-start (+ (* sample-index channels) c))
                      (slice-end (+ (* (+ sample-index slice-len) channels) c))
                      (best-rank #xffffffffffffffff)
                      (best-slice 0)
                      (best-lms nil)
                      (best-scalefactor 0))
                 ;; Brute force search for the best scalefactor
                 (dotimes (sfi 16)
                   ;; Start testing the best scalefactor of the previous slice first
                   (let* ((scalefactor (logand (+ sfi (aref prev-scalefactor c)) 15))
                          (lms (copy-qoa-lms (svref (qoa-desc-lms qoa) c)))
                          (slice scalefactor)
                          (current-rank 0))
                     (loop for si from slice-start below slice-end by channels
                           do (let* ((sample (aref sample-data (+ sample-start si)))
                                     (predicted (qoa-lms-predict lms))
                                     (residual (- sample predicted))
                                     (scaled (qoa-div residual scalefactor))
                                     (clamped (qoa-clamp scaled -8 8))
                                     (quantized (svref +qoa-quant-tab+ (+ clamped 8)))
                                     (dequantized (aref +qoa-dequant-tab+ scalefactor quantized))
                                     (reconstructed (qoa-clamp-s16 (+ predicted dequantized)))
                                     ;; If the weights have grown too large, introduce a penalty
                                     (w (qoa-lms-weights lms))
                                     (weights-penalty (max 0 (- (ash (%i32 (+ (* (aref w 0) (aref w 0)) (* (aref w 1) (aref w 1))
                                                                               (* (aref w 2) (aref w 2)) (* (aref w 3) (aref w 3))))
                                                                     -18)
                                                                #x8ff)))
                                     (err (- sample reconstructed)))
                                (setf current-rank (logand (+ current-rank (* err err) (* weights-penalty weights-penalty))
                                                           #xffffffffffffffff))
                                (when (> current-rank best-rank) (return))
                                (qoa-lms-update lms reconstructed dequantized)
                                (setf slice (logand (logior (ash slice 3) quantized) #xffffffffffffffff))))
                     (when (< current-rank best-rank)
                       (setf best-rank current-rank
                             best-slice slice
                             best-lms lms
                             best-scalefactor scalefactor))))
                 (setf (aref prev-scalefactor c) best-scalefactor
                       (svref (qoa-desc-lms qoa) c) best-lms)
                 ;; Left-shift all encoded data of a slice shorter than QOA_SLICE_LEN
                 (setf best-slice (logand (ash best-slice (* (- +qoa-slice-len+ slice-len) 3)) #xffffffffffffffff))
                 (%qoa-write-u64 best-slice bytes p)
                 (incf p 8))))
    (- p start)))

(defun qoa-encode (sample-data qoa)
  "Encode SAMPLE-DATA (interleaved s16 samples), returns (values bytes size) or NIL"
  (when (or (= (qoa-desc-samples qoa) 0)
            (= (qoa-desc-samplerate qoa) 0) (> (qoa-desc-samplerate qoa) #xffffff)
            (= (qoa-desc-channels qoa) 0) (> (qoa-desc-channels qoa) +qoa-max-channels+))
    (return-from qoa-encode nil))
  (let* ((samples (qoa-desc-samples qoa))
         (channels (qoa-desc-channels qoa))
         (num-frames (floor (+ samples +qoa-frame-len+ -1) +qoa-frame-len+))
         (num-slices (floor (+ samples +qoa-slice-len+ -1) +qoa-slice-len+))
         (encoded-size (+ 8 (* num-frames 8) (* num-frames +qoa-lms-len+ 4 channels) (* num-slices 8 channels)))
         (bytes (make-array encoded-size :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set the initial LMS weights to {0, 0, -1, 2}
    (dotimes (c channels)
      (let ((lms (make-qoa-lms)))
        (setf (aref (qoa-lms-weights lms) 2) (- (ash 1 13))
              (aref (qoa-lms-weights lms) 3) (ash 1 14)
              (svref (qoa-desc-lms qoa) c) lms)))
    (let ((p (qoa-encode-header qoa bytes 0)))
      (loop for sample-index from 0 below samples by +qoa-frame-len+
            do (let ((frame-len (qoa-clamp +qoa-frame-len+ 0 (- samples sample-index))))
                 (incf p (qoa-encode-frame sample-data (* sample-index channels) qoa frame-len bytes p))))
      (values bytes p))))

;;;----------------------------------------------------------------------------------
;;; Decoder
;;;----------------------------------------------------------------------------------

(defun qoa-max-frame-size (qoa)
  (qoa-frame-size (qoa-desc-channels qoa) +qoa-slices-per-frame+))

(defun qoa-decode-header (bytes size qoa &optional (start 0))
  (when (< size +qoa-min-filesize+)
    (return-from qoa-decode-header 0))
  ;; Read the file header, verify the magic number ('qoaf') and read the total number of samples
  (let ((file-header (%qoa-read-u64 bytes start)))
    (unless (= (ash file-header -32) +qoa-magic+)
      (return-from qoa-decode-header 0))
    (setf (qoa-desc-samples qoa) (logand file-header #xffffffff))
    (when (= (qoa-desc-samples qoa) 0)
      (return-from qoa-decode-header 0))
    ;; Peek into the first frame header to get the number of channels and the samplerate
    (let ((frame-header (%qoa-read-u64 bytes (+ start 8))))
      (setf (qoa-desc-channels qoa) (logand (ash frame-header -56) #xff)
            (qoa-desc-samplerate qoa) (logand (ash frame-header -32) #xffffff))
      (if (or (= (qoa-desc-channels qoa) 0) (= (qoa-desc-samplerate qoa) 0)
              (> (qoa-desc-channels qoa) +qoa-max-channels+))
          0
          8))))

;; Decode one frame from BYTES at START into SAMPLE-DATA at sample SAMPLE-START
;; Returns (values bytes-read frame-len)
(defun qoa-decode-frame (bytes start size qoa sample-data sample-start)
  (let ((channels-desc (qoa-desc-channels qoa)))
    (when (< size (+ 8 (* +qoa-lms-len+ 4 channels-desc)))
      (return-from qoa-decode-frame (values 0 0)))
    ;; Read and verify the frame header
    (let* ((p start)
           (frame-header (prog1 (%qoa-read-u64 bytes p) (incf p 8)))
           (channels (logand (ash frame-header -56) #xff))
           (samplerate (logand (ash frame-header -32) #xffffff))
           (samples (logand (ash frame-header -16) #xffff))
           (frame-size (logand frame-header #xffff))
           (header-size (+ 8 (* +qoa-lms-len+ 4 channels)))
           (data-size (- frame-size header-size))
           (max-total-slices (floor data-size 8))
           (num-slices (floor (+ samples +qoa-slice-len+ -1) +qoa-slice-len+)))
      (when (or (/= channels channels-desc)
                (/= samplerate (qoa-desc-samplerate qoa))
                (< frame-size header-size)
                (> frame-size size)
                (> num-slices +qoa-slices-per-frame+)
                (> (* num-slices channels) max-total-slices))
        (return-from qoa-decode-frame (values 0 0)))
      ;; Read the LMS state: 4 x 2 bytes history, 4 x 2 bytes weights per channel
      (dotimes (c channels)
        (let ((history (%qoa-read-u64 bytes p))
              (weights (%qoa-read-u64 bytes (+ p 8)))
              (lms (svref (qoa-desc-lms qoa) c)))
          (incf p 16)
          (dotimes (i +qoa-lms-len+)
            (setf (aref (qoa-lms-history lms) i) (%s16 (ldb (byte 16 (* (- 3 i) 16)) history))
                  (aref (qoa-lms-weights lms) i) (%s16 (ldb (byte 16 (* (- 3 i) 16)) weights))))))
      ;; Decode all slices for all channels in this frame
      (loop for sample-index from 0 below samples by +qoa-slice-len+
            do (dotimes (c channels)
                 (let* ((slice (%qoa-read-u64 bytes p))
                        (scalefactor (logand (ash slice -60) #xf))
                        (slice-start (+ (* sample-index channels) c))
                        (slice-end (+ (* (qoa-clamp (+ sample-index +qoa-slice-len+) 0 samples) channels) c))
                        (lms (svref (qoa-desc-lms qoa) c))
                        (shift 57))                   ; Skip the scalefactor bits
                   (incf p 8)
                   (loop for si from slice-start below slice-end by channels
                         do (let* ((predicted (qoa-lms-predict lms))
                                   (quantized (logand (ash slice (- shift)) #x7))
                                   (dequantized (aref +qoa-dequant-tab+ scalefactor quantized))
                                   (reconstructed (qoa-clamp-s16 (+ predicted dequantized))))
                              (setf (aref sample-data (+ sample-start si)) reconstructed)
                              (decf shift 3)
                              (qoa-lms-update lms reconstructed dequantized))))))
      (values (- p start) samples))))

(defun qoa-decode (bytes size qoa)
  "Decode a QOA file in memory, returns the s16 sample data or NIL"
  (let ((p (qoa-decode-header bytes size qoa)))
    (when (= p 0)
      (return-from qoa-decode nil))
    ;; Calculate the required size of the sample buffer, round up to full frames
    (let* ((num-frames (floor (+ (qoa-desc-samples qoa) +qoa-frame-len+ -1) +qoa-frame-len+))
           (total-samples (* num-frames +qoa-frame-len+ (qoa-desc-channels qoa))))
      (when (> total-samples #x7fffffff)
        (return-from qoa-decode nil))
      (let ((sample-data (make-array total-samples :element-type '(signed-byte 16) :initial-element 0))
            (sample-index 0))
        ;; Decode all frames
        (loop
          (multiple-value-bind (frame-size frame-len)
              (qoa-decode-frame bytes p (- size p) qoa sample-data (* sample-index (qoa-desc-channels qoa)))
            (incf p frame-size)
            (incf sample-index frame-len)
            (unless (and (= frame-len +qoa-frame-len+) (< sample-index (qoa-desc-samples qoa)))
              (return))))
        (setf (qoa-desc-samples qoa) sample-index)
        sample-data))))

(defun qoa-write (file-name sample-data qoa)
  "Encode and write sample data to a QOA file, returns the bytes written or 0"
  (multiple-value-bind (encoded size) (qoa-encode sample-data qoa)
    (if (and encoded (save-file-data file-name encoded size))
        size
        0)))

;;;----------------------------------------------------------------------------------
;;; qoaplay - QOA stream playing helper functions
;;;----------------------------------------------------------------------------------

(defstruct qoaplay-desc
  (info (make-qoa-desc))                ; QOA descriptor data
  (file-data nil)                       ; QOA file data on memory
  (file-data-size 0)                    ; QOA file data on memory size
  (file-data-offset 0)                  ; QOA file data on memory offset for next read
  (first-frame-pos 0)                   ; First frame position (after QOA header, required for offset)
  (sample-position 0)                   ; Current streaming sample position
  (sample-data nil)                     ; Sample data decoded
  (sample-data-len 0)                   ; Sample data decoded length
  (sample-data-pos 0))                  ; Sample data decoded position

(defun qoaplay-open-memory (data data-size)
  (when (< data-size +qoa-min-filesize+)
    (return-from qoaplay-open-memory nil))
  (let* ((qoa (make-qoa-desc))
         (first-frame-pos (qoa-decode-header data +qoa-min-filesize+ qoa)))
    (when (= first-frame-pos 0)
      (return-from qoaplay-open-memory nil))
    (make-qoaplay-desc :info qoa
                       :file-data (subseq data 0 data-size)
                       :file-data-size data-size
                       :file-data-offset first-frame-pos
                       :first-frame-pos first-frame-pos
                       :sample-data (make-array (* (qoa-desc-channels qoa) +qoa-frame-len+ 2)
                                                :element-type '(signed-byte 16) :initial-element 0))))

;; NOTE: The file is loaded to memory instead of being read frame by frame
(defun qoaplay-open (path)
  (multiple-value-bind (data size) (load-file-data path)
    (when data (qoaplay-open-memory data size))))

(defun qoaplay-close (qoa-ctx)
  (setf (qoaplay-desc-file-data-size qoa-ctx) 0
        (qoaplay-desc-file-data qoa-ctx) nil))

(defun qoaplay-decode-frame (qoa-ctx)
  ;; NOTE: Buffer length is clamped to the data available (like fread() does for files)
  (let* ((offset (qoaplay-desc-file-data-offset qoa-ctx))
         (buffer-len (max 0 (min (qoa-max-frame-size (qoaplay-desc-info qoa-ctx))
                                 (- (qoaplay-desc-file-data-size qoa-ctx) offset)))))
    (incf (qoaplay-desc-file-data-offset qoa-ctx) (qoa-max-frame-size (qoaplay-desc-info qoa-ctx)))
    (let ((frame-len (nth-value 1 (qoa-decode-frame (qoaplay-desc-file-data qoa-ctx) offset buffer-len
                                                    (qoaplay-desc-info qoa-ctx) (qoaplay-desc-sample-data qoa-ctx) 0))))
      (setf (qoaplay-desc-sample-data-pos qoa-ctx) 0
            (qoaplay-desc-sample-data-len qoa-ctx) frame-len)
      frame-len)))

(defun qoaplay-rewind (qoa-ctx)
  (setf (qoaplay-desc-file-data-offset qoa-ctx) (qoaplay-desc-first-frame-pos qoa-ctx)
        (qoaplay-desc-sample-position qoa-ctx) 0
        (qoaplay-desc-sample-data-len qoa-ctx) 0
        (qoaplay-desc-sample-data-pos qoa-ctx) 0))

;; Decode NUM-SAMPLES frames as normalized float samples into SAMPLE-DATA, rewinds at the end
(defun qoaplay-decode (qoa-ctx sample-data num-samples)
  (let* ((channels (qoa-desc-channels (qoaplay-desc-info qoa-ctx)))
         (src-index (* (qoaplay-desc-sample-data-pos qoa-ctx) channels))
         (dst-index 0)
         (decoded (qoaplay-desc-sample-data qoa-ctx)))
    (dotimes (i num-samples)
      (when (= (- (qoaplay-desc-sample-data-len qoa-ctx) (qoaplay-desc-sample-data-pos qoa-ctx)) 0)
        (when (= (qoaplay-decode-frame qoa-ctx) 0)
          (qoaplay-rewind qoa-ctx)
          (qoaplay-decode-frame qoa-ctx))
        (setf src-index 0))
      (dotimes (c channels)
        (setf (aref sample-data dst-index) (coerce (/ (aref decoded src-index) 32768d0) 'single-float))
        (incf dst-index)
        (incf src-index))
      (incf (qoaplay-desc-sample-data-pos qoa-ctx))
      (incf (qoaplay-desc-sample-position qoa-ctx)))
    num-samples))

(defun qoaplay-get-duration (qoa-ctx)
  (/ (float (qoa-desc-samples (qoaplay-desc-info qoa-ctx)) 1d0)
     (float (qoa-desc-samplerate (qoaplay-desc-info qoa-ctx)) 1d0)))

(defun qoaplay-get-time (qoa-ctx)
  (/ (float (qoaplay-desc-sample-position qoa-ctx) 1d0)
     (float (qoa-desc-samplerate (qoaplay-desc-info qoa-ctx)) 1d0)))

(defun qoaplay-get-frame (qoa-ctx)
  (floor (qoaplay-desc-sample-position qoa-ctx) +qoa-frame-len+))

(defun qoaplay-seek-frame (qoa-ctx frame)
  (let ((max-frame (floor (qoa-desc-samples (qoaplay-desc-info qoa-ctx)) +qoa-frame-len+)))
    (setf frame (max 0 (min frame max-frame)))
    (setf (qoaplay-desc-sample-position qoa-ctx) (* frame +qoa-frame-len+)
          (qoaplay-desc-sample-data-len qoa-ctx) 0
          (qoaplay-desc-sample-data-pos qoa-ctx) 0
          (qoaplay-desc-file-data-offset qoa-ctx)
          (+ (qoaplay-desc-first-frame-pos qoa-ctx) (* frame (qoa-max-frame-size (qoaplay-desc-info qoa-ctx)))))))
