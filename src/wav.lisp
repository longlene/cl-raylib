(in-package #:cl-raylib)

;;;===================================================================================
;;; dr_wav - WAV audio loader and writer
;;; Port of the subset of raylib/src/external/dr_wav.h (v0.14.x) used by raudio
;;;
;;; Supported: RIFF, RIFX and RF64 containers, formats PCM, IEEE float, A-law, mu-law,
;;; Microsoft ADPCM and IMA ADPCM, reading as s16/f32 from memory, seeking,
;;; writing RIFF PCM/IEEE float to memory
;;; NOTE: W64 and AIFF containers and metadata parsing are not ported
;;; NOTE: Data is always read from a byte vector in memory (drwav_init_memory)
;;;===================================================================================

(defconstant +dr-wave-format-pcm+ #x1)
(defconstant +dr-wave-format-adpcm+ #x2)
(defconstant +dr-wave-format-ieee-float+ #x3)
(defconstant +dr-wave-format-alaw+ #x6)
(defconstant +dr-wave-format-mulaw+ #x7)
(defconstant +dr-wave-format-dvi-adpcm+ #x11)
(defconstant +dr-wave-format-extensible+ #xfffe)

(defconstant +drwav-max-sample-rate+ 384000)
(defconstant +drwav-max-channels+ 256)
(defconstant +drwav-max-bits-per-sample+ 64)

(defstruct (drwav (:constructor %make-drwav))
  (data nil :type (or null (simple-array (unsigned-byte 8) (*))))  ; Memory stream data
  (data-size 0 :type fixnum)
  (current-read-pos 0 :type fixnum)
  (container :riff)                             ; :riff, :rifx or :rf64
  ;; fmt chunk
  (fmt-format-tag 0) (fmt-channels 0) (fmt-sample-rate 0) (fmt-avg-bytes-per-sec 0)
  (fmt-block-align 0) (fmt-bits-per-sample 0) (fmt-extended-size 0)
  (fmt-valid-bits-per-sample 0) (fmt-channel-mask 0) (fmt-sub-format nil)
  (sample-rate 0)
  (channels 0)
  (bits-per-sample 0)
  (translated-format-tag 0)
  (total-pcm-frame-count 0)
  (data-chunk-data-size 0)
  (data-chunk-data-pos 0)
  (bytes-remaining 0)
  (read-cursor-in-pcm-frames 0)
  ;; Microsoft ADPCM state
  (msadpcm-bytes-remaining-in-block 0)
  (msadpcm-predictor (make-array 2 :initial-element 0))
  (msadpcm-delta (make-array 2 :initial-element 0))
  (msadpcm-cached-frames (make-array 4 :initial-element 0))  ; Samples are stored in this cache during decoding
  (msadpcm-cached-frame-count 0)
  (msadpcm-prev-frames (make-array '(2 2) :initial-element 0))  ; The previous 2 samples for each channel (2 channels at most)
  ;; IMA ADPCM state
  (ima-bytes-remaining-in-block 0)
  (ima-predictor (make-array 2 :initial-element 0))
  (ima-step-index (make-array 2 :initial-element 0))
  (ima-cached-frames (make-array 16 :initial-element 0))
  (ima-cached-frame-count 0))

;;;----------------------------------------------------------------------------------
;;; Byte helpers
;;;----------------------------------------------------------------------------------

(declaim (inline %u16le %u32le %u64le %u16be %u32be))
(defun %u16le (v i) (logior (aref v i) (ash (aref v (+ i 1)) 8)))
(defun %u32le (v i) (logior (%u16le v i) (ash (%u16le v (+ i 2)) 16)))
(defun %u64le (v i) (logior (%u32le v i) (ash (%u32le v (+ i 4)) 32)))
(defun %u16be (v i) (logior (ash (aref v i) 8) (aref v (+ i 1))))
(defun %u32be (v i) (logior (ash (%u16be v i) 16) (%u16be v (+ i 2))))
(defun %s16 (x) (if (>= x #x8000) (- x #x10000) x))
(defun %s32 (x) (if (>= x #x80000000) (- x #x100000000) x))
(defun %s64 (x) (if (>= x #x8000000000000000) (- x #x10000000000000000) x))

(defun %drwav-bytes-to-u16 (wav v i)
  (if (eq (drwav-container wav) :rifx) (%u16be v i) (%u16le v i)))

(defun %drwav-bytes-to-u32 (wav v i)
  (if (eq (drwav-container wav) :rifx) (%u32be v i) (%u32le v i)))

(defun %fourcc= (v i string)
  (loop for k from 0 below 4
        always (= (aref v (+ i k)) (char-code (char string k)))))

;;;----------------------------------------------------------------------------------
;;; Memory stream (drwav__on_read_memory, drwav__on_seek_memory)
;;;----------------------------------------------------------------------------------

(defun %drwav-read (wav bytes-to-read)
  "Read up to BYTES-TO-READ bytes, returns a byte vector with the bytes read"
  (let* ((pos (drwav-current-read-pos wav))
         (n (min bytes-to-read (- (drwav-data-size wav) pos)))
         (out (make-array (max n 0) :element-type '(unsigned-byte 8))))
    (when (> n 0)
      (replace out (drwav-data wav) :start2 pos :end2 (+ pos n))
      (incf (drwav-current-read-pos wav) n))
    out))

(defun %drwav-seek (wav offset origin)
  (let ((new-cursor (+ offset (ecase origin
                                (:set 0)
                                (:cur (drwav-current-read-pos wav))
                                (:end (drwav-data-size wav))))))
    (cond ((< new-cursor 0) nil)        ; Trying to seek prior to the start of the buffer
          ((> new-cursor (drwav-data-size wav)) nil)  ; Trying to seek beyond the end of the buffer
          (t (setf (drwav-current-read-pos wav) new-cursor) t))))

(defun %drwav-chunk-padding-size-riff (chunk-size)
  (mod chunk-size 2))

;;;----------------------------------------------------------------------------------
;;; Initialization
;;;----------------------------------------------------------------------------------

(defun %drwav-is-compressed-format-tag (format-tag)
  (or (= format-tag +dr-wave-format-adpcm+) (= format-tag +dr-wave-format-dvi-adpcm+)))

(defun %drwav-get-bytes-per-pcm-frame (wav)
  ;; If the bits per sample is a multiple of 8, use floor(bitsPerSample*channels/8),
  ;; otherwise fall back to the block align
  (let ((bytes-per-frame (if (= (logand (drwav-bits-per-sample wav) 7) 0)
                             (ash (* (drwav-bits-per-sample wav) (drwav-fmt-channels wav)) -3)
                             (drwav-fmt-block-align wav))))
    ;; a-law and mu-law should be 1 byte per channel
    (if (and (or (= (drwav-translated-format-tag wav) +dr-wave-format-alaw+)
                 (= (drwav-translated-format-tag wav) +dr-wave-format-mulaw+))
             (/= bytes-per-frame (drwav-fmt-channels wav)))
        0
        bytes-per-frame)))

;; drwav__read_chunk_header(), returns (values id size padding) or NIL at end
(defun %drwav-read-chunk-header (wav)
  (let ((header (%drwav-read wav 8)))
    (when (= (length header) 8)
      (let ((size (%drwav-bytes-to-u32 wav header 4)))
        (values (subseq header 0 4) size (%drwav-chunk-padding-size-riff size))))))

;; drwav_init_memory() -> drwav_init__internal()
(defun drwav-init-memory (data &optional (data-size (length data)))
  "Initialize a WAV decoder from data in memory, returns NIL on failure"
  (let ((wav (%make-drwav :data data :data-size data-size))
        (data-chunk-size 0)
        (sample-count-from-fact-chunk 0)
        (found-fmt nil)
        (found-data nil))
    ;; The first 4 bytes can be used to identify the container
    (let ((riff (%drwav-read wav 4)))
      (when (< (length riff) 4) (return-from drwav-init-memory nil))
      (cond ((%fourcc= riff 0 "RIFF") (setf (drwav-container wav) :riff))
            ((%fourcc= riff 0 "RIFX") (setf (drwav-container wav) :rifx))
            ((%fourcc= riff 0 "RF64") (setf (drwav-container wav) :rf64))
            (t (return-from drwav-init-memory nil))))   ; Unknown or unsupported container
    (let ((chunk-size-bytes (%drwav-read wav 4)))
      (when (< (length chunk-size-bytes) 4) (return-from drwav-init-memory nil))
      ;; Chunk size should always be set to 0xFFFFFFFF for RF64, the actual size is retrieved later
      (when (and (eq (drwav-container wav) :rf64) (/= (%u32le chunk-size-bytes 0) #xffffffff))
        (return-from drwav-init-memory nil)))
    (let ((wave (%drwav-read wav 4)))
      (unless (and (= (length wave) 4) (%fourcc= wave 0 "WAVE"))
        (return-from drwav-init-memory nil)))
    ;; For RF64, the "ds64" chunk must come next, before the "fmt " chunk
    (when (eq (drwav-container wav) :rf64)
      (multiple-value-bind (id size padding) (%drwav-read-chunk-header wav)
        (unless (and id (%fourcc= id 0 "ds64")) (return-from drwav-init-memory nil))
        (let ((bytes-remaining (+ size padding)))
          ;; Skip the size of the RIFF chunk
          (unless (%drwav-seek wav 8 :cur) (return-from drwav-init-memory nil))
          (decf bytes-remaining 8)
          (let ((size-bytes (%drwav-read wav 16)))
            (when (< (length size-bytes) 16) (return-from drwav-init-memory nil))
            (setf data-chunk-size (%u64le size-bytes 0)
                  sample-count-from-fact-chunk (%u64le size-bytes 8)))
          (decf bytes-remaining 16)
          (unless (%drwav-seek wav bytes-remaining :cur) (return-from drwav-init-memory nil)))))
    ;; Chunks might be in any order
    (loop
      (multiple-value-bind (id chunk-size padding) (%drwav-read-chunk-header wav)
        (unless id (return))
        (cond
          ;; "fmt "
          ((%fourcc= id 0 "fmt ")
           (when (< chunk-size 16) (return-from drwav-init-memory nil))   ; Invalid fmt chunk
           (setf found-fmt t)
           (let ((fmt-data (%drwav-read wav 16)))
             (when (< (length fmt-data) 16) (return-from drwav-init-memory nil))
             (setf (drwav-fmt-format-tag wav) (%drwav-bytes-to-u16 wav fmt-data 0)
                   (drwav-fmt-channels wav) (%drwav-bytes-to-u16 wav fmt-data 2)
                   (drwav-fmt-sample-rate wav) (%drwav-bytes-to-u32 wav fmt-data 4)
                   (drwav-fmt-avg-bytes-per-sec wav) (%drwav-bytes-to-u32 wav fmt-data 8)
                   (drwav-fmt-block-align wav) (%drwav-bytes-to-u16 wav fmt-data 12)
                   (drwav-fmt-bits-per-sample wav) (%drwav-bytes-to-u16 wav fmt-data 14)
                   (drwav-fmt-sub-format wav) (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
           (when (> chunk-size 16)
             (let ((cb-size (%drwav-read wav 2))
                   (bytes-read-so-far 18))
               (when (< (length cb-size) 2) (return-from drwav-init-memory nil))
               (setf (drwav-fmt-extended-size wav) (%drwav-bytes-to-u16 wav cb-size 0))
               (when (> (drwav-fmt-extended-size wav) 0)
                 (if (= (drwav-fmt-format-tag wav) +dr-wave-format-extensible+)
                     (progn
                       (unless (= (drwav-fmt-extended-size wav) 22) (return-from drwav-init-memory nil))
                       (let ((fmtext (%drwav-read wav 22)))
                         (when (< (length fmtext) 22) (return-from drwav-init-memory nil))
                         (setf (drwav-fmt-valid-bits-per-sample wav) (%drwav-bytes-to-u16 wav fmtext 0)
                               (drwav-fmt-channel-mask wav) (%drwav-bytes-to-u32 wav fmtext 2)
                               (drwav-fmt-sub-format wav) (subseq fmtext 6 22))))
                     (unless (%drwav-seek wav (drwav-fmt-extended-size wav) :cur)
                       (return-from drwav-init-memory nil)))
                 (incf bytes-read-so-far (drwav-fmt-extended-size wav)))
               ;; Seek past any leftover bytes
               (unless (%drwav-seek wav (- chunk-size bytes-read-so-far) :cur)
                 (return-from drwav-init-memory nil))))
           (when (> padding 0)
             (unless (%drwav-seek wav padding :cur) (return))))
          ;; "data"
          ((%fourcc= id 0 "data")
           (setf found-data t
                 (drwav-data-chunk-data-pos wav) (drwav-current-read-pos wav))
           ;; The data chunk size for RF64 was set to it's true value earlier
           (unless (eq (drwav-container wav) :rf64)
             (setf data-chunk-size chunk-size))
           ;; Not reading metadata, no need to keep reading beyond the data chunk
           (return))
          ;; "fact", the sample count is only used for Microsoft ADPCM
          ;; NOTE: translatedFormatTag is not set yet at this point, so it is always ignored for RIFF
          ((%fourcc= id 0 "fact")
           (when (member (drwav-container wav) '(:riff :rifx))
             (let ((sample-count (%drwav-read wav 4)))
               (when (< (length sample-count) 4) (return-from drwav-init-memory nil))
               (decf chunk-size 4)
               (setf sample-count-from-fact-chunk
                     (if (= (drwav-translated-format-tag wav) +dr-wave-format-adpcm+)
                         (%drwav-bytes-to-u32 wav sample-count 0)
                         0))))
           (unless (%drwav-seek wav (+ chunk-size padding) :cur) (return)))
          ;; Skip past the content of any other chunk
          (t (unless (%drwav-seek wav (+ chunk-size padding) :cur) (return))))))
    ;; There's some mandatory chunks that must exist
    (unless (and found-fmt found-data)
      (return-from drwav-init-memory nil))
    ;; Basic validation
    (when (or (= (drwav-fmt-sample-rate wav) 0) (> (drwav-fmt-sample-rate wav) +drwav-max-sample-rate+)
              (= (drwav-fmt-channels wav) 0) (> (drwav-fmt-channels wav) +drwav-max-channels+)
              (= (drwav-fmt-bits-per-sample wav) 0) (> (drwav-fmt-bits-per-sample wav) +drwav-max-bits-per-sample+)
              (= (drwav-fmt-block-align wav) 0))
      (return-from drwav-init-memory nil))    ; Probably an invalid WAV file
    ;; Translate the internal format
    (let ((translated-format-tag (drwav-fmt-format-tag wav)))
      (when (= translated-format-tag +dr-wave-format-extensible+)
        (setf translated-format-tag (%drwav-bytes-to-u16 wav (drwav-fmt-sub-format wav) 0)))
      ;; We may have moved passed the data chunk, move back
      (unless (%drwav-seek wav (drwav-data-chunk-data-pos wav) :set)
        (return-from drwav-init-memory nil))
      ;; It's possible for the size reported in the data chunk to be greater than that of the file
      (when (> (+ data-chunk-size (drwav-data-chunk-data-pos wav)) (drwav-data-size wav))
        (setf data-chunk-size (- (drwav-data-size wav) (drwav-data-chunk-data-pos wav))))
      ;; RIFF files with the "data" chunk size set to 0xFFFFFFFF, assume the rest of the file is audio data
      (when (and (= data-chunk-size #xffffffff) (member (drwav-container wav) '(:riff :rifx)))
        (setf data-chunk-size (- (drwav-data-size wav) (drwav-data-chunk-data-pos wav))))
      (setf (drwav-sample-rate wav) (drwav-fmt-sample-rate wav)
            (drwav-channels wav) (drwav-fmt-channels wav)
            (drwav-bits-per-sample wav) (drwav-fmt-bits-per-sample wav)
            (drwav-translated-format-tag wav) translated-format-tag)
      ;; Round the data chunk size down to the nearest multiple of the frame size
      (unless (%drwav-is-compressed-format-tag translated-format-tag)
        (let ((bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav)))
          (when (> bytes-per-frame 0)
            (decf data-chunk-size (mod data-chunk-size bytes-per-frame)))))
      (setf (drwav-bytes-remaining wav) data-chunk-size
            (drwav-data-chunk-data-size wav) data-chunk-size)
      (if (/= sample-count-from-fact-chunk 0)
          (setf (drwav-total-pcm-frame-count wav) sample-count-from-fact-chunk)
          (let ((bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav))
                (block-align (drwav-fmt-block-align wav))
                (channels (drwav-fmt-channels wav)))
            (when (= bytes-per-frame 0) (return-from drwav-init-memory nil))   ; Invalid file
            (setf (drwav-total-pcm-frame-count wav) (floor data-chunk-size bytes-per-frame))
            (when (or (= translated-format-tag +dr-wave-format-adpcm+)
                      (= translated-format-tag +dr-wave-format-dvi-adpcm+))
              (let* ((block-count (ceiling data-chunk-size block-align))  ; Make sure any trailing partial block is accounted for
                     (total-block-header-size-in-bytes
                       (* block-count (if (= translated-format-tag +dr-wave-format-adpcm+) 6 4) channels)))
                (when (>= total-block-header-size-in-bytes data-chunk-size)
                  (return-from drwav-init-memory nil))   ; Invalid file
                ;; Two samples are decoded per byte
                (setf (drwav-total-pcm-frame-count wav)
                      (floor (* (- data-chunk-size total-block-header-size-in-bytes) 2) channels))
                ;; IMA ADPCM header includes a decoded sample for each channel
                (when (= translated-format-tag +dr-wave-format-dvi-adpcm+)
                  (incf (drwav-total-pcm-frame-count wav) block-count))))))
      ;; Some formats only support a certain number of channels
      (when (and (%drwav-is-compressed-format-tag translated-format-tag) (> (drwav-channels wav) 2))
        (return-from drwav-init-memory nil))
      (when (= (%drwav-get-bytes-per-pcm-frame wav) 0)
        (return-from drwav-init-memory nil))
      wav)))

(defun drwav-uninit (wav)
  (declare (ignore wav))
  t)

;;;----------------------------------------------------------------------------------
;;; Reading
;;;----------------------------------------------------------------------------------

;; drwav_read_raw(), returns the bytes read
(defun %drwav-read-raw (wav bytes-to-read)
  (let ((bytes-to-read (min bytes-to-read (drwav-bytes-remaining wav)))
        (bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav)))
    (if (or (<= bytes-to-read 0) (= bytes-per-frame 0))
        (make-array 0 :element-type '(unsigned-byte 8))
        (let ((bytes (%drwav-read wav bytes-to-read)))
          (incf (drwav-read-cursor-in-pcm-frames wav) (floor (length bytes) bytes-per-frame))
          (decf (drwav-bytes-remaining wav) (length bytes))
          bytes))))

;; drwav_read_pcm_frames(), returns (values frames-read bytes) with samples in little-endian byte order
(defun drwav-read-pcm-frames (wav frames-to-read)
  (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
    ;; Cannot use this function for compressed formats
    (when (or (<= frames-to-read 0) (%drwav-is-compressed-format-tag (drwav-translated-format-tag wav)))
      (return-from drwav-read-pcm-frames (values 0 empty)))
    (let ((frames-to-read (min frames-to-read (- (drwav-total-pcm-frame-count wav) (drwav-read-cursor-in-pcm-frames wav))))
          (bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav)))
      (when (or (<= frames-to-read 0) (= bytes-per-frame 0))
        (return-from drwav-read-pcm-frames (values 0 empty)))
      (let* ((bytes (%drwav-read-raw wav (* frames-to-read bytes-per-frame)))
             (frames-read (floor (length bytes) bytes-per-frame)))
        ;; Big-endian container, swap the bytes of each sample (drwav_read_pcm_frames_be)
        (when (eq (drwav-container wav) :rifx)
          (let ((bytes-per-sample (floor bytes-per-frame (drwav-channels wav))))
            (loop for i from 0 below (* frames-read (drwav-channels wav))
                  for start = (* i bytes-per-sample)
                  do (setf (subseq bytes start (+ start bytes-per-sample))
                           (reverse (subseq bytes start (+ start bytes-per-sample)))))))
        (values frames-read bytes)))))

(defun drwav-seek-to-first-pcm-frame (wav)
  (unless (%drwav-seek wav (drwav-data-chunk-data-pos wav) :set)
    (return-from drwav-seek-to-first-pcm-frame nil))
  ;; Cached data needs to be cleared for compressed formats
  (setf (drwav-msadpcm-bytes-remaining-in-block wav) 0
        (drwav-msadpcm-cached-frame-count wav) 0
        (drwav-ima-bytes-remaining-in-block wav) 0
        (drwav-ima-cached-frame-count wav) 0)
  (fill (drwav-msadpcm-predictor wav) 0)
  (fill (drwav-msadpcm-delta wav) 0)
  (fill (drwav-msadpcm-cached-frames wav) 0)
  (dotimes (i 4) (setf (row-major-aref (drwav-msadpcm-prev-frames wav) i) 0))
  (fill (drwav-ima-predictor wav) 0)
  (fill (drwav-ima-step-index wav) 0)
  (fill (drwav-ima-cached-frames wav) 0)
  (setf (drwav-read-cursor-in-pcm-frames wav) 0
        (drwav-bytes-remaining wav) (drwav-data-chunk-data-size wav))
  t)

(defun drwav-seek-to-pcm-frame (wav target-frame-index)
  (when (= (drwav-total-pcm-frame-count wav) 0)
    (return-from drwav-seek-to-pcm-frame t))
  (setf target-frame-index (min target-frame-index (drwav-total-pcm-frame-count wav)))
  (if (%drwav-is-compressed-format-tag (drwav-translated-format-tag wav))
      ;; Compressed formats use a slow generic seek
      (progn
        (when (< target-frame-index (drwav-read-cursor-in-pcm-frames wav))
          (unless (drwav-seek-to-first-pcm-frame wav) (return-from drwav-seek-to-pcm-frame nil)))
        (when (> target-frame-index (drwav-read-cursor-in-pcm-frames wav))
          (let* ((offset-in-frames (- target-frame-index (drwav-read-cursor-in-pcm-frames wav)))
                 (devnull (make-array 2048 :element-type '(signed-byte 16))))
            (loop while (> offset-in-frames 0)
                  do (let* ((frames-to-read (min offset-in-frames (floor 2048 (drwav-channels wav))))
                            (frames-read (if (= (drwav-translated-format-tag wav) +dr-wave-format-adpcm+)
                                             (%drwav-read-pcm-frames-s16-msadpcm wav frames-to-read devnull 0)
                                             (%drwav-read-pcm-frames-s16-ima wav frames-to-read devnull 0))))
                       (unless (= frames-read frames-to-read) (return-from drwav-seek-to-pcm-frame nil))
                       (decf offset-in-frames frames-read)))))
        t)
      (let* ((bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav))
             (total-size-in-bytes (* (drwav-total-pcm-frame-count wav) bytes-per-frame))
             (current-byte-pos (- total-size-in-bytes (drwav-bytes-remaining wav)))
             (target-byte-pos (* target-frame-index bytes-per-frame))
             (offset 0))
        (when (= bytes-per-frame 0) (return-from drwav-seek-to-pcm-frame nil))
        (if (< current-byte-pos target-byte-pos)
            (setf offset (- target-byte-pos current-byte-pos))   ; Offset forwards
            (progn                                               ; Offset backwards
              (unless (drwav-seek-to-first-pcm-frame wav) (return-from drwav-seek-to-pcm-frame nil))
              (setf offset target-byte-pos)))
        (when (> offset 0)
          (unless (%drwav-seek wav offset :cur) (return-from drwav-seek-to-pcm-frame nil))
          (incf (drwav-read-cursor-in-pcm-frames wav) (floor offset bytes-per-frame))
          (decf (drwav-bytes-remaining wav) offset))
        t)))

;;; ADPCM decoders

(alexandria:define-constant +drwav-msadpcm-adaptation-table+
  #(230 230 230 230 307 409 512 614 768 614 512 409 307 230 230 230) :test #'equalp)
(alexandria:define-constant +drwav-msadpcm-coeff1-table+ #(256 512 0 192 240 460 392) :test #'equalp)
(alexandria:define-constant +drwav-msadpcm-coeff2-table+ #(0 -256 0 64 0 -208 -232) :test #'equalp)

;; drwav_read_pcm_frames_s16__msadpcm()
(defun %drwav-read-pcm-frames-s16-msadpcm (wav frames-to-read out out-pos)
  (let ((total-frames-read 0)
        (channels (drwav-channels wav))
        (cached (drwav-msadpcm-cached-frames wav))
        (prev (drwav-msadpcm-prev-frames wav))
        (predictor (drwav-msadpcm-predictor wav))
        (delta (drwav-msadpcm-delta wav)))
    (flet ((bad-predictor-p (c) (>= (aref predictor c) 7))
           (decode-nibble (c nibble raw)
             (let* ((p (aref predictor c))
                    (new-sample (ash (+ (* (aref prev c 1) (svref +drwav-msadpcm-coeff1-table+ p))
                                        (* (aref prev c 0) (svref +drwav-msadpcm-coeff2-table+ p)))
                                     -8)))
               (setf new-sample (max -32768 (min 32767 (+ new-sample (* nibble (aref delta c))))))
               (setf (aref delta c) (max 16 (min #x7fffffff (ash (* (svref +drwav-msadpcm-adaptation-table+ raw)
                                                                     (aref delta c))
                                                                  -8))))
               (setf (aref prev c 0) (aref prev c 1)
                     (aref prev c 1) new-sample)
               new-sample)))
      (loop while (< (drwav-read-cursor-in-pcm-frames wav) (drwav-total-pcm-frame-count wav))
            do ;; If there are no cached frames we need to load a new block
               (when (and (= (drwav-msadpcm-cached-frame-count wav) 0)
                          (= (drwav-msadpcm-bytes-remaining-in-block wav) 0))
                 (if (= channels 1)
                     (let ((header (%drwav-read wav 7)))
                       (when (< (length header) 7) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                       (setf (drwav-msadpcm-bytes-remaining-in-block wav) (- (drwav-fmt-block-align wav) 7)
                             (aref predictor 0) (aref header 0)
                             (aref delta 0) (%s16 (%u16le header 1))
                             (aref prev 0 1) (%s16 (%u16le header 3))
                             (aref prev 0 0) (%s16 (%u16le header 5))
                             (aref cached 2) (aref prev 0 0)
                             (aref cached 3) (aref prev 0 1)
                             (drwav-msadpcm-cached-frame-count wav) 2)
                       (when (bad-predictor-p 0) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read)))
                     (let ((header (%drwav-read wav 14)))
                       (when (< (length header) 14) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                       (setf (drwav-msadpcm-bytes-remaining-in-block wav) (- (drwav-fmt-block-align wav) 14)
                             (aref predictor 0) (aref header 0)
                             (aref predictor 1) (aref header 1)
                             (aref delta 0) (%s16 (%u16le header 2))
                             (aref delta 1) (%s16 (%u16le header 4))
                             (aref prev 0 1) (%s16 (%u16le header 6))
                             (aref prev 1 1) (%s16 (%u16le header 8))
                             (aref prev 0 0) (%s16 (%u16le header 10))
                             (aref prev 1 0) (%s16 (%u16le header 12))
                             (aref cached 0) (aref prev 0 0)
                             (aref cached 1) (aref prev 1 0)
                             (aref cached 2) (aref prev 0 1)
                             (aref cached 3) (aref prev 1 1)
                             (drwav-msadpcm-cached-frame-count wav) 2)
                       (when (or (bad-predictor-p 0) (bad-predictor-p 1))
                         (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read)))))
               ;; Output anything that's cached
               (loop while (and (> frames-to-read 0) (> (drwav-msadpcm-cached-frame-count wav) 0)
                                (< (drwav-read-cursor-in-pcm-frames wav) (drwav-total-pcm-frame-count wav)))
                     do (when out
                          (dotimes (s channels)
                            (setf (aref out out-pos) (%i16 (aref cached (+ (- 4 (* (drwav-msadpcm-cached-frame-count wav) channels)) s))))
                            (incf out-pos)))
                        (decf frames-to-read)
                        (incf total-frames-read)
                        (incf (drwav-read-cursor-in-pcm-frames wav))
                        (decf (drwav-msadpcm-cached-frame-count wav)))
               (when (= frames-to-read 0) (return))
               ;; If there's nothing left in the cache, load more
               (when (= (drwav-msadpcm-cached-frame-count wav) 0)
                 (unless (= (drwav-msadpcm-bytes-remaining-in-block wav) 0)
                   (let ((nibbles-data (%drwav-read wav 1)))
                     (when (< (length nibbles-data) 1) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                     (decf (drwav-msadpcm-bytes-remaining-in-block wav))
                     (let* ((nibbles (aref nibbles-data 0))
                            (raw0 (ash (logand nibbles #xf0) -4))
                            (raw1 (logand nibbles #x0f))
                            (nibble0 (if (logtest nibbles #x80) (- raw0 16) raw0))
                            (nibble1 (if (logtest nibbles #x08) (- raw1 16) raw1)))
                       (if (= channels 1)
                           (progn
                             (when (bad-predictor-p 0) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                             (setf (aref cached 2) (decode-nibble 0 nibble0 raw0)
                                   (aref cached 3) (decode-nibble 0 nibble1 raw1)
                                   (drwav-msadpcm-cached-frame-count wav) 2))
                           (progn
                             (when (bad-predictor-p 0) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                             (setf (aref cached 2) (decode-nibble 0 nibble0 raw0))
                             (when (bad-predictor-p 1) (return-from %drwav-read-pcm-frames-s16-msadpcm total-frames-read))
                             (setf (aref cached 3) (decode-nibble 1 nibble1 raw1)
                                   (drwav-msadpcm-cached-frame-count wav) 1)))))))))
    total-frames-read))

(alexandria:define-constant +drwav-ima-index-table+
  #(-1 -1 -1 -1 2 4 6 8 -1 -1 -1 -1 2 4 6 8) :test #'equalp)
(alexandria:define-constant +drwav-ima-step-table+
  #(7 8 9 10 11 12 13 14 16 17 19 21 23 25 28 31 34 37 41 45
    50 55 60 66 73 80 88 97 107 118 130 143 157 173 190 209 230 253 279 307
    337 371 408 449 494 544 598 658 724 796 876 963 1060 1166 1282 1411 1552 1707 1878 2066
    2272 2499 2749 3024 3327 3660 4026 4428 4871 5358 5894 6484 7132 7845 8630 9493 10442 11487 12635 13899
    15289 16818 18500 20350 22385 24623 27086 29794 32767) :test #'equalp)

;; drwav_read_pcm_frames_s16__ima()
(defun %drwav-read-pcm-frames-s16-ima (wav frames-to-read out out-pos)
  (let ((total-frames-read 0)
        (channels (drwav-channels wav))
        (cached (drwav-ima-cached-frames wav))
        (predictor (drwav-ima-predictor wav))
        (step-index (drwav-ima-step-index wav)))
    (flet ((decode-nibble (c nibble)
             (let* ((step (svref +drwav-ima-step-table+ (aref step-index c)))
                    (diff (ash step -3)))
               (when (logtest nibble 1) (incf diff (ash step -2)))
               (when (logtest nibble 2) (incf diff (ash step -1)))
               (when (logtest nibble 4) (incf diff step))
               (when (logtest nibble 8) (setf diff (- diff)))
               (setf (aref predictor c) (max -32768 (min 32767 (+ (aref predictor c) diff)))
                     (aref step-index c) (max 0 (min 88 (+ (aref step-index c) (svref +drwav-ima-index-table+ nibble)))))
               (aref predictor c))))
      (loop while (< (drwav-read-cursor-in-pcm-frames wav) (drwav-total-pcm-frame-count wav))
            do ;; If there are no cached samples we need to load a new block
               (when (and (= (drwav-ima-cached-frame-count wav) 0) (= (drwav-ima-bytes-remaining-in-block wav) 0))
                 (let* ((header-size (if (= channels 1) 4 8))
                        (header (%drwav-read wav header-size)))
                   (when (< (length header) header-size) (return-from %drwav-read-pcm-frames-s16-ima total-frames-read))
                   (setf (drwav-ima-bytes-remaining-in-block wav) (- (drwav-fmt-block-align wav) header-size))
                   (when (or (>= (aref header 2) 89) (and (= channels 2) (>= (aref header 6) 89)))
                     (%drwav-seek wav (drwav-ima-bytes-remaining-in-block wav) :cur)
                     (setf (drwav-ima-bytes-remaining-in-block wav) 0)
                     (return-from %drwav-read-pcm-frames-s16-ima total-frames-read))   ; Invalid data
                   (setf (aref predictor 0) (%s16 (%u16le header 0))
                         (aref step-index 0) (aref header 2))
                   (if (= channels 1)
                       (setf (aref cached 15) (aref predictor 0))
                       (setf (aref predictor 1) (%s16 (%u16le header 4))
                             (aref step-index 1) (aref header 6)
                             (aref cached 14) (aref predictor 0)
                             (aref cached 15) (aref predictor 1)))
                   (setf (drwav-ima-cached-frame-count wav) 1)))
               ;; Output anything that's cached
               (loop while (and (> frames-to-read 0) (> (drwav-ima-cached-frame-count wav) 0)
                                (< (drwav-read-cursor-in-pcm-frames wav) (drwav-total-pcm-frame-count wav)))
                     do (when out
                          (dotimes (s channels)
                            (setf (aref out out-pos) (%i16 (aref cached (+ (- 16 (* (drwav-ima-cached-frame-count wav) channels)) s))))
                            (incf out-pos)))
                        (decf frames-to-read)
                        (incf total-frames-read)
                        (incf (drwav-read-cursor-in-pcm-frames wav))
                        (decf (drwav-ima-cached-frame-count wav)))
               (when (= frames-to-read 0) (return))
               ;; Every 4 bytes (8 samples) is for one channel
               (when (and (= (drwav-ima-cached-frame-count wav) 0) (/= (drwav-ima-bytes-remaining-in-block wav) 0))
                 (setf (drwav-ima-cached-frame-count wav) 8)
                 (dotimes (c channels)
                   (let ((nibbles (%drwav-read wav 4)))
                     (when (< (length nibbles) 4)
                       (setf (drwav-ima-cached-frame-count wav) 0)
                       (return-from %drwav-read-pcm-frames-s16-ima total-frames-read))
                     (decf (drwav-ima-bytes-remaining-in-block wav) 4)
                     (dotimes (i 4)
                       (let ((base (- 16 (* (drwav-ima-cached-frame-count wav) channels))))
                         (setf (aref cached (+ base (* (+ (* i 2) 0) channels) c))
                               (decode-nibble c (logand (aref nibbles i) #x0f)))
                         (setf (aref cached (+ base (* (+ (* i 2) 1) channels) c))
                               (decode-nibble c (ash (logand (aref nibbles i) #xf0) -4))))))))))
    total-frames-read))

;;; Sample conversion

(alexandria:define-constant +drwav-alaw-table+
  #(#xEA80 #xEB80 #xE880 #xE980 #xEE80 #xEF80 #xEC80 #xED80 #xE280 #xE380 #xE080 #xE180 #xE680 #xE780 #xE480 #xE580
    #xF540 #xF5C0 #xF440 #xF4C0 #xF740 #xF7C0 #xF640 #xF6C0 #xF140 #xF1C0 #xF040 #xF0C0 #xF340 #xF3C0 #xF240 #xF2C0
    #xAA00 #xAE00 #xA200 #xA600 #xBA00 #xBE00 #xB200 #xB600 #x8A00 #x8E00 #x8200 #x8600 #x9A00 #x9E00 #x9200 #x9600
    #xD500 #xD700 #xD100 #xD300 #xDD00 #xDF00 #xD900 #xDB00 #xC500 #xC700 #xC100 #xC300 #xCD00 #xCF00 #xC900 #xCB00
    #xFEA8 #xFEB8 #xFE88 #xFE98 #xFEE8 #xFEF8 #xFEC8 #xFED8 #xFE28 #xFE38 #xFE08 #xFE18 #xFE68 #xFE78 #xFE48 #xFE58
    #xFFA8 #xFFB8 #xFF88 #xFF98 #xFFE8 #xFFF8 #xFFC8 #xFFD8 #xFF28 #xFF38 #xFF08 #xFF18 #xFF68 #xFF78 #xFF48 #xFF58
    #xFAA0 #xFAE0 #xFA20 #xFA60 #xFBA0 #xFBE0 #xFB20 #xFB60 #xF8A0 #xF8E0 #xF820 #xF860 #xF9A0 #xF9E0 #xF920 #xF960
    #xFD50 #xFD70 #xFD10 #xFD30 #xFDD0 #xFDF0 #xFD90 #xFDB0 #xFC50 #xFC70 #xFC10 #xFC30 #xFCD0 #xFCF0 #xFC90 #xFCB0
    #x1580 #x1480 #x1780 #x1680 #x1180 #x1080 #x1380 #x1280 #x1D80 #x1C80 #x1F80 #x1E80 #x1980 #x1880 #x1B80 #x1A80
    #x0AC0 #x0A40 #x0BC0 #x0B40 #x08C0 #x0840 #x09C0 #x0940 #x0EC0 #x0E40 #x0FC0 #x0F40 #x0CC0 #x0C40 #x0DC0 #x0D40
    #x5600 #x5200 #x5E00 #x5A00 #x4600 #x4200 #x4E00 #x4A00 #x7600 #x7200 #x7E00 #x7A00 #x6600 #x6200 #x6E00 #x6A00
    #x2B00 #x2900 #x2F00 #x2D00 #x2300 #x2100 #x2700 #x2500 #x3B00 #x3900 #x3F00 #x3D00 #x3300 #x3100 #x3700 #x3500
    #x0158 #x0148 #x0178 #x0168 #x0118 #x0108 #x0138 #x0128 #x01D8 #x01C8 #x01F8 #x01E8 #x0198 #x0188 #x01B8 #x01A8
    #x0058 #x0048 #x0078 #x0068 #x0018 #x0008 #x0038 #x0028 #x00D8 #x00C8 #x00F8 #x00E8 #x0098 #x0088 #x00B8 #x00A8
    #x0560 #x0520 #x05E0 #x05A0 #x0460 #x0420 #x04E0 #x04A0 #x0760 #x0720 #x07E0 #x07A0 #x0660 #x0620 #x06E0 #x06A0
    #x02B0 #x0290 #x02F0 #x02D0 #x0230 #x0210 #x0270 #x0250 #x03B0 #x0390 #x03F0 #x03D0 #x0330 #x0310 #x0370 #x0350)
  :test #'equalp)

(alexandria:define-constant +drwav-mulaw-table+
  #(#x8284 #x8684 #x8A84 #x8E84 #x9284 #x9684 #x9A84 #x9E84 #xA284 #xA684 #xAA84 #xAE84 #xB284 #xB684 #xBA84 #xBE84
    #xC184 #xC384 #xC584 #xC784 #xC984 #xCB84 #xCD84 #xCF84 #xD184 #xD384 #xD584 #xD784 #xD984 #xDB84 #xDD84 #xDF84
    #xE104 #xE204 #xE304 #xE404 #xE504 #xE604 #xE704 #xE804 #xE904 #xEA04 #xEB04 #xEC04 #xED04 #xEE04 #xEF04 #xF004
    #xF0C4 #xF144 #xF1C4 #xF244 #xF2C4 #xF344 #xF3C4 #xF444 #xF4C4 #xF544 #xF5C4 #xF644 #xF6C4 #xF744 #xF7C4 #xF844
    #xF8A4 #xF8E4 #xF924 #xF964 #xF9A4 #xF9E4 #xFA24 #xFA64 #xFAA4 #xFAE4 #xFB24 #xFB64 #xFBA4 #xFBE4 #xFC24 #xFC64
    #xFC94 #xFCB4 #xFCD4 #xFCF4 #xFD14 #xFD34 #xFD54 #xFD74 #xFD94 #xFDB4 #xFDD4 #xFDF4 #xFE14 #xFE34 #xFE54 #xFE74
    #xFE8C #xFE9C #xFEAC #xFEBC #xFECC #xFEDC #xFEEC #xFEFC #xFF0C #xFF1C #xFF2C #xFF3C #xFF4C #xFF5C #xFF6C #xFF7C
    #xFF88 #xFF90 #xFF98 #xFFA0 #xFFA8 #xFFB0 #xFFB8 #xFFC0 #xFFC8 #xFFD0 #xFFD8 #xFFE0 #xFFE8 #xFFF0 #xFFF8 #x0000
    #x7D7C #x797C #x757C #x717C #x6D7C #x697C #x657C #x617C #x5D7C #x597C #x557C #x517C #x4D7C #x497C #x457C #x417C
    #x3E7C #x3C7C #x3A7C #x387C #x367C #x347C #x327C #x307C #x2E7C #x2C7C #x2A7C #x287C #x267C #x247C #x227C #x207C
    #x1EFC #x1DFC #x1CFC #x1BFC #x1AFC #x19FC #x18FC #x17FC #x16FC #x15FC #x14FC #x13FC #x12FC #x11FC #x10FC #x0FFC
    #x0F3C #x0EBC #x0E3C #x0DBC #x0D3C #x0CBC #x0C3C #x0BBC #x0B3C #x0ABC #x0A3C #x09BC #x093C #x08BC #x083C #x07BC
    #x075C #x071C #x06DC #x069C #x065C #x061C #x05DC #x059C #x055C #x051C #x04DC #x049C #x045C #x041C #x03DC #x039C
    #x036C #x034C #x032C #x030C #x02EC #x02CC #x02AC #x028C #x026C #x024C #x022C #x020C #x01EC #x01CC #x01AC #x018C
    #x0174 #x0164 #x0154 #x0144 #x0134 #x0124 #x0114 #x0104 #x00F4 #x00E4 #x00D4 #x00C4 #x00B4 #x00A4 #x0094 #x0084
    #x0078 #x0070 #x0068 #x0060 #x0058 #x0050 #x0048 #x0040 #x0038 #x0030 #x0028 #x0020 #x0018 #x0010 #x0008 #x0000)
  :test #'equalp)

;; Read a little-endian signed integer of SIZE bytes left justified in 64 bits (generic, slow converter)
(defun %drwav-sample-s64 (bytes start size)
  (let ((sample 0)
        (shift (* (- 8 size) 8)))
    (dotimes (j size)
      (setf sample (logior sample (ash (aref bytes (+ start j)) shift)))
      (incf shift 8))
    (%s64 sample)))

;; drwav__pcm_to_s16() for one sample
(defun %drwav-pcm-to-s16 (bytes start bytes-per-sample)
  (case bytes-per-sample
    (1 (%i16 (- (ash (aref bytes start) 8) 32768)))                    ; drwav_u8_to_s16
    (2 (%s16 (%u16le bytes start)))
    (3 (ash (%s32 (logior (ash (aref bytes start) 8) (ash (aref bytes (+ start 1)) 16)    ; drwav_s24_to_s16
                          (ash (aref bytes (+ start 2)) 24)))
            -16))
    (4 (ash (%s32 (%u32le bytes start)) -16))                           ; drwav_s32_to_s16
    (t (if (> bytes-per-sample 8)
           0
           (%i16 (ash (%drwav-sample-s64 bytes start bytes-per-sample) -48))))))

;; drwav__pcm_to_f32() for one sample
(defun %drwav-pcm-to-f32 (bytes start bytes-per-sample)
  (case bytes-per-sample
    (1 (- (* (float (aref bytes start) 1f0) 0.00784313725490196078f0) 1f0))   ; drwav_u8_to_f32
    (2 (* (float (%s16 (%u16le bytes start)) 1f0) 0.000030517578125f0))         ; drwav_s16_to_f32
    (3 (coerce (* (float (ash (%s32 (logior (ash (aref bytes start) 8) (ash (aref bytes (+ start 1)) 16)
                                            (ash (aref bytes (+ start 2)) 24)))
                              -8)
                         1d0)
                  0.00000011920928955078125d0)
               'single-float))
    (4 (coerce (/ (float (%s32 (%u32le bytes start)) 1d0) 2147483648d0) 'single-float))
    (t (if (> bytes-per-sample 8)
           0f0
           (coerce (/ (float (%drwav-sample-s64 bytes start bytes-per-sample) 1d0)
                      (float 9223372036854775807 1d0))
                   'single-float)))))

(defun %drwav-f32-bits (bytes start)
  (ieee-floats:decode-float32 (%u32le bytes start)))

(defun %drwav-f64-bits (bytes start)
  (ieee-floats:decode-float64 (%u64le bytes start)))

;; drwav__ieee_to_s16() for one sample
(defun %drwav-ieee-to-s16 (bytes start bytes-per-sample)
  (case bytes-per-sample
    (4 (let ((c (%ma-clip-f32 (%drwav-f32-bits bytes start))))        ; drwav_f32_to_s16
         (%i16 (- (truncate (* (+ c 1f0) 32767.5f0)) 32768))))
    (8 (let* ((x (%drwav-f64-bits bytes start))                        ; drwav_f64_to_s16
              (c (cond ((< x -1) -1d0) ((> x 1) 1d0) (t x))))
         (%i16 (- (truncate (* (+ c 1d0) 32767.5d0)) 32768))))
    (t 0)))

;; drwav__ieee_to_f32() for one sample
(defun %drwav-ieee-to-f32 (bytes start bytes-per-sample)
  (case bytes-per-sample
    (4 (%drwav-f32-bits bytes start))
    (8 (coerce (%drwav-f64-bits bytes start) 'single-float))
    (t 0f0)))

;; Read uncompressed frames converting each sample with CONVERTER
(defun %drwav-read-pcm-frames-converted (wav frames-to-read out out-pos converter)
  (let ((bytes-per-frame (%drwav-get-bytes-per-pcm-frame wav)))
    (when (= bytes-per-frame 0) (return-from %drwav-read-pcm-frames-converted 0))
    (let ((bytes-per-sample (floor bytes-per-frame (drwav-channels wav))))
      ;; Only byte-aligned formats are supported
      (when (or (= bytes-per-sample 0) (/= (mod bytes-per-frame (drwav-channels wav)) 0))
        (return-from %drwav-read-pcm-frames-converted 0))
      (multiple-value-bind (frames-read bytes) (drwav-read-pcm-frames wav frames-to-read)
        (dotimes (i (* frames-read (drwav-channels wav)))
          (setf (aref out (+ out-pos i)) (funcall converter bytes (* i bytes-per-sample) bytes-per-sample)))
        frames-read))))

(defun drwav-read-pcm-frames-s16 (wav frames-to-read out &optional (out-pos 0))
  "Read FRAMES-TO-READ frames as s16 into OUT (a (signed-byte 16) array) at sample OUT-POS"
  (when (or (null wav) (<= frames-to-read 0))
    (return-from drwav-read-pcm-frames-s16 0))
  (let ((tag (drwav-translated-format-tag wav)))
    (cond ((= tag +dr-wave-format-pcm+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos #'%drwav-pcm-to-s16))
          ((= tag +dr-wave-format-ieee-float+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos #'%drwav-ieee-to-s16))
          ((= tag +dr-wave-format-alaw+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos
                                             (lambda (bytes start size) (declare (ignore size))
                                               (%s16 (svref +drwav-alaw-table+ (aref bytes start))))))
          ((= tag +dr-wave-format-mulaw+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos
                                             (lambda (bytes start size) (declare (ignore size))
                                               (%s16 (svref +drwav-mulaw-table+ (aref bytes start))))))
          ((= tag +dr-wave-format-adpcm+) (%drwav-read-pcm-frames-s16-msadpcm wav frames-to-read out out-pos))
          ((= tag +dr-wave-format-dvi-adpcm+) (%drwav-read-pcm-frames-s16-ima wav frames-to-read out out-pos))
          (t 0))))

(defun drwav-read-pcm-frames-f32 (wav frames-to-read out &optional (out-pos 0))
  "Read FRAMES-TO-READ frames as f32 into OUT (a single-float array) at sample OUT-POS"
  (when (or (null wav) (<= frames-to-read 0))
    (return-from drwav-read-pcm-frames-f32 0))
  (let ((tag (drwav-translated-format-tag wav)))
    (cond ((= tag +dr-wave-format-pcm+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos #'%drwav-pcm-to-f32))
          ;; drwav_read_pcm_frames_f32__msadpcm_ima(): read as s16 and convert
          ((%drwav-is-compressed-format-tag tag)
           (let* ((samples16 (make-array (* frames-to-read (drwav-channels wav)) :element-type '(signed-byte 16)))
                  (frames-read (drwav-read-pcm-frames-s16 wav frames-to-read samples16 0)))
             (dotimes (i (* frames-read (drwav-channels wav)))
               (setf (aref out (+ out-pos i)) (* (float (aref samples16 i) 1f0) 0.000030517578125f0)))
             frames-read))
          ((= tag +dr-wave-format-ieee-float+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos #'%drwav-ieee-to-f32))
          ((= tag +dr-wave-format-alaw+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos
                                             (lambda (bytes start size) (declare (ignore size))
                                               (/ (float (%s16 (svref +drwav-alaw-table+ (aref bytes start))) 1f0) 32768f0))))
          ((= tag +dr-wave-format-mulaw+)
           (%drwav-read-pcm-frames-converted wav frames-to-read out out-pos
                                             (lambda (bytes start size) (declare (ignore size))
                                               (/ (float (%s16 (svref +drwav-mulaw-table+ (aref bytes start))) 1f0) 32768f0))))
          (t 0))))

;;;----------------------------------------------------------------------------------
;;; Writing
;;;----------------------------------------------------------------------------------

;; drwav_init_memory_write() + drwav_write_pcm_frames() + drwav_uninit() for a RIFF container
;; DATA is the raw little-endian sample data, returns the file data as a byte vector
(defun drwav-write-memory (format-tag channels sample-rate bits-per-sample frame-count data)
  (let* ((data-size (floor (* frame-count bits-per-sample channels) 8))
         (padding (%drwav-chunk-padding-size-riff data-size))
         (block-align (floor (* channels bits-per-sample) 8))
         (avg-bytes-per-sec (floor (* bits-per-sample sample-rate channels) 8))
         ;; The "RIFF" chunk size, 4 = "WAVE", 24 = "fmt " chunk, 8 = "data" + u32 data size
         (riff-chunk-size (min #xffffffff (+ 4 24 8 data-size padding)))
         (out (make-array (+ 44 data-size padding) :element-type '(unsigned-byte 8) :initial-element 0))
         (pos 0))
    (labels ((write-fourcc (s) (loop for ch across s do (setf (aref out pos) (char-code ch)) (incf pos)))
             (write-le (value size) (dotimes (i size) (setf (aref out pos) (ldb (byte 8 (* i 8)) value)) (incf pos))))
      (write-fourcc "RIFF") (write-le riff-chunk-size 4) (write-fourcc "WAVE")
      (write-fourcc "fmt ") (write-le 16 4)
      (write-le format-tag 2) (write-le channels 2) (write-le sample-rate 4)
      (write-le avg-bytes-per-sec 4) (write-le block-align 2) (write-le bits-per-sample 2)
      (write-fourcc "data") (write-le (min data-size #xffffffff) 4)
      (replace out data :start1 pos :end2 data-size))
    out))
