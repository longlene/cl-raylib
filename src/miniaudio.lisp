(in-package #:cl-raylib)

;;;===================================================================================
;;; miniaudio - Subset of miniaudio required by raudio
;;; Port of raylib/src/external/miniaudio.h (v0.11.x)
;;;
;;; Ported parts:
;;;   - PCM sample format conversion (ma_pcm_convert, reference versions, no dithering)
;;;   - Channel converter (passthrough, mono in, mono out)
;;;   - Low-pass filters (ma_lpf1, ma_biquad/ma_lpf2, ma_lpf)
;;;   - Linear resampler (ma_linear_resampler)
;;;   - Data converter (ma_data_converter, ma_convert_frames)
;;;   - Playback device: replaces miniaudio backends with a PulseAudio (libpulse-simple)
;;;     output thread that calls the device data callback like ma_device does
;;;
;;; NOTE: Sample buffers are typed Lisp arrays: u8 -> (unsigned-byte 8), s16 -> (signed-byte 16),
;;; f32 -> single-float. Positions and counts are given in samples/frames instead of bytes
;;; NOTE: Only formats u8, s16 and f32 are supported (the ones used by raudio)
;;; NOTE: Channel conversion between multichannel layouts (weights/shuffle paths) is
;;; not ported, channels are copied by index and missing channels are silenced
;;; NOTE: f32 -> s16 conversion uses the reference (clipping) version, miniaudio SSE2
;;; version saturates out of range negative samples to -32768 instead of -32767
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

;; ma_format
(defconstant +ma-format-unknown+ 0)
(defconstant +ma-format-u8+ 1)
(defconstant +ma-format-s16+ 2)
(defconstant +ma-format-s24+ 3)
(defconstant +ma-format-s32+ 4)
(defconstant +ma-format-f32+ 5)

(defconstant +ma-max-filter-order+ 8)
(defconstant +ma-default-resampler-lpf-order+ 4)
(defconstant +ma-biquad-fixed-point-shift+ 14)

(deftype %ma-buffer ()
  '(or (simple-array (unsigned-byte 8) (*))
       (simple-array (signed-byte 16) (*))
       (simple-array single-float (*))))

(defun ma-get-bytes-per-sample (format)
  (case format (1 1) (2 2) (3 3) (4 4) (5 4) (t 0)))

(defun ma-get-bytes-per-frame (format channels)
  (* (ma-get-bytes-per-sample format) channels))

(defun ma-get-format-name (format)
  (case format
    (1 "8-bit Unsigned Integer")
    (2 "16-bit Signed Integer")
    (3 "24-bit Signed Integer (Tightly Packed)")
    (4 "32-bit Signed Integer")
    (5 "32-bit IEEE Floating Point")
    (t "Unknown")))

(defun %ma-make-buffer (format sample-count)
  "Allocate a zeroed sample buffer for FORMAT (RL_CALLOC equivalent)"
  (ecase format
    (1 (make-array sample-count :element-type '(unsigned-byte 8) :initial-element 0))
    (2 (make-array sample-count :element-type '(signed-byte 16) :initial-element 0))
    (5 (make-array sample-count :element-type 'single-float :initial-element 0f0))))

(defun %ma-buffer-format (buffer)
  "Get the ma_format of a typed sample buffer"
  (etypecase buffer
    ((simple-array (unsigned-byte 8) (*)) +ma-format-u8+)
    ((simple-array (signed-byte 16) (*)) +ma-format-s16+)
    ((simple-array single-float (*)) +ma-format-f32+)))

(declaim (inline %i16 %i32 %u32 %ma-clip-f32))
(defun %i16 (x) (- (logand (+ x 32768) #xffff) 32768))
(defun %i32 (x) (- (logand (+ x #x80000000) #xffffffff) #x80000000))
(defun %u32 (x) (logand x #xffffffff))
(defun %ma-clip-f32 (x)
  (declare (type single-float x))
  (cond ((< x -1f0) -1f0) ((> x 1f0) 1f0) (t x)))

(defun ma-gcf-u32 (a b)
  (loop until (= b 0)
        do (psetf a b b (mod a b)))
  a)

;;;----------------------------------------------------------------------------------
;;; Format Conversion
;;;----------------------------------------------------------------------------------

;; Convert COUNT samples from IN (starting at IN-START) to OUT (starting at OUT-START)
;; NOTE: Dither mode is always ma_dither_mode_none
(defun ma-pcm-convert (out format-out out-start in format-in in-start count)
  (declare (type fixnum out-start in-start count)
           (optimize speed (safety 0)))
  (if (= format-out format-in)
      (replace out in :start1 out-start :start2 in-start :end2 (+ in-start count))
      (macrolet ((conv ((out-type in-type) x expr)
                   `(let ((out out) (in in))
                      (declare (type (simple-array ,out-type (*)) out)
                               (type (simple-array ,in-type (*)) in))
                      (dotimes (i count)
                        (let ((,x (aref in (+ in-start i))))
                          (setf (aref out (+ out-start i)) ,expr))))))
        (cond
          ;; ma_pcm_u8_to_s16__reference
          ((and (= format-in +ma-format-u8+) (= format-out +ma-format-s16+))
           (conv ((signed-byte 16) (unsigned-byte 8)) x (ash (- x 128) 8)))
          ;; ma_pcm_u8_to_f32__reference
          ((and (= format-in +ma-format-u8+) (= format-out +ma-format-f32+))
           (conv (single-float (unsigned-byte 8)) x
                 (- (* (float x 1f0) 0.00784313725490196078f0) 1f0)))
          ;; ma_pcm_s16_to_u8__reference
          ((and (= format-in +ma-format-s16+) (= format-out +ma-format-u8+))
           (conv ((unsigned-byte 8) (signed-byte 16)) x (logand (+ (ash x -8) 128) #xff)))
          ;; ma_pcm_s16_to_f32__reference
          ((and (= format-in +ma-format-s16+) (= format-out +ma-format-f32+))
           (conv (single-float (signed-byte 16)) x (* (float x 1f0) 0.000030517578125f0)))
          ;; ma_pcm_f32_to_u8__reference
          ((and (= format-in +ma-format-f32+) (= format-out +ma-format-u8+))
           (conv ((unsigned-byte 8) single-float) x
                 (the (unsigned-byte 8) (truncate (* (+ (%ma-clip-f32 x) 1f0) 127.5f0)))))
          ;; ma_pcm_f32_to_s16__reference
          ((and (= format-in +ma-format-f32+) (= format-out +ma-format-s16+))
           (conv ((signed-byte 16) single-float) x
                 (the (signed-byte 16) (truncate (* (%ma-clip-f32 x) 32767f0)))))
          (t (error "miniaudio: Unsupported format conversion ~a -> ~a" format-in format-out)))))
  out)

(defun ma-convert-pcm-frames-format (out format-out out-start in format-in in-start frame-count channels)
  (ma-pcm-convert out format-out (* out-start channels) in format-in (* in-start channels) (* frame-count channels)))

;;;----------------------------------------------------------------------------------
;;; Low-Pass Filtering
;;;----------------------------------------------------------------------------------

;; First order low-pass filter (ma_lpf1)
(defstruct (ma-lpf1 (:constructor %make-ma-lpf1))
  (format 0 :type fixnum)
  (channels 0 :type fixnum)
  (a 0)                                 ; single-float (f32) or fixed point integer (s16)
  (r1 nil))                             ; Per channel state

;; Biquad filter, used by second order low-pass filter (ma_biquad, ma_lpf2)
(defstruct (ma-biquad (:constructor %make-ma-biquad))
  (format 0 :type fixnum)
  (channels 0 :type fixnum)
  (b0 0) (b1 0) (b2 0) (a1 0) (a2 0)
  (r1 nil)
  (r2 nil))

;; Low-pass filter of any order (ma_lpf)
(defstruct (ma-lpf (:constructor %make-ma-lpf))
  (format 0 :type fixnum)
  (channels 0 :type fixnum)
  (lpf1 #() :type simple-vector)
  (lpf2 #() :type simple-vector))

(defun %ma-biquad-float-to-fp (x)
  (%i32 (truncate (* x (ash 1 +ma-biquad-fixed-point-shift+)))))

(defun %ma-make-state (format channels)
  (if (= format +ma-format-f32+)
      (make-array channels :element-type 'single-float :initial-element 0f0)
      (make-array channels :element-type '(signed-byte 32) :initial-element 0)))

(defun ma-lpf1-reinit (lpf format channels sample-rate cutoff-frequency)
  (let ((a (exp (/ (* -2 pi cutoff-frequency) sample-rate))))
    (setf (ma-lpf1-format lpf) format
          (ma-lpf1-channels lpf) channels
          (ma-lpf1-a lpf) (if (= format +ma-format-f32+)
                              (coerce a 'single-float)
                              (%ma-biquad-float-to-fp a)))
    lpf))

(defun ma-lpf1-init (format channels sample-rate cutoff-frequency)
  (ma-lpf1-reinit (%make-ma-lpf1 :r1 (%ma-make-state format channels))
                  format channels sample-rate cutoff-frequency))

;; ma_lpf2__get_biquad_config() + ma_biquad_reinit()
(defun ma-lpf2-reinit (bq format channels sample-rate cutoff-frequency q)
  (when (= q 0) (setf q 0.707107d0))
  (let* ((w (/ (* 2 pi cutoff-frequency) sample-rate))
         (s (sin w))
         (c (sin (- (* pi 0.5d0) w)))   ; ma_cosd()
         (a (/ s (* 2 q)))
         (b0 (/ (- 1 c) 2))
         (b1 (- 1 c))
         (b2 (/ (- 1 c) 2))
         (a0 (+ 1 a))
         (a1 (* -2 c))
         (a2 (- 1 a)))
    (setf (ma-biquad-format bq) format
          (ma-biquad-channels bq) channels)
    (flet ((norm (x)
             (if (= format +ma-format-f32+)
                 (coerce (/ x a0) 'single-float)
                 (%ma-biquad-float-to-fp (/ x a0)))))
      (setf (ma-biquad-b0 bq) (norm b0)
            (ma-biquad-b1 bq) (norm b1)
            (ma-biquad-b2 bq) (norm b2)
            (ma-biquad-a1 bq) (norm a1)
            (ma-biquad-a2 bq) (norm a2)))
    bq))

(defun ma-lpf2-init (format channels sample-rate cutoff-frequency q)
  (ma-lpf2-reinit (%make-ma-biquad :r1 (%ma-make-state format channels)
                                   :r2 (%ma-make-state format channels))
                  format channels sample-rate cutoff-frequency q))

;; ma_lpf_reinit__internal()
(defun %ma-lpf-reinit (lpf format channels sample-rate cutoff-frequency order is-new)
  (let ((lpf1-count (mod order 2))
        (lpf2-count (floor order 2)))
    (when is-new
      (setf (ma-lpf-lpf1 lpf) (make-array lpf1-count)
            (ma-lpf-lpf2 lpf) (make-array lpf2-count)))
    (dotimes (i lpf1-count)
      (if is-new
          (setf (svref (ma-lpf-lpf1 lpf) i) (ma-lpf1-init format channels sample-rate cutoff-frequency))
          (ma-lpf1-reinit (svref (ma-lpf-lpf1 lpf) i) format channels sample-rate cutoff-frequency)))
    (dotimes (i lpf2-count)
      ;; Tempting to use 0.707107, but won't result in a Butterworth filter if the order is > 2
      (let* ((a (if (= lpf1-count 1)
                    (* (+ 1 i) (/ pi order))           ; Odd order
                    (* (+ 1 (* i 2)) (/ pi (* order 2))))) ; Even order
             (q (/ 1 (* 2 (sin (- (* pi 0.5d0) a))))))
        (if is-new
            (setf (svref (ma-lpf-lpf2 lpf) i) (ma-lpf2-init format channels sample-rate cutoff-frequency q))
            (ma-lpf2-reinit (svref (ma-lpf-lpf2 lpf) i) format channels sample-rate cutoff-frequency q))))
    (setf (ma-lpf-format lpf) format
          (ma-lpf-channels lpf) channels)
    lpf))

(defun ma-lpf-init (format channels sample-rate cutoff-frequency order)
  (%ma-lpf-reinit (%make-ma-lpf) format channels sample-rate cutoff-frequency
                  (min order +ma-max-filter-order+) t))

(defun ma-lpf-reinit (lpf format channels sample-rate cutoff-frequency order)
  (%ma-lpf-reinit lpf format channels sample-rate cutoff-frequency (min order +ma-max-filter-order+) nil))

;; Process one frame in place, FRAME is a buffer and START the position of the first sample
(defun %ma-lpf-process-pcm-frame-f32 (lpf frame start)
  (declare (type (simple-array single-float (*)) frame)
           (type fixnum start)
           (optimize speed (safety 0)))
  (let ((channels (ma-lpf-channels lpf)))
    (loop for f across (ma-lpf-lpf1 lpf)
          do (let* ((a (the single-float (ma-lpf1-a f)))
                    (b (- 1f0 a))
                    (r1 (ma-lpf1-r1 f)))
               (declare (type (simple-array single-float (*)) r1))
               (dotimes (c channels)
                 (let ((y (+ (* b (aref frame (+ start c))) (* a (aref r1 c)))))
                   (setf (aref frame (+ start c)) y
                         (aref r1 c) y)))))
    (loop for bq across (ma-lpf-lpf2 lpf)
          do (let ((b0 (the single-float (ma-biquad-b0 bq)))
                   (b1 (the single-float (ma-biquad-b1 bq)))
                   (b2 (the single-float (ma-biquad-b2 bq)))
                   (a1 (the single-float (ma-biquad-a1 bq)))
                   (a2 (the single-float (ma-biquad-a2 bq)))
                   (pr1 (ma-biquad-r1 bq))
                   (pr2 (ma-biquad-r2 bq)))
               (declare (type (simple-array single-float (*)) pr1 pr2))
               ;; ma_biquad_process_pcm_frame_f32__direct_form_2_transposed()
               (dotimes (c channels)
                 (let* ((r1 (aref pr1 c))
                        (r2 (aref pr2 c))
                        (x (aref frame (+ start c)))
                        (y (+ (* b0 x) r1)))
                   (setf r1 (+ (- (* b1 x) (* a1 y)) r2)
                         r2 (- (* b2 x) (* a2 y)))
                   (setf (aref frame (+ start c)) y
                         (aref pr1 c) r1
                         (aref pr2 c) r2)))))))

(defun %ma-lpf-process-pcm-frame-s16 (lpf frame start)
  (declare (type (simple-array (signed-byte 16) (*)) frame)
           (type fixnum start))
  (let ((channels (ma-lpf-channels lpf))
        (shift +ma-biquad-fixed-point-shift+))
    (loop for f across (ma-lpf-lpf1 lpf)
          do (let* ((a (ma-lpf1-a f))
                    (b (- (ash 1 shift) a))
                    (r1 (ma-lpf1-r1 f)))
               (declare (type (simple-array (signed-byte 32) (*)) r1))
               (dotimes (c channels)
                 (let ((y (ash (%i32 (+ (* b (aref frame (+ start c))) (* a (aref r1 c)))) (- shift))))
                   (setf (aref frame (+ start c)) (%i16 y)
                         (aref r1 c) y)))))
    (loop for bq across (ma-lpf-lpf2 lpf)
          do (let ((b0 (ma-biquad-b0 bq)) (b1 (ma-biquad-b1 bq)) (b2 (ma-biquad-b2 bq))
                   (a1 (ma-biquad-a1 bq)) (a2 (ma-biquad-a2 bq))
                   (pr1 (ma-biquad-r1 bq)) (pr2 (ma-biquad-r2 bq)))
               (declare (type (simple-array (signed-byte 32) (*)) pr1 pr2))
               ;; ma_biquad_process_pcm_frame_s16__direct_form_2_transposed()
               (dotimes (c channels)
                 (let* ((r1 (aref pr1 c))
                        (r2 (aref pr2 c))
                        (x (aref frame (+ start c)))
                        (y (ash (%i32 (+ (* b0 x) r1)) (- shift))))
                   (setf r1 (%i32 (+ (- (* b1 x) (* a1 y)) r2))
                         r2 (%i32 (- (* b2 x) (* a2 y))))
                   (setf (aref frame (+ start c)) (max -32768 (min 32767 y))
                         (aref pr1 c) r1
                         (aref pr2 c) r2)))))))

;;;----------------------------------------------------------------------------------
;;; Linear Resampler
;;;----------------------------------------------------------------------------------

(defstruct (ma-linear-resampler (:constructor %make-ma-linear-resampler))
  (format 0 :type fixnum)
  (channels 0 :type fixnum)
  (sample-rate-in 0 :type (unsigned-byte 32))   ; Simplified by the greatest common factor
  (sample-rate-out 0 :type (unsigned-byte 32))
  (lpf-order 0 :type fixnum)
  (lpf-nyquist-factor 1d0 :type double-float)
  (in-advance-int 0 :type (unsigned-byte 32))
  (in-advance-frac 0 :type (unsigned-byte 32))
  (in-time-int 0 :type (unsigned-byte 32))
  (in-time-frac 0 :type (unsigned-byte 32))
  (x0 nil)                                      ; The previous input frame
  (x1 nil)                                      ; The next input frame
  (lpf nil))

;; ma_linear_resampler_adjust_timer_for_new_rate()
(defun %ma-linear-resampler-adjust-timer-for-new-rate (r old-sample-rate-out new-sample-rate-out)
  (let ((old-whole (floor (ma-linear-resampler-in-time-frac r) old-sample-rate-out))
        (old-fract (mod (ma-linear-resampler-in-time-frac r) old-sample-rate-out)))
    (setf (ma-linear-resampler-in-time-frac r)
          (%u32 (+ (%u32 (* old-whole new-sample-rate-out))
                   (floor (%u32 (* old-fract new-sample-rate-out)) old-sample-rate-out))))
    ;; Make sure the fractional part is less than the output sample rate
    (multiple-value-bind (whole frac)
        (floor (ma-linear-resampler-in-time-frac r) (ma-linear-resampler-sample-rate-out r))
      (setf (ma-linear-resampler-in-time-int r) (%u32 (+ (ma-linear-resampler-in-time-int r) whole))
            (ma-linear-resampler-in-time-frac r) frac))))

;; ma_linear_resampler_set_rate_internal()
(defun %ma-linear-resampler-set-rate-internal (r sample-rate-in sample-rate-out initialized-p)
  (when (or (= sample-rate-in 0) (= sample-rate-out 0))
    (return-from %ma-linear-resampler-set-rate-internal nil))
  (let ((old-sample-rate-out (ma-linear-resampler-sample-rate-out r))
        (gcf (ma-gcf-u32 sample-rate-in sample-rate-out)))
    ;; Simplify the sample rate
    (setf (ma-linear-resampler-sample-rate-in r) (floor sample-rate-in gcf)
          (ma-linear-resampler-sample-rate-out r) (floor sample-rate-out gcf))
    ;; Always initialize the low-pass filter, even when the order is 0
    (let* ((in (ma-linear-resampler-sample-rate-in r))
           (out (ma-linear-resampler-sample-rate-out r))
           (lpf-sample-rate (max in out))
           (lpf-cutoff-frequency (* (min in out) 0.5d0 (ma-linear-resampler-lpf-nyquist-factor r))))
      ;; If the resampler is already initialized, the low-pass filter is re-initialized
      ;; to keep the cached frames
      (if initialized-p
          (ma-lpf-reinit (ma-linear-resampler-lpf r) (ma-linear-resampler-format r) (ma-linear-resampler-channels r)
                         lpf-sample-rate lpf-cutoff-frequency (ma-linear-resampler-lpf-order r))
          (setf (ma-linear-resampler-lpf r)
                (ma-lpf-init (ma-linear-resampler-format r) (ma-linear-resampler-channels r)
                             lpf-sample-rate lpf-cutoff-frequency (ma-linear-resampler-lpf-order r))))
      (multiple-value-bind (advance-int advance-frac) (floor in out)
        (setf (ma-linear-resampler-in-advance-int r) advance-int
              (ma-linear-resampler-in-advance-frac r) advance-frac))
      ;; Our timer was based on the old rate, it needs to be adjusted for the new rate
      (%ma-linear-resampler-adjust-timer-for-new-rate r old-sample-rate-out out))
    t))

;; ma_linear_resampler_config_init() + ma_linear_resampler_init()
(defun ma-linear-resampler-init (format channels sample-rate-in sample-rate-out
                                 &optional (lpf-order (min +ma-default-resampler-lpf-order+ +ma-max-filter-order+)))
  (unless (or (= format +ma-format-f32+) (= format +ma-format-s16+))
    (return-from ma-linear-resampler-init nil))
  (let ((r (%make-ma-linear-resampler :format format :channels channels
                                      :sample-rate-in sample-rate-in :sample-rate-out sample-rate-out
                                      :lpf-order lpf-order :lpf-nyquist-factor 1d0
                                      :x0 (%ma-make-buffer format channels)
                                      :x1 (%ma-make-buffer format channels))))
    (when (%ma-linear-resampler-set-rate-internal r sample-rate-in sample-rate-out nil)
      ;; Set this to one to force an input sample to always be loaded for the first output frame
      (setf (ma-linear-resampler-in-time-int r) 1
            (ma-linear-resampler-in-time-frac r) 0)
      r)))

(defun ma-linear-resampler-set-rate (r sample-rate-in sample-rate-out)
  (%ma-linear-resampler-set-rate-internal r sample-rate-in sample-rate-out t))

;; Advance the time forward after generating an output frame
(defmacro %ma-linear-resampler-advance-time (r)
  `(progn
     (setf (ma-linear-resampler-in-time-int ,r) (+ (ma-linear-resampler-in-time-int ,r) (ma-linear-resampler-in-advance-int ,r))
           (ma-linear-resampler-in-time-frac ,r) (+ (ma-linear-resampler-in-time-frac ,r) (ma-linear-resampler-in-advance-frac ,r)))
     (when (>= (ma-linear-resampler-in-time-frac ,r) (ma-linear-resampler-sample-rate-out ,r))
       (decf (ma-linear-resampler-in-time-frac ,r) (ma-linear-resampler-sample-rate-out ,r))
       (incf (ma-linear-resampler-in-time-int ,r)))))

;; ma_linear_resampler_process_pcm_frames_f32_downsample/upsample()
(defun %ma-linear-resampler-process-pcm-frames-f32 (r in in-start frame-count-in out out-start frame-count-out)
  (declare (type (simple-array single-float (*)) in out)
           (type fixnum in-start frame-count-in out-start frame-count-out)
           (optimize speed (safety 0)))
  (let* ((channels (ma-linear-resampler-channels r))
         (x0 (ma-linear-resampler-x0 r))
         (x1 (ma-linear-resampler-x1 r))
         (lpf (ma-linear-resampler-lpf r))
         (filter-p (/= (ma-linear-resampler-sample-rate-in r) (ma-linear-resampler-sample-rate-out r)))
         (downsample-p (> (ma-linear-resampler-sample-rate-in r) (ma-linear-resampler-sample-rate-out r)))
         (frames-processed-in 0)
         (frames-processed-out 0)
         (in-pos (* in-start channels))
         (out-pos (* out-start channels)))
    (declare (type (simple-array single-float (*)) x0 x1)
             (type fixnum channels frames-processed-in frames-processed-out in-pos out-pos))
    (loop while (< frames-processed-out frame-count-out)
          do ;; Before interpolating we need to load the buffers
             (loop while (and (> (ma-linear-resampler-in-time-int r) 0) (> frame-count-in frames-processed-in))
                   do (dotimes (c channels)
                        (setf (aref x0 c) (aref x1 c)
                              (aref x1 c) (aref in (+ in-pos c))))
                      (incf in-pos channels)
                      ;; Filter (downsampling filters every input sample)
                      ;; Do not apply filtering if sample rates are the same or else you'll get dangerous glitching
                      (when (and downsample-p filter-p)
                        (%ma-lpf-process-pcm-frame-f32 lpf x1 0))
                      (incf frames-processed-in)
                      (decf (ma-linear-resampler-in-time-int r)))
             (when (> (ma-linear-resampler-in-time-int r) 0)
               (return))                ; Ran out of input data
             ;; Getting here means the frames have been loaded and we can generate the next output frame
             ;; ma_linear_resampler_interpolate_frame_f32()
             (let ((a (/ (float (ma-linear-resampler-in-time-frac r) 1f0)
                         (float (ma-linear-resampler-sample-rate-out r) 1f0))))
               (declare (type single-float a))
               (dotimes (c channels)
                 ;; ma_mix_f32_fast()
                 (setf (aref out (+ out-pos c)) (+ (aref x0 c) (* (- (aref x1 c) (aref x0 c)) a)))))
             ;; Filter (upsampling filters every output sample)
             (when (and (not downsample-p) filter-p)
               (%ma-lpf-process-pcm-frame-f32 lpf out out-pos))
             (incf out-pos channels)
             (incf frames-processed-out)
             (%ma-linear-resampler-advance-time r))
    (values frames-processed-in frames-processed-out)))

;; ma_linear_resampler_process_pcm_frames_s16_downsample/upsample()
(defun %ma-linear-resampler-process-pcm-frames-s16 (r in in-start frame-count-in out out-start frame-count-out)
  (declare (type (simple-array (signed-byte 16) (*)) in out)
           (type fixnum in-start frame-count-in out-start frame-count-out))
  (let* ((channels (ma-linear-resampler-channels r))
         (x0 (ma-linear-resampler-x0 r))
         (x1 (ma-linear-resampler-x1 r))
         (lpf (ma-linear-resampler-lpf r))
         (filter-p (/= (ma-linear-resampler-sample-rate-in r) (ma-linear-resampler-sample-rate-out r)))
         (downsample-p (> (ma-linear-resampler-sample-rate-in r) (ma-linear-resampler-sample-rate-out r)))
         (shift 12)
         (frames-processed-in 0)
         (frames-processed-out 0)
         (in-pos (* in-start channels))
         (out-pos (* out-start channels)))
    (declare (type (simple-array (signed-byte 16) (*)) x0 x1)
             (type fixnum channels frames-processed-in frames-processed-out in-pos out-pos))
    (loop while (< frames-processed-out frame-count-out)
          do (loop while (and (> (ma-linear-resampler-in-time-int r) 0) (> frame-count-in frames-processed-in))
                   do (dotimes (c channels)
                        (setf (aref x0 c) (aref x1 c)
                              (aref x1 c) (aref in (+ in-pos c))))
                      (incf in-pos channels)
                      (when (and downsample-p filter-p)
                        (%ma-lpf-process-pcm-frame-s16 lpf x1 0))
                      (incf frames-processed-in)
                      (decf (ma-linear-resampler-in-time-int r)))
             (when (> (ma-linear-resampler-in-time-int r) 0)
               (return))
             ;; ma_linear_resampler_interpolate_frame_s16()
             (let ((a (floor (%u32 (ash (ma-linear-resampler-in-time-frac r) shift))
                             (ma-linear-resampler-sample-rate-out r))))
               (dotimes (c channels)
                 ;; ma_linear_resampler_mix_s16()
                 (let ((b (* (aref x0 c) (- (ash 1 shift) a)))
                       (d (* (aref x1 c) a)))
                   (setf (aref out (+ out-pos c)) (%i16 (ash (%i32 (+ b d)) (- shift)))))))
             (when (and (not downsample-p) filter-p)
               (%ma-lpf-process-pcm-frame-s16 lpf out out-pos))
             (incf out-pos channels)
             (incf frames-processed-out)
             (%ma-linear-resampler-advance-time r))
    (values frames-processed-in frames-processed-out)))

(defun ma-linear-resampler-process-pcm-frames (r in in-start frame-count-in out out-start frame-count-out)
  "Resample frames, returns (values input-frames-consumed output-frames-generated)"
  (if (= (ma-linear-resampler-format r) +ma-format-s16+)
      (%ma-linear-resampler-process-pcm-frames-s16 r in in-start frame-count-in out out-start frame-count-out)
      (%ma-linear-resampler-process-pcm-frames-f32 r in in-start frame-count-in out out-start frame-count-out)))

(defun ma-linear-resampler-get-required-input-frame-count (r output-frame-count)
  (if (= output-frame-count 0)
      0
      ;; Any whole input frames are consumed before the first output frame is generated
      (let ((output-frame-count (- output-frame-count 1)))
        (+ (ma-linear-resampler-in-time-int r)
           (* output-frame-count (ma-linear-resampler-in-advance-int r))
           (floor (+ (ma-linear-resampler-in-time-frac r) (* output-frame-count (ma-linear-resampler-in-advance-frac r)))
                  (ma-linear-resampler-sample-rate-out r))))))

(defun ma-linear-resampler-get-expected-output-frame-count (r input-frame-count)
  (let* ((output-frame-count (floor (* input-frame-count (ma-linear-resampler-sample-rate-out r))
                                    (ma-linear-resampler-sample-rate-in r)))
         (preliminary-input-frame-count-from-frac
           (floor (+ (ma-linear-resampler-in-time-frac r) (* output-frame-count (ma-linear-resampler-in-advance-frac r)))
                  (ma-linear-resampler-sample-rate-out r)))
         (preliminary-input-frame-count
           (+ (ma-linear-resampler-in-time-int r) (* output-frame-count (ma-linear-resampler-in-advance-int r))
              preliminary-input-frame-count-from-frac)))
    ;; If the input frames required for the preliminary output frame count are available,
    ;; an extra output frame can be generated
    (if (<= preliminary-input-frame-count input-frame-count)
        (+ output-frame-count 1)
        output-frame-count)))

;; Resampler wrapper (ma_resampler), keeps the original (not simplified) sample rates
(defstruct (ma-resampler (:constructor %make-ma-resampler))
  (format 0 :type fixnum)
  (channels 0 :type fixnum)
  (sample-rate-in 0 :type (unsigned-byte 32))
  (sample-rate-out 0 :type (unsigned-byte 32))
  (linear nil))

(defun ma-resampler-init (format channels sample-rate-in sample-rate-out lpf-order)
  (let ((linear (ma-linear-resampler-init format channels sample-rate-in sample-rate-out lpf-order)))
    (when linear
      (%make-ma-resampler :format format :channels channels
                          :sample-rate-in sample-rate-in :sample-rate-out sample-rate-out
                          :linear linear))))

(defun ma-resampler-set-rate (resampler sample-rate-in sample-rate-out)
  (when (and (> sample-rate-in 0) (> sample-rate-out 0)
             (ma-linear-resampler-set-rate (ma-resampler-linear resampler) sample-rate-in sample-rate-out))
    (setf (ma-resampler-sample-rate-in resampler) sample-rate-in
          (ma-resampler-sample-rate-out resampler) sample-rate-out)
    t))

;;;----------------------------------------------------------------------------------
;;; Channel Conversion
;;;----------------------------------------------------------------------------------

;; Convert FRAME-COUNT frames between CHANNELS-IN and CHANNELS-OUT, buffers in the same format
(defun %ma-channel-converter-process-pcm-frames (path out out-start in in-start frame-count channels-in channels-out)
  (declare (type fixnum out-start in-start frame-count channels-in channels-out))
  (ecase path
    (:passthrough
     (replace out in :start1 (* out-start channels-out)
                     :start2 (* in-start channels-in) :end2 (* (+ in-start frame-count) channels-in)))
    ;; ma_channel_converter_process_pcm_frames__mono_in(): duplicate the mono channel
    (:mono-in
     (dotimes (i frame-count)
       (let ((x (aref in (+ in-start i))))
         (dotimes (c channels-out)
           (setf (aref out (+ (* (+ out-start i) channels-out) c)) x)))))
    ;; ma_channel_converter_process_pcm_frames__mono_out(): average all the channels
    (:mono-out
     (etypecase in
       ((simple-array single-float (*))
        (dotimes (i frame-count)
          (let ((tt 0f0))
            (declare (type single-float tt))
            (dotimes (c channels-in)
              (setf tt (+ tt (aref in (+ (* (+ in-start i) channels-in) c)))))
            (setf (aref out (+ out-start i)) (/ tt (float channels-in 1f0))))))
       ((simple-array (signed-byte 16) (*))
        (dotimes (i frame-count)
          (let ((tt 0))
            (dotimes (c channels-in)
              (incf tt (aref in (+ (* (+ in-start i) channels-in) c))))
            ;; NOTE: ma_int32 divided by ma_uint32 channel count, negative sums are converted to unsigned
            (setf (aref out (+ out-start i)) (%i16 (floor (%u32 tt) channels-in))))))))
    ;; NOTE: Weights/shuffle conversion paths are not ported, copy channels by index
    (:weights
     (dotimes (i frame-count)
       (dotimes (c channels-out)
         (setf (aref out (+ (* (+ out-start i) channels-out) c))
               (if (< c channels-in)
                   (aref in (+ (* (+ in-start i) channels-in) c))
                   (if (typep out '(simple-array single-float (*))) 0f0 0))))))))

;;;----------------------------------------------------------------------------------
;;; Data Conversion
;;;----------------------------------------------------------------------------------

(defstruct (ma-data-converter (:constructor %make-ma-data-converter))
  (format-in 0 :type fixnum)
  (format-out 0 :type fixnum)
  (channels-in 0 :type fixnum)
  (channels-out 0 :type fixnum)
  (sample-rate-in 0 :type (unsigned-byte 32))
  (sample-rate-out 0 :type (unsigned-byte 32))
  (mid-format 0 :type fixnum)
  (channel-conversion-path :passthrough)
  (resampler nil)                               ; ma_resampler, NIL if not required
  (has-pre-format-conversion nil)
  (has-post-format-conversion nil)
  (has-channel-converter nil)
  (has-resampler nil)
  (execution-path :passthrough)
  (scratch (make-array 3 :initial-element nil) :type simple-vector))  ; Temp buffers for the conversion stages

;; ma_data_converter_config_init() + ma_data_converter_init()
;; NOTE: Default config uses linear resampling with LPF order 1 (ma_data_converter_config_init_default)
(defun ma-data-converter-init (format-in format-out channels-in channels-out sample-rate-in sample-rate-out
                               &key allow-dynamic-sample-rate (lpf-order 1))
  (when (or (= channels-in 0) (= channels-out 0))
    (return-from ma-data-converter-init nil))
  (let* ((resampling-required (or allow-dynamic-sample-rate (/= sample-rate-in sample-rate-out)))
         ;; ma_data_converter_config_get_mid_format()
         (mid-format (cond ((or (= format-out +ma-format-s16+) (= format-out +ma-format-f32+)) format-out)
                           ((or (= format-in +ma-format-s16+) (= format-in +ma-format-f32+)) format-in)
                           (t +ma-format-f32+)))
         ;; ma_channel_map_get_conversion_path() with default channel maps
         (channel-path (cond ((= channels-in channels-out) :passthrough)
                             ((= channels-out 1) :mono-out)
                             ((= channels-in 1) :mono-in)
                             (t :weights)))
         (conv (%make-ma-data-converter :format-in format-in :format-out format-out
                                        :channels-in channels-in :channels-out channels-out
                                        :sample-rate-in sample-rate-in :sample-rate-out sample-rate-out
                                        :mid-format mid-format :channel-conversion-path channel-path)))
    (setf (ma-data-converter-has-channel-converter conv) (not (eq channel-path :passthrough)))
    (when resampling-required
      ;; The resampler is used at the stage where the channel count is at it's lowest
      (let ((resampler (ma-resampler-init mid-format (min channels-in channels-out)
                                          sample-rate-in sample-rate-out lpf-order)))
        (unless resampler
          (return-from ma-data-converter-init nil))
        (setf (ma-data-converter-resampler conv) resampler
              (ma-data-converter-has-resampler conv) t)))
    ;; Pre- and post-format conversion
    (if (and (not (ma-data-converter-has-channel-converter conv)) (not (ma-data-converter-has-resampler conv)))
        (setf (ma-data-converter-has-post-format-conversion conv) (/= format-in format-out))
        (setf (ma-data-converter-has-pre-format-conversion conv) (/= format-in mid-format)
              (ma-data-converter-has-post-format-conversion conv) (/= format-out mid-format)))
    ;; Execution path
    (setf (ma-data-converter-execution-path conv)
          (cond ((and (not (ma-data-converter-has-pre-format-conversion conv))
                      (not (ma-data-converter-has-post-format-conversion conv))
                      (not (ma-data-converter-has-channel-converter conv))
                      (not (ma-data-converter-has-resampler conv)))
                 :passthrough)
                ((< channels-in channels-out)
                 (if (ma-data-converter-has-resampler conv) :resample-first :channels-only))
                ((ma-data-converter-has-channel-converter conv)
                 (if (ma-data-converter-has-resampler conv) :channels-first :channels-only))
                ((ma-data-converter-has-resampler conv) :resample-only)
                (t :format-only)))
    conv))

(defun %ma-data-converter-scratch (conv index format sample-count)
  "Get a reusable temp buffer of at least SAMPLE-COUNT samples"
  (let ((buffer (svref (ma-data-converter-scratch conv) index)))
    (if (and buffer (= (%ma-buffer-format buffer) format) (>= (length buffer) sample-count))
        buffer
        (setf (svref (ma-data-converter-scratch conv) index)
              (%ma-make-buffer format (max sample-count 1024))))))

;; ma_data_converter_process_pcm_frames()
;; NOTE: Intermediate stages are processed in one go instead of through fixed size stack buffers,
;; the output is the same since all the stages are processed sample by sample
(defun ma-data-converter-process-pcm-frames (conv in in-start frame-count-in out out-start frame-count-out)
  "Convert frames from IN to OUT, returns (values input-frames-consumed output-frames-generated)"
  (let ((fi (ma-data-converter-format-in conv))
        (fo (ma-data-converter-format-out conv))
        (ci (ma-data-converter-channels-in conv))
        (co (ma-data-converter-channels-out conv))
        (mid (ma-data-converter-mid-format conv))
        (path (ma-data-converter-channel-conversion-path conv)))
    (case (ma-data-converter-execution-path conv)
      (:passthrough
       (let ((n (min frame-count-in frame-count-out)))
         (replace out in :start1 (* out-start co) :start2 (* in-start ci) :end2 (* (+ in-start n) ci))
         (values n n)))
      (:format-only
       (let ((n (min frame-count-in frame-count-out)))
         (ma-convert-pcm-frames-format out fo out-start in fi in-start n ci)
         (values n n)))
      (otherwise
       ;; Pre-format conversion to the mid format
       (let ((a in) (a-start in-start)
             (n-in (if (eq (ma-data-converter-execution-path conv) :channels-only)
                       (min frame-count-in frame-count-out)
                       frame-count-in)))
         (when (ma-data-converter-has-pre-format-conversion conv)
           (setf a (%ma-data-converter-scratch conv 0 mid (* n-in ci)) a-start 0)
           (ma-convert-pcm-frames-format a mid 0 in fi in-start n-in ci))
         (flet ((post (buffer frames)
                  (when (ma-data-converter-has-post-format-conversion conv)
                    (ma-convert-pcm-frames-format out fo out-start buffer mid 0 frames co))))
           (let ((post-p (ma-data-converter-has-post-format-conversion conv)))
             (ecase (ma-data-converter-execution-path conv)
               (:channels-only
                (let ((b (if post-p (%ma-data-converter-scratch conv 1 mid (* n-in co)) out)))
                  (%ma-channel-converter-process-pcm-frames path b (if post-p 0 out-start) a a-start n-in ci co)
                  (post b n-in)
                  (values n-in n-in)))
               (:resample-only
                (let ((b (if post-p (%ma-data-converter-scratch conv 1 mid (* frame-count-out co)) out)))
                  (multiple-value-bind (consumed produced)
                      (ma-linear-resampler-process-pcm-frames (ma-resampler-linear (ma-data-converter-resampler conv))
                                                              a a-start n-in b (if post-p 0 out-start) frame-count-out)
                    (post b produced)
                    (values consumed produced))))
               (:resample-first
                (let ((b (%ma-data-converter-scratch conv 1 mid (* frame-count-out ci)))
                      (c (if post-p (%ma-data-converter-scratch conv 2 mid (* frame-count-out co)) out)))
                  (multiple-value-bind (consumed produced)
                      (ma-linear-resampler-process-pcm-frames (ma-resampler-linear (ma-data-converter-resampler conv))
                                                              a a-start n-in b 0 frame-count-out)
                    (%ma-channel-converter-process-pcm-frames path c (if post-p 0 out-start) b 0 produced ci co)
                    (post c produced)
                    (values consumed produced))))
               (:channels-first
                (let ((b (%ma-data-converter-scratch conv 1 mid (* n-in co)))
                      (c (if post-p (%ma-data-converter-scratch conv 2 mid (* frame-count-out co)) out)))
                  (%ma-channel-converter-process-pcm-frames path b 0 a a-start n-in ci co)
                  (multiple-value-bind (consumed produced)
                      (ma-linear-resampler-process-pcm-frames (ma-resampler-linear (ma-data-converter-resampler conv))
                                                              b 0 n-in c (if post-p 0 out-start) frame-count-out)
                    (post c produced)
                    (values consumed produced))))))))))))

(defun ma-data-converter-set-rate (conv sample-rate-in sample-rate-out)
  (when (ma-data-converter-has-resampler conv)
    (ma-resampler-set-rate (ma-data-converter-resampler conv) sample-rate-in sample-rate-out)))

(defun ma-data-converter-get-expected-output-frame-count (conv input-frame-count)
  (if (ma-data-converter-has-resampler conv)
      (ma-linear-resampler-get-expected-output-frame-count
       (ma-resampler-linear (ma-data-converter-resampler conv)) input-frame-count)
      input-frame-count))

;; ma_convert_frames(), OUT is NIL to get the required output frame count
(defun ma-convert-frames (out frame-count-out format-out channels-out sample-rate-out
                          in frame-count-in format-in channels-in sample-rate-in)
  (if (= frame-count-in 0)
      0
      (let ((conv (ma-data-converter-init format-in format-out channels-in channels-out sample-rate-in sample-rate-out
                                          :lpf-order (min +ma-default-resampler-lpf-order+ +ma-max-filter-order+))))
        (cond ((null conv) 0)
              ((null out) (ma-data-converter-get-expected-output-frame-count conv frame-count-in))
              (t (nth-value 1 (ma-data-converter-process-pcm-frames conv in 0 frame-count-in out 0 frame-count-out)))))))

;;;----------------------------------------------------------------------------------
;;; Playback Device (PulseAudio backend)
;;;----------------------------------------------------------------------------------

(cffi:define-foreign-library %libpulse-simple
  (:unix (:or "libpulse-simple.so.0" "libpulse-simple.so")))

(cffi:defcstruct %pa-sample-spec
  (format :int)
  (rate :uint32)
  (channels :uint8))

(cffi:defcstruct %pa-buffer-attr
  (maxlength :uint32)
  (tlength :uint32)
  (prebuf :uint32)
  (minreq :uint32)
  (fragsize :uint32))

(defconstant +pa-stream-playback+ 1)
(defconstant +pa-sample-float32le+ 5)

(defvar *ma-default-sample-rate* 48000 "Device sample rate used when none is requested")
(defvar *ma-default-period-size-in-frames* 1024
  "Device period size, larger than miniaudio's 10ms default to survive garbage collection pauses")
(defvar *ma-default-periods* 4)

(defstruct (ma-device (:constructor %make-ma-device))
  (format +ma-format-f32+ :type fixnum)
  (channels 2 :type fixnum)
  (sample-rate 0 :type fixnum)
  (internal-format +ma-format-f32+ :type fixnum)
  (internal-channels 2 :type fixnum)
  (internal-sample-rate 0 :type fixnum)
  (internal-period-size-in-frames 0 :type fixnum)
  (internal-periods 0 :type fixnum)
  (master-volume 1f0 :type single-float)
  (no-clip nil)
  (data-callback nil)                           ; (lambda (device frames-out frame-count))
  (callback-error nil)                          ; An error was already reported by the data callback
  (handle (cffi:null-pointer))                  ; pa_simple
  (thread nil)
  (running nil))

(defun ma-device-init (&key (format +ma-format-f32+) (channels 2) (sample-rate 0) (period-size-in-frames 0) data-callback)
  "Initialize a playback device, returns NIL on failure"
  (unless (= format +ma-format-f32+)
    (return-from ma-device-init nil))
  (handler-case (cffi:load-foreign-library '%libpulse-simple)
    (error (e)
      (trace-log +log-warning+ "miniaudio: Failed to load libpulse-simple: ~a" e)
      (return-from ma-device-init nil)))
  (let* ((sample-rate (if (= sample-rate 0) *ma-default-sample-rate* sample-rate))
         (period (if (= period-size-in-frames 0) *ma-default-period-size-in-frames* period-size-in-frames))
         (periods *ma-default-periods*)
         (bytes-per-frame (ma-get-bytes-per-frame format channels))
         (handle (cffi:with-foreign-objects ((ss '(:struct %pa-sample-spec))
                                             (attr '(:struct %pa-buffer-attr))
                                             (err :int))
                   (setf (cffi:foreign-slot-value ss '(:struct %pa-sample-spec) 'format) +pa-sample-float32le+
                         (cffi:foreign-slot-value ss '(:struct %pa-sample-spec) 'rate) sample-rate
                         (cffi:foreign-slot-value ss '(:struct %pa-sample-spec) 'channels) channels)
                   (setf (cffi:foreign-slot-value attr '(:struct %pa-buffer-attr) 'maxlength) #xffffffff
                         (cffi:foreign-slot-value attr '(:struct %pa-buffer-attr) 'tlength) (* period periods bytes-per-frame)
                         (cffi:foreign-slot-value attr '(:struct %pa-buffer-attr) 'prebuf) #xffffffff
                         (cffi:foreign-slot-value attr '(:struct %pa-buffer-attr) 'minreq) (* period bytes-per-frame)
                         (cffi:foreign-slot-value attr '(:struct %pa-buffer-attr) 'fragsize) #xffffffff)
                   (cffi:foreign-funcall "pa_simple_new"
                                         :pointer (cffi:null-pointer) :string "raylib"
                                         :int +pa-stream-playback+ :pointer (cffi:null-pointer)
                                         :string "Playback" :pointer ss :pointer (cffi:null-pointer)
                                         :pointer attr :pointer err :pointer))))
    (if (cffi:null-pointer-p handle)
        nil
        (%make-ma-device :format format :channels channels :sample-rate sample-rate
                         :internal-format format :internal-channels channels :internal-sample-rate sample-rate
                         :internal-period-size-in-frames period :internal-periods periods
                         :data-callback data-callback :handle handle))))

;; ma_device__handle_data_callback(): data callback, master volume and clipping
(defun %ma-device-handle-data-callback (device frames-out frame-count)
  (declare (type (simple-array single-float (*)) frames-out))
  (let ((master-volume-factor (ma-device-master-volume device))
        (sample-count (* frame-count (ma-device-channels device))))
    (when (ma-device-data-callback device)
      ;; NOTE: Errors on the audio thread (i.e. from user processors) are reported once and output silence
      (handler-case (funcall (ma-device-data-callback device) device frames-out frame-count)
        (error (e)
          (fill frames-out 0f0)
          (unless (ma-device-callback-error device)
            (setf (ma-device-callback-error device) t)
            (trace-log +log-warning+ "miniaudio: Error on audio data callback: ~a" e)))))
    (when (/= master-volume-factor 1f0)
      (dotimes (i sample-count)
        (setf (aref frames-out i) (* (aref frames-out i) master-volume-factor))))
    (unless (ma-device-no-clip device)
      (dotimes (i sample-count)
        (setf (aref frames-out i) (%ma-clip-f32 (aref frames-out i)))))))

(defun %ma-device-thread (device)
  (let* ((frame-count (ma-device-internal-period-size-in-frames device))
         (frames (make-array (* frame-count (ma-device-channels device)) :element-type 'single-float
                                                                         :initial-element 0f0))
         (bytes (* frame-count (ma-get-bytes-per-frame (ma-device-format device) (ma-device-channels device)))))
    (float-features:with-float-traps-masked t
      (cffi:with-foreign-object (err :int)
        (loop while (ma-device-running device)
              do (%ma-device-handle-data-callback device frames frame-count)
                 (when (< (cffi:with-pointer-to-vector-data (ptr frames)
                            (cffi:foreign-funcall "pa_simple_write" :pointer (ma-device-handle device)
                                                  :pointer ptr :size bytes :pointer err :int))
                          0)
                   (trace-log +log-warning+ "miniaudio: Failed to write to playback device")
                   (return)))))))

(defun ma-device-start (device)
  (setf (ma-device-running device) t
        (ma-device-thread device) (bt:make-thread (lambda () (%ma-device-thread device))
                                                  :name "raylib audio device"))
  t)

(defun ma-device-uninit (device)
  "Stop the device (joins the playback thread) and release it"
  (when (ma-device-thread device)
    (setf (ma-device-running device) nil)
    (bt:join-thread (ma-device-thread device))
    (setf (ma-device-thread device) nil))
  (unless (cffi:null-pointer-p (ma-device-handle device))
    (cffi:foreign-funcall "pa_simple_free" :pointer (ma-device-handle device) :void)
    (setf (ma-device-handle device) (cffi:null-pointer))))

(defun ma-device-set-master-volume (device volume)
  (when (and device (>= volume 0))
    (setf (ma-device-master-volume device) (coerce volume 'single-float))
    t))

(defun ma-device-get-master-volume (device)
  (if device (ma-device-master-volume device) 0f0))
