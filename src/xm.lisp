(in-package #:cl-raylib)

;;;===================================================================================
;;; jar_xm - XM module player
;;; Port of raylib/src/external/jar_xm.h (based on libxm), used by raudio
;;;
;;; NOTE: sinf() and powf() are called from libm to match the C single precision results
;;; NOTE: Sample reads out of the sample data range return 0 (C reads the memory pool)
;;; NOTE: The RayLib visualizer extension (JAR_XM_RAYLIB) is not ported
;;;===================================================================================

(defconstant +xm-num-notes+ 96)
(defconstant +xm-num-envelope-points+ 12)
(defconstant +xm-max-num-rows+ 256)
(defconstant +xm-sample-ramping-points+ 8)
(defconstant +xm-note-off+ 97)

;; jar_xm_waveform_type_t
(defconstant +xm-sine-waveform+ 0)
(defconstant +xm-ramp-down-waveform+ 1)
(defconstant +xm-square-waveform+ 2)
(defconstant +xm-random-waveform+ 3)
(defconstant +xm-ramp-up-waveform+ 4)

;; jar_xm_loop_type_t
(defconstant +xm-no-loop+ 0)
(defconstant +xm-forward-loop+ 1)
(defconstant +xm-ping-pong-loop+ 2)

;; jar_xm_frequency_type_t
(defconstant +xm-linear-frequencies+ 0)
(defconstant +xm-amiga-frequencies+ 1)

(defconstant +xm-trigger-keep-volume+ 1)
(defconstant +xm-trigger-keep-period+ 2)
(defconstant +xm-trigger-keep-sample-position+ 4)

(alexandria:define-constant +xm-amiga-frequencies-table+
  #(1712 1616 1525 1440 1357 1281 1209 1141 1077 1017 961 907 856) :test #'equalp)
(alexandria:define-constant +xm-multi-retrig-add+
  (make-array 16 :element-type 'single-float
                 :initial-contents '(0f0 -1f0 -2f0 -4f0 -8f0 -16f0 0f0 0f0 0f0 1f0 2f0 4f0 8f0 16f0 0f0 0f0))
  :test #'equalp)
(alexandria:define-constant +xm-multi-retrig-multiply+
  (make-array 16 :element-type 'single-float
                 :initial-contents '(1f0 1f0 1f0 1f0 1f0 1f0 0.6666667f0 0.5f0 1f0 1f0 1f0 1f0 1f0 1f0 1.5f0 2f0))
  :test #'equalp)

(declaim (inline %xm-sinf %xm-powf %u8 %u16))
(defun %xm-sinf (x) (cffi:foreign-funcall "sinf" :float x :float))
(defun %xm-powf (x y) (cffi:foreign-funcall "powf" :float x :float y :float))
(defun %u8 (x) (logand x #xff))
(defun %u16 (x) (logand x #xffff))

(defstruct (xm-envelope (:conc-name xenv-))
  (frames (make-array +xm-num-envelope-points+ :initial-element 0))
  (values (make-array +xm-num-envelope-points+ :initial-element 0))
  (num-points 0) (sustain-point 0) (loop-start-point 0) (loop-end-point 0)
  (enabled nil) (sustain-enabled nil) (loop-enabled nil))

(defstruct (xm-sample (:conc-name xs-))
  (bits 8) (stereo 0)
  (length 0) (loop-start 0) (loop-length 0) (loop-end 0)
  (volume 0f0 :type single-float)
  (finetune 0)
  (loop-type +xm-no-loop+)
  (panning 0f0 :type single-float)
  (relative-note 0)
  (latest-trigger 0)
  (data nil))

(defstruct (xm-instrument (:conc-name xi-))
  (name "")
  (num-samples 0)
  (sample-of-notes (make-array +xm-num-notes+ :initial-element 0))
  (volume-envelope (make-xm-envelope))
  (panning-envelope (make-xm-envelope))
  (vibrato-type 0) (vibrato-sweep 0) (vibrato-depth 0) (vibrato-rate 0)
  (volume-fadeout 0)
  (latest-trigger 0)
  (muted nil)
  (samples #()))

(defstruct (xm-slot (:conc-name xsl-))
  (note 0) (instrument 0) (volume-column 0) (effect-type 0) (effect-param 0))

(defstruct (xm-pattern (:conc-name xp-))
  (num-rows 0)
  (slots #())                           ; Slots of all the patterns (contiguous like in the C memory pool)
  (base 0))                             ; Index of the first slot of this pattern

(defstruct (xm-channel (:conc-name xc-))
  (note 0f0 :type single-float)
  (orig-note 0f0 :type single-float)
  (instrument nil)
  (sample nil)
  (current nil)
  (sample-position 0f0 :type single-float)
  (period 0f0 :type single-float)
  (frequency 0f0 :type single-float)
  (step 0f0 :type single-float)
  (ping t)
  (volume 1f0 :type single-float)
  (panning 0.5f0 :type single-float)
  (autovibrato-ticks 0)
  (sustained nil)
  (fadeout-volume 1f0 :type single-float)
  (volume-envelope-volume 1f0 :type single-float)
  (panning-envelope-panning 0.5f0 :type single-float)
  (volume-envelope-frame-count 0)
  (panning-envelope-frame-count 0)
  (autovibrato-note-offset 0f0 :type single-float)
  (arp-in-progress nil)
  (arp-note-offset 0)
  (volume-slide-param 0) (fine-volume-slide-param 0) (global-volume-slide-param 0) (panning-slide-param 0)
  (portamento-up-param 0) (portamento-down-param 0) (fine-portamento-up-param 0) (fine-portamento-down-param 0)
  (extra-fine-portamento-up-param 0) (extra-fine-portamento-down-param 0)
  (tone-portamento-param 0)
  (tone-portamento-target-period 0f0 :type single-float)
  (multi-retrig-param 0)
  (note-delay-param 0)
  (pattern-loop-origin 0)
  (pattern-loop-count 0)
  (vibrato-in-progress nil)
  (vibrato-waveform +xm-sine-waveform+)
  (vibrato-waveform-retrigger t)
  (vibrato-param 0)
  (vibrato-ticks 0)
  (vibrato-note-offset 0f0 :type single-float)
  (tremolo-waveform +xm-sine-waveform+)
  (tremolo-waveform-retrigger t)
  (tremolo-param 0)
  (tremolo-ticks 0)
  (tremolo-volume 0f0 :type single-float)
  (tremor-param 0)
  (tremor-on nil)
  (latest-trigger 0)
  (muted nil)
  (target-panning 0f0 :type single-float)
  (target-volume 0f0 :type single-float)
  (frame-count 0)
  (end-of-previous-sample-left (make-array +xm-sample-ramping-points+ :element-type 'single-float :initial-element 0f0))
  (end-of-previous-sample-right (make-array +xm-sample-ramping-points+ :element-type 'single-float :initial-element 0f0))
  (curr-left 0f0 :type single-float)
  (curr-right 0f0 :type single-float)
  (actual-panning 0.5f0 :type single-float)
  (actual-volume 0f0 :type single-float))

(defstruct (jar-xm-context (:conc-name xm-) (:constructor %make-jar-xm-context))
  ;; Module
  (name "") (tracker-name "")
  (length 0) (restart-position 0) (num-channels 0) (num-patterns 0) (num-instruments 0)
  (linear-interpolation 1) (ramping 1)
  (frequency-type +xm-linear-frequencies+)
  (pattern-table (make-array 256 :element-type '(unsigned-byte 8) :initial-element 0))
  (patterns #()) (instruments #())
  ;; Context
  (rate 0)
  (default-tempo 0) (default-bpm 0)
  (default-global-volume 1f0 :type single-float)
  (tempo 0) (bpm 0)
  (global-volume 1f0 :type single-float)
  (volume-ramp (/ 1f0 128f0) :type single-float)
  (panning-ramp (/ 1f0 128f0) :type single-float)
  (current-table-index 0) (current-row 0) (current-tick 0)
  (remaining-samples-in-tick 0f0 :type single-float)
  (generated-samples 0)
  (position-jump nil) (pattern-break nil)
  (jump-dest 0) (jump-row 0)
  (extra-ticks 0)
  (row-loop-count nil)
  (loop-count 0) (max-loop-count 0)
  (channels #()))

;;;----------------------------------------------------------------------------------
;;; Loading
;;;----------------------------------------------------------------------------------

(defun %xm-check-sanity-preload (module module-length)
  (cond ((< module-length 60) 4)
        ((mismatch (map 'vector #'char-code "Extended Module: ") module :end2 17) 1)
        ((/= (aref module 37) #x1a) 2)
        ((or (/= (aref module 59) #x01) (/= (aref module 58) #x04)) 3)   ; Not XM 1.04
        (t 0)))

(defun %xm-check-sanity-postload (ctx)
  ;; Check the POT
  (let ((i 0))
    (loop while (< i (xm-length ctx))
          do (when (>= (aref (xm-pattern-table ctx) i) (xm-num-patterns ctx))
               (if (and (= (+ i 1) (xm-length ctx)) (> (xm-length ctx) 1))
                   (decf (xm-length ctx))   ; Trimming invalid POT
                   (return-from %xm-check-sanity-postload 1)))
             (setf i (%u8 (+ i 1)))
             (when (= i 0) (return))))
  0)

(defun %xm-load-module (ctx moddata moddata-length)
  (macrolet ((read-u8 (offset) `(let ((o ,offset)) (if (< o moddata-length) (aref moddata o) 0))))
    (labels ((read-u16 (o) (logior (read-u8 o) (ash (read-u8 (+ o 1)) 8)))
             (read-u32 (o) (logior (read-u16 o) (ash (read-u16 (+ o 2)) 16)))
             (read-string (o n) (map 'string #'code-char (remove 0 (loop for i below n collect (read-u8 (+ o i)))))))
      (let ((offset 0) (slots-vector #()) (slot-base 0))
        ;; Read XM header
        (setf (xm-name ctx) (read-string 17 20)
              (xm-tracker-name ctx) (read-string 38 20))
        (incf offset 60)
        ;; Read module header
        (let ((header-size (read-u32 offset)))
          (setf (xm-length ctx) (read-u16 (+ offset 4))
                (xm-restart-position ctx) (read-u16 (+ offset 6))
                (xm-num-channels ctx) (read-u16 (+ offset 8))
                (xm-num-patterns ctx) (read-u16 (+ offset 10))
                (xm-num-instruments ctx) (read-u16 (+ offset 12))
                (xm-linear-interpolation ctx) 1   ; Linear interpolation can be set after loading
                (xm-ramping ctx) 1)               ; Ramping can be set after loading
          (let ((flags (%u16 (read-u32 (+ offset 14)))))
            (setf (xm-frequency-type ctx) (if (logtest flags 1) +xm-linear-frequencies+ +xm-amiga-frequencies+)))
          (setf (xm-default-tempo ctx) (read-u16 (+ offset 16))
                (xm-default-bpm ctx) (read-u16 (+ offset 18))
                (xm-tempo ctx) (xm-default-tempo ctx)
                (xm-bpm ctx) (xm-default-bpm ctx))
          (dotimes (i 256)
            (setf (aref (xm-pattern-table ctx) i) (read-u8 (+ offset 20 i))))
          (incf offset header-size))
        ;; Read patterns
        ;; NOTE: Slots of all patterns are kept in a single vector, rows out of a pattern read the next one like in C
        (setf (xm-patterns ctx) (make-array (xm-num-patterns ctx)))
        (let ((total 0) (o offset))
          (dotimes (i (xm-num-patterns ctx))
            (incf total (* (xm-num-channels ctx) (read-u16 (+ o 5))))
            (incf o (+ (read-u32 o) (read-u16 (+ o 7)))))
          (setf slots-vector (let ((v (make-array total))) (dotimes (k total v) (setf (svref v k) (make-xm-slot))))))
        (dotimes (i (xm-num-patterns ctx))
          (let* ((packed-patterndata-size (read-u16 (+ offset 7)))
                 (num-rows (read-u16 (+ offset 5)))
                 (nslots (* (xm-num-channels ctx) num-rows))
                 (base slot-base)
                 (slots slots-vector)
                 (pat (make-xm-pattern :num-rows num-rows :slots slots :base base)))
            (incf slot-base nslots)
            (setf (svref (xm-patterns ctx) i) pat)
            (incf offset (read-u32 offset))   ; Pattern header length
            (unless (= packed-patterndata-size 0)
              ;; This isn't your typical for loop
              (let ((j 0) (k 0))
                (loop while (< j packed-patterndata-size)
                      do (let ((note (read-u8 (+ offset j)))
                               (slot (if (< (+ base k) (length slots)) (svref slots (+ base k)) (make-xm-slot))))
                           (if (logbitp 7 note)
                               ;; MSB is set, this is a compressed packet
                               (progn
                                 (incf j)
                                 (flet ((field (bit)
                                          (if (logbitp bit note)
                                              (prog1 (read-u8 (+ offset j)) (incf j))
                                              0)))
                                   (setf (xsl-note slot) (field 0)
                                         (xsl-instrument slot) (field 1)
                                         (xsl-volume-column slot) (field 2)
                                         (xsl-effect-type slot) (field 3)
                                         (xsl-effect-param slot) (field 4))))
                               ;; Uncompressed packet
                               (setf (xsl-note slot) note
                                     (xsl-instrument slot) (read-u8 (+ offset j 1))
                                     (xsl-volume-column slot) (read-u8 (+ offset j 2))
                                     (xsl-effect-type slot) (read-u8 (+ offset j 3))
                                     (xsl-effect-param slot) (read-u8 (+ offset j 4))
                                     j (+ j 5)))
                           (setf j (%u16 j))
                           (incf k)))))
            (incf offset packed-patterndata-size)))
        ;; Read instruments
        (setf (xm-instruments ctx) (make-array (xm-num-instruments ctx)))
        (dotimes (i (xm-num-instruments ctx))
          (let ((sample-header-size 0)
                (instr (make-xm-instrument)))
            (setf (svref (xm-instruments ctx) i) instr)
            (setf (xi-name instr) (read-string (+ offset 4) 22)
                  (xi-num-samples instr) (read-u16 (+ offset 27)))
            (when (> (xi-num-samples instr) 0)
              ;; Read extra header properties
              (setf sample-header-size (read-u32 (+ offset 29)))
              (dotimes (n +xm-num-notes+)
                (setf (aref (xi-sample-of-notes instr) n) (read-u8 (+ offset 33 n))))
              (let ((venv (xi-volume-envelope instr))
                    (penv (xi-panning-envelope instr)))
                ;; NOTE: Point counts are clamped to the envelope size
                (setf (xenv-num-points venv) (min (read-u8 (+ offset 225)) +xm-num-envelope-points+)
                      (xenv-num-points penv) (min (read-u8 (+ offset 226)) +xm-num-envelope-points+))
                (dotimes (j (min (xenv-num-points venv) +xm-num-envelope-points+))
                  (setf (aref (xenv-frames venv) j) (read-u16 (+ offset 129 (* 4 j)))
                        (aref (xenv-values venv) j) (read-u16 (+ offset 129 (* 4 j) 2))))
                (dotimes (j (min (xenv-num-points penv) +xm-num-envelope-points+))
                  (setf (aref (xenv-frames penv) j) (read-u16 (+ offset 177 (* 4 j)))
                        (aref (xenv-values penv) j) (read-u16 (+ offset 177 (* 4 j) 2))))
                (setf (xenv-sustain-point venv) (read-u8 (+ offset 227))
                      (xenv-loop-start-point venv) (read-u8 (+ offset 228))
                      (xenv-loop-end-point venv) (read-u8 (+ offset 229))
                      (xenv-sustain-point penv) (read-u8 (+ offset 230))
                      (xenv-loop-start-point penv) (read-u8 (+ offset 231))
                      (xenv-loop-end-point penv) (read-u8 (+ offset 232)))
                (let ((flags (read-u8 (+ offset 233))))
                  (setf (xenv-enabled venv) (logbitp 0 flags)
                        (xenv-sustain-enabled venv) (logbitp 1 flags)
                        (xenv-loop-enabled venv) (logbitp 2 flags)))
                (let ((flags (read-u8 (+ offset 234))))
                  (setf (xenv-enabled penv) (logbitp 0 flags)
                        (xenv-sustain-enabled penv) (logbitp 1 flags)
                        (xenv-loop-enabled penv) (logbitp 2 flags))))
              (setf (xi-vibrato-type instr) (case (read-u8 (+ offset 235)) (2 1) (1 2) (t (read-u8 (+ offset 235))))
                    (xi-vibrato-sweep instr) (read-u8 (+ offset 236))
                    (xi-vibrato-depth instr) (read-u8 (+ offset 237))
                    (xi-vibrato-rate instr) (read-u8 (+ offset 238))
                    (xi-volume-fadeout instr) (read-u16 (+ offset 239))
                    (xi-samples instr) (let ((v (make-array (xi-num-samples instr))))
                                         (dotimes (j (length v) v) (setf (svref v j) (make-xm-sample))))))
            ;; Instrument header size
            (incf offset (read-u32 offset))
            (dotimes (j (xi-num-samples instr))
              ;; Read sample header
              (let ((sample (svref (xi-samples instr) j)))
                (setf (xs-length sample) (read-u32 offset)
                      (xs-loop-start sample) (read-u32 (+ offset 4))
                      (xs-loop-length sample) (read-u32 (+ offset 8))
                      (xs-loop-end sample) (logand (+ (xs-loop-start sample) (xs-loop-length sample)) #xffffffff)
                      (xs-volume sample) (min 1f0 (/ (float (ash (read-u8 (+ offset 12)) 2) 1f0) 256f0))
                      (xs-finetune sample) (let ((v (read-u8 (+ offset 13)))) (if (>= v 128) (- v 256) v)))
                (let ((flags (read-u8 (+ offset 14))))
                  ;; NOTE: Missing break in jar_xm, ping-pong loops are loaded as forward loops
                  (setf (xs-loop-type sample) (if (= (logand flags 3) 0) +xm-no-loop+ +xm-forward-loop+)
                        (xs-bits sample) (if (logtest flags #x10) 16 8)
                        (xs-stereo sample) (if (logtest flags #x20) 1 0)))
                (setf (xs-panning sample) (/ (float (read-u8 (+ offset 15)) 1f0) 255f0)
                      (xs-relative-note sample) (let ((v (read-u8 (+ offset 16)))) (if (>= v 128) (- v 256) v)))
                (when (= (xs-bits sample) 16)
                  (setf (xs-loop-start sample) (ash (xs-loop-start sample) -1)
                        (xs-loop-length sample) (ash (xs-loop-length sample) -1)
                        (xs-loop-end sample) (ash (xs-loop-end sample) -1)
                        (xs-length sample) (ash (xs-length sample) -1)))
                (when (and (/= (xs-stereo sample) 0) (/= (xs-loop-type sample) +xm-no-loop+))
                  (setf (xs-loop-start sample) (floor (read-u32 (+ offset 4)) 2)
                        (xs-loop-length sample) (floor (read-u32 (+ offset 8)) 2)
                        (xs-loop-end sample) (+ (xs-loop-start sample) (xs-loop-length sample))))
                (incf offset sample-header-size)))
            (dotimes (j (xi-num-samples instr))
              ;; Read sample data
              (let* ((sample (svref (xi-samples instr) j))
                     (length (xs-length sample))
                     (data (make-array length :element-type 'single-float :initial-element 0f0))
                     (half (floor (xs-length sample) 2))
                     (v 0))
                (setf (xs-data sample) data)
                (dotimes (k length)
                  (when (and (/= (xs-stereo sample) 0) (= k half)) (setf v 0))
                  (if (= (xs-bits sample) 16)
                      (setf v (%i16 (+ v (%i16 (read-u16 (+ offset (ash k 1))))))
                            (aref data k) (/ (float v 1f0) 32768f0))
                      (setf v (let ((x (logand (+ v (read-u8 (+ offset k))) #xff))) (if (>= x 128) (- x 256) x))
                            (aref data k) (/ (float v 1f0) 128f0)))
                  (setf (aref data k) (max -1f0 (min 1f0 (aref data k)))))
                (incf offset (if (= (xs-bits sample) 16) (ash (xs-length sample) 1) (xs-length sample)))
                (when (/= (xs-stereo sample) 0)
                  (setf (xs-length sample) half))))))))))

(defun jar-xm-create-context-safe (moddata moddata-length rate)
  "Create an XM player context, returns (values context error)"
  (let ((ret (%xm-check-sanity-preload moddata moddata-length)))
    (unless (= ret 0)
      (return-from jar-xm-create-context-safe (values nil 1))))
  (let ((ctx (%make-jar-xm-context :rate rate)))
    (%xm-load-module ctx moddata moddata-length)
    (setf (xm-channels ctx) (let ((v (make-array (xm-num-channels ctx))))
                              (dotimes (i (length v) v) (setf (svref v i) (make-xm-channel))))
          (xm-default-global-volume ctx) 1f0
          (xm-global-volume ctx) 1f0
          (xm-volume-ramp ctx) (/ 1f0 128f0)
          (xm-panning-ramp ctx) (/ 1f0 128f0)
          (xm-row-loop-count ctx) (make-array (* +xm-max-num-rows+ (max 1 (xm-length ctx)))
                                              :element-type '(unsigned-byte 8) :initial-element 0))
    (unless (= (%xm-check-sanity-postload ctx) 0)
      (return-from jar-xm-create-context-safe (values nil 1)))
    (values ctx 0)))

(defun jar-xm-create-context-from-file (rate file-name)
  (multiple-value-bind (data size) (load-file-data file-name)
    (if data
        (jar-xm-create-context-safe data size rate)
        (values nil 3))))

(defun jar-xm-free-context (ctx)
  (declare (ignore ctx))
  nil)

(defun jar-xm-set-max-loop-count (ctx loopcnt)
  (setf (xm-max-loop-count ctx) loopcnt))

(defun jar-xm-get-loop-count (ctx)
  (xm-loop-count ctx))

(defun jar-xm-get-position (ctx)
  "Returns (values pattern-index pattern row samples)"
  (values (xm-current-table-index ctx)
          (aref (xm-pattern-table ctx) (xm-current-table-index ctx))
          (xm-current-row ctx)
          (xm-generated-samples ctx)))

;;;----------------------------------------------------------------------------------
;;; Effects and frequencies
;;;----------------------------------------------------------------------------------

(defvar *xm-next-rand* 24492)

(defun %xm-waveform (waveform step)
  (setf step (mod (%u8 step) #x40))
  (case waveform
    (#.+xm-sine-waveform+ (- (%xm-sinf (/ (* (* 2f0 3.141592f0) (float step 1f0)) (float #x40 1f0)))))
    (#.+xm-ramp-down-waveform+ (/ (float (- #x20 step) 1f0) (float #x20 1f0)))
    (#.+xm-square-waveform+ (if (>= step #x20) 1f0 -1f0))
    (#.+xm-random-waveform+
     ;; Use the POSIX.1-2001 example, just to be deterministic across different machines
     (setf *xm-next-rand* (logand (+ (* *xm-next-rand* 1103515245) 12345) #xffffffff))
     (- (/ (float (logand (ash *xm-next-rand* -16) #x7fff) 1f0) (float #x4000 1f0)) 1f0))
    (#.+xm-ramp-up-waveform+ (/ (float (- step #x20) 1f0) (float #x20 1f0)))
    (t 0f0)))

(defun %xm-lerp (u v tt)
  (+ u (* tt (- v u))))

(defun %xm-linear-period (note)
  (- 7680f0 (* note 64f0)))

(defun %xm-linear-frequency (period)
  (* 8363f0 (%xm-powf 2f0 (/ (- 4608f0 period) 768f0))))

(defun %xm-amiga-period (note)
  ;; NOTE: unsigned int intnote = note; negative notes wrap like on x86-64 (cvttss2si to 64 bit)
  (let* ((intnote (logand (truncate note) #xffffffff))
         (a (mod intnote 12))
         (octave (%i8 (truncate (- (/ note 12f0) 2f0))))
         (p1 (aref +xm-amiga-frequencies-table+ a))
         (p2 (aref +xm-amiga-frequencies-table+ (+ a 1))))
    (cond ((> octave 0) (setf p1 (ash p1 (- octave)) p2 (ash p2 (- octave))))
          ((< octave 0) (setf p1 (%u16 (ash p1 (- octave))) p2 (%u16 (ash p2 (- octave))))))
    (%xm-lerp (float p1 1f0) (float p2 1f0) (- note (float intnote 1f0)))))

(defun %i8 (x) (- (logand (+ x 128) #xff) 128))

(defun %xm-amiga-frequency (period)
  (if (= period 0f0)
      0f0
      (/ 7093789.2f0 (* period 2f0))))   ; This is the PAL value

(defun %xm-period (ctx note)
  (if (= (xm-frequency-type ctx) +xm-linear-frequencies+)
      (%xm-linear-period note)
      (%xm-amiga-period note)))

(defun %xm-frequency (ctx period note-offset)
  (if (= (xm-frequency-type ctx) +xm-linear-frequencies+)
      (%xm-linear-frequency (- period (* 64f0 note-offset)))
      (if (= note-offset 0)
          (%xm-amiga-frequency period)
          (let ((octave 0) (a 0) (p1 0) (p2 0)
                (table +xm-amiga-frequencies-table+))
            ;; Find the octave of the current period
            (cond ((> period (aref table 0))
                   (decf octave)
                   (loop while (> period (ash (aref table 0) (- octave))) do (decf octave)))
                  ((< period (aref table 12))
                   (incf octave)
                   (loop while (< period (ash (aref table 12) (- octave))) do (incf octave))))
            ;; Find the smallest note closest to the current period
            (dotimes (i 12)
              (setf p1 (aref table i) p2 (aref table (+ i 1)))
              (cond ((> octave 0) (setf p1 (ash p1 (- octave)) p2 (ash p2 (- octave))))
                    ((< octave 0) (setf p1 (%u16 (ash p1 (- octave))) p2 (%u16 (ash p2 (- octave))))))
              (when (and (<= p2 period) (<= period p1))
                (setf a i)
                (return)))
            (let ((note (+ (+ (* 12f0 (float (+ octave 2) 1f0)) (float a 1f0))
                           (/ (- period (float p1 1f0)) (float (- p2 p1) 1f0)))))
              (%xm-amiga-frequency (%xm-amiga-period (+ note note-offset))))))))

(defun %xm-update-frequency (ctx ch)
  (setf (xc-frequency ch) (%xm-frequency ctx (xc-period ch)
                                         (if (> (xc-arp-note-offset ch) 0)
                                             (float (xc-arp-note-offset ch) 1f0)
                                             (+ (xc-vibrato-note-offset ch) (xc-autovibrato-note-offset ch))))
        (xc-step ch) (/ (xc-frequency ch) (float (xm-rate ctx) 1f0))))

(defun %xm-autovibrato (ctx ch)
  (let ((instr (xc-instrument ch)))
    (when (or (null instr) (= (xi-vibrato-depth instr) 0))
      (return-from %xm-autovibrato))
    (let ((sweep 1f0))
      (when (< (xc-autovibrato-ticks ch) (xi-vibrato-sweep instr))
        (setf sweep (%xm-lerp 0f0 1f0 (/ (float (xc-autovibrato-ticks ch) 1f0) (float (xi-vibrato-sweep instr) 1f0)))))
      (let ((step (ash (* (xc-autovibrato-ticks ch) (xi-vibrato-rate instr)) -2)))
        (setf (xc-autovibrato-ticks ch) (%u16 (+ (xc-autovibrato-ticks ch) 1)))
        (setf (xc-autovibrato-note-offset ch)
              (* (/ (* (* 0.25f0 (%xm-waveform (xi-vibrato-type instr) step)) (float (xi-vibrato-depth instr) 1f0))
                    (float #xf 1f0))
                 sweep))
        (%xm-update-frequency ctx ch)))))

(defun %xm-vibrato (ctx ch param pos)
  (let ((step (* pos (ash param -4))))
    (setf (xc-vibrato-note-offset ch)
          (/ (* (* 2f0 (%xm-waveform (xc-vibrato-waveform ch) step)) (float (logand param #x0f) 1f0)) (float #xf 1f0)))
    (%xm-update-frequency ctx ch)))

(defun %xm-tremolo (ch param pos)
  (let ((step (* pos (ash param -4))))
    (setf (xc-tremolo-volume ch)
          (/ (* (* -1f0 (%xm-waveform (xc-tremolo-waveform ch) step)) (float (logand param #x0f) 1f0)) (float #xf 1f0)))))

(defun %xm-arpeggio (ctx ch param tick)
  (case (mod tick 3)
    (0 (setf (xc-arp-in-progress ch) nil (xc-arp-note-offset ch) 0))
    (2 (setf (xc-arp-in-progress ch) t (xc-arp-note-offset ch) (ash param -4)))
    (1 (setf (xc-arp-in-progress ch) t (xc-arp-note-offset ch) (logand param #x0f))))
  (%xm-update-frequency ctx ch))

(defmacro %xm-slide-towards (val goal incr)
  `(let ((g ,goal) (i ,incr))
     (cond ((> ,val g) (setf ,val (- ,val i)) (when (< ,val g) (setf ,val g)))
           ((< ,val g) (setf ,val (+ ,val i)) (when (> ,val g) (setf ,val g))))))

(defun %xm-tone-portamento (ctx ch)
  ;; 3xx called without a note, wait until we get an actual target note
  (when (= (xc-tone-portamento-target-period ch) 0f0) (return-from %xm-tone-portamento))
  (when (/= (xc-period ch) (xc-tone-portamento-target-period ch))
    (%xm-slide-towards (xc-period ch) (xc-tone-portamento-target-period ch)
                       (* (if (= (xm-frequency-type ctx) +xm-linear-frequencies+) 4f0 1f0)
                          (float (xc-tone-portamento-param ch) 1f0)))
    (%xm-update-frequency ctx ch)))

(defun %xm-pitch-slide (ctx ch period-offset)
  (let ((period-offset (float period-offset 1f0)))
    ;; Don't ask about the 4.f coefficient. I found mention of it nowhere. Found by ear
    (when (= (xm-frequency-type ctx) +xm-linear-frequencies+)
      (setf period-offset (* period-offset 4f0)))
    (setf (xc-period ch) (+ (xc-period ch) period-offset))
    (when (< (xc-period ch) 0f0) (setf (xc-period ch) 0f0))
    (%xm-update-frequency ctx ch)))

(defun %xm-panning-slide (ch rawval)
  (when (logtest rawval #xf0)
    (setf (xc-panning ch) (+ (xc-panning ch) (/ (float (ash (logand rawval #xf0) -4) 1f0) (float #xff 1f0)))))
  (when (logtest rawval #x0f)
    (setf (xc-panning ch) (- (xc-panning ch) (/ (float (logand rawval #x0f) 1f0) (float #xff 1f0))))))

(defun %xm-volume-slide (ch rawval)
  (when (logtest rawval #xf0)
    (setf (xc-volume ch) (+ (xc-volume ch) (/ (float (ash (logand rawval #xf0) -4) 1f0) (float #x40 1f0)))))
  (when (logtest rawval #x0f)
    (setf (xc-volume ch) (- (xc-volume ch) (/ (float (logand rawval #x0f) 1f0) (float #x40 1f0))))))

(defun %xm-envelope-lerp (env a b pos)
  ;; Linear interpolation between two envelope points
  (let ((fa (aref (xenv-frames env) a)) (va (aref (xenv-values env) a))
        (fb (aref (xenv-frames env) b)) (vb (aref (xenv-values env) b)))
    (cond ((<= pos fa) (float va 1f0))
          ((>= pos fb) (float vb 1f0))
          (t (let ((p (/ (float (- pos fa) 1f0) (float (- fb fa) 1f0))))
               (+ (* (float va 1f0) (- 1f0 p)) (* (float vb 1f0) p)))))))

(defun %xm-post-pattern-change (ctx)
  ;; Loop if necessary
  (when (>= (xm-current-table-index ctx) (xm-length ctx))
    (setf (xm-current-table-index ctx) (%u8 (xm-restart-position ctx))
          (xm-tempo ctx) (xm-default-tempo ctx)
          (xm-bpm ctx) (xm-default-bpm ctx)
          (xm-global-volume ctx) (xm-default-global-volume ctx))))

(defun %xm-has-tone-portamento (s)
  (or (= (xsl-effect-type s) 3) (= (xsl-effect-type s) 5) (= (ash (xsl-volume-column s) -4) #xf)))

(defun %xm-has-arpeggio (s)
  (and (= (xsl-effect-type s) 0) (/= (xsl-effect-param s) 0)))

(defun %xm-has-vibrato (s)
  (or (= (xsl-effect-type s) 4) (= (xsl-effect-param s) 6) (= (ash (xsl-volume-column s) -4) #xb)))

(defun %xm-note-is-valid (n)
  (and (> n 0) (< n 97)))

(defun %xm-sample-note (s sample)
  "s->note + sample->relative_note + sample->finetune / 128.f - 1.f"
  (- (+ (float (+ (xsl-note s) (xs-relative-note sample)) 1f0) (/ (float (xs-finetune sample) 1f0) 128f0)) 1f0))

(defun %xm-cut-note (ch)
  (setf (xc-volume ch) 0f0))   ; NB: this is not the same as Key Off

(defun %xm-key-off (ch)
  (setf (xc-sustained ch) nil)   ; Key Off
  ;; If no volume envelope is used, also cut the note
  (when (or (null (xc-instrument ch)) (not (xenv-enabled (xi-volume-envelope (xc-instrument ch)))))
    (%xm-cut-note ch)))

(defun %xm-trigger-note (ctx ch flags)
  (unless (logtest flags +xm-trigger-keep-sample-position+)
    (setf (xc-sample-position ch) 0f0
          (xc-ping ch) t))
  (unless (logtest flags +xm-trigger-keep-volume+)
    (when (xc-sample ch)
      (setf (xc-volume ch) (xs-volume (xc-sample ch)))))
  (setf (xc-panning ch) (xs-panning (xc-sample ch))
        (xc-sustained ch) t
        (xc-fadeout-volume ch) 1f0
        (xc-volume-envelope-volume ch) 1f0
        (xc-panning-envelope-panning ch) 0.5f0
        (xc-volume-envelope-frame-count ch) 0
        (xc-panning-envelope-frame-count ch) 0
        (xc-vibrato-note-offset ch) 0f0
        (xc-tremolo-volume ch) 0f0
        (xc-tremor-on ch) nil
        (xc-autovibrato-ticks ch) 0)
  (when (xc-vibrato-waveform-retrigger ch) (setf (xc-vibrato-ticks ch) 0))
  (when (xc-tremolo-waveform-retrigger ch) (setf (xc-tremolo-ticks ch) 0))
  (unless (logtest flags +xm-trigger-keep-period+)
    (setf (xc-period ch) (%xm-period ctx (xc-note ch)))
    (%xm-update-frequency ctx ch))
  (setf (xc-latest-trigger ch) (xm-generated-samples ctx))
  (when (xc-instrument ch) (setf (xi-latest-trigger (xc-instrument ch)) (xm-generated-samples ctx)))
  (when (xc-sample ch) (setf (xs-latest-trigger (xc-sample ch)) (xm-generated-samples ctx))))

(defun %xm-handle-note-and-instrument (ctx ch s)
  (when (> (xsl-instrument s) 0)
    (cond ((and (%xm-has-tone-portamento (xc-current ch)) (xc-instrument ch) (xc-sample ch))
           ;; Tone portamento in effect
           (%xm-trigger-note ctx ch (logior +xm-trigger-keep-period+ +xm-trigger-keep-sample-position+)))
          ((> (xsl-instrument s) (xm-num-instruments ctx))
           ;; Invalid instrument, Cut current note
           (%xm-cut-note ch)
           (setf (xc-instrument ch) nil
                 (xc-sample ch) nil))
          (t
           (setf (xc-instrument ch) (svref (xm-instruments ctx) (- (xsl-instrument s) 1)))
           (when (and (= (xsl-note s) 0) (xc-sample ch))
             ;; Ghost instrument, trigger note: sample position is kept, but envelopes are reset
             (%xm-trigger-note ctx ch +xm-trigger-keep-sample-position+)))))
  (cond
    ((%xm-note-is-valid (xsl-note s))
     (let ((instr (xc-instrument ch)))
       (cond ((and (%xm-has-tone-portamento (xc-current ch)) instr (xc-sample ch))
              ;; Tone portamento in effect
              (setf (xc-note ch) (%xm-sample-note s (xc-sample ch))
                    (xc-tone-portamento-target-period ch) (%xm-period ctx (xc-note ch))))
             ((or (null instr) (= (xi-num-samples (xc-instrument ch)) 0))
              ;; Issue on instrument
              (%xm-cut-note ch))
             ((< (aref (xi-sample-of-notes instr) (- (xsl-note s) 1)) (xi-num-samples instr))
              (when (/= (xm-ramping ctx) 0)
                (dotimes (i +xm-sample-ramping-points+)
                  (%xm-next-of-sample ctx ch i))
                (setf (xc-frame-count ch) 0))
              (setf (xc-sample ch) (svref (xi-samples instr) (aref (xi-sample-of-notes instr) (- (xsl-note s) 1))))
              (setf (xc-note ch) (%xm-sample-note s (xc-sample ch))
                    (xc-orig-note ch) (xc-note ch))
              (if (> (xsl-instrument s) 0)
                  (%xm-trigger-note ctx ch 0)
                  ;; Ghost note: keep old volume
                  (%xm-trigger-note ctx ch +xm-trigger-keep-volume+)))
             (t (%xm-cut-note ch)))))
    ((= (xsl-note s) +xm-note-off+)
     (%xm-key-off ch)))
  (let ((param (xsl-effect-param s)))
    (case (xsl-effect-type s)
      (1 (when (> param 0) (setf (xc-portamento-up-param ch) param)))     ; 1xx: Portamento up
      (2 (when (> param 0) (setf (xc-portamento-down-param ch) param)))   ; 2xx: Portamento down
      (3 (when (> param 0) (setf (xc-tone-portamento-param ch) param)))   ; 3xx: Tone portamento
      (4                                                                  ; 4xy: Vibrato
       (when (logtest param #x0f)   ; Set vibrato depth
         (setf (xc-vibrato-param ch) (logior (logand (xc-vibrato-param ch) #xf0) (logand param #x0f))))
       (when (/= (ash param -4) 0)  ; Set vibrato speed
         (setf (xc-vibrato-param ch) (logior (logand param #xf0) (logand (xc-vibrato-param ch) #x0f)))))
      (5 (when (> param 0) (setf (xc-volume-slide-param ch) param)))      ; 5xy: Tone portamento + Volume slide
      (6 (when (> param 0) (setf (xc-volume-slide-param ch) param)))      ; 6xy: Vibrato + Volume slide
      (7                                                                  ; 7xy: Tremolo
       (when (logtest param #x0f)
         (setf (xc-tremolo-param ch) (logior (logand (xc-tremolo-param ch) #xf0) (logand param #x0f))))
       (when (/= (ash param -4) 0)
         (setf (xc-tremolo-param ch) (logior (logand param #xf0) (logand (xc-tremolo-param ch) #x0f)))))
      (8 (setf (xc-panning ch) (/ (float param 1f0) 255f0)))              ; 8xx: Set panning
      (9                                                                  ; 9xx: Sample offset
       (let ((sample (xc-sample ch)))
         (when sample
           (let ((final-offset (ash param (if (= (xs-bits sample) 16) 7 8))))
             (case (xs-loop-type sample)
               (#.+xm-no-loop+
                (if (>= final-offset (xs-length sample))
                    (setf (xc-sample-position ch) -1f0)   ; Pretend the sample dosen't loop and is done playing
                    (setf (xc-sample-position ch) (float final-offset 1f0))))
               (#.+xm-forward-loop+
                (cond ((>= final-offset (xs-loop-end sample))
                       (setf (xc-sample-position ch) (- (xc-sample-position ch) (float (xs-loop-length sample) 1f0))))
                      ((>= final-offset (xs-length sample))
                       (setf (xc-sample-position ch) (float (xs-loop-start sample) 1f0)))
                      (t (setf (xc-sample-position ch) (float final-offset 1f0)))))
               (#.+xm-ping-pong-loop+
                (cond ((>= final-offset (xs-loop-end sample))
                       (setf (xc-ping ch) nil
                             (xc-sample-position ch) (- (float (logand (ash (xs-loop-end sample) 1) #xffffffff) 1f0)
                                                        (xc-sample-position ch))))
                      ((>= final-offset (xs-length sample))
                       (setf (xc-ping ch) nil
                             (xc-sample-position ch) (- (xc-sample-position ch)
                                                        (float (logand (- (xs-length sample) 1) #xffffffff) 1f0))))
                      (t (setf (xc-sample-position ch) (float final-offset 1f0))))))))))
      (#xa (when (> param 0) (setf (xc-volume-slide-param ch) param)))    ; Axy: Volume slide
      (#xb                                                                ; Bxx: Position jump
       (when (< param (xm-length ctx))
         (setf (xm-position-jump ctx) t
               (xm-jump-dest ctx) param)))
      (#xc (setf (xc-volume ch) (/ (float (min param #x40) 1f0) (float #x40 1f0))))   ; Cxx: Set volume
      (#xd                                                                ; Dxx: Pattern break
       ;; Jump after playing this line
       (setf (xm-pattern-break ctx) t
             (xm-jump-row ctx) (%u8 (+ (* (ash param -4) 10) (logand param #x0f)))))
      (#xe                                                                ; EXy: Extended command
       (case (ash param -4)
         (1 (when (logtest param #x0f) (setf (xc-fine-portamento-up-param ch) (logand param #x0f)))   ; E1y: Fine portamento up
            (%xm-pitch-slide ctx ch (- (xc-fine-portamento-up-param ch))))
         (2 (when (logtest param #x0f) (setf (xc-fine-portamento-down-param ch) (logand param #x0f)))   ; E2y: Fine portamento down
            (%xm-pitch-slide ctx ch (xc-fine-portamento-down-param ch)))
         (4 (setf (xc-vibrato-waveform ch) (logand param 3)                 ; E4y: Set vibrato control
                  (xc-vibrato-waveform-retrigger ch) (not (logbitp 2 param))))
         (5                                                                 ; E5y: Set finetune
          (when (and (%xm-note-is-valid (xsl-note (xc-current ch))) (xc-sample ch))
            (setf (xc-note ch) (- (+ (float (+ (xsl-note (xc-current ch)) (xs-relative-note (xc-sample ch))) 1f0)
                                     (/ (float (ash (- (logand param #x0f) 8) 4) 1f0) 128f0))
                                  1f0)
                  (xc-period ch) (%xm-period ctx (xc-note ch)))
            (%xm-update-frequency ctx ch)))
         (6                                                                 ; E6y: Pattern loop
          (if (logtest param #x0f)
              (if (= (logand param #x0f) (xc-pattern-loop-count ch))
                  ;; Loop is over
                  (setf (xc-pattern-loop-count ch) 0
                        (xm-position-jump ctx) nil)
                  ;; Jump to the beginning of the loop
                  (setf (xc-pattern-loop-count ch) (%u8 (+ (xc-pattern-loop-count ch) 1))
                        (xm-position-jump ctx) t
                        (xm-jump-row ctx) (xc-pattern-loop-origin ch)
                        (xm-jump-dest ctx) (xm-current-table-index ctx)))
              (setf (xc-pattern-loop-origin ch) (xm-current-row ctx)   ; Set loop start point
                    (xm-jump-row ctx) (xc-pattern-loop-origin ch))))  ; Replicate FT2 E60 bug
         (7 (setf (xc-tremolo-waveform ch) (logand param 3)                 ; E7y: Set tremolo control
                  (xc-tremolo-waveform-retrigger ch) (not (logbitp 2 param))))
         (#xa (when (logtest param #x0f) (setf (xc-fine-volume-slide-param ch) (logand param #x0f)))   ; EAy: Fine volume slide up
          (%xm-volume-slide ch (%u8 (ash (xc-fine-volume-slide-param ch) 4))))
         (#xb (when (logtest param #x0f) (setf (xc-fine-volume-slide-param ch) (logand param #x0f)))   ; EBy: Fine volume slide down
          (%xm-volume-slide ch (xc-fine-volume-slide-param ch)))
         (#xd                                                               ; EDy: Note delay
          (when (and (= (xsl-note s) 0) (= (xsl-instrument s) 0))
            (let ((flags +xm-trigger-keep-volume+))
              (if (logtest (xsl-effect-param (xc-current ch)) #x0f)
                  (progn (setf (xc-note ch) (xc-orig-note ch))
                         (%xm-trigger-note ctx ch flags))
                  (%xm-trigger-note ctx ch (logior flags +xm-trigger-keep-period+ +xm-trigger-keep-sample-position+))))))
         (#xe                                                               ; EEy: Pattern delay
          (setf (xm-extra-ticks ctx) (%u16 (* (logand (xsl-effect-param (xc-current ch)) #x0f) (xm-tempo ctx)))))))
      (#xf                                                                ; Fxx: Set tempo/BPM
       (when (> param 0)
         (if (<= param #x1f)
             (setf (xm-tempo ctx) param)    ; First 32 possible values adjust the ticks (goes into tempo)
             (setf (xm-bpm ctx) param))))   ; 32 and greater values adjust the BPM
      (16 (setf (xm-global-volume ctx) (/ (float (min param #x40) 1f0) (float #x40 1f0))))   ; Gxx: Set global volume
      (17 (when (> param 0) (setf (xc-global-volume-slide-param ch) param)))                  ; Hxy: Global volume slide
      (21 (setf (xc-volume-envelope-frame-count ch) param                                    ; Lxx: Set envelope position
                (xc-panning-envelope-frame-count ch) param))
      (25 (when (> param 0) (setf (xc-panning-slide-param ch) param)))                        ; Pxy: Panning slide
      (27                                                                                     ; Rxy: Multi retrig note
       (when (> param 0)
         (if (= (ash param -4) 0)
             (setf (xc-multi-retrig-param ch) (logior (logand (xc-multi-retrig-param ch) #xf0) (logand param #x0f)))
             (setf (xc-multi-retrig-param ch) param))))
      (29 (when (> param 0) (setf (xc-tremor-param ch) param)))                               ; Txy: Tremor
      (33                                                                                     ; Xxy: Extra stuff
       (case (ash param -4)
         (1 (when (logtest param #x0f) (setf (xc-extra-fine-portamento-up-param ch) (logand param #x0f)))
            (%xm-pitch-slide ctx ch (* -1f0 (float (xc-extra-fine-portamento-up-param ch) 1f0))))
         (2 (when (logtest param #x0f) (setf (xc-extra-fine-portamento-down-param ch) (logand param #x0f)))
            (%xm-pitch-slide ctx ch (xc-extra-fine-portamento-down-param ch))))))))

(defun %xm-row (ctx)
  (cond ((xm-position-jump ctx)
         (setf (xm-current-table-index ctx) (xm-jump-dest ctx)
               (xm-current-row ctx) (xm-jump-row ctx)
               (xm-position-jump ctx) nil
               (xm-pattern-break ctx) nil
               (xm-jump-row ctx) 0)
         (%xm-post-pattern-change ctx))
        ((xm-pattern-break ctx)
         (setf (xm-current-table-index ctx) (%u8 (+ (xm-current-table-index ctx) 1))
               (xm-current-row ctx) (xm-jump-row ctx)
               (xm-pattern-break ctx) nil
               (xm-jump-row ctx) 0)
         (%xm-post-pattern-change ctx)))
  (let ((cur (svref (xm-patterns ctx) (aref (xm-pattern-table ctx) (xm-current-table-index ctx))))
        (in-a-loop nil)
        (nch (xm-num-channels ctx)))
    ;; Read notes information for all channels into temporary pattern slot
    (dotimes (i nch)
      (let* ((index (+ (xp-base cur) (* (xm-current-row ctx) nch) i))
             (s (if (< index (length (xp-slots cur))) (svref (xp-slots cur) index) (make-xm-slot)))
             (ch (svref (xm-channels ctx) i)))
        (setf (xc-current ch) s)
        (if (or (/= (xsl-effect-type s) #xe) (/= (ash (xsl-effect-param s) -4) #xd))
            (%xm-handle-note-and-instrument ctx ch s)
            (setf (xc-note-delay-param ch) (logand (xsl-effect-param s) #x0f)))
        (when (and (not in-a-loop) (> (xc-pattern-loop-count ch) 0))
          (setf in-a-loop t))))
    (unless in-a-loop
      ;; No E6y loop is in effect (or we are in the first pass)
      (let ((index (+ (* +xm-max-num-rows+ (xm-current-table-index ctx)) (xm-current-row ctx)))
            (counts (xm-row-loop-count ctx)))
        (when (< index (length counts))
          (setf (xm-loop-count ctx) (aref counts index)
                (aref counts index) (%u8 (+ (aref counts index) 1))))))
    ;; uint8 warning: can increment from 255 to 0, in which case it is still necessary to go the next pattern
    (setf (xm-current-row ctx) (%u8 (+ (xm-current-row ctx) 1)))
    (when (and (not (xm-position-jump ctx)) (not (xm-pattern-break ctx))
               (or (>= (xm-current-row ctx) (xp-num-rows cur)) (= (xm-current-row ctx) 0)))
      (setf (xm-current-table-index ctx) (%u8 (+ (xm-current-table-index ctx) 1))
            (xm-current-row ctx) (xm-jump-row ctx)   ; This will be 0 most of the time, except when E60 is used
            (xm-jump-row ctx) 0)
      (%xm-post-pattern-change ctx))))

;; Returns the new (values counter outval)
(defun %xm-envelope-tick (ch env counter outval)
  (if (< (xenv-num-points env) 2)
      (when (= (xenv-num-points env) 1)
        (setf outval (min 1f0 (/ (float (aref (xenv-values env) 0) 1f0) (float #x40 1f0)))))
      (progn
        (when (xenv-loop-enabled env)
          (let* ((loop-start (aref (xenv-frames env) (xenv-loop-start-point env)))
                 (loop-end (aref (xenv-frames env) (xenv-loop-end-point env)))
                 (loop-length (%u16 (- loop-end loop-start))))
            (when (>= counter loop-end)
              (setf counter (%u16 (- counter loop-length))))))
        (dotimes (j (- (xenv-num-points env) 1))
          (when (and (<= (aref (xenv-frames env) j) counter) (>= (aref (xenv-frames env) (+ j 1)) counter))
            (setf outval (/ (%xm-envelope-lerp env j (+ j 1) counter) (float #x40 1f0)))
            (return)))
        ;; Make sure it is safe to increment frame count
        (when (or (not (xc-sustained ch)) (not (xenv-sustain-enabled env))
                  (/= counter (aref (xenv-frames env) (xenv-sustain-point env))))
          (setf counter (%u16 (+ counter 1))))))
  (values counter outval))

(defun %xm-envelopes (ch)
  (let ((instr (xc-instrument ch)))
    (when instr
      (when (xenv-enabled (xi-volume-envelope instr))
        (unless (xc-sustained ch)
          (setf (xc-fadeout-volume ch) (- (xc-fadeout-volume ch) (/ (float (xi-volume-fadeout instr) 1f0) 65536f0)))
          (when (< (xc-fadeout-volume ch) 0f0) (setf (xc-fadeout-volume ch) 0f0)))
        (multiple-value-bind (counter outval)
            (%xm-envelope-tick ch (xi-volume-envelope instr) (xc-volume-envelope-frame-count ch) (xc-volume-envelope-volume ch))
          (setf (xc-volume-envelope-frame-count ch) counter
                (xc-volume-envelope-volume ch) outval)))
      (when (xenv-enabled (xi-panning-envelope instr))
        (multiple-value-bind (counter outval)
            (%xm-envelope-tick ch (xi-panning-envelope instr) (xc-panning-envelope-frame-count ch) (xc-panning-envelope-panning ch))
          (setf (xc-panning-envelope-frame-count ch) counter
                (xc-panning-envelope-panning ch) outval))))))

(defun %xm-tick (ctx)
  (when (= (xm-current-tick ctx) 0)
    (%xm-row ctx))   ; We have processed all ticks and we run the row
  (dotimes (i (xm-num-channels ctx))
    (let* ((ch (svref (xm-channels ctx) i))
           (cur (xc-current ch))
           (tick (xm-current-tick ctx)))
      (%xm-envelopes ch)
      (%xm-autovibrato ctx ch)
      (when (and (xc-arp-in-progress ch) (not (%xm-has-arpeggio cur)))
        (setf (xc-arp-in-progress ch) nil
              (xc-arp-note-offset ch) 0)
        (%xm-update-frequency ctx ch))
      (when (and (xc-vibrato-in-progress ch) (not (%xm-has-vibrato cur)))
        (setf (xc-vibrato-in-progress ch) nil
              (xc-vibrato-note-offset ch) 0f0)
        (%xm-update-frequency ctx ch))
      (let ((vc (xsl-volume-column cur)))
        (case (logand vc #xf0)
          ((#x10 #x20 #x30 #x40 #x50)
           ;; NOTE: 0x50 only for volume = 64
           (unless (and (= (logand vc #xf0) #x50) (/= vc #x50))
             (setf (xc-volume ch) (/ (float (- vc 16) 1f0) 64f0))))
          (#x60 (%xm-volume-slide ch (logand vc #x0f)))          ; Volume slide down
          (#x70 (%xm-volume-slide ch (%u8 (ash vc 4))))          ; Volume slide up
          (#x80 (%xm-volume-slide ch (logand vc #x0f)))          ; Fine volume slide down
          (#x90 (%xm-volume-slide ch (%u8 (ash vc 4))))          ; Fine volume slide up
          (#xa0 (setf (xc-vibrato-param ch) (%u8 (logior (logand (xc-vibrato-param ch) #x0f) (ash (logand vc #x0f) 4)))))   ; Set vibrato speed
          (#xb0                                                  ; Vibrato
           (setf (xc-vibrato-in-progress ch) nil)
           (%xm-vibrato ctx ch (xc-vibrato-param ch) (prog1 (xc-vibrato-ticks ch)
                                                       (setf (xc-vibrato-ticks ch) (%u16 (+ (xc-vibrato-ticks ch) 1))))))
          (#xc0 (when (= tick 0) (setf (xc-panning ch) (/ (float (logand vc #x0f) 1f0) 15f0))))   ; Set panning
          (#xd0 (%xm-panning-slide ch (logand vc #x0f)))         ; Panning slide left
          (#xe0 (%xm-panning-slide ch (%u8 (ash vc 4))))         ; Panning slide right
          (#xf0                                                  ; Tone portamento
           (when (and (= tick 0) (logtest vc #x0f))
             (setf (xc-tone-portamento-param ch) (logior (ash (logand vc #x0f) 4) (logand vc #x0f))))
           (%xm-tone-portamento ctx ch))))
      (let ((param (xsl-effect-param cur)))
        (case (xsl-effect-type cur)
          (0                                                     ; 0xy: Arpeggio
           (when (> param 0)
             (let ((arp-offset (mod (xm-tempo ctx) 3)))
               (block arp
                 (when (= arp-offset 2)            ; 0 -> x -> 0 -> y -> x -> ...
                   (when (= tick 1)
                     (setf (xc-arp-in-progress ch) t
                           (xc-arp-note-offset ch) (ash param -4))
                     (%xm-update-frequency ctx ch)
                     (return-from arp)))
                 (when (>= arp-offset 1)           ; 0 -> 0 -> y -> x -> ...
                   (when (= tick 0)
                     (setf (xc-arp-in-progress ch) nil
                           (xc-arp-note-offset ch) 0)
                     (%xm-update-frequency ctx ch)
                     (return-from arp)))
                 ;; 0 -> y -> x -> ...
                 (%xm-arpeggio ctx ch param (%u16 (- tick arp-offset)))))))
          (1 (unless (= tick 0) (%xm-pitch-slide ctx ch (- (xc-portamento-up-param ch)))))      ; 1xx: Portamento up
          (2 (unless (= tick 0) (%xm-pitch-slide ctx ch (xc-portamento-down-param ch))))        ; 2xx: Portamento down
          (3 (unless (= tick 0) (%xm-tone-portamento ctx ch)))                                  ; 3xx: Tone portamento
          (4 (unless (= tick 0)                                                                 ; 4xy: Vibrato
               (setf (xc-vibrato-in-progress ch) t)
               (%xm-vibrato ctx ch (xc-vibrato-param ch) (prog1 (xc-vibrato-ticks ch)
                                                           (setf (xc-vibrato-ticks ch) (%u16 (+ (xc-vibrato-ticks ch) 1)))))))
          (5 (unless (= tick 0)                                                                 ; 5xy: Tone portamento + Volume slide
               (%xm-tone-portamento ctx ch)
               (%xm-volume-slide ch (xc-volume-slide-param ch))))
          (6 (unless (= tick 0)                                                                 ; 6xy: Vibrato + Volume slide
               (setf (xc-vibrato-in-progress ch) t)
               (%xm-vibrato ctx ch (xc-vibrato-param ch) (prog1 (xc-vibrato-ticks ch)
                                                           (setf (xc-vibrato-ticks ch) (%u16 (+ (xc-vibrato-ticks ch) 1)))))
               (%xm-volume-slide ch (xc-volume-slide-param ch))))
          (7 (unless (= tick 0)                                                                 ; 7xy: Tremolo
               (%xm-tremolo ch (xc-tremolo-param ch) (prog1 (xc-tremolo-ticks ch)
                                                       (setf (xc-tremolo-ticks ch) (%u8 (+ (xc-tremolo-ticks ch) 1)))))))
          (#xa (unless (= tick 0) (%xm-volume-slide ch (xc-volume-slide-param ch))))            ; Axy: Volume slide
          (#xe                                                                                  ; EXy: Extended command
           (case (ash param -4)
             (#x9 (when (and (/= tick 0) (logtest param #x0f))                                  ; E9y: Retrigger note
                    (when (= (mod tick (logand param #x0f)) 0)
                      (%xm-trigger-note ctx ch 0)
                      (%xm-envelopes ch))))
             (#xc (when (= (logand param #x0f) tick)                                            ; ECy: Note cut
                    (%xm-cut-note ch)))
             (#xd (when (= (xc-note-delay-param ch) tick)                                       ; EDy: Note delay
                    (%xm-handle-note-and-instrument ctx ch cur)
                    (%xm-envelopes ch)))))
          (17                                                                                   ; Hxy: Global volume slide
           (unless (or (= tick 0)
                       (and (logtest (xc-global-volume-slide-param ch) #xf0) (logtest (xc-global-volume-slide-param ch) #x0f)))
             (if (logtest (xc-global-volume-slide-param ch) #xf0)
                 (let ((f (/ (float (ash (xc-global-volume-slide-param ch) -4) 1f0) (float #x40 1f0))))
                   (setf (xm-global-volume ctx) (+ (xm-global-volume ctx) f))
                   (when (> (xm-global-volume ctx) 1f0) (setf (xm-global-volume ctx) 1f0)))
                 (let ((f (/ (float (logand (xc-global-volume-slide-param ch) #x0f) 1f0) (float #x40 1f0))))
                   (setf (xm-global-volume ctx) (- (xm-global-volume ctx) f))
                   (when (< (xm-global-volume ctx) 0f0) (setf (xm-global-volume ctx) 0f0))))))
          (20 (when (= tick param) (%xm-key-off ch)))                                           ; Kxx: Key off
          (25 (unless (= tick 0) (%xm-panning-slide ch (xc-panning-slide-param ch))))           ; Pxy: Panning slide
          (27                                                                                   ; Rxy: Multi retrig note
           (unless (or (= tick 0) (= (logand (xc-multi-retrig-param ch) #x0f) 0))
             (when (= (mod tick (logand (xc-multi-retrig-param ch) #x0f)) 0)
               (let ((v (+ (* (xc-volume ch) (aref +xm-multi-retrig-multiply+ (ash (xc-multi-retrig-param ch) -4)))
                           (aref +xm-multi-retrig-add+ (ash (xc-multi-retrig-param ch) -4)))))
                 (setf v (max 0f0 (min 1f0 v)))
                 (%xm-trigger-note ctx ch 0)
                 (setf (xc-volume ch) v)))))
          (29                                                                                   ; Txy: Tremor
           (unless (= tick 0)
             (setf (xc-tremor-on ch)
                   (> (mod (- tick 1) (+ (ash (xc-tremor-param ch) -4) (logand (xc-tremor-param ch) #x0f) 2))
                      (ash (xc-tremor-param ch) -4)))))))
      (let ((panning (coerce (+ (xc-panning ch)
                                (* (* (float (- (xc-panning-envelope-panning ch) 0.5f0) 1d0)
                                      (- 0.5d0 (abs (float (- (xc-panning ch) 0.5f0) 1d0))))
                                   2d0))
                             'single-float))
            (volume 0f0))
        (unless (xc-tremor-on ch)
          (setf volume (+ (xc-volume ch) (xc-tremolo-volume ch)))
          (setf volume (max 0f0 (min 1f0 volume)))
          (setf volume (* volume (* (xc-fadeout-volume ch) (xc-volume-envelope-volume ch)))))
        (if (/= (xm-ramping ctx) 0)
            (setf (xc-target-panning ch) panning
                  (xc-target-volume ch) volume)
            (setf (xc-actual-panning ch) panning
                  (xc-actual-volume ch) volume)))))
  (setf (xm-current-tick ctx) (%u16 (+ (xm-current-tick ctx) 1)))   ; Ticks increment within the row
  (when (>= (xm-current-tick ctx) (+ (xm-tempo ctx) (xm-extra-ticks ctx)))
    (setf (xm-current-tick ctx) 0
          (xm-extra-ticks ctx) 0))
  (setf (xm-remaining-samples-in-tick ctx)
        (+ (xm-remaining-samples-in-tick ctx) (/ (float (xm-rate ctx) 1f0) (* (float (xm-bpm ctx) 1f0) 0.4f0)))))

;;;----------------------------------------------------------------------------------
;;; Mixing
;;;----------------------------------------------------------------------------------

(declaim (inline %xm-sref))
(defun %xm-sref (data i)
  (if (and (>= i 0) (< i (length data))) (aref data i) 0f0))

(defun %xm-next-of-sample (ctx ch previous)
  (let ((sample (xc-sample ch))
        (ramping (/= (xm-ramping ctx) 0))
        (left (xc-end-of-previous-sample-left ch))
        (right (xc-end-of-previous-sample-right ch)))
    (flet ((ramp (endval-left endval-right)
             (when (and ramping (< (xc-frame-count ch) +xm-sample-ramping-points+))
               ;; Smoothly transition between old and new sample
               (let ((tt (/ (float (xc-frame-count ch) 1f0) (float +xm-sample-ramping-points+ 1f0)))
                     (fc (xc-frame-count ch)))
                 (if (> previous -1)
                     (setf (aref left previous) (%xm-lerp (aref left fc) endval-left tt)
                           (aref right previous) (%xm-lerp (aref right fc) endval-right tt))
                     (setf (xc-curr-left ch) (%xm-lerp (aref left fc) endval-left tt)
                           (xc-curr-right ch) (%xm-lerp (aref right fc) endval-right tt)))))))
      (when (or (null (xc-instrument ch)) (null sample) (< (xc-sample-position ch) 0))
        (setf (xc-curr-left ch) 0f0
              (xc-curr-right ch) 0f0)
        (ramp (xc-curr-left ch) (xc-curr-right ch))
        (return-from %xm-next-of-sample))
      (when (= (xs-length sample) 0)
        (return-from %xm-next-of-sample))
      (let* ((linear (/= (xm-linear-interpolation ctx) 0))
             (data (xs-data sample))
             (len (xs-length sample))
             (stereo (/= (xs-stereo sample) 0))
             (pos (xc-sample-position ch))
             (ipos (truncate pos))
             (tt 0f0) (b 0)
             (u-left (%xm-sref data ipos))
             (u-right 0f0) (v-left 0f0) (v-right 0f0))
        (when linear
          (setf b (logand (truncate (+ pos 1f0)) #xffffffff)
                tt (- pos (float ipos 1f0))))   ; Cheaper than fmodf(., 1.f)
        (setf u-right (if stereo (%xm-sref data (+ ipos len)) u-left))
        (case (xs-loop-type sample)
          (#.+xm-no-loop+
           (when linear
             (setf v-left (if (< b len) (%xm-sref data b) 0f0)
                   v-right (if stereo (if (< b len) (%xm-sref data (+ b len)) 0f0) v-left)))
           (setf (xc-sample-position ch) (+ (xc-sample-position ch) (xc-step ch)))
           (when (>= (xc-sample-position ch) (float len 1f0))
             (setf (xc-sample-position ch) -1f0)))   ; Stop playing this sample
          (#.+xm-forward-loop+
           (when linear
             (setf v-left (%xm-sref data (if (= b (xs-loop-end sample)) (xs-loop-start sample) b))
                   v-right (if stereo
                               (%xm-sref data (if (= b (xs-loop-end sample)) (+ (xs-loop-start sample) len) (+ b len)))
                               v-left)))
           (setf (xc-sample-position ch) (+ (xc-sample-position ch) (xc-step ch)))
           (when (>= (xc-sample-position ch) (float (xs-loop-end sample) 1f0))
             (setf (xc-sample-position ch) (- (xc-sample-position ch) (float (xs-loop-length sample) 1f0))))
           (when (>= (xc-sample-position ch) (float len 1f0))
             (setf (xc-sample-position ch) (float (xs-loop-start sample) 1f0))))
          (#.+xm-ping-pong-loop+
           (if (xc-ping ch)
               (progn
                 (when linear
                   (setf v-left (if (>= b (xs-loop-end sample)) (%xm-sref data ipos) (%xm-sref data b))
                         v-right (if stereo
                                     (if (>= b (xs-loop-end sample)) (%xm-sref data (+ ipos len)) (%xm-sref data (+ b len)))
                                     v-left)))
                 (setf (xc-sample-position ch) (+ (xc-sample-position ch) (xc-step ch)))
                 (when (>= (xc-sample-position ch) (float (xs-loop-end sample) 1f0))
                   (setf (xc-ping ch) nil
                         (xc-sample-position ch) (- (float (logand (ash (xs-loop-end sample) 1) #xffffffff) 1f0)
                                                    (xc-sample-position ch))))
                 (when (>= (xc-sample-position ch) (float len 1f0))
                   (setf (xc-ping ch) nil
                         (xc-sample-position ch) (- (xc-sample-position ch) (float (logand (- len 1) #xffffffff) 1f0)))))
               (progn
                 (when linear
                   (setf v-left u-left
                         v-right u-right)
                   (let ((back (or (= b 1) (<= (logand (- b 2) #xffffffff) (xs-loop-start sample)))))
                     (setf u-left (if back (%xm-sref data ipos) (%xm-sref data (- b 2)))
                           u-right (if stereo
                                       (if back (%xm-sref data (+ ipos len)) (%xm-sref data (- (+ b len) 2)))
                                       u-left))))
                 (setf (xc-sample-position ch) (- (xc-sample-position ch) (xc-step ch)))
                 (when (<= (xc-sample-position ch) (float (xs-loop-start sample) 1f0))
                   (setf (xc-ping ch) t
                         (xc-sample-position ch) (- (float (logand (ash (xs-loop-start sample) 1) #xffffffff) 1f0)
                                                    (xc-sample-position ch))))
                 (when (<= (xc-sample-position ch) 0f0)
                   (setf (xc-ping ch) t
                         (xc-sample-position ch) 0f0)))))
          (t (setf v-left 0f0 v-right 0f0)))
        (let ((endval-left (if linear (%xm-lerp u-left v-left tt) u-left))
              (endval-right (if linear (%xm-lerp u-right v-right tt) u-right)))
          (ramp endval-left endval-right)
          (if (> previous -1)
              (setf (aref left previous) endval-left
                    (aref right previous) endval-right)
              (setf (xc-curr-left ch) endval-left
                    (xc-curr-right ch) endval-right)))))))

;; Returns (values left right)
(defun %xm-mixdown (ctx)
  (when (<= (xm-remaining-samples-in-tick ctx) 0)
    (%xm-tick ctx))
  (setf (xm-remaining-samples-in-tick ctx) (- (xm-remaining-samples-in-tick ctx) 1f0))
  (let ((left 0f0) (right 0f0))
    (declare (type single-float left right))
    (when (and (> (xm-max-loop-count ctx) 0) (> (xm-loop-count ctx) (xm-max-loop-count ctx)))
      (return-from %xm-mixdown (values left right)))
    (dotimes (i (xm-num-channels ctx))
      (let ((ch (svref (xm-channels ctx) i)))
        (when (and (xc-instrument ch) (xc-sample ch) (>= (xc-sample-position ch) 0))
          (%xm-next-of-sample ctx ch -1)
          (when (and (not (xc-muted ch)) (not (xi-muted (xc-instrument ch))))
            (setf left (+ left (* (* (xc-curr-left ch) (xc-actual-volume ch)) (- 1f0 (xc-actual-panning ch))))
                  right (+ right (* (* (xc-curr-right ch) (xc-actual-volume ch)) (xc-actual-panning ch)))))
          (when (/= (xm-ramping ctx) 0)
            (incf (xc-frame-count ch))
            (%xm-slide-towards (xc-actual-volume ch) (xc-target-volume ch) (xm-volume-ramp ctx))
            (%xm-slide-towards (xc-actual-panning ch) (xc-target-panning ch) (xm-panning-ramp ctx))))))
    (when (/= (xm-global-volume ctx) 1f0)
      (setf left (* left (xm-global-volume ctx))
            right (* right (xm-global-volume ctx))))
    (values (max -1f0 (min 1f0 left)) (max -1f0 (min 1f0 right)))))

(defun jar-xm-generate-samples (ctx output numsamples &optional (start 0))
  "Generate NUMSAMPLES stereo frames as floats into OUTPUT"
  (when (and ctx output)
    (incf (xm-generated-samples ctx) numsamples)
    (dotimes (i numsamples)
      (multiple-value-bind (left right) (%xm-mixdown ctx)
        (setf (aref output (+ start (* 2 i))) left
              (aref output (+ start (* 2 i) 1)) right)))))

(defun jar-xm-get-remaining-samples (ctx)
  (let ((total 0)
        (current-loop-count (jar-xm-get-loop-count ctx)))
    (jar-xm-set-max-loop-count ctx 0)
    (loop while (= (jar-xm-get-loop-count ctx) current-loop-count)
          do ;; NOTE: uint64 total += float is computed in float
             (setf total (truncate (+ (float total 1f0) (xm-remaining-samples-in-tick ctx))))
             (setf (xm-remaining-samples-in-tick ctx) 0f0)
             (%xm-tick ctx))
    (setf (xm-loop-count ctx) current-loop-count)
    total))

(defun jar-xm-reset (ctx)
  (loop for ch across (xm-channels ctx)
        do (%xm-cut-note ch))
  (setf (xm-generated-samples ctx) 0
        (xm-current-row ctx) 0
        (xm-current-table-index ctx) 0
        (xm-current-tick ctx) 0
        (xm-tempo ctx) (xm-default-tempo ctx)
        (xm-bpm ctx) (xm-default-bpm ctx)
        (xm-global-volume ctx) (xm-default-global-volume ctx)))
