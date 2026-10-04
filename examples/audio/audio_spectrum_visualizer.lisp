;;;; raylib [audio] example - spectrum visualizer
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Inspired by Inigo Quilez's https://www.shadertoy.com/
;;;; Resources/specification: https://gist.github.com/soulthreads/2efe50da4be1fb5f7ab60ff14ca434b8
;;;;
;;;; Example created by created by IANN (@meisei4) reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 IANN (@meisei4)
;;;; Common Lisp port of raylib/examples/audio/audio_spectrum_visualizer.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-spectrum-visualizer
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-spectrum-visualizer)

(defconstant +glsl-version+ 330)

(defconstant +mono+ 1)
(defconstant +sample-rate+ 44100)
(defconstant +sample-rate-f+ 44100.0)
(defconstant +fft-window-size+ 1024)
(defconstant +buffer-size+ 512)
(defconstant +per-sample-bit-depth+ 16)
(defconstant +audio-stream-ring-buffer-size+ (* +fft-window-size+ 2))
(defconstant +effective-sample-rate+ (* +sample-rate-f+ 0.5))
(defconstant +window-time+ (/ (float +fft-window-size+ 1d0) (float +effective-sample-rate+ 1d0)))
(defconstant +fft-historical-smoothing-dur+ 2.0)
(defconstant +min-decibels+ -100.0)     ; https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/minDecibels
(defconstant +max-decibels+ -30.0)      ; https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/maxDecibels
(defconstant +inverse-decibel-range+ (/ 1.0 (- +max-decibels+ +min-decibels+)))
(defconstant +db-to-linear-scale+ (/ 20.0 2.302585092994046))
(defconstant +smoothing-time-constant+ 0.8) ; https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode/smoothingTimeConstant
(defconstant +texture-height+ 1)
(defconstant +fft-row+ 0)
(defconstant +unused-channel+ 0.0)

;; libm float functions, as used by the C example
(defun sinf (x) (cffi:foreign-funcall "sinf" :float x :float))
(defun cosf (x) (cffi:foreign-funcall "cosf" :float x :float))
(defun logf (x) (cffi:foreign-funcall "logf" :float x :float))

;; NOTE: FFTComplex arrays are stored as interleaved (real, imaginary) single-float pairs
(defstruct fft-data
  spectrum
  work-buffer
  prev-magnitudes
  fft-history
  (fft-history-len 0)
  (history-pos 0)
  (last-fft-time 0d0)
  (tapback-pos 0.0))

;; Cooley–Tukey FFT https://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm#Data_reordering,_bit_reversal,_and_in-place_algorithms
(defun cooley-tukey-fft-slow (spectrum n)
  (let ((j 0))
    (loop for i from 1 below (1- n)
          do (let ((bit (ash n -1)))
               (loop while (>= j bit)
                     do (decf j bit)
                        (setf bit (ash bit -1)))
               (incf j bit)
               (when (< i j)
                 (rotatef (aref spectrum (* i 2)) (aref spectrum (* j 2)))
                 (rotatef (aref spectrum (1+ (* i 2))) (aref spectrum (1+ (* j 2))))))))

  (loop for len = 2 then (ash len 1)
        while (<= len n)
        do (let* ((angle (/ (* -2.0 +pi+) len))
                  (unit-real (cosf angle))
                  (unit-imaginary (sinf angle))
                  (half (floor len 2)))
             (loop for i from 0 below n by len
                   do (let ((current-real 1.0)
                            (current-imaginary 0.0))
                        (dotimes (j half)
                          (let* ((e (* (+ i j) 2))
                                 (o (* (+ i j half) 2))
                                 (even-real (aref spectrum e))
                                 (even-imaginary (aref spectrum (1+ e)))
                                 (odd-real (aref spectrum o))
                                 (odd-imaginary (aref spectrum (1+ o)))
                                 (twiddled-real (- (* odd-real current-real) (* odd-imaginary current-imaginary)))
                                 (twiddled-imaginary (+ (* odd-real current-imaginary) (* odd-imaginary current-real))))

                            (setf (aref spectrum e) (+ even-real twiddled-real))
                            (setf (aref spectrum (1+ e)) (+ even-imaginary twiddled-imaginary))
                            (setf (aref spectrum o) (- even-real twiddled-real))
                            (setf (aref spectrum (1+ o)) (- even-imaginary twiddled-imaginary))

                            (let ((twiddle-real-next (- (* current-real unit-real) (* current-imaginary unit-imaginary))))
                              (setf current-imaginary (+ (* current-real unit-imaginary) (* current-imaginary unit-real)))
                              (setf current-real twiddle-real-next)))))))))

(defun capture-frame (fft-data audio-samples)
  (let ((work-buffer (fft-data-work-buffer fft-data))
        (prev-magnitudes (fft-data-prev-magnitudes fft-data)))
    (dotimes (i +fft-window-size+)
      (let* ((x (/ (* 2.0 +pi+ i) (- +fft-window-size+ 1.0)))
             (blackman-weight (+ (- 0.42 (* 0.5 (cosf x))) (* 0.08 (cosf (* 2.0 x)))))) ; https://en.wikipedia.org/wiki/Window_function#Blackman_window
        (setf (aref work-buffer (* i 2)) (* (aref audio-samples i) blackman-weight))
        (setf (aref work-buffer (1+ (* i 2))) 0.0)))

    (cooley-tukey-fft-slow work-buffer +fft-window-size+)
    (replace (fft-data-spectrum fft-data) work-buffer)

    (let ((smoothed-spectrum (make-array +buffer-size+ :element-type 'single-float :initial-element 0.0)))

      (dotimes (bin +buffer-size+)
        (let* ((re (aref work-buffer (* bin 2)))
               (im (aref work-buffer (1+ (* bin 2))))
               (linear-magnitude (/ (sqrt (+ (* re re) (* im im))) +fft-window-size+))

               (smoothed-magnitude (+ (* +smoothing-time-constant+ (aref prev-magnitudes bin))
                                      (* (- 1.0 +smoothing-time-constant+) linear-magnitude))))
          (setf (aref prev-magnitudes bin) smoothed-magnitude)

          (let* ((db (* (logf (max smoothed-magnitude 1e-40)) +db-to-linear-scale+))
                 (normalized (* (- db +min-decibels+) +inverse-decibel-range+)))
            (setf (aref smoothed-spectrum bin) (clamp normalized 0.0 1.0)))))

      (setf (fft-data-last-fft-time fft-data) (get-time))
      (setf (aref (fft-data-fft-history fft-data) (fft-data-history-pos fft-data)) smoothed-spectrum)
      (setf (fft-data-history-pos fft-data) (mod (1+ (fft-data-history-pos fft-data)) (fft-data-fft-history-len fft-data))))))

(defun render-frame (fft-data fft-image)
  (let ((frames-since-tapback (float (floor (float (/ (fft-data-tapback-pos fft-data) +window-time+) 1.0)) 1.0)))
    (setf frames-since-tapback (clamp frames-since-tapback 0.0 (float (1- (fft-data-fft-history-len fft-data)))))

    (let ((history-position (rem (- (fft-data-history-pos fft-data) 1 (truncate frames-since-tapback)) (fft-data-fft-history-len fft-data))))
      (when (< history-position 0) (incf history-position (fft-data-fft-history-len fft-data)))

      (let ((amplitude (aref (fft-data-fft-history fft-data) history-position)))
        (dotimes (bin +buffer-size+)
          (image-draw-pixel fft-image bin +fft-row+ (color-from-normalized (list (aref amplitude bin) +unused-channel+ +unused-channel+ +unused-channel+))))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - spectrum visualizer")

    (let* ((fft-image (gen-image-color +buffer-size+ +texture-height+ +white+))
           (fft-texture (load-texture-from-image fft-image))
           (buffer-a (load-render-texture screen-width screen-height))
           (i-resolution (vec2 (float screen-width) (float screen-height)))

           (shader (load-shader nil (text-format "resources/shaders/glsl%i/fft.fs" +glsl-version+)))

           (i-resolution-location (get-shader-location shader "iResolution"))
           (i-channel0-location (get-shader-location shader "iChannel0")))
      (set-shader-value shader i-resolution-location i-resolution +shader-uniform-vec2+)
      (set-shader-value-texture shader i-channel0-location fft-texture)

      (init-audio-device)
      (set-audio-stream-buffer-size-default +audio-stream-ring-buffer-size+)

      ;; WARNING: Memory out-of-bounds on PLATFORM_WEB
      (let ((wav (load-wave "resources/country.mp3")))
        (wave-format wav +sample-rate+ +per-sample-bit-depth+ +mono+)

        (let* ((audio-stream (load-audio-stream +sample-rate+ +per-sample-bit-depth+ +mono+))
               (fft-history-len (+ (ceiling (float (/ +fft-historical-smoothing-dur+ +window-time+) 1.0)) 1))
               (fft (make-fft-data
                     :spectrum (make-array (* +fft-window-size+ 2) :element-type 'single-float :initial-element 0.0)
                     :work-buffer (make-array (* +fft-window-size+ 2) :element-type 'single-float :initial-element 0.0)
                     :prev-magnitudes (make-array +buffer-size+ :element-type 'single-float :initial-element 0.0)
                     :fft-history (coerce (loop repeat fft-history-len
                                                collect (make-array +buffer-size+ :element-type 'single-float :initial-element 0.0))
                                          'vector)
                     :fft-history-len fft-history-len
                     :history-pos 0
                     :last-fft-time 0d0
                     :tapback-pos 0.01))

               (wav-cursor 0)
               (wav-pcm16 (wave-data wav))

               (chunk-samples (make-array +audio-stream-ring-buffer-size+ :element-type '(signed-byte 16) :initial-element 0))
               (audio-samples (make-array +fft-window-size+ :element-type 'single-float :initial-element 0.0)))
          (play-audio-stream audio-stream)

          (set-target-fps 60)
          ;;----------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (loop while (is-audio-stream-processed audio-stream)
                         do (dotimes (i +audio-stream-ring-buffer-size+)
                              (let* ((left (if (= (wave-channels wav) 2) (aref wav-pcm16 (+ (* wav-cursor 2) 0)) (aref wav-pcm16 wav-cursor)))
                                     (right (if (= (wave-channels wav) 2) (aref wav-pcm16 (+ (* wav-cursor 2) 1)) left)))
                                (setf (aref chunk-samples i) (truncate (+ left right) 2))

                                (when (>= (incf wav-cursor) (wave-frame-count wav)) (setf wav-cursor 0))))

                            (update-audio-stream audio-stream chunk-samples +audio-stream-ring-buffer-size+)

                            (dotimes (i +fft-window-size+)
                              (setf (aref audio-samples i) (/ (* (+ (aref chunk-samples (* i 2)) (aref chunk-samples (1+ (* i 2)))) 0.5) 32767.0))))

                   (capture-frame fft audio-samples)
                   (render-frame fft fft-image)
                   (update-texture fft-texture (image-data fft-image))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-shader-mode shader)
                   (set-shader-value-texture shader i-channel0-location fft-texture)
                   (draw-texture-rec (render-texture-texture buffer-a)
                                     (make-rectangle :x 0.0 :y 0.0 :width (float screen-width) :height (float (- screen-height)))
                                     (vec2 0.0 0.0) +white+)
                   (end-shader-mode)

                   (end-drawing))
          ;;------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-shader shader)
          (unload-render-texture buffer-a)
          (unload-texture fft-texture)
          (unload-image fft-image)
          (unload-audio-stream audio-stream)
          (unload-wave wav)
          (close-audio-device)

          (close-window))))))           ; Close window and OpenGL context

(main)
