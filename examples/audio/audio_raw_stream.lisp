;;;; raylib [audio] example - raw stream
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.6, last time updated with raylib 6.0
;;;;
;;;; Example created by Ramon Santamaria (@raysan5) and reviewed by James Hofmann (@triplefox)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2026 Ramon Santamaria (@raysan5) and James Hofmann (@triplefox)
;;;; Common Lisp port of raylib/examples/audio/audio_raw_stream.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-raw-stream
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-raw-stream)

(defconstant +buffer-size+ 4096)
(defconstant +sample-rate+ 44100)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - raw stream")

    (init-audio-device)

    ;; Set the number of samples the stream will keep in memory at a time to BUFFER_SIZE
    (set-audio-stream-buffer-size-default +buffer-size+)
    (let ((buffer (make-array +buffer-size+ :element-type 'single-float :initial-element 0.0))

          ;; Init raw audio stream (sample rate: 44100, sample size: 32bit-float, channels: 1-mono)
          (stream (load-audio-stream +sample-rate+ 32 1))

          (pan 0.0)

          (sine-frequency 440)
          (new-sine-frequency 440)
          (sine-index 0)
          (sine-start-time 0d0))

      (set-audio-stream-pan stream pan)

      (play-audio-stream stream)

      (set-target-fps 30)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-down +key-up+)
                 (incf new-sine-frequency 10)
                 (when (> new-sine-frequency 12500) (setf new-sine-frequency 12500)))

               (when (is-key-down +key-down+)
                 (decf new-sine-frequency 10)
                 (when (< new-sine-frequency 20) (setf new-sine-frequency 20)))

               (when (is-key-down +key-left+)
                 (decf pan 0.01)
                 (when (< pan -1.0) (setf pan -1.0))
                 (set-audio-stream-pan stream pan))

               (when (is-key-down +key-right+)
                 (incf pan 0.01)
                 (when (> pan 1.0) (setf pan 1.0))
                 (set-audio-stream-pan stream pan))

               (when (is-audio-stream-processed stream)
                 (dotimes (i +buffer-size+)
                   (let ((wavelength (floor +sample-rate+ sine-frequency)))
                     (setf (aref buffer i) (sin (/ (* 2 +pi+ sine-index) wavelength)))
                     (incf sine-index)

                     (when (>= sine-index wavelength)
                       (setf sine-frequency new-sine-frequency
                             sine-index 0
                             sine-start-time (get-time)))))

                 (update-audio-stream stream buffer +buffer-size+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text (text-format "sine frequency: %i" sine-frequency) (- screen-width 220) 10 20 +red+)
               (draw-text (text-format "pan: %.2f" pan) (- screen-width 220) 30 20 +red+)
               (draw-text "Up/down to change frequency" 10 10 20 +darkgray+)
               (draw-text "Left/right to pan" 10 30 20 +darkgray+)

               (let ((window-start (truncate (* (- (get-time) sine-start-time) +sample-rate+)))
                     (window-size (floor +sample-rate+ 10))
                     (wavelength (floor +sample-rate+ sine-frequency)))

                 ;; Draw a sine wave with the same frequency as the one being sent to the audio stream
                 (dotimes (i screen-width)
                   (let* ((t0 (+ window-start (truncate (* i window-size) screen-width)))
                          (t1 (+ window-start (truncate (* (1+ i) window-size) screen-width)))
                          (start-pos (vec2 (float i) (+ 250 (* 50 (sin (/ (* 2 +pi+ t0) wavelength))))))
                          (end-pos (vec2 (+ (float i) 1) (+ 250 (* 50 (sin (/ (* 2 +pi+ t1) wavelength)))))))
                     (draw-line-v start-pos end-pos +red+))))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-audio-stream stream)      ; Close raw audio stream and delete buffers from RAM
      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (close-window))))                 ; Close window and OpenGL context

(main)
