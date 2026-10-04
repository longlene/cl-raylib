;;;; raylib [audio] example - mixed processor
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example contributed by hkc (@hatkidchan) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 hkc (@hatkidchan)
;;;; Common Lisp port of raylib/examples/audio/audio_mixed_processor.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-mixed-processor
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-mixed-processor)

(defvar *exponent* 1.0)                 ; Audio exponentiation value
(defvar *average-volume* (make-array 400 :element-type 'single-float :initial-element 0.0)) ; Average volume history

;;------------------------------------------------------------------------------------
;; Audio processing function
;;------------------------------------------------------------------------------------
;; NOTE: BUFFER holds the frames as interleaved stereo floats
(defun process-audio (buffer frames)
  (let ((samples buffer)                ; Samples internally stored as <float>s
        (average 0.0))                  ; Temporary average volume

    (dotimes (frame frames)
      (let ((left (aref samples (+ (* frame 2) 0)))
            (right (aref samples (+ (* frame 2) 1))))
        (setf left (* (expt (abs left) *exponent*) (if (< left 0.0) -1.0 1.0))
              right (* (expt (abs right) *exponent*) (if (< right 0.0) -1.0 1.0)))
        (setf (aref samples (+ (* frame 2) 0)) left
              (aref samples (+ (* frame 2) 1)) right)

        (incf average (/ (abs left) frames)) ; accumulating average volume
        (incf average (/ (abs right) frames))))

    ;; Moving history to the left
    (dotimes (i 399) (setf (aref *average-volume* i) (aref *average-volume* (1+ i))))

    (setf (aref *average-volume* 399) average))) ; Adding last average value

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - mixed processor")

    (init-audio-device)                 ; Initialize audio device

    (attach-audio-mixed-processor #'process-audio)

    (let ((music (load-music-stream "resources/country.mp3"))
          (sound (load-sound "resources/coin.wav")))

      (play-music-stream music)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-music-stream music) ; Update music buffer with new stream data

               ;; Modify processing variables
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-left+) (decf *exponent* 0.05))
               (when (is-key-pressed +key-right+) (incf *exponent* 0.05))

               (when (<= *exponent* 0.5) (setf *exponent* 0.5))
               (when (>= *exponent* 3.0) (setf *exponent* 3.0))

               (when (is-key-pressed +key-space+) (play-sound sound))

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "MUSIC SHOULD BE PLAYING!" 255 150 20 +lightgray+)

               (draw-text (text-format "EXPONENT = %.2f" *exponent*) 215 180 20 +lightgray+)

               (draw-rectangle 199 199 402 34 +lightgray+)
               (dotimes (i 400)
                 (draw-line (+ 201 i) (- 232 (truncate (* (aref *average-volume* i) 32))) (+ 201 i) 232 +maroon+))
               (draw-rectangle-lines 199 199 402 34 +gray+)

               (draw-text "PRESS SPACE TO PLAY OTHER SOUND" 200 250 20 +lightgray+)
               (draw-text "USE LEFT AND RIGHT ARROWS TO ALTER DISTORTION" 140 280 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-music-stream music)       ; Unload music stream buffers from RAM
      (unload-sound sound)

      (detach-audio-mixed-processor #'process-audio) ; Disconnect audio processor

      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (close-window))))                 ; Close window and OpenGL context

(main)
