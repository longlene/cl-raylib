;;;; raylib [audio] example - music stream
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/audio/audio_music_stream.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-music-stream
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-music-stream)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - music stream")

    (init-audio-device)                 ; Initialize audio device

    (let ((music (load-music-stream "resources/country.mp3"))
          (time-played 0.0)             ; Time played normalized [0.0f..1.0f]
          (pause nil)                   ; Music playing paused
          (pan 0.0)                     ; Default audio pan center [-1.0f..1.0f]
          (volume 0.8))                 ; Default audio volume [0.0f..1.0f]

      (play-music-stream music)

      (set-music-pan music pan)
      (set-music-volume music volume)

      (set-target-fps 30)               ; Set our game to run at 30 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-music-stream music) ; Update music buffer with new stream data

               ;; Restart music playing (stop and play)
               (when (is-key-pressed +key-space+)
                 (stop-music-stream music)
                 (play-music-stream music))

               ;; Pause/Resume music playing
               (when (is-key-pressed +key-p+)
                 (setf pause (not pause))

                 (if pause
                     (pause-music-stream music)
                     (resume-music-stream music)))

               ;; Set audio pan
               (cond ((is-key-down +key-left+)
                      (decf pan 0.05)
                      (when (< pan -1.0) (setf pan -1.0))
                      (set-music-pan music pan))
                     ((is-key-down +key-right+)
                      (incf pan 0.05)
                      (when (> pan 1.0) (setf pan 1.0))
                      (set-music-pan music pan)))

               ;; Set audio volume
               (cond ((is-key-down +key-down+)
                      (decf volume 0.05)
                      (when (< volume 0.0) (setf volume 0.0))
                      (set-music-volume music volume))
                     ((is-key-down +key-up+)
                      (incf volume 0.05)
                      (when (> volume 1.0) (setf volume 1.0))
                      (set-music-volume music volume)))

               ;; Get normalized time played for current music stream
               (setf time-played (/ (get-music-time-played music) (get-music-time-length music)))

               (when (> time-played 1.0) (setf time-played 1.0)) ; Make sure time played is no longer than music
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "MUSIC SHOULD BE PLAYING!" 255 150 20 +lightgray+)

               (draw-text "LEFT-RIGHT for PAN CONTROL" 320 74 10 +darkblue+)
               (draw-rectangle 300 100 200 12 +lightgray+)
               (draw-rectangle-lines 300 100 200 12 +gray+)
               (draw-rectangle (truncate (- (+ 300 (* (/ (+ pan 1.0) 2.0) 200)) 5)) 92 10 28 +darkgray+)

               (draw-rectangle 200 200 400 12 +lightgray+)
               (draw-rectangle 200 200 (truncate (* time-played 400.0)) 12 +maroon+)
               (draw-rectangle-lines 200 200 400 12 +gray+)

               (draw-text "PRESS SPACE TO RESTART MUSIC" 215 250 20 +lightgray+)
               (draw-text "PRESS P TO PAUSE/RESUME MUSIC" 208 280 20 +lightgray+)

               (draw-text "UP-DOWN for VOLUME CONTROL" 320 334 10 +darkgreen+)
               (draw-rectangle 300 360 200 12 +lightgray+)
               (draw-rectangle-lines 300 360 200 12 +gray+)
               (draw-rectangle (truncate (- (+ 300 (* volume 200)) 5)) 352 10 28 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-music-stream music)       ; Unload music stream buffers from RAM

      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (close-window))))                 ; Close window and OpenGL context

(main)
