;;;; raylib [audio] example - module playing
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.5, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/audio/audio_module_playing.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-module-playing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-module-playing)

(defconstant +max-circles+ 64)

(defstruct circle-wave
  (position (vec2 0.0 0.0))
  (radius 0.0)
  (alpha 0.0)
  (speed 0.0)
  (color +blank+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; NOTE: Try to enable MSAA 4X

    (init-window screen-width screen-height "raylib [audio] example - module playing")

    (init-audio-device)                 ; Initialize audio device

    (let ((colors (vector +orange+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+ +pink+
                          +yellow+ +green+ +skyblue+ +purple+ +beige+))

          ;; Creates some circles for visual effect
          (circles (make-array +max-circles+))
          (music nil)
          (pitch 1.0)
          (time-played 0.0)
          (pause nil))

      (loop for i from (1- +max-circles+) downto 0
            do (let ((circle (make-circle-wave)))
                 (setf (circle-wave-alpha circle) 0.0)
                 (setf (circle-wave-radius circle) (float (get-random-value 10 40)))
                 (setf (vx (circle-wave-position circle)) (float (get-random-value (truncate (circle-wave-radius circle)) (truncate (- screen-width (circle-wave-radius circle))))))
                 (setf (vy (circle-wave-position circle)) (float (get-random-value (truncate (circle-wave-radius circle)) (truncate (- screen-height (circle-wave-radius circle))))))
                 (setf (circle-wave-speed circle) (/ (float (get-random-value 1 100)) 2000.0))
                 (setf (circle-wave-color circle) (aref colors (get-random-value 0 13)))
                 (setf (aref circles i) circle)))

      (setf music (load-music-stream "resources/mini1111.xm"))
      (setf (music-looping music) nil)

      (play-music-stream music)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-music-stream music) ; Update music buffer with new stream data

               ;; Restart music playing (stop and play)
               (when (is-key-pressed +key-space+)
                 (stop-music-stream music)
                 (play-music-stream music)
                 (setf pause nil))

               ;; Pause/Resume music playing
               (when (is-key-pressed +key-p+)
                 (setf pause (not pause))

                 (if pause
                     (pause-music-stream music)
                     (resume-music-stream music)))

               (cond ((is-key-down +key-down+) (decf pitch 0.01))
                     ((is-key-down +key-up+) (incf pitch 0.01)))

               (set-music-pitch music pitch)

               ;; Get timePlayed scaled to bar dimensions
               (setf time-played (* (/ (get-music-time-played music) (get-music-time-length music)) (- screen-width 40)))

               ;; Color circles animation
               (loop for i from (1- +max-circles+) downto 0
                     while (not pause)
                     do (let ((circle (aref circles i)))
                          (incf (circle-wave-alpha circle) (circle-wave-speed circle))
                          (incf (circle-wave-radius circle) (* (circle-wave-speed circle) 10.0))

                          (when (> (circle-wave-alpha circle) 1.0) (setf (circle-wave-speed circle) (* (circle-wave-speed circle) -1)))

                          (when (<= (circle-wave-alpha circle) 0.0)
                            (setf (circle-wave-alpha circle) 0.0)
                            (setf (circle-wave-radius circle) (float (get-random-value 10 40)))
                            (setf (vx (circle-wave-position circle)) (float (get-random-value (truncate (circle-wave-radius circle)) (truncate (- screen-width (circle-wave-radius circle))))))
                            (setf (vy (circle-wave-position circle)) (float (get-random-value (truncate (circle-wave-radius circle)) (truncate (- screen-height (circle-wave-radius circle))))))
                            (setf (circle-wave-color circle) (aref colors (get-random-value 0 13)))
                            (setf (circle-wave-speed circle) (/ (float (get-random-value 1 100)) 2000.0)))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (loop for i from (1- +max-circles+) downto 0
                     do (let ((circle (aref circles i)))
                          (draw-circle-v (circle-wave-position circle) (circle-wave-radius circle) (fade (circle-wave-color circle) (circle-wave-alpha circle)))))

               ;; Draw time bar
               (draw-rectangle 20 (- screen-height 20 12) (- screen-width 40) 12 +lightgray+)
               (draw-rectangle 20 (- screen-height 20 12) (truncate time-played) 12 +maroon+)
               (draw-rectangle-lines 20 (- screen-height 20 12) (- screen-width 40) 12 +gray+)

               ;; Draw help instructions
               (draw-rectangle 20 20 425 145 +white+)
               (draw-rectangle-lines 20 20 425 145 +gray+)
               (draw-text "PRESS SPACE TO RESTART MUSIC" 40 40 20 +black+)
               (draw-text "PRESS P TO PAUSE/RESUME" 40 70 20 +black+)
               (draw-text "PRESS UP/DOWN TO CHANGE SPEED" 40 100 20 +black+)
               (draw-text (text-format "SPEED: %f" pitch) 40 130 20 +maroon+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-music-stream music)       ; Unload music stream buffers from RAM

      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (close-window))))                 ; Close window and OpenGL context

(main)
