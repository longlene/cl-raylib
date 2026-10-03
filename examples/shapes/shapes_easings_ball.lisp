;;;; raylib [shapes] example - easings ball
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_easings_ball.c

(require :cl-raylib)
(load (merge-pathnames "reasings.lisp" *load-truename*)) ; Required for easing functions

(defpackage #:raylib-examples/shapes-easings-ball
  (:use #:cl #:raylib #:reasings))
(in-package #:raylib-examples/shapes-easings-ball)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - easings ball")

    ;; Ball variable value to be animated with easings
    (let ((ball-position-x -100)
          (ball-radius 20)
          (ball-alpha 0.0)
          (state 0)
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (case state
                 (0                     ; Move ball position X with easing
                  (incf frames-counter)
                  (setf ball-position-x (truncate (ease-elastic-out (float frames-counter) -100 (+ (/ screen-width 2.0) 100) 120)))

                  (when (>= frames-counter 120)
                    (setf frames-counter 0
                          state 1)))
                 (1                     ; Increase ball radius with easing
                  (incf frames-counter)
                  (setf ball-radius (truncate (ease-elastic-in (float frames-counter) 20 500 200)))

                  (when (>= frames-counter 200)
                    (setf frames-counter 0
                          state 2)))
                 (2                     ; Change ball alpha with easing (background color blending)
                  (incf frames-counter)
                  (setf ball-alpha (ease-cubic-out (float frames-counter) 0.0 1.0 200))

                  (when (>= frames-counter 200)
                    (setf frames-counter 0
                          state 3)))
                 (3                     ; Reset state to play again
                  (when (is-key-pressed +key-enter+)
                    ;; Reset required variables to play again
                    (setf ball-position-x -100
                          ball-radius 20
                          ball-alpha 0.0
                          state 0))))

               (when (is-key-pressed +key-r+) (setf frames-counter 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (when (>= state 2) (draw-rectangle 0 0 screen-width screen-height +green+))
               (draw-circle ball-position-x 200 (float ball-radius) (fade +red+ (- 1.0 ball-alpha)))

               (when (= state 3) (draw-text "PRESS [ENTER] TO PLAY AGAIN!" 240 200 20 +black+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
