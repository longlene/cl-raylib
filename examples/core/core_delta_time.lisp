;;;; raylib [core] example - delta time
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Robin (@RobinsAviary)
;;;; Common Lisp port of raylib/examples/core/core_delta_time.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-delta-time
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-delta-time)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - delta time")

    (let* ((current-fps 60)
           ;; Store the position for the both of the circles
           (delta-circle (vec2 0.0 (/ screen-height 3.0)))
           (frame-circle (vec2 0.0 (* screen-height (/ 2.0 3.0))))
           ;; The speed applied to both circles
           (speed 10.0)
           (circle-radius 32.0))

      (set-target-fps current-fps)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Adjust the FPS target based on the mouse wheel
               (let ((mouse-wheel (get-mouse-wheel-move)))
                 (when (/= mouse-wheel 0)
                   (incf current-fps (truncate mouse-wheel))
                   (when (< current-fps 0) (setf current-fps 0))
                   (set-target-fps current-fps)))

               ;; GetFrameTime() returns the time it took to draw the last frame, in seconds (usually called delta time)
               ;; Uses the delta time to make the circle look like it's moving at a "consistent" speed regardless of FPS

               ;; Multiply by 6.0 (an arbitrary value) in order to make the speed
               ;; visually closer to the other circle (at 60 fps), for comparison
               (incf (vx delta-circle) (* (get-frame-time) 6.0 speed))
               ;; This circle can move faster or slower visually depending on the FPS
               (incf (vx frame-circle) (* 0.1 speed))

               ;; If either circle is off the screen, reset it back to the start
               (when (> (vx delta-circle) screen-width) (setf (vx delta-circle) 0.0))
               (when (> (vx frame-circle) screen-width) (setf (vx frame-circle) 0.0))

               ;; Reset both circles positions
               (when (is-key-pressed +key-r+)
                 (setf (vx delta-circle) 0.0
                       (vx frame-circle) 0.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw both circles to the screen
               (draw-circle-v delta-circle circle-radius +red+)
               (draw-circle-v frame-circle circle-radius +blue+)

               ;; Draw the help text
               ;; Determine what help text to show depending on the current FPS target
               (let ((fps-text (if (<= current-fps 0)
                                   (text-format "FPS: unlimited (%i)" (get-fps))
                                   (text-format "FPS: %i (target: %i)" (get-fps) current-fps))))
                 (draw-text fps-text 10 10 20 +darkgray+))
               (draw-text (text-format "Frame time: %02.02f ms" (* (get-frame-time) 1000.0)) 10 30 20 +darkgray+)
               (draw-text "Use the scroll wheel to change the fps limit, r to reset" 10 50 20 +darkgray+)

               ;; Draw the text above the circles
               (draw-text "FUNC: x += GetFrameTime()*speed" 10 90 20 +red+)
               (draw-text "FUNC: x += speed" 10 240 20 +blue+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
