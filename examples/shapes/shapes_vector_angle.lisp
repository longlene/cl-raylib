;;;; raylib [shapes] example - vector angle
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 5.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_vector_angle.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-vector-angle
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-vector-angle)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - vector angle")

    (let* ((v0 (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))
           (v1 (vector2-add v0 (vec2 100.0 80.0)))
           (v2 (vec2 0.0 0.0))          ; Updated with mouse position
           (angle 0.0)                  ; Angle in degrees
           (angle-mode 0))              ; 0-Vector2Angle(), 1-Vector2LineAngle()

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((startangle 0.0))

                 (when (= angle-mode 0) (setf startangle (* (- (vector2-line-angle v0 v1)) +rad2deg+)))
                 (when (= angle-mode 1) (setf startangle 0.0))

                 (setf v2 (get-mouse-position))

                 (when (is-key-pressed +key-space+) (setf angle-mode (if (= angle-mode 0) 1 0)))

                 (when (and (= angle-mode 0) (is-mouse-button-down +mouse-button-right+)) (setf v1 (get-mouse-position)))

                 (cond ((= angle-mode 0)
                        ;; Calculate angle between two vectors, considering a common origin (v0)
                        (let ((v1-normal (vector2-normalize (vector2-subtract v1 v0)))
                              (v2-normal (vector2-normalize (vector2-subtract v2 v0))))
                          (setf angle (* (vector2-angle v1-normal v2-normal) +rad2deg+))))
                       ((= angle-mode 1)
                        ;; Calculate angle defined by a two vectors line, in reference to horizontal line
                        (setf angle (* (vector2-line-angle v0 v2) +rad2deg+))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (cond ((= angle-mode 0)
                        (draw-text "MODE 0: Angle between V1 and V2" 10 10 20 +black+)
                        (draw-text "Right Click to Move V2" 10 30 20 +darkgray+)

                        (draw-line-ex v0 v1 2.0 +black+)
                        (draw-line-ex v0 v2 2.0 +red+)

                        (draw-circle-sector v0 40.0 startangle (+ startangle angle) 32 (fade +green+ 0.6)))
                       ((= angle-mode 1)
                        (draw-text "MODE 1: Angle formed by line V1 to V2" 10 10 20 +black+)

                        (draw-line 0 (truncate screen-height 2) screen-width (truncate screen-height 2) +lightgray+)
                        (draw-line-ex v0 v2 2.0 +red+)

                        (draw-circle-sector v0 40.0 startangle (- startangle angle) 32 (fade +green+ 0.6))))

                 (draw-text "v0" (truncate (vx v0)) (truncate (vy v0)) 10 +darkgray+)

                 ;; If the line from v0 to v1 would overlap the text, move it's position up 10
                 (when (and (= angle-mode 0) (> (vy (vector2-subtract v0 v1)) 0.0)) (draw-text "v1" (truncate (vx v1)) (- (truncate (vy v1)) 10) 10 +darkgray+))
                 (when (and (= angle-mode 0) (< (vy (vector2-subtract v0 v1)) 0.0)) (draw-text "v1" (truncate (vx v1)) (truncate (vy v1)) 10 +darkgray+))

                 ;; If angle mode 1, use v1 to emphasize the horizontal line
                 (when (= angle-mode 1) (draw-text "v1" (+ (truncate (vx v0)) 40) (truncate (vy v0)) 10 +darkgray+))

                 ;; position adjusted by -10 so it isn't hidden by cursor
                 (draw-text "v2" (- (truncate (vx v2)) 10) (- (truncate (vy v2)) 10) 10 +darkgray+)

                 (draw-text "Press SPACE to change MODE" 460 10 20 +darkgray+)

                 (draw-text (text-format "ANGLE: %2.2f" angle) 10 70 20 +lime+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
