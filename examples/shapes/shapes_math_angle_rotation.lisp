;;;; raylib [shapes] example - math angle rotation
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Kris (@krispy-snacc) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Kris (@krispy-snacc)
;;;; Common Lisp port of raylib/examples/shapes/shapes_math_angle_rotation.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-math-angle-rotation
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-math-angle-rotation)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 720)
        (screen-height 400))

    (init-window screen-width screen-height "raylib [shapes] example - math angle rotation")
    (set-target-fps 60)

    (let* ((center (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))
           (line-length 150.0)
           ;; Predefined angles for fixed lines
           (angles (vector 0 30 60 90))
           (num-angles (length angles))
           (total-angle 0.0))           ; Animated rotation angle
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf total-angle 1.0)   ; degrees per frame
               (when (>= total-angle 360.0) (decf total-angle 360.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +white+)

               (draw-text "Fixed angles + rotating line" 10 10 20 +lightgray+)

               ;; Draw fixed-angle lines with colorful gradient
               (dotimes (i num-angles)
                 (let* ((rad (* (aref angles i) +deg2rad+))
                        (end (vec2 (+ (vx center) (* (cos rad) line-length))
                                   (+ (vy center) (* (sin rad) line-length))))
                        ;; Gradient color from green → cyan → blue → magenta
                        (col (case i
                               (0 +green+)
                               (1 +orange+)
                               (2 +blue+)
                               (3 +magenta+)
                               (t +white+))))

                   (draw-line-ex center end 5.0 col)

                   ;; Draw angle label slightly offset along the line
                   (let ((text-pos (vec2 (+ (vx center) (* (cos rad) (+ line-length 20)))
                                         (+ (vy center) (* (sin rad) (+ line-length 20))))))
                     (draw-text (text-format "%d°" (aref angles i)) (truncate (vx text-pos)) (truncate (vy text-pos)) 20 col))))

               ;; Draw animated rotating line with changing color
               (let* ((anim-rad (* total-angle +deg2rad+))
                      (anim-end (vec2 (+ (vx center) (* (cos anim-rad) line-length))
                                      (+ (vy center) (* (sin anim-rad) line-length))))
                      ;; Cycle through HSV colors for animated line
                      (anim-col (color-from-hsv (rem total-angle 360.0) 0.8 0.9)))
                 (draw-line-ex center anim-end 5.0 anim-col))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
