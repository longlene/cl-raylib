;;;; raylib [shapes] example - rectangle advanced
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Everton Jr. (@evertonse) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 Everton Jr. (@evertonse) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_rectangle_advanced.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-rectangle-advanced
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-rectangle-advanced)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Draw rectangle with rounded edges and horizontal gradient, with options to choose side of roundness
;; NOTE: Adapted from both 'DrawRectangleRounded()' and 'DrawRectangleGradientH()' raylib [rshapes] implementations
;; NOTE: Only the RL_TRIANGLES path is ported, SUPPORT_QUADS_DRAW_MODE is not defined for the C example
(defun draw-rectangle-rounded-gradient-h (rec roundness-left roundness-right segments left right)
  (let ((x (rectangle-x rec)) (y (rectangle-y rec))
        (width (rectangle-width rec)) (height (rectangle-height rec)))

    ;; Neither side is rounded
    (when (or (and (<= roundness-left 0.0) (<= roundness-right 0.0)) (< width 1) (< height 1))
      (draw-rectangle-gradient-ex rec left left right right)
      (return-from draw-rectangle-rounded-gradient-h))

    (when (>= roundness-left 1.0) (setf roundness-left 1.0))
    (when (>= roundness-right 1.0) (setf roundness-right 1.0))

    ;; Calculate corner radius both from right and left
    (let* ((rec-size (if (> width height) height width))
           (radius-left (/ (* rec-size roundness-left) 2))
           (radius-right (/ (* rec-size roundness-right) 2)))

      (when (<= radius-left 0.0) (setf radius-left 0.0))
      (when (<= radius-right 0.0) (setf radius-right 0.0))

      (when (and (<= radius-right 0.0) (<= radius-left 0.0)) (return-from draw-rectangle-rounded-gradient-h))

      (let* ((step-length (/ 90.0 (float segments)))
             #|
                  P0____________________P1
                  /|                    |\
                 /1|          2         |3\
             P7 /__|____________________|__\ P2
               |   |P8                P9|   |
               | 8 |          9         | 4 |
               | __|____________________|__ |
             P6 \  |P11              P10|  / P3
                 \7|          6         |5/
                  \|____________________|/
                  P5                    P4
             |#
             ;; Coordinates of the 12 points also apdated from `DrawRectangleRounded`
             (point (vector
                     ;; PO, P1, P2
                     (vec2 (+ x radius-left) y) (vec2 (- (+ x width) radius-right) y) (vec2 (+ x width) (+ y radius-right))
                     ;; P3, P4
                     (vec2 (+ x width) (- (+ y height) radius-right)) (vec2 (- (+ x width) radius-right) (+ y height))
                     ;; P5, P6, P7
                     (vec2 (+ x radius-left) (+ y height)) (vec2 x (- (+ y height) radius-left)) (vec2 x (+ y radius-left))
                     ;; P8, P9
                     (vec2 (+ x radius-left) (+ y radius-left)) (vec2 (- (+ x width) radius-right) (+ y radius-right))
                     ;; P10, P11
                     (vec2 (- (+ x width) radius-right) (- (+ y height) radius-right)) (vec2 (+ x radius-left) (- (+ y height) radius-left))))
             (centers (vector (aref point 8) (aref point 9) (aref point 10) (aref point 11)))
             (angles (vector 180.0 270.0 0.0 90.0)))
        (flet ((color (c) (rl-color4ub (first c) (second c) (third c) (fourth c)))
               (vertex (i) (rl-vertex2f (vx (aref point i)) (vy (aref point i)))))

          ;; Here we use the 'Diagram' to guide ourselves to which point receives what color
          ;; By choosing the color correctly associated with a pointe the gradient effect
          ;; will naturally come from OpenGL interpolation
          ;; But this time instead of Quad, we think in triangles
          (rl-begin +rl-triangles+)

          ;; Draw all of the 4 corners: [1] Upper Left Corner, [3] Upper Right Corner, [5] Lower Right Corner, [7] Lower Left Corner
          (dotimes (k 4)
            (let ((color (if (or (= k 0) (= k 3)) left right)) ; [1] Upper Left, [3] Upper Right, [5] Lower Right, [7] Lower Left
                  (radius (if (or (= k 0) (= k 3)) radius-left radius-right))
                  (angle (aref angles k))
                  (center (aref centers k)))
              (dotimes (i segments)
                (color color)
                (rl-vertex2f (vx center) (vy center))
                (rl-vertex2f (+ (vx center) (* (cos (* +deg2rad+ (+ angle step-length))) radius))
                             (+ (vy center) (* (sin (* +deg2rad+ (+ angle step-length))) radius)))
                (rl-vertex2f (+ (vx center) (* (cos (* +deg2rad+ angle)) radius))
                             (+ (vy center) (* (sin (* +deg2rad+ angle)) radius)))
                (incf angle step-length))))

          ;; [2] Upper Rectangle
          (color left)
          (vertex 0)
          (vertex 8)
          (color right)
          (vertex 9)
          (vertex 1)
          (color left)
          (vertex 0)
          (color right)
          (vertex 9)

          ;; [4] Right Rectangle
          (color right)
          (vertex 9)
          (vertex 10)
          (vertex 3)
          (vertex 2)
          (vertex 9)
          (vertex 3)

          ;; [6] Bottom Rectangle
          (color left)
          (vertex 11)
          (vertex 5)
          (color right)
          (vertex 4)
          (vertex 10)
          (color left)
          (vertex 11)
          (color right)
          (vertex 4)

          ;; [8] Left Rectangle
          (color left)
          (vertex 7)
          (vertex 6)
          (vertex 11)
          (vertex 8)
          (vertex 7)
          (vertex 11)

          ;; [9] Middle Rectangle
          (color left)
          (vertex 8)
          (vertex 11)
          (color right)
          (vertex 10)
          (vertex 9)
          (color left)
          (vertex 8)
          (color right)
          (vertex 10)
          (rl-end))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - rectangle advanced")

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update rectangle bounds
             ;;----------------------------------------------------------------------------------
             (let* ((width (/ (get-screen-width) 2.0)) (height (/ (get-screen-height) 6.0))
                    (rec (make-rectangle :x (- (/ (get-screen-width) 2.0) (/ width 2))
                                         :y (- (/ (get-screen-height) 2.0) (* 5 (/ height 2)))
                                         :width width :height height)))
               ;;--------------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               ;; Draw All Rectangles with different roundess  for each side and different gradients
               (draw-rectangle-rounded-gradient-h rec 0.8 0.8 36 +blue+ +red+)

               (incf (rectangle-y rec) (+ (rectangle-height rec) 1))
               (draw-rectangle-rounded-gradient-h rec 0.5 1.0 36 +red+ +pink+)

               (incf (rectangle-y rec) (+ (rectangle-height rec) 1))
               (draw-rectangle-rounded-gradient-h rec 1.0 0.5 36 +red+ +blue+)

               (incf (rectangle-y rec) (+ (rectangle-height rec) 1))
               (draw-rectangle-rounded-gradient-h rec 0.0 1.0 36 +blue+ +black+)

               (incf (rectangle-y rec) (+ (rectangle-height rec) 1))
               (draw-rectangle-rounded-gradient-h rec 1.0 0.0 36 +blue+ +pink+)
               (end-drawing)))
    ;;--------------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (close-window)))                    ; Close window and OpenGL context

(main)
