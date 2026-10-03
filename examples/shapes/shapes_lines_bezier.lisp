;;;; raylib [shapes] example - lines bezier
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.7, last time updated with raylib 1.7
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_lines_bezier.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-lines-bezier
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-lines-bezier)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - lines bezier")

    (let ((start-point (vec2 30.0 30.0))
          (end-point (vec2 (- (float screen-width) 30) (- (float screen-height) 30)))
          (move-start-point nil)
          (move-end-point nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((mouse (get-mouse-position)))

                 (cond ((and (check-collision-point-circle mouse start-point 10.0) (is-mouse-button-down +mouse-button-left+))
                        (setf move-start-point t))
                       ((and (check-collision-point-circle mouse end-point 10.0) (is-mouse-button-down +mouse-button-left+))
                        (setf move-end-point t)))

                 (when move-start-point
                   (setf start-point mouse)
                   (when (is-mouse-button-released +mouse-button-left+) (setf move-start-point nil)))

                 (when move-end-point
                   (setf end-point mouse)
                   (when (is-mouse-button-released +mouse-button-left+) (setf move-end-point nil)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-text "MOVE START-END POINTS WITH MOUSE" 15 20 20 +gray+)

                 ;; Draw line Cubic Bezier, in-out interpolation (easing), no control points
                 (draw-line-bezier start-point end-point 4.0 +blue+)

                 ;; Draw start-end spline circles with some details
                 (draw-circle-v start-point (if (check-collision-point-circle mouse start-point 10.0) 14.0 8.0) (if move-start-point +red+ +blue+))
                 (draw-circle-v end-point (if (check-collision-point-circle mouse end-point 10.0) 14.0 8.0) (if move-end-point +red+ +blue+))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
