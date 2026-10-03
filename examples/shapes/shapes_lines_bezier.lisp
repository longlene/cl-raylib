;;;; shapes_lines_bezier.lisp - Cubic-bezier lines
;;;; Translated from raylib/examples/shapes/shapes_lines_bezier.c

(require :cl-raylib)

(defpackage :shapes-lines-bezier
  (:use :cl :cl-raylib))

(in-package :shapes-lines-bezier)

(defun main ()
  "Main function - cubic-bezier lines"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - cubic-bezier lines")

    (let ((start-point (vec2 30.0 30.0))
          (end-point (vec2 (- screen-width 30.0) (- screen-height 30.0)))
          (move-start-point nil)
          (move-end-point nil))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (let ((mouse (get-mouse-position)))

          (when (and (check-collision-point-circle mouse start-point 10.0)
                     (is-mouse-button-down +mouse-button-left+))
            (setf move-start-point t))

          (when (and (check-collision-point-circle mouse end-point 10.0)
                     (is-mouse-button-down +mouse-button-left+))
            (setf move-end-point t))

          (when move-start-point
            (setf start-point mouse)
            (when (is-mouse-button-released +mouse-button-left+)
              (setf move-start-point nil)))

          (when move-end-point
            (setf end-point mouse)
            (when (is-mouse-button-released +mouse-button-left+)
              (setf move-end-point nil))))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-text "MOVE START-END POINTS WITH MOUSE" 15 20 20 +gray+)

          ;; Draw line Cubic Bezier, in-out interpolation (easing), no control points
          (draw-line-bezier start-point end-point 4.0 +blue+)
          
          ;; Draw start-end spline circles with some details
          (draw-circle-v start-point 
                        (if (check-collision-point-circle (get-mouse-position) start-point 10.0) 14.0 8.0)
                        (if move-start-point +red+ +blue+))
          (draw-circle-v end-point 
                        (if (check-collision-point-circle (get-mouse-position) end-point 10.0) 14.0 8.0)
                        (if move-end-point +red+ +blue+))

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)