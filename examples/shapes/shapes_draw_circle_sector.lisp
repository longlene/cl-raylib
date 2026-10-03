;;;; shapes_draw_circle_sector.lisp - Draw circle sector example (simplified without GUI)
;;;; Translated from raylib/examples/shapes/shapes_draw_circle_sector.c

(require :cl-raylib)

(defpackage :shapes-draw-circle-sector
  (:use :cl :cl-raylib))

(in-package :shapes-draw-circle-sector)

(defun main ()
  "Main function - draw circle sector example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - draw circle sector")

    (let ((center (list (/ (- (get-screen-width) 300) 2.0) (/ (get-screen-height) 2.0)))
          (outer-radius 180.0)
          (start-angle 0.0)
          (end-angle 180.0)
          (segments 10)
          (min-segments 4)
          (angle-speed 1.0)) ; Animation speed for angles

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; Animate the angles
        (setf start-angle (+ start-angle angle-speed))
        (setf end-angle (+ end-angle (* angle-speed 0.7)))
        
        ;; Keep angles within reasonable bounds
        (when (> start-angle 360.0)
          (setf start-angle (- start-angle 360.0)))
        (when (> end-angle 360.0)
          (setf end-angle (- end-angle 360.0)))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Draw separator line and background
          (draw-line 500 0 500 (get-screen-height) (fade +lightgray+ 0.6))
          (draw-rectangle 500 0 (- (get-screen-width) 500) (get-screen-height) (fade +lightgray+ 0.3))

          ;; Draw circle sector
          (draw-circle-sector center outer-radius start-angle end-angle segments (fade +maroon+ 0.3))
          (draw-circle-sector-lines center outer-radius start-angle end-angle segments (fade +maroon+ 0.6))

          ;; Draw info text
          (draw-text (text-format "Start Angle: %.2f" start-angle) 520 40 10 +darkgray+)
          (draw-text (text-format "End Angle: %.2f" end-angle) 520 60 10 +darkgray+)
          (draw-text (text-format "Radius: %.2f" outer-radius) 520 80 10 +darkgray+)
          (draw-text (text-format "Segments: %d" segments) 520 100 10 +darkgray+)
          
          (setf min-segments (max 4 (ceiling (/ (- end-angle start-angle) 90))))
          (draw-text (text-format "MODE: %s" (if (>= segments min-segments) "MANUAL" "AUTO")) 
                    520 120 10 (if (>= segments min-segments) +maroon+ +darkgray+))

          (draw-text "Press ESC to exit" 520 160 10 +darkgray+)
          (draw-fps 10 10)

        (end-drawing)))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
