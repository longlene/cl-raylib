;;;; shapes_draw_ring.lisp - Draw ring example (simplified)
;;;; Translated from raylib/examples/shapes/shapes_draw_ring.c

(require :cl-raylib)

(defpackage :shapes-draw-ring
  (:use :cl :cl-raylib))

(in-package :shapes-draw-ring)

(defun main ()
  "Main function - draw ring example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - draw ring")

    (let ((center (list (/ (- (get-screen-width) 300) 2.0) (/ (get-screen-height) 2.0)))
          (inner-radius 80.0)
          (outer-radius 190.0)
          (start-angle 0.0)
          (end-angle 360.0)
          (segments 0)
          (draw-ring t)
          (draw-ring-lines nil)
          (draw-circle-lines nil)
          (angle-speed 2.0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; Animate the angles
        (incf start-angle angle-speed)
        (incf end-angle (* angle-speed 0.8))
        
        ;; Keep angles within bounds
        (when (> start-angle 360.0)
          (setf start-angle (- start-angle 360.0)))
        (when (> end-angle 720.0)
          (setf end-angle (- end-angle 360.0)))

        ;; Handle input for toggling modes
        (when (is-key-pressed +key-one+)
          (setf draw-ring (not draw-ring)))
        (when (is-key-pressed +key-two+)
          (setf draw-ring-lines (not draw-ring-lines)))
        (when (is-key-pressed +key-three+)
          (setf draw-circle-lines (not draw-circle-lines)))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Draw separator line and background
          (draw-line 500 0 500 (get-screen-height) (fade +lightgray+ 0.6))
          (draw-rectangle 500 0 (- (get-screen-width) 500) (get-screen-height) (fade +lightgray+ 0.3))

          ;; Draw rings based on flags
          (when draw-ring 
            (draw-ring center inner-radius outer-radius start-angle end-angle segments (fade +maroon+ 0.3)))
          (when draw-ring-lines 
            (draw-ring-lines center inner-radius outer-radius start-angle end-angle segments (fade +black+ 0.4)))
          (when draw-circle-lines 
            (draw-circle-sector-lines center outer-radius start-angle end-angle segments (fade +black+ 0.4)))

          ;; Draw info text
          (draw-text (text-format "Start Angle: %.2f" start-angle) 520 40 10 +darkgray+)
          (draw-text (text-format "End Angle: %.2f" end-angle) 520 60 10 +darkgray+)
          (draw-text (text-format "Inner Radius: %.2f" inner-radius) 520 100 10 +darkgray+)
          (draw-text (text-format "Outer Radius: %.2f" outer-radius) 520 120 10 +darkgray+)
          (draw-text (text-format "Segments: %d" segments) 520 160 10 +darkgray+)
          
          (let ((min-segments (max 1 (ceiling (/ (- end-angle start-angle) 90)))))
            (draw-text (text-format "MODE: %s" (if (>= segments min-segments) "MANUAL" "AUTO")) 
                      520 180 10 (if (>= segments min-segments) +maroon+ +darkgray+)))

          ;; Draw toggle instructions
          (draw-text "Press 1 to toggle ring" 520 220 10 (if draw-ring +maroon+ +darkgray+))
          (draw-text "Press 2 to toggle ring lines" 520 240 10 (if draw-ring-lines +maroon+ +darkgray+))
          (draw-text "Press 3 to toggle circle lines" 520 260 10 (if draw-circle-lines +maroon+ +darkgray+))
          (draw-text "Press ESC to exit" 520 300 10 +darkgray+)

          (draw-fps 10 10)

        (end-drawing))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
