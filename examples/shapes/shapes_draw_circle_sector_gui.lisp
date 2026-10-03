;;;; shapes_draw_circle_sector_gui.lisp - Draw circle sector example with GUI controls
;;;; Translated from raylib/examples/shapes/shapes_draw_circle_sector.c

(require :cl-raylib)

(defpackage :shapes-draw-circle-sector-gui
  (:use :cl :cl-raylib))

(in-package :shapes-draw-circle-sector-gui)

(defun main ()
  "Main function - draw circle sector example with GUI controls"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - draw circle sector (with GUI)")

    (let ((center (list (/ (- (get-screen-width) 300) 2.0) (/ (get-screen-height) 2.0)))
          (outer-radius 180.0)
          (start-angle 0.0)
          (end-angle 180.0)
          (segments 10.0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update handled inside GUI controls

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Draw separator line and background
          (draw-line 500 0 500 (get-screen-height) (fade +lightgray+ 0.6))
          (draw-rectangle 500 0 (- (get-screen-width) 500) (get-screen-height) (fade +lightgray+ 0.3))

          ;; Draw circle sector
          (draw-circle-sector center outer-radius start-angle end-angle (truncate segments) (fade +maroon+ 0.3))
          (draw-circle-sector-lines center outer-radius start-angle end-angle (truncate segments) (fade +maroon+ 0.6))

          ;; Draw GUI controls
          (draw-text "Start Angle" 520 40 10 +darkgray+)
          (setf start-angle (gui-slider (make-rectangle :x 600.0 :y 40.0 :width 120.0 :height 20.0)
                                       nil (text-format "%.2f" start-angle) start-angle 0.0 720.0))
          
          (draw-text "End Angle" 520 70 10 +darkgray+)
          (setf end-angle (gui-slider (make-rectangle :x 600.0 :y 70.0 :width 120.0 :height 20.0)
                                     nil (text-format "%.2f" end-angle) end-angle 0.0 720.0))

          (draw-text "Radius" 520 140 10 +darkgray+)
          (setf outer-radius (gui-slider (make-rectangle :x 600.0 :y 140.0 :width 120.0 :height 20.0)
                                        nil (text-format "%.2f" outer-radius) outer-radius 0.0 200.0))
          
          (draw-text "Segments" 520 170 10 +darkgray+)
          (setf segments (gui-slider (make-rectangle :x 600.0 :y 170.0 :width 120.0 :height 20.0)
                                    nil (text-format "%.2f" segments) segments 0.0 100.0))

          (let ((min-segments (ceiling (/ (- end-angle start-angle) 90))))
            (draw-text (text-format "MODE: %s" (if (>= segments min-segments) "MANUAL" "AUTO")) 
                      600 200 10 (if (>= segments min-segments) +maroon+ +darkgray+)))

          (draw-fps 10 10)

        (end-drawing))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)