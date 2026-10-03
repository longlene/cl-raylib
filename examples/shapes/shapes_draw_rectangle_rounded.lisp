;;;; shapes_draw_rectangle_rounded.lisp - Draw rectangle rounded example (simplified)
;;;; Translated from raylib/examples/shapes/shapes_draw_rectangle_rounded.c

(require :cl-raylib)

(defpackage :shapes-draw-rectangle-rounded
  (:use :cl :cl-raylib))

(in-package :shapes-draw-rectangle-rounded)

(defun main ()
  "Main function - draw rectangle rounded example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - draw rectangle rounded")

    (let ((roundness 0.2)
          (width 200.0)
          (height 100.0)
          (segments 0)
          (line-thick 1.0)
          (draw-rect t)
          (draw-rounded-rect t)
          (draw-rounded-lines nil)
          (roundness-speed 0.01))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (let ((rec (make-rectangle :x (/ (- (get-screen-width) width 250) 2.0)
                                  :y (/ (- (get-screen-height) height) 2.0)
                                  :width width
                                  :height height)))

          ;; Animate roundness
          (incf roundness roundness-speed)
          (when (or (> roundness 1.0) (< roundness 0.0))
            (setf roundness-speed (- roundness-speed)))
          (setf roundness (max 0.0 (min 1.0 roundness)))

          ;; Handle input for toggling modes
          (when (is-key-pressed +key-one+)
            (setf draw-rect (not draw-rect)))
          (when (is-key-pressed +key-two+)
            (setf draw-rounded-rect (not draw-rounded-rect)))
          (when (is-key-pressed +key-three+)
            (setf draw-rounded-lines (not draw-rounded-lines)))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw separator line and background
            (draw-line 560 0 560 (get-screen-height) (fade +lightgray+ 0.6))
            (draw-rectangle 560 0 (- (get-screen-width) 560) (get-screen-height) (fade +lightgray+ 0.3))

            ;; Draw rectangles based on flags
            (when draw-rect 
              (draw-rectangle-rec rec (fade +gold+ 0.6)))
            (when draw-rounded-rect 
              (draw-rectangle-rounded rec roundness segments (fade +maroon+ 0.2)))
            (when draw-rounded-lines 
              (draw-rectangle-rounded-lines-ex rec roundness segments line-thick (fade +maroon+ 0.4)))

            ;; Draw info text
            (draw-text (text-format "Width: %.2f" width) 580 40 10 +darkgray+)
            (draw-text (text-format "Height: %.2f" height) 580 60 10 +darkgray+)
            (draw-text (text-format "Roundness: %.2f" roundness) 580 80 10 +darkgray+)
            (draw-text (text-format "Thickness: %.2f" line-thick) 580 100 10 +darkgray+)
            (draw-text (text-format "Segments: %d" segments) 580 120 10 +darkgray+)
            
            (draw-text (text-format "MODE: %s" (if (>= segments 4) "MANUAL" "AUTO")) 
                      580 140 10 (if (>= segments 4) +maroon+ +darkgray+))

            ;; Draw toggle instructions
            (draw-text "Press 1 to toggle normal rectangle" 580 180 10 (if draw-rect +maroon+ +darkgray+))
            (draw-text "Press 2 to toggle rounded rectangle" 580 200 10 (if draw-rounded-rect +maroon+ +darkgray+))
            (draw-text "Press 3 to toggle rounded lines" 580 220 10 (if draw-rounded-lines +maroon+ +darkgray+))
            (draw-text "Press ESC to exit" 580 260 10 +darkgray+)

            (draw-fps 10 10)

          (end-drawing))))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
