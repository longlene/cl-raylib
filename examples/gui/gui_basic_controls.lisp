;;;; gui_basic_controls.lisp - Basic GUI controls example using cl-raylib raygui bindings
;;;; Demonstrates buttons, checkboxes, sliders, and progress bars

(require :cl-raylib)

(defpackage :gui-basic-controls
  (:use :cl :cl-raylib))

(in-package :gui-basic-controls)

(defun main ()
  "Main function - basic GUI controls demonstration"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [gui] example - basic controls")

    ;; GUI state variables
    (let ((button-pressed 0)
          (checkbox1-checked nil)
          (checkbox2-checked t)
          (slider-value 50.0)
          (progress-value 0.0)
          (progress-speed 1.0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; Animate progress bar
        (incf progress-value progress-speed)
        (when (>= progress-value 100.0)
          (setf progress-value 100.0)
          (setf progress-speed (- progress-speed)))
        (when (<= progress-value 0.0)
          (setf progress-value 0.0)
          (setf progress-speed (- progress-speed)))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Title
          (draw-text "raygui Basic Controls Demo" 210 20 20 +darkgray+)
          (draw-line 0 50 screen-width 50 +lightgray+)

          ;; Button example
          (when (gui-button (make-rectangle :x 100.0 :y 80.0 :width 120.0 :height 30.0) "Click Me!")
            (incf button-pressed))
          (draw-text (text-format "Button pressed: %d times" button-pressed) 250 85 10 +darkgray+)

          ;; Checkboxes example
          (draw-text "Checkboxes:" 100 130 10 +darkgray+)
          (setf checkbox1-checked 
                (gui-checkbox (make-rectangle :x 100.0 :y 150.0 :width 20.0 :height 20.0) 
                             "Option 1" checkbox1-checked))
          (setf checkbox2-checked 
                (gui-checkbox (make-rectangle :x 100.0 :y 180.0 :width 20.0 :height 20.0) 
                             "Option 2" checkbox2-checked))

          ;; Slider example
          (draw-text "Slider:" 100 230 10 +darkgray+)
          (setf slider-value 
                (gui-slider (make-rectangle :x 150.0 :y 250.0 :width 200.0 :height 20.0)
                           "Min" "Max" slider-value 0.0 100.0))
          (draw-text (text-format "Value: %.1f" slider-value) 370 250 10 +darkgray+)

          ;; Progress bar example
          (draw-text "Progress Bar:" 100 300 10 +darkgray+)
          (gui-progress-bar (make-rectangle :x 150.0 :y 320.0 :width 200.0 :height 20.0)
                           "0%" "100%" progress-value 0.0 100.0)
          (draw-text (text-format "Progress: %.1f%%" progress-value) 370 320 10 +darkgray+)

          ;; Instructions
          (draw-text "Press ESC to exit" 10 420 10 +darkgray+)

        (end-drawing)))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)