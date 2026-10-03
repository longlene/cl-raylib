;;;; core_input_gestures.lisp - Input Gestures Detection
;;;; Translated from raylib/examples/core/core_input_gestures.c

(require :cl-raylib)

(defpackage :core-input-gestures
  (:use :cl :cl-raylib))

(in-package :core-input-gestures)

(defconstant +max-gesture-strings+ 20)

(defun main ()
  "Main function - input gestures detection"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [core] example - input gestures")

    (let ((touch-position (vec2 0.0 0.0))
          (touch-area (make-rectangle :x 220.0 :y 10.0 
                                     :width (- screen-width 230.0) 
                                     :height (- screen-height 20.0)))
          (gestures-count 0)
          (gesture-strings (make-array +max-gesture-strings+ :element-type 'string :initial-element ""))
          (current-gesture +gesture-none+)
          (last-gesture +gesture-none+))

      ;; SetGesturesEnabled(0b0000000000001001);   // Enable only some gestures to be detected

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (setf last-gesture current-gesture)
        (setf current-gesture (get-gesture-detected))
        (setf touch-position (get-touch-position 0))

        (when (and (check-collision-point-rec touch-position touch-area)
                   (not (= current-gesture +gesture-none+)))
          (when (not (= current-gesture last-gesture))
            ;; Store gesture string
            (setf (aref gesture-strings gestures-count)
                  (case current-gesture
                    (#.+gesture-tap+ "GESTURE TAP")
                    (#.+gesture-doubletap+ "GESTURE DOUBLETAP")
                    (#.+gesture-hold+ "GESTURE HOLD")
                    (#.+gesture-drag+ "GESTURE DRAG")
                    (#.+gesture-swipe-right+ "GESTURE SWIPE RIGHT")
                    (#.+gesture-swipe-left+ "GESTURE SWIPE LEFT")
                    (#.+gesture-swipe-up+ "GESTURE SWIPE UP")
                    (#.+gesture-swipe-down+ "GESTURE SWIPE DOWN")
                    (#.+gesture-pinch-in+ "GESTURE PINCH IN")
                    (#.+gesture-pinch-out+ "GESTURE PINCH OUT")
                    (otherwise "")))

            (incf gestures-count)

            ;; Reset gestures strings
            (when (>= gestures-count +max-gesture-strings+)
              (loop for i from 0 below +max-gesture-strings+ do
                (setf (aref gesture-strings i) ""))
              (setf gestures-count 0))))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-rectangle-rec touch-area +gray+)
          (draw-rectangle 225 15 (- screen-width 240) (- screen-height 30) +raywhite+)

          (draw-text "GESTURES TEST AREA" (- screen-width 270) (- screen-height 40) 20 (fade +gray+ 0.5))

          (loop for i from 0 below gestures-count do
            (if (evenp i)
                (draw-rectangle 10 (+ 30 (* 20 i)) 200 20 (fade +lightgray+ 0.5))
                (draw-rectangle 10 (+ 30 (* 20 i)) 200 20 (fade +lightgray+ 0.3)))

            (if (< i (1- gestures-count))
                (draw-text (aref gesture-strings i) 35 (+ 36 (* 20 i)) 10 +darkgray+)
                (draw-text (aref gesture-strings i) 35 (+ 36 (* 20 i)) 10 +maroon+)))

          (draw-rectangle-lines 10 29 200 (- screen-height 50) +gray+)
          (draw-text "DETECTED GESTURES" 50 15 10 +gray+)

          (when (not (= current-gesture +gesture-none+))
            (draw-circle-v touch-position 30.0 +maroon+))

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)