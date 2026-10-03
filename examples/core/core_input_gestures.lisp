;;;; raylib [core] example - input gestures
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 4.2
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_gestures.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-gestures
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-gestures)

(defconstant +max-gesture-strings+ 20)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input gestures")

    (let ((touch-position (vec2 0.0 0.0))
          (touch-area (make-rectangle :x 220.0 :y 10.0 :width (- screen-width 230.0) :height (- screen-height 20.0)))
          (gestures-count 0)
          (gesture-strings (make-array +max-gesture-strings+ :initial-element ""))
          (current-gesture +gesture-none+)
          (last-gesture +gesture-none+))

      ;;(set-gestures-enabled #b0000000000001001)   ; Enable only some gestures to be detected

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf last-gesture current-gesture
                     current-gesture (get-gesture-detected)
                     touch-position (get-touch-position 0))

               (when (and (check-collision-point-rec touch-position touch-area) (/= current-gesture +gesture-none+))
                 (when (/= current-gesture last-gesture)
                   ;; Store gesture string
                   (let ((name (case current-gesture
                                 (#.+gesture-tap+ "GESTURE TAP")
                                 (#.+gesture-doubletap+ "GESTURE DOUBLETAP")
                                 (#.+gesture-hold+ "GESTURE HOLD")
                                 (#.+gesture-drag+ "GESTURE DRAG")
                                 (#.+gesture-swipe-right+ "GESTURE SWIPE RIGHT")
                                 (#.+gesture-swipe-left+ "GESTURE SWIPE LEFT")
                                 (#.+gesture-swipe-up+ "GESTURE SWIPE UP")
                                 (#.+gesture-swipe-down+ "GESTURE SWIPE DOWN")
                                 (#.+gesture-pinch-in+ "GESTURE PINCH IN")
                                 (#.+gesture-pinch-out+ "GESTURE PINCH OUT"))))
                     (when name (setf (aref gesture-strings gestures-count) name)))

                   (incf gestures-count)

                   ;; Reset gestures strings
                   (when (>= gestures-count +max-gesture-strings+)
                     (fill gesture-strings "")
                     (setf gestures-count 0))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-rectangle-rec touch-area +gray+)
               (draw-rectangle 225 15 (- screen-width 240) (- screen-height 30) +raywhite+)

               (draw-text "GESTURES TEST AREA" (- screen-width 270) (- screen-height 40) 20 (fade +gray+ 0.5))

               (dotimes (i gestures-count)
                 (if (= (mod i 2) 0)
                     (draw-rectangle 10 (+ 30 (* 20 i)) 200 20 (fade +lightgray+ 0.5))
                     (draw-rectangle 10 (+ 30 (* 20 i)) 200 20 (fade +lightgray+ 0.3)))

                 (if (< i (1- gestures-count))
                     (draw-text (aref gesture-strings i) 35 (+ 36 (* 20 i)) 10 +darkgray+)
                     (draw-text (aref gesture-strings i) 35 (+ 36 (* 20 i)) 10 +maroon+)))

               (draw-rectangle-lines 10 29 200 (- screen-height 50) +gray+)
               (draw-text "DETECTED GESTURES" 50 15 10 +gray+)

               (when (/= current-gesture +gesture-none+) (draw-circle-v touch-position 30.0 +maroon+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
