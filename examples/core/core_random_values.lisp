;;;; raylib [core] example - random values
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.1, last time updated with raylib 1.1
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_random_values.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-random-values
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-random-values)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - random values")

    ;; (set-random-seed #xaabbccff)   ; Set a custom random seed if desired, by default: "time(NULL)"

    (let ((rand-value (get-random-value -8 5)) ; Get a random integer number between -8 and 5 (both included)
          (frames-counter 0))           ; Variable used to count frames

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf frames-counter)

               ;; Every two seconds (120 frames) a new random value is generated
               (when (= (mod (floor frames-counter 120) 2) 1)
                 (setf rand-value (get-random-value -8 5)
                       frames-counter 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "Every 2 seconds a new random value is generated:" 130 100 20 +maroon+)

               (draw-text (text-format "%i" rand-value) 360 180 80 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
