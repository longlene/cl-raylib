;;;; raylib [core] example - input mouse wheel
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.1, last time updated with raylib 1.3
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_mouse_wheel.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-mouse-wheel
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-mouse-wheel)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input mouse wheel")

    (let ((box-position-y (- (floor screen-height 2) 40))
          (scroll-speed 4))             ; Scrolling speed in pixels

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (decf box-position-y (truncate (* (get-mouse-wheel-move) scroll-speed)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-rectangle (- (floor screen-width 2) 40) box-position-y 80 80 +maroon+)

               (draw-text "Use mouse wheel to move the cube up and down!" 10 10 20 +gray+)
               (draw-text (text-format "Box position Y: %03i" box-position-y) 10 40 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
