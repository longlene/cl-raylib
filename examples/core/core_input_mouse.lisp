;;;; raylib [core] example - input mouse
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 5.5
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_mouse.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-mouse
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-mouse)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input mouse")

    (let ((ball-position (vec2 -100.0 -100.0))
          (ball-color +darkblue+))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-h+)
                 (if (is-cursor-hidden) (show-cursor) (hide-cursor)))

               (setf ball-position (get-mouse-position))

               (cond ((is-mouse-button-pressed +mouse-button-left+) (setf ball-color +maroon+))
                     ((is-mouse-button-pressed +mouse-button-middle+) (setf ball-color +lime+))
                     ((is-mouse-button-pressed +mouse-button-right+) (setf ball-color +darkblue+))
                     ((is-mouse-button-pressed +mouse-button-side+) (setf ball-color +purple+))
                     ((is-mouse-button-pressed +mouse-button-extra+) (setf ball-color +yellow+))
                     ((is-mouse-button-pressed +mouse-button-forward+) (setf ball-color +orange+))
                     ((is-mouse-button-pressed +mouse-button-back+) (setf ball-color +beige+)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-circle-v ball-position 40.0 ball-color)

               (draw-text "move ball with mouse and click mouse button to change color" 10 10 20 +darkgray+)
               (draw-text "Press 'H' to toggle cursor visibility" 10 30 20 +darkgray+)

               (if (is-cursor-hidden)
                   (draw-text "CURSOR HIDDEN" 20 60 20 +red+)
                   (draw-text "CURSOR VISIBLE" 20 60 20 +lime+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
