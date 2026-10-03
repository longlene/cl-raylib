;;;; raylib [core] example - input keys
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_keys.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-keys
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-keys)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input keys")

    (let ((ball-position (vec2 (/ screen-width 2.0) (/ screen-height 2.0))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-down +key-right+) (incf (vx ball-position) 2.0))
               (when (is-key-down +key-left+) (decf (vx ball-position) 2.0))
               (when (is-key-down +key-up+) (decf (vy ball-position) 2.0))
               (when (is-key-down +key-down+) (incf (vy ball-position) 2.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "move the ball with arrow keys" 10 10 20 +darkgray+)

               (draw-circle-v ball-position 50.0 +maroon+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
