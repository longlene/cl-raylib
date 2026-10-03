;;;; raylib [core] example - basic window
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Welcome to raylib!
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2013-2026 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_basic_window.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-basic-window
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-basic-window)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - basic window")

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             ;; TODO: Update your variables here
             ;;----------------------------------------------------------------------------------

             ;; Draw
             ;;----------------------------------------------------------------------------------
             (begin-drawing)

             (clear-background +raywhite+)

             (draw-text "Congrats! You created your first window!" 190 200 20 +lightgray+)

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (close-window)))                    ; Close window and OpenGL context

(main)
