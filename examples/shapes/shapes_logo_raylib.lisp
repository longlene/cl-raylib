;;;; raylib [shapes] example - logo raylib
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_logo_raylib.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-logo-raylib
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-logo-raylib)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - logo raylib")

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

             (draw-rectangle (- (truncate screen-width 2) 128) (- (truncate screen-height 2) 128) 256 256 +black+)
             (draw-rectangle (- (truncate screen-width 2) 112) (- (truncate screen-height 2) 112) 224 224 +raywhite+)
             (draw-text "raylib" (- (truncate screen-width 2) 44) (+ (truncate screen-height 2) 48) 50 +black+)

             (draw-text "this is NOT a texture!" 350 370 10 +gray+)

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (close-window)))                    ; Close window and OpenGL context

(main)
