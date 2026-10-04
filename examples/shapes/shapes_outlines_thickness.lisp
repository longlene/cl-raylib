;;;; raylib [shapes] example - outlines thickness
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.1, last time updated with raylib 6.1
;;;;
;;;; Example contributed by Matthew Roush (@MatthewRoush) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Matthew Roush (@MatthewRoush)
;;;; Common Lisp port of raylib/examples/shapes/shapes_outlines_thickness.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-outlines-thickness
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-outlines-thickness)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - outlines thickness")

    (let ((thick 5.0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update variables / Implement example logic at this point
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (setf thick (nth-value 1 (gui-slider-bar (make-rectangle :x 290.0 :y 50.0 :width 220.0 :height 24.0) "Thickness" (text-format "%.2f" thick) thick -30.0 30.0)))

               (draw-rectangle 35 180 220 220 +lightgray+)
               (draw-rectangle-lines-ex (make-rectangle :x 35.0 :y 180.0 :width 220.0 :height 220.0) thick +blue+)
               (draw-text "DrawRectangleLinesEx()" 35 160 10 +black+)

               (draw-rectangle-rounded (make-rectangle :x 290.0 :y 180.0 :width 220.0 :height 220.0) 0.2 9 +lightgray+)
               (draw-rectangle-rounded-lines-ex (make-rectangle :x 290.0 :y 180.0 :width 220.0 :height 220.0) 0.2 9 thick +blue+)
               (draw-text "DrawRectangleRoundedLinesEx()" 290 160 10 +black+)

               (draw-circle 655 290 110.0 +lightgray+)
               (draw-circle-lines-ex (vec2 655.0 290.0) 110.0 thick +blue+)
               (draw-text "DrawCircleLinesEx()" 545 160 10 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
