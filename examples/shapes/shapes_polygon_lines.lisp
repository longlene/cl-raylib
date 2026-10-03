;;;; raylib [shapes] example - polygon lines
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.1, last time updated with raylib 6.1
;;;;
;;;; Example contributed by Matthew Roush (@MatthewRoush) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Matthew Roush (@MatthewRoush)
;;;; Common Lisp port of raylib/examples/shapes/shapes_polygon_lines.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-polygon-lines
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-polygon-lines)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - polygon lines")

    (let ((thick 2)
          (rotation 0.0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-up+) (setf thick (+ thick 1)))
               (when (is-key-pressed +key-down+) (setf thick (- thick 1)))
               (setf thick (cond ((< thick 1) 1) ((> thick 60) 60) (t thick)))

               (incf rotation (* (/ 360.0 10.0) (get-frame-time)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text (text-format "thick = %d" thick) 10 280 20 +lime+)

               ;; The square and circle outlines in the top left should match
               ;; The fist pair of outlines are drawn using `DrawRectangleLinesEx()` and `DrawCircleLinesEx()`
               ;; The second pair are drawn using `DrawPolyLinesEx()` with parameters that should visually match the first pair
               (draw-text "These should look identical!" 10 10 20 +maroon+)

               ;; Outline pair 1
               (draw-rectangle 10 40 50 50 +lightgray+)
               (draw-rectangle-lines-ex (make-rectangle :x 10.0 :y 40.0 :width 50.0 :height 50.0) (float thick) +red+)
               (draw-circle 95 65 25.0 +lightgray+)
               (draw-circle-lines-ex (vec2 95.0 65.0) 25.0 (float thick) +red+)
               (draw-text "DrawRectangleLinesEx() and DrawCircleLinesEx()" 130 60 10 +black+)

               ;; Outline pair 2
               (draw-poly (vec2 35.0 125.0) 4 35.355 45.0 +lightgray+)
               (draw-poly-lines-ex (vec2 35.0 125.0) 4 35.355 45.0 (float thick) +red+)
               (draw-poly (vec2 95.0 125.0) 36 25.0 0.0 +lightgray+)
               (draw-poly-lines-ex (vec2 95.0 125.0) 36 25.0 0.0 (float thick) +red+)
               (draw-text "DrawPolyLinesEx()" 130 120 10 +black+)

               ;; Some other shapes, all of these outlines should have the same looking thickness
               (loop for (x y sides) in '((290 220 3) (430 220 4) (570 220 5) (710 220 6)
                                          (290 360 7) (430 360 8) (570 360 9) (710 360 10))
                     do (draw-poly (vec2 (float x) (float y)) sides 60.0 rotation +lightgray+)
                        (draw-poly-lines-ex (vec2 (float x) (float y)) sides 60.0 rotation (float thick) +blue+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
