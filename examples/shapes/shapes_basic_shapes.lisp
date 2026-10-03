;;;; raylib [shapes] example - basic shapes
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_basic_shapes.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-basic-shapes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-basic-shapes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - basic shapes")

    (let ((rotation 0.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf rotation 0.2)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "some basic shapes available on raylib" 20 20 20 +darkgray+)

               ;; Circle shapes and lines
               (draw-circle (truncate screen-width 5) 120 35.0 +darkblue+)
               (draw-circle-gradient (vec2 (/ screen-width 5.0) 220.0) 60.0 +green+ +skyblue+)
               (draw-circle-lines (truncate screen-width 5) 340 80.0 +darkblue+)
               (draw-ellipse (truncate screen-width 5) 120 25.0 20.0 +yellow+)
               (draw-ellipse-lines (truncate screen-width 5) 120 30.0 25.0 +yellow+)

               ;; Rectangle shapes and lines
               (draw-rectangle (- (* (truncate screen-width 4) 2) 60) 100 120 60 +red+)
               (draw-rectangle-gradient-h (- (* (truncate screen-width 4) 2) 90) 170 180 130 +maroon+ +gold+)
               (draw-rectangle-lines (- (* (truncate screen-width 4) 2) 40) 320 80 60 +orange+) ; NOTE: Uses QUADS internally, not lines

               ;; Triangle shapes and lines
               (draw-triangle (vec2 (* (/ screen-width 4.0) 3.0) 80.0)
                              (vec2 (- (* (/ screen-width 4.0) 3.0) 60.0) 150.0)
                              (vec2 (+ (* (/ screen-width 4.0) 3.0) 60.0) 150.0) +violet+)

               (draw-triangle-lines (vec2 (* (/ screen-width 4.0) 3.0) 160.0)
                                    (vec2 (- (* (/ screen-width 4.0) 3.0) 20.0) 230.0)
                                    (vec2 (+ (* (/ screen-width 4.0) 3.0) 20.0) 230.0) +darkblue+)

               ;; Polygon shapes and lines
               (draw-poly (vec2 (* (/ screen-width 4.0) 3) 330.0) 6 80.0 rotation +brown+)
               (draw-poly-lines (vec2 (* (/ screen-width 4.0) 3) 330.0) 6 90.0 rotation +brown+)
               (draw-poly-lines-ex (vec2 (* (/ screen-width 4.0) 3) 330.0) 6 85.0 rotation 6.0 +beige+)

               ;; NOTE: We draw all LINES based shapes together to optimize internal drawing,
               ;; this way, all LINES are rendered in a single draw pass
               (draw-line 18 42 (- screen-width 18) 42 +black+)
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
