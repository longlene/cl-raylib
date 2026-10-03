;;;; raylib [shapes] example - dashed line
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Luís Almeida (@luis605)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Luís Almeida (@luis605)
;;;; Common Lisp port of raylib/examples/shapes/shapes_dashed_line.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-dashed-line
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-dashed-line)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - dashed line")

    (let (;; Line Properties
          (line-start-position (vec2 20.0 50.0))
          (line-end-position (vec2 780.0 400.0))
          (dash-length 25.0)
          (blank-length 15.0)
          ;; Color selection
          (line-colors (vector +red+ +orange+ +gold+ +green+ +blue+ +violet+ +pink+ +black+))
          (color-index 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf line-end-position (get-mouse-position)) ; Line endpoint follows the mouse

               ;; Change Dash Length (UP/DOWN arrows)
               (when (is-key-down +key-up+) (incf dash-length 1.0))
               (when (and (is-key-down +key-down+) (> dash-length 1.0)) (decf dash-length 1.0))

               ;; Change Space Length (LEFT/RIGHT arrows)
               (when (is-key-down +key-right+) (incf blank-length 1.0))
               (when (and (is-key-down +key-left+) (> blank-length 1.0)) (decf blank-length 1.0))

               ;; Cycle through colors ('C' key)
               (when (is-key-pressed +key-c+) (setf color-index (mod (1+ color-index) (length line-colors))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw the dashed line with the current properties
               (draw-line-dashed line-start-position line-end-position (truncate dash-length) (truncate blank-length)
                                 (aref line-colors color-index))

               ;; Draw UI and Instructions
               (draw-rectangle 5 5 265 95 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 5 5 265 95 +blue+)

               (draw-text "CONTROLS:" 15 15 10 +black+)
               (draw-text "UP/DOWN: Change Dash Length" 15 35 10 +black+)
               (draw-text "LEFT/RIGHT: Change Space Length" 15 55 10 +black+)
               (draw-text "C: Cycle Color" 15 75 10 +black+)

               (draw-text (text-format "Dash: %.0f | Space: %.0f" dash-length blank-length) 15 115 10 +darkgray+)

               (draw-fps (- screen-width 80) 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
