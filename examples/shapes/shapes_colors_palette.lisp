;;;; raylib [shapes] example - colors palette
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_colors_palette.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-colors-palette
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-colors-palette)

(defconstant +max-colors-count+ 21)     ; Number of colors available

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - colors palette")

    (let ((colors (vector +darkgray+ +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+ +darkbrown+
                          +gray+ +red+ +gold+ +lime+ +blue+ +violet+ +brown+ +lightgray+ +pink+ +yellow+
                          +green+ +skyblue+ +purple+ +beige+))
          (color-names (vector "DARKGRAY" "MAROON" "ORANGE" "DARKGREEN" "DARKBLUE" "DARKPURPLE"
                               "DARKBROWN" "GRAY" "RED" "GOLD" "LIME" "BLUE" "VIOLET" "BROWN"
                               "LIGHTGRAY" "PINK" "YELLOW" "GREEN" "SKYBLUE" "PURPLE" "BEIGE"))
          (colors-recs (make-array +max-colors-count+)) ; Rectangles array
          (color-state (make-array +max-colors-count+ :initial-element 0)) ; Color state: 0-DEFAULT, 1-MOUSE_HOVER
          (mouse-point (vec2 0.0 0.0)))

      ;; Fills colorsRecs data (for every rectangle)
      (dotimes (i +max-colors-count+)
        (setf (aref colors-recs i)
              (make-rectangle :x (+ 20.0 (* 100.0 (mod i 7)) (* 10.0 (mod i 7)))
                              :y (+ 80.0 (* 100.0 (truncate i 7)) (* 10.0 (/ (float i) 7)))
                              :width 100.0
                              :height 100.0)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf mouse-point (get-mouse-position))

               (dotimes (i +max-colors-count+)
                 (if (check-collision-point-rec mouse-point (aref colors-recs i))
                     (setf (aref color-state i) 1)
                     (setf (aref color-state i) 0)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "raylib colors palette" 28 42 20 +black+)
               (draw-text "press SPACE to see all colors" (- (get-screen-width) 180) (- (get-screen-height) 40) 10 +gray+)

               (dotimes (i +max-colors-count+) ; Draw all rectangles
                 (let ((rec (aref colors-recs i)))
                   (draw-rectangle-rec rec (fade (aref colors i) (if (/= (aref color-state i) 0) 0.6 1.0)))

                   (when (or (is-key-down +key-space+) (/= (aref color-state i) 0))
                     (draw-rectangle (truncate (rectangle-x rec)) (truncate (- (+ (rectangle-y rec) (rectangle-height rec)) 26))
                                     (truncate (rectangle-width rec)) 20 +black+)
                     (draw-rectangle-lines-ex rec 6.0 (fade +black+ 0.3))
                     (draw-text (aref color-names i)
                                (truncate (- (+ (rectangle-x rec) (rectangle-width rec)) (measure-text (aref color-names i) 10) 12))
                                (truncate (- (+ (rectangle-y rec) (rectangle-height rec)) 20)) 10 (aref colors i)))))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
