;;;; shapes_logo_raylib.lisp - Draw raylib logo using basic shapes
;;;; Translated from raylib/examples/shapes/shapes_logo_raylib.c

(require :cl-raylib)

(defpackage :shapes-logo-raylib
  (:use :cl :cl-raylib))

(in-package :shapes-logo-raylib)

(defun main ()
  "Main function - raylib logo using shapes"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - raylib logo using shapes")

    (set-target-fps 60) ; Set game to run at 60 frames-per-second

    ;; Main game loop
    (loop until (window-should-close) do
      ;; Update
      ;; TODO: Update your variables here

      ;; Draw
      (begin-drawing)
        (clear-background +raywhite+)

        (draw-rectangle (- (/ screen-width 2) 128) (- (/ screen-height 2) 128) 256 256 +black+)
        (draw-rectangle (- (/ screen-width 2) 112) (- (/ screen-height 2) 112) 224 224 +raywhite+)
        (draw-text "raylib" (- (/ screen-width 2) 44) (+ (/ screen-height 2) 48) 50 +black+)

        (draw-text "this is NOT a texture!" 350 370 10 +gray+)

      (end-drawing))

    ;; Close window
    (close-window)))

;; Run the example
(main)