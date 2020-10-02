(require :cl-raylib)

(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (with-window (800 450 "raylib [core] example - basic window")
               (set-target-fps 60) ; Set our game to run at 60 FPS
               (loop
                 (if (window-should-close) (return)) ; dectect window close button or ESC key
                 (with-drawing
                   (clear-background +raywhite+)
                   (draw-fps 20 20)
                   (draw-text "cl-raylib" 320 200 20 +red+)))))

(main)
