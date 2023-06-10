(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [core] example - basic window")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        until (window-should-close) ; detect window close button or ESC key
        do (with-drawing
             (clear-background +raywhite+)
             (draw-fps 20 20)
             (draw-text "Congrats! You created your first window!" 190 200 20 +lightgray+))))))

(main)
