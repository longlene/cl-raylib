(require :cl-raylib)

(defpackage :raylib-user
 (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
 (let* ((screen-width 800)
       (screen-height 450)
       (ball-position (make-vector2 :x (float (/ screen-width 2)) :y (float (/ screen-height 2)))))
   (with-window (screen-width screen-height "raylib [core] example - keyboard input")
                (set-target-fps 60) ; Set our game to run at 60 FPS
                (loop
                  (if (window-should-close) (return)) ; dectect window close button or ESC key
                  (if (is-key-down +key-right+)
                   (incf (vector2-x ball-position) 3.0))
                  (if (is-key-down +key-left+)
                   (incf (vector2-x ball-position) (- 3.0)))
                  (if (is-key-down +key-down+)
                   (incf (vector2-y ball-position) 3.0))
                  (if (is-key-down +key-up+)
                   (incf (vector2-y ball-position) (- 3.0)))
                  (with-drawing
                    (clear-background +raywhite+)
                    (draw-circle-v ball-position 5.0 +maroon+))))))

 (main)
