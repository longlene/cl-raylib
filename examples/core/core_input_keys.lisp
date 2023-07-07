(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (ball-position (vec (float (/ screen-width 2)) (float (/ screen-height 2)))))
    (with-window (screen-width screen-height "raylib [core] example - keyboard input")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop until (window-should-close) ; detect window close button or ESC key
            do
            (if (is-key-down :key-right)
                (incf (vx ball-position) 2.0))
            (if (is-key-down :key-left)
                (incf (vx ball-position) (- 2.0)))
            (if (is-key-down :key-down)
                (incf (vy ball-position) 2.0))
            (if (is-key-down :key-up)
                (incf (vy ball-position) (- 2.0)))
            (with-drawing
              (clear-background +raywhite+)
              (draw-text "move the ball with arrow keys" 10 10 20 +darkgray+)
              (draw-circle-v ball-position 50.0 +maroon+))))))

(main)



