(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let ((screen-width 800)
        (screen-height 450)
        (rand-value (get-random-value -8 5))
        (frames-counter 0))
    (with-window (screen-width screen-height "raylib [core] example - generate random values")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop until (window-should-close) ; detect window close button or ESC key
            do (incf frames-counter)
            (if (equal (mod (floor frames-counter 120) 2) 1)
                (setf rand-value (get-random-value -8 5)
                      frames-counter 0))
            (with-drawing
              (clear-background :raywhite)
              (draw-text "Every 2 seconds a new random value is generated:" 130 100 20 :maroon)
              (draw-text (text-format "%i" :int rand-value) 360 180 80 :lightgray))))))

(main)
