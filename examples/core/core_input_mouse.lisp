(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (ball-position (vec -100.0 -100.0))
         (ball-color :darkblue))
    (with-window (screen-width screen-height "raylib [core] example - mouse input")
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop until (window-should-close) ; detect window close button or ESC key
            do
            (setf ball-position (get-mouse-position))
            (cond
              ((is-mouse-button-pressed :mouse-button-left)
               (setf ball-color :maroon))
              ((is-mouse-button-pressed :mouse-button-middle)
               (setf ball-color :lime))
              ((is-mouse-button-pressed :mouse-button-right)
               (setf ball-color :darkblue))
              ((is-mouse-button-pressed :mouse-button-side)
               (setf ball-color :purple))
              ((is-mouse-button-pressed :mouse-button-extra)
               (setf ball-color :yellow))
              ((is-mouse-button-pressed :mouse-button-forward)
               (setf ball-color :orange))
              ((is-mouse-button-pressed :mouse-button-back)
               (setf ball-color :beige)))
            (with-drawing
              (clear-background :raywhite)
              (draw-circle-v ball-position 40.0 ball-color)
              (draw-text "move ball with mouse and click mouse button to change color" 10 10 20 :darkgreen))))))

(main)
