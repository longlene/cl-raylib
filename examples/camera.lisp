(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (title "raylib [core] example - 3d camera mode")
         (camera-pos (vec 0.0 10.0 10.0))
         (camera-target (vec 0.0 0.0 0.0))
         (camera-up (vec 0.0 1.0 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 45.0
                                :projection :camera-perspective))
         (cube-pos (vec 0.0 0.0 0.0)))
    (with-window (screen-width screen-height title)
                 (set-target-fps 60) ; Set our game to run at 60 FPS
                 (loop
                   (if (window-should-close) (return)) ; dectect window close button or ESC key
                   (with-drawing
                     (clear-background +raywhite+)
                     (with-mode-3d (camera)
                                   (draw-cube cube-pos 2.0 2.0 2.0 +red+)
                                   (draw-cube-wires cube-pos 2.0 2.0 2.0 +maroon+)
                                   (draw-grid 10 1.0))

                     (draw-text "Welcome to the third dimension!" 10 40 20 +darkgray+)
                     (draw-fps 10 10))))))

(main)
