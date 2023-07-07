(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (title "raylib [core] example - 3d camera free")
         (camera-pos (vec 10.0 10.0 10.0))
         (camera-target (vec 0.0 0.0 0.0))
         (camera-up (vec 0.0 1.0 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 45.0
                                :projection :camera-perspective))
         (cube-pos (vec 0.0 0.0 0.0))
         (cube-screen-pos (vec 0.0 0.0)))
    (with-window (screen-width screen-height title)
      (disable-cursor)
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        until (window-should-close) ; detect window close button or ESC key
        do (update-camera camera :camera-free)
           (setf cube-screen-pos (get-world-to-screen (v+ cube-pos (vec 0 2.5 0)) camera))
           (with-drawing
             (clear-background +raywhite+)
             (with-mode-3d (camera)
               (draw-cube cube-pos 2.0 2.0 2.0 +red+)
               (draw-cube-wires cube-pos 2.0 2.0 2.0 +maroon+)
               (draw-grid 10 1.0))
             (draw-text "Enemy: 100/100" (- (floor (vx cube-screen-pos)) (floor (measure-text "Enemy: 100/100" 20) 2)) 
                        (floor (vy cube-screen-pos) ) 20 +black+)
             (draw-text "Text is always on top of the cube" (floor (- screen-width (measure-text "Text is always on top of the cube" 20)) 2) 25 20 +gray+))))))

(main)
