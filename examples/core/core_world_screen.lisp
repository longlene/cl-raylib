(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (title "raylib [core] example - 3d camera free")
         (camera-pos (make-vector3 :x 10.0 :y 10.0 :z 10.0))
         (camera-target (make-vector3 :x 0.0 :y 0.0 :z 0.0))
         (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0.0))
         (camera (make-camera3d :position camera-pos :target camera-target :up camera-up :fovy 45.0 :projection +camera-perspective+))
         (cube-pos (make-vector3 :x 0.0 :y 0.0 :z 0.0))
         (cube-screen-pos (make-vector2 :x 0.0 :y 0.0)))
    (with-window (screen-width screen-height title)
      (set-camera-mode camera +camera-free+)
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        until (window-should-close) ; dectect window close button or ESC key
        do (update-camera camera)
        (setf cube-screen-pos (get-world-to-screen (make-vector3 :x (vector3-x cube-pos) :y (+ (vector3-y cube-pos) 2.5) :z (vector3-z cube-pos)) camera))
        (with-drawing
          (clear-background +raywhite+)
          (with-mode-3d (camera)
            (draw-cube cube-pos 2.0 2.0 2.0 +red+)
            (draw-cube-wires cube-pos 2.0 2.0 2.0 +maroon+)
            (draw-grid 10 1.0))

          (draw-text "Enemy: 100/100" (floor (- (vector2-x cube-screen-pos) (measure-text "Enemy: 100/100" 20)) 2) (floor (vector2-y cube-screen-pos)) 20 +black+)
          (draw-text "Text is always on top of the cube" (floor (- screen-width (measure-text "Text is always on top of the cube" 20)) 2) 25 20 +gray+))))))

(main)
