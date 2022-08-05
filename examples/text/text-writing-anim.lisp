(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)         (title "raylib [core] example - 3d camera free")

         (camera (make-camera3d
		  :position (vec 10.0 10.0 10.0)
		  :target (vec 0.0 0.0 0.0)
		  :up (vec 0.0 1.0 0.0)
		  :fovy 45.0
		  :projection +camera-perspective+))
         (cube-pos (vec 0.0 0.0 0.0))
         (cube-screen-pos (vec 0.0 0.0)))
    (with-window (screen-width screen-height title)
      (set-camera-mode camera +camera-free+)
      (set-target-fps 60) ; Set our game to run at 60 FPS
      (loop
        until (window-should-close) ; dectect window close button or ESC key
        do (update-camera camera)
           (setf cube-screen-pos (get-world-to-screen (v+ cube-pos (vec 0 2.5 0)) camera))
           (with-drawing
             (clear-background +raywhite+)
             (with-mode-3d (camera)
               (draw-cube cube-pos 2.0 2.0 2.0 +red+)
               (draw-cube-wires cube-pos 2.0 2.0 2.0 +maroon+)
               (draw-grid 10 1.0))
	       
	     (draw-text (format nil "~a " (measure-text "Enemy 100/100" 20)) 20 20 20 +black+)

;;	     (draw-text "Enemy: 100/100" (floor (vx cube-screen-pos)) (floor (vy cube-screen-pos)) 20 +black+)
	     (draw-text "Enemy: 100/100" (- (floor (vx cube-screen-pos)) (floor (measure-text "Enemy: 100/100" 20) 2))
			(floor (vy cube-screen-pos) ) 20 +black+)
             (draw-text "Text is always on top of the cube" (floor (- screen-width (measure-text "Text is always on top of the cube" 20)) 2) 25 20 +gray+))))))

(main)
