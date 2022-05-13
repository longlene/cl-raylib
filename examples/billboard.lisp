(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (title "raylib [models] example - drawing billboards")
         (camera-pos (make-vector3 :x 5.0 :y 4.0 :z 5.0))
         (camera-target (make-vector3 :x 0.0 :y 2.0 :z 0.0))
         (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 45.0
                                :projection +camera-perspective+))
         (bill-position (make-vector3 :x 0.0 :y 2.0 :z 0.0)))
    (with-window (screen-width screen-height title)
      (set-camera-mode camera +camera-orbital+)
      (set-target-fps 60)
      (let ((bill (load-texture "resources/billboard.png")))
        (loop
          (if (window-should-close) (return)) ; dectect window close button or ESC key
          (update-camera camera)
          (with-drawing
            (clear-background +raywhite+)
            (with-mode-3d (camera)
              (draw-grid 10 1.0)
              (raylib:draw-billboard camera bill bill-position 2.0 +raywhite+))
            (draw-fps 10 10)))))))

(main)
