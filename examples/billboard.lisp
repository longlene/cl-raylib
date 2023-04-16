(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  (let* ((screen-width 800)
         (screen-height 450)
         (title "raylib [models] example - drawing billboards")
         (camera-pos (vec 5.0 4.0 5.0))
         (camera-target (vec 0.0 2.0 0.0))
         (camera-up (vec 0.0 1.0 0.0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 45.0
                                :projection :camera-perspective))
         (bill-position (vec 0.0 2.0 0.0)))
    (with-window (screen-width screen-height title)
      (set-target-fps 60)
      (let ((bill (load-texture "resources/billboard.png")))
        (loop
          (if (window-should-close) (return)) ; dectect window close button or ESC key
          (update-camera camera :camera-orbital)
          (with-drawing
            (clear-background +raywhite+)
            (with-mode-3d (camera)
              (draw-grid 10 1.0)
              (raylib:draw-billboard camera bill bill-position 2.0 +raywhite+))
            (draw-fps 10 10)))))))

(main)
