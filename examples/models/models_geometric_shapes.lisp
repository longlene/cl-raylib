(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  "raylib [models] example - Draw some basic geometric shapes (cube, sphere, cylinder...)"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [models] example - geometric shapes")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec 0.0 10.0 10.0)
                                   :target (vec 0.0 0.0 0.0)
                                   :up (vec 0.0 1.0 0.0)
                                   :fovy 45.0
                                   :projection :camera-perspective)))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               ;; TODO: Update your variables here

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (with-mode-3d (camera)
                   (draw-cube (vec -4.0 0.0 2.0) 2.0 5.0 2.0 :red)
                   (draw-cube-wires (vec -4.0 0.0 2.0) 2.0 5.0 2.0 :gold)
                   (draw-cube-wires (vec -4.0 0.0 -2.0) 3.0 6.0 2.0 :maroon)

                   (draw-sphere (vec -1.0 0.0 -2.0) 1.0 :green)
                   (draw-sphere-wires (vec 1.0 0.0 2.0) 2.0 16 16 :lime)

                   (draw-cylinder (vec 4.0 0.0 -2.0) 1.0 2.0 3.0 4 :skyblue)
                   (draw-cylinder-wires (vec 4.0 0.0 -2.0) 1.0 2.0 3.0 4 :darkblue)
                   (draw-cylinder-wires (vec 4.5 -1.0 2.0) 1.0 1.0 2.0 6 :brown)

                   (draw-cylinder (vec 1.0 0.0 -4.0) 0.0 1.5 3.0 8 :gold)
                   (draw-cylinder-wires (vec 1.0 0.0 -4.0) 0.0 1.5 3.0 8 :pink)

                   (draw-capsule (vec -3.0 1.5 -4.0) (vec -4.0 -1.0 -4.0) 1.2 8 8 :violet)
                   (draw-capsule-wires (vec -3.0 1.5 -4.0) (vec -4.0 -1.0 -4.0) 1.2 8 8 :purple)

                   (draw-grid 10 1.0))

                 (draw-fps 10 10))))))))

(main)