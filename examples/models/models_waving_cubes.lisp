;;;; models_waving_cubes.lisp - Waving cubes
;;;; Translated from raylib/examples/models/models_waving_cubes.c

(require :cl-raylib)

(defpackage :models-waving-cubes
  (:use :cl :cl-raylib))

(in-package :models-waving-cubes)

(defun main ()
  "Main function - waving cubes"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [models] example - waving cubes")

    ;; Initialize the camera
    (let ((camera (make-camera-3d :position (vec3 30.0 20.0 30.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 70.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+))) ; Camera projection type

      ;; Specify the amount of blocks in each direction
      (let ((num-blocks 15))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (let ((time (get-time)))

            ;; Calculate time scale for cube position and size
            (let ((scale (* (+ 2.0 (sin time)) 0.7)))

              ;; Move camera around the scene
              (let ((camera-time (* time 0.3)))
                (setf (vx (camera-3d-position camera)) (* (cos camera-time) 40.0))
                (setf (vz (camera-3d-position camera)) (* (sin camera-time) 40.0)))

              ;; Draw
              (begin-drawing)
                (clear-background +raywhite+)

                (begin-mode-3d camera)

                  (draw-grid 10 5.0)

                  (loop for x from 0 below num-blocks do
                    (loop for y from 0 below num-blocks do
                      (loop for z from 0 below num-blocks do
                        ;; Scale of the blocks depends on x/y/z positions
                        (let ((block-scale (/ (+ x y z) 30.0)))

                          ;; Scatter makes the waving effect by adding blockScale over time
                          (let ((scatter (sin (+ (* block-scale 20.0) (* time 4.0)))))

                            ;; Calculate the cube position
                            (let ((cube-pos (vec3 (+ (* (- x (/ num-blocks 2)) (* scale 3.0)) scatter)
                                                  (+ (* (- y (/ num-blocks 2)) (* scale 2.0)) scatter)
                                                  (+ (* (- z (/ num-blocks 2)) (* scale 3.0)) scatter))))

                              ;; Pick a color with a hue depending on cube position for the rainbow color effect
                              ;; NOTE: This function is quite costly to be done per cube and frame,
                              ;; pre-caching the results into a separate array could improve performance
                              (let ((cube-color (color-from-hsv (float (mod (* (+ x y z) 18) 360)) 0.75 0.9)))

                                ;; Calculate cube size
                                (let ((cube-size (* (- 2.4 scale) block-scale)))

                                  ;; And finally, draw the cube!
                                  (draw-cube cube-pos cube-size cube-size cube-size cube-color)))))))))

                (end-mode-3d)

                (draw-fps 10 10)

              (end-drawing)))))

    ;; Close window
    (close-window)))

;; Run the example
(main)