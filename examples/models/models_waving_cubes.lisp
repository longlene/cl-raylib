;;;; raylib [models] example - waving cubes
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Codecat (@codecat) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Codecat (@codecat) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_waving_cubes.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-waving-cubes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-waving-cubes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - waving cubes")

    ;; Initialize the camera
    (let ((camera (make-camera3d :position (vec3 30.0 20.0 30.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 70.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Specify the amount of blocks in each direction
          (num-blocks 15))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((time (float (get-time) 1d0))

                      ;; Calculate time scale for cube position and size
                      (scale (* (+ 2.0 (float (sin time) 1.0)) 0.7))

                      ;; Move camera around the scene
                      (camera-time (* time 0.3d0)))
                 (setf (vx (camera3d-position camera)) (* (float (cos camera-time) 1.0) 40.0)
                       (vz (camera3d-position camera)) (* (float (sin camera-time) 1.0) 40.0))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-3d camera)

                 (draw-grid 10 5.0)

                 (dotimes (x num-blocks)
                   (dotimes (y num-blocks)
                     (dotimes (z num-blocks)
                       ;; Scale of the blocks depends on x/y/z positions
                       (let* ((block-scale (/ (+ x y z) 30.0))

                              ;; Scatter makes the waving effect by adding blockScale over time
                              (scatter (sin (+ (* block-scale 20.0) (float (* time 4.0d0) 1.0))))

                              ;; Calculate the cube position
                              (cube-pos (vec3 (+ (* (- x (/ (float num-blocks) 2)) (* scale 3.0)) scatter)
                                              (+ (* (- y (/ (float num-blocks) 2)) (* scale 2.0)) scatter)
                                              (+ (* (- z (/ (float num-blocks) 2)) (* scale 3.0)) scatter)))

                              ;; Pick a color with a hue depending on cube position for the rainbow color effect
                              ;; NOTE: This function is quite costly to be done per cube and frame,
                              ;; pre-catching the results into a separate array could improve performance
                              (cube-color (color-from-hsv (float (mod (* (+ x y z) 18) 360)) 0.75 0.9))

                              ;; Calculate cube size
                              (cube-size (* (- 2.4 scale) block-scale)))

                         ;; And finally, draw the cube!
                         (draw-cube cube-pos cube-size cube-size cube-size cube-color)))))

                 (end-mode-3d)

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
