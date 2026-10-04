;;;; raylib [models] example - basic voxel
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Tim Little (@timlittle) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Tim Little (@timlittle)
;;;; Common Lisp port of raylib/examples/models/models_basic_voxel.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-basic-voxel
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-basic-voxel)

(defconstant +world-size+ 8)            ; Size of our voxel world (8x8x8 cubes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - basic voxel")

    (disable-cursor)                    ; Lock mouse to window center

    ;; Define the camera to look into our 3d world (first person)
    (let* ((camera (make-camera3d :position (vec3 -2.0 0.0 -2.0) ; Camera position at ground level
                                  :target (vec3 0.0 0.0 0.0)     ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)         ; Camera up vector
                                  :fovy 45.0                     ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Create a cube model
           (cube-mesh (gen-mesh-cube 1.0 1.0 1.0))     ; Create a unit cube mesh
           (cube-model (load-model-from-mesh cube-mesh)) ; Convert mesh to a model

           ;; Initialize voxel world - fill with voxels
           (voxels (make-array (list +world-size+ +world-size+ +world-size+) :initial-element t)))

      (setf (material-map-color (aref (material-maps (aref (model-materials cube-model) 0)) +material-map-diffuse+)) +beige+)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-first-person+)

               ;; Handle voxel removal with mouse click
               ;; This method is quite inefficient. Ray marching through the voxel grid using DDA would be faster, but more complex.
               (when (is-mouse-button-pressed +mouse-left-button+)
                 ;; Cast a ray from the screen center (where crosshair would be)
                 (let* ((screen-center (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))
                        (ray (get-screen-to-world-ray screen-center camera))

                        ;; Check ray collision with all voxels
                        (closest-distance 99999.0)
                        (closest-voxel-position (vec3 -1.0 -1.0 -1.0))
                        (voxel-found nil))

                   (dotimes (x +world-size+)
                     (dotimes (y +world-size+)
                       (dotimes (z +world-size+)
                         (when (aref voxels x y z) ; Skip empty voxels
                           ;; Build a bounding box for this voxel
                           (let* ((position (vec3 (float x) (float y) (float z)))
                                  (box (make-bounding-box :min (vec3 (- (vx position) 0.5) (- (vy position) 0.5) (- (vz position) 0.5))
                                                          :max (vec3 (+ (vx position) 0.5) (+ (vy position) 0.5) (+ (vz position) 0.5))))

                                  ;; Check ray-box collision
                                  (collision (get-ray-collision-box ray box)))
                             (when (and (ray-collision-hit collision) (< (ray-collision-distance collision) closest-distance))
                               (setf closest-distance (ray-collision-distance collision)
                                     closest-voxel-position (vec3 (float x) (float y) (float z))
                                     voxel-found t)))))))

                   ;; Remove the closest voxel if one was hit
                   (when voxel-found
                     (setf (aref voxels
                                 (truncate (vx closest-voxel-position))
                                 (truncate (vy closest-voxel-position))
                                 (truncate (vz closest-voxel-position)))
                           nil))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-grid 10 1.0)

               ;; Draw all voxels
               (dotimes (x +world-size+)
                 (dotimes (y +world-size+)
                   (dotimes (z +world-size+)
                     (when (aref voxels x y z)
                       (let ((position (vec3 (float x) (float y) (float z))))
                         (draw-model cube-model position 1.0 +beige+)
                         (draw-cube-wires position 1.0 1.0 1.0 +black+))))))

               (end-mode-3d)

               ;; Draw reference point for raycasting to delete blocks
               (draw-circle (floor (get-screen-width) 2) (floor (get-screen-height) 2) 4 +red+)

               (draw-text "Left-click a voxel to remove it!" 10 10 20 +darkgray+)
               (draw-text "WASD to move, mouse to look around" 10 35 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model cube-model)

      (close-window))))

(main)
