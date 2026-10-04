;;;; raylib [models] example - mesh picking
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.7, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Joel Davis (@joeld42) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Joel Davis (@joeld42) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_mesh_picking.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-mesh-picking
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-mesh-picking)

(defconstant +flt-max+ most-positive-single-float) ; Maximum value of a float, from bit pattern 01111111011111111111111111111111

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - mesh picking")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 20.0 20.0 20.0) ; Camera position
                                  :target (vec3 0.0 8.0 0.0)      ; Camera looking at point
                                  :up (vec3 0.0 1.6 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 45.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (ray (make-ray))             ; Picking ray

           (tower (load-model "resources/models/obj/turret.obj"))                 ; Load OBJ model
           (texture (load-texture "resources/models/obj/turret_diffuse.png"))     ; Load model texture

           (tower-pos (vec3 0.0 0.0 0.0))                                     ; Set model position
           (tower-bbox (get-mesh-bounding-box (aref (model-meshes tower) 0))) ; Get mesh bounding box

           ;; Ground quad
           (g0 (vec3 -50.0 0.0 -50.0))
           (g1 (vec3 -50.0 0.0 50.0))
           (g2 (vec3 50.0 0.0 50.0))
           (g3 (vec3 50.0 0.0 -50.0))

           ;; Test triangle
           (ta (vec3 -25.0 0.5 0.0))
           (tb (vec3 -4.0 2.5 1.0))
           (tc (vec3 -8.0 6.5 0.0))

           (bary (vec3 0.0 0.0 0.0))

           ;; Test sphere
           (sp (vec3 -30.0 5.0 5.0))
           (sr 4.0))

      (setf (material-map-texture (aref (material-maps (aref (model-materials tower) 0)) +material-map-diffuse+)) texture) ; Set model diffuse texture

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-cursor-hidden) (update-camera camera +camera-first-person+)) ; Update camera

               ;; Toggle camera controls
               (when (is-mouse-button-pressed +mouse-button-right+)
                 (if (is-cursor-hidden) (enable-cursor) (disable-cursor)))

               ;; Display information about closest hit
               (let ((collision (make-ray-collision :distance +flt-max+ :hit nil))
                     (hit-object-name "None")
                     (cursor-color +white+))

                 ;; Get ray and test against objects
                 (setf ray (get-screen-to-world-ray (get-mouse-position) camera))

                 ;; Check ray collision against ground quad
                 (let ((ground-hit-info (get-ray-collision-quad ray g0 g1 g2 g3)))
                   (when (and (ray-collision-hit ground-hit-info) (< (ray-collision-distance ground-hit-info) (ray-collision-distance collision)))
                     (setf collision ground-hit-info
                           cursor-color +green+
                           hit-object-name "Ground")))

                 ;; Check ray collision against test triangle
                 (let ((tri-hit-info (get-ray-collision-triangle ray ta tb tc))
                       (box-hit-info nil))
                   (when (and (ray-collision-hit tri-hit-info) (< (ray-collision-distance tri-hit-info) (ray-collision-distance collision)))
                     (setf collision tri-hit-info
                           cursor-color +purple+
                           hit-object-name "Triangle")

                     (setf bary (vector3-barycenter (ray-collision-point collision) ta tb tc)))

                   ;; Check ray collision against test sphere
                   (let ((sphere-hit-info (get-ray-collision-sphere ray sp sr)))
                     (when (and (ray-collision-hit sphere-hit-info) (< (ray-collision-distance sphere-hit-info) (ray-collision-distance collision)))
                       (setf collision sphere-hit-info
                             cursor-color +orange+
                             hit-object-name "Sphere")))

                   ;; Check ray collision against bounding box first, before trying the full ray-mesh test
                   (setf box-hit-info (get-ray-collision-box ray tower-bbox))

                   (when (and (ray-collision-hit box-hit-info) (< (ray-collision-distance box-hit-info) (ray-collision-distance collision)))
                     (setf collision box-hit-info
                           cursor-color +orange+
                           hit-object-name "Box")

                     ;; Check ray collision against model meshes
                     (let ((mesh-hit-info (make-ray-collision)))
                       (dotimes (m (model-mesh-count tower))
                         ;; NOTE: We consider the model.transform for the collision check but
                         ;; it can be checked against any transform Matrix, used when checking against same
                         ;; model drawn multiple times with multiple transforms
                         (setf mesh-hit-info (get-ray-collision-mesh ray (aref (model-meshes tower) m) (model-transform tower)))
                         (when (ray-collision-hit mesh-hit-info)
                           ;; Save the closest hit mesh
                           (when (or (not (ray-collision-hit collision)) (> (ray-collision-distance collision) (ray-collision-distance mesh-hit-info)))
                             (setf collision mesh-hit-info))

                           (return)))     ; Stop once one mesh collision is detected, the colliding mesh is m

                       (when (ray-collision-hit mesh-hit-info)
                         (setf collision mesh-hit-info
                               cursor-color +orange+
                               hit-object-name "Mesh"))))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   ;; Draw the tower
                   ;; WARNING: If scale is different than 1.0f,
                   ;; not considered by GetRayCollisionModel()
                   (draw-model tower tower-pos 1.0 +white+)

                   ;; Draw the test triangle
                   (draw-line-3d ta tb +purple+)
                   (draw-line-3d tb tc +purple+)
                   (draw-line-3d tc ta +purple+)

                   ;; Draw the test sphere
                   (draw-sphere-wires sp sr 8 8 +purple+)

                   ;; Draw the mesh bbox if we hit it
                   (when (ray-collision-hit box-hit-info) (draw-bounding-box tower-bbox +lime+))

                   ;; If we hit something, draw the cursor at the hit point
                   (when (ray-collision-hit collision)
                     (let ((point (ray-collision-point collision))
                           (normal (ray-collision-normal collision)))
                       (draw-cube point 0.3 0.3 0.3 cursor-color)
                       (draw-cube-wires point 0.3 0.3 0.3 +red+)

                       (let ((normal-end (vec3 (+ (vx point) (vx normal))
                                               (+ (vy point) (vy normal))
                                               (+ (vz point) (vz normal)))))
                         (draw-line-3d point normal-end +red+))))

                   (draw-ray ray +maroon+)

                   (draw-grid 10 10.0)

                   (end-mode-3d)

                   ;; Draw some debug GUI text
                   (draw-text (text-format "Hit Object: %s" hit-object-name) 10 50 10 +black+)

                   (when (ray-collision-hit collision)
                     (let ((ypos 70)
                           (point (ray-collision-point collision))
                           (normal (ray-collision-normal collision)))

                       (draw-text (text-format "Distance: %3.2f" (ray-collision-distance collision)) 10 ypos 10 +black+)

                       (draw-text (text-format "Hit Pos: %3.2f %3.2f %3.2f" (vx point) (vy point) (vz point)) 10 (+ ypos 15) 10 +black+)

                       (draw-text (text-format "Hit Norm: %3.2f %3.2f %3.2f" (vx normal) (vy normal) (vz normal)) 10 (+ ypos 30) 10 +black+)

                       (when (and (ray-collision-hit tri-hit-info) (text-is-equal hit-object-name "Triangle"))
                         (draw-text (text-format "Barycenter: %3.2f %3.2f %3.2f" (vx bary) (vy bary) (vz bary)) 10 (+ ypos 45) 10 +black+))))

                   (draw-text "Right click mouse to toggle camera controls" 10 430 10 +gray+)

                   (draw-text "(c) Turret 3D model by Alberto Cano" (- screen-width 200) (- screen-height 20) 10 +gray+)

                   (draw-fps 10 10)

                   (end-drawing))))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model tower)              ; Unload model
      (unload-texture texture)          ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
