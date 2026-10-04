;;;; raylib [models] example - decals
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by JP Mortiboys (@themushroompirates) and reviewed by Ramon Santamaria (@raysan5)
;;;; Based on previous work by @mrdoob
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 JP Mortiboys (@themushroompirates) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_decals.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-decals
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-decals)

(defconstant +flt-max+ most-positive-single-float) ; Maximum value of a float, from bit pattern 01111111011111111111111111111111

(defconstant +max-decals+ 256)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct mesh-builder
  (vertex-count 0)
  (vertex-capacity 0)
  (vertices nil)                        ; simple-vector of vec3
  (uvs nil))                            ; simple-vector of vec2

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Add triangles to mesh builder (dynamic array manager)
(defun add-triangle-to-mesh-builder (mb vertices)
  ;; Reallocate and copy if we need to
  (when (<= (mesh-builder-vertex-capacity mb) (+ (mesh-builder-vertex-count mb) 3))
    (let* ((new-vertex-capacity (* (1+ (floor (mesh-builder-vertex-capacity mb) 256)) 256))
           (new-vertices (make-array new-vertex-capacity :initial-element nil)))

      (when (> (mesh-builder-vertex-capacity mb) 0)
        (replace new-vertices (mesh-builder-vertices mb) :end2 (mesh-builder-vertex-count mb)))

      (setf (mesh-builder-vertices mb) new-vertices
            (mesh-builder-vertex-capacity mb) new-vertex-capacity)))

  ;; Add 3 vertices
  (let ((index (mesh-builder-vertex-count mb)))
    (incf (mesh-builder-vertex-count mb) 3)

    (dotimes (i 3) (setf (aref (mesh-builder-vertices mb) (+ index i)) (vcopy (aref vertices i))))))

;; Free mesh builder
(defun free-mesh-builder (mb)
  (setf (mesh-builder-vertex-count mb) 0
        (mesh-builder-vertex-capacity mb) 0
        (mesh-builder-vertices mb) nil
        (mesh-builder-uvs mb) nil))

;; Build a Mesh from MeshBuilder data
(defun build-mesh (mb)
  (let* ((out-mesh (make-mesh))
         (count (mesh-builder-vertex-count mb)))

    (setf (mesh-vertex-count out-mesh) count
          (mesh-triangle-count out-mesh) (floor count 3)
          (mesh-vertices out-mesh) (make-array (* count 3) :element-type 'single-float :initial-element 0.0))
    (when (mesh-builder-uvs mb) (setf (mesh-texcoords out-mesh) (make-array (* count 2) :element-type 'single-float :initial-element 0.0)))

    (dotimes (i count)
      (let ((v (aref (mesh-builder-vertices mb) i)))
        (setf (aref (mesh-vertices out-mesh) (+ (* 3 i) 0)) (vx v)
              (aref (mesh-vertices out-mesh) (+ (* 3 i) 1)) (vy v)
              (aref (mesh-vertices out-mesh) (+ (* 3 i) 2)) (vz v)))

      (when (mesh-builder-uvs mb)
        (let ((uv (aref (mesh-builder-uvs mb) i)))
          (setf (aref (mesh-texcoords out-mesh) (+ (* 2 i) 0)) (vx uv)
                (aref (mesh-texcoords out-mesh) (+ (* 2 i) 1)) (vy uv)))))

    (upload-mesh out-mesh nil)

    out-mesh))

;; Clip segment
(defun clip-segment (v0 v1 p s)
  (let* ((d0 (- (vector3-dot-product v0 p) s))
         (d1 (- (vector3-dot-product v1 p) s))
         (s0 (/ d0 (- d0 d1))))
    (vector3-lerp v0 v1 s0)))

;; We're going to use these to build up our decal meshes
;; They'll resize automatically as we go, we'll free them at the end
;; NOTE: C keeps them as static variables inside GenMeshDecal()
(defvar *mesh-builders* (vector (make-mesh-builder) (make-mesh-builder)))

;; Free the data for decal generation
(defun free-decal-mesh-data ()
  (free-mesh-builder (aref *mesh-builders* 0))
  (free-mesh-builder (aref *mesh-builders* 1)))

;; Generate mesh decals for provided model
(defun gen-mesh-decal (target projection decal-size decal-offset)
  (let ((mesh-builders *mesh-builders*)
        ;; We're going to need the inverse matrix
        (inv-proj (matrix-invert projection))
        ;; We'll be flip-flopping between the two mesh builders
        ;; Reading from one and writing to the other, then swapping
        (mb-index 0))

    ;; Reset the mesh builders
    (setf (mesh-builder-vertex-count (aref mesh-builders 0)) 0
          (mesh-builder-vertex-count (aref mesh-builders 1)) 0)

    ;; First pass, just get any triangle inside the bounding box (for each mesh of the model)
    (dotimes (mesh-index (model-mesh-count target))
      (let* ((mesh (aref (model-meshes target) mesh-index))
             (mv (mesh-vertices mesh))
             (indices (mesh-indices mesh)))
        (dotimes (tri (mesh-triangle-count mesh))
          (let ((vertices (make-array 3)))

            ;; The way we calculate the vertices of the mesh triangle
            ;; depend on whether the mesh vertices are indexed or not
            (if (null indices)
                (dotimes (v 3)
                  (setf (aref vertices v) (vec3 (aref mv (+ (* 3 3 tri) (* 3 v) 0))
                                                (aref mv (+ (* 3 3 tri) (* 3 v) 1))
                                                (aref mv (+ (* 3 3 tri) (* 3 v) 2)))))
                ;; NOTE: Like C, the indexed vertex components are gathered transposed
                (dotimes (v 3)
                  (setf (aref vertices v) (vec3 (aref mv (+ (* 3 (aref indices (+ (* 3 tri) 0))) v))
                                                (aref mv (+ (* 3 (aref indices (+ (* 3 tri) 1))) v))
                                                (aref mv (+ (* 3 (aref indices (+ (* 3 tri) 2))) v))))))

            ;; Transform all 3 vertices of the triangle
            ;; and check if they are inside our decal box
            (let ((inside-count 0))
              (dotimes (i 3)
                ;; To projection space
                (let ((v (vector3-transform (aref vertices i) projection)))

                  (when (or (< (abs (vx v)) decal-size) (<= (abs (vy v)) decal-size) (<= (abs (vz v)) decal-size)) (incf inside-count))

                  ;; We need to keep the transformed vertex
                  (setf (aref vertices i) v)))

              ;; If any of them are inside, we add the triangle - we'll clip it later
              (when (> inside-count 0) (add-triangle-to-mesh-builder (aref mesh-builders mb-index) vertices)))))))

    ;; Clipping time! We need to clip against all 6 directions
    (let ((planes (vector (vec3 1.0 0.0 0.0)
                          (vec3 -1.0 0.0 0.0)
                          (vec3 0.0 1.0 0.0)
                          (vec3 0.0 -1.0 0.0)
                          (vec3 0.0 0.0 1.0)
                          (vec3 0.0 0.0 -1.0))))

      (dotimes (face 6)
        ;; Swap current model builder (so we read from the one we just wrote to)
        (setf mb-index (- 1 mb-index))

        (let ((in-mesh (aref mesh-builders (- 1 mb-index)))
              (out-mesh (aref mesh-builders mb-index))
              (s (* 0.5 decal-size))
              (plane (aref planes face)))

          ;; Reset write builder
          (setf (mesh-builder-vertex-count out-mesh) 0)

          (loop for i from 0 below (mesh-builder-vertex-count in-mesh) by 3
                do (let* ((in (mesh-builder-vertices in-mesh))
                          (d1 (- (vector3-dot-product (aref in (+ i 0)) plane) s))
                          (d2 (- (vector3-dot-product (aref in (+ i 1)) plane) s))
                          (d3 (- (vector3-dot-product (aref in (+ i 2)) plane) s))

                          (v1-out (if (> d1 0) 1 0))
                          (v2-out (if (> d2 0) 1 0))
                          (v3-out (if (> d3 0) 1 0))

                          ;; Calculate, how many vertices of the face lie outside of the clipping plane
                          (total (+ v1-out v2-out v3-out))
                          (nv1 nil) (nv2 nil) (nv3 nil) (nv4 nil))
                     (flet ((add (a b c) (add-triangle-to-mesh-builder out-mesh (vector a b c))))
                       (case total
                         (0
                          ;; The entire face lies inside of the plane, no clipping needed
                          (add (aref in i) (aref in (+ i 1)) (aref in (+ i 2))))
                         (1
                          ;; One vertex lies outside of the plane, perform clipping
                          (block one
                            (when (= v1-out 1)
                              (setf nv1 (aref in (+ i 1))
                                    nv2 (aref in (+ i 2))
                                    nv3 (clip-segment (aref in i) nv1 plane s)
                                    nv4 (clip-segment (aref in i) nv2 plane s)))

                            (when (= v2-out 1)
                              (setf nv1 (aref in i)
                                    nv2 (aref in (+ i 2))
                                    nv3 (clip-segment (aref in (+ i 1)) nv1 plane s)
                                    nv4 (clip-segment (aref in (+ i 1)) nv2 plane s))

                              (add nv3 nv2 nv1)
                              (add nv2 nv3 nv4)
                              (return-from one))

                            (when (= v3-out 1)
                              (setf nv1 (aref in i)
                                    nv2 (aref in (+ i 1))
                                    nv3 (clip-segment (aref in (+ i 2)) nv1 plane s)
                                    nv4 (clip-segment (aref in (+ i 2)) nv2 plane s)))

                            (add nv1 nv2 nv3)
                            (add nv4 nv3 nv2)))
                         (2
                          ;; Two vertices lies outside of the plane, perform clipping
                          (when (= v1-out 0)
                            (setf nv1 (aref in i)
                                  nv2 (clip-segment nv1 (aref in (+ i 1)) plane s)
                                  nv3 (clip-segment nv1 (aref in (+ i 2)) plane s))
                            (add nv1 nv2 nv3))

                          (when (= v2-out 0)
                            (setf nv1 (aref in (+ i 1))
                                  nv2 (clip-segment nv1 (aref in (+ i 2)) plane s)
                                  nv3 (clip-segment nv1 (aref in i) plane s))
                            (add nv1 nv2 nv3))

                          (when (= v3-out 0)
                            (setf nv1 (aref in (+ i 2))
                                  nv2 (clip-segment nv1 (aref in i) plane s)
                                  nv3 (clip-segment nv1 (aref in (+ i 1)) plane s))
                            (add nv1 nv2 nv3)))
                         ;; 3: The entire face lies outside of the plane, so let's discard the corresponding vertices
                         (t nil))))))))

    ;; Now we just need to re-transform the vertices
    (let ((the-mesh (aref mesh-builders mb-index)))

      ;; Allocate room for UVs
      (if (> (mesh-builder-vertex-count the-mesh) 0)
          (let ((vertices (mesh-builder-vertices the-mesh)))
            (setf (mesh-builder-uvs the-mesh) (make-array (mesh-builder-vertex-count the-mesh)))

            (dotimes (i (mesh-builder-vertex-count the-mesh))
              (let ((v (vcopy (aref vertices i))))
                ;; Calculate the UVs based on the projected coords
                ;; They are clipped to (-decalSize .. decalSize) and we want them (0..1)
                (setf (aref (mesh-builder-uvs the-mesh) i) (vec2 (+ (/ (vx v) decal-size) 0.5)
                                                                 (+ (/ (vy v) decal-size) 0.5)))

                ;; Tiny nudge in the normal direction so it renders properly over the mesh
                (decf (vz v) decal-offset)

                ;; From projection space to world space
                (setf (aref vertices i) (vector3-transform v inv-proj))))

            ;; Decal model data ready, create the mesh and return it
            (build-mesh the-mesh))
          ;; Return a blank mesh as there's nothing to add
          (make-mesh)))))

;; Button UI element
(defun gui-button (rec label)
  (let ((bg-color +gray+)
        (pressed nil))

    (when (check-collision-point-rec (get-mouse-position) rec)
      (setf bg-color +lightgray+)
      (when (is-mouse-button-pressed +mouse-button-left+) (setf pressed t)))

    (draw-rectangle-rec rec bg-color)
    (draw-rectangle-lines-ex rec 2.0 +darkgray+)

    (let* ((font-size 10)
           (text-width (measure-text label font-size)))
      (draw-text label (truncate (- (+ (rectangle-x rec) (* (rectangle-width rec) 0.5)) (* text-width 0.5)))
                 (truncate (- (+ (rectangle-y rec) (* (rectangle-height rec) 0.5)) (* font-size 0.5))) font-size +darkgray+))

    pressed))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [models] example - decals")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 5.0 5.0 5.0) ; Camera position
                                  :target (vec3 0.0 1.0 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.6 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load character model
           (model (load-model "resources/models/obj/character.obj"))

           ;; Apply character skin
           (model-texture (load-texture "resources/models/obj/character_diffuse.png"))
           (model-bbox nil)
           (model-size 0.0)
           (decal-size 0.0)
           (decal-offset 0.01)
           (placement-cube nil)
           (decal-material (load-material-default))
           (decal-texture nil)
           (show-model t)
           (decal-models (make-array +max-decals+ :initial-element nil))
           (decal-count 0))

      (set-texture-filter model-texture +texture-filter-bilinear+)
      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) model-texture)

      (setf model-bbox (get-mesh-bounding-box (aref (model-meshes model) 0))) ; Get mesh bounding box

      (setf (camera3d-target camera) (vector3-lerp (bounding-box-min model-bbox) (bounding-box-max model-bbox) 0.5))
      (setf (camera3d-position camera) (vector3-scale (bounding-box-max model-bbox) 1.0))
      (setf (vx (camera3d-position camera)) (* (vx (camera3d-position camera)) 0.1))

      (setf model-size (min (min (abs (- (vx (bounding-box-max model-bbox)) (vx (bounding-box-min model-bbox))))
                                 (abs (- (vy (bounding-box-max model-bbox)) (vy (bounding-box-min model-bbox)))))
                            (abs (- (vz (bounding-box-max model-bbox)) (vz (bounding-box-min model-bbox))))))

      (setf (camera3d-position camera) (vec3 0.0 (* (vy (bounding-box-max model-bbox)) 1.2) (* model-size 3.0)))

      (setf decal-size (* model-size 0.25))

      (setf placement-cube (load-model-from-mesh (gen-mesh-cube decal-size decal-size decal-size)))
      (setf (material-map-color (aref (material-maps (aref (model-materials placement-cube) 0)) 0)) +lime+)

      (setf (material-map-color (aref (material-maps decal-material) 0)) +yellow+)

      (let ((decal-image (load-image "resources/raylib_logo.png")))
        (image-resize-nn decal-image (floor (image-width decal-image) 4) (floor (image-height decal-image) 4))
        (setf decal-texture (load-texture-from-image decal-image))
        (unload-image decal-image))

      (set-texture-filter decal-texture +texture-filter-bilinear+)
      (setf (material-map-texture (aref (material-maps decal-material) +material-map-diffuse+)) decal-texture
            (material-map-color (aref (material-maps decal-material) +material-map-diffuse+)) +raywhite+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-mouse-button-down +mouse-button-right+) (update-camera camera +camera-third-person+))

               ;; Display information about closest hit
               (let ((collision (make-ray-collision :distance +flt-max+ :hit nil))
                     ;; Get mouse ray
                     (ray (get-screen-to-world-ray (get-mouse-position) camera)))

                 ;; Check ray collision against bounding box first, before trying the full ray-mesh test
                 (let ((box-hit-info (get-ray-collision-box ray model-bbox)))
                   (when (and (ray-collision-hit box-hit-info) (< decal-count +max-decals+))
                     ;; Check ray collision against model meshes
                     (let ((mesh-hit-info (make-ray-collision)))
                       (dotimes (m (model-mesh-count model))
                         ;; NOTE: We consider the model.transform for the collision check but
                         ;; it can be checked against any transform Matrix, used when checking against same
                         ;; model drawn multiple times with multiple transforms
                         (setf mesh-hit-info (get-ray-collision-mesh ray (aref (model-meshes model) m) (model-transform model)))

                         (when (ray-collision-hit mesh-hit-info)
                           ;; Save the closest hit mesh
                           (when (or (not (ray-collision-hit collision)) (> (ray-collision-distance collision) (ray-collision-distance mesh-hit-info)))
                             (setf collision mesh-hit-info))))

                       (when (ray-collision-hit mesh-hit-info) (setf collision mesh-hit-info)))))

                 ;; Add decal to mesh on hit point
                 (when (and (ray-collision-hit collision) (is-mouse-button-pressed +mouse-button-left+) (< decal-count +max-decals+))
                   ;; Create the transformation to project the decal
                   (let* ((origin (vector3-add (ray-collision-point collision) (vector3-scale (ray-collision-normal collision) 1.0)))
                          (splat (matrix-look-at (ray-collision-point collision) origin (vec3 0.0 1.0 0.0))))

                     ;; Spin the placement around a bit
                     (setf splat (matrix-multiply splat (matrix-rotate-z (* +deg2rad+ (float (get-random-value -180 180))))))

                     (let ((decal-mesh (gen-mesh-decal model splat decal-size decal-offset)))
                       (when (> (mesh-vertex-count decal-mesh) 0)
                         (let ((decal-index decal-count))
                           (incf decal-count)
                           (setf (aref decal-models decal-index) (load-model-from-mesh decal-mesh))
                           (setf (aref (material-maps (aref (model-materials (aref decal-models decal-index)) 0)) 0)
                                 (aref (material-maps decal-material) 0)))))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-3d camera)

                 ;; Draw the model at the origin and default scale
                 (when show-model (draw-model model (vec3 0.0 0.0 0.0) 1.0 +white+))

                 ;; Draw the decal models
                 (dotimes (i decal-count) (draw-model (aref decal-models i) (vec3 0.0 0.0 0.0) 1.0 +white+))

                 ;; If we hit the mesh, draw the box for the decal
                 (when (ray-collision-hit collision)
                   (let* ((origin (vector3-add (ray-collision-point collision) (vector3-scale (ray-collision-normal collision) 1.0)))
                          (splat (matrix-look-at (ray-collision-point collision) origin (vec3 0.0 1.0 0.0))))
                     (setf (model-transform placement-cube) (matrix-invert splat))
                     (draw-model placement-cube (vec3 0.0 0.0 0.0) 1.0 (fade +white+ 0.5))))

                 (draw-grid 10 10.0)

                 (end-mode-3d)

                 (let* ((y-pos 10.0)
                        (x0 (- (get-screen-width) 300.0))
                        (x1 (+ x0 100))
                        (x2 (+ x1 100))
                        (vertex-count 0)
                        (triangle-count 0))

                   (draw-text "Vertices" (truncate x1) (truncate y-pos) 10 +lime+)
                   (draw-text "Triangles" (truncate x2) (truncate y-pos) 10 +lime+)
                   (incf y-pos 15)

                   (dotimes (i (model-mesh-count model))
                     (incf vertex-count (mesh-vertex-count (aref (model-meshes model) i)))
                     (incf triangle-count (mesh-triangle-count (aref (model-meshes model) i))))

                   (draw-text "Main model" (truncate x0) (truncate y-pos) 10 +lime+)
                   (draw-text (text-format "%d" vertex-count) (truncate x1) (truncate y-pos) 10 +lime+)
                   (draw-text (text-format "%d" triangle-count) (truncate x2) (truncate y-pos) 10 +lime+)
                   (incf y-pos 15)

                   (dotimes (i decal-count)
                     (let ((decal-mesh (aref (model-meshes (aref decal-models i)) 0)))
                       (when (= i 20)
                         (draw-text "..." (truncate x0) (truncate y-pos) 10 +lime+)
                         (incf y-pos 15))

                       (when (< i 20)
                         (draw-text (text-format "Decal #%d" (1+ i)) (truncate x0) (truncate y-pos) 10 +lime+)
                         (draw-text (text-format "%d" (mesh-vertex-count decal-mesh)) (truncate x1) (truncate y-pos) 10 +lime+)
                         (draw-text (text-format "%d" (mesh-triangle-count decal-mesh)) (truncate x2) (truncate y-pos) 10 +lime+)
                         (incf y-pos 15))

                       (incf vertex-count (mesh-vertex-count decal-mesh))
                       (incf triangle-count (mesh-triangle-count decal-mesh))))

                   (draw-text "TOTAL" (truncate x0) (truncate y-pos) 10 +lime+)
                   (draw-text (text-format "%d" vertex-count) (truncate x1) (truncate y-pos) 10 +lime+)
                   (draw-text (text-format "%d" triangle-count) (truncate x2) (truncate y-pos) 10 +lime+)
                   (incf y-pos 15))

                 (draw-text "Hold RMB to move camera" 10 430 10 +gray+)
                 (draw-text "(c) Character model and texture from kenney.nl" (- screen-width 260) (- screen-height 20) 10 +gray+)

                 ;; UI elements
                 (when (gui-button (make-rectangle :x 10.0 :y (- screen-height 100.0) :width 100.0 :height 60.0) (if show-model "Hide Model" "Show Model"))
                   (setf show-model (not show-model)))

                 (when (gui-button (make-rectangle :x (+ 10.0 110) :y (- screen-height 100.0) :width 100.0 :height 60.0) "Clear Decals")
                   ;; Clear decals, unload all decal models
                   (dotimes (i decal-count) (unload-model (aref decal-models i)))
                   (setf decal-count 0))

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)
      (unload-model placement-cube)
      (unload-texture model-texture)

      ;; Unload decal models
      (dotimes (i decal-count) (unload-model (aref decal-models i)))

      (unload-texture decal-texture)

      (free-decal-mesh-data)            ; Free the data for decal generation

      (close-window))))                 ; Close window and OpenGL context

(main)
