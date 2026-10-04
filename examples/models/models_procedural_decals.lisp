;;;; raylib [models] example - procedural decals
;;;;
;;;; Example demonstrates projecting decals onto a 3D mesh surface by transforming the
;;;; mesh's own triangles into the decal's local space and clipping them against a box,
;;;; using only procedurally generated geometry and textures (no external resource files)
;;;;
;;;; NOTE: This is the same clip-space decal projection idea used by models_decals.c
;;;; (in turn based on three.js' DecalGeometry), applied here to a generated mesh and
;;;; generated textures instead of a loaded model, so the example is fully self-contained
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by PanicTitan (@PanicTitan) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 PanicTitan (@PanicTitan)
;;;; Common Lisp port of raylib/examples/models/models_procedural_decals.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-procedural-decals
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/models-procedural-decals)

;;----------------------------------------------------------------------------------
;; Global Definitions
;;----------------------------------------------------------------------------------
(defconstant +max-decals+ 256)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Growable triangle-soup buffer used while building a clipped decal mesh
(defstruct mesh-builder
  (vertex-count 0)
  (vertex-capacity 0)
  (vertices nil)                        ; simple-vector of vec3
  (uvs nil))                            ; simple-vector of vec2

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Appends a triangle to a growable mesh builder buffer, growing it in fixed-size chunks
(defun add-triangle-to-mesh-builder (mb v0 v1 v2)
  (when (<= (mesh-builder-vertex-capacity mb) (+ (mesh-builder-vertex-count mb) 3))
    (let* ((new-vertex-capacity (* (1+ (floor (mesh-builder-vertex-capacity mb) 256)) 256))
           (new-vertices (make-array new-vertex-capacity :initial-element nil)))
      (when (> (mesh-builder-vertex-capacity mb) 0)
        (replace new-vertices (mesh-builder-vertices mb) :end2 (mesh-builder-vertex-count mb)))
      (setf (mesh-builder-vertices mb) new-vertices
            (mesh-builder-vertex-capacity mb) new-vertex-capacity)))

  (let ((index (mesh-builder-vertex-count mb)))
    (incf (mesh-builder-vertex-count mb) 3)
    (setf (aref (mesh-builder-vertices mb) (+ index 0)) (vcopy v0)
          (aref (mesh-builder-vertices mb) (+ index 1)) (vcopy v1)
          (aref (mesh-builder-vertices mb) (+ index 2)) (vcopy v2))))

;; Frees a mesh builder's backing buffers
(defun free-mesh-builder (mb)
  (setf (mesh-builder-vertex-count mb) 0
        (mesh-builder-vertex-capacity mb) 0
        (mesh-builder-vertices mb) nil
        (mesh-builder-uvs mb) nil))

;; Converts a mesh builder's triangle soup into a standard uploaded raylib Mesh
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

;; Clips the segment [v0, v1] against the plane p.x = s, returning the intersection point
(defun clip-segment (v0 v1 p s)
  (let* ((d0 (- (vector3-dot-product v0 p) s))
         (d1 (- (vector3-dot-product v1 p) s))
         (tt (/ d0 (- d0 d1))))
    (vector3-lerp v0 v1 tt)))

;; Builds a decal mesh: the model's triangles are transformed into the decal's local space
;; (so the decal sits at the origin, facing +Z) and clipped against a decalSize-sided box,
;; following the same clip-space projection idea used by engines' decal systems (and by
;; three.js' DecalGeometry, which the technique is commonly traced back to). What's left
;; after clipping becomes the decal's own small mesh, UV-mapped from its local coordinates
(defun gen-mesh-decal (model projection decal-size decal-offset)
  (let ((inv-proj (matrix-invert projection))
        (mesh-builders (vector (make-mesh-builder) (make-mesh-builder)))
        (mb-index 0))

    ;; Gather triangles that land anywhere near the decal box; this is a loose, cheap
    ;; pre-filter meant to skip most of the model, the precise clip happens below
    (dotimes (mesh-index (model-mesh-count model))
      (let* ((mesh (aref (model-meshes model) mesh-index))
             (mv (mesh-vertices mesh))
             (indices (mesh-indices mesh)))
        (dotimes (tri (mesh-triangle-count mesh))
          (let ((vertices (make-array 3)))
            (if (null indices)
                (dotimes (v 3)
                  (setf (aref vertices v) (vec3 (aref mv (+ (* 3 3 tri) (* 3 v) 0))
                                                (aref mv (+ (* 3 3 tri) (* 3 v) 1))
                                                (aref mv (+ (* 3 3 tri) (* 3 v) 2)))))
                (dotimes (v 3)
                  (let ((idx (aref indices (+ (* 3 tri) v))))
                    (setf (aref vertices v) (vec3 (aref mv (+ (* 3 idx) 0))
                                                  (aref mv (+ (* 3 idx) 1))
                                                  (aref mv (+ (* 3 idx) 2)))))))

            (let ((inside-count 0))
              (dotimes (i 3)
                (let ((v (vector3-transform (aref vertices i) projection)))
                  (when (or (< (abs (vx v)) decal-size) (<= (abs (vy v)) decal-size) (<= (abs (vz v)) decal-size)) (incf inside-count))
                  (setf (aref vertices i) v)))

              (when (> inside-count 0)
                (add-triangle-to-mesh-builder (aref mesh-builders mb-index) (aref vertices 0) (aref vertices 1) (aref vertices 2))))))))

    ;; Clip the surviving triangles against each of the decal box's 6 faces in turn
    (let ((planes (vector (vec3 1.0 0.0 0.0) (vec3 -1.0 0.0 0.0)
                          (vec3 0.0 1.0 0.0) (vec3 0.0 -1.0 0.0)
                          (vec3 0.0 0.0 1.0) (vec3 0.0 0.0 -1.0))))

      (dotimes (face 6)
        (setf mb-index (- 1 mb-index))

        (let ((in-mesh (aref mesh-builders (- 1 mb-index)))
              (out-mesh (aref mesh-builders mb-index))
              (s (* 0.5 decal-size))
              (plane (aref planes face)))

          (setf (mesh-builder-vertex-count out-mesh) 0)

          (loop for i from 0 below (mesh-builder-vertex-count in-mesh) by 3
                do (let* ((in (mesh-builder-vertices in-mesh))
                          (d1 (- (vector3-dot-product (aref in (+ i 0)) plane) s))
                          (d2 (- (vector3-dot-product (aref in (+ i 1)) plane) s))
                          (d3 (- (vector3-dot-product (aref in (+ i 2)) plane) s))

                          (v1-out (if (> d1 0) 1 0))
                          (v2-out (if (> d2 0) 1 0))
                          (v3-out (if (> d3 0) 1 0))

                          (total (+ v1-out v2-out v3-out))
                          (nv1 nil) (nv2 nil) (nv3 nil) (nv4 nil))
                     (flet ((add (a b c) (add-triangle-to-mesh-builder out-mesh a b c)))
                       (case total
                         (0
                          ;; Whole triangle is inside this face, keep it as-is
                          (add (aref in i) (aref in (+ i 1)) (aref in (+ i 2))))
                         (1
                          ;; One corner is outside; clip it off, turning the triangle into a quad
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
                          ;; Two corners are outside; only the small corner near the surviving vertex remains
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
                         ;; All 3 corners are outside this face, the triangle is fully clipped away
                         (t nil))))))))

    (let ((final-mesh (aref mesh-builders mb-index))
          (decal-mesh (make-mesh)))
      (when (> (mesh-builder-vertex-count final-mesh) 0)
        (let ((vertices (mesh-builder-vertices final-mesh)))
          (setf (mesh-builder-uvs final-mesh) (make-array (mesh-builder-vertex-count final-mesh)))
          (dotimes (i (mesh-builder-vertex-count final-mesh))
            (let ((v (vcopy (aref vertices i))))
              ;; Clipped coordinates run roughly (-decalSize/2 .. decalSize/2); remap to (0..1)
              (setf (aref (mesh-builder-uvs final-mesh) i) (vec2 (+ (/ (vx v) decal-size) 0.5)
                                                                 (+ (/ (vy v) decal-size) 0.5)))
              ;; Nudge slightly along the normal so the decal doesn't z-fight with the surface
              (decf (vz v) decal-offset)
              (setf (aref vertices i) (vector3-transform v inv-proj)))))

        (setf decal-mesh (build-mesh final-mesh)))

      (free-mesh-builder (aref mesh-builders 0))
      (free-mesh-builder (aref mesh-builders 1))

      decal-mesh)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [models] example - procedural decals")

    (let* ((camera (make-camera3d :position (vec3 0.0 2.5 5.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; Target model: a procedurally generated knot with a checkerboard skin
           (model (load-model-from-mesh (gen-mesh-torus 0.8 1.8 32 64)))
           (model-texture (let ((checker-image (gen-image-checked 512 512 32 32 +lightgray+ +gray+)))
                            (prog1 (load-texture-from-image checker-image)
                              (unload-image checker-image))))
           (model-bbox nil)
           (model-size 0.0)
           (decal-size 0.0)
           (decal-offset 0.01)
           (placement-cube nil)
           (decal-texture nil)
           (decal-material nil)
           (show-model t)
           (decals (make-array +max-decals+ :initial-element nil))
           (decal-count 0))

      (set-texture-filter model-texture +texture-filter-bilinear+)
      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) model-texture)

      (setf model-bbox (get-mesh-bounding-box (aref (model-meshes model) 0)))
      (setf (camera3d-target camera) (vector3-lerp (bounding-box-min model-bbox) (bounding-box-max model-bbox) 0.5))
      (setf model-size (min (min (abs (- (vx (bounding-box-max model-bbox)) (vx (bounding-box-min model-bbox))))
                                 (abs (- (vy (bounding-box-max model-bbox)) (vy (bounding-box-min model-bbox)))))
                            (abs (- (vz (bounding-box-max model-bbox)) (vz (bounding-box-min model-bbox))))))
      (setf decal-size (* model-size 0.35))

      ;; Translucent cube previewing where the next decal will land
      (setf placement-cube (load-model-from-mesh (gen-mesh-cube decal-size decal-size decal-size)))
      (setf (material-map-color (aref (material-maps (aref (model-materials placement-cube) 0)) 0)) +lime+)

      ;; Decal texture: a procedurally drawn target/bullseye, no image file needed
      (let ((decal-image (gen-image-color 128 128 +blank+)))
        (image-draw-circle decal-image 64 64 60 +red+)
        (image-draw-circle decal-image 64 64 45 +white+)
        (image-draw-circle decal-image 64 64 30 +red+)
        (image-draw-circle decal-image 64 64 15 +yellow+)
        (setf decal-texture (load-texture-from-image decal-image))
        (unload-image decal-image))
      (set-texture-filter decal-texture +texture-filter-bilinear+)

      (setf decal-material (load-material-default))
      (setf (material-map-texture (aref (material-maps decal-material) +material-map-diffuse+)) decal-texture
            (material-map-color (aref (material-maps decal-material) +material-map-diffuse+)) +raywhite+)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-mouse-button-down +mouse-button-right+) (update-camera camera +camera-third-person+))

               ;; Cast a ray from the mouse and keep the closest point it hits on the model
               (let ((ray (get-screen-to-world-ray (get-mouse-position) camera))
                     (collision (make-ray-collision)))

                 (when (and (ray-collision-hit (get-ray-collision-box ray model-bbox)) (< decal-count +max-decals+))
                   (dotimes (m (model-mesh-count model))
                     (let ((mesh-hit (get-ray-collision-mesh ray (aref (model-meshes model) m) (model-transform model))))
                       (when (and (ray-collision-hit mesh-hit) (or (not (ray-collision-hit collision)) (< (ray-collision-distance mesh-hit) (ray-collision-distance collision))))
                         (setf collision mesh-hit)))))

                 ;; Project a new decal at the hit point, facing along the surface normal
                 (when (and (ray-collision-hit collision) (is-mouse-button-pressed +mouse-button-left+) (< decal-count +max-decals+))
                   (let* ((look-target (vector3-add (ray-collision-point collision) (ray-collision-normal collision)))
                          (splat (matrix-look-at (ray-collision-point collision) look-target (vec3 0.0 1.0 0.0))))
                     (setf splat (matrix-multiply splat (matrix-rotate-z (* +deg2rad+ (float (get-random-value -180 180))))))

                     (let ((decal-mesh (gen-mesh-decal model splat decal-size decal-offset)))
                       (when (> (mesh-vertex-count decal-mesh) 0)
                         (setf (aref decals decal-count) (load-model-from-mesh decal-mesh))
                         (setf (aref (material-maps (aref (model-materials (aref decals decal-count)) 0)) 0)
                               (aref (material-maps decal-material) 0))
                         (incf decal-count)))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-3d camera)

                 (when show-model (draw-model model (vector3-zero) 1.0 +white+))
                 (dotimes (i decal-count) (draw-model (aref decals i) (vector3-zero) 1.0 +white+))

                 ;; Preview cube at the surface point currently under the mouse
                 (when (ray-collision-hit collision)
                   (let* ((look-target (vector3-add (ray-collision-point collision) (ray-collision-normal collision)))
                          (splat (matrix-look-at (ray-collision-point collision) look-target (vec3 0.0 1.0 0.0))))
                     (setf (model-transform placement-cube) (matrix-invert splat))
                     (draw-model placement-cube (vector3-zero) 1.0 (fade +white+ 0.5))))

                 (draw-grid 10 1.0)

                 (end-mode-3d)

                 ;; Vertex/triangle counts: base model vs total once decal geometry is added
                 (let ((base-vertices 0) (base-triangles 0))
                   (dotimes (i (model-mesh-count model))
                     (incf base-vertices (mesh-vertex-count (aref (model-meshes model) i)))
                     (incf base-triangles (mesh-triangle-count (aref (model-meshes model) i))))

                   (let ((total-vertices base-vertices) (total-triangles base-triangles)
                         (stat-x (- screen-width 280)))
                     (dotimes (i decal-count)
                       (incf total-vertices (mesh-vertex-count (aref (model-meshes (aref decals i)) 0)))
                       (incf total-triangles (mesh-triangle-count (aref (model-meshes (aref decals i)) 0))))

                     (draw-text "Vertices" (+ stat-x 90) 10 10 +lime+)
                     (draw-text "Triangles" (+ stat-x 180) 10 10 +lime+)

                     (draw-text "Base Model" stat-x 25 10 +lime+)
                     (draw-text (text-format "%d" base-vertices) (+ stat-x 90) 25 10 +lime+)
                     (draw-text (text-format "%d" base-triangles) (+ stat-x 180) 25 10 +lime+)

                     (draw-text "TOTAL" stat-x 40 10 +lime+)
                     (draw-text (text-format "%d" total-vertices) (+ stat-x 90) 40 10 +lime+)
                     (draw-text (text-format "%d" total-triangles) (+ stat-x 180) 40 10 +lime+)))

                 (when (/= (gui-button (make-rectangle :x 10.0 :y (- screen-height 80.0) :width 100.0 :height 40.0) (if show-model "Hide Model" "Show Model")) 0)
                   (setf show-model (not show-model)))

                 (when (/= (gui-button (make-rectangle :x 120.0 :y (- screen-height 80.0) :width 100.0 :height 40.0) "Clear Decals") 0)
                   (dotimes (i decal-count) (unload-model (aref decals i)))
                   (setf decal-count 0))

                 (draw-text "Left click: place decal   |   Right drag: orbit camera" 10 (- screen-height 25) 10 +darkgray+)

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (dotimes (i decal-count) (unload-model (aref decals i)))
      (unload-model placement-cube)
      (unload-texture decal-texture)
      (unload-texture model-texture)
      (unload-model model)

      (close-window))))                 ; Close window and OpenGL context

(main)
