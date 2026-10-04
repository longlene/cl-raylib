;;;; raylib [models] example - mesh uv painting
;;;;
;;;; Example demonstrates painting directly onto a mesh's texture: a ray is cast from the
;;;; mouse into the scene, the hit triangle is found, and the hit point is converted into
;;;; UV space using barycentric interpolation so a brush stroke can be drawn on the texture
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
;;;; Common Lisp port of raylib/examples/models/models_mesh_uv_painting.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-mesh-uv-painting
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/models-mesh-uv-painting)

;;----------------------------------------------------------------------------------
;; Global Definitions
;;----------------------------------------------------------------------------------
(defconstant +canvas-size+ 512)
(defconstant +palette-count+ 8)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Tool mode
(defconstant +tool-paint+ 0)
(defconstant +tool-picker+ 1)

;; Shape type
(defconstant +shape-sphere+ 0)
(defconstant +shape-cube+ 1)
(defconstant +shape-cylinder+ 2)
(defconstant +shape-torus+ 3)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Unloads the current model and builds a new primitive shape in its place, sharing the
;; same canvas texture, and recomputes the transformed bounding box used for ray testing
;; NOTE: C updates model, bbox and currentShape through pointers, here they are returned
;; as (values model bbox current-shape)
(defun change-shape (model new-shape canvas-texture)
  (when model (unload-model model))

  (let* ((mesh (case new-shape
                 (#.+shape-sphere+ (gen-mesh-sphere 1.5 32 32))
                 (#.+shape-cube+ (gen-mesh-cube 2.2 2.2 2.2))
                 (#.+shape-cylinder+ (gen-mesh-cylinder 1.2 2.5 24))
                 (#.+shape-torus+ (gen-mesh-torus 0.6 1.6 24 36))
                 (t (gen-mesh-sphere 1.5 32 32))))
         (model (load-model-from-mesh mesh)))

    ;; GenMeshCylinder builds upward from y = 0; shift it down to center on the origin
    (when (= new-shape +shape-cylinder+) (setf (model-transform model) (matrix-translate 0.0 -1.25 0.0)))

    (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) canvas-texture)

    (let ((box (get-mesh-bounding-box (aref (model-meshes model) 0))))
      (setf (bounding-box-min box) (vector3-transform (bounding-box-min box) (model-transform model))
            (bounding-box-max box) (vector3-transform (bounding-box-max box) (model-transform model)))

      (values model box new-shape))))

;; Raycasts against the model's mesh triangles and returns the UV coordinate of the
;; closest hit, interpolated from the hit triangle's vertices with barycentric weights
;; NOTE: Returns the hit UV (vec2) or NIL (C returns false and the UV through outUV)
(defun get-mesh-hit-uv (ray model bbox)
  (unless (ray-collision-hit (get-ray-collision-box ray bbox)) (return-from get-mesh-hit-uv nil)) ; Fast reject

  (let* ((mesh (aref (model-meshes model) 0))
         (vertices (mesh-vertices mesh))
         (texcoords (mesh-texcoords mesh))
         (indices (mesh-indices mesh))
         (closest-distance 1e9)
         (found nil)
         (hit-uv (vec2 0.0 0.0)))

    (dotimes (tri (mesh-triangle-count mesh))
      (multiple-value-bind (i0 i1 i2)
          (if indices
              (values (aref indices (+ (* 3 tri) 0)) (aref indices (+ (* 3 tri) 1)) (aref indices (+ (* 3 tri) 2)))
              (values (* 3 tri) (+ (* 3 tri) 1) (+ (* 3 tri) 2)))
        (flet ((vertex (i) (vector3-transform (vec3 (aref vertices (* 3 i)) (aref vertices (+ (* 3 i) 1)) (aref vertices (+ (* 3 i) 2))) (model-transform model)))
               (uv (i) (vec2 (aref texcoords (* 2 i)) (aref texcoords (+ (* 2 i) 1)))))
          (let* ((a (vertex i0))
                 (b (vertex i1))
                 (c (vertex i2))
                 (hit (get-ray-collision-triangle ray a b c)))

            (when (and (ray-collision-hit hit) (< (ray-collision-distance hit) closest-distance))
              (setf closest-distance (ray-collision-distance hit)
                    found t)

              (let ((uv-a (uv i0))
                    (uv-b (uv i1))
                    (uv-c (uv i2))
                    (w (vector3-barycenter (ray-collision-point hit) a b c)))
                (setf (vx hit-uv) (+ (* (vx w) (vx uv-a)) (* (vy w) (vx uv-b)) (* (vz w) (vx uv-c)))
                      (vy hit-uv) (+ (* (vx w) (vy uv-a)) (* (vy w) (vy uv-b)) (* (vz w) (vy uv-c))))

                ;; Wrap into [0, 1] in case of minor floating point drift at UV seams
                (decf (vx hit-uv) (ffloor (vx hit-uv)))
                (decf (vy hit-uv) (ffloor (vy hit-uv)))))))))

    (when found hit-uv)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - mesh uv painting")

    (let* ((camera (make-camera3d :position (vec3 0.0 3.5 6.5)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; CPU-side canvas plus the GPU texture it gets pushed to after every stroke
           (canvas-image (gen-image-color +canvas-size+ +canvas-size+ +raywhite+))
           (canvas-texture (load-texture-from-image canvas-image))

           (model nil)
           (model-bbox nil)
           (current-shape +shape-sphere+)

           (current-tool +tool-paint+)
           (active-color +red+)
           (brush-radius 12)
           (last-hit-uv (vec2 0.0 0.0))
           (has-last-hit nil)

           (palette (vector +red+ +orange+ +gold+ +lime+ +skyblue+ +purple+ +darkgray+ +white+))
           (ui-panel-rec (make-rectangle :x 10.0 :y 10.0 :width 230.0 :height 490.0)))

      (set-texture-filter canvas-texture +texture-filter-bilinear+)

      (multiple-value-setq (model model-bbox current-shape) (change-shape model +shape-sphere+ canvas-texture))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((mouse-pos (get-mouse-position))
                      (is-mouse-over-ui (check-collision-point-rec mouse-pos ui-panel-rec)))

                 (when (is-mouse-button-down +mouse-button-right+) (update-camera camera +camera-third-person+))

                 (if (and (not is-mouse-over-ui) (is-mouse-button-down +mouse-button-left+))
                     (let* ((ray (get-screen-to-world-ray mouse-pos camera))
                            (hit-uv (get-mesh-hit-uv ray model model-bbox)))
                       (if hit-uv
                           (let ((px (truncate (* (vx hit-uv) +canvas-size+)))
                                 (py (truncate (* (vy hit-uv) +canvas-size+))))
                             (if (= current-tool +tool-paint+)
                                 (progn
                                   ;; Stroke from the last hit to this one, guarding against jumps across a UV seam
                                   (if (and has-last-hit (< (abs (- (vx hit-uv) (vx last-hit-uv))) 0.25) (< (abs (- (vy hit-uv) (vy last-hit-uv))) 0.25))
                                       (let* ((last-px (truncate (* (vx last-hit-uv) +canvas-size+)))
                                              (last-py (truncate (* (vy last-hit-uv) +canvas-size+)))
                                              (dist (vector2-distance (vec2 (float last-px) (float last-py)) (vec2 (float px) (float py))))
                                              (steps (1+ (truncate (/ dist 2.0)))))
                                         (loop for i from 0 to steps
                                               do (let ((tt (/ (float i) (float steps))))
                                                    (image-draw-circle canvas-image (truncate (lerp (float last-px) (float px) tt))
                                                                       (truncate (lerp (float last-py) (float py) tt)) brush-radius active-color))))
                                       (image-draw-circle canvas-image px py brush-radius active-color))

                                   (update-texture canvas-texture (image-data canvas-image))
                                   (setf last-hit-uv hit-uv
                                         has-last-hit t))
                                 (setf active-color (get-image-color canvas-image px py)
                                       current-tool +tool-paint+
                                       has-last-hit nil)))
                           (setf has-last-hit nil)))
                     (setf has-last-hit nil))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background '(30 32 40 255))

                 (begin-mode-3d camera)
                 (draw-model model (vector3-zero) 1.0 +white+)
                 (draw-grid 10 1.0)
                 (end-mode-3d)

                 ;; Side panel
                 (draw-rectangle-rec ui-panel-rec (fade +black+ 0.8))
                 (draw-rectangle-lines-ex ui-panel-rec 2.0 +darkgray+)
                 (draw-text "MESH UV PAINTER" 25 22 20 +gold+)

                 ;; Tool selection toggles
                 (let ((paint-active (= current-tool +tool-paint+))
                       (picker-active (= current-tool +tool-picker+)))
                   (when (/= (gui-toggle (make-rectangle :x 25.0 :y 55.0 :width 95.0 :height 32.0) "PAINT" paint-active) 0) (setf current-tool +tool-paint+))
                   (when (/= (gui-toggle (make-rectangle :x 125.0 :y 55.0 :width 95.0 :height 32.0) "PICKER" picker-active) 0) (setf current-tool +tool-picker+)))

                 ;; Shape selection toggles
                 (draw-text "Mesh Shape:" 25 100 10 +lightgray+)
                 (flet ((shape-toggle (x y text shape)
                          (when (/= (gui-toggle (make-rectangle :x x :y y :width 95.0 :height 28.0) text (= current-shape shape)) 0)
                            (multiple-value-setq (model model-bbox current-shape) (change-shape model shape canvas-texture)))))
                   (shape-toggle 25.0 120.0 "SPHERE" +shape-sphere+)
                   (shape-toggle 125.0 120.0 "CUBE" +shape-cube+)
                   (shape-toggle 25.0 153.0 "CYLINDER" +shape-cylinder+)
                   (shape-toggle 125.0 153.0 "TORUS" +shape-torus+))

                 ;; Color display
                 (draw-text "Active Color:" 25 195 10 +lightgray+)
                 (draw-rectangle 125 193 95 20 active-color)
                 (draw-rectangle-lines 125 193 95 20 +white+)

                 ;; Color swatches
                 (draw-text "Palette Swatches:" 25 225 10 +lightgray+)
                 (dotimes (i +palette-count+)
                   (let ((swatch-rec (make-rectangle :x (+ 25.0 (* (mod i 4) 48)) :y (+ 245.0 (* (floor i 4) 45)) :width 40.0 :height 38.0)))
                     (draw-rectangle-rec swatch-rec (aref palette i))
                     (draw-rectangle-lines-ex swatch-rec 1.0 +white+)
                     (when (and (check-collision-point-rec mouse-pos swatch-rec) (is-mouse-button-pressed +mouse-button-left+)) (setf active-color (aref palette i)))))

                 ;; Brush size buttons
                 (draw-text (text-format "Brush Size: %dpx" brush-radius) 25 345 10 +lightgray+)
                 (when (and (/= (gui-button (make-rectangle :x 25.0 :y 365.0 :width 95.0 :height 30.0) "SIZE -") 0) (> brush-radius 2)) (decf brush-radius 2))
                 (when (and (/= (gui-button (make-rectangle :x 125.0 :y 365.0 :width 95.0 :height 30.0) "SIZE +") 0) (< brush-radius 64)) (incf brush-radius 2))

                 ;; Canvas action buttons
                 (when (/= (gui-button (make-rectangle :x 25.0 :y 410.0 :width 95.0 :height 30.0) "CLEAR") 0)
                   (image-clear-background canvas-image +raywhite+)
                   (update-texture canvas-texture (image-data canvas-image)))
                 (when (/= (gui-button (make-rectangle :x 125.0 :y 410.0 :width 95.0 :height 30.0) "FILL") 0)
                   (image-clear-background canvas-image active-color)
                   (update-texture canvas-texture (image-data canvas-image)))

                 (draw-text "Left click: paint / pick color   |   Right drag: orbit camera" 260 (- screen-height 25) 10 +raywhite+)

                 (draw-fps (- screen-width 90) 15)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)
      (unload-image canvas-image)
      (unload-texture canvas-texture)

      (close-window))))                 ; Close window and OpenGL context

(main)
