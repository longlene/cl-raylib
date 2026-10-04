;;;; raylib [models] example - loading
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: raylib supports multiple models file formats:
;;;;
;;;;   - OBJ  > Text file format. Must include vertex position-texcoords-normals information,
;;;;            if .obj references some .mtl materials file, it will be tried to be loaded
;;;;   - GLTF/GLB > Text/binary file formats. Includes lot of information and it could
;;;;            also reference external files, mesh and materials data will be tried to be loaded
;;;;   - IQM  > Binary file format. Includes mesh vertex data but also animation data,
;;;;            meshes and animation data can be loaded
;;;;   - VOX  > Binary file format. MagikaVoxel mesh format:
;;;;            https://github.com/ephtracy/voxel-model/blob/master/MagicaVoxel-file-format-vox.txt
;;;;   - M3D  > Binary file format. Model 3D format:
;;;;            https://bztsrc.gitlab.io/model3d
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_loading.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-loading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-loading)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - loading")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 50.0 50.0 50.0) ; Camera position
                                  :target (vec3 0.0 12.0 0.0)     ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 45.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera mode type

           (model (load-model "resources/models/obj/castle.obj"))                 ; Load model
           (texture (load-texture "resources/models/obj/castle_diffuse.png"))     ; Load model texture

           (position (vec3 0.0 0.0 0.0))                                  ; Set model position
           (bounds (get-mesh-bounding-box (aref (model-meshes model) 0))) ; Set model bounds

           ;; NOTE: bounds are calculated from the original size of the model,
           ;; if model is scaled on drawing, bounds must be also scaled

           (selected nil))              ; Selected object flag

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set map diffuse texture

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Load new models/textures on drag&drop
               (when (is-file-dropped)
                 (let ((dropped-files (load-dropped-files)))

                   (when (= (file-path-list-count dropped-files) 1) ; Only support one file dropped
                     (let ((path (aref (file-path-list-paths dropped-files) 0)))
                       (cond ((or (is-file-extension path ".obj")
                                  (is-file-extension path ".gltf")
                                  (is-file-extension path ".glb")
                                  (is-file-extension path ".vox")
                                  (is-file-extension path ".iqm")
                                  (is-file-extension path ".m3d")) ; Model file formats supported
                              (unload-model model)                ; Unload previous model
                              (setf model (load-model path))      ; Load new model
                              (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set current map diffuse texture

                              (setf bounds (get-mesh-bounding-box (aref (model-meshes model) 0)))

                              ;; Move camera position from target enough distance to visualize model properly
                              (setf (vx (camera3d-position camera)) (+ (vx (bounding-box-max bounds)) 10.0)
                                    (vy (camera3d-position camera)) (+ (vy (bounding-box-max bounds)) 10.0)
                                    (vz (camera3d-position camera)) (+ (vz (bounding-box-max bounds)) 10.0)))
                             ((is-file-extension path ".png") ; Texture file formats supported
                              ;; Unload current model texture and load new one
                              (unload-texture texture)
                              (setf texture (load-texture path))
                              (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture)))))

                   (unload-dropped-files dropped-files))) ; Unload filepaths from memory

               ;; Select model on mouse click
               (when (is-mouse-button-pressed +mouse-button-left+)
                 ;; Check collision between ray and box
                 (if (ray-collision-hit (get-ray-collision-box (get-screen-to-world-ray (get-mouse-position) camera) bounds))
                     (setf selected (not selected))
                     (setf selected nil)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-model model position 1.0 +white+) ; Draw 3d model with texture

               (draw-grid 20 10.0)       ; Draw a grid

               (when selected (draw-bounding-box bounds +green+)) ; Draw selection box

               (end-mode-3d)

               (draw-text "Drag & drop model to load mesh/texture." 10 (- (get-screen-height) 20) 10 +darkgray+)
               (when selected (draw-text "MODEL SELECTED" (- (get-screen-width) 110) 10 10 +green+))

               (draw-text "(c) Castle 3D model by Alberto Cano" (- screen-width 200) (- screen-height 20) 10 +gray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model

      (close-window))))                 ; Close window and OpenGL context

(main)
