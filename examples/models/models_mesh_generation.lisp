;;;; raylib [models] example - mesh generation
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_mesh_generation.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-mesh-generation
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-mesh-generation)

(defconstant +num-models+ 9)            ; Parametric 3d shapes to generate

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Generate a simple triangle mesh from code
(defun gen-mesh-custom ()
  (let ((mesh (make-mesh)))
    (setf (mesh-triangle-count mesh) 1)
    (setf (mesh-vertex-count mesh) (* (mesh-triangle-count mesh) 3))
    (setf (mesh-vertices mesh) (make-array (* (mesh-vertex-count mesh) 3) :element-type 'single-float :initial-element 0.0)) ; 3 vertices, 3 coordinates each (x, y, z)
    (setf (mesh-texcoords mesh) (make-array (* (mesh-vertex-count mesh) 2) :element-type 'single-float :initial-element 0.0)) ; 3 vertices, 2 coordinates each (x, y)
    (setf (mesh-normals mesh) (make-array (* (mesh-vertex-count mesh) 3) :element-type 'single-float :initial-element 0.0)) ; 3 vertices, 3 coordinates each (x, y, z)

    (let ((vertices (mesh-vertices mesh))
          (normals (mesh-normals mesh))
          (texcoords (mesh-texcoords mesh)))
      ;; Vertex at (0, 0, 0)
      (setf (aref vertices 0) 0.0
            (aref vertices 1) 0.0
            (aref vertices 2) 0.0)
      (setf (aref normals 0) 0.0
            (aref normals 1) 1.0
            (aref normals 2) 0.0)
      (setf (aref texcoords 0) 0.0
            (aref texcoords 1) 0.0)

      ;; Vertex at (1, 0, 2)
      (setf (aref vertices 3) 1.0
            (aref vertices 4) 0.0
            (aref vertices 5) 2.0)
      (setf (aref normals 3) 0.0
            (aref normals 4) 1.0
            (aref normals 5) 0.0)
      (setf (aref texcoords 2) 0.5
            (aref texcoords 3) 1.0)

      ;; Vertex at (2, 0, 0)
      (setf (aref vertices 6) 2.0
            (aref vertices 7) 0.0
            (aref vertices 8) 0.0)
      (setf (aref normals 6) 0.0
            (aref normals 7) 1.0
            (aref normals 8) 0.0)
      (setf (aref texcoords 4) 1.0
            (aref texcoords 5) 0.0))

    ;; Upload mesh data from CPU (RAM) to GPU (VRAM) memory
    (upload-mesh mesh nil)

    mesh))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - mesh generation")

    ;; We generate a checked image for texturing
    (let* ((checked (gen-image-checked 2 2 1 1 +red+ +green+))
           (texture (load-texture-from-image checked))
           (models (make-array +num-models+)))
      (unload-image checked)

      (setf (aref models 0) (load-model-from-mesh (gen-mesh-plane 2 2 4 3))
            (aref models 1) (load-model-from-mesh (gen-mesh-cube 2.0 1.0 2.0))
            (aref models 2) (load-model-from-mesh (gen-mesh-sphere 2 32 32))
            (aref models 3) (load-model-from-mesh (gen-mesh-hemi-sphere 2 16 16))
            (aref models 4) (load-model-from-mesh (gen-mesh-cylinder 1 2 16))
            (aref models 5) (load-model-from-mesh (gen-mesh-torus 0.25 4.0 16 32))
            (aref models 6) (load-model-from-mesh (gen-mesh-knot 1.0 2.0 16 128))
            (aref models 7) (load-model-from-mesh (gen-mesh-poly 5 2.0))
            (aref models 8) (load-model-from-mesh (gen-mesh-custom)))

      ;; NOTE: Generated meshes could be exported using ExportMesh()

      ;; Set checked texture as default diffuse component for all models material
      (dotimes (i +num-models+)
        (setf (material-map-texture (aref (material-maps (aref (model-materials (aref models i)) 0)) +material-map-diffuse+)) texture))

      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec3 5.0 5.0 5.0) :target (vec3 0.0 0.0 0.0) :up (vec3 0.0 1.0 0.0) :fovy 45.0 :projection 0))

            ;; Model drawing position
            (position (vec3 0.0 0.0 0.0))

            (current-model 0))

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (update-camera camera +camera-orbital+)

                 (when (is-mouse-button-pressed +mouse-button-left+)
                   (setf current-model (mod (1+ current-model) +num-models+))) ; Cycle between the textures

                 (cond ((is-key-pressed +key-right+)
                        (incf current-model)
                        (when (>= current-model +num-models+) (setf current-model 0)))
                       ((is-key-pressed +key-left+)
                        (decf current-model)
                        (when (< current-model 0) (setf current-model (1- +num-models+)))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-3d camera)

                 (draw-model (aref models current-model) position 1.0 +white+)
                 (draw-grid 10 1.0)

                 (end-mode-3d)

                 (draw-rectangle 30 400 310 30 (fade +skyblue+ 0.5))
                 (draw-rectangle-lines 30 400 310 30 (fade +darkblue+ 0.5))
                 (draw-text "MOUSE LEFT BUTTON to CYCLE PROCEDURAL MODELS" 40 410 10 +blue+)

                 (case current-model
                   (0 (draw-text "PLANE" 680 10 20 +darkblue+))
                   (1 (draw-text "CUBE" 680 10 20 +darkblue+))
                   (2 (draw-text "SPHERE" 680 10 20 +darkblue+))
                   (3 (draw-text "HEMISPHERE" 640 10 20 +darkblue+))
                   (4 (draw-text "CYLINDER" 680 10 20 +darkblue+))
                   (5 (draw-text "TORUS" 680 10 20 +darkblue+))
                   (6 (draw-text "KNOT" 680 10 20 +darkblue+))
                   (7 (draw-text "POLY" 680 10 20 +darkblue+))
                   (8 (draw-text "Custom (triangle)" 580 10 20 +darkblue+)))

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture texture)        ; Unload texture

        ;; Unload models data (GPU VRAM)
        (dotimes (i +num-models+) (unload-model (aref models i)))

        (close-window)))))              ; Close window and OpenGL context

(main)
