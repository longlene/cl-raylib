;;;; raylib [shaders] example - mesh instancing
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 3.7, last time updated with raylib 4.2
;;;;
;;;; Example contributed by seanpringle (@seanpringle) and reviewed by Max (@moliad) and Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 seanpringle (@seanpringle), Max (@moliad) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_mesh_instancing.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/shaders-mesh-instancing
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/shaders-mesh-instancing)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-instances+ 10000)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - mesh instancing")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 -125.0 125.0 -125.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)           ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)               ; Camera up vector (rotation towards target)
                                  :fovy 45.0                           ; Camera field-of-view Y
                                  :projection +camera-perspective+))   ; Camera projection type

           ;; Define mesh to be instanced
           (cube (gen-mesh-cube 1.0 1.0 1.0))

           ;; Define transforms to be uploaded to GPU for instances
           (transforms (make-array +max-instances+)) ; Pre-multiplied transformations passed to rlgl
           (shader nil)
           (ambient-loc 0)
           (mat-instances nil)
           (mat-default nil))

      ;; Translate and rotate cubes randomly
      (dotimes (i +max-instances+)
        ;; NOTE: gcc evaluates the MatrixTranslate() arguments right to left (unspecified order in C),
        ;; and the Vector3 initializer left to right, the random values are drawn in that order
        (let* ((tz (float (get-random-value -50 50)))
               (ty (float (get-random-value -50 50)))
               (tx (float (get-random-value -50 50)))
               (translation (matrix-translate tx ty tz))
               (ax (float (get-random-value 0 360)))
               (ay (float (get-random-value 0 360)))
               (az (float (get-random-value 0 360)))
               (axis (vector3-normalize (vec3 ax ay az)))
               (angle (* (float (get-random-value 0 180)) +deg2rad+))
               (rotation (matrix-rotate axis angle)))
          (setf (aref transforms i) (matrix-multiply rotation translation))))

      ;; Load lighting shader
      (setf shader (load-shader (text-format "resources/shaders/glsl%i/lighting_instancing.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/lighting.fs" +glsl-version+)))
      ;; Get shader locations
      (setf (aref (shader-locs shader) +shader-loc-matrix-mvp+) (get-shader-location shader "mvp")
            (aref (shader-locs shader) +shader-loc-vector-view+) (get-shader-location shader "viewPos"))

      ;; Set shader value: ambient light level
      (setf ambient-loc (get-shader-location shader "ambient"))
      (set-shader-value shader ambient-loc '(0.2 0.2 0.2 1.0) +shader-uniform-vec4+)

      ;; Create one light
      (create-light +light-directional+ (vec3 50.0 50.0 0.0) (vector3-zero) +white+ shader)

      ;; NOTE: We are assigning the intancing shader to material.shader
      ;; to be used on mesh drawing with DrawMeshInstanced()
      (setf mat-instances (load-material-default))
      (setf (material-shader mat-instances) shader)
      (setf (material-map-color (aref (material-maps mat-instances) +material-map-diffuse+)) +red+)

      ;; Load default material (using raylib intenral default shader) for non-instanced mesh drawing
      ;; WARNING: Default shader enables vertex color attribute BUT GenMeshCube() does not generate vertex colors, so,
      ;; when drawing the color attribute is disabled and a default color value is provided as input for thevertex attribute
      (setf mat-default (load-material-default))
      (setf (material-map-color (aref (material-maps mat-default) +material-map-diffuse+)) +blue+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update the light shader with the camera view position
               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value shader (aref (shader-locs shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               ;; Draw cube mesh with default material (BLUE)
               (draw-mesh cube mat-default (matrix-translate -10.0 0.0 0.0))

               ;; Draw meshes instanced using material containing instancing shader (RED + lighting),
               ;; transforms[] for the instances should be provided, they are dynamically
               ;; updated in GPU every frame, so we can animate the different mesh instances
               (draw-mesh-instanced cube mat-instances transforms +max-instances+)

               ;; Draw cube mesh with default material (BLUE)
               (draw-mesh cube mat-default (matrix-translate 10.0 0.0 0.0))

               (end-mode-3d)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
