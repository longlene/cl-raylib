;;;; raylib [shaders] example - fog rendering
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_fog_rendering.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/shaders-fog-rendering
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/shaders-fog-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; Enable Multi Sampling Anti Aliasing 4x (if available)
    (init-window screen-width screen-height "raylib [shaders] example - fog rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 2.0 2.0 6.0) ; Camera position
                                  :target (vec3 0.0 0.5 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load models and texture
           (model-a (load-model-from-mesh (gen-mesh-torus 0.4 1.0 16 32)))
           (model-b (load-model-from-mesh (gen-mesh-cube 1.0 1.0 1.0)))
           (model-c (load-model-from-mesh (gen-mesh-sphere 0.5 32 32)))
           (texture (load-texture "resources/texel_checker.png"))

           ;; Load shader and set up some uniforms
           (shader (load-shader (text-format "resources/shaders/glsl%i/lighting.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/fog.fs" +glsl-version+)))

           ;; Ambient light level
           (ambient (vec4 0.2 0.2 0.2 1.0))
           (ambient-loc (get-shader-location shader "ambient"))

           (fog-color (color-normalize +gray+))
           (fog-color-loc (get-shader-location shader "fogColor"))

           (fog-density 0.15)
           (fog-density-loc (get-shader-location shader "fogDensity")))

      ;; Assign texture to default model material
      (dolist (model (list model-a model-b model-c))
        (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture))

      (setf (aref (shader-locs shader) +shader-loc-matrix-model+) (get-shader-location shader "matModel")
            (aref (shader-locs shader) +shader-loc-vector-view+) (get-shader-location shader "viewPos"))

      (set-shader-value shader ambient-loc ambient +shader-uniform-vec4+)
      (set-shader-value shader fog-color-loc fog-color +shader-uniform-vec4+)
      (set-shader-value shader fog-density-loc fog-density +shader-uniform-float+)

      ;; NOTE: All models share the same shader
      (dolist (model (list model-a model-b model-c))
        (setf (material-shader (aref (model-materials model) 0)) shader))

      ;; Using just 1 point lights
      (create-light +light-point+ (vec3 0.0 2.0 6.0) (vector3-zero) +white+ shader)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (when (is-key-down +key-up+)
                 (incf fog-density 0.001)
                 (when (> fog-density 1.0) (setf fog-density 1.0)))

               (when (is-key-down +key-down+)
                 (decf fog-density 0.001)
                 (when (< fog-density 0.0) (setf fog-density 0.0)))

               (set-shader-value shader fog-density-loc fog-density +shader-uniform-float+)

               ;; Rotate the torus
               (setf (model-transform model-a) (matrix-multiply (model-transform model-a) (matrix-rotate-x -0.025)))
               (setf (model-transform model-a) (matrix-multiply (model-transform model-a) (matrix-rotate-z 0.012)))

               ;; Update the light shader with the camera view position
               (set-shader-value shader (aref (shader-locs shader) +shader-loc-vector-view+) (camera3d-position camera) +shader-uniform-vec3+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +gray+)

               (begin-mode-3d camera)

               ;; Draw the three models
               (draw-model model-a (vector3-zero) 1.0 +white+)
               (draw-model model-b (vec3 -2.6 0.0 0.0) 1.0 +white+)
               (draw-model model-c (vec3 2.6 0.0 0.0) 1.0 +white+)

               (loop for i from -20 below 20 by 2 do (draw-model model-a (vec3 (float i) 0.0 2.0) 1.0 +white+))

               (end-mode-3d)

               (draw-text (text-format "Use KEY_UP/KEY_DOWN to change fog density [%.2f]" fog-density) 10 10 20 +raywhite+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model-a)            ; Unload the model A
      (unload-model model-b)            ; Unload the model B
      (unload-model model-c)            ; Unload the model C
      (unload-texture texture)          ; Unload the texture
      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)
