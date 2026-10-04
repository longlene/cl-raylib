;;;; raylib [shaders] example - simple mask
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
;;;;
;;;; *******************************************************************************************
;;;;
;;;; After a model is loaded it has a default material, this material can be
;;;; modified in place rather than creating one from scratch...
;;;; While all of the maps have particular names, they can be used for any purpose
;;;; except for three maps that are applied as cubic maps (see below)
;;;; Common Lisp port of raylib/examples/shaders/shaders_simple_mask.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-simple-mask
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-simple-mask)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - simple mask")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 0.0 1.0 2.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Define our three models to show the shader on
           (torus (gen-mesh-torus 0.3 1 16 32))
           (model1 (load-model-from-mesh torus))

           (cube (gen-mesh-cube 0.8 0.8 0.8))
           (model2 (load-model-from-mesh cube))

           ;; Generate model to be shaded just to see the gaps in the other two
           (sphere (gen-mesh-sphere 1 16 16))
           (model3 (load-model-from-mesh sphere))

           ;; Load the shader
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/mask.fs" +glsl-version+)))

           ;; Load and apply the diffuse texture (colour map)
           (tex-diffuse (load-texture "resources/plasma.png"))

           ;; Using MATERIAL_MAP_EMISSION as a spare slot to use for 2nd texture
           ;; NOTE: Don't use MATERIAL_MAP_IRRADIANCE, MATERIAL_MAP_PREFILTER or  MATERIAL_MAP_CUBEMAP as they are bound as cube maps
           (tex-mask (load-texture "resources/mask.png"))
           (shader-frame 0)

           (frames-counter 0)
           (rotation (vec3 0.0 0.0 0.0))) ; Model rotation angles

      (flet ((map-of (model index) (aref (material-maps (aref (model-materials model) 0)) index)))
        (setf (material-map-texture (map-of model1 +material-map-diffuse+)) tex-diffuse
              (material-map-texture (map-of model2 +material-map-diffuse+)) tex-diffuse)

        (setf (material-map-texture (map-of model1 +material-map-emission+)) tex-mask
              (material-map-texture (map-of model2 +material-map-emission+)) tex-mask))
      (setf (aref (shader-locs shader) +shader-loc-map-emission+) (get-shader-location shader "mask"))

      ;; Frame is incremented each frame to animate the shader
      (setf shader-frame (get-shader-location shader "frame"))

      ;; Apply the shader to the two models
      (setf (material-shader (aref (model-materials model1) 0)) shader
            (material-shader (aref (model-materials model2) 0)) shader)

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set  to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-first-person+)

               (incf frames-counter)
               (incf (vx rotation) 0.01)
               (incf (vy rotation) 0.005)
               (decf (vz rotation) 0.0025)

               ;; Send frames counter to shader for animation
               (set-shader-value shader shader-frame frames-counter +shader-uniform-int+)

               ;; Rotate one of the models
               (setf (model-transform model1) (matrix-rotate-xyz rotation))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +darkblue+)

               (begin-mode-3d camera)

               (draw-model model1 (vec3 0.5 0.0 0.0) 1 +white+)
               (draw-model-ex model2 (vec3 -0.5 0.0 0.0) (vec3 1.0 1.0 0.0) 50 (vec3 1.0 1.0 1.0) +white+)
               (draw-model model3 (vec3 0.0 0.0 -1.5) 1 +white+)
               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-rectangle 16 698 (+ (measure-text (text-format "Frame: %i" frames-counter) 20) 8) 42 +blue+)
               (draw-text (text-format "Frame: %i" frames-counter) 20 700 20 +white+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model1)
      (unload-model model2)
      (unload-model model3)

      (unload-texture tex-diffuse)      ; Unload default diffuse texture
      (unload-texture tex-mask)         ; Unload texture mask

      (unload-shader shader)            ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)
