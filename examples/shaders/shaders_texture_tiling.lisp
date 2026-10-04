;;;; raylib [shaders] example - texture tiling
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example demonstrates how to tile a texture on a 3D model using raylib
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Luis Almeida (@luis605) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Luis Almeida (@luis605)
;;;; Common Lisp port of raylib/examples/shaders/shaders_texture_tiling.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-texture-tiling
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-texture-tiling)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - texture tiling")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 4.0 4.0 4.0) ; Camera position
                                  :target (vec3 0.0 0.5 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load a cube model
           (cube (gen-mesh-cube 1.0 1.0 1.0))
           (model (load-model-from-mesh cube))

           ;; Load a texture and assign to cube model
           (texture (load-texture "resources/cubicmap_atlas.png"))

           ;; Set the texture tiling using a shader
           (tiling '(3.0 3.0))
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/tiling.fs" +glsl-version+))))

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture)

      (set-texture-wrap texture +texture-wrap-repeat+)
      (set-shader-value shader (get-shader-location shader "tiling") tiling +shader-uniform-vec2+)
      (setf (material-shader (aref (model-materials model) 0)) shader)

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+)

               (when (is-key-pressed +key-z+) (setf (camera3d-target camera) (vec3 0.0 0.5 0.0)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (begin-shader-mode shader)
               (draw-model model (vec3 0.0 0.0 0.0) 2.0 +white+)
               (end-shader-mode)

               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-text "Use mouse to rotate the camera" 10 10 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)              ; Unload model
      (unload-shader shader)            ; Unload shader
      (unload-texture texture)          ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
