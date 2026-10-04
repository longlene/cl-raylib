;;;; raylib [shaders] example - model shader
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3), to test this example
;;;;       on OpenGL ES 2.0 platforms (Android, Raspberry Pi, HTML5), use #version 100 shaders
;;;;       raylib comes with shaders ready for both versions, check raylib/shaders install folder
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 3.7
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_model_shader.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-model-shader
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-model-shader)

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

    (init-window screen-width screen-height "raylib [shaders] example - model shader")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 4.0 4.0 4.0)  ; Camera position
                                 :target (vec3 0.0 1.0 -1.0)   ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 45.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          (model (load-model "resources/models/watermill.obj"))                  ; Load OBJ model
          (texture (load-texture "resources/models/watermill_diffuse.png"))      ; Load model texture

          ;; Load shader for model
          ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
          (shader (load-shader nil (text-format "resources/shaders/glsl%i/grayscale.fs" +glsl-version+)))

          (position (vec3 0.0 0.0 0.0))) ; Set model position

      (setf (material-shader (aref (model-materials model) 0)) shader) ; Set shader effect to 3d model
      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Bind texture to model

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-model model position 0.2 +white+) ; Draw 3d model with texture

               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-text "(c) Watermill 3D model by Alberto Cano" (- screen-width 210) (- screen-height 20) 10 +gray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model

      (close-window))))                 ; Close window and OpenGL context

(main)
