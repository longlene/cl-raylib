;;;; raylib [shaders] example - custom uniform
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
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_custom_uniform.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-custom-uniform
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-custom-uniform)

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

    (init-window screen-width screen-height "raylib [shaders] example - custom uniform")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 8.0 8.0 8.0)  ; Camera position
                                  :target (vec3 0.0 1.5 0.0)    ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                  :fovy 45.0                    ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (model (load-model "resources/models/barracks.obj"))                  ; Load OBJ model
           (texture (load-texture "resources/models/barracks_diffuse.png"))      ; Load model texture (diffuse map)
           (position (vec3 0.0 0.0 0.0))                                         ; Set model position

           ;; Load postprocessing shader
           ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/swirl.fs" +glsl-version+)))

           ;; Get variable (uniform) location on the shader to connect with the program
           ;; NOTE: If uniform variable could not be found in the shader, function returns -1
           (swirl-center-loc (get-shader-location shader "center"))

           (swirl-center (list (/ (float screen-width) 2) (/ (float screen-height) 2)))

           ;; Create a RenderTexture2D to be used for render to texture
           (target (load-render-texture screen-width screen-height)))

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set model diffuse texture

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (let ((mouse-position (get-mouse-position)))
                 (setf swirl-center (list (vx mouse-position) (- screen-height (vy mouse-position)))))

               ;; Send new value to the shader to be used on drawing
               (set-shader-value shader swirl-center-loc swirl-center +shader-uniform-vec2+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode target)      ; Enable drawing to texture
               (clear-background +raywhite+)    ; Clear texture background

               (begin-mode-3d camera)           ; Begin 3d mode drawing
               (draw-model model position 0.5 +white+) ; Draw 3d model with texture
               (draw-grid 10 1.0)               ; Draw a grid
               (end-mode-3d)                    ; End 3d mode drawing, returns to orthographic 2d mode

               (draw-text "TEXT DRAWN IN RENDER TEXTURE" 200 10 30 +red+)
               (end-texture-mode)               ; End drawing to texture (now we have a texture available for next passes)

               (begin-drawing)
               (clear-background +raywhite+)    ; Clear screen background

               ;; Enable shader using the custom uniform
               (begin-shader-mode shader)
               ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
               (let ((tex (render-texture-texture target)))
                 (draw-texture-rec tex (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width tex)) :height (float (- (texture-height tex)))) (vec2 0.0 0.0) +white+))
               (end-shader-mode)

               ;; Draw some 2d text over drawn texture
               (draw-text "(c) Barracks 3D model by Alberto Cano" (- screen-width 220) (- screen-height 20) 10 +gray+)
               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model
      (unload-render-texture target)    ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
