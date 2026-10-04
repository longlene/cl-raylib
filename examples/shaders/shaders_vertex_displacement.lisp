;;;; raylib [shaders] example - vertex displacement
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Alex ZH (@ZzzhHe) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Alex ZH (@ZzzhHe)
;;;; Common Lisp port of raylib/examples/shaders/shaders_vertex_displacement.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-vertex-displacement
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-vertex-displacement)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - vertex displacement")

    ;; set up camera
    (let* ((camera (make-camera3d :position (vec3 20.0 5.0 -20.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 60.0
                                  :projection +camera-perspective+))

           ;; Load vertex and fragment shaders
           (shader (load-shader (text-format "resources/shaders/glsl%i/vertex_displacement.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/vertex_displacement.fs" +glsl-version+)))

           ;; Load perlin noise texture
           (perlin-noise-image (gen-image-perlin-noise 512 512 0 0 1.0))
           (perlin-noise-map (load-texture-from-image perlin-noise-image))

           ;; Set shader uniform location
           (perlin-noise-map-loc (get-shader-location shader "perlinNoiseMap"))

           (plane-mesh nil)
           (plane-model nil)
           (time 0.0))

      (unload-image perlin-noise-image)

      (rl-enable-shader (shader-id shader))
      (rl-active-texture-slot 1)
      (rl-enable-texture (texture-id perlin-noise-map))
      (rl-set-uniform-sampler perlin-noise-map-loc 1)

      ;; Create a plane mesh and model
      (setf plane-mesh (gen-mesh-plane 50 50 50 50))
      (setf plane-model (load-model-from-mesh plane-mesh))
      ;; Set plane model material
      (setf (material-shader (aref (model-materials plane-model) 0)) shader)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+) ; Update camera

               (incf time (get-frame-time)) ; Update time variable
               (set-shader-value shader (get-shader-location shader "time") time +shader-uniform-float+) ; Send time value to shader
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (begin-shader-mode shader)
               ;; Draw plane model
               (draw-model plane-model (vec3 0.0 0.0 0.0) 1.0 '(255 255 255 255))
               (end-shader-mode)

               (end-mode-3d)

               (draw-text "Vertex displacement" 10 10 20 +darkgray+)
               (draw-fps 10 40)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)
      (unload-model plane-model)
      (unload-texture perlin-noise-map)

      (close-window))))                 ; Close window and OpenGL context

(main)
