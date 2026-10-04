;;;; raylib [shaders] example - lightmap rendering
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Jussi Viitala (@nullstare) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Jussi Viitala (@nullstare) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_lightmap_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-lightmap-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-lightmap-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +map-size+ 16)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; Enable Multi Sampling Anti Aliasing 4x (if available)
    (init-window screen-width screen-height "raylib [shaders] example - lightmap rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 4.0 6.0 8.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (mesh (gen-mesh-plane (float +map-size+) (float +map-size+) 1 1))
           (shader nil)
           (texture nil)
           (light nil)
           (lightmap nil)
           (material nil))

      ;; GenMeshPlane doesn't generate texcoords2 so we will upload them separately
      (setf (mesh-texcoords2 mesh) (make-array (* (mesh-vertex-count mesh) 2) :element-type 'single-float :initial-element 0.0))

      ;;                                         X                                          Y
      (setf (aref (mesh-texcoords2 mesh) 0) 0.0   (aref (mesh-texcoords2 mesh) 1) 0.0
            (aref (mesh-texcoords2 mesh) 2) 1.0   (aref (mesh-texcoords2 mesh) 3) 0.0
            (aref (mesh-texcoords2 mesh) 4) 0.0   (aref (mesh-texcoords2 mesh) 5) 1.0
            (aref (mesh-texcoords2 mesh) 6) 1.0   (aref (mesh-texcoords2 mesh) 7) 1.0)

      ;; Load a new texcoords2 attributes buffer
      (setf (aref (mesh-vbo-id mesh) +shader-loc-vertex-texcoord02+)
            (rl-load-vertex-buffer (mesh-texcoords2 mesh) (* (mesh-vertex-count mesh) 2 4) nil))
      (rl-enable-vertex-array (mesh-vao-id mesh))

      ;; Index 5 is for texcoords2
      (rl-set-vertex-attribute 5 2 +rl-float+ nil 0 0)
      (rl-enable-vertex-attribute 5)
      (rl-disable-vertex-array)

      ;; Load lightmap shader
      (setf shader (load-shader (text-format "resources/shaders/glsl%i/lightmap.vs" +glsl-version+)
                                (text-format "resources/shaders/glsl%i/lightmap.fs" +glsl-version+)))

      (setf texture (load-texture "resources/cubicmap_atlas.png")
            light (load-texture "resources/spark_flame.png"))

      (gen-texture-mipmaps texture)
      (set-texture-filter texture +texture-filter-trilinear+)

      (setf lightmap (load-render-texture +map-size+ +map-size+))

      (setf material (load-material-default))
      (setf (material-shader material) shader)
      (setf (material-map-texture (aref (material-maps material) +material-map-albedo+)) texture
            ;; NOTE: C copies the Texture struct, the later GenTextureMipmaps() only updates lightmap.texture
            (material-map-texture (aref (material-maps material) +material-map-metalness+)) (copy-structure (render-texture-texture lightmap)))

      ;; Drawing to lightmap
      (begin-texture-mode lightmap)
      (clear-background +black+)

      (begin-blend-mode +blend-additive+)
      (draw-texture-pro light
                        (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width light)) :height (float (texture-height light)))
                        (make-rectangle :x 0.0 :y 0.0 :width (* 2.0 +map-size+) :height (* 2.0 +map-size+))
                        (vec2 (float +map-size+) (float +map-size+))
                        0.0
                        +red+)
      (draw-texture-pro light
                        (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width light)) :height (float (texture-height light)))
                        (make-rectangle :x (* (float +map-size+) 0.8) :y (/ (float +map-size+) 2.0) :width (* 2.0 +map-size+) :height (* 2.0 +map-size+))
                        (vec2 (float +map-size+) (float +map-size+))
                        0.0
                        +blue+)
      (draw-texture-pro light
                        (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width light)) :height (float (texture-height light)))
                        (make-rectangle :x (* (float +map-size+) 0.8) :y (* (float +map-size+) 0.8) :width (float +map-size+) :height (float +map-size+))
                        (vec2 (/ (float +map-size+) 2.0) (/ (float +map-size+) 2.0))
                        0.0
                        +green+)
      (begin-blend-mode +blend-alpha+)
      (end-texture-mode)

      ;; NOTE: To enable trilinear filtering we need mipmaps available for texture
      (gen-texture-mipmaps (render-texture-texture lightmap))
      (set-texture-filter (render-texture-texture lightmap) +texture-filter-trilinear+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)
               (draw-mesh mesh material (matrix-identity))
               (end-mode-3d)

               (draw-texture-pro (render-texture-texture lightmap)
                                 (make-rectangle :x 0.0 :y 0.0 :width (float (- +map-size+)) :height (float (- +map-size+)))
                                 (make-rectangle :x (- (float (get-render-width)) (* +map-size+ 8) 10) :y 10.0 :width (float (* +map-size+ 8)) :height (float (* +map-size+ 8)))
                                 (vec2 0.0 0.0)
                                 0.0
                                 +white+)

               (draw-text (text-format "LIGHTMAP: %ix%i pixels" +map-size+ +map-size+) (- (get-render-width) 130) (+ 20 (* +map-size+ 8)) 10 +green+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-mesh mesh)                ; Unload the mesh
      (unload-shader shader)            ; Unload shader
      (unload-texture texture)          ; Unload texture
      (unload-texture light)            ; Unload texture
      (unload-render-texture lightmap)  ; Unload lightmap render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
