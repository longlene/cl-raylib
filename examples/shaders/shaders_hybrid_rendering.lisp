;;;; raylib [shaders] example - hybrid rendering
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Buğra Alptekin Sarı (@BugraAlptekinSari) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Buğra Alptekin Sarı (@BugraAlptekinSari)
;;;; Common Lisp port of raylib/examples/shaders/shaders_hybrid_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-hybrid-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-hybrid-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Types and Structures Definition
;;------------------------------------------------------------------------------------
(defstruct ray-locs
  (cam-pos 0)
  (cam-dir 0)
  (screen-center 0))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Load custom render texture, create a writable depth texture buffer
(defun load-render-texture-depth-tex (width height)
  (let ((target (make-render-texture :texture (make-texture) :depth (make-texture))))

    (setf (render-texture-id target) (rl-load-framebuffer)) ; Load an empty framebuffer

    (if (> (render-texture-id target) 0)
        (let ((texture (render-texture-texture target))
              (depth (render-texture-depth target)))
          (rl-enable-framebuffer (render-texture-id target))

          ;; Create color texture (default to RGBA)
          (setf (texture-id texture) (rl-load-texture nil width height +pixelformat-uncompressed-r8g8b8a8+ 1)
                (texture-width texture) width
                (texture-height texture) height
                (texture-format texture) +pixelformat-uncompressed-r8g8b8a8+
                (texture-mipmaps texture) 1)

          ;; Create depth texture buffer (instead of raylib default renderbuffer)
          (setf (texture-id depth) (rl-load-texture-depth width height nil)
                (texture-width depth) width
                (texture-height depth) height
                (texture-format depth) 19   ; DEPTH_COMPONENT_24BIT?
                (texture-mipmaps depth) 1)

          ;; Attach color texture and depth texture to FBO
          (rl-framebuffer-attach (render-texture-id target) (texture-id texture) +rl-attachment-color-channel0+ +rl-attachment-texture2d+ 0)
          (rl-framebuffer-attach (render-texture-id target) (texture-id depth) +rl-attachment-depth+ +rl-attachment-texture2d+ 0)

          ;; Check if fbo is complete with attachments (valid)
          (when (rl-framebuffer-complete (render-texture-id target))
            (trace-log +log-info+ (text-format "FBO: [ID %i] Framebuffer object created successfully" (render-texture-id target))))

          (rl-disable-framebuffer))
        (trace-log +log-warning+ "FBO: Framebuffer object can not be created"))

    target))

;; Unload render texture from GPU memory (VRAM)
(defun unload-render-texture-depth-tex (target)
  (when (> (render-texture-id target) 0)
    ;; Color texture attached to FBO is deleted
    (rl-unload-texture (texture-id (render-texture-texture target)))
    (rl-unload-texture (texture-id (render-texture-depth target)))

    ;; NOTE: Depth texture is automatically
    ;; queried and deleted before deleting framebuffer
    (rl-unload-framebuffer (render-texture-id target))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - hybrid rendering")

    (let* (;; This Shader calculates pixel depth and color using raymarch
           (shdr-raymarch (load-shader nil (text-format "resources/shaders/glsl%i/hybrid_raymarch.fs" +glsl-version+)))

           ;; This Shader is a standard rasterization fragment shader with the addition of depth writing
           ;; You are required to write depth for all shaders if one shader does it
           (shdr-raster (load-shader nil (text-format "resources/shaders/glsl%i/hybrid_raster.fs" +glsl-version+)))

           ;; Declare Struct used to store camera locs
           (march-locs (make-ray-locs))

           ;; Transfer screenCenter position to shader. Which is used to calculate ray direction
           (screen-center (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))

           ;; Use Customized function to create writable depth texture buffer
           (target (load-render-texture-depth-tex screen-width screen-height))

           ;; Define the camera to look into our 3d world
           (camera (make-camera3d :position (vec3 0.5 1.0 1.5)  ; Camera position
                                  :target (vec3 0.0 0.5 0.0)    ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                  :fovy 45.0                    ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Camera FOV is pre-calculated in the camera distance
           (cam-dist (/ 1.0 (tan (* (camera3d-fovy camera) 0.5 +deg2rad+)))))

      ;; Fill the struct with shader locs
      (setf (ray-locs-cam-pos march-locs) (get-shader-location shdr-raymarch "camPos")
            (ray-locs-cam-dir march-locs) (get-shader-location shdr-raymarch "camDir")
            (ray-locs-screen-center march-locs) (get-shader-location shdr-raymarch "screenCenter"))

      (set-shader-value shdr-raymarch (ray-locs-screen-center march-locs) screen-center +shader-uniform-vec2+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update Camera Postion in the ray march shader
               (set-shader-value shdr-raymarch (ray-locs-cam-pos march-locs) (camera3d-position camera) +rl-shader-uniform-vec3+)

               ;; Update Camera Looking Vector. Vector length determines FOV
               (let ((cam-dir (vector3-scale (vector3-normalize (vector3-subtract (camera3d-target camera) (camera3d-position camera))) cam-dist)))
                 (set-shader-value shdr-raymarch (ray-locs-cam-dir march-locs) cam-dir +rl-shader-uniform-vec3+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               ;; Draw into our custom render texture (framebuffer)
               (begin-texture-mode target)
               (clear-background +white+)

               ;; Raymarch Scene
               (rl-enable-depth-test)   ; Manually enable Depth Test to handle multiple rendering methods
               (begin-shader-mode shdr-raymarch)
               (draw-rectangle-rec (make-rectangle :x 0.0 :y 0.0 :width (float screen-width) :height (float screen-height)) +white+)
               (end-shader-mode)

               ;; Rasterize Scene
               (begin-mode-3d camera)
               (begin-shader-mode shdr-raster)
               (draw-cube-wires-v (vec3 0.0 0.5 1.0) (vec3 1.0 1.0 1.0) +red+)
               (draw-cube-v (vec3 0.0 0.5 1.0) (vec3 1.0 1.0 1.0) +purple+)
               (draw-cube-wires-v (vec3 0.0 0.5 -1.0) (vec3 1.0 1.0 1.0) +darkgreen+)
               (draw-cube-v (vec3 0.0 0.5 -1.0) (vec3 1.0 1.0 1.0) +yellow+)
               (draw-grid 10 1.0)
               (end-shader-mode)
               (end-mode-3d)
               (end-texture-mode)

               ;; Draw into screen our custom render texture
               (begin-drawing)
               (clear-background +raywhite+)

               (draw-texture-rec (render-texture-texture target) (make-rectangle :x 0.0 :y 0.0 :width (float screen-width) :height (float (- screen-height))) (vec2 0.0 0.0) +white+)
               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture-depth-tex target)
      (unload-shader shdr-raymarch)
      (unload-shader shdr-raster)

      (close-window))))                 ; Close window and OpenGL context

(main)
