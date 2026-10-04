;;;; raylib [shaders] example - depth rendering
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Luís Almeida (@luis605) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Luís Almeida (@luis605)
;;;; Common Lisp port of raylib/examples/shaders/shaders_depth_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-depth-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-depth-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

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
                (texture-format depth) 19   ; DEPTH_COMPONENT_24BIT: Not defined in raylib
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

    (init-window screen-width screen-height "raylib [shaders] example - depth rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 4.0 1.0 5.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; Load render texture with a depth texture attached
           (target (load-render-texture-depth-tex screen-width screen-height))

           ;; Load depth shader and get depth texture shader location
           (depth-shader (load-shader nil (text-format "resources/shaders/glsl%i/depth_render.fs" +glsl-version+)))
           (depth-loc (get-shader-location depth-shader "depthTexture"))
           (flip-texture-loc (get-shader-location depth-shader "flipY"))

           ;; Load scene models
           (cube (load-model-from-mesh (gen-mesh-cube 1.0 1.0 1.0)))
           (floor (load-model-from-mesh (gen-mesh-plane 20.0 20.0 1 1))))

      (set-shader-value depth-shader flip-texture-loc 1 +shader-uniform-int+) ; Flip Y texture

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
               (begin-texture-mode target)
               (clear-background +white+)

               (begin-mode-3d camera)
               (draw-model cube (vec3 0.0 0.0 0.0) 3.0 +yellow+)
               (draw-model floor (vec3 10.0 0.0 2.0) 2.0 +red+)
               (end-mode-3d)
               (end-texture-mode)

               ;; Draw into screen (main framebuffer)
               (begin-drawing)
               (clear-background +raywhite+)

               (begin-shader-mode depth-shader)
               (set-shader-value-texture depth-shader depth-loc (render-texture-depth target))
               (draw-texture (render-texture-depth target) 0 0 +white+)
               (end-shader-mode)

               (draw-rectangle 10 10 320 93 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 10 10 320 93 +blue+)

               (draw-text "Camera Controls:" 20 20 10 +black+)
               (draw-text "- WASD to move" 40 40 10 +darkgray+)
               (draw-text "- Mouse Wheel Pressed to Pan" 40 60 10 +darkgray+)
               (draw-text "- Z to zoom to (0, 0, 0)" 40 80 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model cube)               ; Unload model
      (unload-model floor)              ; Unload model
      (unload-render-texture-depth-tex target)
      (unload-shader depth-shader)      ; Unload shader

      (close-window))))                 ; Close window and OpenGL context

(main)
