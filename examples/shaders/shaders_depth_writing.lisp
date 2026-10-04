;;;; raylib [shaders] example - depth writing
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Buğra Alptekin Sarı (@BugraAlptekinSari) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Buğra Alptekin Sarı (@BugraAlptekinSari)
;;;; Common Lisp port of raylib/examples/shaders/shaders_depth_writing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-depth-writing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-depth-writing)

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

    (init-window screen-width screen-height "raylib [shaders] example - depth writing")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 2.0 2.0 3.0)  ; Camera position
                                 :target (vec3 0.0 0.5 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 45.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load custom render texture with writable depth texture buffer
          (target (load-render-texture-depth-tex screen-width screen-height))

          ;; Load depth writing shader
          ;; NOTE: The shader inverts the depth buffer by writing into it by `gl_FragDepth = 1 - gl_FragCoord.z;`
          (shader (load-shader nil (text-format "resources/shaders/glsl%i/depth_write.fs" +glsl-version+))))

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
               ;; Draw into our custom render texture
               (begin-texture-mode target)
               (clear-background +white+)

               (begin-mode-3d camera)
               (begin-shader-mode shader)
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
      (unload-shader shader)

      (close-window))))                 ; Close window and OpenGL context

(main)
