;;;; raylib [shaders] example - shadowmap rendering
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example contributed by TheManTheMythTheGameDev (@TheManTheMythTheGameDev) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 TheManTheMythTheGameDev (@TheManTheMythTheGameDev)
;;;; Common Lisp port of raylib/examples/shaders/shaders_shadowmap_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-shadowmap-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-shadowmap-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +shadowmap-resolution+ 1024)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Load render texture for shadowmap projection
;; NOTE: Load framebuffer with only a texture depth attachment,
;; no color attachment required for shadowmap
(defun load-shadowmap-render-texture (width height)
  (let ((target (make-render-texture :texture (make-texture :width width :height height) :depth (make-texture))))

    (setf (render-texture-id target) (rl-load-framebuffer)) ; Load an empty framebuffer

    (if (> (render-texture-id target) 0)
        (let ((depth (render-texture-depth target)))
          (rl-enable-framebuffer (render-texture-id target))

          ;; Create depth texture
          ;; NOTE: No need a color texture attachment for the shadowmap
          (setf (texture-id depth) (rl-load-texture-depth width height nil)
                (texture-width depth) width
                (texture-height depth) height
                (texture-format depth) 19   ; DEPTH_COMPONENT_24BIT?
                (texture-mipmaps depth) 1)

          ;; Attach depth texture to FBO
          (rl-framebuffer-attach (render-texture-id target) (texture-id depth) +rl-attachment-depth+ +rl-attachment-texture2d+ 0)

          ;; Check if fbo is complete with attachments (valid)
          (when (rl-framebuffer-complete (render-texture-id target))
            (trace-log +log-info+ (text-format "FBO: [ID %i] Framebuffer object created successfully" (render-texture-id target))))

          (rl-disable-framebuffer))
        (trace-log +log-warning+ "FBO: Framebuffer object can not be created"))

    target))

;; Unload shadowmap render texture from GPU memory (VRAM)
(defun unload-shadowmap-render-texture (target)
  (when (> (render-texture-id target) 0)
    ;; NOTE: Depth texture/renderbuffer is automatically
    ;; queried and deleted before deleting framebuffer
    (rl-unload-framebuffer (render-texture-id target))))

;; Draw full scene projecting shadows
;; NOTE: Required  to be called several time to generate shadowmap
(defun draw-scene (cube robot)
  (draw-model-ex cube (vector3-zero) (vec3 0.0 1.0 0.0) 0.0 (vec3 10.0 1.0 10.0) +blue+)
  (draw-model-ex cube (vec3 1.5 1.0 -1.5) (vec3 0.0 1.0 0.0) 0.0 (vector3-one) +white+)
  (draw-model-ex robot (vec3 0.0 0.5 0.0) (vec3 0.0 1.0 0.0) 0.0 (vec3 1.0 1.0 1.0) +red+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; Shadows are a HUGE topic, and this example shows an extremely simple implementation of the shadowmapping algorithm,
    ;; which is the industry standard for shadows. This algorithm can be extended in a ridiculous number of ways to improve
    ;; realism and also adapt it for different scenes. This is pretty much the simplest possible implementation
    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shaders] example - shadowmap rendering")

    (let* ((camera (make-camera3d :position (vec3 10.0 10.0 10.0)
                                  :target (vector3-zero)
                                  :projection +camera-perspective+
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0))

           (shadow-shader (load-shader (text-format "resources/shaders/glsl%i/shadowmap.vs" +glsl-version+)
                                       (text-format "resources/shaders/glsl%i/shadowmap.fs" +glsl-version+)))
           (light-dir (vector3-normalize (vec3 0.35 -1.0 -0.35)))
           (light-color +white+)
           (light-color-normalized (color-normalize light-color))
           (light-dir-loc (get-shader-location shadow-shader "lightDir"))
           (light-col-loc (get-shader-location shadow-shader "lightColor"))
           (ambient-loc (get-shader-location shadow-shader "ambient"))
           (ambient '(0.1 0.1 0.1 1.0))
           (light-vp-loc (get-shader-location shadow-shader "lightVP"))
           (shadow-map-loc (get-shader-location shadow-shader "shadowMap"))
           (shadow-map-resolution +shadowmap-resolution+)

           (cube (load-model-from-mesh (gen-mesh-cube 1.0 1.0 1.0)))
           (robot (load-model "resources/models/robot.glb")))

      (setf (aref (shader-locs shadow-shader) +shader-loc-vector-view+) (get-shader-location shadow-shader "viewPos"))

      (set-shader-value shadow-shader light-dir-loc light-dir +shader-uniform-vec3+)
      (set-shader-value shadow-shader light-col-loc light-color-normalized +shader-uniform-vec4+)
      (set-shader-value shadow-shader ambient-loc ambient +shader-uniform-vec4+)
      (set-shader-value shadow-shader (get-shader-location shadow-shader "shadowMapResolution") shadow-map-resolution +shader-uniform-int+)

      (setf (material-shader (aref (model-materials cube) 0)) shadow-shader)
      (dotimes (i (model-material-count robot)) (setf (material-shader (aref (model-materials robot) i)) shadow-shader))

      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/robot.glb")
        (let* ((shadow-map (load-shadowmap-render-texture +shadowmap-resolution+ +shadowmap-resolution+))

               ;; For the shadowmapping algorithm, we will be rendering everything from the light's point of view
               (light-camera (make-camera3d :position (vector3-scale light-dir -15.0)
                                            :target (vector3-zero)
                                            :projection +camera-orthographic+ ; Use an orthographic projection for directional lights
                                            :up (vec3 0.0 1.0 0.0)
                                            :fovy 20.0))

               (frame-counter 0)

               ;; Store the light matrices
               (light-view nil)
               (light-proj nil)
               (light-view-proj nil)
               (texture-active-slot 10))  ; Can be anything 0 to 15, but 0 will probably be taken up

          (set-target-fps 60)
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (let ((delta-time (get-frame-time)))

                     (update-camera camera +camera-orbital+)

                     ;; send the updated camera position to the shader so that it can calculate the correct lighting for the scene
                     (let ((camera-pos (vcopy (camera3d-position camera))))
                       (set-shader-value shadow-shader (aref (shader-locs shadow-shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

                     (incf frame-counter)
                     (setf frame-counter (mod frame-counter (model-animation-keyframe-count (aref anims 0))))
                     (update-model-animation robot (aref anims 0) (float frame-counter))

                     ;; Move light with arrow keys
                     (let ((camera-speed 0.05))
                       (when (is-key-down +key-left+)
                         (when (< (vx light-dir) 0.6) (incf (vx light-dir) (* camera-speed 60.0 delta-time))))
                       (when (is-key-down +key-right+)
                         (when (> (vx light-dir) -0.6) (decf (vx light-dir) (* camera-speed 60.0 delta-time))))
                       (when (is-key-down +key-up+)
                         (when (< (vz light-dir) 0.6) (incf (vz light-dir) (* camera-speed 60.0 delta-time))))
                       (when (is-key-down +key-down+)
                         (when (> (vz light-dir) -0.6) (decf (vz light-dir) (* camera-speed 60.0 delta-time)))))

                     (setf light-dir (vector3-normalize light-dir))
                     (setf (camera3d-position light-camera) (vector3-scale light-dir -15.0))
                     (set-shader-value shadow-shader light-dir-loc light-dir +shader-uniform-vec3+))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   ;; PASS 01: Render all objects into the shadowmap render texture
                   ;; We record all the objects' depths (as rendered from the light source's point of view) in a buffer
                   ;; Anything that is "visible" to the light is in light, anything that isn't is in shadow
                   ;; We can later use the depth buffer when rendering everything from the player's point of view
                   ;; to determine whether a given point is "visible" to the light
                   (begin-texture-mode shadow-map)
                   (clear-background +white+)

                   (begin-mode-3d light-camera)
                   (setf light-view (rl-get-matrix-modelview)
                         light-proj (rl-get-matrix-projection))
                   (draw-scene cube robot)
                   (end-mode-3d)

                   (end-texture-mode)

                   (setf light-view-proj (matrix-multiply light-view light-proj))

                   ;; PASS 02: Draw the scene into main framebuffer, using the generated shadowmap
                   (begin-drawing)
                   (clear-background +raywhite+)

                   (set-shader-value-matrix shadow-shader light-vp-loc light-view-proj)
                   (rl-enable-shader (shader-id shadow-shader))

                   (rl-active-texture-slot texture-active-slot)
                   (rl-enable-texture (texture-id (render-texture-depth shadow-map)))
                   (rl-set-uniform shadow-map-loc texture-active-slot +shader-uniform-int+ 1)

                   (begin-mode-3d camera)
                   (draw-scene cube robot)    ; Draw the same exact things as we drew in the shadowmap!
                   (end-mode-3d)

                   (rl-active-texture-slot texture-active-slot)
                   (rl-disable-texture)

                   (draw-text "Use the arrow keys to rotate the light!" 10 10 30 +red+)
                   (draw-text "Shadows in raylib using the shadowmapping algorithm!" (- screen-width 280) (- screen-height 20) 10 +gray+)

                   (end-drawing)

                   (when (is-key-pressed +key-f+) (take-screenshot "shaders_shadowmap.png")))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-shader shadow-shader)
          (unload-model cube)
          (unload-model robot)
          (unload-model-animations anims anim-count)
          (unload-shadowmap-render-texture shadow-map)

          (close-window))))))           ; Close window and OpenGL context

(main)
