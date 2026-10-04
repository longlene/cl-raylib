;;;; raylib [shaders] example - deferred rendering
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or OpenGL ES 3.0
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Justin Andreas Lacoste (@27justin) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Justin Andreas Lacoste (@27justin)
;;;; Common Lisp port of raylib/examples/shaders/shaders_deferred_rendering.c

(require :cl-raylib)
(load (merge-pathnames "rlights.lisp" *load-truename*)) ; Required for: lights

(defpackage #:raylib-examples/shaders-deferred-rendering
  (:use #:cl #:raylib #:rlights))
(in-package #:raylib-examples/shaders-deferred-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-cubes+ 30)

;; NOTE: The cubes are placed with the C library rand() like the C example, so it is the same
;; pseudo-random sequence (unseeded: seed 1)
(defun crand () (cffi:foreign-funcall "rand" :int))

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; GBuffer data
(defstruct gbuffer
  (framebuffer-id 0)
  (position-texture-id 0)
  (normal-texture-id 0)
  (albedo-spec-texture-id 0)
  (depth-renderbuffer-id 0))

;; Deferred mode passes
(defconstant +deferred-position+ 0)
(defconstant +deferred-normal+ 1)
(defconstant +deferred-albedo+ 2)
(defconstant +deferred-shading+ 3)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;; -------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - deferred rendering")

    (let* ((camera (make-camera3d :position (vec3 5.0 4.0 5.0) ; Camera position
                                  :target (vec3 0.0 1.0 0.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 60.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load plane model from a generated mesh
           (model (load-model-from-mesh (gen-mesh-plane 10.0 10.0 3 3)))
           (cube (load-model-from-mesh (gen-mesh-cube 2.0 2.0 2.0)))

           ;; Load geometry buffer (G-buffer) shader and deferred shader
           (gbuffer-shader (load-shader (text-format "resources/shaders/glsl%i/gbuffer.vs" +glsl-version+)
                                        (text-format "resources/shaders/glsl%i/gbuffer.fs" +glsl-version+)))

           (deferred-shader (load-shader (text-format "resources/shaders/glsl%i/deferred_shading.vs" +glsl-version+)
                                         (text-format "resources/shaders/glsl%i/deferred_shading.fs" +glsl-version+)))

           ;; Initialize the G-buffer
           (g-buffer (make-gbuffer))

           (tex-unit-position 0)
           (tex-unit-normal 1)
           (tex-unit-albedo-spec 2)

           (lights (make-array +max-lights+))

           (cube-scale 0.25)
           (cube-positions (make-array +max-cubes+))
           (cube-rotations (make-array +max-cubes+ :initial-element 0.0))

           (mode +deferred-shading+))

      (setf (aref (shader-locs deferred-shader) +shader-loc-vector-view+) (get-shader-location deferred-shader "viewPosition"))

      (setf (gbuffer-framebuffer-id g-buffer) (rl-load-framebuffer))

      (when (= (gbuffer-framebuffer-id g-buffer) 0) (trace-log +log-warning+ "Failed to create framebufferId"))

      (rl-enable-framebuffer (gbuffer-framebuffer-id g-buffer))

      ;; NOTE: Vertex positions are stored in a texture for simplicity. A better approach would use a depth texture
      ;; (instead of a detph renderbuffer) to reconstruct world positions in the final render shader via clip-space position,
      ;; depth, and the inverse view/projection matrices

      ;; 16-bit precision ensures OpenGL ES 3 compatibility, though it may lack precision for real scenarios
      ;; But as mentioned above, the positions could be reconstructed instead of stored. If not targeting OpenGL ES
      ;; and you wish to maintain this approach, consider using `RL_PIXELFORMAT_UNCOMPRESSED_R32G32B32`
      (setf (gbuffer-position-texture-id g-buffer) (rl-load-texture nil screen-width screen-height +rl-pixelformat-uncompressed-r16g16b16+ 1))

      ;; Similarly, 16-bit precision is used for normals ensures OpenGL ES 3 compatibility
      ;; This is generally sufficient, but a 16-bit fixed-point format offer a better uniform precision in all orientations
      (setf (gbuffer-normal-texture-id g-buffer) (rl-load-texture nil screen-width screen-height +rl-pixelformat-uncompressed-r16g16b16+ 1))

      ;; Albedo (diffuse color) and specular strength can be combined into one texture
      ;; The color in RGB, and the specular strength in the alpha channel
      (setf (gbuffer-albedo-spec-texture-id g-buffer) (rl-load-texture nil screen-width screen-height +rl-pixelformat-uncompressed-r8g8b8a8+ 1))

      ;; Activate the draw buffers for our framebufferId
      (rl-active-draw-buffers 3)

      ;; Now we attach our textures to the framebufferId
      (rl-framebuffer-attach (gbuffer-framebuffer-id g-buffer) (gbuffer-position-texture-id g-buffer) +rl-attachment-color-channel0+ +rl-attachment-texture2d+ 0)
      (rl-framebuffer-attach (gbuffer-framebuffer-id g-buffer) (gbuffer-normal-texture-id g-buffer) +rl-attachment-color-channel1+ +rl-attachment-texture2d+ 0)
      (rl-framebuffer-attach (gbuffer-framebuffer-id g-buffer) (gbuffer-albedo-spec-texture-id g-buffer) +rl-attachment-color-channel2+ +rl-attachment-texture2d+ 0)

      ;; Finally we attach the depth buffer
      (setf (gbuffer-depth-renderbuffer-id g-buffer) (rl-load-texture-depth screen-width screen-height t))
      (rl-framebuffer-attach (gbuffer-framebuffer-id g-buffer) (gbuffer-depth-renderbuffer-id g-buffer) +rl-attachment-depth+ +rl-attachment-renderbuffer+ 0)

      ;; Make sure our framebufferId is complete
      ;; NOTE: rlFramebufferComplete() automatically unbinds the framebufferId, so we don't have to rlDisableFramebuffer() here
      (unless (rl-framebuffer-complete (gbuffer-framebuffer-id g-buffer)) (trace-log +log-warning+ "Framebuffer is not complete"))

      ;; Now we initialize the sampler2D uniform's in the deferred shader
      ;; We do this by setting the uniform's values to the texture units that
      ;; we later bind our g-buffer textures to
      (rl-enable-shader (shader-id deferred-shader))
      (set-shader-value deferred-shader (rl-get-location-uniform (shader-id deferred-shader) "gPosition") tex-unit-position +rl-shader-uniform-sampler2d+)
      (set-shader-value deferred-shader (rl-get-location-uniform (shader-id deferred-shader) "gNormal") tex-unit-normal +rl-shader-uniform-sampler2d+)
      (set-shader-value deferred-shader (rl-get-location-uniform (shader-id deferred-shader) "gAlbedoSpec") tex-unit-albedo-spec +rl-shader-uniform-sampler2d+)
      (rl-disable-shader)

      ;; Assign out lighting shader to model
      (setf (material-shader (aref (model-materials model) 0)) gbuffer-shader
            (material-shader (aref (model-materials cube) 0)) gbuffer-shader)

      ;; Create lights
      (setf (aref lights 0) (create-light +light-point+ (vec3 -2.0 1.0 -2.0) (vector3-zero) +yellow+ deferred-shader)
            (aref lights 1) (create-light +light-point+ (vec3 2.0 1.0 2.0) (vector3-zero) +red+ deferred-shader)
            (aref lights 2) (create-light +light-point+ (vec3 -2.0 1.0 2.0) (vector3-zero) +green+ deferred-shader)
            (aref lights 3) (create-light +light-point+ (vec3 2.0 1.0 -2.0) (vector3-zero) +blue+ deferred-shader))

      (dotimes (i +max-cubes+)
        (let* ((x (- (float (mod (crand) 10)) 5))
               (y (float (mod (crand) 5)))
               (z (- (float (mod (crand) 10)) 5)))
          (setf (aref cube-positions i) (vec3 x y z)))

        (setf (aref cube-rotations i) (float (mod (crand) 360))))

      (rl-enable-depth-test)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update the shader with the camera view vector (points towards { 0.0f, 0.0f, 0.0f })
               (let ((camera-pos (list (vx (camera3d-position camera)) (vy (camera3d-position camera)) (vz (camera3d-position camera)))))
                 (set-shader-value deferred-shader (aref (shader-locs deferred-shader) +shader-loc-vector-view+) camera-pos +shader-uniform-vec3+))

               ;; Check key inputs to enable/disable lights
               (when (is-key-pressed +key-y+) (setf (light-enabled (aref lights 0)) (not (light-enabled (aref lights 0)))))
               (when (is-key-pressed +key-r+) (setf (light-enabled (aref lights 1)) (not (light-enabled (aref lights 1)))))
               (when (is-key-pressed +key-g+) (setf (light-enabled (aref lights 2)) (not (light-enabled (aref lights 2)))))
               (when (is-key-pressed +key-b+) (setf (light-enabled (aref lights 3)) (not (light-enabled (aref lights 3)))))

               ;; Check key inputs to switch between G-buffer textures
               (when (is-key-pressed +key-one+) (setf mode +deferred-position+))
               (when (is-key-pressed +key-two+) (setf mode +deferred-normal+))
               (when (is-key-pressed +key-three+) (setf mode +deferred-albedo+))
               (when (is-key-pressed +key-four+) (setf mode +deferred-shading+))

               ;; Update light values (actually, only enable/disable them)
               (dotimes (i +max-lights+) (update-light-values deferred-shader (aref lights i)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;; ---------------------------------------------------------------------------------
               (begin-drawing)

               ;; Draw to the geometry buffer by first activating it
               (rl-enable-framebuffer (gbuffer-framebuffer-id g-buffer))
               (rl-clear-color 0 0 0 0)
               (rl-clear-screen-buffers)        ; Clear color and depth buffer
               (rl-disable-color-blend)

               (begin-mode-3d camera)
               ;; NOTE: We have to use rlEnableShader here. `BeginShaderMode` or thus `rlSetShader`
               ;; will not work, as they won't immediately load the shader program
               (rl-enable-shader (shader-id gbuffer-shader))
               ;; When drawing a model here, make sure that the material's shaders are set to the gbuffer shader!
               (draw-model model (vector3-zero) 1.0 +white+)
               (draw-model cube (vec3 0.0 1.0 0.0) 1.0 +white+)

               (dotimes (i +max-cubes+)
                 (let ((position (aref cube-positions i)))
                   (draw-model-ex cube position (vec3 1.0 1.0 1.0) (aref cube-rotations i) (vec3 cube-scale cube-scale cube-scale) +white+)))

               (rl-disable-shader)
               (end-mode-3d)

               (rl-enable-color-blend)

               ;; Go back to the default framebufferId (0) and draw our deferred shading
               (rl-disable-framebuffer)
               (rl-clear-screen-buffers)        ; Clear color & depth buffer

               (flet ((draw-gbuffer-texture (id title)
                        (draw-texture-rec (make-texture :id id :width screen-width :height screen-height)
                                          (make-rectangle :x 0.0 :y 0.0 :width (float screen-width) :height (float (- screen-height)))
                                          (vector2-zero) +raywhite+)
                        (draw-text title 10 (- screen-height 30) 20 +darkgreen+)))
                 (case mode
                   (#.+deferred-shading+
                    (begin-mode-3d camera)
                    (rl-disable-color-blend)
                    (rl-enable-shader (shader-id deferred-shader))
                    ;; Bind our g-buffer textures
                    ;; We are binding them to locations that we earlier set in sampler2D uniforms `gPosition`, `gNormal`,
                    ;; and `gAlbedoSpec`
                    (rl-active-texture-slot tex-unit-position)
                    (rl-enable-texture (gbuffer-position-texture-id g-buffer))
                    (rl-active-texture-slot tex-unit-normal)
                    (rl-enable-texture (gbuffer-normal-texture-id g-buffer))
                    (rl-active-texture-slot tex-unit-albedo-spec)
                    (rl-enable-texture (gbuffer-albedo-spec-texture-id g-buffer))

                    ;; Finally, we draw a fullscreen quad to our default framebufferId
                    ;; This will now be shaded using our deferred shader
                    (rl-load-draw-quad)
                    (rl-disable-shader)
                    (rl-enable-color-blend)
                    (end-mode-3d)

                    ;; As a last step, we now copy over the depth buffer from our g-buffer to the default framebufferId
                    (rl-bind-framebuffer +rl-read-framebuffer+ (gbuffer-framebuffer-id g-buffer))
                    (rl-bind-framebuffer +rl-draw-framebuffer+ 0)
                    (rl-blit-framebuffer 0 0 screen-width screen-height 0 0 screen-width screen-height #x00000100) ; GL_DEPTH_BUFFER_BIT
                    (rl-disable-framebuffer)

                    ;; Since our shader is now done and disabled, we can draw spheres
                    ;; that represent light positions in default forward rendering
                    (begin-mode-3d camera)
                    (rl-enable-shader (rl-get-shader-id-default))
                    (dotimes (i +max-lights+)
                      (let ((light (aref lights i)))
                        (if (light-enabled light)
                            (draw-sphere-ex (light-position light) 0.2 8 8 (light-color light))
                            (draw-sphere-wires (light-position light) 0.2 8 8 (color-alpha (light-color light) 0.3)))))
                    (rl-disable-shader)
                    (end-mode-3d)

                    (draw-text "FINAL RESULT" 10 (- screen-height 30) 20 +darkgreen+))
                   (#.+deferred-position+
                    (draw-gbuffer-texture (gbuffer-position-texture-id g-buffer) "POSITION TEXTURE"))
                   (#.+deferred-normal+
                    (draw-gbuffer-texture (gbuffer-normal-texture-id g-buffer) "NORMAL TEXTURE"))
                   (#.+deferred-albedo+
                    (draw-gbuffer-texture (gbuffer-albedo-spec-texture-id g-buffer) "ALBEDO TEXTURE"))))

               (draw-text "Toggle lights keys: [Y][R][G][B]" 10 40 20 +darkgray+)
               (draw-text "Switch G-buffer textures: [1][2][3][4]" 10 70 20 +darkgray+)

               (draw-fps 10 10)

               (end-drawing))
      ;; -----------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unload the models
      (unload-model model)
      (unload-model cube)

      ;; Unload shaders
      (unload-shader deferred-shader)
      (unload-shader gbuffer-shader)

      ;; Unload geometry buffer and all attached textures
      (rl-unload-framebuffer (gbuffer-framebuffer-id g-buffer))
      (rl-unload-texture (gbuffer-position-texture-id g-buffer))
      (rl-unload-texture (gbuffer-normal-texture-id g-buffer))
      (rl-unload-texture (gbuffer-albedo-spec-texture-id g-buffer))
      (rl-unload-texture (gbuffer-depth-renderbuffer-id g-buffer))

      (close-window))))                 ; Close window and OpenGL context

(main)
