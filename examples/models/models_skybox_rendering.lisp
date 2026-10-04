;;;; raylib [models] example - skybox rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_skybox_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-skybox-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-skybox-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Generate cubemap texture from HDR texture
(defun gen-texture-cubemap (shader panorama size format)
  (let ((cubemap (make-texture)))

    (rl-disable-backface-culling)       ; Disable backface culling to render inside the cube

    ;; STEP 1: Setup framebuffer
    ;;------------------------------------------------------------------------------------------
    (let ((rbo (rl-load-texture-depth size size t))
          (fbo 0))
      (setf (texture-id cubemap) (rl-load-texture-cubemap nil size format 1))

      (setf fbo (rl-load-framebuffer))
      (rl-framebuffer-attach fbo rbo +rl-attachment-depth+ +rl-attachment-renderbuffer+ 0)
      (rl-framebuffer-attach fbo (texture-id cubemap) +rl-attachment-color-channel0+ +rl-attachment-cubemap-positive-x+ 0)

      ;; Check if framebuffer is complete with attachments (valid)
      (when (rl-framebuffer-complete fbo) (trace-log +log-info+ (text-format "FBO: [ID %i] Framebuffer object created successfully" fbo)))
      ;;------------------------------------------------------------------------------------------

      ;; STEP 2: Draw to framebuffer
      ;;------------------------------------------------------------------------------------------
      ;; NOTE: Shader is used to convert HDR equirectangular environment map to cubemap equivalent (6 faces)
      (rl-enable-shader (shader-id shader))

      ;; Define projection matrix and send it to shader
      (let ((mat-fbo-projection (matrix-perspective (* 90.0d0 +deg2rad+) 1.0d0 (rl-get-cull-distance-near) (rl-get-cull-distance-far)))
            ;; Define view matrix for every side of the cubemap
            (fbo-views (vector (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 1.0 0.0 0.0) (vec3 0.0 -1.0 0.0))
                               (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 -1.0 0.0 0.0) (vec3 0.0 -1.0 0.0))
                               (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0))
                               (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 0.0 -1.0 0.0) (vec3 0.0 0.0 -1.0))
                               (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 0.0 0.0 1.0) (vec3 0.0 -1.0 0.0))
                               (matrix-look-at (vec3 0.0 0.0 0.0) (vec3 0.0 0.0 -1.0) (vec3 0.0 -1.0 0.0)))))
        (rl-set-uniform-matrix (aref (shader-locs shader) +shader-loc-matrix-projection+) mat-fbo-projection)

        (rl-viewport 0 0 size size)     ; Set viewport to current fbo dimensions

        ;; Activate and enable texture for drawing to cubemap faces
        (rl-active-texture-slot 0)
        (rl-enable-texture (texture-id panorama))

        (dotimes (i 6)
          ;; Set the view matrix for the current cube face
          (rl-set-uniform-matrix (aref (shader-locs shader) +shader-loc-matrix-view+) (aref fbo-views i))

          ;; Select the current cubemap face attachment for the fbo
          ;; WARNING: This function by default enables->attach->disables fbo!!!
          (rl-framebuffer-attach fbo (texture-id cubemap) +rl-attachment-color-channel0+ (+ +rl-attachment-cubemap-positive-x+ i) 0)
          (rl-enable-framebuffer fbo)

          ;; Load and draw a cube, it uses the current enabled texture
          (rl-clear-screen-buffers)
          (rl-load-draw-cube)

          ;; ALTERNATIVE: Try to use internal batch system to draw the cube instead of rlLoadDrawCube
          ;; for some reason this method does not work, maybe due to cube triangles definition? normals pointing out?
          ;; TODO: Investigate this issue...
          ;;(rl-set-texture (texture-id panorama)) ; WARNING: It must be called after enabling current framebuffer if using internal batch system!
          ;;(rl-clear-screen-buffers)
          ;;(draw-cube-v (vector3-zero) (vector3-one) +white+)
          ;;(rl-draw-render-batch-active)
          ))
      ;;------------------------------------------------------------------------------------------

      ;; STEP 3: Unload framebuffer and reset state
      ;;------------------------------------------------------------------------------------------
      (rl-disable-shader)               ; Unbind shader
      (rl-disable-texture)              ; Unbind texture
      (rl-disable-framebuffer)          ; Unbind framebuffer
      (rl-unload-framebuffer fbo))      ; Unload framebuffer (and automatically attached depth texture/renderbuffer)

    ;; Reset viewport dimensions to default
    (rl-viewport 0 0 (rl-get-framebuffer-width) (rl-get-framebuffer-height))
    (rl-enable-backface-culling)
    ;;------------------------------------------------------------------------------------------

    (setf (texture-width cubemap) size
          (texture-height cubemap) size
          (texture-mipmaps cubemap) 1
          (texture-format cubemap) format)

    cubemap))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - skybox rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 1.0 1.0 1.0) ; Camera position
                                  :target (vec3 4.0 1.0 4.0)   ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                  :fovy 45.0                   ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           ;; Load skybox model
           (cube (gen-mesh-cube 1.0 1.0 1.0))
           (skybox (load-model-from-mesh cube))
           (material (aref (model-materials skybox) 0))

           ;; Set this to true to use an HDR Texture
           ;; NOTE: raylib must be built with HDR Support for this to work: SUPPORT_FILEFORMAT_HDR
           (use-hdr nil)

           (shdr-cubemap nil)
           (skybox-file-name ""))

      ;; Load skybox shader and set required locations
      ;; NOTE: Some locations are automatically set at shader loading
      (setf (material-shader material) (load-shader (text-format "resources/shaders/glsl%i/skybox.vs" +glsl-version+)
                                                    (text-format "resources/shaders/glsl%i/skybox.fs" +glsl-version+)))

      (set-shader-value (material-shader material) (get-shader-location (material-shader material) "environmentMap") +material-map-cubemap+ +shader-uniform-int+)
      (set-shader-value (material-shader material) (get-shader-location (material-shader material) "doGamma") (if use-hdr 1 0) +shader-uniform-int+)
      (set-shader-value (material-shader material) (get-shader-location (material-shader material) "vflipped") (if use-hdr 1 0) +shader-uniform-int+)

      ;; Load cubemap shader and setup required shader locations
      (setf shdr-cubemap (load-shader (text-format "resources/shaders/glsl%i/cubemap.vs" +glsl-version+)
                                      (text-format "resources/shaders/glsl%i/cubemap.fs" +glsl-version+)))

      (set-shader-value shdr-cubemap (get-shader-location shdr-cubemap "equirectangularMap") 0 +shader-uniform-int+)

      (if use-hdr
          (progn
            (setf skybox-file-name "resources/dresden_square_2k.hdr")

            ;; Load HDR panorama (sphere) texture
            (let ((panorama (load-texture skybox-file-name)))

              ;; Generate cubemap (texture with 6 quads-cube-mapping) from panorama HDR texture
              ;; NOTE 1: New texture is generated rendering to texture, shader calculates the sphere->cube coordinates mapping
              ;; NOTE 2: It seems on some Android devices WebGL, fbo does not properly support a FLOAT-based attachment,
              ;; despite texture can be successfully created.. so using PIXELFORMAT_UNCOMPRESSED_R8G8B8A8 instead of PIXELFORMAT_UNCOMPRESSED_R32G32B32A32
              (setf (material-map-texture (aref (material-maps material) +material-map-cubemap+))
                    (gen-texture-cubemap shdr-cubemap panorama 1024 +pixelformat-uncompressed-r8g8b8a8+))

              (unload-texture panorama))) ; Texture not required anymore, cubemap already generated
          ;; TODO: WARNING: On PLATFORM_WEB it requires a big amount of memory to process input image
          ;; and generate the required cubemap image to be passed to rlLoadTextureCubemap()
          (let ((image (load-image "resources/skybox.png")))
            (setf (material-map-texture (aref (material-maps material) +material-map-cubemap+))
                  (load-texture-cubemap image +cubemap-layout-auto-detect+))
            (unload-image image)))

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-first-person+)

               ;; Load new cubemap texture on drag&drop
               (when (is-file-dropped)
                 (let ((dropped-files (load-dropped-files)))

                   (when (= (file-path-list-count dropped-files) 1) ; Only support one file dropped
                     (let ((path (aref (file-path-list-paths dropped-files) 0)))
                       (when (is-file-extension path ".png;.jpg;.hdr;.bmp;.tga")
                         ;; Unload current cubemap texture to load new one
                         (unload-texture (material-map-texture (aref (material-maps material) +material-map-cubemap+)))

                         (if use-hdr
                             ;; Load HDR panorama (sphere) texture
                             (let ((panorama (load-texture path)))

                               ;; Generate cubemap from panorama texture
                               (setf (material-map-texture (aref (material-maps material) +material-map-cubemap+))
                                     (gen-texture-cubemap shdr-cubemap panorama 1024 +pixelformat-uncompressed-r8g8b8a8+))

                               (unload-texture panorama)) ; Texture not required anymore, cubemap already generated
                             (let ((image (load-image path)))
                               (setf (material-map-texture (aref (material-maps material) +material-map-cubemap+))
                                     (load-texture-cubemap image +cubemap-layout-auto-detect+))
                               (unload-image image)))

                         (setf skybox-file-name path))))

                   (unload-dropped-files dropped-files))) ; Unload filepaths from memory
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               ;; We are inside the cube, we need to disable backface culling!
               (rl-disable-backface-culling)
               (rl-disable-depth-mask)
               (draw-model skybox (vec3 0.0 0.0 0.0) 1.0 +white+)
               (rl-enable-backface-culling)
               (rl-enable-depth-mask)

               (draw-grid 10 1.0)

               (end-mode-3d)

               (if use-hdr
                   (draw-text (text-format "Panorama image from hdrihaven.com: %s" (get-file-name skybox-file-name)) 10 (- (get-screen-height) 20) 10 +black+)
                   (draw-text (text-format ": %s" (get-file-name skybox-file-name)) 10 (- (get-screen-height) 20) 10 +black+))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader (material-shader material))
      (unload-texture (material-map-texture (aref (material-maps material) +material-map-cubemap+)))

      (unload-model skybox)             ; Unload skybox model
      (unload-shader shdr-cubemap)

      (close-window))))                 ; Close window and OpenGL context

(main)
