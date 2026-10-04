;;;; raylib [shaders] example - postprocessing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
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
;;;; Common Lisp port of raylib/examples/shaders/shaders_postprocessing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-postprocessing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-postprocessing)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-postpro-shaders+ 12)

;; Postpro shader
(defconstant +fx-grayscale+ 0)
(defconstant +fx-posterization+ 1)
(defconstant +fx-dream-vision+ 2)
(defconstant +fx-pixelizer+ 3)
(defconstant +fx-cross-hatching+ 4)
(defconstant +fx-cross-stitching+ 5)
(defconstant +fx-predator-view+ 6)
(defconstant +fx-scanlines+ 7)
(defconstant +fx-fisheye+ 8)
(defconstant +fx-sobel+ 9)
(defconstant +fx-bloom+ 10)
(defconstant +fx-blur+ 11)
;;(defconstant +fx-fxaa+ 12)

;;------------------------------------------------------------------------------------
;; Global Variables Definition
;;------------------------------------------------------------------------------------
(defparameter *postpro-shader-text* #("GRAYSCALE"
                                      "POSTERIZATION"
                                      "DREAM_VISION"
                                      "PIXELIZER"
                                      "CROSS_HATCHING"
                                      "CROSS_STITCHING"
                                      "PREDATOR_VIEW"
                                      "SCANLINES"
                                      "FISHEYE"
                                      "SOBEL"
                                      "BLOOM"
                                      "BLUR"
                                      ;;"FXAA"
                                      ))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; Enable Multi Sampling Anti Aliasing 4x (if available)

    (init-window screen-width screen-height "raylib [shaders] example - postprocessing")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 2.0 3.0 2.0)  ; Camera position
                                 :target (vec3 0.0 1.0 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 45.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          (model (load-model "resources/models/church.obj"))                 ; Load OBJ model
          (texture (load-texture "resources/models/church_diffuse.png"))     ; Load model texture (diffuse map)

          (position (vec3 0.0 0.0 0.0)) ; Set model position

          ;; Load all postpro shaders
          ;; NOTE 1: All postpro shader use the base vertex shader (DEFAULT_VERTEX_SHADER)
          ;; NOTE 2: We load the correct shader depending on GLSL version
          (shaders (make-array +max-postpro-shaders+))
          (current-shader +fx-grayscale+)
          (target nil))

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set model diffuse texture

      ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
      (flet ((load-fs (name) (load-shader nil (text-format "resources/shaders/glsl%i/%s.fs" +glsl-version+ name))))
        (setf (aref shaders +fx-grayscale+) (load-fs "grayscale")
              (aref shaders +fx-posterization+) (load-fs "posterization")
              (aref shaders +fx-dream-vision+) (load-fs "dream_vision")
              (aref shaders +fx-pixelizer+) (load-fs "pixelizer")
              (aref shaders +fx-cross-hatching+) (load-fs "cross_hatching")
              (aref shaders +fx-cross-stitching+) (load-fs "cross_stitching")
              (aref shaders +fx-predator-view+) (load-fs "predator")
              (aref shaders +fx-scanlines+) (load-fs "scanlines")
              (aref shaders +fx-fisheye+) (load-fs "fisheye")
              (aref shaders +fx-sobel+) (load-fs "sobel")
              (aref shaders +fx-bloom+) (load-fs "bloom")
              (aref shaders +fx-blur+) (load-fs "blur")))

      ;; Create a RenderTexture2D to be used for render to texture
      (setf target (load-render-texture screen-width screen-height))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (cond ((is-key-pressed +key-right+) (incf current-shader))
                     ((is-key-pressed +key-left+) (decf current-shader)))

               (cond ((>= current-shader +max-postpro-shaders+) (setf current-shader 0))
                     ((< current-shader 0) (setf current-shader (1- +max-postpro-shaders+))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode target)      ; Enable drawing to texture
               (clear-background +raywhite+)    ; Clear texture background

               (begin-mode-3d camera)           ; Begin 3d mode drawing
               (draw-model model position 0.1 +white+) ; Draw 3d model with texture
               (draw-grid 10 1.0)               ; Draw a grid
               (end-mode-3d)                    ; End 3d mode drawing, returns to orthographic 2d mode
               (end-texture-mode)               ; End drawing to texture (now we have a texture available for next passes)

               (begin-drawing)
               (clear-background +raywhite+)    ; Clear screen background

               ;; Render generated texture using selected postprocessing shader
               (begin-shader-mode (aref shaders current-shader))
               ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
               (let ((tex (render-texture-texture target)))
                 (draw-texture-rec tex (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width tex)) :height (float (- (texture-height tex)))) (vec2 0.0 0.0) +white+))
               (end-shader-mode)

               ;; Draw 2d shapes and text over drawn texture
               (draw-rectangle 0 9 580 30 (fade +lightgray+ 0.7))

               (draw-text "(c) Church 3D model by Alberto Cano" (- screen-width 200) (- screen-height 20) 10 +gray+)
               (draw-text "CURRENT POSTPRO SHADER:" 10 15 20 +black+)
               (draw-text (aref *postpro-shader-text* current-shader) 330 15 20 +red+)
               (draw-text "< >" 540 10 30 +darkblue+)
               (draw-fps 700 15)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unload all postpro shaders
      (dotimes (i +max-postpro-shaders+) (unload-shader (aref shaders i)))

      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model
      (unload-render-texture target)    ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
