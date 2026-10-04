;;;; raylib [shaders] example - multi sample2d
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
;;;; Example originally created with raylib 3.5, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_multi_sample2d.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-multi-sample2d
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-multi-sample2d)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - multi sample2d")

    (let* ((im-red (gen-image-color 800 450 '(255 0 0 255)))
           (tex-red (load-texture-from-image im-red))
           (im-blue (gen-image-color 800 450 '(0 0 255 255)))
           (tex-blue (load-texture-from-image im-blue))

           (shader (load-shader nil (text-format "resources/shaders/glsl%i/color_mix.fs" +glsl-version+)))

           ;; Get an additional sampler2D location to be enabled on drawing
           (tex-blue-loc (get-shader-location shader "texture1"))

           ;; Get shader uniform for divider
           (divider-loc (get-shader-location shader "divider"))
           (divider-value 0.5))

      (unload-image im-red)
      (unload-image im-blue)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (cond ((is-key-down +key-right+) (incf divider-value 0.01))
                     ((is-key-down +key-left+) (decf divider-value 0.01)))

               (cond ((< divider-value 0.0) (setf divider-value 0.0))
                     ((> divider-value 1.0) (setf divider-value 1.0)))

               (set-shader-value shader divider-loc divider-value +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shader)

               ;; WARNING: Additional textures (sampler2D) are enabled for ALL draw calls in the batch,
               ;; but EndShaderMode() forces batch drawing and resets active textures, this way
               ;; other textures (sampler2D) can be activated on consequent drawings (if required)
               ;; The downside of this approach is that SetShaderValue() must be called inside the loop,
               ;; to be set again after every EndShaderMode() reset
               (set-shader-value-texture shader tex-blue-loc tex-blue)

               ;; We are drawing texRed using default [sampler2D texture0] but
               ;; an additional texture units is enabled for texBlue [sampler2D texture1]
               (draw-texture tex-red 0 0 +white+)

               (end-shader-mode)                ; Texture sampler2D is reseted, needs to be set again for next frame

               (draw-text "Use KEY_LEFT/KEY_RIGHT to move texture mixing in shader!" 80 (- (get-screen-height) 40) 20 +raywhite+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-texture tex-red)          ; Unload texture
      (unload-texture tex-blue)         ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
