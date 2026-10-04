;;;; raylib [shaders] example - texture waves
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
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Anata (@anatagawa) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Anata (@anatagawa) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_texture_waves.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-texture-waves
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-texture-waves)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - texture waves")

    ;; Load texture texture to apply shaders
    (let* ((texture (load-texture "resources/space.png"))

           ;; Load shader and setup location points and values
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/wave.fs" +glsl-version+)))

           (seconds-loc (get-shader-location shader "seconds"))
           (freq-x-loc (get-shader-location shader "freqX"))
           (freq-y-loc (get-shader-location shader "freqY"))
           (amp-x-loc (get-shader-location shader "ampX"))
           (amp-y-loc (get-shader-location shader "ampY"))
           (speed-x-loc (get-shader-location shader "speedX"))
           (speed-y-loc (get-shader-location shader "speedY"))

           ;; Shader uniform values that can be updated at any time
           (freq-x 25.0)
           (freq-y 25.0)
           (amp-x 5.0)
           (amp-y 5.0)
           (speed-x 8.0)
           (speed-y 8.0)

           (screen-size (list (float (get-screen-width)) (float (get-screen-height))))

           (seconds 0.0))

      (set-shader-value shader (get-shader-location shader "size") screen-size +shader-uniform-vec2+)
      (set-shader-value shader freq-x-loc freq-x +shader-uniform-float+)
      (set-shader-value shader freq-y-loc freq-y +shader-uniform-float+)
      (set-shader-value shader amp-x-loc amp-x +shader-uniform-float+)
      (set-shader-value shader amp-y-loc amp-y +shader-uniform-float+)
      (set-shader-value shader speed-x-loc speed-x +shader-uniform-float+)
      (set-shader-value shader speed-y-loc speed-y +shader-uniform-float+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;; -------------------------------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf seconds (get-frame-time))

               (set-shader-value shader seconds-loc seconds +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shader)

               (draw-texture texture 0 0 +white+)
               (draw-texture texture (texture-width texture) 0 +white+)

               (end-shader-mode)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-texture texture)          ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
