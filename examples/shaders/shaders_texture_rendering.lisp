;;;; raylib [shaders] example - texture rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Michał Ciesielski (@ciessielski) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Michał Ciesielski (@ciessielski) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_texture_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-texture-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-texture-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - texture rendering")

    (let* ((im-blank (gen-image-color 1024 1024 +blank+))
           (texture (load-texture-from-image im-blank)) ; Load blank texture to fill on shader
           ;; NOTE: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/cubes_panning.fs" +glsl-version+)))
           (time 0.0)
           (time-loc (get-shader-location shader "uTime")))
      (unload-image im-blank)

      (set-shader-value shader time-loc time +shader-uniform-float+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;; -------------------------------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf time (float (get-time) 1.0))
               (set-shader-value shader time-loc time +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shader)       ; Enable our custom shader for next shapes/textures drawings
               (draw-texture texture 0 0 +white+) ; Drawing BLANK texture, all rendering magic happens on shader
               (end-shader-mode)                ; Disable our custom shader, return to default shader

               (draw-text "BACKGROUND is PAINTED and ANIMATED on SHADER!" 10 10 20 +maroon+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)
      (unload-texture texture)

      (close-window))))                 ; Close window and OpenGL context

(main)
