;;;; raylib [shaders] example - texture outline
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; Example originally created with raylib 4.0, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Serenity Skiff (@GoldenThumbs) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Serenity Skiff (@GoldenThumbs) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_texture_outline.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-texture-outline
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-texture-outline)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - texture outline")

    (let* ((texture (load-texture "resources/fudesumi.png"))

           (shdr-outline (load-shader nil (text-format "resources/shaders/glsl%i/outline.fs" +glsl-version+)))

           (outline-size 2.0)
           (outline-color '(1.0 0.0 0.0 1.0))      ; Normalized RED color
           (texture-size (list (float (texture-width texture)) (float (texture-height texture))))

           ;; Get shader locations
           (outline-size-loc (get-shader-location shdr-outline "outlineSize"))
           (outline-color-loc (get-shader-location shdr-outline "outlineColor"))
           (texture-size-loc (get-shader-location shdr-outline "textureSize")))

      ;; Set shader values (they can be changed later)
      (set-shader-value shdr-outline outline-size-loc outline-size +shader-uniform-float+)
      (set-shader-value shdr-outline outline-color-loc outline-color +shader-uniform-vec4+)
      (set-shader-value shdr-outline texture-size-loc texture-size +shader-uniform-vec2+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf outline-size (get-mouse-wheel-move))
               (when (< outline-size 1.0) (setf outline-size 1.0))

               (set-shader-value shdr-outline outline-size-loc outline-size +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shdr-outline)

               (draw-texture texture (- (floor (get-screen-width) 2) (floor (texture-width texture) 2)) -30 +white+)

               (end-shader-mode)

               (draw-text (format nil "Shader-based~%texture~%outline") 10 10 20 +gray+)
               (draw-text (format nil "Scroll mouse wheel to~%change outline size") 10 72 20 +gray+)
               (draw-text (text-format "Outline size: %i px" (truncate outline-size)) 10 120 20 +maroon+)

               (draw-fps 710 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)
      (unload-shader shdr-outline)

      (close-window))))                 ; Close window and OpenGL context

(main)
