;;;; raylib [shaders] example - shapes textures
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
;;;; Example originally created with raylib 1.7, last time updated with raylib 3.7
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_shapes_textures.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-shapes-textures
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-shapes-textures)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - shapes textures")

    (let ((fudesumi (load-texture "resources/fudesumi.png"))

          ;; Load shader to be used on some parts drawing
          ;; NOTE 1: Using GLSL 330 shader version, on OpenGL ES 2.0 use GLSL 100 shader version
          ;; NOTE 2: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
          (shader (load-shader nil (text-format "resources/shaders/glsl%i/grayscale.fs" +glsl-version+))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update your variables here
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Start drawing with default shader

               (draw-text "USING DEFAULT SHADER" 20 40 10 +red+)

               (draw-circle 80 120 35 +darkblue+)
               (draw-circle-gradient (vec2 80.0 220.0) 60 +green+ +skyblue+)
               (draw-circle-lines 80 340 80 +darkblue+)

               ;; Activate our custom shader to be applied on next shapes/textures drawings
               (begin-shader-mode shader)

               (draw-text "USING CUSTOM SHADER" 190 40 10 +red+)

               (draw-rectangle (- 250 60) 90 120 60 +red+)
               (draw-rectangle-gradient-h (- 250 90) 170 180 130 +maroon+ +gold+)
               (draw-rectangle-lines (- 250 40) 320 80 60 +orange+)

               ;; Activate our default shader for next drawings
               (end-shader-mode)

               (draw-text "USING DEFAULT SHADER" 370 40 10 +red+)

               (draw-triangle (vec2 430.0 80.0)
                              (vec2 (- 430.0 60) 150.0)
                              (vec2 (+ 430.0 60) 150.0) +violet+)

               (draw-triangle-lines (vec2 430.0 160.0)
                                    (vec2 (- 430.0 20) 230.0)
                                    (vec2 (+ 430.0 20) 230.0) +darkblue+)

               (draw-poly (vec2 430.0 320.0) 6 80 0 +brown+)

               ;; Activate our custom shader to be applied on next shapes/textures drawings
               (begin-shader-mode shader)

               (draw-texture fudesumi 500 -30 +white+) ; Using custom shader

               ;; Activate our default shader for next drawings
               (end-shader-mode)

               (draw-text "(c) Fudesumi sprite by Eiden Marsal" 380 (- screen-height 20) 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-texture fudesumi)         ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
