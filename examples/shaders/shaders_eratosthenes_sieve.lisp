;;;; raylib [shaders] example - eratosthenes sieve
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: Sieve of Eratosthenes, the earliest known (ancient Greek) prime number sieve
;;;;
;;;;     "Sift the twos and sift the threes,
;;;;      The Sieve of Eratosthenes.
;;;;      When the multiples sublime,
;;;;      the numbers that are left are prime."
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; NOTE: Shaders used in this example are #version 330 (OpenGL 3.3)
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by ProfJski (@ProfJski) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 ProfJski (@ProfJski) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shaders/shaders_eratosthenes_sieve.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-eratosthenes-sieve
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-eratosthenes-sieve)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - eratosthenes sieve")

    (let ((target (load-render-texture screen-width screen-height))

          ;; Load Eratosthenes shader
          ;; NOTE: Defining 0 (NULL) for vertex shader forces usage of internal default vertex shader
          (shader (load-shader nil (text-format "resources/shaders/glsl%i/eratosthenes.fs" +glsl-version+))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Nothing to do here, everything is happening in the shader
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode target)      ; Enable drawing to texture
               (clear-background +black+)       ; Clear the render texture

               ;; Draw a rectangle in shader mode to be used as shader canvas
               ;; NOTE: Rectangle uses font white character texture coordinates,
               ;; so shader can not be applied here directly because input vertexTexCoord
               ;; do not represent full screen coordinates (space where want to apply shader)
               (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)
               (end-texture-mode)               ; End drawing to texture (now we have a blank texture available for the shader)

               (begin-drawing)
               (clear-background +raywhite+)    ; Clear screen background

               (begin-shader-mode shader)
               ;; NOTE: Render texture must be y-flipped due to default OpenGL coordinates (left-bottom)
               (let ((texture (render-texture-texture target)))
                 (draw-texture-rec texture (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (- (texture-height texture))))
                                   (vec2 0.0 0.0) +white+))
               (end-shader-mode)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-shader shader)            ; Unload shader
      (unload-render-texture target)    ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
