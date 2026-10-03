;;;; shaders_shapes_textures.lisp
;;;; 
;;;; cl-raylib [shaders] example - Apply a shader to some shape or texture
;;;;
;;;; Translation of raylib's shaders_shapes_textures.c example
;;;; This example demonstrates how to apply custom shaders to shapes and textures
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-shaders-shapes-textures
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-shaders-shapes-textures)

;;; Constants
(defconstant +glsl-version+ 330 "GLSL version for desktop OpenGL")

(defun shaders-shapes-textures ()
  "Shader shapes and textures example - equivalent to raylib's shaders_shapes_textures"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [shaders] example - shapes and texture shaders")
      
      ;; Initialize shader system first
      (init-shader-system)
      
      ;; Load texture
      (let ((fudesumi (load-texture "examples/shaders/resources/fudesumi.png")))
        
        ;; Load shader to be used on some parts drawing
        ;; NOTE: Defining nil for vertex shader forces usage of internal default vertex shader
        (let ((shader (load-shader nil (format nil "examples/shaders/resources/shaders/glsl~d/grayscale.fs" +glsl-version+))))
          
          (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
          
          ;; Debug: Print initial state
          (format t "Starting main loop, ESC to exit~%")
          
          ;; Main game loop
          (loop until (window-should-close) do  ; Detect window close button or ESC key
            
            ;; Debug: Check ESC key explicitly
            (when (is-key-pressed +key-escape+)
              (format t "ESC key detected!~%")
              (return))
            
            ;; Update
            ;; TODO: Update your variables here
            
            ;; Draw
            (with-drawing
              (clear-background +raywhite+)
              
              ;; Start drawing with default shader
              (draw-text "USING DEFAULT SHADER" 20 40 10 +red+)
              (draw-text "Press ESC to exit" 20 60 10 +black+)
              
              (draw-circle 80 120 35 +darkblue+)
              (draw-circle-gradient (vec2 80.0 220.0) 60.0 +green+ +skyblue+)
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
              
              (draw-triangle (vec2 430 80)
                           (vec2 (- 430 60) 150)
                           (vec2 (+ 430 60) 150) +violet+)
              
              (draw-triangle-lines (vec2 430 160)
                                 (vec2 (- 430 20) 230)
                                 (vec2 (+ 430 20) 230) +darkblue+)
              
              (draw-poly (vec2 430 320) 6 80 0 +brown+)
              
              ;; Activate our custom shader to be applied on next shapes/textures drawings
              (begin-shader-mode shader)
              
              (when (is-texture-valid fudesumi)
                (draw-texture fudesumi 500 -30 +white+))  ; Using custom shader
              
              ;; Activate our default shader for next drawings
              (end-shader-mode)
              
              (draw-text "(c) Fudesumi sprite by Eiden Marsal" 380 (- screen-height 20) 10 +gray+)
              
              ;; Show shader status and debug info
              (draw-text (format nil "Shader ID: ~d" (if shader (shader-id shader) 0)) 10 10 16 +darkgreen+)
              (draw-text (format nil "Window should close: ~a" (window-should-close)) 10 80 10 +darkgreen+)))
          
          ;; De-Initialization
          (format t "Cleaning up...~%")
          (unload-shader shader)        ; Unload shader
          (unload-texture fudesumi)     ; Unload texture
          (cleanup-shader-system)))))

;; Run the example
(shaders-shapes-textures)