;;;; raylib [textures] example - image generation
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 1.8
;;;;
;;;; Example contributed by Wilhem Barbier (@nounoursheureux) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Wilhem Barbier (@nounoursheureux) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_generation.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-generation
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-generation)

(defconstant +num-textures+ 9)          ; Currently we have 8 generation algorithms but some have multiple purposes (Linear and Square Gradients)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image generation")

    (let* ((vertical-gradient (gen-image-gradient-linear screen-width screen-height 0 +red+ +blue+))
           (horizontal-gradient (gen-image-gradient-linear screen-width screen-height 90 +red+ +blue+))
           (diagonal-gradient (gen-image-gradient-linear screen-width screen-height 45 +red+ +blue+))
           (radial-gradient (gen-image-gradient-radial screen-width screen-height 0.0 +white+ +black+))
           (square-gradient (gen-image-gradient-square screen-width screen-height 0.0 +white+ +black+))
           (checked (gen-image-checked screen-width screen-height 32 32 +red+ +blue+))
           (white-noise (gen-image-white-noise screen-width screen-height 0.5))
           (perlin-noise (gen-image-perlin-noise screen-width screen-height 50 50 4.0))
           (cellular (gen-image-cellular screen-width screen-height 32))

           (textures (make-array +num-textures+))
           (current-texture 0))

      (setf (aref textures 0) (load-texture-from-image vertical-gradient)
            (aref textures 1) (load-texture-from-image horizontal-gradient)
            (aref textures 2) (load-texture-from-image diagonal-gradient)
            (aref textures 3) (load-texture-from-image radial-gradient)
            (aref textures 4) (load-texture-from-image square-gradient)
            (aref textures 5) (load-texture-from-image checked)
            (aref textures 6) (load-texture-from-image white-noise)
            (aref textures 7) (load-texture-from-image perlin-noise)
            (aref textures 8) (load-texture-from-image cellular))

      ;; Unload image data (CPU RAM)
      (unload-image vertical-gradient)
      (unload-image horizontal-gradient)
      (unload-image diagonal-gradient)
      (unload-image radial-gradient)
      (unload-image square-gradient)
      (unload-image checked)
      (unload-image white-noise)
      (unload-image perlin-noise)
      (unload-image cellular)

      (set-target-fps 60)
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (or (is-mouse-button-pressed +mouse-button-left+) (is-key-pressed +key-right+))
                 (setf current-texture (mod (1+ current-texture) +num-textures+))) ; Cycle between the textures
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-texture (aref textures current-texture) 0 0 +white+)

               (draw-rectangle 30 400 325 30 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 30 400 325 30 (fade +white+ 0.5))
               (draw-text "MOUSE LEFT BUTTON to CYCLE PROCEDURAL TEXTURES" 40 410 10 +white+)

               (case current-texture
                 (0 (draw-text "VERTICAL GRADIENT" 560 10 20 +raywhite+))
                 (1 (draw-text "HORIZONTAL GRADIENT" 540 10 20 +raywhite+))
                 (2 (draw-text "DIAGONAL GRADIENT" 540 10 20 +raywhite+))
                 (3 (draw-text "RADIAL GRADIENT" 580 10 20 +lightgray+))
                 (4 (draw-text "SQUARE GRADIENT" 580 10 20 +lightgray+))
                 (5 (draw-text "CHECKED" 680 10 20 +raywhite+))
                 (6 (draw-text "WHITE NOISE" 640 10 20 +red+))
                 (7 (draw-text "PERLIN NOISE" 640 10 20 +red+))
                 (8 (draw-text "CELLULAR" 670 10 20 +raywhite+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unload textures data (GPU VRAM)
      (dotimes (i +num-textures+) (unload-texture (aref textures i)))

      (close-window))))                 ; Close window and OpenGL context

(main)
