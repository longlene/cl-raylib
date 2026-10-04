;;;; raylib [textures] example - image rotate
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_rotate.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-rotate
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-rotate)

(defconstant +num-textures+ 3)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image rotate")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((image45 (load-image "resources/raylib_logo.png"))
          (image90 (load-image "resources/raylib_logo.png"))
          (image-neg90 (load-image "resources/raylib_logo.png"))
          (textures (make-array +num-textures+))
          (current-texture 0))

      (image-rotate image45 45)
      (image-rotate image90 90)
      (image-rotate image-neg90 -90)

      (setf (aref textures 0) (load-texture-from-image image45)
            (aref textures 1) (load-texture-from-image image90)
            (aref textures 2) (load-texture-from-image image-neg90))

      (unload-image image45)
      (unload-image image90)
      (unload-image image-neg90)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (or (is-mouse-button-pressed +mouse-button-left+) (is-key-pressed +key-right+))
                 (setf current-texture (mod (1+ current-texture) +num-textures+))) ; Cycle between the textures
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (let ((texture (aref textures current-texture)))
                 (draw-texture texture (- (truncate screen-width 2) (truncate (texture-width texture) 2))
                               (- (truncate screen-height 2) (truncate (texture-height texture) 2)) +white+))

               (draw-text "Press LEFT MOUSE BUTTON to rotate the image clockwise" 250 420 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (dotimes (i +num-textures+) (unload-texture (aref textures i)))

      (close-window))))                 ; Close window and OpenGL context

(main)
