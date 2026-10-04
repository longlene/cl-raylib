;;;; raylib [textures] example - to image
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_to_image.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-to-image
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-to-image)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - to image")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let* ((image (load-image "resources/raylib_logo.png")) ; Load image data into CPU memory (RAM)
           (texture (load-texture-from-image image)))       ; Image converted to texture, GPU memory (RAM -> VRAM)
      (unload-image image)              ; Unload image data from CPU memory (RAM)

      (setf image (load-image-from-texture texture)) ; Load image from GPU texture (VRAM -> RAM)
      (unload-texture texture)          ; Unload texture from GPU memory (VRAM)

      (setf texture (load-texture-from-image image)) ; Recreate texture from retrieved image data (RAM -> VRAM)
      (unload-image image)              ; Unload retrieved image data from CPU memory (RAM)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

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

               (draw-texture texture (- (truncate screen-width 2) (truncate (texture-width texture) 2))
                             (- (truncate screen-height 2) (truncate (texture-height texture) 2)) +white+)

               (draw-text "this IS a texture loaded from an image!" 300 370 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Texture unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
