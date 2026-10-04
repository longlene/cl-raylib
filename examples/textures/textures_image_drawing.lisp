;;;; raylib [textures] example - image drawing
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 1.4
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-drawing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-drawing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image drawing")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((cat (load-image "resources/cat.png")) ; Load image in CPU memory (RAM)
          (parrots nil)
          (texture nil))
      (image-crop cat (make-rectangle :x 100.0 :y 10.0 :width 280.0 :height 380.0)) ; Crop an image piece
      (image-flip-horizontal cat)       ; Flip cropped image horizontally
      (image-resize cat 150 200)        ; Resize flipped-cropped image

      (setf parrots (load-image "resources/parrots.png")) ; Load image in CPU memory (RAM)

      ;; Draw one image over the other with a scaling of 1.5f
      (image-draw-image-pro parrots cat (make-rectangle :x 0.0 :y 0.0 :width (float (image-width cat)) :height (float (image-height cat)))
                            (make-rectangle :x 30.0 :y 40.0 :width (* (image-width cat) 1.5) :height (* (image-height cat) 1.5))
                            (vec2 0.0 0.0) 0.0 +white+)
      (image-crop parrots (make-rectangle :x 0.0 :y 50.0 :width (float (image-width parrots)) :height (- (float (image-height parrots)) 100))) ; Crop resulting image

      ;; Draw on the image with a few image draw methods
      (image-draw-pixel parrots 10 10 +raywhite+)
      (image-draw-circle-lines parrots 10 10 5 +raywhite+)
      (image-draw-rectangle parrots 5 20 10 10 +raywhite+)

      (unload-image cat)                ; Unload image from RAM

      ;; Load custom font for drawing on image
      (let ((font (load-font "resources/custom_jupiter_crash.png")))

        ;; Draw over image using custom font
        (image-draw-text-ex parrots font "PARROTS & CAT" (vec2 300.0 230.0) (float (font-base-size font)) -2.0 +white+)

        (unload-font font))             ; Unload custom font (already drawn used on image)

      (setf texture (load-texture-from-image parrots)) ; Image converted to texture, uploaded to GPU memory (VRAM)
      (unload-image parrots)            ; Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM

      (set-target-fps 60)
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
                             (- (truncate screen-height 2) (truncate (texture-height texture) 2) 40) +white+)
               (draw-rectangle-lines (- (truncate screen-width 2) (truncate (texture-width texture) 2))
                                     (- (truncate screen-height 2) (truncate (texture-height texture) 2) 40)
                                     (texture-width texture) (texture-height texture) +darkgray+)

               (draw-text "We are drawing only one texture from various images composed!" 240 350 10 +darkgray+)
               (draw-text "Source images have been cropped, scaled, flipped and copied one over the other." 190 370 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Texture unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
