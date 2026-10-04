;;;; raylib [textures] example - image text
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_image_text.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-image-text
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-image-text)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - image text")

    (let ((parrots (load-image "resources/parrots.png")) ; Load image in CPU memory (RAM)
          (font nil))

      ;; When no codepoints are provided, LoadFontEx() loads a default set of codepoints
      ;; This set includes '@', which "KAISG.ttf" doesn't have a glyph for, causing a warning to be logged
      ;; We avoid this by just excluding it from the list of codepoints we load
      (multiple-value-bind (codepoints codepoint-count)
          (load-codepoints " !\"#$%&'()*+,-./0123456789:;<=>?ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

        ;; TTF Font loading with custom generation parameters
        (setf font (load-font-ex "resources/KAISG.ttf" 64 codepoints codepoint-count))
        (unload-codepoints codepoints))

      ;; Draw over image using custom font
      (image-draw-text-ex parrots font "[Parrots font drawing]" (vec2 20.0 20.0) (float (font-base-size font)) 0.0 +red+)

      (let* ((texture (load-texture-from-image parrots)) ; Image converted to texture, uploaded to GPU memory (VRAM)
             (position (vec2 (- (/ (float screen-width) 2) (/ (float (texture-width texture)) 2))
                             (- (/ (float screen-height) 2) (/ (float (texture-height texture)) 2) 20)))
             (show-font nil))
        (unload-image parrots)          ; Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM

        (set-target-fps 60)
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (if (is-key-down +key-space+)
                     (setf show-font t)
                     (setf show-font nil))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (if (not show-font)
                     (progn
                       ;; Draw texture with text already drawn inside
                       (draw-texture-v texture position +white+)

                       ;; Draw text directly using sprite font
                       (draw-text-ex font "[Parrots font drawing]" (vec2 (+ (vx position) 20) (+ (vy position) 20 280))
                                     (float (font-base-size font)) 0.0 +white+))
                     ;; Make the font atlas texture fit the screen, so we can see the whole thing
                     (let* ((font-texture (font-texture font))
                            (scale 1.0)
                            (atlas-ratio (/ (float (texture-width font-texture)) (float (texture-height font-texture))))
                            (screen-ratio (/ (float screen-width) (float screen-height))))

                       (if (>= atlas-ratio screen-ratio)
                           (setf scale (/ (float screen-width) (float (texture-width font-texture))))
                           (setf scale (/ (float screen-height) (float (texture-height font-texture)))))

                       (let ((width (* (float (texture-width font-texture)) scale))
                             (height (* (float (texture-height font-texture)) scale)))
                         (draw-texture-ex font-texture (vec2 (/ (- (float screen-width) width) 2) (/ (- (float screen-height) height) 2)) 0.0 scale +black+))))

                 (draw-text "PRESS SPACE to SHOW FONT ATLAS USED" 290 420 10 +darkgray+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture texture)        ; Texture unloading
        (unload-font font)              ; Unload custom font

        (close-window)))))              ; Close window and OpenGL context

(main)
