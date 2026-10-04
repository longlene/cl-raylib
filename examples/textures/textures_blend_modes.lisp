;;;; raylib [textures] example - blend modes
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: Images are loaded in CPU memory (RAM); textures are loaded in GPU memory (VRAM)
;;;;
;;;; Example originally created with raylib 3.5, last time updated with raylib 3.5
;;;;
;;;; Example contributed by Karlo Licudine (@accidentalrebel) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Karlo Licudine (@accidentalrebel)
;;;; Common Lisp port of raylib/examples/textures/textures_blend_modes.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-blend-modes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-blend-modes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - blend modes")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let* ((bg-image (load-image "resources/cyberpunk_street_background.png")) ; Loaded in CPU memory (RAM)
           (bg-texture (load-texture-from-image bg-image)) ; Image converted to texture, GPU memory (VRAM)

           (fg-image (load-image "resources/cyberpunk_street_foreground.png")) ; Loaded in CPU memory (RAM)
           (fg-texture (load-texture-from-image fg-image)) ; Image converted to texture, GPU memory (VRAM)

           (blend-count-max 4)
           (blend-mode 0))

      ;; Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM
      (unload-image bg-image)
      (unload-image fg-image)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+)
                 (if (>= blend-mode (- blend-count-max 1))
                     (setf blend-mode 0)
                     (incf blend-mode)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-texture bg-texture (- (truncate screen-width 2) (truncate (texture-width bg-texture) 2))
                             (- (truncate screen-height 2) (truncate (texture-height bg-texture) 2)) +white+)

               ;; Apply the blend mode and then draw the foreground texture
               (begin-blend-mode blend-mode)
               (draw-texture fg-texture (- (truncate screen-width 2) (truncate (texture-width fg-texture) 2))
                             (- (truncate screen-height 2) (truncate (texture-height fg-texture) 2)) +white+)
               (end-blend-mode)

               ;; Draw the texts
               (draw-text "Press SPACE to change blend modes." 310 350 10 +gray+)

               (case blend-mode
                 (#.+blend-alpha+ (draw-text "Current: BLEND_ALPHA" (- (truncate screen-width 2) 60) 370 10 +gray+))
                 (#.+blend-additive+ (draw-text "Current: BLEND_ADDITIVE" (- (truncate screen-width 2) 60) 370 10 +gray+))
                 (#.+blend-multiplied+ (draw-text "Current: BLEND_MULTIPLIED" (- (truncate screen-width 2) 60) 370 10 +gray+))
                 (#.+blend-add-colors+ (draw-text "Current: BLEND_ADD_COLORS" (- (truncate screen-width 2) 60) 370 10 +gray+)))

               (draw-text "(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)" (- screen-width 330) (- screen-height 20) 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture fg-texture)       ; Unload foreground texture
      (unload-texture bg-texture)       ; Unload background texture

      (close-window))))                 ; Close window and OpenGL context

(main)
