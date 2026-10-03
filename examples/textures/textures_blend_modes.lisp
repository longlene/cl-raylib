;;;; textures_blend_modes.lisp - Blend modes
;;;; Translated from raylib/examples/textures/textures_blend_modes.c

(require :cl-raylib)

(defpackage :textures-blend-modes
  (:use :cl :cl-raylib))

(in-package :textures-blend-modes)

(defun main ()
  "Main function - blend modes"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - blend modes")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((bg-image (load-image "examples/textures/resources/cyberpunk_street_background.png")) ; Loaded in CPU memory (RAM)
          (fg-image (load-image "examples/textures/resources/cyberpunk_street_foreground.png"))) ; Loaded in CPU memory (RAM)
      
      (let ((bg-texture (load-texture-from-image bg-image)) ; Image converted to texture, GPU memory (VRAM)
            (fg-texture (load-texture-from-image fg-image))) ; Image converted to texture, GPU memory (VRAM)

        ;; Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM
        (unload-image bg-image)
        (unload-image fg-image)

        (let ((blend-count-max 4)
              (blend-mode 0))

          (set-target-fps 60) ; Set game to run at 60 frames-per-second

          ;; Main game loop
          (loop until (window-should-close) do
            ;; Update
            (when (is-key-pressed +key-space+)
              (if (>= blend-mode (1- blend-count-max))
                  (setf blend-mode 0)
                  (incf blend-mode)))

            ;; Draw
            (begin-drawing)
              (clear-background +raywhite+)

              (draw-texture bg-texture 
                           (- (/ screen-width 2) (/ (texture-width bg-texture) 2))
                           (- (/ screen-height 2) (/ (texture-height bg-texture) 2))
                           +white+)

              ;; Apply the blend mode and then draw the foreground texture
              (begin-blend-mode blend-mode)
                (draw-texture fg-texture 
                             (- (/ screen-width 2) (/ (texture-width fg-texture) 2))
                             (- (/ screen-height 2) (/ (texture-height fg-texture) 2))
                             +white+)
              (end-blend-mode)

              ;; Draw the texts
              (draw-text "Press SPACE to change blend modes." 310 350 10 +gray+)

              (case blend-mode
                (#.+blend-alpha+ (draw-text "Current: BLEND_ALPHA" (- (/ screen-width 2) 60) 370 10 +gray+))
                (#.+blend-additive+ (draw-text "Current: BLEND_ADDITIVE" (- (/ screen-width 2) 60) 370 10 +gray+))
                (#.+blend-multiplied+ (draw-text "Current: BLEND_MULTIPLIED" (- (/ screen-width 2) 60) 370 10 +gray+))
                (#.+blend-add-colors+ (draw-text "Current: BLEND_ADD_COLORS" (- (/ screen-width 2) 60) 370 10 +gray+)))

              (draw-text "(c) Cyberpunk Street Environment by Luis Zuno (@ansimuz)" 
                        (- screen-width 330) (- screen-height 20) 10 +gray+)

            (end-drawing))

          ;; De-Initialization
          (unload-texture fg-texture) ; Unload foreground texture
          (unload-texture bg-texture))) ; Unload background texture

    ;; Close window
    (close-window)))

;; Run the example
(main)