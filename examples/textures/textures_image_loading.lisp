(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  "raylib [textures] example - Image loading and texture creation"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [textures] example - image loading")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
      (let* ((image (load-image "resources/raylib_logo.png"))        ; Loaded in CPU memory (RAM)
             (texture (load-texture-from-image image)))               ; Image converted to texture, GPU memory (VRAM)
        
        (unload-image image) ; Once image has been converted to texture and uploaded to VRAM, it can be unloaded from RAM

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               ;; TODO: Update your variables here

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (draw-texture texture 
                               (- (/ screen-width 2) (/ (texture-width texture) 2))
                               (- (/ screen-height 2) (/ (texture-height texture) 2))
                               :white)

                 (draw-text "this IS a texture loaded from an image!" 300 370 10 :gray))))

        ;; Cleanup
        (unload-texture texture)))))

(main)