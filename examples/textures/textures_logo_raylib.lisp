;;;; textures_logo_raylib.lisp - Texture loading and drawing
;;;; Translated from raylib/examples/textures/textures_logo_raylib.c

(require :cl-raylib)

(defpackage :textures-logo-raylib
  (:use :cl :cl-raylib))

(in-package :textures-logo-raylib)

(defun main ()
  "Main function - texture loading and drawing"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - texture loading and drawing")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((texture (load-texture "resources/raylib_logo.png"))) ; Texture loading

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; TODO: Update your variables here

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-texture texture 
                       (- (/ screen-width 2) (/ (texture-width texture) 2))
                       (- (/ screen-height 2) (/ (texture-height texture) 2)) 
                       +white+)

          (draw-text "this IS a texture\!" 360 370 10 +gray+)

        (end-drawing))

      ;; De-Initialization
      (unload-texture texture) ; Texture unloading
      )

    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
