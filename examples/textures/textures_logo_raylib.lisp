;;;; raylib [textures] example - logo raylib
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_logo_raylib.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-logo-raylib
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-logo-raylib)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - logo raylib")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((texture (load-texture "resources/raylib_logo.png"))) ; Texture loading

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

               (draw-text "this IS a texture!" 360 370 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Texture unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
