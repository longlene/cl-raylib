;;;; raylib [textures] example - srcrec dstrec
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.3
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_srcrec_dstrec.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-srcrec-dstrec
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-srcrec-dstrec)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - srcrec dstrec")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let* ((scarfy (load-texture "resources/scarfy.png")) ; Texture loading

           (frame-width (truncate (texture-width scarfy) 6))
           (frame-height (texture-height scarfy))

           ;; Source rectangle (part of the texture to use for drawing)
           (source-rec (make-rectangle :x 0.0 :y 0.0 :width (float frame-width) :height (float frame-height)))

           ;; Destination rectangle (screen rectangle where drawing part of texture)
           (dest-rec (make-rectangle :x (/ screen-width 2.0) :y (/ screen-height 2.0) :width (* frame-width 2.0) :height (* frame-height 2.0)))

           ;; Origin of the texture (rotation/scale point), it's relative to destination rectangle size
           (origin (vec2 (float frame-width) (float frame-height)))

           (rotation 0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf rotation)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; NOTE: Using DrawTexturePro() we can easily rotate and scale the part of the texture we draw
               ;; sourceRec defines the part of the texture we use for drawing
               ;; destRec defines the rectangle where our texture part will fit (scaling it to fit)
               ;; origin defines the point of the texture used as reference for rotation and scaling
               ;; rotation defines the texture rotation (using origin as rotation point)
               (draw-texture-pro scarfy source-rec dest-rec origin (float rotation) +white+)

               (draw-line (truncate (rectangle-x dest-rec)) 0 (truncate (rectangle-x dest-rec)) screen-height +gray+)
               (draw-line 0 (truncate (rectangle-y dest-rec)) screen-width (truncate (rectangle-y dest-rec)) +gray+)

               (draw-text "(c) Scarfy sprite by Eiden Marsal" (- screen-width 200) (- screen-height 20) 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture scarfy)           ; Texture unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
