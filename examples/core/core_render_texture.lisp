;;;; raylib [core] example - render texture
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Copyright (c) 2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_render_texture.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-render-texture
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-render-texture)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - render texture")

    ;; Define a render texture to render
    (let* ((render-texture-width 300)
           (render-texture-height 300)
           (target (load-render-texture render-texture-width render-texture-height))
           (ball-position (vec2 (/ render-texture-width 2.0) (/ render-texture-height 2.0)))
           (ball-speed (vec2 5.0 4.0))
           (ball-radius 20)
           (rotation 0.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;-----------------------------------------------------
               ;; Ball movement logic
               (incf (vx ball-position) (vx ball-speed))
               (incf (vy ball-position) (vy ball-speed))

               ;; Check walls collision for bouncing
               (when (or (>= (vx ball-position) (- render-texture-width ball-radius)) (<= (vx ball-position) ball-radius))
                 (setf (vx ball-speed) (* (vx ball-speed) -1.0)))
               (when (or (>= (vy ball-position) (- render-texture-height ball-radius)) (<= (vy ball-position) ball-radius))
                 (setf (vy ball-speed) (* (vy ball-speed) -1.0)))

               ;; Render texture rotation
               (incf rotation 0.5)
               ;;-----------------------------------------------------

               ;; Draw
               ;;-----------------------------------------------------
               ;; Draw our scene to the render texture
               (begin-texture-mode target)

               (clear-background +skyblue+)

               (draw-rectangle 0 0 20 20 +red+)
               (draw-circle-v ball-position (float ball-radius) +maroon+)

               (end-texture-mode)

               ;; Draw render texture to main framebuffer
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw our render texture with rotation applied
               ;; NOTE 1: We set the origin of the texture to the center of the render texture
               ;; NOTE 2: We flip vertically the texture setting negative source rectangle height
               (let ((tex (render-texture-texture target)))
                 (draw-texture-pro tex
                                   (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width tex)) :height (float (- (texture-height tex))))
                                   (make-rectangle :x (/ screen-width 2.0) :y (/ screen-height 2.0)
                                                   :width (float (texture-width tex)) :height (float (texture-height tex)))
                                   (vec2 (/ (texture-width tex) 2.0) (/ (texture-height tex) 2.0)) rotation +white+))

               (draw-text "DRAWING BOUNCING BALL INSIDE RENDER TEXTURE!" 10 (- screen-height 40) 20 +black+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture target)

      (close-window))))                 ; Close window and OpenGL context

(main)
