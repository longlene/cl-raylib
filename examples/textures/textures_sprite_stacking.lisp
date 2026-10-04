;;;; raylib [textures] example - sprite stacking
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Redbooth model (c) 2017-2025 @kluchek under https://creativecommons.org/licenses/by/4.0/ https://github.com/kluchek/vox-models/
;;;;
;;;; Copyright (c) 2025 Robin (@RobinsAviary)
;;;; Common Lisp port of raylib/examples/textures/textures_sprite_stacking.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-sprite-stacking
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-sprite-stacking)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - sprite stacking")

    (let ((booth (load-texture "resources/booth.png"))
          (stack-scale 3.0)             ; Overall scale of the stacked sprite
          (stack-spacing 2.0)           ; Vertical spacing between each layer
          (stack-count 122)             ; Number of layers, used for calculating the size of a single slice
          (rotation-speed 30.0)         ; Stacked sprites rotation speed
          (rotation 0.0)                ; Current rotation of the stacked sprite
          (speed-change 0.25))          ; Amount speed will change by when the user presses A/D

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Use mouse wheel to affect stack separation
               (incf stack-spacing (* (get-mouse-wheel-move) 0.1))
               (setf stack-spacing (clamp stack-spacing 0.0 5.0))

               ;; Add a positive/negative offset to spin right/left at different speeds
               (when (or (is-key-down +key-left+) (is-key-down +key-a+)) (decf rotation-speed speed-change))
               (when (or (is-key-down +key-right+) (is-key-down +key-d+)) (incf rotation-speed speed-change))

               (incf rotation (* rotation-speed (get-frame-time)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Get the size of a single slice
               (let* ((frame-width (float (texture-width booth)))
                      (frame-height (/ (float (texture-height booth)) (float stack-count)))

                      ;; Get the scaled resolution to draw at
                      (scaled-width (* frame-width stack-scale))
                      (scaled-height (* frame-height stack-scale)))

                 ;; Draw the stacked sprite, rotated to the correct angle, with an vertical offset applied based on its y location
                 (loop for i from (1- stack-count) downto 0
                       do ;; Center vertically
                          (let ((source (make-rectangle :x 0.0 :y (* (float i) frame-height) :width frame-width :height frame-height))
                                (dest (make-rectangle :x (/ screen-width 2.0)
                                                      :y (- (+ (/ screen-height 2.0) (* i stack-spacing)) (/ (* stack-spacing stack-count) 2.0))
                                                      :width scaled-width :height scaled-height))
                                (origin (vec2 (/ scaled-width 2.0) (/ scaled-height 2.0))))

                            (draw-texture-pro booth source dest origin rotation +white+))))

               (draw-text (format nil "A/D to spin~%mouse wheel to change separation (aka 'angle')") 10 10 20 +darkgray+)
               (draw-text (text-format "current spacing: %.01f" stack-spacing) 10 50 20 +darkgray+)
               (draw-text (text-format "current speed: %.02f" rotation-speed) 10 70 20 +darkgray+)
               (draw-text "redbooth model (c) kluchek under cc 4.0" 10 420 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture booth)

      (close-window))))                 ; Close window and OpenGL context

(main)
