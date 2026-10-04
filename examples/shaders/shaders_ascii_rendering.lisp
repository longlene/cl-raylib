;;;; raylib [shaders] example - ascii rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Maicon Santana (@maiconpintoabreu) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Maicon Santana (@maiconpintoabreu)
;;;; Common Lisp port of raylib/examples/shaders/shaders_ascii_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-ascii-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-ascii-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - ascii rendering")

    (let* (;; Texture to test static drawing
           (fudesumi (load-texture "resources/fudesumi.png"))
           ;; Texture to test moving drawing
           (raysan (load-texture "resources/raysan.png"))

           ;; Load shader to be used on postprocessing
           (shader (load-shader nil (text-format "resources/shaders/glsl%i/ascii.fs" +glsl-version+)))

           ;; These locations are used to send data to the GPU
           (resolution-loc (get-shader-location shader "resolution"))
           (font-size-loc (get-shader-location shader "fontSize"))

           ;; Set the character size for the ASCII effect
           ;; Fontsize should be 9 or more
           (font-size 9.0)

           ;; Send the updated values to the shader
           (resolution (list (float screen-width) (float screen-height)))

           (circle-pos (vec2 40.0 (* (float screen-height) 0.5)))
           (circle-speed 1.0)

           ;; RenderTexture to apply the postprocessing later
           (target (load-render-texture screen-width screen-height)))

      (set-shader-value shader resolution-loc resolution +shader-uniform-vec2+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf (vx circle-pos) circle-speed)

               (when (or (> (vx circle-pos) 200.0) (< (vx circle-pos) 40.0)) (setf circle-speed (* circle-speed -1))) ; Revert speed

               (when (and (is-key-pressed +key-left+) (> font-size 9.0)) (decf font-size 1)) ; Reduce fontSize
               (when (and (is-key-pressed +key-right+) (< font-size 15.0)) (incf font-size 1)) ; Increase fontSize

               ;; Set fontsize for the shader
               (set-shader-value shader font-size-loc font-size +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode target)
               (clear-background +white+)

               ;; Draw scene in our render texture
               (draw-texture fudesumi 500 -30 +white+)
               (draw-texture-v raysan circle-pos +white+)
               (end-texture-mode)

               (begin-drawing)
               (clear-background +raywhite+)

               (begin-shader-mode shader)
               ;; Draw the scene texture (that we rendered earlier) to the screen
               ;; The shader will process every pixel of this texture
               (let ((texture (render-texture-texture target)))
                 (draw-texture-rec texture
                                   (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (- (texture-height texture))))
                                   (vec2 0.0 0.0) +white+))
               (end-shader-mode)

               (draw-rectangle 0 0 screen-width 40 +black+)
               (draw-text (text-format "Ascii effect - FontSize:%2.0f - [Left] -1 [Right] +1 " font-size) 120 10 20 +lightgray+)
               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture target)    ; Unload render texture
      (unload-shader shader)            ; Unload shader
      (unload-texture fudesumi)         ; Unload texture
      (unload-texture raysan)           ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
