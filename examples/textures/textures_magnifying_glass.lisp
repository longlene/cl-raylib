;;;; raylib [textures] example - magnifying glass
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Luke Vaughan (@badram) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Luke Vaughan (@badram)
;;;; Common Lisp port of raylib/examples/textures/textures_magnifying_glass.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-magnifying-glass
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-magnifying-glass)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - magnifying glass")

    (let* ((bunny (load-texture "resources/raybunny.png"))
           (parrots (load-texture "resources/parrots.png"))

           ;; Use image draw to generate a mask texture instead of loading it from a file.
           (circle (gen-image-color 256 256 +blank+))
           (mask (progn
                   (image-draw-circle circle 128 128 128 +white+)
                   (load-texture-from-image circle))) ; Copy the mask image from RAM to VRAM

           (magnified-world (progn
                              (unload-image circle) ; Unload the image from RAM
                              (load-render-texture 256 256)))

           (camera (make-camera2d :offset (vec2 128.0 128.0) ; Offset by half the size of the magnifying glass to counteract drawing the texture centered on the mouse position
                                  :target (vec2 0.0 0.0)
                                  :rotation 0.0
                                  :zoom 2.0)))      ; Set magnifying glass zoom

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((m-pos (get-mouse-position)))
                 (setf (camera2d-target camera) m-pos)
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw the normal version of the world
                 (draw-texture parrots 144 33 +white+)
                 (draw-text "Use the magnifying glass to find hidden bunnies!" 154 6 20 +black+)

                 ;; Render to a the magnifying glass
                 (begin-texture-mode magnified-world)

                 (clear-background +raywhite+)

                 (begin-mode-2d camera)

                 ;; Draw the same things in the magnified world as were in the normal version
                 (draw-texture parrots 144 33 +white+)
                 (draw-text "Use the magnifying glass to find hidden bunnies!" 154 6 20 +black+)

                 ;; Draw bunnies only in the magnified world.
                 ;; BLEND_MULTIPLIED lets them take on the color of the image below them.
                 (begin-blend-mode +blend-multiplied+)
                 (draw-texture bunny 250 350 +white+)
                 (draw-texture bunny 500 100 +white+)
                 (draw-texture bunny 420 300 +white+)
                 (draw-texture bunny 650 10 +white+)
                 (end-blend-mode)

                 (end-mode-2d)

                 ;; Mask the magnifying glass view texture to a circle
                 ;; To make the mask affect only alpha, a CUSTOM blend mode is used with SEPARATE color/alpha functions
                 (begin-blend-mode +blend-custom-separate+)
                 ;; C: Color, A: Alpha, s: source (texture to draw), d: destination (texture drawn to)
                 ;;   glSrcRGB: RL_ZERO      - Cs * 0 = 0  - discard source rgb because we don't want to draw our texture's colors at all
                 ;;   glDstRGB: RL_ONE       - Cd * 1 = Cd - use destination colors unmodified
                 ;;   glSrcAlpha: RL_ONE     - As * 1 = As - use source alpha unmodified
                 ;;   glDstAlpha: RL_ZERO    - Ad * 0 = 0  - discard destination alpha
                 ;;   glEqRGB: RL_FUNC_ADD   - Cs(0) + Cd = Cd - destination color is unmodified
                 ;;   glEqAlpha: RL_FUNC_ADD - As + Ad(0) = As - destination alpha is set to source alpha
                 (rl-set-blend-factors-separate +rl-zero+ +rl-one+ +rl-one+ +rl-zero+ +rl-func-add+ +rl-func-add+)
                 (draw-texture mask 0 0 +white+)
                 (end-blend-mode)

                 (end-texture-mode)

                 ;; Draw magnifiedWorld to screen, centered on cursor
                 (draw-texture-rec (render-texture-texture magnified-world) (make-rectangle :x 0.0 :y 0.0 :width 256.0 :height -256.0)
                                   (vec2 (- (vx m-pos) 128) (- (vy m-pos) 128)) +white+)

                 ;; Draw the outer ring of the magnifying glass
                 (draw-ring m-pos 126.0 130.0 0.0 360.0 64 +black+)

                 ;; Draw floating specular highlight on the glass
                 (let ((rx (/ (vx m-pos) 800))
                       (ry (/ (vy m-pos) 800)))
                   (draw-circle (- (truncate (- (vx m-pos) (* 64 rx))) 32) (- (truncate (- (vy m-pos) (* 64 ry))) 32) 4.0 (color-alpha +white+ 0.5)))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture parrots)
      (unload-texture bunny)
      (unload-texture mask)
      (unload-render-texture magnified-world)

      (close-window))))                 ; Close window and OpenGL context

(main)
