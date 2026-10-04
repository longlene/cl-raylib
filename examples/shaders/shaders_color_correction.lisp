;;;; raylib [shaders] example - color correction
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: This example requires raylib OpenGL 3.3 or ES2 versions for shaders support,
;;;;       OpenGL 1.1 does not support shaders, recompile raylib to OpenGL 3.3 version
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jordi Santonja (@JordSant) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jordi Santonja (@JordSant)
;;;; Common Lisp port of raylib/examples/shaders/shaders_color_correction.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-color-correction
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shaders-color-correction)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-textures+ 4)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - color correction")

    (let* ((texture (vector (load-texture "resources/parrots.png")
                            (load-texture "resources/cat.png")
                            (load-texture "resources/mandrill.png")
                            (load-texture "resources/fudesumi.png")))

           (shdr-color-correction (load-shader nil (text-format "resources/shaders/glsl%i/color_correction.fs" +glsl-version+)))

           (image-index 0)
           (reset-button-clicked 0)

           (contrast 0.0)
           (saturation 0.0)
           (brightness 0.0)

           ;; Get shader locations
           (contrast-loc (get-shader-location shdr-color-correction "contrast"))
           (saturation-loc (get-shader-location shdr-color-correction "saturation"))
           (brightness-loc (get-shader-location shdr-color-correction "brightness")))

      ;; Set shader values (they can be changed later)
      (set-shader-value shdr-color-correction contrast-loc contrast +shader-uniform-float+)
      (set-shader-value shdr-color-correction saturation-loc saturation +shader-uniform-float+)
      (set-shader-value shdr-color-correction brightness-loc brightness +shader-uniform-float+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Select texture to draw
               (cond ((is-key-pressed +key-one+) (setf image-index 0))
                     ((is-key-pressed +key-two+) (setf image-index 1))
                     ((is-key-pressed +key-three+) (setf image-index 2))
                     ((is-key-pressed +key-four+) (setf image-index 3)))

               ;; Reset values to 0
               (when (or (is-key-pressed +key-r+) (/= reset-button-clicked 0))
                 (setf contrast 0.0
                       saturation 0.0
                       brightness 0.0))

               ;; Send the values to the shader
               (set-shader-value shdr-color-correction contrast-loc contrast +shader-uniform-float+)
               (set-shader-value shdr-color-correction saturation-loc saturation +shader-uniform-float+)
               (set-shader-value shdr-color-correction brightness-loc brightness +shader-uniform-float+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-shader-mode shdr-color-correction)

               (let ((tex (aref texture image-index)))
                 (draw-texture tex (- (floor 580 2) (floor (texture-width tex) 2)) (- (floor (get-screen-height) 2) (floor (texture-height tex) 2)) +white+))

               (end-shader-mode)

               (draw-line 580 0 580 (get-screen-height) '(218 218 218 255))
               (draw-rectangle 580 0 (get-screen-width) (get-screen-height) '(232 232 232 255))

               ;; Draw UI info text
               (draw-text "Color Correction" 585 40 20 +gray+)

               (draw-text "Picture" 602 75 10 +gray+)
               (draw-text "Press [1] - [4] to Change Picture" 600 230 8 +gray+)
               (draw-text "Press [R] to Reset Values" 600 250 8 +gray+)

               ;; Draw GUI controls
               ;;------------------------------------------------------------------------------
               (setf image-index (nth-value 1 (gui-toggle-group (make-rectangle :x 645.0 :y 70.0 :width 20.0 :height 20.0) "1;2;3;4" image-index)))

               (setf contrast (nth-value 1 (gui-slider-bar (make-rectangle :x 645.0 :y 100.0 :width 120.0 :height 20.0) "Contrast" (text-format "%.0f" contrast) contrast -100.0 100.0)))
               (setf saturation (nth-value 1 (gui-slider-bar (make-rectangle :x 645.0 :y 130.0 :width 120.0 :height 20.0) "Saturation" (text-format "%.0f" saturation) saturation -100.0 100.0)))
               (setf brightness (nth-value 1 (gui-slider-bar (make-rectangle :x 645.0 :y 160.0 :width 120.0 :height 20.0) "Brightness" (text-format "%.0f" brightness) brightness -100.0 100.0)))

               (setf reset-button-clicked (gui-button (make-rectangle :x 645.0 :y 190.0 :width 40.0 :height 20.0) "Reset"))
               ;;------------------------------------------------------------------------------

               (draw-fps 710 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (dotimes (i +max-textures+) (unload-texture (aref texture i)))
      (unload-shader shdr-color-correction)

      (close-window))))                 ; Close window and OpenGL context

(main)
