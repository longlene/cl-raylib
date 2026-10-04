;;;; raylib [text] example - font spritefont
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: Sprite fonts should be generated following this conventions:
;;;;
;;;;   - Characters must be ordered starting with character 32 (Space)
;;;;   - Every character must be contained within the same Rectangle height
;;;;   - Every character and every line must be separated by the same distance (margin/padding)
;;;;   - Rectangles must be defined by a MAGENTA color background
;;;;
;;;; Following those constraints, a font can be provided just by an image,
;;;; this is quite handy to avoid additional font descriptor files (like BMFonts use)
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_font_spritefont.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-font-spritefont
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-font-spritefont)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - font spritefont")

    (let* ((msg1 "THIS IS A custom SPRITE FONT...")
           (msg2 "...and this is ANOTHER CUSTOM font...")
           (msg3 "...and a THIRD one! GREAT! :D")

           ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)
           (font1 (load-font "resources/custom_mecha.png"))         ; Font loading
           (font2 (load-font "resources/custom_alagard.png"))       ; Font loading
           (font3 (load-font "resources/custom_jupiter_crash.png")) ; Font loading

           (font-position1 (vec2 (- (/ screen-width 2.0) (/ (vx (measure-text-ex font1 msg1 (float (font-base-size font1)) -3)) 2))
                                 (- (/ screen-height 2.0) (/ (font-base-size font1) 2.0) 80.0)))

           (font-position2 (vec2 (- (/ screen-width 2.0) (/ (vx (measure-text-ex font2 msg2 (float (font-base-size font2)) -2.0)) 2.0))
                                 (- (/ screen-height 2.0) (/ (font-base-size font2) 2.0) 10.0)))

           (font-position3 (vec2 (- (/ screen-width 2.0) (/ (vx (measure-text-ex font3 msg3 (float (font-base-size font3)) 2.0)) 2.0))
                                 (+ (- (/ screen-height 2.0) (/ (font-base-size font3) 2.0)) 50.0))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update variables here...
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text-ex font1 msg1 font-position1 (float (font-base-size font1)) -3 +white+)
               (draw-text-ex font2 msg2 font-position2 (float (font-base-size font2)) -2 +white+)
               (draw-text-ex font3 msg3 font-position3 (float (font-base-size font3)) 2 +white+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-font font1)               ; Font unloading
      (unload-font font2)               ; Font unloading
      (unload-font font3)               ; Font unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
