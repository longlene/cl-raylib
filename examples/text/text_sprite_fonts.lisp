;;;; raylib [text] example - sprite fonts
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: raylib is distributed with some free to use fonts (even for commercial pourposes!)
;;;;       To view details and credits for those fonts, check raylib license file
;;;;
;;;; Example originally created with raylib 1.7, last time updated with raylib 3.7
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_sprite_fonts.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-sprite-fonts
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-sprite-fonts)

(defconstant +max-fonts+ 8)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - sprite fonts")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let ((fonts (vector (load-font "resources/sprite_fonts/alagard.png")
                         (load-font "resources/sprite_fonts/pixelplay.png")
                         (load-font "resources/sprite_fonts/mecha.png")
                         (load-font "resources/sprite_fonts/setback.png")
                         (load-font "resources/sprite_fonts/romulus.png")
                         (load-font "resources/sprite_fonts/pixantiqua.png")
                         (load-font "resources/sprite_fonts/alpha_beta.png")
                         (load-font "resources/sprite_fonts/jupiter_crash.png")))

          (messages #("ALAGARD FONT designed by Hewett Tsoi"
                      "PIXELPLAY FONT designed by Aleksander Shevchuk"
                      "MECHA FONT designed by Captain Falcon"
                      "SETBACK FONT designed by Brian Kent (AEnigma)"
                      "ROMULUS FONT designed by Hewett Tsoi"
                      "PIXANTIQUA FONT designed by Gerhard Grossmann"
                      "ALPHA_BETA FONT designed by Brian Kent (AEnigma)"
                      "JUPITER_CRASH FONT designed by Brian Kent (AEnigma)"))

          (spacings #(2 4 8 4 3 4 4 1))

          (positions (make-array +max-fonts+))

          (colors (vector +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+ +lime+ +gold+ +red+)))

      (dotimes (i +max-fonts+)
        (setf (aref positions i)
              (vec2 (- (/ screen-width 2.0) (/ (vx (measure-text-ex (aref fonts i) (aref messages i) (* (font-base-size (aref fonts i)) 2.0) (float (aref spacings i)))) 2.0))
                    (+ 60.0 (font-base-size (aref fonts i)) (* 45.0 i)))))

      ;; Small Y position corrections
      (incf (vy (aref positions 3)) 8)
      (incf (vy (aref positions 4)) 2)
      (decf (vy (aref positions 7)) 8)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

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

               (draw-text "free sprite fonts included with raylib" 220 20 20 +darkgray+)
               (draw-line 220 50 600 50 +darkgray+)

               (dotimes (i +max-fonts+)
                 (draw-text-ex (aref fonts i) (aref messages i) (aref positions i) (* (font-base-size (aref fonts i)) 2.0) (float (aref spacings i)) (aref colors i)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------

      ;; Fonts unloading
      (dotimes (i +max-fonts+) (unload-font (aref fonts i)))

      (close-window))))                 ; Close window and OpenGL context

(main)
