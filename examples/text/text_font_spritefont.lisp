;;;; text_font_spritefont.lisp - Sprite font loading example
;;;; Translated from raylib/examples/text/text_font_spritefont.c

(require :cl-raylib)

(defpackage :text-font-spritefont
  (:use :cl :cl-raylib))

(in-package :text-font-spritefont)

(defun main ()
  "Main function - sprite font loading example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - sprite font loading")

    (let ((msg1 "THIS IS A custom SPRITE FONT...")
          (msg2 "...and this is ANOTHER CUSTOM font...")
          (msg3 "...and a THIRD one! GREAT! :D"))

      ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)
      (let ((font1 (load-font "examples/text/resources/custom_mecha.png"))
            (font2 (load-font "examples/text/resources/custom_alagard.png"))
            (font3 (load-font "examples/text/resources/custom_jupiter_crash.png")))

        ;; Calculate font positions to center text
        (let ((font-position1 (vec2 (- (/ screen-width 2.0)
                                       (/ (vx (measure-text-ex font1 msg1 (float (font-base-size font1)) -3.0)) 2.0))
                                    (- (/ screen-height 2.0)
                                       (/ (font-base-size font1) 2.0) 80.0)))
              (font-position2 (vec2 (- (/ screen-width 2.0)
                                       (/ (vx (measure-text-ex font2 msg2 (float (font-base-size font2)) -2.0)) 2.0))
                                    (- (/ screen-height 2.0)
                                       (/ (font-base-size font2) 2.0) 10.0)))
              (font-position3 (vec2 (- (/ screen-width 2.0)
                                       (/ (vx (measure-text-ex font3 msg3 (float (font-base-size font3)) 2.0)) 2.0))
                                    (- (/ screen-height 2.0)
                                       (/ (font-base-size font3) 2.0) -50.0))))

          (set-target-fps 60) ; Set game to run at 60 frames-per-second

          ;; Main game loop
          (loop until (window-should-close) do
            ;; Update
            ;; TODO: Update variables here...

            ;; Draw
            (begin-drawing)
              (clear-background +raywhite+)

              (draw-text-ex font1 msg1 font-position1 (float (font-base-size font1)) -3.0 +white+)
              (draw-text-ex font2 msg2 font-position2 (float (font-base-size font2)) -2.0 +white+)
              (draw-text-ex font3 msg3 font-position3 (float (font-base-size font3)) 2.0 +white+)

            (end-drawing))

          ;; De-Initialization
          (unload-font font1)
          (unload-font font2)
          (unload-font font3))))

    ;; Close window
    (close-window)))

;; Run the example
(main)