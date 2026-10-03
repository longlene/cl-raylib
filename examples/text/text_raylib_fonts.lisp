;;;; text_raylib_fonts.lisp - Raylib fonts loading example
;;;; Translated from raylib/examples/text/text_raylib_fonts.c

(require :cl-raylib)

(defpackage :text-raylib-fonts
  (:use :cl :cl-raylib))

(in-package :text-raylib-fonts)

(defconstant +max-fonts+ 8)

(defun main ()
  "Main function - raylib fonts example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - raylib fonts")

    ;; Load fonts
    (let ((fonts (vector (load-font "resources/fonts/alagard.png")
                        (load-font "resources/fonts/pixelplay.png")
                        (load-font "resources/fonts/mecha.png")
                        (load-font "resources/fonts/setback.png")
                        (load-font "resources/fonts/romulus.png")
                        (load-font "resources/fonts/pixantiqua.png")
                        (load-font "resources/fonts/alpha_beta.png")
                        (load-font "resources/fonts/jupiter_crash.png")))
          
          (messages (vector "ALAGARD FONT designed by Hewett Tsoi"
                           "PIXELPLAY FONT designed by Aleksander Shevchuk"
                           "MECHA FONT designed by Captain Falcon"
                           "SETBACK FONT designed by Brian Kent (AEnigma)"
                           "ROMULUS FONT designed by Hewett Tsoi"
                           "PIXANTIQUA FONT designed by Gerhard Grossmann"
                           "ALPHA_BETA FONT designed by Brian Kent (AEnigma)"
                           "JUPITER_CRASH FONT designed by Brian Kent (AEnigma)"))
          
          (spacings (vector 2 4 8 4 3 4 4 1))
          
          (positions (make-array +max-fonts+))
          
          (colors (vector +maroon+ +orange+ +darkgreen+ +darkblue+ +darkpurple+ +lime+ +gold+ +red+)))

      ;; Calculate positions for each font
      (dotimes (i +max-fonts+)
        (let ((font (aref fonts i))
              (message (aref messages i))
              (spacing (aref spacings i)))
          (let ((text-size (measure-text-ex font message (* (font-base-size font) 2.0) (float spacing))))
            (setf (aref positions i) 
                  (vec2 (- (/ screen-width 2.0) (/ (vx text-size) 2.0))
                        (+ 60.0 (font-base-size font) (* 45.0 i)))))))

      ;; Small Y position corrections
      (incf (vy (aref positions 3)) 8)
      (incf (vy (aref positions 4)) 2)
      (decf (vy (aref positions 7)) 8)

      (set-target-fps 60)

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; No update logic needed for this example

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-text "free fonts included with raylib" 250 20 20 +darkgray+)
          (draw-text "Some fonts may be excluded from non-desktop platforms" 114 50 20 +gray+)

          ;; Draw all fonts
          (dotimes (i +max-fonts+)
            (let ((font (aref fonts i))
                  (message (aref messages i))
                  (position (aref positions i))
                  (spacing (aref spacings i))
                  (color (aref colors i)))
              (draw-text-ex font message position 
                           (* (font-base-size font) 2.0) 
                           (float spacing) 
                           color)))

        (end-drawing))

      ;; De-Initialization
      (dotimes (i +max-fonts+)
        (unload-font (aref fonts i))))

    ;; Close window
    (close-window)))

;; Run the example
(main)