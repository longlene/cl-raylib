(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defun main ()
  "raylib [text] example - Font loading"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [text] example - font loading")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Define characters to draw
      ;; NOTE: raylib supports UTF-8 encoding, following list is actually codified as UTF8 internally
      (let ((msg "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI
JKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn
opqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ
ÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷
øùúûüýþÿ"))

        ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

        ;; BMFont (AngelCode) : Font data and image atlas have been generated using external program
        (let ((font-bm (load-font "resources/pixantiqua.fnt"))
              ;; TTF font : Font data and atlas are generated directly from TTF
              ;; NOTE: We define a font base size of 32 pixels tall and up-to 250 characters
              (font-ttf (load-font-ex "resources/pixantiqua.ttf" 32 nil 250))
              (use-ttf nil))

          (set-text-line-spacing 16) ; Set line spacing for multiline text (when line breaks are included '\n')

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update
                 (setf use-ttf (is-key-down :key-space))

                 ;; Draw
                 (with-drawing
                   (clear-background :raywhite)

                   (draw-text "Hold SPACE to use TTF generated font" 20 20 20 :lightgray)

                   (if (not use-ttf)
                       (progn
                         (draw-text-ex font-bm msg (vec2 20.0 100.0) (font-base-size font-bm) 2 :maroon)
                         (draw-text "Using BMFont (Angelcode) imported" 20 (- (get-screen-height) 30) 20 :gray))
                       (progn
                         (draw-text-ex font-ttf msg (vec2 20.0 100.0) (font-base-size font-ttf) 2 :lime)
                         (draw-text "Using TTF font generated" 20 (- (get-screen-height) 30) 20 :gray))))))

          ;; Cleanup
          (unload-font font-bm)
          (unload-font font-ttf))))))

(main)