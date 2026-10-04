;;;; raylib [text] example - font loading
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: raylib can load fonts from multiple input file formats:
;;;;
;;;;   - TTF/OTF > Sprite font atlas is generated on loading, user can configure
;;;;               some of the generation parameters (size, characters to include)
;;;;   - BMFonts > Angel code font fileformat, sprite font image must be provided
;;;;               together with the .fnt file, font generation can not be configured
;;;;   - XNA Spritefont > Sprite font image, following XNA Spritefont conventions,
;;;;               Characters in image must follow some spacing and order rules
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 3.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_font_loading.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-font-loading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-font-loading)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - font loading")

    ;; Define characters to draw
    ;; NOTE: raylib supports UTF-8 encoding, following list is actually codified as UTF8 internally
    (let* ((msg (format nil "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI~%JKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklm~%nopqrstuvwxyz{|}~~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒ~%ÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö~%÷øùúûüýþÿ"))

           ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

           ;; BMFont (AngelCode) : Font data and image atlas have been generated using external program
           (font-bm (load-font "resources/pixantiqua.fnt")) ; Requires "resources/pixantiqua.png"

           ;; TTF font : Font data and atlas are generated directly from TTF
           ;; NOTE: We define a font base size of 32 pixels tall and up-to 250 characters
           (font-ttf (load-font-ex "resources/pixantiqua.ttf" 32 nil 250))

           (use-ttf nil))

      (set-text-line-spacing 16)        ; Set line spacing for multiline text (when line breaks are included '\n')

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf use-ttf (is-key-down +key-space+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "Hold SPACE to use TTF generated font" 20 20 20 +lightgray+)

               (if (not use-ttf)
                   (progn
                     (draw-text-ex font-bm msg (vec2 20.0 100.0) (float (font-base-size font-bm)) 2 +maroon+)
                     (draw-text "Using BMFont (Angelcode) imported" 20 (- (get-screen-height) 30) 20 +gray+))
                   (progn
                     (draw-text-ex font-ttf msg (vec2 20.0 100.0) (float (font-base-size font-ttf)) 2 +lime+)
                     (draw-text "Using TTF font generated" 20 (- (get-screen-height) 30) 20 +gray+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-font font-bm)             ; AngelCode Font unloading
      (unload-font font-ttf)            ; TTF Font unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
