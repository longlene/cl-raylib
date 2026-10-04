;;;; raylib [text] example - font sdf
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_font_sdf.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-font-sdf
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-font-sdf)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - font sdf")

    ;; NOTE: Textures/Fonts MUST be loaded after Window initialization (OpenGL context is required)

    (let ((msg "Signed Distance Fields")
          (font-default (make-font))
          (font-sdf (make-font)))

      ;; Loading file to memory
      (multiple-value-bind (file-data file-size) (load-file-data "resources/anonymous_pro_bold.ttf")

        ;; Default font generation from TTF font
        (setf (font-base-size font-default) 16
              (font-glyph-count font-default) 95)

        ;; Loading font data from memory data
        ;; Parameters > font size: 16, no glyphs array provided (0), glyphs count: 95 (autogenerate chars array)
        (multiple-value-bind (glyphs count) (load-font-data file-data file-size 16 nil 95 +font-default+)
          (setf (font-glyphs font-default) glyphs
                (font-glyph-count font-default) count))
        ;; Parameters > glyphs count: 95, font size: 16, glyphs padding in image: 4 px, pack method: 0 (default)
        (multiple-value-bind (atlas recs) (gen-image-font-atlas (font-glyphs font-default) 95 16 4 0)
          (setf (font-recs font-default) recs
                (font-texture font-default) (load-texture-from-image atlas))
          (unload-image atlas))

        ;; SDF font generation from TTF font
        (setf (font-base-size font-sdf) 16
              (font-glyph-count font-sdf) 95)
        ;; Parameters > font size: 16, no glyphs array provided (0), glyphs count: 0 (defaults to 95)
        (multiple-value-bind (glyphs count) (load-font-data file-data file-size 16 nil 0 +font-sdf+)
          (setf (font-glyphs font-sdf) glyphs
                (font-glyph-count font-sdf) count))
        ;; Parameters > glyphs count: 95, font size: 16, glyphs padding in image: 0 px, pack method: 1 (Skyline algorythm)
        (multiple-value-bind (atlas recs) (gen-image-font-atlas (font-glyphs font-sdf) 95 16 0 1)
          (setf (font-recs font-sdf) recs
                (font-texture font-sdf) (load-texture-from-image atlas))
          (unload-image atlas))

        (unload-file-data file-data))     ; Free memory from loaded file

      ;; Load SDF required shader (we use default vertex shader)
      (let ((shader (load-shader nil (text-format "resources/shaders/glsl%i/sdf.fs" +glsl-version+)))
            (font-position (vec2 40.0 (- (/ screen-height 2.0) 50)))
            (text-size (vec2 0.0 0.0))
            (font-size 16.0)
            (current-font 0))           ; 0 - fontDefault, 1 - fontSDF

        (set-texture-filter (font-texture font-sdf) +texture-filter-bilinear+) ; Required for SDF font

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (incf font-size (* (get-mouse-wheel-move) 8.0))

                 (when (< font-size 6) (setf font-size 6.0))

                 (setf current-font (if (is-key-down +key-space+) 1 0))

                 (setf text-size (if (= current-font 0)
                                     (measure-text-ex font-default msg font-size 0)
                                     (measure-text-ex font-sdf msg font-size 0)))

                 (setf (vx font-position) (- (/ (float (get-screen-width)) 2) (/ (vx text-size) 2))
                       (vy font-position) (+ (- (/ (float (get-screen-height)) 2) (/ (vy text-size) 2)) 80))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (if (= current-font 1)
                     (progn
                       ;; NOTE: SDF fonts require a custom SDf shader to compute fragment color
                       (begin-shader-mode shader) ; Activate SDF font shader
                       (draw-text-ex font-sdf msg font-position font-size 0 +black+)
                       (end-shader-mode)          ; Activate our default shader for next drawings

                       (draw-texture (font-texture font-sdf) 10 10 +black+))
                     (progn
                       (draw-text-ex font-default msg font-position font-size 0 +black+)
                       (draw-texture (font-texture font-default) 10 10 +black+)))

                 (if (= current-font 1)
                     (draw-text "SDF!" 320 20 80 +red+)
                     (draw-text "default font" 315 40 30 +gray+))

                 (draw-text "FONT SIZE: 16.0" (- (get-screen-width) 240) 20 20 +darkgray+)
                 (draw-text (text-format "RENDER SIZE: %02.02f" font-size) (- (get-screen-width) 240) 50 20 +darkgray+)
                 (draw-text "Use MOUSE WHEEL to SCALE TEXT!" (- (get-screen-width) 240) 90 10 +darkgray+)

                 (draw-text "HOLD SPACE to USE SDF FONT VERSION!" 340 (- (get-screen-height) 30) 20 +maroon+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-font font-default)      ; Default font unloading
        (unload-font font-sdf)          ; SDF font unloading

        (unload-shader shader)          ; Unload SDF shader

        (close-window)))))              ; Close window and OpenGL context

(main)
