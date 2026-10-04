;;;; raylib [text] example - unicode ranges
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Vadim Gunko (@GuvaCode) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Vadim Gunko (@GuvaCode) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_unicode_ranges.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-unicode-ranges
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-unicode-ranges)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Add codepoint range to existing font
;; NOTE: C updates the font through a pointer, here the new font is returned
(defun add-codepoint-range (font font-path start stop)
  (let* ((range-size (1+ (- stop start)))
         (current-range-size (font-glyph-count font))

         ;; TODO: Load glyphs from provided vector font (if available),
         ;; add them to existing font, regenerating font image and texture

         (updated-codepoint-count (+ current-range-size range-size))
         (updated-codepoints (make-array updated-codepoint-count :initial-element 0)))

    ;; Get current codepoint list
    (dotimes (i current-range-size) (setf (aref updated-codepoints i) (glyph-info-value (aref (font-glyphs font) i))))

    ;; Add new codepoints to list (provided range)
    (loop for i from current-range-size below updated-codepoint-count
          do (setf (aref updated-codepoints i) (+ start (- i current-range-size))))

    (unload-font font)
    (load-font-ex font-path 32 updated-codepoints updated-codepoint-count)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - unicode ranges")

    ;; Load font with default Unicode range: Basic ASCII [32-127]
    (let ((font (load-font "resources/NotoSansTC-Regular.ttf"))
          (unicode-range 0)             ; Track the ranges of codepoints added to font
          (prev-unicode-range 0))       ; Previous Unicode range to avoid reloading every frame

      (set-texture-filter (font-texture font) +texture-filter-bilinear+)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (/= unicode-range prev-unicode-range)
                 (unload-font font)

                 ;; Load font with default Unicode range: Basic ASCII [32-127]
                 (setf font (load-font "resources/NotoSansTC-Regular.ttf"))

                 ;; Add required ranges to loaded font
                 ;; NOTE: The C switch falls through, every range also adds the lower ones
                 (flet ((add (start stop) (setf font (add-codepoint-range font "resources/NotoSansTC-Regular.ttf" start stop))))
                   (when (>= unicode-range 4)
                     ;; Unicode range: CJK (Japanese and Chinese)
                     ;; WARNING: Loading thousands of codepoints requires lot of time!
                     ;; A better strategy is prefilter the required codepoints for the text
                     ;; in the game and just load the required ones
                     (add #x4e00 #x9fff)
                     (add #x3400 #x4dbf)
                     (add #x3000 #x303f)
                     (add #x3040 #x309f)
                     (add #x30A0 #x30ff)
                     (add #x31f0 #x31ff)
                     (add #xff00 #xffef)
                     (add #xac00 #xd7af)
                     (add #x1100 #x11ff))
                   (when (>= unicode-range 3)
                     ;; Unicode range: Cyrillic
                     (add #x400 #x4ff)
                     (add #x500 #x52f)
                     (add #x2de0 #x2Dff)
                     (add #xa640 #xA69f))
                   (when (>= unicode-range 2)
                     ;; Unicode range: Greek
                     (add #x370 #x3ff)
                     (add #x1f00 #x1fff))
                   (when (>= unicode-range 1)
                     ;; Unicode range: European Languages
                     (add #xc0 #x17f)
                     (add #x180 #x24f)
                     ;;(add #x1e00 #x1eff)
                     ;;(add #x2c60 #x2c7f)
                     ))

                 (setf prev-unicode-range unicode-range)
                 (set-texture-filter (font-texture font) +texture-filter-bilinear+)) ; Set font atlas scale filter

               (cond ((is-key-pressed +key-zero+) (setf unicode-range 0))
                     ((is-key-pressed +key-one+) (setf unicode-range 1))
                     ((is-key-pressed +key-two+) (setf unicode-range 2))
                     ((is-key-pressed +key-three+) (setf unicode-range 3))
                     ((is-key-pressed +key-four+) (setf unicode-range 4)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "ADD CODEPOINTS: [1][2][3][4]" 20 20 20 +maroon+)

               ;; Render test strings in different languages
               (draw-text-ex font "> English: Hello World!" (vec2 50.0 70.0) 32 1 +darkgray+) ; English
               (draw-text-ex font "> Español: Hola mundo!" (vec2 50.0 120.0) 32 1 +darkgray+) ; Spanish
               (draw-text-ex font "> Ελληνικά: Γειά σου κόσμε!" (vec2 50.0 170.0) 32 1 +darkgray+) ; Greek
               (draw-text-ex font "> Русский: Привет мир!" (vec2 50.0 220.0) 32 0 +darkgray+) ; Russian
               (draw-text-ex font "> 中文: 你好世界!" (vec2 50.0 270.0) 32 1 +darkgray+) ; Chinese
               (draw-text-ex font "> 日本語: こんにちは世界!" (vec2 50.0 320.0) 32 1 +darkgray+) ; Japanese
               ;;(draw-text-ex font "देवनागरी: होला मुंडो!" (vec2 50.0 350.0) 32 1 +darkgray+) ; Devanagari (glyphs not available in font)

               ;; Draw font texture scaled to screen
               (let* ((texture (font-texture font))
                      (atlas-scale (/ 380.0 (texture-width texture))))
                 (draw-rectangle-rec (make-rectangle :x 400.0 :y 16.0 :width (* (texture-width texture) atlas-scale) :height (* (texture-height texture) atlas-scale)) +black+)
                 (draw-texture-pro texture (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width texture)) :height (float (texture-height texture)))
                                   (make-rectangle :x 400.0 :y 16.0 :width (* (texture-width texture) atlas-scale) :height (* (texture-height texture) atlas-scale)) (vec2 0.0 0.0) 0.0 +white+)
                 (draw-rectangle-lines 400 16 380 380 +red+)

                 (draw-text (text-format "ATLAS SIZE: %ix%i px (x%02.2f)" (texture-width texture) (texture-height texture) atlas-scale) 20 380 20 +blue+))
               (draw-text (text-format "CODEPOINTS GLYPHS LOADED: %i" (font-glyph-count font)) 20 410 20 +lime+)

               ;; Display font attribution
               (draw-text "Font: Noto Sans TC. License: SIL Open Font License 1.1" (- screen-width 300) (- screen-height 20) 10 +gray+)

               (when (/= prev-unicode-range unicode-range)
                 (draw-rectangle 0 0 screen-width screen-height (fade +white+ 0.8))
                 (draw-rectangle 0 125 screen-width 200 +gray+)
                 (draw-text "GENERATING FONT ATLAS..." 120 210 40 +black+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-font font)                ; Unload font resource

      (close-window))))                 ; Close window and OpenGL context

(main)
