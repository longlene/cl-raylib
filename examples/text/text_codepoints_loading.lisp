;;;; raylib [text] example - codepoints loading
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_codepoints_loading.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-codepoints-loading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-codepoints-loading)

;; Text to be displayed, must be UTF-8 (save this code file as UTF-8)
;; NOTE: It can contain all the required text for the game,
;; this text will be scanned to get all the required codepoints
(defparameter *text* (format nil "いろはにほへと　ちりぬるを~%わかよたれそ　つねならむ~%うゐのおくやま　けふこえて~%あさきゆめみし　ゑひもせす"))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Remove codepoint duplicates if requested
;; WARNING: This process could be a bit slow if the text to process is very long
;; Returns (values codepoints-no-dups codepoints-no-dups-count)
(defun codepoint-remove-duplicates (codepoints codepoint-count)
  (let ((codepoints-no-dups-count codepoint-count)
        (codepoints-no-dups (subseq codepoints 0 codepoint-count)))

    ;; Remove duplicates
    (let ((i 0))
      (loop while (< i codepoints-no-dups-count)
            do (let ((j (1+ i)))
                 (loop while (< j codepoints-no-dups-count)
                       do (when (= (aref codepoints-no-dups i) (aref codepoints-no-dups j))
                            (loop for k from j below (1- codepoints-no-dups-count)
                                  do (setf (aref codepoints-no-dups k) (aref codepoints-no-dups (1+ k))))

                            (decf codepoints-no-dups-count)
                            (decf j))
                          (incf j)))
               (incf i)))

    ;; NOTE: The size of codepointsNoDups is the same as original array but
    ;; only required positions are filled (codepointsNoDupsCount)
    (values codepoints-no-dups codepoints-no-dups-count)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))
    (declare (ignorable screen-height))

    (init-window screen-width screen-height "raylib [text] example - codepoints loading")

    ;; Convert each utf-8 character into its
    ;; corresponding codepoint in the font file
    (multiple-value-bind (codepoints codepoint-count) (load-codepoints *text*)

      ;; Removed duplicate codepoints to generate smaller font atlas
      (multiple-value-bind (codepoints-no-dups codepoints-no-dups-count)
          (codepoint-remove-duplicates codepoints codepoint-count)
        (unload-codepoints codepoints)

        ;; Load font containing all the provided codepoint glyphs
        ;; A texture font atlas is automatically generated
        (let ((font (load-font-ex "resources/DotGothic16-Regular.ttf" 36 codepoints-no-dups codepoints-no-dups-count))
              (show-font-atlas nil)
              ;; NOTE: C keeps a char pointer, here PTR is a character index into *text*
              (ptr 0))

          ;; Set bilinear scale filter for better font scaling
          (set-texture-filter (font-texture font) +texture-filter-bilinear+)

          (set-text-line-spacing 20)    ; Set line spacing for multiline text (when line breaks are included '\n')

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (when (is-key-pressed +key-space+) (setf show-font-atlas (not show-font-atlas)))

                   ;; Testing code: getting next and previous codepoints on provided text
                   (cond ((is-key-pressed +key-right+)
                          ;; Get next codepoint in string and move pointer
                          (get-codepoint-next *text* ptr)
                          (setf ptr (min (1+ ptr) (length *text*))))
                         ((is-key-pressed +key-left+)
                          ;; Get previous codepoint in string and move pointer
                          (get-codepoint-previous *text* ptr)
                          (setf ptr (max (1- ptr) 0))))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (draw-rectangle 0 0 (get-screen-width) 70 +black+)
                   (draw-text (text-format "Total codepoints contained in provided text: %i" codepoint-count) 10 10 20 +green+)
                   (draw-text (text-format "Total codepoints required for font atlas (duplicates excluded): %i" codepoints-no-dups-count) 10 40 20 +green+)

                   (if show-font-atlas
                       (progn
                         ;; Draw generated font texture atlas containing provided codepoints
                         (draw-texture (font-texture font) 150 100 +black+)
                         (draw-rectangle-lines 150 100 (texture-width (font-texture font)) (texture-height (font-texture font)) +black+))
                       ;; Draw provided text with loaded font, containing all required codepoint glyphs
                       (draw-text-ex font *text* (vec2 160.0 110.0) 48 5 +black+))

                   (draw-text "Press SPACE to toggle font atlas view!" 10 (- (get-screen-height) 30) 20 +gray+)

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-font font)            ; Unload font

          (close-window))))))           ; Close window and OpenGL context

(main)
