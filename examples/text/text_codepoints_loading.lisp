;;;; text_codepoints_loading.lisp - Codepoints loading example
;;;; Translated from raylib/examples/text/text_codepoints_loading.c

(require :cl-raylib)

(defpackage :text-codepoints-loading
  (:use :cl :cl-raylib))

(in-package :text-codepoints-loading)

;; Text to be displayed, must be UTF-8
;; NOTE: It can contain all the required text for the game,
;; this text will be scanned to get all the required codepoints
(defparameter *text* "いろはにほへと　ちりぬるを
わかよたれそ　つねならむ
うゐのおくやま　けふこえて
あさきゆめみし　ゑひもせす")

(defun codepoint-remove-duplicates (codepoints)
  "Remove codepoint duplicates from array"
  (let ((result (make-array (length codepoints) :fill-pointer 0)))
    (loop for codepoint across codepoints do
      (unless (find codepoint result)
        (vector-push codepoint result)))
    result))

(defun main ()
  "Main function - codepoints loading example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - codepoints loading")

    ;; Convert each utf-8 character into its corresponding codepoint in the font file
    (let* ((codepoints (load-codepoints *text*))
           (codepoint-count (length codepoints))
           
           ;; Remove duplicate codepoints to generate smaller font atlas
           (codepoints-no-dups (codepoint-remove-duplicates codepoints))
           (codepoints-no-dups-count (length codepoints-no-dups)))

      ;; Load font containing all the provided codepoint glyphs
      ;; A texture font atlas is automatically generated
      (let ((font (load-font-ex "resources/DotGothic16-Regular.ttf" 36 
                                (coerce codepoints-no-dups 'vector))))

        ;; Set bilinear scale filter for better font scaling
        (set-texture-filter (font-texture font) +texture-filter-bilinear+)

        (set-text-line-spacing 20)  ; Set line spacing for multiline text

        (let ((show-font-atlas nil)
              (codepoint-size 0)
              (ptr 0))  ; Character pointer position

          (set-target-fps 60)

          ;; Main game loop
          (loop until (window-should-close) do
            ;; Update
            (when (is-key-pressed +key-space+)
              (setf show-font-atlas (not show-font-atlas)))

            ;; Testing code: getting next and previous codepoints on provided text
            (when (is-key-pressed +key-right+)
              ;; Get next codepoint in string and move pointer
              (multiple-value-bind (codepoint size)
                  (get-codepoint-next *text* ptr)
                (declare (ignore codepoint))
                (setf codepoint-size size)
                (incf ptr codepoint-size)))

            (when (is-key-pressed +key-left+)
              ;; Get previous codepoint in string and move pointer
              (when (> ptr 0)
                (multiple-value-bind (codepoint size)
                    (get-codepoint-previous *text* ptr)
                  (declare (ignore codepoint))
                  (setf codepoint-size size)
                  (decf ptr codepoint-size))))

            ;; Draw
            (begin-drawing)
              (clear-background +raywhite+)

              (draw-rectangle 0 0 (get-screen-width) 70 +black+)
              (draw-text (format nil "Total codepoints contained in provided text: ~a" codepoint-count) 
                        10 10 20 +green+)
              (draw-text (format nil "Total codepoints required for font atlas (duplicates excluded): ~a" codepoints-no-dups-count) 
                        10 40 20 +green+)

              (if show-font-atlas
                  (progn
                    ;; Draw generated font texture atlas containing provided codepoints
                    (draw-texture (font-texture font) 150 100 +black+)
                    (draw-rectangle-lines 150 100 
                                         (texture-width (font-texture font)) 
                                         (texture-height (font-texture font)) 
                                         +black+))
                  (progn
                    ;; Draw provided text with loaded font, containing all required codepoint glyphs
                    (draw-text-ex font *text* (vec2 160.0 110.0) 48 5 +black+)))

              (draw-text "Press SPACE to toggle font atlas view!" 10 (- (get-screen-height) 30) 20 +gray+)

            (end-drawing)))

        ;; De-Initialization
        (unload-font font)))

    ;; Close window
    (close-window)))

;; Run the example
(main)