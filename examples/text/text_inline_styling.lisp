;;;; raylib [text] example - inline styling
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Wagner Barongello (@SultansOfCode) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Wagner Barongello (@SultansOfCode) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_inline_styling.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-inline-styling
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-inline-styling)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; NOTE: C walks the UTF-8 bytes of TEXT, here I indexes the characters of the Lisp string,
;; so every codepoint takes one position

(defun hex-digit-p (ch)
  (or (char<= #\0 ch #\9) (char<= #\A ch #\F) (char<= #\a ch #\f)))

(defun hex-count (text start)
  "Number of hex digits in TEXT from START, stopping at the end or at ']'"
  (loop for n from 0
        for j = (+ start n)
        while (and (< j (length text)) (char/= (char text j) #\]) (hex-digit-p (char text j)))
        finally (return n)))

;; Draw text using inline styling
;; PARAM: color is the default text color, background color is BLANK by default
;; NOTE: Using input color as the base alpha multiplied to inline styles
(defun draw-text-styled (font text position font-size spacing color)
  ;; Text inline styling strategy used: [ ] delimiters for format
  ;; - Define foreground color:      [cRRGGBBAA]
  ;; - Define background color:      [bRRGGBBAA]
  ;; - Reset formating:              [r]
  ;; Example: [bAA00AAFF][cFF0000FF]red text on gray background[r] normal text

  (when (= (texture-id (font-texture font)) 0) (setf font (get-font-default)))

  (let ((text-len (length text))

        (col-front color)
        (col-back +blank+)
        (back-rec-padding 4)            ; Background rectangle padding

        (text-offset-y 0.0)
        (text-offset-x 0.0)
        (text-line-spacing 0.0)
        (scale-factor (/ font-size (font-base-size font)))
        (i 0))

    (loop while (< i text-len)
          do (block next
               (let ((codepoint (char-code (char text i))))

                 (if (= codepoint (char-code #\Newline))
                     (progn
                       (incf text-offset-y (+ font-size text-line-spacing))
                       (setf text-offset-x 0.0))
                     (progn
                       (when (= codepoint (char-code #\[)) ; Process pipe styling
                         (cond ((and (< (+ i 2) text-len) (char= (char text (+ i 1)) #\r) (char= (char text (+ i 2)) #\])) ; Reset styling
                                (setf col-front color
                                      col-back +blank+)

                                (incf i 3)      ; Skip "[r]"
                                (return-from next)) ; Do not draw characters
                               ((and (< (+ i 1) text-len) (member (char text (+ i 1)) '(#\c #\b)))
                                (incf i 2)      ; Skip "[c" or "[b" to start parsing color

                                ;; Parse following color
                                (let* ((col-hex-count (hex-count text i))
                                       ;; Convert hex color text into actual Color
                                       (col-hex-value (if (> col-hex-count 0) (parse-integer text :start i :end (+ i col-hex-count) :radix 16) 0)))
                                  (case (char text (1- i))
                                    (#\c (setf col-front (get-color col-hex-value)))
                                    ;;colFront.a *= (unsigned char)(colFront.a*(float)color.a/255.0f); // TODO: Review
                                    (#\b (setf col-back (get-color col-hex-value))))
                                    ;;colBack.a *= (unsigned char)(colFront.a*(float)color.a/255.0f);

                                  (incf i (1+ col-hex-count))) ; Skip color value retrieved and ']'
                                (return-from next)))) ; Do not draw characters

                       (let* ((index (get-glyph-index font codepoint))
                              (glyph (aref (font-glyphs font) index))
                              (increase-x (if (= (glyph-info-advance-x glyph) 0)
                                              (+ (* (float (rectangle-width (aref (font-recs font) index))) scale-factor) spacing)
                                              (+ (* (float (glyph-info-advance-x glyph)) scale-factor) spacing))))

                         ;; Draw background rectangle color (if required)
                         (when (> (fourth col-back) 0)
                           (draw-rectangle-rec (make-rectangle :x (+ (vx position) text-offset-x) :y (- (+ (vy position) text-offset-y) back-rec-padding)
                                                               :width increase-x :height (+ font-size (* 2 back-rec-padding)))
                                               col-back))

                         (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab)))
                           (draw-text-codepoint font codepoint (vec2 (+ (vx position) text-offset-x) (+ (vy position) text-offset-y)) font-size col-front))

                         (incf text-offset-x increase-x))))

                 (incf i))))))

;; Measure inline styled text
;; NOTE: Measuring styled text requires skipping styling data
;; WARNING: Not considering line breaks
;; WARNING: Like C, a '[' not starting a style or a '\n' is never skipped (endless loop)
(defun measure-text-styled (font text font-size spacing)
  (let ((text-size (vec2 0.0 0.0)))

    (when (or (= (texture-id (font-texture font)) 0) (null text) (= (length text) 0)) (return-from measure-text-styled text-size)) ; Security check

    (let ((text-len (length text))
          ;;(text-line-spacing (* font-size 1.5)) ; Not used...

          (text-width 0.0)
          (text-height font-size)
          (scale-factor (/ font-size (float (font-base-size font))))

          (valid-codepoint-counter 0)
          (i 0))

      (loop while (< i text-len)
            do (let ((codepoint (char-code (char text i))))
                 (cond ((= codepoint (char-code #\[)) ; Ignore pipe inline styling
                        (cond ((and (< (+ i 2) text-len) (char= (char text (+ i 1)) #\r) (char= (char text (+ i 2)) #\])) ; Reset styling
                               (incf i 3))   ; Skip "[r]"
                              ((and (< (+ i 1) text-len) (member (char text (+ i 1)) '(#\c #\b)))
                               (incf i 2)    ; Skip "[c" or "[b" to start parsing color
                               (incf i (1+ (hex-count text i)))))) ; Skip color value retrieved and ']'
                       ((/= codepoint (char-code #\Newline))
                        (let* ((index (get-glyph-index font codepoint))
                               (glyph (aref (font-glyphs font) index)))
                          (if (> (glyph-info-advance-x glyph) 0)
                              (incf text-width (glyph-info-advance-x glyph))
                              (incf text-width (+ (rectangle-width (aref (font-recs font) index)) (glyph-info-offset-x glyph))))

                          (incf valid-codepoint-counter)
                          (incf i))))))

      (setf (vx text-size) (+ (* text-width scale-factor) (* (1- valid-codepoint-counter) spacing))
            (vy text-size) text-height)

      text-size)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - inline styling")

    (let ((text-size (vec2 0.0 0.0))    ; Measure text box for provided font and text
          (col-random (copy-list +red+)) ; Random color used on text
          (frame-counter 0))            ; Used to generate a new random color every certain frames

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf frame-counter)

               (when (= (mod frame-counter 20) 0)
                 (setf col-random (list (get-random-value 0 255)
                                        (get-random-value 0 255)
                                        (get-random-value 0 255)
                                        255)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Text inline styling strategy used: [ ] delimiters for format
               ;; - Define foreground color:      [cRRGGBBAA]
               ;; - Define background color:      [bRRGGBBAA]
               ;; - Reset formating:              [r]
               ;; Colors defined with [cRRGGBBAA] or [bRRGGBBAA] are multiplied by the base color alpha
               ;; This allows global transparency control while keeping per-section styling (ex. text fade effects)
               ;; Example: [bAA00AAFF][cFF0000FF]red text on gray background[r] normal text

               (draw-text-styled (get-font-default) "This changes the [cFF0000FF]foreground color[r] of provided text!!!"
                                 (vec2 100.0 80.0) 20.0 2.0 +black+)

               (draw-text-styled (get-font-default) "This changes the [bFF00FFFF]background color[r] of provided text!!!"
                                 (vec2 100.0 120.0) 20.0 2.0 +black+)

               (draw-text-styled (get-font-default) "This changes the [c00ff00ff][bff0000ff]foreground and background colors[r]!!!"
                                 (vec2 100.0 160.0) 20.0 2.0 +black+)

               (draw-text-styled (get-font-default) "This changes the [c00ff00ff]alpha[r] relative [cffffffff][b000000ff]from source[r] [cff000088]color[r]!!!"
                                 (vec2 100.0 200.0) 20.0 2.0 '(0 0 0 100))

               ;; Get formated text
               (let ((text (text-format "Let's be [c%02x%02x%02xFF]CREATIVE[r] !!!" (first col-random) (second col-random) (third col-random))))
                 (draw-text-styled (get-font-default) text (vec2 100.0 240.0) 40.0 2.0 +black+)

                 (setf text-size (measure-text-styled (get-font-default) text 40.0 2.0))
                 (draw-rectangle-lines 100 240 (truncate (vx text-size)) (truncate (vy text-size)) +green+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
