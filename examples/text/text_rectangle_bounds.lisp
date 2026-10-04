;;;; raylib [text] example - rectangle bounds
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_rectangle_bounds.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-rectangle-bounds
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-rectangle-bounds)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------

;; Draw text using font inside rectangle limits with support for text selection
;; NOTE: C walks the UTF-8 bytes of TEXT, here I and K index the characters of the Lisp
;; string, so every codepoint takes one position (codepointByteCount is 1)
(defun draw-text-boxed-selectable (font text rec font-size spacing word-wrap tint select-start select-length select-tint select-back-tint)
  (let* ((length (length text))         ; Total length of the text, scanned by codepoints in loop

         (text-offset-y 0.0)            ; Offset between lines (on line break '\n')
         (text-offset-x 0.0)            ; Offset X to next character to draw

         (scale-factor (/ font-size (float (font-base-size font)))) ; Character rectangle scaling factor

         ;; Word/character wrapping mechanism variables
         (measure-state 0)
         (draw-state 1)
         (state (if word-wrap measure-state draw-state))

         (start-line -1)                ; Index where to begin drawing (where a line begins)
         (end-line -1)                  ; Index where to stop drawing (where a line ends)
         (lastk -1)                     ; Holds last value of the character position
         (codepoint-byte-count 1)
         (i 0)
         (k 0))

    (loop while (< i length)
          do (let* (;; Get next codepoint from string and glyph index in font
                    (codepoint (char-code (char text i)))
                    (index (get-glyph-index font codepoint))
                    (glyph-width 0.0))

               (when (/= codepoint (char-code #\Newline))
                 (setf glyph-width (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                                       (* (rectangle-width (aref (font-recs font) index)) scale-factor)
                                       (* (glyph-info-advance-x (aref (font-glyphs font) index)) scale-factor)))

                 (when (< (1+ i) length) (setf glyph-width (+ glyph-width spacing))))

               ;; NOTE: When wordWrap is ON we first measure how much of the text we can draw before going outside of the rec container
               ;; We store this info in startLine and endLine, then we change states, draw the text between those two variables
               ;; and change states again and again recursively until the end of the text (or until we get outside of the container)
               ;; When wordWrap is OFF we don't need the measure state so we go to the drawing state immediately
               ;; and begin drawing on the next line before we can get outside the container
               (if (= state measure-state)
                   (progn
                     ;; TODO: There are multiple types of spaces in UNICODE, maybe it's a good idea to add support for more
                     ;; Ref: http://jkorpela.fi/chars/spaces.html
                     (when (member codepoint (list (char-code #\Space) (char-code #\Tab) (char-code #\Newline))) (setf end-line i))

                     (cond ((> (+ text-offset-x glyph-width) (rectangle-width rec))
                            (setf end-line (if (< end-line 1) i end-line))
                            (when (= i end-line) (decf end-line codepoint-byte-count))
                            (when (= (+ start-line codepoint-byte-count) end-line) (setf end-line (- i codepoint-byte-count)))

                            (setf state (- 1 state)))
                           ((= (1+ i) length)
                            (setf end-line i)
                            (setf state (- 1 state)))
                           ((= codepoint (char-code #\Newline)) (setf state (- 1 state))))

                     (when (= state draw-state)
                       (setf text-offset-x 0.0
                             i start-line
                             glyph-width 0.0)

                       ;; Save character position when we switch states
                       (let ((tmp lastk))
                         (setf lastk (1- k)
                               k tmp))))
                   (progn
                     (if (= codepoint (char-code #\Newline))
                         (unless word-wrap
                           (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                           (setf text-offset-x 0.0))
                         (progn
                           (when (and (not word-wrap) (> (+ text-offset-x glyph-width) (rectangle-width rec)))
                             (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                             (setf text-offset-x 0.0))

                           ;; When text overflows rectangle height limit, just stop drawing
                           (when (> (+ text-offset-y (* (font-base-size font) scale-factor)) (rectangle-height rec)) (return))

                           ;; Draw selection background
                           (let ((is-glyph-selected nil))
                             (when (and (>= select-start 0) (>= k select-start) (< k (+ select-start select-length)))
                               (draw-rectangle-rec (make-rectangle :x (- (+ (rectangle-x rec) text-offset-x) 1) :y (+ (rectangle-y rec) text-offset-y)
                                                                   :width glyph-width :height (* (float (font-base-size font)) scale-factor))
                                                   select-back-tint)
                               (setf is-glyph-selected t))

                             ;; Draw current character glyph
                             (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab)))
                               (draw-text-codepoint font codepoint (vec2 (+ (rectangle-x rec) text-offset-x) (+ (rectangle-y rec) text-offset-y))
                                                    font-size (if is-glyph-selected select-tint tint))))))

                     (when (and word-wrap (= i end-line))
                       (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                       (setf text-offset-x 0.0
                             start-line end-line
                             end-line -1
                             glyph-width 0.0)
                       (incf select-start (- lastk k))
                       (setf k lastk)

                       (setf state (- 1 state)))))

               (when (or (/= text-offset-x 0) (/= codepoint (char-code #\Space))) (incf text-offset-x glyph-width))) ; avoid leading spaces
             (incf i)
             (incf k))))

;; Draw text using font inside rectangle limits
(defun draw-text-boxed (font text rec font-size spacing word-wrap tint)
  (draw-text-boxed-selectable font text rec font-size spacing word-wrap tint 0 0 +white+ +white+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - rectangle bounds")

    (let* ((text (format nil "Text cannot escape~Cthis container~C...word wrap also works when active so here's ~
a long text for testing.~%~%Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod ~
tempor incididunt ut labore et dolore magna aliqua. Nec ullamcorper sit amet risus nullam eget felis eget." #\Tab #\Tab))

           (resizing nil)
           (word-wrap t)

           (container (make-rectangle :x 25.0 :y 25.0 :width (- screen-width 50.0) :height (- screen-height 250.0)))
           (resizer (make-rectangle :x (- (+ (rectangle-x container) (rectangle-width container)) 17) :y (- (+ (rectangle-y container) (rectangle-height container)) 17) :width 14.0 :height 14.0))

           ;; Minimum width and heigh for the container rectangle
           (min-width 60.0)
           (min-height 60.0)
           (max-width (- screen-width 50.0))
           (max-height (- screen-height 160.0))

           (last-mouse (vec2 0.0 0.0))  ; Stores last mouse coordinates
           (border-color +maroon+)      ; Container border color
           (font (get-font-default)))   ; Get default system font

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+) (setf word-wrap (not word-wrap)))

               (let ((mouse (get-mouse-position)))

                 ;; Check if the mouse is inside the container and toggle border color
                 (cond ((check-collision-point-rec mouse container) (setf border-color (fade +maroon+ 0.4)))
                       ((not resizing) (setf border-color +maroon+)))

                 ;; Container resizing logic
                 (if resizing
                     (progn
                       (when (is-mouse-button-released +mouse-button-left+) (setf resizing nil))

                       (let ((width (+ (rectangle-width container) (- (vx mouse) (vx last-mouse)))))
                         (setf (rectangle-width container) (if (> width min-width) (if (< width max-width) width max-width) min-width)))

                       (let ((height (+ (rectangle-height container) (- (vy mouse) (vy last-mouse)))))
                         (setf (rectangle-height container) (if (> height min-height) (if (< height max-height) height max-height) min-height))))
                     ;; Check if we're resizing
                     (when (and (is-mouse-button-down +mouse-button-left+) (check-collision-point-rec mouse resizer)) (setf resizing t)))

                 ;; Move resizer rectangle properly
                 (setf (rectangle-x resizer) (- (+ (rectangle-x container) (rectangle-width container)) 17)
                       (rectangle-y resizer) (- (+ (rectangle-y container) (rectangle-height container)) 17))

                 (setf last-mouse mouse))       ; Update mouse
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-rectangle-lines-ex container 3 border-color) ; Draw container border

               ;; Draw text in container (add some padding)
               (draw-text-boxed font text (make-rectangle :x (+ (rectangle-x container) 4) :y (+ (rectangle-y container) 4)
                                                          :width (- (rectangle-width container) 4) :height (- (rectangle-height container) 4))
                                20.0 2.0 word-wrap +gray+)

               (draw-rectangle-rec resizer border-color) ; Draw the resize box

               ;; Draw bottom info
               (draw-rectangle 0 (- screen-height 54) screen-width 54 +gray+)
               (draw-rectangle-rec (make-rectangle :x 382.0 :y (- screen-height 34.0) :width 12.0 :height 12.0) +maroon+)

               (draw-text "Word Wrap: " 313 (- screen-height 115) 20 +black+)
               (if word-wrap
                   (draw-text "ON" 447 (- screen-height 115) 20 +red+)
                   (draw-text "OFF" 447 (- screen-height 115) 20 +black+))

               (draw-text "Press [SPACE] to toggle word wrap" 218 (- screen-height 86) 20 +gray+)

               (draw-text "Click hold & drag the    to resize the container" 155 (- screen-height 38) 20 +raywhite+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
