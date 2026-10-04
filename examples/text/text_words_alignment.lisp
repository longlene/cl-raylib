;;;; raylib [text] example - words alignment
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by JP Mortiboys (@themushroompirates) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 JP Mortiboys (@themushroompirates)
;;;; Common Lisp port of raylib/examples/text/text_words_alignment.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-words-alignment
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-words-alignment)

;; Text alignment
(defconstant +text-align-left+ 0)
(defconstant +text-align-top+ 0)
(defconstant +text-align-centre+ 1)
(defconstant +text-align-middle+ 1)
(defconstant +text-align-right+ 2)
(defconstant +text-align-bottom+ 2)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - words alignment")

    (multiple-value-bind (words word-count)
        ;; Define the text we're going to draw in the rectangle
        (text-split "raylib is a simple and easy-to-use library to enjoy videogames programming" #\Space)
      (let* (;; Define the rectangle we will draw the text in
             (text-container-rect (make-rectangle :x (- (/ (float screen-width) 2) (/ (float screen-width) 4))
                                                  :y (- (/ (float screen-height) 2) (/ (float screen-height) 3))
                                                  :width (/ (float screen-width) 2)
                                                  :height (/ (* (float screen-height) 2) 3)))

             ;; Some text to display the current alignment
             (text-align-name-h #("Left" "Centre" "Right"))
             (text-align-name-v #("Top" "Middle" "Bottom"))

             (word-index 0)

             ;; Initialize the font size we're going to use
             (font-size 40)

             ;; And of course the font...
             (font (get-font-default))

             ;; Initialize the alignment variables
             (h-align +text-align-centre+)
             (v-align +text-align-middle+))

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (when (is-key-pressed +key-left+)
                   (when (> h-align 0) (decf h-align)))

                 (when (is-key-pressed +key-right+)
                   (incf h-align)
                   (when (> h-align 2) (setf h-align 2)))

                 (when (is-key-pressed +key-up+)
                   (when (> v-align 0) (decf v-align)))

                 (when (is-key-pressed +key-down+)
                   (incf v-align)
                   (when (> v-align 2) (setf v-align 2)))

                 ;; One word per second
                 (setf word-index (if (> word-count 0) (rem (truncate (get-time)) word-count) 0))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +darkblue+)

                 (draw-text "Use Arrow Keys to change the text alignment" 20 20 20 +lightgray+)
                 (draw-text (text-format "Alignment: Horizontal = %s, Vertical = %s" (aref text-align-name-h h-align) (aref text-align-name-v v-align)) 20 40 20 +lightgray+)

                 (draw-rectangle-rec text-container-rect +blue+)

                 ;; Get the size of the text to draw
                 (let* ((text-size (measure-text-ex font (nth word-index words) (float font-size) (* font-size 0.1)))

                        ;; Calculate the top-left text position based on the rectangle and alignment
                        (text-pos (vec2 (+ (rectangle-x text-container-rect) (lerp 0.0 (- (rectangle-width text-container-rect) (vx text-size)) (* (float h-align) 0.5)))
                                        (+ (rectangle-y text-container-rect) (lerp 0.0 (- (rectangle-height text-container-rect) (vy text-size)) (* (float v-align) 0.5))))))

                   ;; Draw the text
                   (draw-text-ex font (nth word-index words) text-pos (float font-size) (* font-size 0.1) +raywhite+))

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (close-window)))))              ; Close window and OpenGL context

(main)
