;;;; raylib [text] example - input box
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.7, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_input_box.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-input-box
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-input-box)

(defconstant +max-input-chars+ 9)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - input box")

    (let ((name (make-array +max-input-chars+ :element-type 'character :fill-pointer 0))
          (letter-count 0)

          (text-box (make-rectangle :x (- (/ screen-width 2.0) 100) :y 180.0 :width 225.0 :height 50.0))
          (mouse-on-text nil)

          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf mouse-on-text (check-collision-point-rec (get-mouse-position) text-box))

               (if mouse-on-text
                   (progn
                     ;; Set the window's cursor to the I-Beam
                     (set-mouse-cursor +mouse-cursor-ibeam+)

                     ;; Get char pressed (unicode character) on the queue
                     (loop for key = (get-char-pressed) ; Check next character in the queue
                           ;; Check if more characters have been pressed on the same frame
                           while (> key 0)
                           ;; NOTE: Only allow keys in range [32..125]
                           do (when (and (>= key 32) (<= key 125) (< letter-count +max-input-chars+))
                                (vector-push (code-char key) name)
                                (incf letter-count)))

                     (when (is-key-pressed +key-backspace+)
                       (decf letter-count)
                       (when (< letter-count 0) (setf letter-count 0))
                       (setf (fill-pointer name) letter-count)))
                   (set-mouse-cursor +mouse-cursor-default+))

               (if mouse-on-text
                   (incf frames-counter)
                   (setf frames-counter 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "PLACE MOUSE OVER INPUT BOX!" 240 140 20 +gray+)

               (draw-rectangle-rec text-box +lightgray+)
               (if mouse-on-text
                   (draw-rectangle-lines (truncate (rectangle-x text-box)) (truncate (rectangle-y text-box)) (truncate (rectangle-width text-box)) (truncate (rectangle-height text-box)) +red+)
                   (draw-rectangle-lines (truncate (rectangle-x text-box)) (truncate (rectangle-y text-box)) (truncate (rectangle-width text-box)) (truncate (rectangle-height text-box)) +darkgray+))

               (draw-text name (+ (truncate (rectangle-x text-box)) 5) (+ (truncate (rectangle-y text-box)) 8) 40 +maroon+)

               (draw-text (text-format "INPUT CHARS: %i/%i" letter-count +max-input-chars+) 315 250 20 +darkgray+)

               (when mouse-on-text
                 (if (< letter-count +max-input-chars+)
                     ;; Draw blinking underscore char
                     (when (= (mod (floor frames-counter 20) 2) 0)
                       (draw-text "_" (+ (truncate (rectangle-x text-box)) 8 (measure-text name 40)) (+ (truncate (rectangle-y text-box)) 12) 40 +maroon+))
                     (draw-text "Press BACKSPACE to delete chars..." 230 300 20 +gray+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

;; Check if any key is pressed
;; NOTE: We limit keys check to keys between 32 (KEY_SPACE) and 126
(defun is-any-key-pressed ()
  (let ((key (get-key-pressed)))
    (and (>= key 32) (<= key 126))))

(main)
