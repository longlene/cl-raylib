;;;; text_writing_anim.lisp - Text Writing Animation
;;;; Translated from raylib/examples/text/text_writing_anim.c

(require :cl-raylib)

(defpackage :text-writing-anim
  (:use :cl :cl-raylib))

(in-package :text-writing-anim)

(defun main ()
  "Main function - text writing animation"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - text writing anim")

    (let ((message "This sample illustrates a text writing~nanimation effect\! Check it out\! ;)")
          (frames-counter 0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (if (is-key-down +key-space+) 
            (incf frames-counter 8)
            (incf frames-counter))

        (when (is-key-pressed +key-enter+) 
          (setf frames-counter 0))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Draw animated text (show characters progressively)
          (let ((chars-to-show (floor (/ frames-counter 10))))
            (draw-text (text-subtext message 0 chars-to-show) 210 160 20 +maroon+))

          (draw-text "PRESS [ENTER] to RESTART\!" 240 260 20 +lightgray+)
          (draw-text "HOLD [SPACE] to SPEED UP\!" 239 300 20 +lightgray+)

        (end-drawing)))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
