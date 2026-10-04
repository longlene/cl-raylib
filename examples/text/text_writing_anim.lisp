;;;; raylib [text] example - writing anim
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.4, last time updated with raylib 1.4
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2016-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_writing_anim.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-writing-anim
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-writing-anim)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - writing anim")

    (let ((message (format nil "This sample illustrates a text writing~%animation effect! Check it out! ;)"))
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (if (is-key-down +key-space+)
                   (incf frames-counter 8)
                   (incf frames-counter))

               (when (is-key-pressed +key-enter+) (setf frames-counter 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text (text-subtext message 0 (floor frames-counter 10)) 210 160 20 +maroon+)

               (draw-text "PRESS [ENTER] to RESTART!" 240 260 20 +lightgray+)
               (draw-text "HOLD [SPACE] to SPEED UP!" 239 300 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
