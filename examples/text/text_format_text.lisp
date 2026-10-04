;;;; raylib [text] example - format text
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.1, last time updated with raylib 3.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_format_text.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-format-text
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-format-text)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - format text")

    (let ((score 100020)
          (hiscore 200450)
          (lives 5))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update your variables here
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text (text-format "Score: %08i" score) 200 80 20 +red+)

               (draw-text (text-format "HiScore: %08i" hiscore) 200 120 20 +green+)

               (draw-text (text-format "Lives: %02i" lives) 200 160 40 +blue+)

               (draw-text (text-format "Elapsed Time: %02.02f ms" (* (get-frame-time) 1000)) 200 220 20 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
