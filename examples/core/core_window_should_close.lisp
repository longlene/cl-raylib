;;;; raylib [core] example - window should close
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2013-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_window_should_close.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-window-should-close
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-window-should-close)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - window should close")

    (set-exit-key +key-null+)           ; Disable KEY_ESCAPE to close window, X-button still works

    (let ((exit-window-requested nil)   ; Flag to request window to exit
          (exit-window nil))            ; Flag to set window to exit

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until exit-window
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Detect if X-button or KEY_ESCAPE have been pressed to close window
               (when (or (window-should-close) (is-key-pressed +key-escape+)) (setf exit-window-requested t))

               (when exit-window-requested
                 ;; A request for close window has been issued, we can save data before closing
                 ;; or just show a message asking for confirmation

                 (cond ((is-key-pressed +key-y+) (setf exit-window t))
                       ((is-key-pressed +key-n+) (setf exit-window-requested nil))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (if exit-window-requested
                   (progn
                     (draw-rectangle 0 100 screen-width 200 +black+)
                     (draw-text "Are you sure you want to exit program? [Y/N]" 40 180 30 +white+))
                   (draw-text "Try to close the window to get confirmation message!" 120 200 20 +lightgray+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
