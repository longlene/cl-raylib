;;;; raylib [core] example - scissor test
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.0
;;;;
;;;; Example contributed by Chris Dill (@MysteriousSpace) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2019-2025 Chris Dill (@MysteriousSpace)
;;;; Common Lisp port of raylib/examples/core/core_scissor_test.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-scissor-test
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-scissor-test)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - scissor test")

    (let ((scissor-area (make-rectangle :x 0.0 :y 0.0 :width 300.0 :height 300.0))
          (scissor-mode t))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-s+) (setf scissor-mode (not scissor-mode)))

               ;; Centre the scissor area around the mouse position
               (setf (rectangle-x scissor-area) (- (get-mouse-x) (/ (rectangle-width scissor-area) 2))
                     (rectangle-y scissor-area) (- (get-mouse-y) (/ (rectangle-height scissor-area) 2)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (when scissor-mode
                 (begin-scissor-mode (truncate (rectangle-x scissor-area)) (truncate (rectangle-y scissor-area))
                                     (truncate (rectangle-width scissor-area)) (truncate (rectangle-height scissor-area))))

               ;; Draw full screen rectangle and some text
               ;; NOTE: Only part defined by scissor area will be rendered
               (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +red+)
               (draw-text "Move the mouse around to reveal this text!" 190 200 20 +lightgray+)

               (when scissor-mode (end-scissor-mode))

               (draw-rectangle-lines-ex scissor-area 1.0 +black+)
               (draw-text "Press S to toggle scissor test" 10 10 20 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
