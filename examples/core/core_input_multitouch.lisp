;;;; raylib [core] example - input multitouch
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 2.1, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Berni (@Berni8k) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2019-2025 Berni (@Berni8k) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_multitouch.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-multitouch
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-multitouch)

(defconstant +max-touch-points+ 10)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input multitouch")

    (let ((touch-positions (make-array +max-touch-points+ :initial-element (vec2 0.0 0.0))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;---------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Get the touch point count ( how many fingers are touching the screen )
               (let ((t-count (get-touch-point-count)))
                 ;; Clamp touch points available ( set the maximum touch points allowed )
                 (when (> t-count +max-touch-points+) (setf t-count +max-touch-points+))
                 ;; Get touch points positions
                 (dotimes (i t-count) (setf (aref touch-positions i) (get-touch-position i)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (dotimes (i t-count)
                   ;; Make sure point is not (0, 0) as this means there is no touch for it
                   (let ((p (aref touch-positions i)))
                     (when (and (> (vx p) 0) (> (vy p) 0))
                       ;; Draw circle and touch index number
                       (draw-circle-v p 34.0 +orange+)
                       (draw-text (text-format "%d" i) (- (truncate (vx p)) 10) (- (truncate (vy p)) 70) 40 +black+))))

                 (draw-text "touch the screen at multiple locations to get multiple balls" 10 10 20 +darkgray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
