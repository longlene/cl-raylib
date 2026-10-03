;;;; raylib [shapes] example - easings box
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_easings_box.c

(require :cl-raylib)
(load (merge-pathnames "reasings.lisp" *load-truename*)) ; Required for easing functions

(defpackage #:raylib-examples/shapes-easings-box
  (:use #:cl #:raylib #:reasings))
(in-package #:raylib-examples/shapes-easings-box)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - easings box")

    ;; Box variables to be animated with easings
    (let ((rec (make-rectangle :x (/ (get-screen-width) 2.0) :y -100.0 :width 100.0 :height 100.0))
          (rotation 0.0)
          (alpha 1.0)
          (state 0)
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (case state
                 (0                     ; Move box down to center of screen
                  (incf frames-counter)

                  ;; NOTE: Remember that 3rd parameter of easing function refers to
                  ;; desired value variation, do not confuse it with expected final value!
                  (setf (rectangle-y rec) (ease-elastic-out (float frames-counter) -100 (+ (/ (get-screen-height) 2.0) 100) 120))

                  (when (>= frames-counter 120)
                    (setf frames-counter 0
                          state 1)))
                 (1                     ; Scale box to an horizontal bar
                  (incf frames-counter)
                  (setf (rectangle-height rec) (ease-bounce-out (float frames-counter) 100 -90 120))
                  (setf (rectangle-width rec) (ease-bounce-out (float frames-counter) 100 (float (get-screen-width)) 120))

                  (when (>= frames-counter 120)
                    (setf frames-counter 0
                          state 2)))
                 (2                     ; Rotate horizontal bar rectangle
                  (incf frames-counter)
                  (setf rotation (ease-quad-out (float frames-counter) 0.0 270.0 240))

                  (when (>= frames-counter 240)
                    (setf frames-counter 0
                          state 3)))
                 (3                     ; Increase bar size to fill all screen
                  (incf frames-counter)
                  (setf (rectangle-height rec) (ease-circ-out (float frames-counter) 10 (float (get-screen-width)) 120))

                  (when (>= frames-counter 120)
                    (setf frames-counter 0
                          state 4)))
                 (4                     ; Fade out animation
                  (incf frames-counter)
                  (setf alpha (ease-sine-out (float frames-counter) 1.0 -1.0 160))

                  (when (>= frames-counter 160)
                    (setf frames-counter 0
                          state 5)))
                 (t nil))

               ;; Reset animation at any moment
               (when (is-key-pressed +key-space+)
                 (setf rec (make-rectangle :x (/ (get-screen-width) 2.0) :y -100.0 :width 100.0 :height 100.0)
                       rotation 0.0
                       alpha 1.0
                       state 0
                       frames-counter 0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-rectangle-pro rec (vec2 (/ (rectangle-width rec) 2) (/ (rectangle-height rec) 2)) rotation (fade +black+ alpha))

               (draw-text "PRESS [SPACE] TO RESET BOX ANIMATION!" 10 (- (get-screen-height) 25) 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
