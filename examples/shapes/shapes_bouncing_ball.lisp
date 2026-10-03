;;;; raylib [shapes] example - bouncing ball
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Ramon Santamaria (@raysan5), reviewed by Jopestpe (@jopestpe)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2013-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_bouncing_ball.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-bouncing-ball
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-bouncing-ball)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;---------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - bouncing ball")

    (let ((ball-position (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))
          (ball-speed (vec2 5.0 4.0))
          (ball-radius 20)
          (gravity 0.2)
          (use-gravity t)
          (pause nil)
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;----------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;-----------------------------------------------------
               (when (is-key-pressed +key-g+) (setf use-gravity (not use-gravity)))
               (when (is-key-pressed +key-space+) (setf pause (not pause)))

               (if (not pause)
                   (progn
                     (incf (vx ball-position) (vx ball-speed))
                     (incf (vy ball-position) (vy ball-speed))

                     (when use-gravity (incf (vy ball-speed) gravity))

                     ;; Check walls collision for bouncing
                     (when (or (>= (vx ball-position) (- (get-screen-width) ball-radius)) (<= (vx ball-position) ball-radius))
                       (setf (vx ball-speed) (* (vx ball-speed) -1.0)))
                     (when (or (>= (vy ball-position) (- (get-screen-height) ball-radius)) (<= (vy ball-position) ball-radius))
                       (setf (vy ball-speed) (* (vy ball-speed) -0.95))))
                   (incf frames-counter))
               ;;-----------------------------------------------------

               ;; Draw
               ;;-----------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-circle-v ball-position (float ball-radius) +maroon+)
               (draw-text "PRESS SPACE to PAUSE BALL MOVEMENT" 10 (- (get-screen-height) 25) 20 +lightgray+)

               (if use-gravity
                   (draw-text "GRAVITY: ON (Press G to disable)" 10 (- (get-screen-height) 50) 20 +darkgreen+)
                   (draw-text "GRAVITY: OFF (Press G to enable)" 10 (- (get-screen-height) 50) 20 +red+))

               ;; On pause, we draw a blinking message
               (when (and pause (oddp (truncate frames-counter 30))) (draw-text "PAUSED" 350 200 30 +gray+))

               (draw-fps 10 10)

               (end-drawing))
      ;;-----------------------------------------------------

      ;; De-Initialization
      ;;---------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
