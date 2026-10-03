;;;; raylib [shapes] example - following eyes
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2013-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_following_eyes.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-following-eyes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-following-eyes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - following eyes")

    (let ((sclera-left-position (vec2 (- (/ (get-screen-width) 2.0) 100.0) (/ (get-screen-height) 2.0)))
          (sclera-right-position (vec2 (+ (/ (get-screen-width) 2.0) 100.0) (/ (get-screen-height) 2.0)))
          (sclera-radius 80.0)
          (iris-left-position (vec2 (- (/ (get-screen-width) 2.0) 100.0) (/ (get-screen-height) 2.0)))
          (iris-right-position (vec2 (+ (/ (get-screen-width) 2.0) 100.0) (/ (get-screen-height) 2.0)))
          (iris-radius 24.0)
          (angle 0.0)
          (dx 0.0) (dy 0.0) (dxx 0.0) (dyy 0.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf iris-left-position (get-mouse-position))
               (setf iris-right-position (get-mouse-position))

               ;; Check not inside the left eye sclera
               (unless (check-collision-point-circle iris-left-position sclera-left-position (- sclera-radius iris-radius))
                 (setf dx (- (vx iris-left-position) (vx sclera-left-position))
                       dy (- (vy iris-left-position) (vy sclera-left-position))

                       angle (atan dy dx)

                       dxx (* (- sclera-radius iris-radius) (cos angle))
                       dyy (* (- sclera-radius iris-radius) (sin angle)))

                 (setf (vx iris-left-position) (+ (vx sclera-left-position) dxx)
                       (vy iris-left-position) (+ (vy sclera-left-position) dyy)))

               ;; Check not inside the right eye sclera
               (unless (check-collision-point-circle iris-right-position sclera-right-position (- sclera-radius iris-radius))
                 (setf dx (- (vx iris-right-position) (vx sclera-right-position))
                       dy (- (vy iris-right-position) (vy sclera-right-position))

                       angle (atan dy dx)

                       dxx (* (- sclera-radius iris-radius) (cos angle))
                       dyy (* (- sclera-radius iris-radius) (sin angle)))

                 (setf (vx iris-right-position) (+ (vx sclera-right-position) dxx)
                       (vy iris-right-position) (+ (vy sclera-right-position) dyy)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-circle-v sclera-left-position sclera-radius +lightgray+)
               (draw-circle-v iris-left-position iris-radius +brown+)
               (draw-circle-v iris-left-position 10.0 +black+)

               (draw-circle-v sclera-right-position sclera-radius +lightgray+)
               (draw-circle-v iris-right-position iris-radius +darkgreen+)
               (draw-circle-v iris-right-position 10.0 +black+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
