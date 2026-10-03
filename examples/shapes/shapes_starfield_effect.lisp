;;;; raylib [shapes] example - starfield effect
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by JP Mortiboys (@themushroompirates) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 JP Mortiboys (@themushroompirates)
;;;; Common Lisp port of raylib/examples/shapes/shapes_starfield_effect.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-starfield-effect
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-starfield-effect)

(defconstant +star-count+ 420)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - starfield effect")

    (let ((bg-color (color-lerp +darkblue+ +black+ 0.69))
          ;; Speed at which we fly forward
          (speed (/ 10.0 9.0))
          ;; We're either drawing lines or circles
          (draw-lines t)
          (stars (make-array +star-count+))
          (stars-screen-pos (make-array +star-count+)))

      ;; Setup the stars with a random position
      (dotimes (i +star-count+)
        (let* ((x (float (get-random-value (truncate (- screen-width) 2) (truncate screen-width 2))))
               (y (float (get-random-value (truncate (- screen-height) 2) (truncate screen-height 2)))))
          (setf (aref stars i) (vec3 x y 1.0)
                (aref stars-screen-pos i) (vec2 0.0 0.0))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Change speed based on mouse
               (let ((mouse-move (get-mouse-wheel-move)))
                 (when (/= (truncate mouse-move) 0) (incf speed (/ (* 2.0 mouse-move) 9.0)))
                 (cond ((< speed 0.0) (setf speed 0.1))
                       ((> speed 2.0) (setf speed 2.0))))

               ;; Toggle lines / points with space bar
               (when (is-key-pressed +key-space+) (setf draw-lines (not draw-lines)))

               (let ((dt (get-frame-time)))
                 (dotimes (i +star-count+)
                   (let ((star (aref stars i)))
                     ;; Update star's timer
                     (decf (vz star) (* dt speed))

                     ;; Calculate the screen position
                     (setf (aref stars-screen-pos i) (vec2 (+ (* screen-width 0.5) (/ (vx star) (vz star)))
                                                           (+ (* screen-height 0.5) (/ (vy star) (vz star)))))

                     ;; If the star is too old, or offscreen, it dies and we make a new random one
                     (let ((pos (aref stars-screen-pos i)))
                       (when (or (< (vz star) 0.0) (< (vx pos) 0) (< (vy pos) 0.0)
                                 (> (vx pos) screen-width) (> (vy pos) screen-height))
                         (setf (vx star) (float (get-random-value (truncate (- screen-width) 2) (truncate screen-width 2)))
                               (vy star) (float (get-random-value (truncate (- screen-height) 2) (truncate screen-height 2)))
                               (vz star) 1.0))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background bg-color)

               (dotimes (i +star-count+)
                 (let ((star (aref stars i)))
                   (if draw-lines
                       ;; Get the time a little while ago for this star, but clamp it
                       (let ((tt (clamp (+ (vz star) (/ 1.0 32.0)) 0.0 1.0)))
                         ;; If it's different enough from the current time, we proceed
                         (when (> (- tt (vz star)) 1d-3)
                           ;; Calculate the screen position of the old point
                           (let ((start-pos (vec2 (+ (* screen-width 0.5) (/ (vx star) tt))
                                                  (+ (* screen-height 0.5) (/ (vy star) tt)))))
                             ;; Draw a line connecting the old point to the current point
                             (draw-line-v start-pos (aref stars-screen-pos i) +raywhite+))))
                       ;; Make the radius grow as the star ages
                       (let ((radius (lerp (vz star) 1.0 5.0)))
                         ;; Draw the circle
                         (draw-circle-v (aref stars-screen-pos i) radius +raywhite+)))))

               (draw-text (text-format "[MOUSE WHEEL] Current Speed: %.0f" (/ (* 9.0 speed) 2.0)) 10 40 20 +raywhite+)
               (draw-text (text-format "[SPACE] Current draw mode: %s" (if draw-lines "Lines" "Circles")) 10 70 20 +raywhite+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
