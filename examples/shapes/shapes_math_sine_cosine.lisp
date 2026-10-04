;;;; raylib [shapes] example - math sine cosine
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jopestpe (@jopestpe) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jopestpe (@jopestpe)
;;;; Common Lisp port of raylib/examples/shapes/shapes_math_sine_cosine.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-math-sine-cosine
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-math-sine-cosine)

;; Wave points for sine/cosine visualization
(defconstant +wave-points+ 36)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - math sine cosine")

    (let ((sine-points (make-array +wave-points+))
          (cos-points (make-array +wave-points+))
          (center (vec2 (- (/ screen-width 2.0) 30.0) (/ screen-height 2.0)))
          (start (make-rectangle :x 20.0 :y (- screen-height 120.0) :width 200.0 :height 100.0))
          (radius 130.0)
          (angle 0.0)
          (pause nil))

      (dotimes (i +wave-points+)
        (let* ((tt (/ i (float (- +wave-points+ 1))))
               (current-angle (* tt 360.0 +deg2rad+)))
          (setf (aref sine-points i) (vec2 (+ (rectangle-x start) (* tt (rectangle-width start)))
                                           (- (+ (rectangle-y start) (/ (rectangle-height start) 2.0)) (* (sin current-angle) (/ (rectangle-height start) 2.0)))))
          (setf (aref cos-points i) (vec2 (+ (rectangle-x start) (* tt (rectangle-width start)))
                                          (- (+ (rectangle-y start) (/ (rectangle-height start) 2.0)) (* (cos current-angle) (/ (rectangle-height start) 2.0)))))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((angle-rad (* angle +deg2rad+))
                      (cos-rad (cos angle-rad))
                      (sin-rad (sin angle-rad))

                      (point (vec2 (+ (vx center) (* cos-rad radius)) (- (vy center) (* sin-rad radius))))
                      (limit-min (vec2 (- (vx center) radius) (- (vy center) radius)))
                      (limit-max (vec2 (+ (vx center) radius) (+ (vy center) radius)))

                      (complementary (- 90.0 angle))
                      (supplementary (- 180.0 angle))
                      (explementary (- 360.0 angle))

                      (tangent (clamp (tan angle-rad) -10.0 10.0))
                      (cotangent (if (> (abs tangent) 0.001) (clamp (/ 1.0 tangent) (- radius) radius) 0.0))
                      (tangent-point (vec2 (+ (vx center) radius) (- (vy center) (* tangent radius))))
                      (cotangent-point (vec2 (+ (vx center) (* cotangent radius)) (- (vy center) radius)))

                      (sx (rectangle-x start)) (sy (rectangle-y start)) (sw (rectangle-width start)) (sh (rectangle-height start)))

                 (setf angle (wrap (+ angle (if (not pause) 1.0 0.0)) 0.0 360.0))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Cotangent (orange)
                 (draw-line-ex (vec2 (vx center) (vy limit-min)) (vec2 (vx cotangent-point) (vy limit-min)) 2.0 +orange+)
                 (draw-line-dashed center cotangent-point 10 4 +orange+)

                 ;; Side background
                 (draw-line 580 0 580 (get-screen-height) (list 218 218 218 255))
                 (draw-rectangle 580 0 (get-screen-width) (get-screen-height) (list 232 232 232 255))

                 ;; Base circle and axes
                 (draw-circle-lines-v center radius +gray+)
                 (draw-line-ex (vec2 (vx center) (vy limit-min)) (vec2 (vx center) (vy limit-max)) 1.0 +gray+)
                 (draw-line-ex (vec2 (vx limit-min) (vy center)) (vec2 (vx limit-max) (vy center)) 1.0 +gray+)

                 ;; Wave graph axes
                 (draw-line-ex (vec2 sx sy) (vec2 sx (+ sy sh)) 2.0 +gray+)
                 (draw-line-ex (vec2 (+ sx sw) sy) (vec2 (+ sx sw) (+ sy sh)) 2.0 +gray+)
                 (draw-line-ex (vec2 sx (+ sy (/ sh 2))) (vec2 (+ sx sw) (+ sy (/ sh 2))) 2.0 +gray+)

                 ;; Wave graph axis labels
                 (draw-text "1" (- (truncate sx) 8) (truncate sy) 6 +gray+)
                 (draw-text "0" (- (truncate sx) 8) (- (+ (truncate sy) (truncate (truncate sh) 2)) 6) 6 +gray+)
                 (draw-text "-1" (- (truncate sx) 12) (- (+ (truncate sy) (truncate sh)) 8) 6 +gray+)
                 (draw-text "0" (- (truncate sx) 2) (+ (truncate sy) (truncate sh) 4) 6 +gray+)
                 (draw-text "360" (- (+ (truncate sx) (truncate sw)) 8) (+ (truncate sy) (truncate sh) 4) 6 +gray+)

                 ;; Sine (red - vertical)
                 (draw-line-ex (vec2 (vx center) (vy center)) (vec2 (vx center) (vy point)) 2.0 +red+)
                 (draw-line-dashed (vec2 (vx point) (vy center)) (vec2 (vx point) (vy point)) 10 4 +red+)
                 (draw-text (text-format "Sine %.2f" sin-rad) 640 190 6 +red+)
                 (draw-circle-v (vec2 (+ sx (* (/ angle 360.0) sw)) (+ sy (/ (* (+ (- sin-rad) 1) sh) 2.0))) 4.0 +red+)
                 (draw-spline-linear sine-points +wave-points+ 1.0 +red+)

                 ;; Cosine (blue - horizontal)
                 (draw-line-ex (vec2 (vx center) (vy center)) (vec2 (vx point) (vy center)) 2.0 +blue+)
                 (draw-line-dashed (vec2 (vx center) (vy point)) (vec2 (vx point) (vy point)) 10 4 +blue+)
                 (draw-text (text-format "Cosine %.2f" cos-rad) 640 210 6 +blue+)
                 (draw-circle-v (vec2 (+ sx (* (/ angle 360.0) sw)) (+ sy (/ (* (+ (- cos-rad) 1) sh) 2.0))) 4.0 +blue+)
                 (draw-spline-linear cos-points +wave-points+ 1.0 +blue+)

                 ;; Tangent (purple)
                 (draw-line-ex (vec2 (vx limit-max) (vy center)) (vec2 (vx limit-max) (vy tangent-point)) 2.0 +purple+)
                 (draw-line-dashed center tangent-point 10 4 +purple+)
                 (draw-text (text-format "Tangent %.2f" tangent) 640 230 6 +purple+)

                 ;; Cotangent (orange)
                 (draw-text (text-format "Cotangent %.2f" cotangent) 640 250 6 +orange+)

                 ;; Complementary angle (beige)
                 (draw-circle-sector-lines center (* radius 0.6) (- angle) -90.0 36 +beige+)
                 (draw-text (text-format "Complementary  %0.f°" complementary) 640 150 6 +beige+)

                 ;; Supplementary angle (darkblue)
                 (draw-circle-sector-lines center (* radius 0.5) (- angle) -180.0 36 +darkblue+)
                 (draw-text (text-format "Supplementary  %0.f°" supplementary) 640 130 6 +darkblue+)

                 ;; Explementary angle (pink)
                 (draw-circle-sector-lines center (* radius 0.4) (- angle) -360.0 36 +pink+)
                 (draw-text (text-format "Explementary  %0.f°" explementary) 640 170 6 +pink+)

                 ;; Current angle - arc (lime), radius (black), endpoint (black)
                 (draw-circle-sector-lines center (* radius 0.7) (- angle) 0.0 36 +lime+)
                 (draw-line-ex (vec2 (vx center) (vy center)) point 2.0 +black+)
                 (draw-circle-v point 4.0 +black+)

                 ;; Draw GUI controls
                 ;;------------------------------------------------------------------------------
                 (gui-set-style +label+ +text-color-normal+ (color-to-int +gray+))
                 (setf pause (nth-value 1 (gui-toggle (make-rectangle :x 640.0 :y 70.0 :width 120.0 :height 20.0) (text-format "Pause") pause)))
                 (gui-set-style +label+ +text-color-normal+ (color-to-int +lime+))
                 (setf angle (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 40.0 :width 120.0 :height 20.0) "Angle" (text-format "%.0f°" angle) angle 0.0 360.0)))

                 ;; Angle values panel
                 (gui-group-box (make-rectangle :x 620.0 :y 110.0 :width 140.0 :height 170.0) "Angle Values")
                 ;;------------------------------------------------------------------------------

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
