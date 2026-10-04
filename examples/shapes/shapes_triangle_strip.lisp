;;;; raylib [shapes] example - triangle strip
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jopestpe (@jopestpe)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jopestpe (@jopestpe)
;;;; Common Lisp port of raylib/examples/shapes/shapes_triangle_strip.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-triangle-strip
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-triangle-strip)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - triangle strip")

    (let ((points (let ((a (make-array 122))) (dotimes (i 122 a) (setf (aref a i) (vec2 0.0 0.0)))))
          (center (vec2 (- (/ screen-width 2.0) 125.0) (/ screen-height 2.0)))
          (segments 6.0)
          (inside-radius 100.0)
          (outside-radius 150.0)
          (outline t))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((point-count (truncate segments))
                      (angle-step (* (/ 360.0 point-count) +deg2rad+)))

                 (loop for i from 0 below point-count
                       for i2 from 0 by 2
                       do (let* ((angle1 (* i angle-step))
                                 (angle2 (+ angle1 (/ angle-step 2.0))))
                            (setf (aref points i2) (vec2 (+ (vx center) (* (cos angle1) inside-radius)) (+ (vy center) (* (sin angle1) inside-radius))))
                            (setf (aref points (+ i2 1)) (vec2 (+ (vx center) (* (cos angle2) outside-radius)) (+ (vy center) (* (sin angle2) outside-radius))))))

                 (setf (aref points (* point-count 2)) (aref points 0))
                 (setf (aref points (+ (* point-count 2) 1)) (aref points 1))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (dotimes (i point-count)
                   (let ((a (aref points (* i 2)))
                         (b (aref points (+ (* i 2) 1)))
                         (c (aref points (+ (* i 2) 2)))
                         (d (aref points (+ (* i 2) 3)))
                         (angle1 (* i angle-step)))
                     (draw-triangle c b a (color-from-hsv (* angle1 +rad2deg+) 1.0 1.0))
                     (draw-triangle d b c (color-from-hsv (* (+ angle1 (/ angle-step 2)) +rad2deg+) 1.0 1.0))
                     (when outline
                       (draw-triangle-lines a b c +black+)
                       (draw-triangle-lines c b d +black+))))

                 (draw-line 580 0 580 (get-screen-height) (list 218 218 218 255))
                 (draw-rectangle 580 0 (get-screen-width) (get-screen-height) (list 232 232 232 255))

                 ;; Draw GUI controls
                 ;;------------------------------------------------------------------------------
                 (setf segments (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 40.0 :width 120.0 :height 20.0) "Segments" (text-format "%.0f" segments) segments 6.0 60.0)))
                 (setf outline (nth-value 1 (gui-check-box (make-rectangle :x 640.0 :y 70.0 :width 20.0 :height 20.0) "Outline" outline)))
                 ;;------------------------------------------------------------------------------

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
