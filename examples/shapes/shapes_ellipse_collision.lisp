;;;; raylib [shapes] example - ellipse collision
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Ziya (@Monjaris)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Ziya (@Monjaris)
;;;; Common Lisp port of raylib/examples/shapes/shapes_ellipse_collision.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-ellipse-collision
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-ellipse-collision)

;; Check if point is inside ellipse
(defun check-collision-point-ellipse (point center rx ry)
  (let ((dx (/ (- (vx point) (vx center)) rx))
        (dy (/ (- (vy point) (vy center)) ry)))
    (<= (+ (* dx dx) (* dy dy)) 1.0)))

;; Check if two ellipses collide
;; Uses radial boundary distance in the direction between centers — scales correctly with radii
(defun check-collision-ellipses (c1 rx1 ry1 c2 rx2 ry2)
  (let* ((dx (- (vx c2) (vx c1)))
         (dy (- (vy c2) (vy c1)))
         (dist (sqrt (+ (* dx dx) (* dy dy)))))

    ;; Ellipses are on top of each other
    (when (= dist 0.0) (return-from check-collision-ellipses t))

    (let* ((theta (atan dy dx))
           (cos-t (cos theta))
           (sin-t (sin theta))

           ;; Radial distance from center to ellipse boundary in direction theta
           ;; r(theta) = (rx * ry) / sqrt((ry*cos)^2 + (rx*sin)^2)
           (r1 (/ (* rx1 ry1) (sqrt (+ (* (* ry1 cos-t) (* ry1 cos-t)) (* (* rx1 sin-t) (* rx1 sin-t))))))
           (r2 (/ (* rx2 ry2) (sqrt (+ (* (* ry2 cos-t) (* ry2 cos-t)) (* (* rx2 sin-t) (* rx2 sin-t)))))))

      (<= dist (+ r1 r2)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - ellipse collision")

    (set-target-fps 60)

    (let ((ellipse-a-center (vec2 (/ (float screen-width) 4) (/ (float screen-height) 2)))
          (ellipse-a-rx 120.0)
          (ellipse-a-ry 70.0)

          (ellipse-b-center (vec2 (/ (* (float screen-width) 3) 4) (/ (float screen-height) 2)))
          (ellipse-b-rx 90.0)
          (ellipse-b-ry 140.0)

          ;; 0 = controlling A, 1 = controlling B
          (controlled 0))
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-a+) (setf controlled 0))
               (when (is-key-pressed +key-b+) (setf controlled 1))

               (if (= controlled 0)
                   (setf ellipse-a-center (get-mouse-position))
                   (setf ellipse-b-center (get-mouse-position)))

               (let ((ellipses-collide (check-collision-ellipses ellipse-a-center ellipse-a-rx ellipse-a-ry
                                                                 ellipse-b-center ellipse-b-rx ellipse-b-ry))
                     (mouse-in-a (check-collision-point-ellipse (get-mouse-position) ellipse-a-center ellipse-a-rx ellipse-a-ry))
                     (mouse-in-b (check-collision-point-ellipse (get-mouse-position) ellipse-b-center ellipse-b-rx ellipse-b-ry)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-ellipse (truncate (vx ellipse-a-center)) (truncate (vy ellipse-a-center)) ellipse-a-rx ellipse-a-ry (if ellipses-collide +red+ +blue+))
                 (draw-ellipse (truncate (vx ellipse-b-center)) (truncate (vy ellipse-b-center)) ellipse-b-rx ellipse-b-ry (if ellipses-collide +red+ +green+))

                 (draw-ellipse-lines (truncate (vx ellipse-a-center)) (truncate (vy ellipse-a-center)) ellipse-a-rx ellipse-a-ry +white+)
                 (draw-ellipse-lines (truncate (vx ellipse-b-center)) (truncate (vy ellipse-b-center)) ellipse-b-rx ellipse-b-ry +white+)

                 (draw-circle-v ellipse-a-center 4.0 +white+)
                 (draw-circle-v ellipse-b-center 4.0 +white+)

                 (if ellipses-collide
                     (draw-text "ELLIPSES COLLIDE" (- (truncate screen-width 2) 120) 40 28 +red+)
                     (draw-text "NO COLLISION" (- (truncate screen-width 2) 80) 40 28 +darkgray+))

                 (draw-text (if (= controlled 0) "Controlling: A" "Controlling: B") 20 (- screen-height 40) 20 +yellow+)

                 (when (and mouse-in-a (/= controlled 0)) (draw-text "Mouse inside ellipse A" 20 (- screen-height 70) 20 +blue+))
                 (when (and mouse-in-b (/= controlled 1)) (draw-text "Mouse inside ellipse B" 20 (- screen-height 70) 20 +green+))

                 (draw-text "Press [A] or [B] to switch control" 20 20 20 +gray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
