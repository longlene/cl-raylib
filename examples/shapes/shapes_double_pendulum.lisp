;;;; raylib [shapes] example - double pendulum
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by JoeCheong (@Joecheong2006) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 JoeCheong (@Joecheong2006)
;;;; Common Lisp port of raylib/examples/shapes/shapes_double_pendulum.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-double-pendulum
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-double-pendulum)

;; Constant for Simulation
(defconstant +simulation-steps+ 30)
(defconstant +g+ 9.81)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Calculate pendulum end point
(defun calculate-pendulum-end-point (l theta)
  (vec2 (* 10 l (sin theta)) (* 10 l (cos theta))))

;; Calculate double pendulum end point
(defun calculate-double-pendulum-end-point (l1 theta1 l2 theta2)
  (let ((endpoint1 (calculate-pendulum-end-point l1 theta1))
        (endpoint2 (calculate-pendulum-end-point l2 theta2)))
    (vec2 (+ (vx endpoint1) (vx endpoint2)) (+ (vy endpoint1) (vy endpoint2)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-window-highdpi+)
    (init-window screen-width screen-height "raylib [shapes] example - double pendulum")

    (let* (;; Simulation Parameters
           (l1 15.0) (m1 0.2) (theta1 (* +deg2rad+ 170)) (w1 0.0)
           (l2 15.0) (m2 0.1) (theta2 (* +deg2rad+ 0)) (w2 0.0)
           (length-scaler 0.1)
           (total-m (+ m1 m2))

           (previous-position (calculate-double-pendulum-end-point l1 theta1 l2 theta2))

           ;; Scale length
           (big-l1 (* l1 length-scaler))
           (big-l2 (* l2 length-scaler))

           ;; Draw parameters
           (line-thick 20.0) (trail-thick 2.0)
           (fate-alpha 0.01)

           ;; Create framebuffer
           (target (load-render-texture screen-width screen-height)))

      (incf (vx previous-position) (/ (float screen-width) 2))
      (incf (vy previous-position) (- (/ (float screen-height) 2) 100))

      (set-texture-filter (render-texture-texture target) +texture-filter-bilinear+)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((dt (get-frame-time))
                      (step (/ dt +simulation-steps+)) (step2 (* step step)))

                 ;; Update Physics - larger steps = better approximation
                 (dotimes (i +simulation-steps+)
                   (let* ((delta (- theta1 theta2))
                          (sin-d (sin delta)) (cos-d (cos delta)) (cos2-d (cos (* 2 delta)))
                          (ww1 (* w1 w1)) (ww2 (* w2 w2))

                          ;; Calculate a1
                          (a1 (/ (- (* (- +g+) (+ (* 2 m1) m2) (sin theta1))
                                    (* m2 +g+ (sin (- theta1 (* 2 theta2))))
                                    (* 2 sin-d m2 (+ (* ww2 big-l2) (* ww1 big-l1 cos-d))))
                                 (* big-l1 (- (+ (* 2 m1) m2) (* m2 cos2-d)))))

                          ;; Calculate a2
                          (a2 (/ (* 2 sin-d (+ (* ww1 big-l1 total-m)
                                               (* +g+ total-m (cos theta1))
                                               (* ww2 big-l2 m2 cos-d)))
                                 (* big-l2 (- (+ (* 2 m1) m2) (* m2 cos2-d))))))

                     ;; Update thetas
                     (incf theta1 (+ (* w1 step) (* 0.5 a1 step2)))
                     (incf theta2 (+ (* w2 step) (* 0.5 a2 step2)))

                     ;; Update omegas
                     (incf w1 (* a1 step))
                     (incf w2 (* a2 step))))

                 ;; Calculate position
                 (let ((current-position (calculate-double-pendulum-end-point l1 theta1 l2 theta2)))
                   (incf (vx current-position) (/ (float screen-width) 2))
                   (incf (vy current-position) (- (/ (float screen-height) 2) 100))

                   ;; Draw to render texture
                   (begin-texture-mode target)
                   ;; Draw a transparent rectangle - smaller alpha = longer trails
                   (draw-rectangle 0 0 screen-width screen-height (fade +black+ fate-alpha))

                   ;; Draw trail
                   (draw-circle-v previous-position trail-thick +red+)
                   (draw-line-ex previous-position current-position (* trail-thick 2) +red+)
                   (end-texture-mode)

                   ;; Update previous position
                   (setf previous-position current-position)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +black+)

               ;; Draw trails texture
               (let ((texture (render-texture-texture target)))
                 (draw-texture-rec texture (make-rectangle :x 0.0 :y 0.0
                                                           :width (float (texture-width texture))
                                                           :height (float (- (texture-height texture))))
                                   (vec2 0.0 0.0) +white+))

               ;; Draw double pendulum
               (draw-rectangle-pro (make-rectangle :x (/ screen-width 2.0) :y (- (/ screen-height 2.0) 100) :width (* 10 l1) :height line-thick)
                                   (vec2 0.0 (* line-thick 0.5)) (- 90 (* +rad2deg+ theta1)) +raywhite+)

               (let ((endpoint1 (calculate-pendulum-end-point l1 theta1)))
                 (draw-rectangle-pro (make-rectangle :x (+ (/ screen-width 2.0) (vx endpoint1)) :y (+ (- (/ screen-height 2.0) 100) (vy endpoint1))
                                                     :width (* 10 l2) :height line-thick)
                                     (vec2 0.0 (* line-thick 0.5)) (- 90 (* +rad2deg+ theta2)) +raywhite+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture target)

      (close-window))))                 ; Close window and OpenGL context

(main)
