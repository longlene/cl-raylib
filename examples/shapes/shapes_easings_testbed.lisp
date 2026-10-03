;;;; raylib [shapes] example - easings testbed
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Juan Miguel López (@flashback-fx) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Juan Miguel López (@flashback-fx) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_easings_testbed.c

(require :cl-raylib)
(load (merge-pathnames "reasings.lisp" *load-truename*)) ; Required for: easing functions

(defpackage #:raylib-examples/shapes-easings-testbed
  (:use #:cl #:raylib #:reasings))
(in-package #:raylib-examples/shapes-easings-testbed)

(defconstant +font-size+ 20)

(defconstant +d-step+ 20.0)
(defconstant +d-step-fine+ 2.0)
(defconstant +d-min+ 1.0)
(defconstant +d-max+ 10000.0)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Easing types: index into *easings*, the last one (EASING_NONE) selects NoEase
(defconstant +num-easing-types+ 28)
(defconstant +easing-none+ +num-easing-types+)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; NoEase function, used when "no easing" is selected for any axis
;; It just ignores all parameters besides b
(defun no-ease (tt b c d)
  (declare (ignore tt c d))
  b)

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
;; Easing functions reference data: (name . func)
(defparameter *easings*
  (vector (cons "EaseLinearNone" #'ease-linear-none)
          (cons "EaseLinearIn" #'ease-linear-in)
          (cons "EaseLinearOut" #'ease-linear-out)
          (cons "EaseLinearInOut" #'ease-linear-in-out)
          (cons "EaseSineIn" #'ease-sine-in)
          (cons "EaseSineOut" #'ease-sine-out)
          (cons "EaseSineInOut" #'ease-sine-in-out)
          (cons "EaseCircIn" #'ease-circ-in)
          (cons "EaseCircOut" #'ease-circ-out)
          (cons "EaseCircInOut" #'ease-circ-in-out)
          (cons "EaseCubicIn" #'ease-cubic-in)
          (cons "EaseCubicOut" #'ease-cubic-out)
          (cons "EaseCubicInOut" #'ease-cubic-in-out)
          (cons "EaseQuadIn" #'ease-quad-in)
          (cons "EaseQuadOut" #'ease-quad-out)
          (cons "EaseQuadInOut" #'ease-quad-in-out)
          (cons "EaseExpoIn" #'ease-expo-in)
          (cons "EaseExpoOut" #'ease-expo-out)
          (cons "EaseExpoInOut" #'ease-expo-in-out)
          (cons "EaseBackIn" #'ease-back-in)
          (cons "EaseBackOut" #'ease-back-out)
          (cons "EaseBackInOut" #'ease-back-in-out)
          (cons "EaseBounceOut" #'ease-bounce-out)
          (cons "EaseBounceIn" #'ease-bounce-in)
          (cons "EaseBounceInOut" #'ease-bounce-in-out)
          (cons "EaseElasticIn" #'ease-elastic-in)
          (cons "EaseElasticOut" #'ease-elastic-out)
          (cons "EaseElasticInOut" #'ease-elastic-in-out)
          (cons "None" #'no-ease)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - easings testbed")

    (let ((ball-position (vec2 100.0 100.0))
          (tt 0.0)                      ; Current time (in any unit measure, but same unit as duration)
          (d 300.0)                     ; Total time it should take to complete (duration)
          (paused t)
          (bounded-t t)                 ; If true, t will stop when d >= td, otherwise t will keep adding td to its value every loop
          (easing-x +easing-none+)      ; Easing selected for x axis
          (easing-y +easing-none+))     ; Easing selected for y axis

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-t+) (setf bounded-t (not bounded-t)))

               ;; Choose easing for the X axis
               (cond ((is-key-pressed +key-right+)
                      (incf easing-x)

                      (when (> easing-x +easing-none+) (setf easing-x 0)))
                     ((is-key-pressed +key-left+)
                      (if (= easing-x 0) (setf easing-x +easing-none+) (decf easing-x))))

               ;; Choose easing for the Y axis
               (cond ((is-key-pressed +key-down+)
                      (incf easing-y)

                      (when (> easing-y +easing-none+) (setf easing-y 0)))
                     ((is-key-pressed +key-up+)
                      (if (= easing-y 0) (setf easing-y +easing-none+) (decf easing-y))))

               ;; Change d (duration) value
               (cond ((and (is-key-pressed +key-w+) (< d (- +d-max+ +d-step+))) (incf d +d-step+))
                     ((and (is-key-pressed +key-q+) (> d (+ +d-min+ +d-step+))) (decf d +d-step+)))

               (cond ((and (is-key-down +key-s+) (< d (- +d-max+ +d-step-fine+))) (incf d +d-step-fine+))
                     ((and (is-key-down +key-a+) (> d (+ +d-min+ +d-step-fine+))) (decf d +d-step-fine+)))

               ;; Play, pause and restart controls
               (when (or (is-key-pressed +key-space+) (is-key-pressed +key-t+)
                         (is-key-pressed +key-right+) (is-key-pressed +key-left+)
                         (is-key-pressed +key-down+) (is-key-pressed +key-up+)
                         (is-key-pressed +key-w+) (is-key-pressed +key-q+)
                         (is-key-down +key-s+) (is-key-down +key-a+)
                         (and (is-key-pressed +key-enter+) bounded-t (>= tt d)))
                 (setf tt 0.0
                       (vx ball-position) 100.0
                       (vy ball-position) 100.0
                       paused t))

               (when (is-key-pressed +key-enter+) (setf paused (not paused)))

               ;; Movement computation
               (when (and (not paused) (or (and bounded-t (< tt d)) (not bounded-t)))
                 (setf (vx ball-position) (funcall (cdr (aref *easings* easing-x)) tt 100.0 (- 700.0 170.0) d)
                       (vy ball-position) (funcall (cdr (aref *easings* easing-y)) tt 100.0 (- 400.0 170.0) d))
                 (incf tt 1.0))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw information text
               (draw-text (text-format "Easing x: %s" (car (aref *easings* easing-x))) 20 +font-size+ +font-size+ +lightgray+)
               (draw-text (text-format "Easing y: %s" (car (aref *easings* easing-y))) 20 (* +font-size+ 2) +font-size+ +lightgray+)
               (draw-text (text-format "t (%c) = %.2f d = %.2f" (if bounded-t #\b #\u) tt d) 20 (* +font-size+ 3) +font-size+ +lightgray+)

               ;; Draw instructions text
               (draw-text "Use ENTER to play or pause movement, use SPACE to restart" 20 (- (get-screen-height) (* +font-size+ 2)) +font-size+ +lightgray+)
               (draw-text "Use Q and W or A and S keys to change duration" 20 (- (get-screen-height) (* +font-size+ 3)) +font-size+ +lightgray+)
               (draw-text "Use LEFT or RIGHT keys to choose easing for the x axis" 20 (- (get-screen-height) (* +font-size+ 4)) +font-size+ +lightgray+)
               (draw-text "Use UP or DOWN keys to choose easing for the y axis" 20 (- (get-screen-height) (* +font-size+ 5)) +font-size+ +lightgray+)

               ;; Draw ball
               (draw-circle-v ball-position 16.0 +maroon+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
