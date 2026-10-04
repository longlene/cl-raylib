;;;; raylib [shapes] example - hilbert curve
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Hamza RAHAL (@hmz-rhl) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Hamza RAHAL (@hmz-rhl)
;;;; Common Lisp port of raylib/examples/shapes/shapes_hilbert_curve.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-hilbert-curve
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shapes-hilbert-curve)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Compute Hilbert path U positions
(defun compute-hilbert-step (order index)
  ;; Hilbert points base pattern
  (let* ((hilbert-points (vector (vec2 0.0 0.0) (vec2 0.0 1.0) (vec2 1.0 1.0) (vec2 1.0 0.0)))
         (hilbert-index (logand index 3))
         (vect (vcopy (aref hilbert-points hilbert-index)))
         (temp 0.0)
         (len 0))

    (loop for j from 1 below order
          do (setf index (ash index -2))
             (setf hilbert-index (logand index 3))
             (setf len (ash 1 j))

             (case hilbert-index
               (0
                (setf temp (vx vect))
                (setf (vx vect) (vy vect))
                (setf (vy vect) temp))
               (2 (incf (vx vect) len)
                (incf (vy vect) len))   ; case 2 falls through case 1
               (1 (incf (vy vect) len))
               (3
                (setf temp (- len 1 (vx vect)))
                (setf (vx vect) (- (* 2 len) 1 (vy vect)))
                (setf (vy vect) temp))))

    vect))

;; Load the whole Hilbert Path (including each U and their link)
;; Returns (values path stroke-count)
(defun load-hilbert-path (order size)
  (let* ((n (ash 1 order))
         (len (/ size n))
         (stroke-count (* n n))
         (hilbert-path (make-array stroke-count)))

    (dotimes (i stroke-count)
      (let ((step (compute-hilbert-step order i)))
        (setf (vx step) (+ (* (vx step) len) (/ len 2.0))
              (vy step) (+ (* (vy step) len) (/ len 2.0)))
        (setf (aref hilbert-path i) step)))

    (values hilbert-path stroke-count)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - hilbert curve")

    (let ((order 2)
          (size (float (get-screen-height)))
          (stroke-count 0)
          (hilbert-path nil))
      (multiple-value-setq (hilbert-path stroke-count) (load-hilbert-path order size))

      (let ((prev-order order)
            (prev-size (truncate size))  ; NOTE: Size from slider is float but for comparison we use int
            (counter 0)
            (thick 2.0)
            (animate t))

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Check if order or size have changed to regenerate
                 ;; NOTE: Size from slider is float but for comparison we use int
                 (when (or (/= prev-order order) (/= prev-size (truncate size)))
                   (multiple-value-setq (hilbert-path stroke-count) (load-hilbert-path order size))

                   (if animate (setf counter 0) (setf counter stroke-count))

                   (setf prev-order order
                         prev-size (truncate size)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (if (< counter stroke-count)
                     (progn
                       ;; Draw Hilbert path animation, one stroke every frame
                       (loop for i from 1 to counter
                             do (draw-line-ex (aref hilbert-path i) (aref hilbert-path (1- i)) thick (color-from-hsv (* (/ (float i) stroke-count) 360.0) 1.0 1.0)))

                       (incf counter 1))
                     ;; Draw full Hilbert path
                     (loop for i from 1 below stroke-count
                           do (draw-line-ex (aref hilbert-path i) (aref hilbert-path (1- i)) thick (color-from-hsv (* (/ (float i) stroke-count) 360.0) 1.0 1.0))))

                 ;; Draw UI using raygui
                 (setf animate (nth-value 1 (gui-check-box (make-rectangle :x 450.0 :y 50.0 :width 20.0 :height 20.0) "ANIMATE GENERATION ON CHANGE" animate)))
                 (setf order (nth-value 1 (gui-spinner (make-rectangle :x 585.0 :y 100.0 :width 180.0 :height 30.0) "HILBERT CURVE ORDER:  " order 2 8 nil)))
                 (setf thick (nth-value 1 (gui-slider (make-rectangle :x 524.0 :y 150.0 :width 240.0 :height 24.0) "THICKNESS:  " nil thick 1.0 10.0)))
                 (setf size (nth-value 1 (gui-slider (make-rectangle :x 524.0 :y 190.0 :width 240.0 :height 24.0) "TOTAL SIZE: " nil size 10.0 (* (get-screen-height) 1.5))))

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (close-window)))))              ; Close window and OpenGL context

(main)
