;;;; raylib [shapes] example - rectangle scaling
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_rectangle_scaling.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-rectangle-scaling
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-rectangle-scaling)

(defconstant +mouse-scale-mark-size+ 12)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - rectangle scaling")

    (let ((rec (make-rectangle :x 100.0 :y 100.0 :width 200.0 :height 80.0))
          (mouse-position (vec2 0.0 0.0))
          (mouse-scale-ready nil)
          (mouse-scale-mode nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf mouse-position (get-mouse-position))

               (if (check-collision-point-rec mouse-position
                                              (make-rectangle :x (- (+ (rectangle-x rec) (rectangle-width rec)) +mouse-scale-mark-size+)
                                                              :y (- (+ (rectangle-y rec) (rectangle-height rec)) +mouse-scale-mark-size+)
                                                              :width (float +mouse-scale-mark-size+)
                                                              :height (float +mouse-scale-mark-size+)))
                   (progn
                     (setf mouse-scale-ready t)
                     (when (is-mouse-button-pressed +mouse-button-left+) (setf mouse-scale-mode t)))
                   (setf mouse-scale-ready nil))

               (when mouse-scale-mode
                 (setf mouse-scale-ready t)

                 (setf (rectangle-width rec) (- (vx mouse-position) (rectangle-x rec))
                       (rectangle-height rec) (- (vy mouse-position) (rectangle-y rec)))

                 ;; Check minimum rec size
                 (when (< (rectangle-width rec) +mouse-scale-mark-size+) (setf (rectangle-width rec) (float +mouse-scale-mark-size+)))
                 (when (< (rectangle-height rec) +mouse-scale-mark-size+) (setf (rectangle-height rec) (float +mouse-scale-mark-size+)))

                 ;; Check maximum rec size
                 (when (> (rectangle-width rec) (- (get-screen-width) (rectangle-x rec))) (setf (rectangle-width rec) (- (get-screen-width) (rectangle-x rec))))
                 (when (> (rectangle-height rec) (- (get-screen-height) (rectangle-y rec))) (setf (rectangle-height rec) (- (get-screen-height) (rectangle-y rec))))

                 (when (is-mouse-button-released +mouse-button-left+) (setf mouse-scale-mode nil)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "Scale rectangle dragging from bottom-right corner!" 10 10 20 +gray+)

               (draw-rectangle-rec rec (fade +green+ 0.5))

               (when mouse-scale-ready
                 (let ((x (rectangle-x rec)) (y (rectangle-y rec)) (w (rectangle-width rec)) (h (rectangle-height rec)))
                   (draw-rectangle-lines-ex rec 1.0 +red+)
                   (draw-triangle (vec2 (- (+ x w) +mouse-scale-mark-size+) (+ y h))
                                  (vec2 (+ x w) (+ y h))
                                  (vec2 (+ x w) (- (+ y h) +mouse-scale-mark-size+)) +red+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
