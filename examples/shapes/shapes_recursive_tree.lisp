;;;; raylib [shapes] example - recursive tree
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jopestpe (@jopestpe)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jopestpe (@jopestpe)
;;;; Common Lisp port of raylib/examples/shapes/shapes_recursive_tree.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-recursive-tree
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-recursive-tree)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct branch
  (start (vec2 0.0 0.0))
  (end (vec2 0.0 0.0))
  (angle 0.0)
  (length 0.0))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - recursive tree")

    (let ((start (vec2 (- (/ screen-width 2.0) 125.0) (float screen-height)))
          (angle 40.0)
          (thick 1.0)
          (tree-depth 10.0)
          (branch-decay 0.66)
          (length 120.0)
          (bezier nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let* ((theta (* angle +deg2rad+))
                      (max-branches (truncate (expt 2.0 (ffloor tree-depth))))
                      (branches (make-array 1030 :initial-element nil))
                      (count 0)
                      (initial-end (vec2 (+ (vx start) (* length (sin 0.0))) (- (vy start) (* length (cos 0.0))))))

                 (setf (aref branches count) (make-branch :start start :end initial-end :angle 0.0 :length length))
                 (incf count)

                 (loop for i from 0
                       while (< i count)
                       do (let ((branch (aref branches i)))
                            (when (>= (branch-length branch) 2)
                              (let ((next-length (* (branch-length branch) branch-decay)))
                                (when (and (< count max-branches) (>= next-length 2))
                                  (let* ((branch-start (branch-end branch))
                                         (angle1 (+ (branch-angle branch) theta))
                                         (branch-end1 (vec2 (+ (vx branch-start) (* next-length (sin angle1))) (- (vy branch-start) (* next-length (cos angle1)))))
                                         (angle2 (- (branch-angle branch) theta))
                                         (branch-end2 (vec2 (+ (vx branch-start) (* next-length (sin angle2))) (- (vy branch-start) (* next-length (cos angle2))))))
                                    (setf (aref branches count) (make-branch :start branch-start :end branch-end1 :angle angle1 :length next-length))
                                    (incf count)
                                    (setf (aref branches count) (make-branch :start branch-start :end branch-end2 :angle angle2 :length next-length))
                                    (incf count)))))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (dotimes (i count)
                   (let ((branch (aref branches i)))
                     (when (>= (branch-length branch) 2)
                       (if bezier
                           (draw-line-bezier (branch-start branch) (branch-end branch) thick +red+)
                           (draw-line-ex (branch-start branch) (branch-end branch) thick +red+)))))

                 (draw-line 580 0 580 (get-screen-height) (list 218 218 218 255))
                 (draw-rectangle 580 0 (get-screen-width) (get-screen-height) (list 232 232 232 255))

                 ;; Draw GUI controls
                 ;;------------------------------------------------------------------------------
                 (setf angle (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 40.0 :width 120.0 :height 20.0) "Angle" (text-format "%.0f" angle) angle 0 180)))
                 (setf length (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 70.0 :width 120.0 :height 20.0) "Length" (text-format "%.0f" length) length 12.0 240.0)))
                 (setf branch-decay (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 100.0 :width 120.0 :height 20.0) "Decay" (text-format "%.2f" branch-decay) branch-decay 0.1 0.78)))
                 (setf tree-depth (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 130.0 :width 120.0 :height 20.0) "Depth" (text-format "%.0f" tree-depth) tree-depth 1.0 10.0)))
                 (setf thick (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 160.0 :width 120.0 :height 20.0) "Thick" (text-format "%.0f" thick) thick 1 8)))
                 (setf bezier (nth-value 1 (gui-check-box (make-rectangle :x 640.0 :y 190.0 :width 20.0 :height 20.0) "Bezier" bezier)))
                 ;;------------------------------------------------------------------------------

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
