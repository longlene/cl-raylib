;;;; raylib [core] example - random sequence
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example contributed by Dalton Overmyer (@REDl3east) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2023-2025 Dalton Overmyer (@REDl3east)
;;;; Common Lisp port of raylib/examples/core/core_random_sequence.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-random-sequence
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-random-sequence)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct color-rect
  color
  rect)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun generate-random-color ()
  (list (get-random-value 0 255)
        (get-random-value 0 255)
        (get-random-value 0 255)
        255))

(defun generate-random-color-rect-sequence (rect-count rect-width screen-width screen-height)
  (let* ((rectangles (make-array (truncate rect-count)))
         (seq (load-random-sequence (truncate rect-count) 0 (1- (truncate rect-count))))
         (rect-seq-width (* rect-count rect-width))
         (start-x (* (- screen-width rect-seq-width) 0.5)))

    (dotimes (i (truncate rect-count))
      (let ((rect-height (truncate (remap (float (aref seq i)) 0.0 (- rect-count 1) 0.0 screen-height))))
        (setf (aref rectangles i)
              (make-color-rect :color (generate-random-color)
                               :rect (make-rectangle :x (+ start-x (* i rect-width)) :y (- screen-height rect-height)
                                                     :width rect-width :height (float rect-height))))))

    (unload-random-sequence seq)

    rectangles))

(defun shuffle-color-rect-sequence (rectangles rect-count)
  (let ((seq (load-random-sequence rect-count 0 (1- rect-count))))

    (dotimes (i1 rect-count)
      (let* ((r1 (aref rectangles i1))
             (r2 (aref rectangles (aref seq i1)))
             ;; Swap only the color and height
             (tmp-color (color-rect-color r1))
             (tmp-height (rectangle-height (color-rect-rect r1)))
             (tmp-y (rectangle-y (color-rect-rect r1))))
        (setf (color-rect-color r1) (color-rect-color r2)
              (rectangle-height (color-rect-rect r1)) (rectangle-height (color-rect-rect r2))
              (rectangle-y (color-rect-rect r1)) (rectangle-y (color-rect-rect r2)))
        (setf (color-rect-color r2) tmp-color
              (rectangle-height (color-rect-rect r2)) tmp-height
              (rectangle-y (color-rect-rect r2)) tmp-y)))

    (unload-random-sequence seq)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - random sequence")

    (let* ((rect-count 20)
           (rect-size (/ (float screen-width) rect-count))
           (rectangles (generate-random-color-rect-sequence (float rect-count) rect-size (float screen-width) (* 0.75 screen-height))))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+) (shuffle-color-rect-sequence rectangles rect-count))

               (when (is-key-pressed +key-up+)
                 (incf rect-count)
                 (setf rect-size (/ (float screen-width) rect-count))

                 ;; Re-generate random sequence with new count
                 (setf rectangles (generate-random-color-rect-sequence (float rect-count) rect-size (float screen-width) (* 0.75 screen-height))))

               (when (is-key-pressed +key-down+)
                 (when (>= rect-count 4)
                   (decf rect-count)
                   (setf rect-size (/ (float screen-width) rect-count))

                   ;; Re-generate random sequence with new count
                   (setf rectangles (generate-random-color-rect-sequence (float rect-count) rect-size (float screen-width) (* 0.75 screen-height)))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i rect-count)
                 (draw-rectangle-rec (color-rect-rect (aref rectangles i)) (color-rect-color (aref rectangles i)))

                 (draw-text "Press SPACE to shuffle the current sequence" 10 (- screen-height 96) 20 +black+)
                 (draw-text "Press UP to add a rectangle and generate a new sequence" 10 (- screen-height 64) 20 +black+)
                 (draw-text "Press DOWN to remove a rectangle and generate a new sequence" 10 (- screen-height 32) 20 +black+))

               (draw-text (text-format "Count: %d rectangles" rect-count) 10 10 20 +maroon+)

               (draw-fps (- screen-width 80) 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
