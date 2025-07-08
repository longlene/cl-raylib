;;;; core_random_sequence.lisp
;;;; 
;;;; cl-raylib [core] example - Generates a random sequence
;;;;
;;;; Translation of raylib's core_random_sequence.c example
;;;; This example demonstrates random sequence generation and shuffling
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-random-sequence
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-random-sequence)

;; Structure to hold color and rectangle data
(defstruct color-rect
  color
  rectangle)

;; Helper functions
(defun generate-random-color ()
  "Generate a random color"
  (make-color (get-random-value 0 255)
              (get-random-value 0 255)
              (get-random-value 0 255)
              255))

(defun generate-random-color-rect-sequence (rect-count rect-width screen-width screen-height)
  "Generate a random sequence of colored rectangles"
  (let ((rectangles (make-array rect-count))
        (seq (load-random-sequence rect-count 0 (1- rect-count)))
        (rect-seq-width (* rect-count rect-width))
        (start-x (* (- screen-width rect-seq-width) 0.5)))
    
    (loop for i from 0 below rect-count do
      (let ((rect-height (remap (float (aref seq i)) 0.0 (1- rect-count) 0.0 screen-height)))
        (setf (aref rectangles i)
              (make-color-rect 
                :color (generate-random-color)
                :rectangle (make-rectangle (+ start-x (* i rect-width))
                                         (- screen-height rect-height)
                                         rect-width
                                         rect-height)))))
    
    rectangles))

(defun shuffle-color-rect-sequence (rectangles rect-count)
  "Shuffle the color and height of rectangles while keeping positions"
  (let ((seq (load-random-sequence rect-count 0 (1- rect-count))))
    
    (loop for i1 from 0 below rect-count do
      (let* ((r1 (aref rectangles i1))
             (r2 (aref rectangles (aref seq i1)))
             ;; Store original values
             (tmp-color (color-rect-color r1))
             (tmp-height (rectangle-height (color-rect-rectangle r1)))
             (tmp-y (rectangle-y (color-rect-rectangle r1))))
        
        ;; Swap colors and heights
        (setf (color-rect-color r1) (color-rect-color r2))
        (setf (rectangle-height (color-rect-rectangle r1)) 
              (rectangle-height (color-rect-rectangle r2)))
        (setf (rectangle-y (color-rect-rectangle r1)) 
              (rectangle-y (color-rect-rectangle r2)))
        
        (setf (color-rect-color r2) tmp-color)
        (setf (rectangle-height (color-rect-rectangle r2)) tmp-height)
        (setf (rectangle-y (color-rect-rectangle r2)) tmp-y)))))

(defun draw-text-center-key-help (key text pos-x pos-y font-size color)
  "Draw help text with highlighted key"
  (let* ((space-size (measure-text " " font-size))
         (press-size (measure-text "Press" font-size))
         (key-size (measure-text key font-size))
         (text-size-current 0))
    
    (draw-text "Press" pos-x pos-y font-size color)
    (incf text-size-current (+ press-size (* 2 space-size)))
    (draw-text key (+ pos-x text-size-current) pos-y font-size +red+)
    (draw-rectangle (+ pos-x text-size-current) (+ pos-y font-size) key-size 3 +red+)
    (incf text-size-current (+ key-size (* 2 space-size)))
    (draw-text text (+ pos-x text-size-current) pos-y font-size color)))

(defun load-random-sequence (count min max)
  "Generate a random sequence of unique numbers (simplified implementation)"
  (let ((seq (make-array count)))
    ;; Fill with sequential numbers
    (loop for i from 0 below count do
      (setf (aref seq i) (+ min i)))
    ;; Fisher-Yates shuffle
    (loop for i from (1- count) downto 1 do
      (let ((j (random (1+ i))))
        (rotatef (aref seq i) (aref seq j))))
    seq))

(defun remap (value input-min input-max output-min output-max)
  "Remap a value from one range to another"
  (+ output-min
     (* (- output-max output-min)
        (/ (- value input-min) (- input-max input-min)))))

(defun core-random-sequence ()
  "Random sequence example - equivalent to raylib's core_random_sequence"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - Generates a random sequence")
      (let* ((rect-count 20)
             (rect-size (/ screen-width rect-count))
             (rectangles (generate-random-color-rect-sequence rect-count rect-size screen-width (* 0.75 screen-height))))
        
        (set-target-fps 60)
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (when (is-key-pressed :key-space)
            (shuffle-color-rect-sequence rectangles rect-count))
          
          (when (is-key-pressed :key-up)
            (incf rect-count)
            (setf rect-size (/ screen-width rect-count))
            (setf rectangles (generate-random-color-rect-sequence rect-count rect-size screen-width (* 0.75 screen-height))))
          
          (when (is-key-pressed :key-down)
            (when (>= rect-count 4)
              (decf rect-count)
              (setf rect-size (/ screen-width rect-count))
              (setf rectangles (generate-random-color-rect-sequence rect-count rect-size screen-width (* 0.75 screen-height)))))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (let ((font-size 20))
              ;; Draw rectangles
              (loop for i from 0 below rect-count do
                (let ((rect-data (aref rectangles i)))
                  (draw-rectangle-rec (color-rect-rectangle rect-data) (color-rect-color rect-data))))
              
              ;; Draw help text
              (draw-text-center-key-help "SPACE" "to shuffle the sequence." 10 (- screen-height 96) font-size +black+)
              (draw-text-center-key-help "UP" "to add a rectangle and generate a new sequence." 10 (- screen-height 64) font-size +black+)
              (draw-text-center-key-help "DOWN" "to remove a rectangle and generate a new sequence." 10 (- screen-height 32) font-size +black+)
              
              ;; Draw rectangle count
              (let* ((rect-count-text (text-format "%d rectangles" rect-count))
                     (rect-count-text-size (measure-text rect-count-text font-size)))
                (draw-text rect-count-text (- screen-width rect-count-text-size 10) 10 font-size +black+))
              
              (draw-fps 10 10)))))))

;; Run the example
(core-random-sequence)