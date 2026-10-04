;;;; raylib [shapes] example - kaleidoscope
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Hugo ARNAL (@hugoarnal) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Hugo ARNAL (@hugoarnal) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_kaleidoscope.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-kaleidoscope
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shapes-kaleidoscope)

(defconstant +max-draw-lines+ 8192)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Line data type
(defstruct line
  (start (vec2 0.0 0.0))
  (end (vec2 0.0 0.0)))

;; Lines array as a global static variable to be stored
;; in heap and avoid potential stack overflow (on Web platform)
(defparameter *lines* (let ((a (make-array +max-draw-lines+))) (dotimes (i +max-draw-lines+ a) (setf (aref a i) (make-line)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - kaleidoscope")

    ;; Line drawing properties
    (let* ((symmetry 6)
           (angle (/ 360.0 (float symmetry)))
           (thickness 3.0)
           (reset-button-rec (make-rectangle :x (- screen-width 55.0) :y 5.0 :width 50.0 :height 25.0))
           (back-button-rec (make-rectangle :x (- screen-width 55.0) :y (- screen-height 30.0) :width 25.0 :height 25.0))
           (next-button-rec (make-rectangle :x (- screen-width 30.0) :y (- screen-height 30.0) :width 25.0 :height 25.0))
           (mouse-pos (vec2 0.0 0.0))
           (prev-mouse-pos (vec2 0.0 0.0))
           (scale-vector (vec2 1.0 -1.0))
           (offset (vec2 (/ (float screen-width) 2.0) (/ (float screen-height) 2.0)))

           (camera (make-camera2d :target (vec2 0.0 0.0) :offset offset :rotation 0.0 :zoom 1.0))

           (current-line-counter 0)
           (total-line-counter 0)
           (reset-button-clicked 0)
           (back-button-clicked 0)
           (next-button-clicked 0))

      (set-target-fps 20)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf prev-mouse-pos mouse-pos
                     mouse-pos (get-mouse-position))

               (let ((line-start (vector2-subtract mouse-pos offset))
                     (line-end (vector2-subtract prev-mouse-pos offset)))

                 (when (and (is-mouse-button-down +mouse-left-button+)
                            (not (check-collision-point-rec mouse-pos reset-button-rec))
                            (not (check-collision-point-rec mouse-pos back-button-rec))
                            (not (check-collision-point-rec mouse-pos next-button-rec)))
                   (loop for s from 0
                         while (and (< s symmetry) (< total-line-counter (- +max-draw-lines+ 1)))
                         do (setf line-start (vector2-rotate line-start (* angle +deg2rad+))
                                  line-end (vector2-rotate line-end (* angle +deg2rad+)))

                            ;; Store mouse line
                            (setf (line-start (aref *lines* total-line-counter)) line-start
                                  (line-end (aref *lines* total-line-counter)) line-end)

                            ;; Store reflective line
                            (setf (line-start (aref *lines* (+ total-line-counter 1))) (vector2-multiply line-start scale-vector)
                                  (line-end (aref *lines* (+ total-line-counter 1))) (vector2-multiply line-end scale-vector))

                            (incf total-line-counter 2)
                            (setf current-line-counter total-line-counter))))

               (when (/= reset-button-clicked 0)
                 (dotimes (i +max-draw-lines+) (setf (aref *lines* i) (make-line)))
                 (setf current-line-counter 0
                       total-line-counter 0))

               (when (and (/= back-button-clicked 0) (> current-line-counter 0))
                 (decf current-line-counter 1))

               (when (and (/= next-button-clicked 0) (< current-line-counter +max-draw-lines+) (<= (+ current-line-counter 1) total-line-counter))
                 (incf current-line-counter 1))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-2d camera)
               (dotimes (s symmetry)
                 (loop for i from 0 below current-line-counter by 2
                       do (draw-line-ex (line-start (aref *lines* i)) (line-end (aref *lines* i)) thickness +black+)
                          (draw-line-ex (line-start (aref *lines* (+ i 1))) (line-end (aref *lines* (+ i 1))) thickness +black+)))
               (end-mode-2d)

               (when (< (- current-line-counter 1) 0) (gui-disable))
               (setf back-button-clicked (gui-button back-button-rec "<"))
               (gui-enable)

               (when (> (+ current-line-counter 1) total-line-counter) (gui-disable))
               (setf next-button-clicked (gui-button next-button-rec ">"))
               (gui-enable)

               (setf reset-button-clicked (gui-button reset-button-rec "Reset"))

               (draw-text (text-format "LINES: %i/%i" current-line-counter +max-draw-lines+) 10 (- screen-height 30) 20 +maroon+)
               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
