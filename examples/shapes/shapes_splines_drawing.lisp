;;;; raylib [shapes] example - splines drawing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_splines_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-splines-drawing
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for UI controls
(in-package #:raylib-examples/shapes-splines-drawing)

(defconstant +max-spline-points+ 32)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Cubic Bezier spline control points
;; NOTE: Every segment has two control points, kept in the CONTROL-START and
;; CONTROL-END arrays; the C Vector2 pointers to them are the vec2 objects themselves

;; Spline types
(defconstant +spline-linear+ 0)         ; Linear
(defconstant +spline-basis+ 1)          ; B-Spline
(defconstant +spline-catmullrom+ 2)     ; Catmull-Rom
(defconstant +spline-bezier+ 3)         ; Cubic Bezier

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - splines drawing")

    (let* ((points (let ((p (make-array +max-spline-points+)))
                     (dotimes (i +max-spline-points+) (setf (aref p i) (vec2 0.0 0.0)))
                     (replace p (list (vec2 50.0 400.0)
                                      (vec2 160.0 220.0)
                                      (vec2 340.0 380.0)
                                      (vec2 520.0 60.0)
                                      (vec2 710.0 260.0)))))

           ;; Array required for spline bezier-cubic,
           ;; including control points interleaved with start-end segment points
           (points-interleaved (make-array (1+ (* 3 (1- +max-spline-points+))) :initial-element (vec2 0.0 0.0)))

           (point-count 5)
           (selected-point -1)
           (focused-point -1)
           (selected-control-point nil)
           (focused-control-point nil)

           ;; Cubic Bezier control points initialization
           (control-start (make-array (1- +max-spline-points+)))
           (control-end (make-array (1- +max-spline-points+)))

           ;; Spline config variables
           (spline-thickness 8.0)
           (spline-type-active +spline-linear+) ; 0-Linear, 1-BSpline, 2-CatmullRom, 3-Bezier
           (spline-type-edit-mode nil)
           (spline-helpers-active t))

      (dotimes (i (1- +max-spline-points+))
        (setf (aref control-start i) (vec2 0.0 0.0)
              (aref control-end i) (vec2 0.0 0.0)))
      (dotimes (i (1- point-count))
        (setf (aref control-start i) (vec2 (+ (vx (aref points i)) 50) (vy (aref points i)))
              (aref control-end i) (vec2 (- (vx (aref points (1+ i))) 50) (vy (aref points (1+ i))))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Spline points creation logic (at the end of spline)
               (when (and (is-mouse-button-pressed +mouse-right-button+) (< point-count +max-spline-points+))
                 (setf (aref points point-count) (vcopy (get-mouse-position)))
                 (let ((i (1- point-count)))
                   (setf (aref control-start i) (vec2 (+ (vx (aref points i)) 50) (vy (aref points i)))
                         (aref control-end i) (vec2 (- (vx (aref points (1+ i))) 50) (vy (aref points (1+ i))))))
                 (incf point-count))

               ;; Spline point focus and selection logic
               (when (and (= selected-point -1) (or (/= spline-type-active +spline-bezier+) (null selected-control-point)))
                 (setf focused-point -1)
                 (dotimes (i point-count)
                   (when (check-collision-point-circle (get-mouse-position) (aref points i) 8.0)
                     (setf focused-point i)
                     (return)))
                 (when (is-mouse-button-pressed +mouse-left-button+) (setf selected-point focused-point)))

               ;; Spline point movement logic
               (when (>= selected-point 0)
                 (setf (aref points selected-point) (vcopy (get-mouse-position)))
                 (when (is-mouse-button-released +mouse-left-button+) (setf selected-point -1)))

               ;; Cubic Bezier spline control points logic
               (when (and (= spline-type-active +spline-bezier+) (= focused-point -1))
                 ;; Spline control point focus and selection logic
                 (when (null selected-control-point)
                   (setf focused-control-point nil)
                   (dotimes (i (1- point-count))
                     (cond ((check-collision-point-circle (get-mouse-position) (aref control-start i) 6.0)
                            (setf focused-control-point (aref control-start i))
                            (return))
                           ((check-collision-point-circle (get-mouse-position) (aref control-end i) 6.0)
                            (setf focused-control-point (aref control-end i))
                            (return))))
                   (when (is-mouse-button-pressed +mouse-left-button+) (setf selected-control-point focused-control-point)))

                 ;; Spline control point movement logic
                 (when selected-control-point
                   (let ((mouse (get-mouse-position)))
                     (setf (vx selected-control-point) (vx mouse)
                           (vy selected-control-point) (vy mouse)))
                   (when (is-mouse-button-released +mouse-left-button+) (setf selected-control-point nil))))

               ;; Spline selection logic
               (cond ((is-key-pressed +key-one+) (setf spline-type-active 0))
                     ((is-key-pressed +key-two+) (setf spline-type-active 1))
                     ((is-key-pressed +key-three+) (setf spline-type-active 2))
                     ((is-key-pressed +key-four+) (setf spline-type-active 3)))

               ;; Clear selection when changing to a spline without control points
               (when (or (is-key-pressed +key-one+) (is-key-pressed +key-two+) (is-key-pressed +key-three+)) (setf selected-control-point nil))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (cond
                 ((= spline-type-active +spline-linear+)
                  ;; Draw spline: linear
                  (draw-spline-linear points point-count spline-thickness +red+))
                 ((= spline-type-active +spline-basis+)
                  ;; Draw spline: basis
                  (draw-spline-basis points point-count spline-thickness +red+)) ; Provide connected points array
                 ((= spline-type-active +spline-catmullrom+)
                  ;; Draw spline: catmull-rom
                  (draw-spline-catmull-rom points point-count spline-thickness +red+)) ; Provide connected points array
                 ((= spline-type-active +spline-bezier+)
                  ;; NOTE: Cubic-bezier spline requires the 2 control points of each segnment to be
                  ;; provided interleaved with the start and end point of every segment
                  (dotimes (i (1- point-count))
                    (setf (aref points-interleaved (* 3 i)) (aref points i)
                          (aref points-interleaved (+ (* 3 i) 1)) (aref control-start i)
                          (aref points-interleaved (+ (* 3 i) 2)) (aref control-end i)))

                  (setf (aref points-interleaved (* 3 (1- point-count))) (aref points (1- point-count)))

                  ;; Draw spline: cubic-bezier (with control points)
                  (draw-spline-bezier-cubic points-interleaved (1+ (* 3 (1- point-count))) spline-thickness +red+)

                  ;; Draw spline control points
                  (dotimes (i (1- point-count))
                    ;; Every cubic bezier point have two control points
                    (draw-circle-v (aref control-start i) 6 +gold+)
                    (draw-circle-v (aref control-end i) 6 +gold+)
                    (cond ((eq focused-control-point (aref control-start i)) (draw-circle-v (aref control-start i) 8 +green+))
                          ((eq focused-control-point (aref control-end i)) (draw-circle-v (aref control-end i) 8 +green+)))
                    (draw-line-ex (aref points i) (aref control-start i) 1.0 +lightgray+)
                    (draw-line-ex (aref points (1+ i)) (aref control-end i) 1.0 +lightgray+)

                    ;; Draw spline control lines
                    (draw-line-v (aref points i) (aref control-start i) +gray+)
                    ;;(draw-line-v (aref control-start i) (aref control-end i) +lightgray+)
                    (draw-line-v (aref control-end i) (aref points (1+ i)) +gray+))))

               (when spline-helpers-active
                 ;; Draw spline point helpers
                 (dotimes (i point-count)
                   (draw-circle-lines-v (aref points i) (if (= focused-point i) 12.0 8.0) (if (= focused-point i) +blue+ +darkblue+))
                   (when (and (/= spline-type-active +spline-linear+)
                              (/= spline-type-active +spline-bezier+)
                              (< i (1- point-count)))
                     (draw-line-v (aref points i) (aref points (1+ i)) +gray+))

                   (draw-text (text-format "[%.0f, %.0f]" (vx (aref points i)) (vy (aref points i))) (truncate (vx (aref points i))) (+ (truncate (vy (aref points i))) 10) 10 +black+)))

               ;; Check all possible UI states that require controls lock
               (when (or spline-type-edit-mode (/= selected-point -1) selected-control-point) (gui-lock))

               ;; Draw spline config
               (gui-label (make-rectangle :x 12.0 :y 62.0 :width 140.0 :height 24.0) (text-format "Spline thickness: %i" (truncate spline-thickness)))
               (setf spline-thickness (nth-value 1 (gui-slider-bar (make-rectangle :x 12.0 :y (+ 60.0 24) :width 140.0 :height 16.0) nil nil spline-thickness 1.0 40.0)))

               (setf spline-helpers-active (nth-value 1 (gui-check-box (make-rectangle :x 12.0 :y 110.0 :width 20.0 :height 20.0) "Show point helpers" spline-helpers-active)))

               (when spline-type-edit-mode (gui-unlock))

               (gui-label (make-rectangle :x 12.0 :y 10.0 :width 140.0 :height 24.0) "Spline type:")
               (multiple-value-bind (result active)
                   (gui-dropdown-box (make-rectangle :x 12.0 :y (+ 8.0 24) :width 140.0 :height 28.0) "LINEAR;BSPLINE;CATMULLROM;BEZIER" spline-type-active spline-type-edit-mode)
                 (setf spline-type-active active)
                 (when (/= result 0) (setf spline-type-edit-mode (not spline-type-edit-mode))))

               (gui-unlock)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
