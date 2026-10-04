;;;; raylib [textures] example - textured curve
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Jeffery Myers (@JeffM2501) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_textured_curve.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-textured-curve
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-textured-curve)

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *tex-road* nil)

(defparameter *show-curve* nil)

(defparameter *curve-width* 50.0)
(defparameter *curve-segments* 24)

(defparameter *curve-start-position* (vec2 0.0 0.0))
(defparameter *curve-start-position-tangent* (vec2 0.0 0.0))

(defparameter *curve-end-position* (vec2 0.0 0.0))
(defparameter *curve-end-position-tangent* (vec2 0.0 0.0))

(defparameter *curve-selected-point* nil)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Draw textured curve using Spline Cubic Bezier
(defun draw-textured-curve ()
  (let ((step (/ 1.0 *curve-segments*))
        (previous (vcopy *curve-start-position*))
        (previous-tangent (vec2 0.0 0.0))
        (previous-v 0.0)
        ;; We can't compute a tangent for the first point, so we need to reuse the tangent from the first segment
        (tangent-set nil)
        (current (vec2 0.0 0.0))
        (tt 0.0))

    (loop for i from 1 to *curve-segments*
          do (setf tt (* step (float i)))

             (let* ((a (expt (- 1.0 tt) 3.0))
                    (b (* 3.0 (expt (- 1.0 tt) 2.0) tt))
                    (c (* 3.0 (- 1.0 tt) (expt tt 2.0)))
                    (d (expt tt 3.0)))

               ;; Compute the endpoint for this segment
               (setf (vy current) (+ (* a (vy *curve-start-position*)) (* b (vy *curve-start-position-tangent*))
                                     (* c (vy *curve-end-position-tangent*)) (* d (vy *curve-end-position*))))
               (setf (vx current) (+ (* a (vx *curve-start-position*)) (* b (vx *curve-start-position-tangent*))
                                     (* c (vx *curve-end-position-tangent*)) (* d (vx *curve-end-position*)))))

             (let* (;; Vector from previous to current
                    (delta (vec2 (- (vx current) (vx previous)) (- (vy current) (vy previous))))

                    ;; The right hand normal to the delta vector
                    (normal (vector2-normalize (vec2 (- (vy delta)) (vx delta))))

                    ;; The v texture coordinate of the segment (add up the length of all the segments so far)
                    (v (+ previous-v (/ (vector2-length delta) (float (* (texture-height *tex-road*) 2))))))

               ;; Make sure the start point has a normal
               (unless tangent-set
                 (setf previous-tangent normal
                       tangent-set t))

               ;; Extend out the normals from the previous and current points to get the quad for this segment
               (let ((prev-pos-normal (vector2-add previous (vector2-scale previous-tangent *curve-width*)))
                     (prev-neg-normal (vector2-add previous (vector2-scale previous-tangent (- *curve-width*))))

                     (current-pos-normal (vector2-add current (vector2-scale normal *curve-width*)))
                     (current-neg-normal (vector2-add current (vector2-scale normal (- *curve-width*)))))

                 ;; Draw the segment as a quad
                 (rl-set-texture (texture-id *tex-road*))
                 (rl-begin +rl-quads+)
                 (rl-color4ub 255 255 255 255)
                 (rl-normal3f 0.0 0.0 1.0)

                 (rl-tex-coord2f 0.0 previous-v)
                 (rl-vertex2f (vx prev-neg-normal) (vy prev-neg-normal))

                 (rl-tex-coord2f 1.0 previous-v)
                 (rl-vertex2f (vx prev-pos-normal) (vy prev-pos-normal))

                 (rl-tex-coord2f 1.0 v)
                 (rl-vertex2f (vx current-pos-normal) (vy current-pos-normal))

                 (rl-tex-coord2f 0.0 v)
                 (rl-vertex2f (vx current-neg-normal) (vy current-neg-normal))
                 (rl-end))

               ;; The current step is the start of the next step
               (setf previous (vcopy current)
                     previous-tangent normal
                     previous-v v)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags (logior +flag-vsync-hint+ +flag-msaa-4x-hint+))
    (init-window screen-width screen-height "raylib [textures] example - textured curve")

    ;; Load the road texture
    (setf *tex-road* (load-texture "resources/road.png"))
    (set-texture-filter *tex-road* +texture-filter-bilinear+)

    ;; Setup the curve
    (setf *curve-start-position* (vec2 80.0 100.0)
          *curve-start-position-tangent* (vec2 100.0 300.0)

          *curve-end-position* (vec2 700.0 350.0)
          *curve-end-position-tangent* (vec2 600.0 100.0))

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             ;; Curve config options
             (when (is-key-pressed +key-space+) (setf *show-curve* (not *show-curve*)))
             (when (is-key-pressed +key-equal+) (incf *curve-width* 2))
             (when (is-key-pressed +key-minus+) (decf *curve-width* 2))
             (when (< *curve-width* 2) (setf *curve-width* 2.0))

             ;; Update segments
             (when (is-key-pressed +key-left+) (decf *curve-segments* 2))
             (when (is-key-pressed +key-right+) (incf *curve-segments* 2))

             (when (< *curve-segments* 2) (setf *curve-segments* 2))

             ;; Update curve logic
             ;; If the mouse is not down, we are not editing the curve so clear the selection
             (unless (is-mouse-button-down +mouse-left-button+) (setf *curve-selected-point* nil))

             ;; If a point was selected, move it
             (when *curve-selected-point*
               (let ((moved (vector2-add *curve-selected-point* (get-mouse-delta))))
                 (setf (vx *curve-selected-point*) (vx moved)
                       (vy *curve-selected-point*) (vy moved))))

             ;; The mouse is down, and nothing was selected, so see if anything was picked
             (let ((mouse (get-mouse-position)))
               (cond ((check-collision-point-circle mouse *curve-start-position* 6.0) (setf *curve-selected-point* *curve-start-position*))
                     ((check-collision-point-circle mouse *curve-start-position-tangent* 6.0) (setf *curve-selected-point* *curve-start-position-tangent*))
                     ((check-collision-point-circle mouse *curve-end-position* 6.0) (setf *curve-selected-point* *curve-end-position*))
                     ((check-collision-point-circle mouse *curve-end-position-tangent* 6.0) (setf *curve-selected-point* *curve-end-position-tangent*)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-textured-curve)    ; Draw a textured Spline Cubic Bezier

               ;; Draw spline for reference
               (when *show-curve* (draw-spline-segment-bezier-cubic *curve-start-position* *curve-end-position* *curve-start-position-tangent* *curve-end-position-tangent* 2.0 +blue+))

               ;; Draw the various control points and highlight where the mouse is
               (draw-line-v *curve-start-position* *curve-start-position-tangent* +skyblue+)
               (draw-line-v *curve-start-position-tangent* *curve-end-position-tangent* (fade +lightgray+ 0.4))
               (draw-line-v *curve-end-position* *curve-end-position-tangent* +purple+)

               (when (check-collision-point-circle mouse *curve-start-position* 6.0) (draw-circle-v *curve-start-position* 7.0 +yellow+))
               (draw-circle-v *curve-start-position* 5.0 +red+)

               (when (check-collision-point-circle mouse *curve-start-position-tangent* 6.0) (draw-circle-v *curve-start-position-tangent* 7.0 +yellow+))
               (draw-circle-v *curve-start-position-tangent* 5.0 +maroon+)

               (when (check-collision-point-circle mouse *curve-end-position* 6.0) (draw-circle-v *curve-end-position* 7.0 +yellow+))
               (draw-circle-v *curve-end-position* 5.0 +green+)

               (when (check-collision-point-circle mouse *curve-end-position-tangent* 6.0) (draw-circle-v *curve-end-position-tangent* 7.0 +yellow+))
               (draw-circle-v *curve-end-position-tangent* 5.0 +darkgreen+)

               ;; Draw usage info
               (draw-text "Drag points to move curve, press SPACE to show/hide base curve" 10 10 10 +darkgray+)
               (draw-text (text-format "Curve width: %2.0f (Use + and - to adjust)" *curve-width*) 10 30 10 +darkgray+)
               (draw-text (text-format "Curve segments: %d (Use LEFT and RIGHT to adjust)" *curve-segments*) 10 50 10 +darkgray+)

               (end-drawing)))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (unload-texture *tex-road*)

    (close-window)))                    ; Close window and OpenGL context

(main)
