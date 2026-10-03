;;;; raylib [shapes] example - drag puzzle
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Gabriel Piangers (@gabriel-piangers) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Gabriel Piangers (@gabriel-piangers)
;;;; Common Lisp port of raylib/examples/shapes/shapes_drag_puzzle.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-drag-puzzle
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-drag-puzzle)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct circle
  (center (vec2 0.0 0.0))
  (radius 0.0))

(defstruct triangle
  (v1 (vec2 0.0 0.0))
  (v2 (vec2 0.0 0.0))
  (v3 (vec2 0.0 0.0)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - drag puzzle")

    (let (;; Rectangle
          (rec (make-rectangle :x (float (- (truncate screen-width 2) 250)) :y (float (+ (truncate screen-height 2) 50)) :width 100.0 :height 100.0))
          (rec-area (make-rectangle :x (float (- (truncate screen-width 2) 60)) :y (float (- (truncate screen-height 2) 110)) :width 110.0 :height 110.0))
          (rec-picked-up nil)

          ;; Circle
          (circ (make-circle :center (vec2 (float (truncate screen-width 2)) (float (+ (truncate screen-height 2) 100))) :radius 50.0))
          (circ-area (make-circle :center (vec2 (float (- (truncate screen-width 2) 195)) (float (- (truncate screen-height 2) 55))) :radius 55.0))
          (circ-picked-up nil)

          ;; Triangle
          (tri (make-triangle :v1 (vec2 600.0 282.0) :v2 (vec2 550.0 369.0) :v3 (vec2 650.0 369.0)))
          (tri-area (make-triangle :v1 (vec2 600.0 115.0) :v2 (vec2 540.0 222.0) :v3 (vec2 660.0 222.0)))
          (tri-picked-up nil)

          (mouse-offset (vec2 0.0 0.0))) ; Stores the offset of the mouse relative to the object's pivot point

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((mouse-position (get-mouse-position))
                     (rec-placed nil)
                     (circ-placed nil)
                     (tri-placed nil))

                 ;; Detect object pickup input
                 (cond ((and (is-mouse-button-pressed +mouse-button-left+) (check-collision-point-rec mouse-position rec))
                        (setf rec-picked-up t)
                        (setf mouse-offset (vec2 (- (rectangle-x rec) (vx mouse-position)) (- (rectangle-y rec) (vy mouse-position)))))
                       ((and (is-mouse-button-pressed +mouse-button-left+) (check-collision-point-circle mouse-position (circle-center circ) (circle-radius circ)))
                        (setf circ-picked-up t)
                        (setf mouse-offset (vec2 (- (vx (circle-center circ)) (vx mouse-position)) (- (vy (circle-center circ)) (vy mouse-position)))))
                       ((and (is-mouse-button-pressed +mouse-button-left+) (check-collision-point-triangle mouse-position (triangle-v1 tri) (triangle-v2 tri) (triangle-v3 tri)))
                        (setf tri-picked-up t)
                        (setf mouse-offset (vec2 (- (vx (triangle-v1 tri)) (vx mouse-position)) (- (vy (triangle-v1 tri)) (vy mouse-position)))))) ; Uses v1 as the pivot point

                 ;; Detect object drop input
                 (cond ((and (is-mouse-button-released +mouse-button-left+) rec-picked-up) (setf rec-picked-up nil))
                       ((and (is-mouse-button-released +mouse-button-left+) circ-picked-up) (setf circ-picked-up nil))
                       ((and (is-mouse-button-released +mouse-button-left+) tri-picked-up) (setf tri-picked-up nil)))

                 ;; Rectangle update
                 (when rec-picked-up
                   (setf (rectangle-x rec) (+ (vx mouse-position) (vx mouse-offset))
                         (rectangle-y rec) (+ (vy mouse-position) (vy mouse-offset))))

                 (let ((rec-col (get-collision-rec rec-area rec)))
                   (when (and (= (rectangle-width rec-col) (rectangle-width rec)) (= (rectangle-height rec-col) (rectangle-height rec))) (setf rec-placed t)))

                 ;; Circle update
                 (when circ-picked-up
                   (setf (vx (circle-center circ)) (+ (vx mouse-position) (vx mouse-offset))
                         (vy (circle-center circ)) (+ (vy mouse-position) (vy mouse-offset))))

                 (when (< (vector2-distance (circle-center circ) (circle-center circ-area)) (- (circle-radius circ-area) (circle-radius circ))) (setf circ-placed t))

                 ;; Triangle update
                 (when tri-picked-up
                   (let ((v2-offset (vec2 (- (vx (triangle-v2 tri)) (vx (triangle-v1 tri))) (- (vy (triangle-v2 tri)) (vy (triangle-v1 tri)))))
                         (v3-offset (vec2 (- (vx (triangle-v3 tri)) (vx (triangle-v1 tri))) (- (vy (triangle-v3 tri)) (vy (triangle-v1 tri))))))
                     (setf (triangle-v1 tri) (vec2 (+ (vx mouse-position) (vx mouse-offset)) (+ (vy mouse-position) (vy mouse-offset))))
                     (setf (triangle-v2 tri) (vec2 (+ (vx (triangle-v1 tri)) (vx v2-offset)) (+ (vy (triangle-v1 tri)) (vy v2-offset))))
                     (setf (triangle-v3 tri) (vec2 (+ (vx (triangle-v1 tri)) (vx v3-offset)) (+ (vy (triangle-v1 tri)) (vy v3-offset))))))

                 (when (and (check-collision-point-triangle (triangle-v1 tri) (triangle-v1 tri-area) (triangle-v2 tri-area) (triangle-v3 tri-area))
                            (check-collision-point-triangle (triangle-v2 tri) (triangle-v1 tri-area) (triangle-v2 tri-area) (triangle-v3 tri-area))
                            (check-collision-point-triangle (triangle-v2 tri) (triangle-v1 tri-area) (triangle-v2 tri-area) (triangle-v3 tri-area)))
                   (setf tri-placed t))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-rectangle-lines-ex rec-area 2.0 (if rec-placed +green+ +red+))
                 (draw-circle-lines-ex (circle-center circ-area) (circle-radius circ-area) 2.0 (if circ-placed +green+ +red+))
                 (draw-triangle-lines-ex (triangle-v1 tri-area) (triangle-v2 tri-area) (triangle-v3 tri-area) 2.0 (if tri-placed +green+ +red+))

                 ;; Draws objects that are not picked up first
                 (unless tri-picked-up (draw-triangle (triangle-v1 tri) (triangle-v2 tri) (triangle-v3 tri) +violet+))
                 (unless circ-picked-up (draw-circle-v (circle-center circ) (circle-radius circ) +blue+))
                 (unless rec-picked-up (draw-rectangle-rec rec +orange+))

                 ;; Draws the object that is being dragged on top of others
                 (when tri-picked-up (draw-triangle (triangle-v1 tri) (triangle-v2 tri) (triangle-v3 tri) +violet+))
                 (when circ-picked-up (draw-circle-v (circle-center circ) (circle-radius circ) +blue+))
                 (when rec-picked-up (draw-rectangle-rec rec +orange+))

                 (draw-text "Use mouse to drag and drop the objects into the right spot!" 10 10 20 +gray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
