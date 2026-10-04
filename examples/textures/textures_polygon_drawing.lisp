;;;; raylib [textures] example - polygon drawing
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 3.7, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_polygon_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-polygon-drawing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-polygon-drawing)

(defconstant +max-points+ 11)           ; 10 points and back to the start

;; Draw textured polygon, defined by vertex and texture coordinates
;; NOTE: Polygon center must have straight line path to all points
;; without crossing perimeter, points must be in anticlockwise order
(defun draw-texture-poly (texture center points texcoords point-count tint)
  (rl-set-texture (texture-id texture))

  (rl-begin +rl-triangles+)

  (rl-color4ub (first tint) (second tint) (third tint) (fourth tint))

  (dotimes (i (1- point-count))
    (rl-tex-coord2f 0.5 0.5)
    (rl-vertex2f (vx center) (vy center))

    (rl-tex-coord2f (vx (aref texcoords i)) (vy (aref texcoords i)))
    (rl-vertex2f (+ (vx (aref points i)) (vx center)) (+ (vy (aref points i)) (vy center)))

    (rl-tex-coord2f (vx (aref texcoords (1+ i))) (vy (aref texcoords (1+ i))))
    (rl-vertex2f (+ (vx (aref points (1+ i))) (vx center)) (+ (vy (aref points (1+ i))) (vy center))))
  (rl-end)

  (rl-set-texture 0))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - polygon drawing")

    (let* (;; Define texture coordinates to map our texture to poly
           (texcoords (vector (vec2 0.75 0.0)
                              (vec2 0.25 0.0)
                              (vec2 0.0 0.5)
                              (vec2 0.0 0.75)
                              (vec2 0.25 1.0)
                              (vec2 0.375 0.875)
                              (vec2 0.625 0.875)
                              (vec2 0.75 1.0)
                              (vec2 1.0 0.75)
                              (vec2 1.0 0.5)
                              (vec2 0.75 0.0))) ; Close the poly

           ;; Define the base poly vertices from the UV's
           ;; NOTE: They can be specified in any other way
           (points (map 'vector (lambda (tc) (vec2 (* (- (vx tc) 0.5) 256.0) (* (- (vy tc) 0.5) 256.0))) texcoords))

           ;; Define the vertices drawing position
           ;; NOTE: Initially same as points but updated every frame
           (positions (map 'vector #'vcopy points))

           ;; Load texture to be mapped to poly
           (texture (load-texture "resources/cat.png"))

           (angle 0.0))                 ; Rotation angle (in degrees)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Update points rotation with an angle transform
               ;; NOTE: Base points position are not modified
               (incf angle)
               (dotimes (i +max-points+) (setf (aref positions i) (vector2-rotate (aref points i) (* angle +deg2rad+))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "textured polygon" 20 20 20 +darkgray+)

               (draw-texture-poly texture (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0))
                                  positions texcoords +max-points+ +white+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
