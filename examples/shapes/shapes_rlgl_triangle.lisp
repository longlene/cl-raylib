;;;; raylib [shapes] example - rlgl triangle
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Robin (@RobinsAviary)
;;;; Common Lisp port of raylib/examples/shapes/shapes_rlgl_triangle.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-rlgl-triangle
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-rlgl-triangle)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - rlgl triangle")

    (let* (;; Starting postions and rendered triangle positions
           (starting-positions (vector (vec2 400.0 150.0) (vec2 300.0 300.0) (vec2 500.0 300.0)))
           (triangle-positions (map 'vector #'vcopy starting-positions))
           ;; Currently selected vertex, -1 means none
           (triangle-index -1)
           (lines-mode nil)
           (handle-radius 8.0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+) (setf lines-mode (not lines-mode)))

               ;; Check selected vertex
               (dotimes (i 3)
                 ;; If the mouse is within the handle circle
                 (when (and (check-collision-point-circle (get-mouse-position) (aref triangle-positions i) handle-radius)
                            (is-mouse-button-down +mouse-button-left+))
                   (setf triangle-index i)
                   (return)))

               ;; If the user has selected a vertex, offset it by the mouse's delta this frame
               (when (/= triangle-index -1)
                 (let ((position (aref triangle-positions triangle-index))
                       (mouse-delta (get-mouse-delta)))
                   (incf (vx position) (vx mouse-delta))
                   (incf (vy position) (vy mouse-delta))))

               ;; Reset index on release
               (when (is-mouse-button-released +mouse-button-left+) (setf triangle-index -1))

               ;; Enable/disable backface culling (2-sided triangles, slower to render)
               (when (is-key-pressed +key-left+) (rl-enable-backface-culling))
               (when (is-key-pressed +key-right+) (rl-disable-backface-culling))

               ;; Reset triangle vertices to starting positions and reset backface culling
               (when (is-key-pressed +key-r+)
                 (setf triangle-positions (map 'vector #'vcopy starting-positions))

                 (rl-enable-backface-culling))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (if lines-mode
                   (progn
                     ;; Draw triangle with lines
                     (rl-begin +rl-lines+)
                     ;; Three lines, six points
                     ;; Define color for next vertex
                     (rl-color4ub 255 0 0 255)
                     ;; Define vertex
                     (rl-vertex2f (vx (aref triangle-positions 0)) (vy (aref triangle-positions 0)))
                     (rl-color4ub 0 255 0 255)
                     (rl-vertex2f (vx (aref triangle-positions 1)) (vy (aref triangle-positions 1)))
                     (rl-color4ub 0 255 0 255)
                     (rl-vertex2f (vx (aref triangle-positions 1)) (vy (aref triangle-positions 1)))
                     (rl-color4ub 0 0 255 255)
                     (rl-vertex2f (vx (aref triangle-positions 2)) (vy (aref triangle-positions 2)))
                     (rl-color4ub 0 0 255 255)
                     (rl-vertex2f (vx (aref triangle-positions 2)) (vy (aref triangle-positions 2)))
                     (rl-color4ub 255 0 0 255)
                     (rl-vertex2f (vx (aref triangle-positions 0)) (vy (aref triangle-positions 0)))
                     (rl-end))
                   (progn
                     ;; Draw triangle as a triangle
                     (rl-begin +rl-triangles+)
                     ;; One triangle, three points
                     ;; Define color for next vertex
                     (rl-color4ub 255 0 0 255)
                     ;; Define vertex
                     (rl-vertex2f (vx (aref triangle-positions 0)) (vy (aref triangle-positions 0)))
                     (rl-color4ub 0 255 0 255)
                     (rl-vertex2f (vx (aref triangle-positions 1)) (vy (aref triangle-positions 1)))
                     (rl-color4ub 0 0 255 255)
                     (rl-vertex2f (vx (aref triangle-positions 2)) (vy (aref triangle-positions 2)))
                     (rl-end)))

               ;; Render the vertex handles, reacting to mouse movement/input
               (dotimes (i 3)
                 ;; Draw handle fill focused by mouse
                 (when (check-collision-point-circle (get-mouse-position) (aref triangle-positions i) handle-radius)
                   (draw-circle-v (aref triangle-positions i) handle-radius (color-alpha +darkgray+ 0.5)))

                 ;; Draw handle fill selected
                 (when (= i triangle-index) (draw-circle-v (aref triangle-positions i) handle-radius +darkgray+))

                 ;; Draw handle outline
                 (draw-circle-lines-v (aref triangle-positions i) handle-radius +black+))

               ;; Draw controls
               (draw-text "SPACE: Toggle lines mode" 10 10 20 +darkgray+)
               (draw-text "LEFT-RIGHT: Toggle backface culling" 10 40 20 +darkgray+)
               (draw-text "MOUSE: Click and drag vertex points" 10 70 20 +darkgray+)
               (draw-text "R: Reset triangle to start positions" 10 100 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
