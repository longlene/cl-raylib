;;;; raylib [models] example - geometric shapes
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_geometric_shapes.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-geometric-shapes
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-geometric-shapes)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - geometric shapes")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0)
                                 :target (vec3 0.0 0.0 0.0)
                                 :up (vec3 0.0 1.0 0.0)
                                 :fovy 45.0
                                 :projection +camera-perspective+)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; TODO: Update your variables here
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-cube (vec3 -4.0 0.0 2.0) 2.0 5.0 2.0 +red+)
               (draw-cube-wires (vec3 -4.0 0.0 2.0) 2.0 5.0 2.0 +gold+)
               (draw-cube-wires (vec3 -4.0 0.0 -2.0) 3.0 6.0 2.0 +maroon+)

               (draw-sphere (vec3 -1.0 0.0 -2.0) 1.0 +green+)
               (draw-sphere-wires (vec3 1.0 0.0 2.0) 2.0 16 16 +lime+)

               (draw-cylinder (vec3 4.0 0.0 -2.0) 1.0 2.0 3.0 4 +skyblue+)
               (draw-cylinder-wires (vec3 4.0 0.0 -2.0) 1.0 2.0 3.0 4 +darkblue+)
               (draw-cylinder-wires (vec3 4.5 -1.0 2.0) 1.0 1.0 2.0 6 +brown+)

               (draw-cylinder (vec3 1.0 0.0 -4.0) 0.0 1.5 3.0 8 +gold+)
               (draw-cylinder-wires (vec3 1.0 0.0 -4.0) 0.0 1.5 3.0 8 +pink+)

               (draw-capsule (vec3 -3.0 1.5 -4.0) (vec3 -4.0 -1.0 -4.0) 1.2 8 8 +violet+)
               (draw-capsule-wires (vec3 -3.0 1.5 -4.0) (vec3 -4.0 -1.0 -4.0) 1.2 8 8 +purple+)

               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
