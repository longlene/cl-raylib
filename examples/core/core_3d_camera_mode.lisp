;;;; raylib [core] example - 3d camera mode
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.0, last time updated with raylib 1.0
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_3d_camera_mode.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-camera-mode
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-camera-mode)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d camera mode")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)     ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)         ; Camera up vector (rotation towards target)
                                 :fovy 45.0                     ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera mode type
          (cube-position (vec3 0.0 0.0 0.0)))

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

               (draw-cube cube-position 2.0 2.0 2.0 +red+)
               (draw-cube-wires cube-position 2.0 2.0 2.0 +maroon+)

               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-text "Welcome to the third dimension!" 10 40 20 +darkgray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
