;;;; raylib [models] example - orthographic projection
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Max Danielsson (@autious) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Max Danielsson (@autious) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_orthographic_projection.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-orthographic-projection
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-orthographic-projection)

(defconstant +fovy-perspective+ 45.0)
(defconstant +width-orthographic+ 10.0)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - orthographic projection")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0) :target (vec3 0.0 0.0 0.0) :up (vec3 0.0 1.0 0.0)
                                 :fovy +fovy-perspective+ :projection +camera-perspective+)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+)
                 (if (= (camera3d-projection camera) +camera-perspective+)
                     (setf (camera3d-fovy camera) +width-orthographic+
                           (camera3d-projection camera) +camera-orthographic+)
                     (setf (camera3d-fovy camera) +fovy-perspective+
                           (camera3d-projection camera) +camera-perspective+)))
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

               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-text "Press Spacebar to switch camera type" 10 (- (get-screen-height) 30) 20 +darkgray+)

               (cond ((= (camera3d-projection camera) +camera-orthographic+) (draw-text "ORTHOGRAPHIC" 10 40 20 +black+))
                     ((= (camera3d-projection camera) +camera-perspective+) (draw-text "PERSPECTIVE" 10 40 20 +black+)))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
