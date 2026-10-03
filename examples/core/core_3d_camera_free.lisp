;;;; raylib [core] example - 3d camera free
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.3
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_3d_camera_free.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-camera-free
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-camera-free)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d camera free")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type
          (cube-position (vec3 0.0 0.0 0.0)))

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+)

               (when (is-key-pressed +key-z+) (setf (camera3d-target camera) (vec3 0.0 0.0 0.0)))
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

               (draw-rectangle 10 10 320 93 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 10 10 320 93 +blue+)

               (draw-text "Free camera default controls:" 20 20 10 +black+)
               (draw-text "- Mouse Wheel to Zoom in-out" 40 40 10 +darkgray+)
               (draw-text "- Mouse Wheel Pressed to Pan" 40 60 10 +darkgray+)
               (draw-text "- Z to zoom to (0, 0, 0)" 40 80 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
