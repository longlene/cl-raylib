;;;; raylib [core] example - world screen
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.4
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_world_screen.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-world-screen
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-world-screen)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - world screen")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type
          (cube-position (vec3 0.0 0.0 0.0))
          (cube-screen-position (vec2 0.0 0.0)))

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-third-person+)

               ;; Calculate cube screen space position (with a little offset to be in top)
               (setf cube-screen-position
                     (get-world-to-screen (vec3 (vx cube-position) (+ (vy cube-position) 2.5) (vz cube-position)) camera))
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

               (draw-text "Enemy: 100/100" (- (truncate (vx cube-screen-position)) (truncate (measure-text "Enemy: 100/100" 20) 2))
                          (truncate (vy cube-screen-position)) 20 +black+)

               (draw-text (text-format "Cube position in screen space coordinates: [%i, %i]"
                                       (truncate (vx cube-screen-position)) (truncate (vy cube-screen-position)))
                          10 10 20 +lime+)
               (draw-text "Text 2d should be always on top of the cube" 10 40 20 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
