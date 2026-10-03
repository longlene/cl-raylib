;;;; raylib [core] example - 3d picking
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 4.0
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_3d_picking.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-picking
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-picking)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d picking")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type
          (cube-position (vec3 0.0 1.0 0.0))
          (cube-size (vec3 2.0 2.0 2.0))
          (ray (make-ray))                ; Picking line ray
          (collision (make-ray-collision))) ; Ray collision hit info

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-cursor-hidden) (update-camera camera +camera-first-person+))

               ;; Toggle camera controls
               (when (is-mouse-button-pressed +mouse-button-right+)
                 (if (is-cursor-hidden) (enable-cursor) (disable-cursor)))

               (when (is-mouse-button-pressed +mouse-button-left+)
                 (if (not (ray-collision-hit collision))
                     (progn
                       (setf ray (get-screen-to-world-ray (get-mouse-position) camera))

                       ;; Check collision between ray and box
                       (setf collision (get-ray-collision-box
                                        ray
                                        (make-bounding-box
                                         :min (vec3 (- (vx cube-position) (/ (vx cube-size) 2))
                                                    (- (vy cube-position) (/ (vy cube-size) 2))
                                                    (- (vz cube-position) (/ (vz cube-size) 2)))
                                         :max (vec3 (+ (vx cube-position) (/ (vx cube-size) 2))
                                                    (+ (vy cube-position) (/ (vy cube-size) 2))
                                                    (+ (vz cube-position) (/ (vz cube-size) 2)))))))
                     (setf (ray-collision-hit collision) nil)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (if (ray-collision-hit collision)
                   (progn
                     (draw-cube cube-position (vx cube-size) (vy cube-size) (vz cube-size) +red+)
                     (draw-cube-wires cube-position (vx cube-size) (vy cube-size) (vz cube-size) +maroon+)

                     (draw-cube-wires cube-position (+ (vx cube-size) 0.2) (+ (vy cube-size) 0.2) (+ (vz cube-size) 0.2) +green+))
                   (progn
                     (draw-cube cube-position (vx cube-size) (vy cube-size) (vz cube-size) +gray+)
                     (draw-cube-wires cube-position (vx cube-size) (vy cube-size) (vz cube-size) +darkgray+)))

               (draw-ray ray +maroon+)
               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-text "Try clicking on the box with your mouse!" 240 10 20 +darkgray+)

               (when (ray-collision-hit collision)
                 (draw-text "BOX SELECTED" (floor (- screen-width (measure-text "BOX SELECTED" 30)) 2)
                            (truncate (* screen-height 0.1)) 30 +green+))

               (draw-text "Right click mouse to toggle camera controls" 10 430 10 +gray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
