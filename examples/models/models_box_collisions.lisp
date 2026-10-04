;;;; raylib [models] example - box collisions
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_box_collisions.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-box-collisions
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-box-collisions)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - box collisions")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0) :target (vec3 0.0 0.0 0.0) :up (vec3 0.0 1.0 0.0) :fovy 45.0 :projection 0))

          (player-position (vec3 0.0 1.0 2.0))
          (player-size (vec3 1.0 2.0 1.0))
          (player-color +green+)

          (enemy-box-pos (vec3 -4.0 1.0 0.0))
          (enemy-box-size (vec3 2.0 2.0 2.0))

          (enemy-sphere-pos (vec3 4.0 0.0 0.0))
          (enemy-sphere-size 1.5)

          (collision nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------

               ;; Move player
               (cond ((is-key-down +key-right+) (incf (vx player-position) 0.2))
                     ((is-key-down +key-left+) (decf (vx player-position) 0.2))
                     ((is-key-down +key-down+) (incf (vz player-position) 0.2))
                     ((is-key-down +key-up+) (decf (vz player-position) 0.2)))

               (setf collision nil)

               (flet ((box (position size)
                        (make-bounding-box :min (vec3 (- (vx position) (/ (vx size) 2))
                                                      (- (vy position) (/ (vy size) 2))
                                                      (- (vz position) (/ (vz size) 2)))
                                           :max (vec3 (+ (vx position) (/ (vx size) 2))
                                                      (+ (vy position) (/ (vy size) 2))
                                                      (+ (vz position) (/ (vz size) 2))))))
                 ;; Check collisions player vs enemy-box
                 (when (check-collision-boxes (box player-position player-size) (box enemy-box-pos enemy-box-size)) (setf collision t))

                 ;; Check collisions player vs enemy-sphere
                 (when (check-collision-box-sphere (box player-position player-size) enemy-sphere-pos enemy-sphere-size) (setf collision t)))

               (setf player-color (if collision +red+ +green+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               ;; Draw enemy-box
               (draw-cube enemy-box-pos (vx enemy-box-size) (vy enemy-box-size) (vz enemy-box-size) +gray+)
               (draw-cube-wires enemy-box-pos (vx enemy-box-size) (vy enemy-box-size) (vz enemy-box-size) +darkgray+)

               ;; Draw enemy-sphere
               (draw-sphere enemy-sphere-pos enemy-sphere-size +gray+)
               (draw-sphere-wires enemy-sphere-pos enemy-sphere-size 16 16 +darkgray+)

               ;; Draw player
               (draw-cube-v player-position player-size player-color)

               (draw-grid 10 1.0)        ; Draw a grid

               (end-mode-3d)

               (draw-text "Move player with arrow keys to collide" 220 40 20 +gray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
