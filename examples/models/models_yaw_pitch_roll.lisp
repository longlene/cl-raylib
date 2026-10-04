;;;; raylib [models] example - yaw pitch roll
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Berni (@Berni8k) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Berni (@Berni8k) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_yaw_pitch_roll.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-yaw-pitch-roll
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-yaw-pitch-roll)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;;(set-config-flags (logior +flag-msaa-4x-hint+ +flag-window-highdpi+))
    (init-window screen-width screen-height "raylib [models] example - yaw pitch roll")

    (let ((camera (make-camera3d :position (vec3 0.0 50.0 -120.0) ; Camera position perspective
                                 :target (vec3 0.0 0.0 0.0)       ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)           ; Camera up vector (rotation towards target)
                                 :fovy 30.0                       ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera type

          (model (load-model "resources/models/obj/plane.obj"))                   ; Load model
          (texture (load-texture "resources/models/obj/plane_diffuse.png"))       ; Load model texture

          (pitch 0.0)
          (roll 0.0)
          (yaw 0.0))

      (set-texture-wrap texture +texture-wrap-repeat+) ; Force Repeat to avoid issue on Web version
      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set map diffuse texture

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Plane pitch (x-axis) controls
               (cond ((is-key-down +key-down+) (incf pitch 0.6))
                     ((is-key-down +key-up+) (decf pitch 0.6))
                     (t (cond ((> pitch 0.3) (decf pitch 0.3))
                              ((< pitch -0.3) (incf pitch 0.3)))))

               ;; Plane yaw (y-axis) controls
               (cond ((is-key-down +key-s+) (decf yaw 1.0))
                     ((is-key-down +key-a+) (incf yaw 1.0))
                     (t (cond ((> yaw 0.0) (decf yaw 0.5))
                              ((< yaw 0.0) (incf yaw 0.5)))))

               ;; Plane roll (z-axis) controls
               (cond ((is-key-down +key-left+) (decf roll 1.0))
                     ((is-key-down +key-right+) (incf roll 1.0))
                     (t (cond ((> roll 0.0) (decf roll 0.5))
                              ((< roll 0.0) (incf roll 0.5)))))

               ;; Tranformation matrix for rotations
               (setf (model-transform model) (matrix-rotate-xyz (vec3 (* +deg2rad+ pitch) (* +deg2rad+ yaw) (* +deg2rad+ roll))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw 3D model (recomended to draw 3D always before 2D)
               (begin-mode-3d camera)

               (draw-model model (vec3 0.0 -8.0 0.0) 1.0 +white+) ; Draw 3d model with texture
               (draw-grid 10 10.0)

               (end-mode-3d)

               ;; Draw controls info
               (draw-rectangle 30 370 260 70 (fade +green+ 0.5))
               (draw-rectangle-lines 30 370 260 70 (fade +darkgreen+ 0.5))
               (draw-text "Pitch controlled with: KEY_UP / KEY_DOWN" 40 380 10 +darkgray+)
               (draw-text "Roll controlled with: KEY_LEFT / KEY_RIGHT" 40 400 10 +darkgray+)
               (draw-text "Yaw controlled with: KEY_A / KEY_S" 40 420 10 +darkgray+)

               (draw-text "(c) WWI Plane Model created by GiaHanLam" (- screen-width 240) (- screen-height 20) 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-model model)              ; Unload model data
      (unload-texture texture)          ; Unload texture data

      (close-window))))                 ; Close window and OpenGL context

(main)
