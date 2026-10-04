;;;; raylib [models] example - directional billboard
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
;;;; Killbot art by patvanmackelberg https://opengameart.org/content/killbot-8-directional under CC0
;;;; Common Lisp port of raylib/examples/models/models_directional_billboard.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-directional-billboard
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-directional-billboard)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - directional billboard")

    ;; Set up the camera
    (let ((camera (make-camera3d :position (vec3 2.0 1.0 2.0) ; Starting position
                                 :target (vec3 0.0 0.5 0.0)   ; Target position
                                 :up (vec3 0.0 1.0 0.0)       ; Up vector
                                 :fovy 45.0                   ; FOV
                                 :projection +camera-perspective+)) ; Projection type (Standard 3D perspective)

          ;; Load billboard texture
          (skillbot (load-texture "resources/skillbot.png"))

          ;; Timer to update animation
          (anim-timer 0.0)

          ;; Animation frame
          (anim 0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               ;; Update timer with delta time
               (incf anim-timer (get-frame-time))

               ;; Update frame index after a certain amount of time (half a second)
               (when (> anim-timer 0.5)
                 (setf anim-timer 0.0)
                 (incf anim 1))

               ;; Reset frame index to zero on overflow
               (when (>= anim 4) (setf anim 0))

               ;; Find the current direction frame based on the camera position to the billboard object
               (let ((dir (ffloor (+ (* (/ (vector2-angle (vec2 2.0 0.0) (vec2 (vx (camera3d-position camera)) (vz (camera3d-position camera)))) +pi+) 4.0) 0.25))))

                 ;; Correct frame index if angle is negative
                 (when (< dir 0.0)
                   (setf dir (- 8.0 (float (abs (truncate dir))))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)
                 (clear-background +raywhite+)

                 (begin-mode-3d camera)

                 (draw-grid 10 1.0)

                 ;; Draw billboard pointing straight up to the sky, rotated relative to the camera and offset from the bottom
                 (draw-billboard-pro camera skillbot (make-rectangle :x (+ 0.0 (* anim 24.0)) :y (+ 0.0 (* dir 24.0)) :width 24.0 :height 24.0)
                                     (vector3-zero) (vec3 0.0 1.0 0.0) (vector2-one) (vec2 0.5 0.0) 0 +white+)

                 (end-mode-3d)

                 ;; Render various variables for reference
                 (draw-text (text-format "animation: %d" anim) 10 10 20 +darkgray+)
                 (draw-text (text-format "direction frame: %.0f" dir) 10 40 20 +darkgray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; Unload billboard texture
      (unload-texture skillbot)

      (close-window))))                 ; Close window and OpenGL context

(main)
