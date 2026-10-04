;;;; raylib [models] example - rlgl solar system
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: This example uses [rlgl] module functionality (pseudo-OpenGL 1.1 style coding)
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_rlgl_solar_system.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-rlgl-solar-system
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-rlgl-solar-system)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Draw sphere without any matrix transformation
;; NOTE: Sphere is drawn in world position ( 0, 0, 0 ) with radius 1.0f
(defun draw-sphere-basic (color)
  (let ((rings 16)
        (slices 16))

    ;; Make sure there is enough space in the internal render batch
    ;; buffer to store all required vertex, batch is reseted if required
    (rl-check-render-batch-limit (* (+ rings 2) slices 6))

    (flet ((ring-angle (i) (* +deg2rad+ (+ 270 (* (/ 180.0 (1+ rings)) i))))
           (slice-angle (j) (* +deg2rad+ (/ (* j 360.0) slices))))
      (flet ((vertex (i j)
               (rl-vertex3f (* (cos (ring-angle i)) (sin (slice-angle j)))
                            (sin (ring-angle i))
                            (* (cos (ring-angle i)) (cos (slice-angle j))))))
        (rl-begin +rl-triangles+)
        (rl-color4ub (first color) (second color) (third color) (fourth color))

        (dotimes (i (+ rings 2))
          (dotimes (j slices)
            (vertex i j)
            (vertex (1+ i) (1+ j))
            (vertex (1+ i) j)

            (vertex i j)
            (vertex i (1+ j))
            (vertex (1+ i) (1+ j))))
        (rl-end)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450)

        (sun-radius 4.0)
        (earth-radius 0.6)
        (earth-orbit-radius 8.0)
        (moon-radius 0.16)
        (moon-orbit-radius 1.5))

    (init-window screen-width screen-height "raylib [models] example - rlgl solar system")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 16.0 16.0 16.0) ; Camera position
                                 :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          (rotation-speed 0.2)          ; General system rotation speed

          (earth-rotation 0.0)          ; Rotation of earth around itself (days) in degrees
          (earth-orbit-rotation 0.0)    ; Rotation of earth around the Sun (years) in degrees
          (moon-rotation 0.0)           ; Rotation of moon around itself
          (moon-orbit-rotation 0.0))    ; Rotation of moon around earth in degrees

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf earth-rotation (* 5.0 rotation-speed))
               (incf earth-orbit-rotation (* (/ 365 360.0) (* 5.0 rotation-speed) rotation-speed))
               (incf moon-rotation (* 2.0 rotation-speed))
               (incf moon-orbit-rotation (* 8.0 rotation-speed))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (rl-push-matrix)
               (rl-scalef sun-radius sun-radius sun-radius) ; Scale Sun
               (draw-sphere-basic +gold+)                   ; Draw the Sun
               (rl-pop-matrix)

               (rl-push-matrix)
               (rl-rotatef earth-orbit-rotation 0.0 1.0 0.0) ; Rotation for Earth orbit around Sun
               (rl-translatef earth-orbit-radius 0.0 0.0)    ; Translation for Earth orbit

               (rl-push-matrix)
               (rl-rotatef earth-rotation 0.25 1.0 0.0)      ; Rotation for Earth itself
               (rl-scalef earth-radius earth-radius earth-radius) ; Scale Earth

               (draw-sphere-basic +blue+)                    ; Draw the Earth
               (rl-pop-matrix)

               (rl-rotatef moon-orbit-rotation 0.0 1.0 0.0)  ; Rotation for Moon orbit around Earth
               (rl-translatef moon-orbit-radius 0.0 0.0)     ; Translation for Moon orbit
               (rl-rotatef moon-rotation 0.0 1.0 0.0)        ; Rotation for Moon itself
               (rl-scalef moon-radius moon-radius moon-radius) ; Scale Moon

               (draw-sphere-basic +lightgray+)               ; Draw the Moon
               (rl-pop-matrix)

               ;; Some reference elements (not affected by previous matrix transformations)
               (draw-circle-3d (vec3 0.0 0.0 0.0) earth-orbit-radius (vec3 1.0 0.0 0.0) 90.0 (fade +red+ 0.5))
               (draw-grid 20 1.0)

               (end-mode-3d)

               (draw-text "EARTH ORBITING AROUND THE SUN!" 400 10 20 +maroon+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
