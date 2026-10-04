;;;; raylib [models] example - billboard rendering
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_billboard_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-billboard-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-billboard-rendering)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - billboard rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 5.0 4.0 5.0)  ; Camera position
                                  :target (vec3 0.0 2.0 0.0)    ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                  :fovy 45.0                    ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (bill (load-texture "resources/billboard.png")) ; Our billboard texture
           (bill-position-static (vec3 0.0 2.0 0.0))       ; Position of static billboard
           (bill-position-rotating (vec3 1.0 2.0 1.0))     ; Position of rotating billboard

           ;; Entire billboard texture, source is used to take a segment from a larger texture
           (source (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width bill)) :height (float (texture-height bill))))

           ;; NOTE: Billboard locked on axis-Y
           (bill-up (vec3 0.0 1.0 0.0))

           ;; Set the height of the rotating billboard to 1.0 with the aspect ratio fixed
           (size (vec2 (/ (rectangle-width source) (rectangle-height source)) 1.0))

           ;; Rotate around origin
           ;; Here we choose to rotate around the image center
           (origin (vector2-scale size 0.5))

           ;; Distance is needed for the correct billboard draw order
           ;; Larger distance (further away from the camera) should be drawn prior to smaller distance
           (distance-static 0.0)
           (distance-rotating 0.0)
           (rotation 0.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)

               (incf rotation 0.4)
               (setf distance-static (vector3-distance (camera3d-position camera) bill-position-static))
               (setf distance-rotating (vector3-distance (camera3d-position camera) bill-position-rotating))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-grid 10 1.0)        ; Draw a grid

               ;; Draw order matters!
               (if (> distance-static distance-rotating)
                   (progn
                     (draw-billboard camera bill bill-position-static 2.0 +white+)
                     (draw-billboard-pro camera bill source bill-position-rotating bill-up size origin rotation +white+))
                   (progn
                     (draw-billboard-pro camera bill source bill-position-rotating bill-up size origin rotation +white+)
                     (draw-billboard camera bill bill-position-static 2.0 +white+)))

               (end-mode-3d)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture bill)             ; Unload texture

      (close-window))))                 ; Close window and OpenGL context

(main)
