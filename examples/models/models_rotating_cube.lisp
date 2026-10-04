;;;; raylib [models] example - rotating cube
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Jopestpe (@jopestpe)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jopestpe (@jopestpe)
;;;; Common Lisp port of raylib/examples/models/models_rotating_cube.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-rotating-cube
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-rotating-cube)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - rotating cube")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 0.0 3.0 3.0)
                                  :target (vec3 0.0 0.0 0.0)
                                  :up (vec3 0.0 1.0 0.0)
                                  :fovy 45.0
                                  :projection +camera-perspective+))

           ;; Load image to create texture for the cube
           (model (load-model-from-mesh (gen-mesh-cube 1.0 1.0 1.0)))
           (img (load-image "resources/cubicmap_atlas.png"))
           (crop (image-from-image img (make-rectangle :x 0.0 :y (/ (image-height img) 2.0) :width (/ (image-width img) 2.0) :height (/ (image-height img) 2.0))))
           (texture (load-texture-from-image crop))

           (rotation 0.0))

      (unload-image img)
      (unload-image crop)

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf rotation 1.0)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               ;; Draw model defining: position, size, rotation-axis, rotation (degrees), size, and tint-color
               (draw-model-ex model (vec3 0.0 0.0 0.0) (vec3 0.5 1.0 0.0)
                              rotation (vec3 1.0 1.0 1.0) +white+)

               (draw-grid 10 1.0)

               (end-mode-3d)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model

      (close-window))))                 ; Close window and OpenGL context

(main)
