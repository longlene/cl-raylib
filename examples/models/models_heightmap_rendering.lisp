;;;; raylib [models] example - heightmap rendering
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_heightmap_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-heightmap-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-heightmap-rendering)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - heightmap rendering")

    ;; Define our custom camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 18.0 21.0 18.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 45.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (image (load-image "resources/heightmap.png")) ; Load heightmap image (RAM)
           (texture (load-texture-from-image image))      ; Convert image to texture (VRAM)

           (mesh (gen-mesh-heightmap image (vec3 16.0 8.0 16.0))) ; Generate heightmap mesh (RAM and VRAM)
           (model (load-model-from-mesh mesh))                    ; Load model from generated mesh

           (map-position (vec3 -8.0 0.0 -8.0))) ; Define model position

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set map diffuse texture

      (unload-image image)              ; Unload heightmap image from RAM, already uploaded to VRAM

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-orbital+)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-model model map-position 1.0 +red+)

               (draw-grid 20 1.0)

               (end-mode-3d)

               (draw-texture texture (- screen-width (texture-width texture) 20) 20 +white+)
               (draw-rectangle-lines (- screen-width (texture-width texture) 20) 20 (texture-width texture) (texture-height texture) +green+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture texture)          ; Unload texture
      (unload-model model)              ; Unload model

      (close-window))))                 ; Close window and OpenGL context

(main)
