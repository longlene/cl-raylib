;;;; raylib [models] example - cubicmap rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.8, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_cubicmap_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-cubicmap-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-cubicmap-rendering)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - cubicmap rendering")

    ;; Define the camera to look into our 3d world
    (let* ((camera (make-camera3d :position (vec3 16.0 14.0 16.0) ; Camera position
                                  :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                  :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                  :fovy 45.0                      ; Camera field-of-view Y
                                  :projection +camera-perspective+)) ; Camera projection type

           (image (load-image "resources/cubicmap.png"))  ; Load cubicmap image (RAM)
           (cubicmap (load-texture-from-image image))     ; Convert image to texture to display (VRAM)

           (mesh (gen-mesh-cubicmap image (vec3 1.0 1.0 1.0)))
           (model (load-model-from-mesh mesh))

           ;; NOTE: By default each cube is mapped to one part of texture atlas
           (texture (load-texture "resources/cubicmap_atlas.png")) ; Load map texture

           (map-position (vec3 -16.0 0.0 -8.0)) ; Set model position

           (pause nil))                 ; Pause camera orbital rotation (and zoom)

      (setf (material-map-texture (aref (material-maps (aref (model-materials model) 0)) +material-map-diffuse+)) texture) ; Set map diffuse texture

      (unload-image image)              ; Unload cubesmap image from RAM, already uploaded to VRAM

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-p+) (setf pause (not pause)))

               (unless pause (update-camera camera +camera-orbital+))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-model model map-position 1.0 +white+)

               (end-mode-3d)

               (draw-texture-ex cubicmap (vec2 (- screen-width (* (texture-width cubicmap) 4.0) 20) 20.0) 0.0 4.0 +white+)
               (draw-rectangle-lines (- screen-width (* (texture-width cubicmap) 4) 20) 20 (* (texture-width cubicmap) 4) (* (texture-height cubicmap) 4) +green+)

               (draw-text "cubicmap image used to" 658 90 10 +gray+)
               (draw-text "generate map 3d model" 658 104 10 +gray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture cubicmap)         ; Unload cubicmap texture
      (unload-texture texture)          ; Unload map texture
      (unload-model model)              ; Unload map model

      (close-window))))                 ; Close window and OpenGL context

(main)
