;;;; raylib [models] example - loading gltf
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; LIMITATIONS:
;;;;   - Only supports 1 armature per file, and skips loading it if there are multiple armatures
;;;;   - Only supports linear interpolation (default method in Blender when checked
;;;;     "Always Sample Animations" when exporting a GLTF file)
;;;;   - Only supports translation/rotation/scale animation channel.path,
;;;;     weights not considered (i.e. morph targets)
;;;;
;;;; Example originally created with raylib 3.7, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_loading_gltf.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-loading-gltf
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-loading-gltf)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - loading gltf")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 6.0 6.0 6.0)  ; Camera position
                                 :target (vec3 0.0 2.0 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 45.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load model
          (model (load-model "resources/models/gltf/robot.glb"))
          (position (vec3 0.0 0.0 0.0))) ; Set model world position

      ;; Load model animations
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/gltf/robot.glb")

        ;; Animation playing variables
        (let ((anim-index 0)            ; Current animation playing
              (anim-current-frame 0))   ; Current animation frame

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (update-camera camera +camera-orbital+)

                   ;; Select current animation
                   (cond ((is-key-pressed +key-right+) (setf anim-index (mod (1+ anim-index) anim-count)))
                         ((is-key-pressed +key-left+) (setf anim-index (mod (+ anim-index anim-count -1) anim-count))))

                   ;; Update model animation
                   (setf anim-current-frame (mod (1+ anim-current-frame) (model-animation-keyframe-count (aref anims anim-index))))
                   (update-model-animation model (aref anims anim-index) (float anim-current-frame))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   (draw-model model position 1.0 +white+)
                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   (draw-text (text-format "Current animation: %s" (model-animation-name (aref anims anim-index))) 10 40 20 +maroon+)
                   (draw-text "Use the LEFT/RIGHT keys to switch animation" 10 10 20 +gray+)

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-model-animations anims anim-count) ; Unload model animations data
          (unload-model model)          ; Unload model

          (close-window))))))           ; Close window and OpenGL context

(main)
