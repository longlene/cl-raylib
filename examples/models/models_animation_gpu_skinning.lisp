;;;; raylib [models] example - animation gpu skinning
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Daniel Holden (@orangeduck) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; WARNING: GPU skinning must be enabled in raylib with a compilation flag,
;;;; if not enabled, CPU skinning will be used instead
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 Daniel Holden (@orangeduck)
;;;; Common Lisp port of raylib/examples/models/models_animation_gpu_skinning.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-animation-gpu-skinning
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-animation-gpu-skinning)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - animation gpu skinning")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 5.0 5.0 5.0) ; Camera position
                                 :target (vec3 0.0 1.0 0.0)   ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                 :fovy 45.0                   ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load gltf model
          (model (load-model "resources/models/gltf/greenman.glb")) ; Load character model
          (position (vec3 0.0 0.0 0.0)))                           ; Set model position

      ;; NOTE: C loads the skinning shader under #if SUPPORT_GPU_SKINNING, disabled at raylib compile
      ;; time by default, like in this port (models.lisp). With GPU skinning enabled it would be:
      ;; (setf (material-shader (aref (model-materials model) 1))
      ;;       (load-shader (text-format "resources/shaders/glsl%i/skinning.vs" +glsl-version+)
      ;;                    (text-format "resources/shaders/glsl%i/skinning.fs" +glsl-version+)))

      ;; Load gltf model animations
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/gltf/greenman.glb")

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
          (unload-model-animations anims anim-count) ; Unload model animation
          (unload-model model)          ; Unload model and meshes/material

          (close-window))))))           ; Close window and OpenGL context

(main)
