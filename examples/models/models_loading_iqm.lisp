;;;; raylib [models] example - loading iqm
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.5
;;;;
;;;; Example contributed by Culacant (@culacant) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; NOTES: To export an IQM model from blender, make sure it is not posed, the vertices need
;;;; to be in the same position as they would be in edit mode and the scale of the models is
;;;; set to 0; scaling can be set from the export menu
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Culacant (@culacant) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_loading_iqm.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-loading-iqm
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-loading-iqm)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - loading iqm")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0) ; Camera position
                                 :target (vec3 0.0 4.0 0.0)      ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                 :fovy 45.0                      ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera mode type

          (model (load-model "resources/models/iqm/guy.iqm"))            ; Load the animated model mesh and basic data
          (texture (load-texture "resources/models/iqm/guytex.png"))     ; Load model texture and set material
          (position (vec3 0.0 0.0 0.0)))                                 ; Set model position

      (set-material-texture (aref (model-materials model) 0) +material-map-diffuse+ texture) ; Set model material map texture

      ;; Load animation data
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/iqm/guyanim.iqm")

        ;; Animation playing variables
        (let ((anim-index 0)            ; Current animation playing
              (anim-current-frame 0.0)  ; Current animation frame (supporting interpolated frames)
              (anim-speed 1.0))         ; How fast the animation plays

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (update-camera camera +camera-orbital+)

                   (when (is-key-pressed +key-right+)
                     (setf anim-index (if (= anim-index (1- anim-count)) 0 (1+ anim-index))
                           anim-current-frame 0.0))
                   (when (is-key-pressed +key-left+)
                     (setf anim-index (if (= anim-index 0) (1- anim-count) (1- anim-index))
                           anim-current-frame 0.0))
                   (when (is-key-pressed +key-up+) (setf anim-speed (min 5.0 (+ anim-speed 0.1))))
                   (when (is-key-pressed +key-down+) (setf anim-speed (max 0.0 (- anim-speed 0.1))))

                   (incf anim-current-frame anim-speed)
                   (update-model-animation model (aref anims anim-index) anim-current-frame)
                   (when (>= anim-current-frame (model-animation-keyframe-count (aref anims anim-index))) (setf anim-current-frame 0.0))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   (draw-model-ex model position (vec3 1.0 0.0 0.0) -90.0 (vec3 1.0 1.0 1.0) +white+)

                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   (draw-text (text-format "Current animation: %s" (model-animation-name (aref anims anim-index))) 10 10 20 +maroon+)
                   (draw-text (text-format "Animation speed: %.2f" anim-speed) 10 40 20 +maroon+)
                   (draw-text "Use left and right arrow keys to change current animation" 10 (- screen-height 34) 10 +black+)
                   (draw-text "Use up and down arrow keys to change animation speed" 10 (- screen-height 20) 10 +black+)
                   (draw-text "(c) Guy IQM 3D model by @culacant" (- screen-width 200) (- screen-height 20) 10 +gray+)

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-texture texture)      ; Unload texture
          (unload-model-animations anims anim-count) ; Unload model animations data
          (unload-model model)          ; Unload model

          (close-window))))))           ; Close window and OpenGL context

(main)
