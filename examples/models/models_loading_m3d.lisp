;;;; raylib [models] example - loading m3d
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by bzt (@bztsrc) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; NOTES:
;;;;   - Model3D (M3D) fileformat specs: https://gitlab.com/bztsrc/model3d
;;;;   - Bender M3D exported: https://gitlab.com/bztsrc/model3d/-/tree/master/blender
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 bzt (@bztsrc)
;;;; Common Lisp port of raylib/examples/models/models_loading_m3d.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-loading-m3d
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-loading-m3d)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Draw model skeleton
(defun draw-model-skeleton (skeleton pose scale color)
  ;; Loop to (boneCount - 1) because the last one is a special "no bone" bone,
  ;; needed to workaround buggy models without a -1, a cube is always drawn at the origin
  (dotimes (i (1- (model-skeleton-bone-count skeleton)))
    ;; Display the frame-pose skeleton
    (draw-cube (transform-translation (aref pose i)) (* scale 0.05) (* scale 0.05) (* scale 0.05) color)

    (let ((parent (bone-info-parent (aref (model-skeleton-bones skeleton) i))))
      (when (>= parent 0)
        (draw-line-3d (transform-translation (aref pose i)) (transform-translation (aref pose parent)) color)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - loading m3d")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 1.5 1.5 1.5)  ; Camera position
                                 :target (vec3 0.0 0.4 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 45.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load model
          (model (load-model "resources/models/m3d/cesium_man.m3d")) ; Load the animated model mesh and basic data
          (position (vec3 0.0 0.0 0.0)))                            ; Set model position

      ;; Load animation data
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/m3d/cesium_man.m3d")

        ;; Animation playing variables
        (let ((anim-index 0)            ; Current animation playing
              (anim-current-frame 0.0)) ; Current animation frame (supporting interpolated frames)

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
                   (incf anim-current-frame 1.0)
                   (when (>= anim-current-frame (model-animation-keyframe-count (aref anims anim-index))) (setf anim-current-frame 0.0))
                   (update-model-animation model (aref anims anim-index) anim-current-frame)
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   ;; Draw 3d model with texture
                   (if (not (is-key-down +key-space+))
                       (draw-model model position 1.0 +white+)
                       ;; Draw the animated skeleton
                       (draw-model-skeleton (model-skeleton model) (aref (model-animation-keyframe-poses (aref anims anim-index)) (truncate anim-current-frame)) 1.0 +red+))

                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   (draw-text (text-format "Current animation: %s" (model-animation-name (aref anims anim-index))) 10 10 20 +lightgray+)
                   (draw-text "Press SPACE to draw skeleton" 10 40 20 +maroon+)

                   (draw-text "(c) CesiumMan model by KhronosGroup" (- (get-screen-width) 210) (- (get-screen-height) 20) 10 +gray+)

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-model-animations anims anim-count) ; Unload model animations data
          (unload-model model)          ; Unload model

          (close-window))))))           ; Close window and OpenGL context

(main)
