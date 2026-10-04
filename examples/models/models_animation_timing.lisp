;;;; raylib [models] example - animation timing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_animation_timing.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-animation-timing
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/models-animation-timing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - animation timing")

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
        (let ((anim-index 10)           ; Current animation playing
              (anim-current-frame 0.0)  ; Current animation frame (supporting interpolated frames)
              (anim-frame-speed 0.5)    ; Animation play speed
              (anim-pause nil)          ; Pause animation

              ;; UI required variables
              (anim-names (map 'vector #'model-animation-name anims))

              (dropdown-edit-mode nil)
              (anim-frame-progress 0.0))

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (update-camera camera +camera-orbital+)

                   (when (is-key-pressed +key-p+) (setf anim-pause (not anim-pause)))

                   (when (and (not anim-pause) (< anim-index anim-count))
                     ;; Update model animation
                     (incf anim-current-frame anim-frame-speed)
                     (when (>= anim-current-frame (model-animation-keyframe-count (aref anims anim-index))) (setf anim-current-frame 0.0))
                     (update-model-animation model (aref anims anim-index) anim-current-frame))

                   ;; NOTE: Animation and playing speed selected through UI

                   ;; Update progressbar value with current frame
                   (setf anim-frame-progress anim-current-frame)
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   (draw-model model position 1.0 +white+)

                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   ;; Draw UI, select anim and playing speed
                   (gui-set-style +dropdownbox+ +dropdown-items-spacing+ 1)
                   (multiple-value-bind (result active)
                       (gui-dropdown-box (make-rectangle :x 10.0 :y 10.0 :width 140.0 :height 24.0) (text-join anim-names anim-count ";")
                                         anim-index dropdown-edit-mode)
                     (setf anim-index active)
                     (when (/= result 0) (setf dropdown-edit-mode (not dropdown-edit-mode))))

                   (setf anim-frame-speed (nth-value 1 (gui-slider (make-rectangle :x 260.0 :y 10.0 :width 500.0 :height 24.0) "FRAME SPEED: " (text-format "x%.1f" anim-frame-speed)
                                                                   anim-frame-speed 0.1 2.0)))

                   ;; Draw playing timeline with keyframes
                   (let ((keyframe-count (model-animation-keyframe-count (aref anims anim-index))))
                     (gui-label (make-rectangle :x 10.0 :y (- (get-screen-height) 64.0) :width (- (get-screen-width) 20.0) :height 24.0)
                                (text-format "CURRENT FRAME: %.2f / %i" anim-frame-progress keyframe-count))
                     (setf anim-frame-progress (nth-value 1 (gui-progress-bar (make-rectangle :x 10.0 :y (- (get-screen-height) 40.0) :width (- (get-screen-width) 20.0) :height 24.0) nil nil
                                                                              anim-frame-progress 0.0 (float keyframe-count))))
                     (dotimes (i keyframe-count)
                       (draw-rectangle (+ 10 (truncate (* (/ (float (- (get-screen-width) 20)) (float keyframe-count)) (float i))))
                                       (- (get-screen-height) 40) 1 24 +blue+)))

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-model-animations anims anim-count) ; Unload model animation
          (unload-model model)          ; Unload model and meshes/material

          (close-window))))))           ; Close window and OpenGL context

(main)
