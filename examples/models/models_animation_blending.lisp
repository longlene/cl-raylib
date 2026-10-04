;;;; raylib [models] example - animation blending
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Kirandeep (@Kirandeep-Singh-Khehra) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; WARNING: GPU skinning must be enabled in raylib with a compilation flag,
;;;; if not enabled, CPU skinning will be used instead
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2026 Kirandeep (@Kirandeep-Singh-Khehra) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/models/models_animation_blending.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-animation-blending
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/models-animation-blending)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - animation blending")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 6.0 6.0 6.0) ; Camera position
                                 :target (vec3 0.0 2.0 0.0)   ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                 :fovy 45.0                   ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load model
          (model (load-model "resources/models/gltf/robot.glb")) ; Load character model
          (position (vec3 0.0 0.0 0.0)))                        ; Set model world position

      ;; NOTE: C loads the skinning shader under #if SUPPORT_GPU_SKINNING, disabled at raylib compile
      ;; time by default, like in this port (models.lisp). With GPU skinning enabled it would be:
      ;; (let ((skinning-shader (load-shader (text-format "resources/shaders/glsl%i/skinning.vs" +glsl-version+)
      ;;                                     (text-format "resources/shaders/glsl%i/skinning.fs" +glsl-version+))))
      ;;   (dotimes (i (model-material-count model))
      ;;     (setf (material-shader (aref (model-materials model) i)) skinning-shader)))

      ;; Load model animations
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/gltf/robot.glb")

        ;; Animation playing variables
        ;; NOTE: Two animations are played with a smooth transition between them
        (let ((current-anim-playing 0)  ; Current animation playing (0 o 1)
              (next-anim-to-play 1)     ; Next animation to play (to transition)
              (anim-transition nil)     ; Flag to register anim transition state

              (anim-index0 10)          ; Current animation playing (walking)
              (anim-current-frame0 0.0) ; Current animation frame (supporting interpolated frames)
              (anim-frame-speed0 0.5)   ; Current animation play speed
              (anim-index1 6)           ; Next animation to play (running)
              (anim-current-frame1 0.0) ; Next animation frame (supporting interpolated frames)
              (anim-frame-speed1 0.5)   ; Next animation play speed

              (anim-blend-factor 0.0)   ; Blend factor from anim0[frame0] --> anim1[frame1], [0.0f..1.0f]
                                        ; NOTE: 0.0f results in full anim0[] and 1.0f in full anim1[]

              (anim-blend-time 2.0)     ; Time to blend from one playing animation to another (in seconds)
              (anim-blend-time-counter 0.0) ; Time counter (delta time)

              (anim-pause nil)          ; Pause animation

              ;; UI required variables
              (anim-names (map 'vector #'model-animation-name anims)) ; Animation names for dropdown box

              (dropdown-edit-mode0 nil)
              (dropdown-edit-mode1 nil)
              (anim-frame-progress0 0.0)
              (anim-frame-progress1 0.0)
              (anim-blend-progress 0.0))

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (update-camera camera +camera-orbital+)

                   (when (is-key-pressed +key-p+) (setf anim-pause (not anim-pause)))

                   (unless anim-pause
                     ;; Start transition from anim0[] to anim1[]
                     (when (and (is-key-pressed +key-space+) (not anim-transition))
                       (if (= current-anim-playing 0)
                           ;; Transition anim0 --> anim1
                           (setf next-anim-to-play 1
                                 anim-current-frame1 0.0)
                           ;; Transition anim1 --> anim0
                           (setf next-anim-to-play 0
                                 anim-current-frame0 0.0))

                       ;; Set animation transition
                       (setf anim-transition t
                             anim-blend-time-counter 0.0
                             anim-blend-factor 0.0))

                     (if anim-transition
                         (progn
                           ;; Playing anim0 and anim1 at the same time
                           (incf anim-current-frame0 anim-frame-speed0)
                           (when (>= anim-current-frame0 (model-animation-keyframe-count (aref anims anim-index0))) (setf anim-current-frame0 0.0))
                           (incf anim-current-frame1 anim-frame-speed1)
                           (when (>= anim-current-frame1 (model-animation-keyframe-count (aref anims anim-index1))) (setf anim-current-frame1 0.0))

                           ;; Increment blend factor over time to transition from anim0 --> anim1 over time
                           ;; NOTE: Time blending could be other than linear, using some easing
                           (setf anim-blend-factor (/ anim-blend-time-counter anim-blend-time))
                           (incf anim-blend-time-counter (get-frame-time))
                           (setf anim-blend-progress anim-blend-factor)

                           ;; Update model with animations blending
                           (if (= next-anim-to-play 1)
                               ;; Blend anim0 --> anim1
                               (update-model-animation-ex model (aref anims anim-index0) anim-current-frame0
                                                          (aref anims anim-index1) anim-current-frame1 anim-blend-factor)
                               ;; Blend anim1 --> anim0
                               (update-model-animation-ex model (aref anims anim-index1) anim-current-frame1
                                                          (aref anims anim-index0) anim-current-frame0 anim-blend-factor))

                           ;; Check if transition completed
                           (when (> anim-blend-factor 1.0)
                             ;; Reset frame states
                             (cond ((= current-anim-playing 0) (setf anim-current-frame0 0.0))
                                   ((= current-anim-playing 1) (setf anim-current-frame1 0.0)))
                             (setf current-anim-playing next-anim-to-play ; Update current animation playing
                                   anim-blend-factor 0.0     ; Reset blend factor
                                   anim-transition nil       ; Exit transition mode
                                   anim-blend-time-counter 0.0)))
                         ;; Play only one anim, the current one
                         (cond ((= current-anim-playing 0)
                                ;; Playing anim0 at defined speed
                                (incf anim-current-frame0 anim-frame-speed0)
                                (when (>= anim-current-frame0 (model-animation-keyframe-count (aref anims anim-index0))) (setf anim-current-frame0 0.0))
                                (update-model-animation model (aref anims anim-index0) anim-current-frame0))
                               ;;(update-model-animation-ex model (aref anims anim-index0) anim-current-frame0
                               ;;                           (aref anims anim-index1) anim-current-frame1 0.0) ; Same as above, first animation frame blend
                               ((= current-anim-playing 1)
                                ;; Playing anim1 at defined speed
                                (incf anim-current-frame1 anim-frame-speed1)
                                (when (>= anim-current-frame1 (model-animation-keyframe-count (aref anims anim-index1))) (setf anim-current-frame1 0.0))
                                (update-model-animation model (aref anims anim-index1) anim-current-frame1)))))
                   ;;(update-model-animation-ex model (aref anims anim-index0) anim-current-frame0
                   ;;                           (aref anims anim-index1) anim-current-frame1 1.0) ; Same as above, second animation frame blend

                   ;; Update progress bars values with current frame for each animation
                   (setf anim-frame-progress0 anim-current-frame0
                         anim-frame-progress1 anim-current-frame1)
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   (draw-model model position 1.0 +white+) ; Draw animated model

                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   (when anim-transition (draw-text "ANIM TRANSITION BLENDING!" 170 50 30 +blue+))

                   ;; Draw UI elements
                   ;;---------------------------------------------------------------------------------------------
                   (when dropdown-edit-mode0 (gui-disable))
                   (setf anim-frame-speed0 (nth-value 1 (gui-slider (make-rectangle :x 10.0 :y 38.0 :width 160.0 :height 12.0)
                                                                    nil (text-format "x%.1f" anim-frame-speed0) anim-frame-speed0 0.1 2.0)))
                   (gui-enable)
                   (when dropdown-edit-mode1 (gui-disable))
                   (setf anim-frame-speed1 (nth-value 1 (gui-slider (make-rectangle :x (- (get-screen-width) 170.0) :y 38.0 :width 160.0 :height 12.0)
                                                                    (text-format "%.1fx" anim-frame-speed1) nil anim-frame-speed1 0.1 2.0)))
                   (gui-enable)

                   ;; Draw animation selectors for blending transition
                   ;; NOTE: Transition does not start until requested
                   (gui-set-style +dropdownbox+ +dropdown-items-spacing+ 1)
                   (multiple-value-bind (result active)
                       (gui-dropdown-box (make-rectangle :x 10.0 :y 10.0 :width 160.0 :height 24.0) (text-join anim-names anim-count ";")
                                         anim-index0 dropdown-edit-mode0)
                     (setf anim-index0 active)
                     (when (/= result 0) (setf dropdown-edit-mode0 (not dropdown-edit-mode0))))

                   ;; Blending process progress bar
                   (if (= next-anim-to-play 1)
                       (gui-set-style +progressbar+ +progress-side+ 0) ; Left-->Right
                       (gui-set-style +progressbar+ +progress-side+ 1)) ; Right-->Left
                   (setf anim-blend-progress (nth-value 1 (gui-progress-bar (make-rectangle :x 180.0 :y 14.0 :width 440.0 :height 16.0) nil nil anim-blend-progress 0.0 1.0)))
                   (gui-set-style +progressbar+ +progress-side+ 0) ; Reset to Left-->Right

                   (multiple-value-bind (result active)
                       (gui-dropdown-box (make-rectangle :x (- (get-screen-width) 170.0) :y 10.0 :width 160.0 :height 24.0) (text-join anim-names anim-count ";")
                                         anim-index1 dropdown-edit-mode1)
                     (setf anim-index1 active)
                     (when (/= result 0) (setf dropdown-edit-mode1 (not dropdown-edit-mode1))))

                   (gui-set-style +label+ +text-alignment+ +text-align-center+)
                   (gui-set-style +default+ +text-size+ (* (font-base-size (gui-get-font)) 2))
                   (gui-label (make-rectangle :x 0.0 :y (- (get-screen-height) 100.0) :width (float (get-screen-width)) :height 40.0) "PRESS SPACE to START BLENDING")
                   (gui-set-style +default+ +text-size+ (font-base-size (gui-get-font)))
                   (gui-set-style +label+ +text-alignment+ +text-align-left+)

                   ;; Draw playing timeline with keyframes for anim0[]
                   (let ((keyframe-count (model-animation-keyframe-count (aref anims anim-index0))))
                     (setf anim-frame-progress0 (nth-value 1 (gui-progress-bar (make-rectangle :x 60.0 :y (- (get-screen-height) 60.0) :width (- (get-screen-width) 180.0) :height 20.0) "ANIM 0"
                                                                               (text-format "FRAME: %.2f / %i" anim-frame-progress0 keyframe-count)
                                                                               anim-frame-progress0 0.0 (float keyframe-count))))
                     (dotimes (i keyframe-count)
                       (draw-rectangle (+ 60 (truncate (* (/ (float (- (get-screen-width) 180)) (float keyframe-count)) (float i))))
                                       (- (get-screen-height) 60) 1 20 +blue+)))

                   ;; Draw playing timeline with keyframes for anim1[]
                   (let ((keyframe-count (model-animation-keyframe-count (aref anims anim-index1))))
                     (setf anim-frame-progress1 (nth-value 1 (gui-progress-bar (make-rectangle :x 60.0 :y (- (get-screen-height) 30.0) :width (- (get-screen-width) 180.0) :height 20.0) "ANIM 1"
                                                                               (text-format "FRAME: %.2f / %i" anim-frame-progress1 keyframe-count)
                                                                               anim-frame-progress1 0.0 (float keyframe-count))))
                     (dotimes (i keyframe-count)
                       (draw-rectangle (+ 60 (truncate (* (/ (float (- (get-screen-width) 180)) (float keyframe-count)) (float i))))
                                       (- (get-screen-height) 30) 1 20 +blue+)))
                   ;;---------------------------------------------------------------------------------------------

                   (end-drawing))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-model-animations anims anim-count) ; Unload model animation
          (unload-model model)          ; Unload model and meshes/material

          (close-window))))))           ; Close window and OpenGL context

(main)
