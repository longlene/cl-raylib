;;;; raylib [models] example - animation blend custom
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by dmitrii-brand (@dmitrii-brand) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; DETAILS: Example demonstrates per-bone animation blending, allowing smooth transitions
;;;; between two animations by interpolating bone transforms. This is useful for:
;;;;  - Blending movement animations (walk/run) with action animations (jump/attack)
;;;;  - Creating smooth animation transitions
;;;;  - Layering animations (e.g., upper body attack while lower body walks)
;;;;
;;;; WARNING: GPU skinning must be enabled in raylib with a compilation flag,
;;;; if not enabled, CPU skinning will be used instead
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 dmitrii-brand (@dmitrii-brand)
;;;; Common Lisp port of raylib/examples/models/models_animation_blend_custom.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-animation-blend-custom
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-animation-blend-custom)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Check if a bone is part of upper body (for selective blending)
(defun is-upper-body-bone (bone-name)
  ;; Common upper body bone names (adjust based on your model)
  (when (member bone-name '("spine" "spine1" "spine2"
                            "chest" "upperChest"
                            "neck" "head"
                            "shoulder" "shoulder_L" "shoulder_R"
                            "upperArm" "upperArm_L" "upperArm_R"
                            "lowerArm" "lowerArm_L" "lowerArm_R"
                            "hand" "hand_L" "hand_R"
                            "clavicle" "clavicle_L" "clavicle_R")
                :test #'text-is-equal)
    (return-from is-upper-body-bone t))

  ;; Check if bone name contains upper body keywords
  (when (some (lambda (keyword) (search keyword bone-name))
              '("spine" "chest" "neck" "head" "shoulder" "arm" "hand" "clavicle"))
    (return-from is-upper-body-bone t))

  nil)

;; Blend two animations per-bone with selective upper/lower body blending
(defun update-model-animation-bones (model anim0 frame0 anim1 frame1 blend upper-body-blend)
  (let ((skeleton (model-skeleton model)))
    ;; Validate inputs
    (when (and (/= (model-animation-bone-count anim0) 0) (model-animation-keyframe-poses anim0)
               (/= (model-animation-bone-count anim1) 0) (model-animation-keyframe-poses anim1)
               (/= (model-skeleton-bone-count skeleton) 0) (model-skeleton-bind-pose skeleton))
      ;; Clamp blend factor to [0, 1]
      (setf blend (min 1.0 (max 0.0 blend)))

      ;; Ensure frame indices are valid
      (when (>= frame0 (model-animation-keyframe-count anim0)) (setf frame0 (1- (model-animation-keyframe-count anim0))))
      (when (>= frame1 (model-animation-keyframe-count anim1)) (setf frame1 (1- (model-animation-keyframe-count anim1))))
      (when (< frame0 0) (setf frame0 0))
      (when (< frame1 0) (setf frame1 0))

      ;; Get bone count (use minimum of all to be safe)
      (let ((bone-count (min (model-skeleton-bone-count skeleton) (model-animation-bone-count anim0) (model-animation-bone-count anim1))))

        ;; Blend each bone
        (dotimes (bone-index bone-count)
          ;; Determine blend factor for this bone
          (let ((bone-blend-factor blend))

            ;; If upper body blending is enabled, use different blend factors for upper vs lower body
            (when upper-body-blend
              (let* ((bone-name (bone-info-name (aref (model-skeleton-bones skeleton) bone-index)))
                     (is-upper-body (is-upper-body-bone bone-name)))

                ;; Upper body: use anim1 (attack), Lower body: use anim0 (walk)
                ;; blend = 0.0 means full anim0 (walk), 1.0 means full anim1 (attack)
                (setf bone-blend-factor (if is-upper-body
                                            blend            ; Upper body: blend towards anim1 (attack)
                                            (- 1.0 blend))))) ; Lower body: blend towards anim0 (walk) - invert the blend

            ;; Get transforms from both animations
            (let* ((bind-transform (aref (model-skeleton-bind-pose skeleton) bone-index))
                   (anim-transform0 (aref (aref (model-animation-keyframe-poses anim0) frame0) bone-index))
                   (anim-transform1 (aref (aref (model-animation-keyframe-poses anim1) frame1) bone-index))

                   ;; Blend the transforms
                   (blended (make-transform
                             :translation (vector3-lerp (transform-translation anim-transform0) (transform-translation anim-transform1) bone-blend-factor)
                             :rotation (quaternion-slerp (transform-rotation anim-transform0) (transform-rotation anim-transform1) bone-blend-factor)
                             :scale (vector3-lerp (transform-scale anim-transform0) (transform-scale anim-transform1) bone-blend-factor)))

                   ;; Convert bind pose to matrix
                   (bind-matrix (matrix-multiply (matrix-multiply
                                                  (matrix-scale (vx (transform-scale bind-transform)) (vy (transform-scale bind-transform)) (vz (transform-scale bind-transform)))
                                                  (quaternion-to-matrix (transform-rotation bind-transform)))
                                                 (matrix-translate (vx (transform-translation bind-transform)) (vy (transform-translation bind-transform)) (vz (transform-translation bind-transform)))))

                   ;; Convert blended transform to matrix
                   (blended-matrix (matrix-multiply (matrix-multiply
                                                     (matrix-scale (vx (transform-scale blended)) (vy (transform-scale blended)) (vz (transform-scale blended)))
                                                     (quaternion-to-matrix (transform-rotation blended)))
                                                    (matrix-translate (vx (transform-translation blended)) (vy (transform-translation blended)) (vz (transform-translation blended))))))

              ;; Calculate final bone matrix (similar to UpdateModelAnimationBones)
              (setf (aref (model-bone-matrices model) bone-index) (matrix-multiply (matrix-invert bind-matrix) blended-matrix))))))

      ;; CPU skinning, updates CPU buffers and uploads them to GPU (if available)
      ;; NOTE: Fallback in case GPU skinning is not supported or enabled
      (dotimes (m (model-mesh-count model))
        (let* ((mesh (aref (model-meshes model) m))
               (vertex-values-count (* (mesh-vertex-count mesh) 3))
               (bone-counter 0)
               (buffer-update-required nil) ; Flag to check when anim vertex information is updated
               (vertices (mesh-vertices mesh))
               (normals (mesh-normals mesh))
               (anim-vertices (mesh-anim-vertices mesh))
               (anim-normals (mesh-anim-normals mesh)))

          ;; Skip if missing bone data or missing anim buffers initialization
          (when (and (mesh-bone-weights mesh) (mesh-bone-indices mesh) anim-vertices anim-normals)
            (loop for v-counter from 0 below vertex-values-count by 3
                  do (setf (aref anim-vertices v-counter) 0.0
                           (aref anim-vertices (+ v-counter 1)) 0.0
                           (aref anim-vertices (+ v-counter 2)) 0.0)

                     (when anim-normals
                       (setf (aref anim-normals v-counter) 0.0
                             (aref anim-normals (+ v-counter 1)) 0.0
                             (aref anim-normals (+ v-counter 2)) 0.0))

                     ;; Iterates over 4 bones per vertex
                     (dotimes (j 4)
                       (let ((bone-weight (aref (mesh-bone-weights mesh) bone-counter))
                             (bone-index (aref (mesh-bone-indices mesh) bone-counter)))

                         ;; Early stop when no transformation will be applied
                         (unless (= bone-weight 0.0)
                           (let ((anim-vertex (vector3-transform (vec3 (aref vertices v-counter) (aref vertices (+ v-counter 1)) (aref vertices (+ v-counter 2)))
                                                                 (aref (model-bone-matrices model) bone-index))))
                             (incf (aref anim-vertices v-counter) (* (vx anim-vertex) bone-weight))
                             (incf (aref anim-vertices (+ v-counter 1)) (* (vy anim-vertex) bone-weight))
                             (incf (aref anim-vertices (+ v-counter 2)) (* (vz anim-vertex) bone-weight)))
                           (setf buffer-update-required t)

                           ;; Normals processing
                           ;; NOTE: We use meshes.baseNormals (default normal) to calculate meshes.normals (animated normals)
                           (when (and normals anim-normals)
                             (let ((anim-normal (vector3-transform (vec3 (aref normals v-counter) (aref normals (+ v-counter 1)) (aref normals (+ v-counter 2)))
                                                                   (matrix-transpose (matrix-invert (aref (model-bone-matrices model) bone-index))))))
                               (incf (aref anim-normals v-counter) (* (vx anim-normal) bone-weight))
                               (incf (aref anim-normals (+ v-counter 1)) (* (vy anim-normal) bone-weight))
                               (incf (aref anim-normals (+ v-counter 2)) (* (vz anim-normal) bone-weight)))))
                         (incf bone-counter))))

            (when buffer-update-required
              ;; Update GPU vertex buffers with updated data (position + normals)
              (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) +shader-loc-vertex-position+) anim-vertices (* (mesh-vertex-count mesh) 3 4) 0)
              (when normals (rl-update-vertex-buffer (aref (mesh-vbo-id mesh) +shader-loc-vertex-normal+) anim-normals (* (mesh-vertex-count mesh) 3 4) 0)))))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - animation blend custom")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 4.0 4.0 4.0) ; Camera position
                                 :target (vec3 0.0 1.0 0.0)   ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                 :fovy 45.0                   ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load gltf model
          (model (load-model "resources/models/gltf/greenman.glb"))
          (position (vec3 0.0 0.0 0.0)) ; Set model position

          ;; Load skinning shader
          ;; WARNING: GPU skinning must be enabled in raylib with a compilation flag,
          ;; if not enabled, CPU skinning will be used instead
          (skinning-shader (load-shader (text-format "resources/shaders/glsl%i/skinning.vs" +glsl-version+)
                                        (text-format "resources/shaders/glsl%i/skinning.fs" +glsl-version+))))

      (setf (material-shader (aref (model-materials model) 1)) skinning-shader)

      ;; Load gltf model animations
      (multiple-value-bind (anims anim-count) (load-model-animations "resources/models/gltf/greenman.glb")

        ;; Use specific animation indices: 2-walk/move, 3-attack
        (let ((anim-index0 2)           ; Walk/Move animation (index 2)
              (anim-index1 3)           ; Attack animation (index 3)
              (anim-current-frame0 0)
              (anim-current-frame1 0)

              (upper-body-blend t))     ; Toggle: true = upper/lower body blending, false = uniform blending (50/50)

          ;; Validate indices
          (when (>= anim-index0 anim-count) (setf anim-index0 0))
          (when (>= anim-index1 anim-count) (setf anim-index1 (if (> anim-count 1) 1 0)))

          (set-target-fps 60)           ; Set our game to run at 60 frames-per-second
          ;;--------------------------------------------------------------------------------------

          ;; Main game loop
          (loop until (window-should-close) ; Detect window close button or ESC key
                do ;; Update
                   ;;----------------------------------------------------------------------------------
                   (update-camera camera +camera-orbital+)

                   ;; Toggle upper/lower body blending mode (SPACE key)
                   (when (is-key-pressed +key-space+) (setf upper-body-blend (not upper-body-blend)))

                   ;; Update animation frames
                   (let* ((anim0 (aref anims anim-index0))
                          (anim1 (aref anims anim-index1))
                          ;; Blend the two animations
                          ;; When upperBodyBlend is ON: upper body = attack (1.0), lower body = walk (0.0)
                          ;; When upperBodyBlend is OFF: uniform blend at 0.5 (50% walk, 50% attack)
                          (blend-factor (if upper-body-blend 1.0 0.5)))
                     (setf anim-current-frame0 (mod (1+ anim-current-frame0) (model-animation-keyframe-count anim0))
                           anim-current-frame1 (mod (1+ anim-current-frame1) (model-animation-keyframe-count anim1)))

                     (update-model-animation-bones model anim0 anim-current-frame0
                                                   anim1 anim-current-frame1 blend-factor upper-body-blend)

                     ;; raylib provided animation blending function
                     ;;(update-model-animation-ex model anim0 (float anim-current-frame0)
                     ;;                           anim1 (float anim-current-frame1) blend-factor)
                     ;;----------------------------------------------------------------------------------

                     ;; Draw
                     ;;----------------------------------------------------------------------------------
                     (begin-drawing)

                     (clear-background +raywhite+)

                     (begin-mode-3d camera)

                     (draw-model model position 1.0 +white+)

                     (draw-grid 10 1.0)

                     (end-mode-3d)

                     ;; Draw UI
                     (draw-text (text-format "ANIM 0: %s" (model-animation-name anim0)) 10 10 20 +gray+)
                     (draw-text (text-format "ANIM 1: %s" (model-animation-name anim1)) 10 40 20 +gray+)
                     (draw-text (text-format "[SPACE] Toggle blending mode: %s"
                                             (if upper-body-blend "Upper/Lower Body Blending" "Uniform Blending"))
                                10 (- (get-screen-height) 30) 20 +darkgray+)

                     (end-drawing)))
          ;;----------------------------------------------------------------------------------

          ;; De-Initialization
          ;;--------------------------------------------------------------------------------------
          (unload-model-animations anims anim-count) ; Unload model animation
          (unload-model model)          ; Unload model and meshes/material
          (unload-shader skinning-shader) ; Unload GPU skinning shader

          (close-window))))))           ; Close window and OpenGL context

(main)
