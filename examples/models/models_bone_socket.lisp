;;;; raylib [models] example - bone socket
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by iP (@ipzaur) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2024-2025 iP (@ipzaur)
;;;; Common Lisp port of raylib/examples/models/models_bone_socket.c

(require :cl-raylib)

(defpackage #:raylib-examples/models-bone-socket
  (:use #:cl #:raylib))
(in-package #:raylib-examples/models-bone-socket)

(defconstant +bone-sockets+ 3)
(defconstant +bone-socket-hat+ 0)
(defconstant +bone-socket-hand-r+ 1)
(defconstant +bone-socket-hand-l+ 2)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [models] example - bone socket")

    ;; Define the camera to look into our 3d world
    (let ((camera (make-camera3d :position (vec3 5.0 5.0 5.0) ; Camera position
                                 :target (vec3 0.0 2.0 0.0)   ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)       ; Camera up vector (rotation towards target)
                                 :fovy 45.0                   ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type

          ;; Load gltf model
          (character-model (load-model "resources/models/gltf/greenman.glb")) ; Load character model
          (equip-model (vector (load-model "resources/models/gltf/greenman_hat.glb")      ; Index for the hat model is the same as BONE_SOCKET_HAT
                               (load-model "resources/models/gltf/greenman_sword.glb")    ; Index for the sword model is the same as BONE_SOCKET_HAND_R
                               (load-model "resources/models/gltf/greenman_shield.glb"))) ; Index for the shield model is the same as BONE_SOCKET_HAND_L

          (show-equip (vector t t t))   ; Toggle on/off equip

          ;; Load gltf model animations
          (anim-index 0)
          (anim-current-frame 0)

          ;; Indices of bones for sockets
          (bone-socket-index (vector -1 -1 -1))

          (position (vec3 0.0 0.0 0.0)) ; Set model position
          (angle 0))                    ; Set angle for rotate character

      (multiple-value-bind (model-animations anims-count) (load-model-animations "resources/models/gltf/greenman.glb")

        ;; Search bones for sockets
        (let ((skeleton (model-skeleton character-model)))
          (dotimes (i (model-skeleton-bone-count skeleton))
            (let ((name (bone-info-name (aref (model-skeleton-bones skeleton) i))))
              (cond ((text-is-equal name "socket_hat") (setf (aref bone-socket-index +bone-socket-hat+) i))
                    ((text-is-equal name "socket_hand_R") (setf (aref bone-socket-index +bone-socket-hand-r+) i))
                    ((text-is-equal name "socket_hand_L") (setf (aref bone-socket-index +bone-socket-hand-l+) i))))))

        (disable-cursor)                ; Limit cursor to relative movement inside the window

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (update-camera camera +camera-third-person+)

                 ;; Rotate character
                 (cond ((is-key-down +key-f+) (setf angle (mod (1+ angle) 360)))
                       ((is-key-down +key-h+) (setf angle (mod (+ 360 angle -1) 360))))

                 ;; Select current animation
                 (cond ((is-key-pressed +key-t+) (setf anim-index (mod (1+ anim-index) anims-count)))
                       ((is-key-pressed +key-g+) (setf anim-index (mod (+ anim-index anims-count -1) anims-count))))

                 ;; Toggle shown of equip
                 (when (is-key-pressed +key-one+) (setf (aref show-equip +bone-socket-hat+) (not (aref show-equip +bone-socket-hat+))))
                 (when (is-key-pressed +key-two+) (setf (aref show-equip +bone-socket-hand-r+) (not (aref show-equip +bone-socket-hand-r+))))
                 (when (is-key-pressed +key-three+) (setf (aref show-equip +bone-socket-hand-l+) (not (aref show-equip +bone-socket-hand-l+))))

                 ;; Update model animation
                 (let ((anim (aref model-animations anim-index)))
                   (setf anim-current-frame (mod (1+ anim-current-frame) (model-animation-keyframe-count anim)))
                   (update-model-animation character-model anim (float anim-current-frame))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (begin-mode-3d camera)

                   ;; Draw character
                   (let ((character-rotate (quaternion-from-axis-angle (vec3 0.0 1.0 0.0) (* angle +deg2rad+))))
                     (setf (model-transform character-model) (matrix-multiply (quaternion-to-matrix character-rotate) (matrix-translate (vx position) (vy position) (vz position))))
                     (update-model-animation character-model anim (float anim-current-frame))
                     (draw-mesh (aref (model-meshes character-model) 0) (aref (model-materials character-model) 1) (model-transform character-model)))

                   ;; Draw equipments (hat, sword, shield)
                   (dotimes (i +bone-sockets+)
                     (when (aref show-equip i)
                       (let* ((transform (aref (aref (model-animation-keyframe-poses anim) anim-current-frame) (aref bone-socket-index i)))
                              (in-rotation (transform-rotation (aref (model-skeleton-bind-pose (model-skeleton character-model)) (aref bone-socket-index i))))
                              (out-rotation (transform-rotation transform))

                              ;; Calculate socket rotation (angle between bone in initial pose and same bone in current animation frame)
                              (rotate (quaternion-multiply out-rotation (quaternion-invert in-rotation)))
                              (matrix-transform (quaternion-to-matrix rotate)))

                         ;; Translate socket to its position in the current animation
                         (setf matrix-transform (matrix-multiply matrix-transform (matrix-translate (vx (transform-translation transform)) (vy (transform-translation transform)) (vz (transform-translation transform)))))

                         ;; Transform the socket using the transform of the character (angle and translate)
                         (setf matrix-transform (matrix-multiply matrix-transform (model-transform character-model)))

                         ;; Draw mesh at socket position with socket angle rotation
                         (draw-mesh (aref (model-meshes (aref equip-model i)) 0) (aref (model-materials (aref equip-model i)) 1) matrix-transform))))

                   (draw-grid 10 1.0)

                   (end-mode-3d)

                   (draw-text "Use the T/G to switch animation" 10 10 20 +gray+)
                   (draw-text "Use the F/H to rotate character left/right" 10 35 20 +gray+)
                   (draw-text "Use the 1,2,3 to toggle shown of hat, sword and shield" 10 60 20 +gray+)

                   (end-drawing)))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-model-animations model-animations anims-count)
        (unload-model character-model)  ; Unload character model and meshes/material

        ;; Unload equipment model and meshes/material
        (dotimes (i +bone-sockets+) (unload-model (aref equip-model i)))

        (close-window)))))              ; Close window and OpenGL context

(main)
