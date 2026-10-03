;;;; raylib [core] example - 3d camera first person
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.3
;;;;
;;;; Copyright (c) 2015-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_3d_camera_first_person.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-camera-first-person
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-camera-first-person)

(defconstant +max-columns+ 20)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d camera first person")

    ;; Define the camera to look into our 3d world (position, target, up vector)
    (let ((camera (make-camera3d :position (vec3 0.0 2.0 4.0)  ; Camera position
                                 :target (vec3 0.0 2.0 0.0)    ; Camera looking at point
                                 :up (vec3 0.0 1.0 0.0)        ; Camera up vector (rotation towards target)
                                 :fovy 60.0                    ; Camera field-of-view Y
                                 :projection +camera-perspective+)) ; Camera projection type
          (camera-mode +camera-first-person+)
          ;; Generates some random columns
          (heights (make-array +max-columns+ :initial-element 0.0))
          (positions (make-array +max-columns+))
          (colors (make-array +max-columns+)))

      (dotimes (i +max-columns+)
        (setf (aref heights i) (float (get-random-value 1 12)))
        (setf (aref positions i) (vec3 (float (get-random-value -15 15)) (/ (aref heights i) 2.0) (float (get-random-value -15 15))))
        (setf (aref colors i) (list (get-random-value 20 255) (get-random-value 10 55) 30 255)))

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Switch camera mode
               (when (is-key-pressed +key-one+)
                 (setf camera-mode +camera-free+
                       (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll

               (when (is-key-pressed +key-two+)
                 (setf camera-mode +camera-first-person+
                       (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll

               (when (is-key-pressed +key-three+)
                 (setf camera-mode +camera-third-person+
                       (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll

               (when (is-key-pressed +key-four+)
                 (setf camera-mode +camera-orbital+
                       (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll

               ;; Switch camera projection
               (when (is-key-pressed +key-p+)
                 (cond ((= (camera3d-projection camera) +camera-perspective+)
                        ;; Create isometric view
                        (setf camera-mode +camera-third-person+)
                        ;; Note: The target distance is related to the render distance in the orthographic projection
                        (setf (camera3d-position camera) (vec3 0.0 2.0 -100.0)
                              (camera3d-target camera) (vec3 0.0 2.0 0.0)
                              (camera3d-up camera) (vec3 0.0 1.0 0.0)
                              (camera3d-projection camera) +camera-orthographic+
                              (camera3d-fovy camera) 20.0) ; near plane width in CAMERA_ORTHOGRAPHIC
                        (camera-yaw camera (* -135 +deg2rad+) t)
                        (camera-pitch camera (* -45 +deg2rad+) t t nil))
                       ((= (camera3d-projection camera) +camera-orthographic+)
                        ;; Reset to default view
                        (setf camera-mode +camera-third-person+)
                        (setf (camera3d-position camera) (vec3 0.0 2.0 10.0)
                              (camera3d-target camera) (vec3 0.0 2.0 0.0)
                              (camera3d-up camera) (vec3 0.0 1.0 0.0)
                              (camera3d-projection camera) +camera-perspective+
                              (camera3d-fovy camera) 60.0))))

               ;; Update camera computes movement internally depending on the camera mode
               ;; Some default standard keyboard/mouse inputs are hardcoded to simplify use
               ;; For advanced camera controls, it's recommended to compute camera movement manually
               (update-camera camera camera-mode) ; Update camera

               ;; Camera PRO usage example (EXPERIMENTAL)
               ;; This new camera function allows custom movement/rotation values to be directly provided
               ;; as input parameters, with this approach, rcamera module is internally independent of raylib inputs
               #+(or)
               (update-camera-pro camera
                                  (vec3 (- (if (or (is-key-down +key-w+) (is-key-down +key-up+)) 0.1 0.0) ; Move forward-backward
                                           (if (or (is-key-down +key-s+) (is-key-down +key-down+)) 0.1 0.0))
                                        (- (if (or (is-key-down +key-d+) (is-key-down +key-right+)) 0.1 0.0) ; Move right-left
                                           (if (or (is-key-down +key-a+) (is-key-down +key-left+)) 0.1 0.0))
                                        0.0)  ; Move up-down
                                  (vec3 (* (vx (get-mouse-delta)) 0.05)    ; Rotation: yaw
                                        (* (vy (get-mouse-delta)) 0.05)    ; Rotation: pitch
                                        0.0)                               ; Rotation: roll
                                  (* (get-mouse-wheel-move) 2.0))          ; Move to target (zoom)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)

               (draw-plane (vec3 0.0 0.0 0.0) (vec2 32.0 32.0) +lightgray+) ; Draw ground
               (draw-cube (vec3 -16.0 2.5 0.0) 1.0 5.0 32.0 +blue+)       ; Draw a blue wall
               (draw-cube (vec3 16.0 2.5 0.0) 1.0 5.0 32.0 +lime+)        ; Draw a green wall
               (draw-cube (vec3 0.0 2.5 16.0) 32.0 5.0 1.0 +gold+)        ; Draw a yellow wall

               ;; Draw some cubes around
               (dotimes (i +max-columns+)
                 (draw-cube (aref positions i) 2.0 (aref heights i) 2.0 (aref colors i))
                 (draw-cube-wires (aref positions i) 2.0 (aref heights i) 2.0 +maroon+))

               ;; Draw player cube
               (when (= camera-mode +camera-third-person+)
                 (draw-cube (camera3d-target camera) 0.5 0.5 0.5 +purple+)
                 (draw-cube-wires (camera3d-target camera) 0.5 0.5 0.5 +darkpurple+))

               (end-mode-3d)

               ;; Draw info boxes
               (draw-rectangle 5 5 330 100 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 5 5 330 100 +blue+)

               (draw-text "Camera controls:" 15 15 10 +black+)
               (draw-text "- Move keys: W, A, S, D, Space, Left-Ctrl" 15 30 10 +black+)
               (draw-text "- Look around: arrow keys or mouse" 15 45 10 +black+)
               (draw-text "- Camera mode keys: 1, 2, 3, 4" 15 60 10 +black+)
               (draw-text "- Zoom keys: num-plus, num-minus or mouse scroll" 15 75 10 +black+)
               (draw-text "- Camera projection key: P" 15 90 10 +black+)

               (draw-rectangle 600 5 195 100 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 600 5 195 100 +blue+)

               (draw-text "Camera status:" 610 15 10 +black+)
               (draw-text (text-format "- Mode: %s" (cond ((= camera-mode +camera-free+) "FREE")
                                                          ((= camera-mode +camera-first-person+) "FIRST_PERSON")
                                                          ((= camera-mode +camera-third-person+) "THIRD_PERSON")
                                                          ((= camera-mode +camera-orbital+) "ORBITAL")
                                                          (t "CUSTOM")))
                          610 30 10 +black+)
               (draw-text (text-format "- Projection: %s" (cond ((= (camera3d-projection camera) +camera-perspective+) "PERSPECTIVE")
                                                                ((= (camera3d-projection camera) +camera-orthographic+) "ORTHOGRAPHIC")
                                                                (t "CUSTOM")))
                          610 45 10 +black+)
               (let ((p (camera3d-position camera)) (tg (camera3d-target camera)) (u (camera3d-up camera)))
                 (draw-text (text-format "- Position: (%06.3f, %06.3f, %06.3f)" (vx p) (vy p) (vz p)) 610 60 10 +black+)
                 (draw-text (text-format "- Target: (%06.3f, %06.3f, %06.3f)" (vx tg) (vy tg) (vz tg)) 610 75 10 +black+)
                 (draw-text (text-format "- Up: (%06.3f, %06.3f, %06.3f)" (vx u) (vy u) (vz u)) 610 90 10 +black+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
