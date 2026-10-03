;;;; raylib [core] example - 3d camera fps
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Agnis Aldiņš (@nezvers) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Agnis Aldiņš (@nezvers)
;;;; Common Lisp port of raylib/examples/core/core_3d_camera_fps.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-camera-fps
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-camera-fps)

;;----------------------------------------------------------------------------------
;; Defines and Macros
;;----------------------------------------------------------------------------------
;; Movement constants
(defconstant +gravity+ 32.0)
(defconstant +max-speed+ 20.0)
(defconstant +crouch-speed+ 5.0)
(defconstant +jump-force+ 12.0)
(defconstant +max-accel+ 150.0)
;; Grounded drag
(defconstant +friction+ 0.86)
;; Increasing air drag, increases strafing speed
(defconstant +air-drag+ 0.98)
;; Responsiveness for turning movement direction to looked direction
(defconstant +control+ 15.0)
(defconstant +crouch-height+ 0.0)
(defconstant +stand-height+ 1.0)
(defconstant +bottom-height+ 0.5)

;; NOTE: NORMALIZE_INPUT is defined in the C example (the input normalization is enabled)
(defparameter *normalize-input* t)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Body structure
(defstruct body
  (position (vec3 0.0 0.0 0.0))
  (velocity (vec3 0.0 0.0 0.0))
  (dir (vec3 0.0 0.0 0.0))
  (is-grounded nil))

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *sensitivity* (vec2 0.001 0.001))

(defparameter *player* (make-body))
(defparameter *look-rotation* (vec2 0.0 0.0))
(defparameter *head-timer* 0.0)
(defparameter *walk-lerp* 0.0)
(defparameter *head-lerp* +stand-height+)
(defparameter *lean* (vec2 0.0 0.0))

(defun %key (key) (if (is-key-down key) 1 0))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d camera fps")

    ;; Initialize camera variables
    ;; NOTE: UpdateCameraFPS() takes care of the rest
    (let ((camera (make-camera3d :fovy 60.0 :projection +camera-perspective+
                                 :position (vec3 (vx (body-position *player*))
                                                 (+ (vy (body-position *player*)) (+ +bottom-height+ *head-lerp*))
                                                 (vz (body-position *player*))))))

      (update-camera-fps camera)        ; Update camera parameters

      (disable-cursor)                  ; Limit cursor to relative movement inside the window

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((mouse-delta (get-mouse-delta)))
                 (decf (vx *look-rotation*) (* (vx mouse-delta) (vx *sensitivity*)))
                 (incf (vy *look-rotation*) (* (vy mouse-delta) (vy *sensitivity*))))

               (let* ((sideway (- (%key +key-d+) (%key +key-a+)))
                      (forward (- (%key +key-w+) (%key +key-s+)))
                      (crouching (is-key-down +key-left-control+)))
                 (update-body *player* (vx *look-rotation*) sideway forward (is-key-pressed +key-space+) crouching)

                 (let ((delta (get-frame-time)))
                   (setf *head-lerp* (lerp *head-lerp* (if crouching +crouch-height+ +stand-height+) (* 20.0 delta)))
                   (setf (camera3d-position camera) (vec3 (vx (body-position *player*))
                                                          (+ (vy (body-position *player*)) (+ +bottom-height+ *head-lerp*))
                                                          (vz (body-position *player*))))

                   (if (and (body-is-grounded *player*) (or (/= forward 0) (/= sideway 0)))
                       (progn
                         (incf *head-timer* (* delta 3.0))
                         (setf *walk-lerp* (lerp *walk-lerp* 1.0 (* 10.0 delta)))
                         (setf (camera3d-fovy camera) (lerp (camera3d-fovy camera) 55.0 (* 5.0 delta))))
                       (progn
                         (setf *walk-lerp* (lerp *walk-lerp* 0.0 (* 10.0 delta)))
                         (setf (camera3d-fovy camera) (lerp (camera3d-fovy camera) 60.0 (* 5.0 delta)))))

                   (setf (vx *lean*) (lerp (vx *lean*) (* sideway 0.02) (* 10.0 delta)))
                   (setf (vy *lean*) (lerp (vy *lean*) (* forward 0.015) (* 10.0 delta)))))

               (update-camera-fps camera)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (begin-mode-3d camera)
               (draw-level)
               (end-mode-3d)

               ;; Draw info box
               (draw-rectangle 5 5 330 75 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 5 5 330 75 +blue+)

               (draw-text "Camera controls:" 15 15 10 +black+)
               (draw-text "- Move keys: W, A, S, D, Space, Left-Ctrl" 15 30 10 +black+)
               (draw-text "- Look around: arrow keys or mouse" 15 45 10 +black+)
               (draw-text (text-format "- Velocity Len: (%06.3f)"
                                       (vector2-length (vec2 (vx (body-velocity *player*)) (vz (body-velocity *player*)))))
                          15 60 10 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Update body considering current world state
(defun update-body (body rot side forward jump-pressed crouch-hold)
  (let ((input (vec2 (float side) (float (- forward)))))

    ;; Slow down diagonal movement
    (when (and *normalize-input* (/= side 0) (/= forward 0)) (setf input (vector2-normalize input)))

    (let ((delta (get-frame-time)))

      (unless (body-is-grounded body) (decf (vy (body-velocity body)) (* +gravity+ delta)))

      (when (and (body-is-grounded body) jump-pressed)
        (setf (vy (body-velocity body)) +jump-force+
              (body-is-grounded body) nil)
        ;; Sound can be played at this moment
        ;;SetSoundPitch(fxJump, 1.0f + (GetRandomValue(-100, 100)*0.001));
        ;;PlaySound(fxJump);
        )

      (let* ((front (vec3 (sin rot) 0.0 (cos rot)))
             (right (vec3 (cos (- rot)) 0.0 (sin (- rot))))
             (desired-dir (vec3 (+ (* (vx input) (vx right)) (* (vy input) (vx front)))
                                0.0
                                (+ (* (vx input) (vz right)) (* (vy input) (vz front))))))
        (setf (body-dir body) (vector3-lerp (body-dir body) desired-dir (* +control+ delta)))

        (let* ((decel (if (body-is-grounded body) +friction+ +air-drag+))
               (hvel (vec3 (* (vx (body-velocity body)) decel) 0.0 (* (vz (body-velocity body)) decel)))
               (hvel-length (vector3-length hvel))) ; Magnitude
          (when (< hvel-length (* +max-speed+ 0.01)) (setf hvel (vec3 0.0 0.0 0.0)))

          ;; This is what creates strafing
          (let* ((speed (vector3-dot-product hvel (body-dir body)))
                 ;; Whenever the amount of acceleration to add is clamped by the maximum acceleration constant,
                 ;; a Player can make the speed faster by bringing the direction closer to horizontal velocity angle
                 ;; More info here: https://youtu.be/v3zT3Z5apaM?t=165
                 (max-speed (if crouch-hold +crouch-speed+ +max-speed+))
                 (accel (clamp (- max-speed speed) 0.0 (* +max-accel+ delta))))
            (incf (vx hvel) (* (vx (body-dir body)) accel))
            (incf (vz hvel) (* (vz (body-dir body)) accel))

            (setf (vx (body-velocity body)) (vx hvel)
                  (vz (body-velocity body)) (vz hvel))

            (incf (vx (body-position body)) (* (vx (body-velocity body)) delta))
            (incf (vy (body-position body)) (* (vy (body-velocity body)) delta))
            (incf (vz (body-position body)) (* (vz (body-velocity body)) delta))

            ;; Fancy collision system against the floor
            (when (<= (vy (body-position body)) 0.0)
              (setf (vy (body-position body)) 0.0
                    (vy (body-velocity body)) 0.0
                    (body-is-grounded body) t)))))))) ; Enable jumping

;; Update camera for FPS behaviour
(defun update-camera-fps (camera)
  (let* ((up (vec3 0.0 1.0 0.0))
         (target-offset (vec3 0.0 0.0 -1.0))
         ;; Left and right
         (yaw (vector3-rotate-by-axis-angle target-offset up (vx *look-rotation*))))

    ;; Clamp view up
    (let ((max-angle-up (- (vector3-angle up yaw) 0.001))) ; Avoid numerical errors
      (when (> (- (vy *look-rotation*)) max-angle-up) (setf (vy *look-rotation*) (- max-angle-up))))

    ;; Clamp view down
    (let ((max-angle-down (+ (* (vector3-angle (vector3-negate up) yaw) -1.0) ; Downwards angle is negative
                             0.001)))                                         ; Avoid numerical errors
      (when (< (- (vy *look-rotation*)) max-angle-down) (setf (vy *look-rotation*) (- max-angle-down))))

    ;; Up and down
    (let* ((right (vector3-normalize (vector3-cross-product yaw up)))
           ;; Rotate view vector around right axis
           (pitch-angle (clamp (- (- (vy *look-rotation*)) (vy *lean*)) ; Clamp angle so it doesn't go past straight up or straight down
                               (+ (/ (- +pi+) 2) 0.0001) (- (/ +pi+ 2) 0.0001)))
           (pitch (vector3-rotate-by-axis-angle yaw right pitch-angle))
           ;; Head animation
           ;; Rotate up direction around forward axis
           (head-sin (sin (* *head-timer* +pi+)))
           (head-cos (cos (* *head-timer* +pi+)))
           (step-rotation 0.01))
      (setf (camera3d-up camera) (vector3-rotate-by-axis-angle up pitch (+ (* head-sin step-rotation) (vx *lean*))))

      ;; Camera BOB
      (let* ((bob-side 0.1)
             (bob-up 0.15)
             (bobbing (vector3-scale right (* head-sin bob-side))))
        (setf (vy bobbing) (abs (* head-cos bob-up)))

        (setf (camera3d-position camera) (vector3-add (camera3d-position camera) (vector3-scale bobbing *walk-lerp*)))
        (setf (camera3d-target camera) (vector3-add (camera3d-position camera) pitch))))))

;; Draw game level
(defun draw-level ()
  (let ((floor-extent 25)
        (tile-size 5.0)
        (tile-color1 (list 150 200 200 255)))

    ;; Floor tiles
    (loop for y from (- floor-extent) below floor-extent
          do (loop for x from (- floor-extent) below floor-extent
                   do (cond ((and (logtest y 1) (logtest x 1))
                             (draw-plane (vec3 (* x tile-size) 0.0 (* y tile-size)) (vec2 tile-size tile-size) tile-color1))
                            ((and (not (logtest y 1)) (not (logtest x 1)))
                             (draw-plane (vec3 (* x tile-size) 0.0 (* y tile-size)) (vec2 tile-size tile-size) +lightgray+)))))

    (let ((tower-size (vec3 16.0 32.0 16.0))
          (tower-color (list 150 200 200 255))
          (tower-pos (vec3 16.0 16.0 16.0)))
      (draw-cube-v tower-pos tower-size tower-color)
      (draw-cube-wires-v tower-pos tower-size +darkblue+)

      (setf (vx tower-pos) (* (vx tower-pos) -1))
      (draw-cube-v tower-pos tower-size tower-color)
      (draw-cube-wires-v tower-pos tower-size +darkblue+)

      (setf (vz tower-pos) (* (vz tower-pos) -1))
      (draw-cube-v tower-pos tower-size tower-color)
      (draw-cube-wires-v tower-pos tower-size +darkblue+)

      (setf (vx tower-pos) (* (vx tower-pos) -1))
      (draw-cube-v tower-pos tower-size tower-color)
      (draw-cube-wires-v tower-pos tower-size +darkblue+))

    ;; Red sun
    (draw-sphere (vec3 300.0 300.0 0.0) 100.0 (list 255 0 0 255))))

(main)
