(in-package #:cl-raylib)

;;;===================================================================================
;;; rcamera - Basic camera system with support for multiple camera modes
;;; Port of raylib/src/rcamera.h
;;;===================================================================================

;; Camera3D constructor accepting the cl-raylib.cffi projection keywords
(defun make-camera3d (&key (position (vec3 0.0 0.0 0.0))
                           (target (vec3 0.0 0.0 -1.0))
                           (up (vec3 0.0 1.0 0.0))
                           (fovy 45.0)
                           (projection :camera-perspective))
  "Create a Camera3D (projection: +camera-perspective+/:camera-perspective or +camera-orthographic+/:camera-orthographic)"
  (%make-camera3d :position position :target target :up up :fovy (float fovy 1.0)
                  :projection (case projection
                                (:camera-perspective +camera-perspective+)
                                (:camera-orthographic +camera-orthographic+)
                                (t projection))))

(defun %camera-mode (mode)
  "CameraMode value (accepts the cl-raylib.cffi keywords)"
  (case mode
    (:camera-custom +camera-custom+)
    (:camera-free +camera-free+)
    (:camera-orbital +camera-orbital+)
    (:camera-first-person +camera-first-person+)
    (:camera-third-person +camera-third-person+)
    (t mode)))

;;----------------------------------------------------------------------------------
;; Defines and Macros
;;----------------------------------------------------------------------------------
(defconstant +camera-cull-distance-near+ +rl-cull-distance-near+)
(defconstant +camera-cull-distance-far+ +rl-cull-distance-far+)

(defconstant +camera-move-speed+ 5.4 "Units per second")
(defconstant +camera-rotation-speed+ 0.03)
(defconstant +camera-pan-speed+ 2.0)

;; Camera mouse movement sensitivity
(defconstant +camera-mouse-move-sensitivity+ 0.003)

;; Camera orbital speed in CAMERA_ORBITAL mode
(defconstant +camera-orbital-speed+ 0.5 "Radians per second")

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------

;; Returns the cameras forward vector (normalized)
(defun get-camera-forward (camera)
  "Returns the cameras forward vector (normalized)"
  (vector3-normalize (vector3-subtract (camera3d-target camera) (camera3d-position camera))))

;; Returns the cameras up vector (normalized)
;; Note: The up vector might not be perpendicular to the forward vector
(defun get-camera-up (camera)
  "Returns the cameras up vector (normalized)"
  (vector3-normalize (camera3d-up camera)))

;; Returns the cameras right vector (normalized)
(defun get-camera-right (camera)
  "Returns the cameras right vector (normalized)"
  (let ((forward (get-camera-forward camera))
        (up (get-camera-up camera)))
    (vector3-normalize (vector3-cross-product forward up))))

(defun %camera-project-on-world-plane (camera v)
  "Project vector onto world plane (the plane defined by the up vector)"
  (let ((up (camera3d-up camera)))
    (vector3-normalize
     (cond ((> (abs (vz up)) 0.7071) (vec3 (vx v) (vy v) 0.0))
           ((> (abs (vx up)) 0.7071) (vec3 0.0 (vy v) (vz v)))
           (t (vec3 (vx v) 0.0 (vz v)))))))

(defun %camera-move (camera offset)
  "Move position and target"
  (setf (camera3d-position camera) (vector3-add (camera3d-position camera) offset)
        (camera3d-target camera) (vector3-add (camera3d-target camera) offset)))

;; Moves the camera in its forward direction
(defun camera-move-forward (camera distance move-in-world-plane)
  "Moves the camera in its forward direction"
  (let ((forward (get-camera-forward camera)))
    (when move-in-world-plane
      (setf forward (%camera-project-on-world-plane camera forward)))
    ;; Scale by distance
    (%camera-move camera (vector3-scale forward distance))))

;; Moves the camera in its up direction
(defun camera-move-up (camera distance)
  "Moves the camera in its up direction"
  (%camera-move camera (vector3-scale (get-camera-up camera) distance)))

;; Moves the camera target in its current right direction
(defun camera-move-right (camera distance move-in-world-plane)
  "Moves the camera target in its current right direction"
  (let ((right (get-camera-right camera)))
    (when move-in-world-plane
      (setf right (%camera-project-on-world-plane camera right)))
    ;; Scale by distance
    (%camera-move camera (vector3-scale right distance))))

;; Moves the camera position closer/farther to/from the camera target
(defun camera-move-to-target (camera delta)
  "Moves the camera position closer/farther to/from the camera target"
  (let ((distance (vector3-distance (camera3d-position camera) (camera3d-target camera))))
    ;; Apply delta
    (setf distance (+ distance delta))
    ;; Distance must be greater than 0
    (when (<= distance 0) (setf distance 0.001))
    ;; Set new distance by moving the position along the forward vector
    (let ((forward (get-camera-forward camera)))
      (setf (camera3d-position camera) (vector3-add (camera3d-target camera) (vector3-scale forward (- distance)))))))

(defun %camera-apply-view (camera target-position rotate-around-target)
  (if rotate-around-target
      ;; Move position relative to target
      (setf (camera3d-position camera) (vector3-subtract (camera3d-target camera) target-position))
      ;; Move target relative to position
      (setf (camera3d-target camera) (vector3-add (camera3d-position camera) target-position))))

;; Rotates the camera around its up vector
;; Yaw is "looking left and right"
;; If rotateAroundTarget is false, the camera rotates around its position
;; Note: angle must be provided in radians
(defun camera-yaw (camera angle rotate-around-target)
  "Rotates the camera around its up vector (angle in radians)"
  (let* ((up (get-camera-up camera))                                                  ; Rotation axis
         (target-position (vector3-subtract (camera3d-target camera) (camera3d-position camera)))) ; View vector
    ;; Rotate view vector around up axis
    (%camera-apply-view camera (vector3-rotate-by-axis-angle target-position up angle) rotate-around-target)))

;; Rotates the camera around its right vector, pitch is "looking up and down"
;;  - lockView prevents camera overrotation (aka "somersaults")
;;  - rotateAroundTarget defines if rotation is around target or around its position
;;  - rotateUp rotates the up direction as well (typically only useful in CAMERA_FREE)
;; NOTE: [angle] must be provided in radians
(defun camera-pitch (camera angle lock-view rotate-around-target rotate-up)
  "Rotates the camera around its right vector (angle in radians)"
  (let* ((angle (float angle 1.0))
         (up (get-camera-up camera))                                                  ; Up direction
         (target-position (vector3-subtract (camera3d-target camera) (camera3d-position camera)))) ; View vector
    (when lock-view
      ;; In these camera modes, clamp the Pitch angle
      ;; to allow only viewing straight up or down

      ;; Clamp view up
      (let ((max-angle-up (- (vector3-angle up target-position) 0.001))) ; avoid numerical errors
        (when (> angle max-angle-up) (setf angle max-angle-up)))

      ;; Clamp view down
      (let ((max-angle-down (+ (* (vector3-angle (vector3-negate up) target-position) -1.0) ; downwards angle is negative
                               0.001)))                                             ; avoid numerical errors
        (when (< angle max-angle-down) (setf angle max-angle-down))))

    ;; Rotation axis
    (let ((right (get-camera-right camera)))
      ;; Rotate view vector around right axis
      (%camera-apply-view camera (vector3-rotate-by-axis-angle target-position right angle) rotate-around-target)

      (when rotate-up
        ;; Rotate up direction around right axis
        (setf (camera3d-up camera) (vector3-rotate-by-axis-angle (camera3d-up camera) right angle))))))

;; Rotates the camera around its forward vector
;; Roll is "turning your head sideways to the left or right"
;; Note: angle must be provided in radians
(defun camera-roll (camera angle)
  "Rotates the camera around its forward vector (angle in radians)"
  (let ((forward (get-camera-forward camera)))                                        ; Rotation axis
    ;; Rotate up direction around forward axis
    (setf (camera3d-up camera) (vector3-rotate-by-axis-angle (camera3d-up camera) forward angle))))

;; Returns the camera view matrix
(defun get-camera-view-matrix (camera)
  "Returns the camera view matrix"
  (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera)))

;; Returns the camera projection matrix
(defun get-camera-projection-matrix (camera aspect)
  "Returns the camera projection matrix"
  (case (%camera-projection camera)
    (#.+camera-perspective+
     (matrix-perspective (* (camera3d-fovy camera) +deg2rad+) aspect +camera-cull-distance-near+ +camera-cull-distance-far+))
    (#.+camera-orthographic+
     (let* ((top (/ (float (camera3d-fovy camera) 1d0) 2.0d0))
            (right (* top aspect)))
       (matrix-ortho (- right) right (- top) top +camera-cull-distance-near+ +camera-cull-distance-far+)))
    (t (matrix-identity))))

;; Update camera position for selected mode
;; Camera mode: CAMERA_FREE, CAMERA_FIRST_PERSON, CAMERA_THIRD_PERSON, CAMERA_ORBITAL or CUSTOM
(defun update-camera (camera mode)
  "Update camera position for selected mode"
  (let* ((mode (%camera-mode mode))
         (mouse-position-delta (get-mouse-delta))
         (move-in-world-plane (or (= mode +camera-first-person+) (= mode +camera-third-person+)))
         (rotate-around-target (or (= mode +camera-third-person+) (= mode +camera-orbital+)))
         (lock-view (or (= mode +camera-free+) (= mode +camera-first-person+) (= mode +camera-third-person+) (= mode +camera-orbital+)))
         (rotate-up nil)
         ;; Camera speeds based on frame time
         (camera-move-speed (* +camera-move-speed+ (get-frame-time)))
         (camera-rotation-speed (* +camera-rotation-speed+ (get-frame-time)))
         (camera-pan-speed (* +camera-pan-speed+ (get-frame-time)))
         (camera-orbital-speed (* +camera-orbital-speed+ (get-frame-time))))
    (cond
      ((= mode +camera-custom+))
      ((= mode +camera-orbital+)
       (let* ((rotation (matrix-rotate (get-camera-up camera) camera-orbital-speed))
              (view (vector3-subtract (camera3d-position camera) (camera3d-target camera))))
         (setf view (vector3-transform view rotation))
         (setf (camera3d-position camera) (vector3-add (camera3d-target camera) view))))
      (t
       ;; Camera rotation
       (when (is-key-down +key-down+) (camera-pitch camera (- camera-rotation-speed) lock-view rotate-around-target rotate-up))
       (when (is-key-down +key-up+) (camera-pitch camera camera-rotation-speed lock-view rotate-around-target rotate-up))
       (when (is-key-down +key-right+) (camera-yaw camera (- camera-rotation-speed) rotate-around-target))
       (when (is-key-down +key-left+) (camera-yaw camera camera-rotation-speed rotate-around-target))
       (when (is-key-down +key-q+) (camera-roll camera (- camera-rotation-speed)))
       (when (is-key-down +key-e+) (camera-roll camera camera-rotation-speed))

       ;; Camera movement
       ;; Camera pan (for CAMERA_FREE)
       (if (and (= mode +camera-free+) (is-mouse-button-down +mouse-button-middle+))
           (let ((mouse-delta (get-mouse-delta)))
             (when (> (vx mouse-delta) 0.0) (camera-move-right camera camera-pan-speed move-in-world-plane))
             (when (< (vx mouse-delta) 0.0) (camera-move-right camera (- camera-pan-speed) move-in-world-plane))
             (when (> (vy mouse-delta) 0.0) (camera-move-up camera (- camera-pan-speed)))
             (when (< (vy mouse-delta) 0.0) (camera-move-up camera camera-pan-speed)))
           (progn
             ;; Mouse support
             (camera-yaw camera (* (- (vx mouse-position-delta)) +camera-mouse-move-sensitivity+) rotate-around-target)
             (camera-pitch camera (* (- (vy mouse-position-delta)) +camera-mouse-move-sensitivity+) lock-view rotate-around-target rotate-up)))

       ;; Keyboard support
       (when (is-key-down +key-w+) (camera-move-forward camera camera-move-speed move-in-world-plane))
       (when (is-key-down +key-a+) (camera-move-right camera (- camera-move-speed) move-in-world-plane))
       (when (is-key-down +key-s+) (camera-move-forward camera (- camera-move-speed) move-in-world-plane))
       (when (is-key-down +key-d+) (camera-move-right camera camera-move-speed move-in-world-plane))

       ;; Gamepad movement
       (when (is-gamepad-available 0)
         ;; Gamepad controller support
         (camera-yaw camera (* (- (* (get-gamepad-axis-movement 0 +gamepad-axis-right-x+) 2)) +camera-mouse-move-sensitivity+)
                     rotate-around-target)
         (camera-pitch camera (* (- (* (get-gamepad-axis-movement 0 +gamepad-axis-right-y+) 2)) +camera-mouse-move-sensitivity+)
                       lock-view rotate-around-target rotate-up)

         (when (<= (get-gamepad-axis-movement 0 +gamepad-axis-left-y+) -0.25) (camera-move-forward camera camera-move-speed move-in-world-plane))
         (when (<= (get-gamepad-axis-movement 0 +gamepad-axis-left-x+) -0.25) (camera-move-right camera (- camera-move-speed) move-in-world-plane))
         (when (>= (get-gamepad-axis-movement 0 +gamepad-axis-left-y+) 0.25) (camera-move-forward camera (- camera-move-speed) move-in-world-plane))
         (when (>= (get-gamepad-axis-movement 0 +gamepad-axis-left-x+) 0.25) (camera-move-right camera camera-move-speed move-in-world-plane)))

       (when (= mode +camera-free+)
         (when (is-key-down +key-space+) (camera-move-up camera camera-move-speed))
         (when (is-key-down +key-left-control+) (camera-move-up camera (- camera-move-speed))))))

    (when (or (= mode +camera-third-person+) (= mode +camera-orbital+) (= mode +camera-free+))
      ;; Zoom target distance
      (camera-move-to-target camera (- (get-mouse-wheel-move)))
      (when (is-key-pressed +key-kp-subtract+) (camera-move-to-target camera 2.0))
      (when (is-key-pressed +key-kp-add+) (camera-move-to-target camera -2.0)))
    camera))

;; Update camera movement, movement/rotation values should be provided by user
(defun update-camera-pro (camera movement rotation zoom)
  "Update camera movement/rotation
movement: x forward/backward, y right/left, z up/down; rotation (degrees): x yaw, y pitch, z roll; zoom: move towards target"
  (let ((lock-view t)
        (rotate-around-target nil)
        (rotate-up nil)
        (move-in-world-plane t))
    ;; Camera rotation
    (camera-pitch camera (* (- (vy rotation)) +deg2rad+) lock-view rotate-around-target rotate-up)
    (camera-yaw camera (* (- (vx rotation)) +deg2rad+) rotate-around-target)
    (camera-roll camera (* (vz rotation) +deg2rad+))

    ;; Camera movement
    (camera-move-forward camera (vx movement) move-in-world-plane)
    (camera-move-right camera (vy movement) move-in-world-plane)
    (camera-move-up camera (vz movement))

    ;; Zoom target distance
    (camera-move-to-target camera zoom)
    camera))
