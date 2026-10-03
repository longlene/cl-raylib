(in-package #:cl-raylib)

;;; 3D Camera System
;;; This module provides comprehensive 3D camera functionality
;;; including first-person, third-person, and orbital cameras

;;; Custom constructor that handles vec3 objects directly
(defun make-camera3d (&key (position (vec3 0.0 0.0 0.0))
                           (target (vec3 0.0 0.0 -1.0))
                           (up (vec3 0.0 1.0 0.0))
                           (fovy 45.0)
                           (projection :camera-perspective))
  "Create a 3D camera using vec3 objects directly"
  (%make-camera3d :position position
                  :target target
                  :up up
                  :fovy fovy
                  :projection (keyword-to-projection projection)))

;;; Global camera state
(defvar *current-camera* nil "Currently active camera")
(defvar *camera-mode* +camera-custom+ "Current camera update mode")
(defvar *camera-angle-x* 0.0 "Camera X rotation angle")
(defvar *camera-angle-y* 0.0 "Camera Y rotation angle")
(defvar *camera-target-distance* 5.0 "Distance to target for orbital camera")
(defvar *camera-move-speed* 1.0 "Camera movement speed")
(defvar *camera-rotation-speed* 1.0 "Camera rotation speed")
(defvar *camera-smooth-zoom-speed* 1.0 "Camera zoom speed")
(defvar *camera-min-clamp* 0.3 "Minimum clamp value")
(defvar *camera-max-clamp* -0.3 "Maximum clamp value")

;;; Camera projection keyword conversion
(defun keyword-to-projection (projection-keyword)
  "Convert projection keyword to constant"
  (case projection-keyword
    (:camera-perspective +camera-perspective+)
    (:camera-orthographic +camera-orthographic+)
    (t (if (numberp projection-keyword) projection-keyword projection-keyword))))

;;; Camera creation functions

(defun make-camera-3d (position target up fovy projection)
  "Create a 3D camera with specified parameters"
  (make-camera3d :position position
                 :target target
                 :up up
                 :fovy fovy
                 :projection (keyword-to-projection projection)))

(defun camera3d-default ()
  "Create default 3D camera"
  (make-camera3d :position (vec3 0.0 10.0 10.0)
                 :target (vec3 0.0 0.0 0.0)
                 :up (vec3 0.0 1.0 0.0)
                 :fovy 45.0
                 :projection :camera-perspective))

(defun camera3d-first-person (position target up)
  "Create first-person camera"
  (make-camera3d :position position
                 :target target
                 :up up
                 :fovy 45.0
                 :projection :camera-perspective))

(defun camera3d-third-person (position target up distance)
  "Create third-person camera"
  (setf *camera-target-distance* distance)
  (make-camera3d :position position
                 :target target
                 :up up
                 :fovy 45.0
                 :projection :camera-perspective))

;;; Camera matrix functions

(defun get-camera-projection-matrix (camera aspect)
  "Get camera projection matrix (same projection as BeginMode3D())"
  (case (%camera-projection camera)
    (#.+camera-perspective+
     (matrix-perspective (* (camera3d-fovy camera) +deg2rad+) aspect
                         (rl-get-cull-distance-near) (rl-get-cull-distance-far)))
    (#.+camera-orthographic+
     (let* ((top (/ (camera3d-fovy camera) 2.0d0))
            (right (* top aspect)))
       (matrix-ortho (- right) right (- top) top
                     (rl-get-cull-distance-near) (rl-get-cull-distance-far))))
    (t (matrix-identity))))

;;; Camera transformation functions

(defun camera3d-get-forward (camera)
  "Get camera forward vector"
  (vunit (v- (camera3d-target camera)
             (camera3d-position camera))))

(defun camera3d-get-right (camera)
  "Get camera right vector"
  (vunit (vc (camera3d-get-forward camera)
              (camera3d-up camera))))

(defun camera3d-get-up (camera)
  "Get camera up vector (recalculated from forward and right)"
  (vc (camera3d-get-right camera)
      (camera3d-get-forward camera)))

;;; Camera movement functions

(defun camera3d-move-forward (camera distance &optional (move-in-world-plane t))
  "Move camera forward/backward
   When move-in-world-plane is true, movement is restricted to horizontal plane (Y=0)"
  (let* ((pos (camera3d-position camera))
         (target (camera3d-target camera))
         ;; Calculate forward direction using 3d-math operations
         (forward (v- target pos))
         (forward-normalized (if move-in-world-plane
                               ;; Project to world plane (Y=0) for first-person style movement
                               (let ((f-copy (vcopy forward)))
                                 (setf (vy f-copy) 0.0)
                                 (vunit f-copy))
                               (vunit forward)))
         (movement (v* forward-normalized distance))
         (new-position (v+ pos movement))
         (new-target (v+ target movement)))
    (setf (camera3d-position camera) new-position)
    (setf (camera3d-target camera) new-target)
    camera))

(defun camera3d-move-right (camera distance &optional (move-in-world-plane t))
  "Move camera left/right
   When move-in-world-plane is true, movement is restricted to horizontal plane"
  (let* ((pos (camera3d-position camera))
         (target (camera3d-target camera))
         (up (camera3d-up camera))
         ;; Calculate right direction using 3d-math operations
         (forward (v- target pos))
         (forward-for-right (if move-in-world-plane
                              (let ((f-copy (vcopy forward)))
                                (setf (vy f-copy) 0.0)
                                f-copy)
                              forward))
         (right (vunit (vc (vunit forward-for-right) up)))
         (movement (v* right distance))
         (new-position (v+ pos movement))
         (new-target (v+ target movement)))
    (setf (camera3d-position camera) new-position)
    (setf (camera3d-target camera) new-target)
    camera))

(defun camera3d-move-up (camera distance)
  "Move camera up/down"
  (let* ((pos (camera3d-position camera))
         (target (camera3d-target camera))
         (up (camera3d-up camera))
         ;; Calculate upward movement using 3d-math operations
         (movement (v* up distance))
         (new-position (v+ pos movement))
         (new-target (v+ target movement)))
    (setf (camera3d-position camera) new-position)
    (setf (camera3d-target camera) new-target)
    camera))

(defun camera3d-move-to-target (camera delta)
  "Move camera position closer/farther to/from the camera target - raylib style"
  (let* ((pos (camera3d-position camera))
         (target (camera3d-target camera))
         (distance (+ (vlength (v- pos target)) delta)))
    ;; Distance must be greater than 0
    (when (<= distance 0) (setf distance 0.001))
    ;; Set new distance by moving the position along the forward vector
    (let* ((forward (vunit (v- target pos)))
           (new-position (v+ target (v* forward (- distance)))))
      (setf (camera3d-position camera) new-position))
    camera))

(defun camera3d-rotate-yaw (camera angle rotate-around-target)
  "Rotate camera around its up vector (yaw) - raylib style"
  (let* ((position (camera3d-position camera))
         (target (camera3d-target camera))
         (up (camera3d-up camera))
         ;; View vector from position to target
         (target-position (v- target position)))
    
    ;; Rotate view vector around up axis using 3d-math vrot
    (let ((rotated-target-pos (vrot target-position up angle)))
      (if rotate-around-target
        ;; Move position relative to target
        (setf (camera3d-position camera) (v- target rotated-target-pos))
        ;; Move target relative to position
        (setf (camera3d-target camera) (v+ position rotated-target-pos))))
    camera))

(defun camera3d-rotate-pitch (camera angle lock-view rotate-around-target rotate-up)
  "Rotate camera around its right vector (pitch) - raylib style"
  (let* ((position (camera3d-position camera))
         (target (camera3d-target camera))
         (up (camera3d-up camera))
         ;; View vector from position to target
         (target-position (v- target position)))

    ;; Optional view locking to prevent over-rotation (matches C implementation)
    (when lock-view
      ;; Clamp view up: calculate angle between up and target-position
      (let* ((max-angle-up (acos (max -1.0 (min 1.0 (v. (vunit target-position) (vunit up))))))
             (max-angle-up-clamped (- max-angle-up 0.001)))
        (when (> angle max-angle-up-clamped)
          (setf angle max-angle-up-clamped)))

      ;; Clamp view down: calculate angle between -up and target-position
      (let* ((neg-up (v* up -1.0))
             (max-angle-down (acos (max -1.0 (min 1.0 (v. (vunit target-position) (vunit neg-up))))))
             (max-angle-down-clamped (+ (* max-angle-down -1.0) 0.001))) ; negative angle
        (when (< angle max-angle-down-clamped)
          (setf angle max-angle-down-clamped))))

    ;; Get right vector for rotation axis
    (let* ((forward (vunit target-position))
           (right (vunit (vc forward up)))
           ;; Rotate view vector around right axis
           (rotated-target-pos (vrot target-position right angle)))

      (if rotate-around-target
        ;; Move position relative to target
        (setf (camera3d-position camera) (v- target rotated-target-pos))
        ;; Move target relative to position
        (setf (camera3d-target camera) (v+ position rotated-target-pos)))

      ;; Optionally rotate up vector as well (for FREE camera)
      (when rotate-up
        (setf (camera3d-up camera) (vrot up right angle))))

    camera))

(defun camera3d-rotate-roll (camera angle)
  "Rotate camera around local Z axis (roll)"
  (let* ((up (camera3d-up camera))
         (rotation-matrix (mrotation +vz3+ angle))
         (new-up (m* rotation-matrix up)))
    (setf (camera3d-up camera) new-up)
    camera))

;;; Camera update functions for different modes

(defun set-camera-mode (camera mode)
  "Set camera behavior mode"
  (setf *camera-mode* mode)
  (setf *current-camera* camera))

(defun update-camera (camera mode)
  "Update camera based on specified mode and input (raylib compatible) - matches C implementation"
  ;; Convert keyword mode to constant if needed
  (let* ((actual-mode (keyword-to-camera-mode mode))
         (mouse-position-delta (get-mouse-delta))
         ;; Mode flags - matching C version logic
         (move-in-world-plane (or (= actual-mode +camera-first-person+)
                                   (= actual-mode +camera-third-person+)))
         (rotate-around-target (or (= actual-mode +camera-third-person+)
                                    (= actual-mode +camera-orbital+)))
         (lock-view (or (= actual-mode +camera-free+)
                        (= actual-mode +camera-first-person+)
                        (= actual-mode +camera-third-person+)
                        (= actual-mode +camera-orbital+)))
         (rotate-up nil)
         ;; Camera speeds based on frame time
         (frame-time (get-frame-time))
         (camera-move-speed (* 5.4 frame-time))        ; CAMERA_MOVE_SPEED
         (camera-rotation-speed (* 0.03 frame-time))   ; CAMERA_ROTATION_SPEED
         (camera-pan-speed (* 0.2 frame-time))         ; CAMERA_PAN_SPEED
         (camera-orbital-speed (* 0.5 frame-time))     ; CAMERA_ORBITAL_SPEED
         (mouse-sensitivity 0.003))                    ; CAMERA_MOUSE_MOVE_SENSITIVITY

    (setf *camera-mode* actual-mode)
    (setf *current-camera* camera)

    (cond
      ;; Custom mode - no automatic updates
      ((= actual-mode +camera-custom+) nil)

      ;; Orbital mode - just orbit
      ((= actual-mode +camera-orbital+)
       (let* ((up (camera3d-get-up camera))
              (rotation (mrotation up camera-orbital-speed))
              (view (v- (camera3d-position camera) (camera3d-target camera)))
              (new-view (m* rotation view)))
         (setf (camera3d-position camera) (v+ (camera3d-target camera) new-view))))

      ;; All other modes (FREE, FIRST_PERSON, THIRD_PERSON)
      (t
       ;; Camera rotation with arrow keys
       (when (is-key-down :key-down)
         (camera3d-rotate-pitch camera (- camera-rotation-speed) lock-view rotate-around-target rotate-up))
       (when (is-key-down :key-up)
         (camera3d-rotate-pitch camera camera-rotation-speed lock-view rotate-around-target rotate-up))
       (when (is-key-down :key-right)
         (camera3d-rotate-yaw camera (- camera-rotation-speed) rotate-around-target))
       (when (is-key-down :key-left)
         (camera3d-rotate-yaw camera camera-rotation-speed rotate-around-target))
       (when (is-key-down :key-q)
         (camera3d-rotate-roll camera (- camera-rotation-speed)))
       (when (is-key-down :key-e)
         (camera3d-rotate-roll camera camera-rotation-speed))

       ;; Camera pan (for CAMERA_FREE with middle mouse button)
       (if (and (= actual-mode +camera-free+) (is-mouse-button-down :mouse-button-middle))
         (progn
           (when (> (vx mouse-position-delta) 0.0)
             (camera3d-move-right camera camera-pan-speed move-in-world-plane))
           (when (< (vx mouse-position-delta) 0.0)
             (camera3d-move-right camera (- camera-pan-speed) move-in-world-plane))
           (when (> (vy mouse-position-delta) 0.0)
             (camera3d-move-up camera (- camera-pan-speed)))
           (when (< (vy mouse-position-delta) 0.0)
             (camera3d-move-up camera camera-pan-speed)))
         ;; Mouse support - rotation (only if mouse moved)
         (progn
           (when (/= (vx mouse-position-delta) 0.0)
             (camera3d-rotate-yaw camera (* (- (vx mouse-position-delta)) mouse-sensitivity) rotate-around-target))
           (when (/= (vy mouse-position-delta) 0.0)
             (camera3d-rotate-pitch camera (* (- (vy mouse-position-delta)) mouse-sensitivity) lock-view rotate-around-target rotate-up))))

       ;; Keyboard support - WASD movement (common to all non-orbital modes)
       (when (is-key-down :key-w)
         (camera3d-move-forward camera camera-move-speed move-in-world-plane))
       (when (is-key-down :key-a)
         (camera3d-move-right camera (- camera-move-speed) move-in-world-plane))
       (when (is-key-down :key-s)
         (camera3d-move-forward camera (- camera-move-speed) move-in-world-plane))
       (when (is-key-down :key-d)
         (camera3d-move-right camera camera-move-speed move-in-world-plane))

       ;; Vertical movement (only for FREE mode)
       (when (= actual-mode +camera-free+)
         (when (is-key-down :key-space)
           (camera3d-move-up camera camera-move-speed))
         (when (is-key-down :key-left-control)
           (camera3d-move-up camera (- camera-move-speed))))))

    ;; Mouse wheel zoom (for THIRD_PERSON, ORBITAL, FREE)
    (when (or (= actual-mode +camera-third-person+)
              (= actual-mode +camera-orbital+)
              (= actual-mode +camera-free+))
      (let ((wheel-move (get-mouse-wheel-move)))
        (camera3d-move-to-target camera (- wheel-move))))

    camera))

(defun keyword-to-camera-mode (mode-keyword)
  "Convert keyword camera mode to constant"
  (case mode-keyword
    (:camera-free +camera-free+)
    (:camera-orbital +camera-orbital+)
    (:camera-first-person +camera-first-person+)
    (:camera-third-person +camera-third-person+)
    (:camera-custom +camera-custom+)
    (t (if (numberp mode-keyword) mode-keyword mode-keyword))))

(defun update-camera-free (camera)
  "Update free camera (WASD movement, mouse look) - based on raylib implementation"
  (let* ((frame-time (get-frame-time))
         (camera-move-speed (* 5.4 frame-time))     ; Units per second * frame time
         (camera-pan-speed (* 0.2 frame-time))
         (mouse-sensitivity 0.003))
    
    ;; Get mouse movement delta
    (let ((mouse-delta (get-mouse-delta)))
      ;; Camera movement
      ;; Camera pan (middle mouse button for FREE mode)
      (if (is-mouse-button-down :mouse-button-middle)
        ;; Pan mode - move camera laterally
        (progn
          (when (> (first mouse-delta) 0.0)
            (camera3d-move-right camera camera-pan-speed))
          (when (< (first mouse-delta) 0.0)
            (camera3d-move-right camera (- camera-pan-speed)))
          (when (> (second mouse-delta) 0.0)
            (camera3d-move-up camera (- camera-pan-speed)))
          (when (< (second mouse-delta) 0.0)
            (camera3d-move-up camera camera-pan-speed)))
        ;; Normal mouse look mode
        (progn
          ;; Mouse rotation (automatic in raylib FREE mode)
          (camera3d-rotate-yaw camera (* (- (first mouse-delta)) mouse-sensitivity) nil)
          (camera3d-rotate-pitch camera (* (- (second mouse-delta)) mouse-sensitivity) t nil nil))))
    
    ;; Keyboard movement (FREE mode can move in all directions, not restricted to world plane)
    (when (is-key-down :key-w)
      (camera3d-move-forward camera camera-move-speed nil))
    (when (is-key-down :key-s)
      (camera3d-move-forward camera (- camera-move-speed) nil))
    (when (is-key-down :key-a)
      (camera3d-move-right camera (- camera-move-speed) nil))
    (when (is-key-down :key-d)
      (camera3d-move-right camera camera-move-speed nil))
    
    ;; Vertical movement in FREE mode
    (when (is-key-down :key-space)
      (camera3d-move-up camera camera-move-speed))
    (when (is-key-down :key-left-control)
      (camera3d-move-up camera (- camera-move-speed)))
    
    ;; Mouse wheel zoom
    (let ((wheel-move (get-mouse-wheel-move)))
      (when (/= wheel-move 0)
        (camera3d-move-to-target camera (- wheel-move))))
    
    camera))

(defun update-camera-orbital (camera)
  "Update orbital camera (rotates around target)"
  ;; Mouse rotation around target
  (when (is-mouse-button-down :mouse-button-left)
    (let ((mouse-delta (get-mouse-delta)))
      (incf *camera-angle-x* (* (first mouse-delta) *camera-rotation-speed* -0.01))
      (incf *camera-angle-y* (* (second mouse-delta) *camera-rotation-speed* -0.01))
      
      ;; Clamp vertical angle
      (setf *camera-angle-y* (clamp *camera-angle-y* *camera-max-clamp* *camera-min-clamp*))))
  
  ;; Zoom with mouse wheel
  (let ((wheel-move (get-mouse-wheel-move)))
    (when (/= wheel-move 0)
      (setf *camera-target-distance* 
            (clamp (- *camera-target-distance* (* wheel-move *camera-smooth-zoom-speed*))
                   0.5 50.0))))
  
  ;; Calculate new camera position
  (let* ((target (camera3d-target camera))
         (cos-x (cos *camera-angle-x*))
         (sin-x (sin *camera-angle-x*))
         (cos-y (cos *camera-angle-y*))
         (sin-y (sin *camera-angle-y*))
         (x (+ (first target) (* *camera-target-distance* cos-y cos-x)))
         (y (+ (second target) (* *camera-target-distance* sin-y)))
         (z (+ (third target) (* *camera-target-distance* cos-y sin-x))))
    (setf (camera3d-position camera) (vec3 x y z)))
  
  camera)

(defun update-camera-first-person (camera)
  "Update first-person camera - based on raylib implementation"
  (let ((frame-time (get-frame-time))
        (camera-move-speed 5.4)      ; Units per second (matches raylib CAMERA_MOVE_SPEED)
        (mouse-sensitivity 0.003))   ; Mouse movement sensitivity

    ;; Calculate movement speed for this frame
    (let ((move-speed (* camera-move-speed frame-time)))
      ;; Movement with WASD - move in world plane (horizontal movement only)
      (when (is-key-down :key-w)
        (camera3d-move-forward camera move-speed t))
      (when (is-key-down :key-s)
        (camera3d-move-forward camera (- move-speed) t))
      (when (is-key-down :key-d)
        (camera3d-move-right camera move-speed t))
      (when (is-key-down :key-a)
        (camera3d-move-right camera (- move-speed) t)))

    ;; Mouse look (always active in first person)
    (let ((mouse-delta (get-mouse-delta)))
      (when (/= (first mouse-delta) 0.0)
        (camera3d-rotate-yaw camera (* (- (first mouse-delta)) mouse-sensitivity) nil))
      (when (/= (second mouse-delta) 0.0)
        (camera3d-rotate-pitch camera (* (- (second mouse-delta)) mouse-sensitivity) t nil nil))))

  camera)

(defun update-camera-third-person (camera)
  "Update third-person camera - based on raylib implementation"
  (let ((frame-time (get-frame-time))
        (camera-move-speed 5.4)
        (mouse-sensitivity 0.003))

    (let ((move-speed (* camera-move-speed frame-time)))
      ;; Move target based on WASD input (in world plane)
      (when (is-key-down :key-w)
        (let* ((forward (camera3d-get-forward camera))
               (forward-horizontal (vec3 (vx forward) 0.0 (vz forward)))
               (movement (v* (vunit forward-horizontal) move-speed)))
          (setf (camera3d-target camera) (v+ (camera3d-target camera) movement))))
      (when (is-key-down :key-s)
        (let* ((forward (camera3d-get-forward camera))
               (forward-horizontal (vec3 (vx forward) 0.0 (vz forward)))
               (movement (v* (vunit forward-horizontal) (- move-speed))))
          (setf (camera3d-target camera) (v+ (camera3d-target camera) movement))))
      (when (is-key-down :key-d)
        (let* ((right (camera3d-get-right camera))
               (movement (v* (vunit right) move-speed)))
          (setf (camera3d-target camera) (v+ (camera3d-target camera) movement))))
      (when (is-key-down :key-a)
        (let* ((right (camera3d-get-right camera))
               (movement (v* (vunit right) (- move-speed))))
          (setf (camera3d-target camera) (v+ (camera3d-target camera) movement)))))

    ;; Mouse rotation
    (let ((mouse-delta (get-mouse-delta)))
      (when (/= (vx mouse-delta) 0.0)
        (camera3d-rotate-yaw camera (* (- (vx mouse-delta)) mouse-sensitivity) t))
      (when (/= (vy mouse-delta) 0.0)
        (camera3d-rotate-pitch camera (* (- (vy mouse-delta)) mouse-sensitivity) t t nil)))

    ;; Update camera position to maintain distance from target
    (let* ((direction (vunit (v- (camera3d-position camera)
                                 (camera3d-target camera))))
           (new-position (v+ (camera3d-target camera)
                             (v* direction *camera-target-distance*))))
      (setf (camera3d-position camera) new-position)))

  camera)

(defun get-camera-ray (camera direction)
  "Get ray from camera in specified direction"
  (make-ray :position (camera3d-position camera)
            :direction (vunit direction)))

;;; Utility functions

(defun camera3d-set-position (camera position)
  "Set camera position"
  (setf (camera3d-position camera) position)
  camera)

(defun camera3d-set-target (camera target)
  "Set camera target"
  (setf (camera3d-target camera) target)
  camera)

(defun camera3d-set-up (camera up)
  "Set camera up vector"
  (setf (camera3d-up camera) up)
  camera)

(defun camera3d-set-fovy (camera fovy)
  "Set camera field of view Y"
  (setf (camera3d-fovy camera) fovy)
  camera)

;;; World to screen coordinate conversion

(defun camera3d-get-view-ray (camera x y)
  "Get view ray for screen coordinates"
  (get-screen-to-world-ray (vec2 x y) camera))

