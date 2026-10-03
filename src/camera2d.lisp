(in-package #:cl-raylib)

;;; 2D Camera System
;;; This module provides 2D camera functionality for viewport transformations
;;; Based on raylib's Camera2D structure and functions

;;; Global camera state
(defvar *current-camera2d* nil "Currently active 2D camera")

;;; Camera2D creation functions

(defun make-camera-2d (offset target rotation zoom)
  "Create a 2D camera with specified parameters"
  (make-camera2d :offset offset
                 :target target
                 :rotation rotation
                 :zoom zoom))

(defun camera2d-default ()
  "Create default 2D camera"
  (make-camera2d :offset (vec2 0.0 0.0)
                 :target (vec2 0.0 0.0)
                 :rotation 0.0
                 :zoom 1.0))

;;; Camera2D matrix functions

;;; Screen to world coordinate conversion

;;; Camera2D manipulation functions

(defun camera2d-set-offset (camera offset)
  "Set camera offset"
  (setf (camera2d-offset camera) offset)
  camera)

(defun camera2d-set-target (camera target)
  "Set camera target"
  (setf (camera2d-target camera) target)
  camera)

(defun camera2d-set-rotation (camera rotation)
  "Set camera rotation in degrees"
  (setf (camera2d-rotation camera) rotation)
  camera)

(defun camera2d-set-zoom (camera zoom)
  "Set camera zoom level"
  (setf (camera2d-zoom camera) (max zoom 0.1)) ; Prevent zero/negative zoom
  camera)

;;; Camera2D movement functions

(defun camera2d-move (camera delta)
  "Move camera by delta amount"
  (let* ((current-target (camera2d-target camera))
         (new-target (v+ current-target delta)))
    (setf (camera2d-target camera) new-target)
    camera))

(defun camera2d-rotate (camera delta-rotation)
  "Rotate camera by delta amount (in degrees)"
  (let ((new-rotation (+ (camera2d-rotation camera) delta-rotation)))
    (setf (camera2d-rotation camera) new-rotation)
    camera))

(defun camera2d-zoom-by (camera zoom-factor)
  "Zoom camera by factor (multiply current zoom)"
  (let ((new-zoom (* (camera2d-zoom camera) zoom-factor)))
    (setf (camera2d-zoom camera) (max new-zoom 0.1)) ; Prevent zero/negative zoom
    camera))

(defun camera2d-zoom-to (camera zoom-level)
  "Set camera zoom to specific level"
  (setf (camera2d-zoom camera) (max zoom-level 0.1)) ; Prevent zero/negative zoom
  camera)

;;; 2D Drawing setup functions

;;; Camera2D utility functions

(defun camera2d-get-view-rectangle (camera)
  "Get the rectangle representing the camera's view area in world coordinates"
  (let* ((screen-width (get-screen-width))
         (screen-height (get-screen-height))
         ;; Get world coordinates of screen corners
         (top-left (get-screen-to-world-2d (vec2 0.0 0.0) camera))
         (bottom-right (get-screen-to-world-2d (vec2 screen-width screen-height) camera))
         (width (- (vx2 bottom-right) (vx2 top-left)))
         (height (- (vy2 bottom-right) (vy2 top-left))))
    
    (make-rectangle :x (vx2 top-left)
                    :y (vy2 top-left)
                    :width width
                    :height height)))

(defun camera2d-follow-target (camera target smooth-factor)
  "Make camera smoothly follow a target position"
  (let* ((current-target (camera2d-target camera))
         (delta (v- target current-target))
         (smooth-delta (v* delta smooth-factor)))
    (camera2d-move camera smooth-delta)))

(defun camera2d-constrain-to-bounds (camera bounds)
  "Constrain camera target to stay within specified bounds rectangle"
  (let* ((view-rect (camera2d-get-view-rectangle camera))
         (view-width (rectangle-width view-rect))
         (view-height (rectangle-height view-rect))
         (current-target (camera2d-target camera))
         (target-x (vx2 current-target))
         (target-y (vy2 current-target))
         
         ;; Calculate constrained position
         (min-x (+ (rectangle-x bounds) (/ view-width 2)))
         (max-x (- (+ (rectangle-x bounds) (rectangle-width bounds)) (/ view-width 2)))
         (min-y (+ (rectangle-y bounds) (/ view-height 2)))
         (max-y (- (+ (rectangle-y bounds) (rectangle-height bounds)) (/ view-height 2)))
         
         (constrained-x (clamp target-x min-x max-x))
         (constrained-y (clamp target-y min-y max-y)))
    
    (camera2d-set-target camera (vec2 constrained-x constrained-y))))

;;; Camera2D lerp and animation functions

(defun camera2d-lerp (camera1 camera2 factor)
  "Linear interpolation between two cameras"
  (let* ((offset1 (camera2d-offset camera1))
         (offset2 (camera2d-offset camera2))
         (target1 (camera2d-target camera1))
         (target2 (camera2d-target camera2))
         (rotation1 (camera2d-rotation camera1))
         (rotation2 (camera2d-rotation camera2))
         (zoom1 (camera2d-zoom camera1))
         (zoom2 (camera2d-zoom camera2))
         
         (lerped-offset (vlerp offset1 offset2 factor))
         (lerped-target (vlerp target1 target2 factor))
         (lerped-rotation (lerp rotation1 rotation2 factor))
         (lerped-zoom (lerp zoom1 zoom2 factor)))
    
    (make-camera2d :offset lerped-offset
                   :target lerped-target
                   :rotation lerped-rotation
                   :zoom lerped-zoom)))

(defun camera2d-animate-to (camera target-camera duration)
  "Animate camera towards target camera over duration (returns function for updating)"
  (let ((start-time (get-time))
        (start-camera (copy-structure camera)))
    (lambda ()
      (let* ((elapsed (- (get-time) start-time))
             (t-value (clamp (/ elapsed duration) 0.0 1.0))
             (lerped-camera (camera2d-lerp start-camera target-camera t-value)))
        
        ;; Update current camera
        (setf (camera2d-offset camera) (camera2d-offset lerped-camera))
        (setf (camera2d-target camera) (camera2d-target lerped-camera))
        (setf (camera2d-rotation camera) (camera2d-rotation lerped-camera))
        (setf (camera2d-zoom camera) (camera2d-zoom lerped-camera))
        
        ;; Return whether animation is complete
        (>= t-value 1.0)))))

;;; Camera2D input handling helpers

(defun camera2d-handle-pan-input (camera mouse-sensitivity)
  "Handle mouse pan input for camera movement"
  (when (is-mouse-button-down +mouse-button-middle+)
    (let ((mouse-delta (get-mouse-delta)))
      (when (not (and (zerop (vx2 mouse-delta)) (zerop (vy2 mouse-delta))))
        ;; Convert mouse delta to world space movement
        (let* ((scaled-delta (v* mouse-delta (/ -1.0 (camera2d-zoom camera))))
               (movement (v* scaled-delta mouse-sensitivity)))
          (camera2d-move camera movement))))))

(defun camera2d-handle-zoom-input (camera zoom-sensitivity)
  "Handle mouse wheel zoom input for camera"
  (let ((wheel-move (get-mouse-wheel-move)))
    (when (/= wheel-move 0)
      (let ((zoom-factor (if (> wheel-move 0)
                           (+ 1.0 zoom-sensitivity)
                           (- 1.0 zoom-sensitivity))))
        (camera2d-zoom-by camera zoom-factor)))))

(defun camera2d-handle-rotation-input (camera rotation-sensitivity)
  "Handle keyboard rotation input for camera"
  (let ((rotation-delta 0.0))
    (when (is-key-down +key-q+)
      (incf rotation-delta rotation-sensitivity))
    (when (is-key-down +key-e+)
      (decf rotation-delta rotation-sensitivity))
    
    (when (/= rotation-delta 0.0)
      (camera2d-rotate camera rotation-delta))))

;;; Camera2D presets and utilities

(defun camera2d-fit-to-bounds (camera bounds margin)
  "Adjust camera zoom and position to fit bounds with margin"
  (let* ((screen-width (get-screen-width))
         (screen-height (get-screen-height))
         (bounds-width (rectangle-width bounds))
         (bounds-height (rectangle-height bounds))
         (bounds-center (vec2 (+ (rectangle-x bounds) (/ bounds-width 2))
                             (+ (rectangle-y bounds) (/ bounds-height 2))))
         
         ;; Calculate zoom to fit bounds with margin
         (zoom-x (/ screen-width (* bounds-width (+ 1.0 margin))))
         (zoom-y (/ screen-height (* bounds-height (+ 1.0 margin))))
         (zoom (min zoom-x zoom-y)))
    
    (camera2d-set-target camera bounds-center)
    (camera2d-set-zoom camera zoom)
    camera))

(defun camera2d-is-point-visible (camera point)
  "Check if a world point is visible in the camera view"
  (let* ((screen-pos (get-world-to-screen-2d point camera))
         (screen-x (vx2 screen-pos))
         (screen-y (vy2 screen-pos)))
    (and (>= screen-x 0)
         (<= screen-x (get-screen-width))
         (>= screen-y 0)
         (<= screen-y (get-screen-height)))))

(defun camera2d-is-rectangle-visible (camera rect)
  "Check if a world rectangle intersects with camera view"
  (let ((view-rect (camera2d-get-view-rectangle camera)))
    (check-collision-recs rect view-rect)))

;;; Camera2D debugging and info

(defun camera2d-get-info (camera)
  "Get camera information as formatted string"
  (format nil "Camera2D: Target(~,2f, ~,2f) Offset(~,2f, ~,2f) Rotation(~,2f°) Zoom(~,2fx)"
          (vx2 (camera2d-target camera))
          (vy2 (camera2d-target camera))
          (vx2 (camera2d-offset camera))
          (vy2 (camera2d-offset camera))
          (camera2d-rotation camera)
          (camera2d-zoom camera)))

(defun camera2d-draw-debug-info (camera x y font-size color)
  "Draw camera debug information on screen"
  (let ((info-text (camera2d-get-info camera)))
    (draw-text info-text x y font-size color)))

