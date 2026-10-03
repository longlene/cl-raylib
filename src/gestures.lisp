(in-package #:cl-raylib)

;;;===================================================================================
;;; rgestures - Gestures system, gestures processing based on input events (touch/mouse)
;;; Port of raylib/src/rgestures.h (included by rcore.c, SUPPORT_GESTURES_SYSTEM)
;;;===================================================================================

;;; Touch action (TouchAction enum)
(defconstant +touch-action-up+ 0)
(defconstant +touch-action-down+ 1)
(defconstant +touch-action-move+ 2)
(defconstant +touch-action-cancel+ 3)

;;; Gesture event
(defstruct gesture-event
  (touch-action 0 :type fixnum)
  (point-count 0 :type fixnum)
  (point-id (make-array 8 :initial-element 0) :type simple-vector)                 ; MAX_TOUCH_POINTS
  (position (make-array 8 :initial-element (vec2 0.0 0.0)) :type simple-vector))   ; Vector2[MAX_TOUCH_POINTS]

;;; Defines and Macros
(defconstant +force-to-swipe+ 0.2 "Swipe force, measured in normalized screen units/time")
(defconstant +minimum-drag+ 0.015 "Drag minimum force, measured in normalized screen units (0.0f to 1.0f)")
(defconstant +drag-timeout+ 0.3 "Drag minimum time for web, measured in seconds")
(defconstant +minimum-pinch+ 0.005 "Pinch minimum force, measured in normalized screen units (0.0f to 1.0f)")
(defconstant +tap-timeout+ 0.3 "Tap minimum time, measured in seconds")
(defconstant +pinch-timeout+ 0.3 "Pinch minimum time, measured in seconds")
(defconstant +doubletap-range+ 0.03 "DoubleTap range, measured in normalized screen units (0.0f to 1.0f)")

;;; Gestures module state context
(defstruct gestures-data
  (current 0 :type fixnum)                      ; Current detected gesture
  (enabled-flags #b0000001111111111 :type fixnum) ; Enabled gestures flags (all supported by default)
  ;; Touch
  (touch-first-id -1 :type fixnum)              ; Touch id for first touch point
  (touch-point-count 0 :type fixnum)            ; Touch points counter
  (touch-event-time 0.0d0 :type double-float)   ; Time stamp when an event happened
  (touch-up-position (vec2 0.0 0.0))            ; Touch up position
  (touch-down-position-a (vec2 0.0 0.0))        ; First touch down position
  (touch-down-position-b (vec2 0.0 0.0))        ; Second touch down position
  (touch-down-drag-position (vec2 0.0 0.0))     ; Touch drag position
  (touch-move-down-position-a (vec2 0.0 0.0))   ; First touch down position on move
  (touch-move-down-position-b (vec2 0.0 0.0))   ; Second touch down position on move
  (touch-previous-position-a (vec2 0.0 0.0))    ; Previous position A to compare for pinch gestures
  (touch-previous-position-b (vec2 0.0 0.0))    ; Previous position B to compare for pinch gestures
  (touch-tap-counter 0 :type fixnum)            ; TAP counter (one tap implies TOUCH_ACTION_DOWN and TOUCH_ACTION_UP actions)
  ;; Hold
  (hold-reset-required nil)                     ; HOLD reset to get first touch point again
  (hold-time-duration 0.0d0 :type double-float) ; HOLD duration in seconds
  ;; Drag
  (drag-vector (vec2 0.0 0.0))                  ; DRAG vector (between initial and current position)
  (drag-angle 0.0 :type single-float)           ; DRAG angle (relative to x-axis)
  (drag-distance 0.0 :type single-float)        ; DRAG distance (from initial touch point to final) (normalized [0..1])
  (drag-intensity 0.0 :type single-float)       ; DRAG intensity, how far why did the DRAG (pixels per frame)
  ;; Swipe
  (swipe-start-time 0.0d0 :type double-float)   ; SWIPE start time to calculate drag intensity
  ;; Pinch
  (pinch-vector (vec2 0.0 0.0))                 ; PINCH vector (between first and second touch points)
  (pinch-angle 0.0 :type single-float)          ; PINCH angle (relative to x-axis)
  (pinch-distance 0.0 :type single-float))      ; PINCH displacement distance (normalized [0..1])

(defvar *gestures* (make-gestures-data) "Gestures module state context [136 bytes]")

(defun %v2 (v) (vec2 (vx v) (vy v)))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

(defun set-gestures-enabled (flags)
  "Enable only desired gestures to be detected"
  (setf (gestures-data-enabled-flags *gestures*) flags))

(defun is-gesture-detected (gesture)
  "Check if a gesture have been detected"
  (= (logand (gestures-data-enabled-flags *gestures*) (gestures-data-current *gestures*)) gesture))

(defun process-gesture-event (event)
  "Process gesture event and translate it into gestures"
  (let ((g *gestures*)
        (position0 (aref (gesture-event-position event) 0)))
    ;; Reset required variables
    (setf (gestures-data-touch-point-count g) (gesture-event-point-count event)) ; Required on UpdateGestures()
    (cond
      ((= (gestures-data-touch-point-count g) 1) ; One touch point
       (cond
         ((= (gesture-event-touch-action event) +touch-action-down+)
          (incf (gestures-data-touch-tap-counter g)) ; Tap counter
          ;; Detect GESTURE_DOUBLE_TAP
          (if (and (= (gestures-data-current g) +gesture-none+)
                   (>= (gestures-data-touch-tap-counter g) 2)
                   (< (- (%rg-get-current-time) (gestures-data-touch-event-time g)) +tap-timeout+)
                   (< (%rg-vector2-distance (gestures-data-touch-down-position-a g) position0) +doubletap-range+))
              (setf (gestures-data-current g) +gesture-doubletap+
                    (gestures-data-touch-tap-counter g) 0)
              ;; Detect GESTURE_TAP
              (setf (gestures-data-touch-tap-counter g) 1
                    (gestures-data-current g) +gesture-tap+))
          (setf (gestures-data-touch-down-position-a g) (%v2 position0)
                (gestures-data-touch-down-drag-position g) (%v2 position0)
                (gestures-data-touch-up-position g) (%v2 (gestures-data-touch-down-position-a g))
                (gestures-data-touch-event-time g) (%rg-get-current-time)
                (gestures-data-swipe-start-time g) (%rg-get-current-time)
                (gestures-data-drag-vector g) (vec2 0.0 0.0)))
         ((= (gesture-event-touch-action event) +touch-action-up+)
          ;; A swipe can happen while the current gesture is drag, but (specially for web) also hold, so set upPosition for both cases
          (when (or (= (gestures-data-current g) +gesture-drag+) (= (gestures-data-current g) +gesture-hold+))
            (setf (gestures-data-touch-up-position g) (%v2 position0)))
          ;; NOTE: GESTURES.Drag.intensity dependent on the resolution of the screen
          (setf (gestures-data-drag-distance g)
                (%rg-vector2-distance (gestures-data-touch-down-position-a g) (gestures-data-touch-up-position g)))
          (setf (gestures-data-drag-intensity g)
                (/ (gestures-data-drag-distance g)
                   (float (- (%rg-get-current-time) (gestures-data-swipe-start-time g)) 1.0)))
          ;; Detect GESTURE_SWIPE
          (if (and (> (gestures-data-drag-intensity g) +force-to-swipe+)
                   (/= (gestures-data-current g) +gesture-drag+))
              (let ((angle (- 360.0 (%rg-vector2-angle (gestures-data-touch-down-position-a g)
                                                       (gestures-data-touch-up-position g)))))
                ;; NOTE: Angle should be inverted in Y
                (setf (gestures-data-drag-angle g) angle)
                (setf (gestures-data-current g)
                      (cond ((or (< angle 30) (> angle 330)) +gesture-swipe-right+)          ; Right
                            ((and (>= angle 30) (<= angle 150)) +gesture-swipe-up+)        ; Up
                            ((and (> angle 150) (< angle 210)) +gesture-swipe-left+)       ; Left
                            ((and (>= angle 210) (<= angle 330)) +gesture-swipe-down+)     ; Down
                            (t +gesture-none+))))
              (setf (gestures-data-drag-distance g) 0.0
                    (gestures-data-drag-intensity g) 0.0
                    (gestures-data-drag-angle g) 0.0
                    (gestures-data-current g) +gesture-none+))
          (setf (gestures-data-touch-down-drag-position g) (vec2 0.0 0.0)
                (gestures-data-touch-point-count g) 0))
         ((= (gesture-event-touch-action event) +touch-action-move+)
          (setf (gestures-data-touch-move-down-position-a g) (%v2 position0))
          (when (= (gestures-data-current g) +gesture-hold+)
            (when (gestures-data-hold-reset-required g)
              (setf (gestures-data-touch-down-position-a g) (%v2 position0)))
            (setf (gestures-data-hold-reset-required g) nil)
            ;; Detect GESTURE_DRAG
            (when (> (- (%rg-get-current-time) (gestures-data-touch-event-time g)) +drag-timeout+)
              (setf (gestures-data-touch-event-time g) (%rg-get-current-time)
                    (gestures-data-current g) +gesture-drag+)))
          (setf (gestures-data-drag-vector g)
                (vec2 (- (vx (gestures-data-touch-move-down-position-a g)) (vx (gestures-data-touch-down-drag-position g)))
                      (- (vy (gestures-data-touch-move-down-position-a g)) (vy (gestures-data-touch-down-drag-position g))))))))
      ((= (gestures-data-touch-point-count g) 2) ; Two touch points
       (let ((position1 (aref (gesture-event-position event) 1)))
         (cond
           ((= (gesture-event-touch-action event) +touch-action-down+)
            (setf (gestures-data-touch-down-position-a g) (%v2 position0)
                  (gestures-data-touch-down-position-b g) (%v2 position1)
                  (gestures-data-touch-previous-position-a g) (%v2 position0)
                  (gestures-data-touch-previous-position-b g) (%v2 position1)
                  (gestures-data-pinch-vector g) (vec2 (- (vx position1) (vx position0))
                                                       (- (vy position1) (vy position0)))
                  (gestures-data-current g) +gesture-hold+
                  (gestures-data-hold-time-duration g) (%rg-get-current-time)))
           ((= (gesture-event-touch-action event) +touch-action-move+)
            (setf (gestures-data-pinch-distance g)
                  (%rg-vector2-distance (gestures-data-touch-move-down-position-a g)
                                        (gestures-data-touch-move-down-position-b g)))
            (setf (gestures-data-touch-move-down-position-a g) (%v2 position0)
                  (gestures-data-touch-move-down-position-b g) (%v2 position1))
            (setf (gestures-data-pinch-vector g)
                  (vec2 (- (vx position1) (vx position0)) (- (vy position1) (vy position0))))
            (if (or (>= (%rg-vector2-distance (gestures-data-touch-previous-position-a g)
                                              (gestures-data-touch-move-down-position-a g))
                        +minimum-pinch+)
                    (>= (%rg-vector2-distance (gestures-data-touch-previous-position-b g)
                                              (gestures-data-touch-move-down-position-b g))
                        +minimum-pinch+))
                (setf (gestures-data-current g)
                      (if (> (%rg-vector2-distance (gestures-data-touch-previous-position-a g)
                                                   (gestures-data-touch-previous-position-b g))
                             (%rg-vector2-distance (gestures-data-touch-move-down-position-a g)
                                                   (gestures-data-touch-move-down-position-b g)))
                          +gesture-pinch-in+
                          +gesture-pinch-out+))
                (setf (gestures-data-current g) +gesture-hold+
                      (gestures-data-hold-time-duration g) (%rg-get-current-time)))
            ;; NOTE: Angle should be inverted in Y
            (setf (gestures-data-pinch-angle g)
                  (- 360.0 (%rg-vector2-angle (gestures-data-touch-move-down-position-a g)
                                              (gestures-data-touch-move-down-position-b g)))))
           ((= (gesture-event-touch-action event) +touch-action-up+)
            (setf (gestures-data-pinch-distance g) 0.0
                  (gestures-data-pinch-angle g) 0.0
                  (gestures-data-pinch-vector g) (vec2 0.0 0.0)
                  (gestures-data-touch-point-count g) 0
                  (gestures-data-current g) +gesture-none+)))))
      ;; More than two touch points: TODO in raylib
      )))

(defun update-gestures ()
  "Update gestures detected (must be called every frame)"
  ;; NOTE: Gestures are processed through system callbacks on touch events
  (let ((g *gestures*))
    ;; Detect GESTURE_HOLD
    (when (and (or (= (gestures-data-current g) +gesture-tap+) (= (gestures-data-current g) +gesture-doubletap+))
               (< (gestures-data-touch-point-count g) 2))
      (setf (gestures-data-current g) +gesture-hold+
            (gestures-data-hold-time-duration g) (%rg-get-current-time)))
    ;; Detect GESTURE_NONE
    (when (or (= (gestures-data-current g) +gesture-swipe-right+) (= (gestures-data-current g) +gesture-swipe-up+)
              (= (gestures-data-current g) +gesture-swipe-left+) (= (gestures-data-current g) +gesture-swipe-down+))
      (setf (gestures-data-current g) +gesture-none+))))

(defun get-gesture-detected ()
  "Get latest detected gesture"
  ;; Get current gesture only if enabled
  (logand (gestures-data-enabled-flags *gestures*) (gestures-data-current *gestures*)))

(defun get-gesture-hold-duration ()
  "Hold time measured in seconds"
  ;; NOTE: time is calculated on current gesture HOLD
  (let ((time 0.0d0))
    (when (= (gestures-data-current *gestures*) +gesture-hold+)
      (setf time (- (%rg-get-current-time) (gestures-data-hold-time-duration *gestures*))))
    (float time 1.0)))

(defun get-gesture-drag-vector ()
  "Get drag vector (between initial touch point to current)"
  ;; NOTE: drag vector is calculated on one touch points TOUCH_ACTION_MOVE
  (%v2 (gestures-data-drag-vector *gestures*)))

(defun get-gesture-drag-angle ()
  "Get drag angle"
  ;; NOTE: drag angle is calculated on one touch points TOUCH_ACTION_UP
  (gestures-data-drag-angle *gestures*))

(defun get-gesture-pinch-vector ()
  "Get distance between two pinch points"
  ;; NOTE: Pinch distance is calculated on two touch points TOUCH_ACTION_MOVE
  (%v2 (gestures-data-pinch-vector *gestures*)))

(defun get-gesture-pinch-angle ()
  "Get angle between two pinch points"
  ;; NOTE: pinch angle is calculated on two touch points TOUCH_ACTION_MOVE
  (gestures-data-pinch-angle *gestures*))

;;;----------------------------------------------------------------------------------
;;; Module specific Functions Definition
;;;----------------------------------------------------------------------------------

(defun %rg-vector2-angle (v1 v2)
  "Get angle from two-points vector with X-axis"
  (let ((angle (* (%atan2f (- (vy v2) (vy v1)) (- (vx v2) (vx v1))) (/ 180.0 +pi+))))
    (when (< angle 0) (incf angle 360.0))
    angle))

(defun %rg-vector2-distance (v1 v2)
  "Calculate distance between two Vector2"
  (let ((dx (- (vx v2) (vx v1)))
        (dy (- (vy v2) (vy v1))))
    (float (sqrt (+ (* dx dx) (* dy dy))) 1.0)))

(defun %rg-get-current-time ()
  "Time measure returned are seconds"
  (get-time))
