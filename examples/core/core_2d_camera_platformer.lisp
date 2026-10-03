;;;; raylib [core] example - 2d camera platformer
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.0
;;;;
;;;; Example contributed by arvyy (@arvyy) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2019-2025 arvyy (@arvyy)
;;;; Common Lisp port of raylib/examples/core/core_2d_camera_platformer.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-2d-camera-platformer
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-2d-camera-platformer)

(defconstant +g+ 400)
(defconstant +player-jump-spd+ 350.0)
(defconstant +player-hor-spd+ 200.0)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct player
  (position (vec2 0.0 0.0))
  (speed 0.0)
  (can-jump nil))

(defstruct env-item
  rect
  blocking
  color)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 2d camera platformer")

    (let* ((player (make-player :position (vec2 400.0 280.0) :speed 0.0 :can-jump nil))
           (env-items (vector (make-env-item :rect (make-rectangle :x 0.0 :y 0.0 :width 1000.0 :height 400.0) :blocking 0 :color +lightgray+)
                              (make-env-item :rect (make-rectangle :x 0.0 :y 400.0 :width 1000.0 :height 200.0) :blocking 1 :color +gray+)
                              (make-env-item :rect (make-rectangle :x 300.0 :y 200.0 :width 400.0 :height 10.0) :blocking 1 :color +gray+)
                              (make-env-item :rect (make-rectangle :x 250.0 :y 300.0 :width 100.0 :height 10.0) :blocking 1 :color +gray+)
                              (make-env-item :rect (make-rectangle :x 650.0 :y 300.0 :width 100.0 :height 10.0) :blocking 1 :color +gray+)))
           (camera (make-camera2d :target (vcopy (player-position player))
                                  :offset (vec2 (/ screen-width 2.0) (/ screen-height 2.0))
                                  :rotation 0.0
                                  :zoom 1.0))
           ;; Store the multiple update camera functions
           (camera-updaters (vector #'update-camera-center
                                    #'update-camera-center-inside-map
                                    #'update-camera-center-smooth-follow
                                    #'update-camera-even-out-on-landing
                                    #'update-camera-player-bounds-push))
           (camera-option 0)
           (camera-descriptions (vector "Follow player center"
                                        "Follow player center, but clamp to map edges"
                                        "Follow player center; smoothed"
                                        "Follow player center horizontally; update player center vertically after landing"
                                        "Player push camera on getting too close to screen edge")))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((delta-time (get-frame-time)))

                 (update-player player env-items delta-time)

                 (incf (camera2d-zoom camera) (* (get-mouse-wheel-move) 0.05))

                 (cond ((> (camera2d-zoom camera) 3.0) (setf (camera2d-zoom camera) 3.0))
                       ((< (camera2d-zoom camera) 0.25) (setf (camera2d-zoom camera) 0.25)))

                 (when (is-key-pressed +key-r+)
                   (setf (camera2d-zoom camera) 1.0
                         (player-position player) (vec2 400.0 280.0)))

                 (when (is-key-pressed +key-c+)
                   (setf camera-option (mod (1+ camera-option) (length camera-updaters))))

                 ;; Call update camera function
                 (funcall (aref camera-updaters camera-option)
                          camera player env-items delta-time screen-width screen-height))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +lightgray+)

               (begin-mode-2d camera)

               (loop for ei across env-items do (draw-rectangle-rec (env-item-rect ei) (env-item-color ei)))

               (let ((player-rect (make-rectangle :x (- (vx (player-position player)) 20) :y (- (vy (player-position player)) 40)
                                                  :width 40.0 :height 40.0)))
                 (draw-rectangle-rec player-rect +red+))

               (draw-circle-v (player-position player) 5.0 +gold+)

               (end-mode-2d)

               (draw-text "Controls:" 20 20 10 +black+)
               (draw-text "- Right/Left to move" 40 40 10 +darkgray+)
               (draw-text "- Space to jump" 40 60 10 +darkgray+)
               (draw-text "- Mouse Wheel to Zoom in-out" 40 80 10 +darkgray+)
               (draw-text "- R to reset position + zoom" 40 100 10 +darkgray+)
               (draw-text "- C to change camera mode" 40 120 10 +darkgray+)
               (draw-text "Current camera mode:" 20 140 10 +black+)
               (draw-text (aref camera-descriptions camera-option) 40 160 10 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun update-player (player env-items delta)
  (when (is-key-down +key-left+) (decf (vx (player-position player)) (* +player-hor-spd+ delta)))
  (when (is-key-down +key-right+) (incf (vx (player-position player)) (* +player-hor-spd+ delta)))
  (when (and (is-key-down +key-space+) (player-can-jump player))
    (setf (player-speed player) (- +player-jump-spd+)
          (player-can-jump player) nil))

  (let ((hit-obstacle nil)
        (p (player-position player)))
    (loop for ei across env-items
          for rect = (env-item-rect ei)
          do (when (and (/= (env-item-blocking ei) 0)
                        (<= (rectangle-x rect) (vx p))
                        (>= (+ (rectangle-x rect) (rectangle-width rect)) (vx p))
                        (>= (rectangle-y rect) (vy p))
                        (<= (rectangle-y rect) (+ (vy p) (* (player-speed player) delta))))
               (setf hit-obstacle t
                     (player-speed player) 0.0
                     (vy p) (rectangle-y rect))
               (return)))

    (if (not hit-obstacle)
        (progn
          (incf (vy (player-position player)) (* (player-speed player) delta))
          (incf (player-speed player) (* +g+ delta))
          (setf (player-can-jump player) nil))
        (setf (player-can-jump player) t))))

(defun update-camera-center (camera player env-items delta width height)
  (declare (ignore env-items delta))
  (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0))
        (camera2d-target camera) (vcopy (player-position player))))

(defun update-camera-center-inside-map (camera player env-items delta width height)
  (declare (ignore delta))
  (setf (camera2d-target camera) (vcopy (player-position player))
        (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
  (let ((min-x 1000.0) (min-y 1000.0) (max-x -1000.0) (max-y -1000.0))
    (loop for ei across env-items
          for rect = (env-item-rect ei)
          do (setf min-x (min (rectangle-x rect) min-x)
                   max-x (max (+ (rectangle-x rect) (rectangle-width rect)) max-x)
                   min-y (min (rectangle-y rect) min-y)
                   max-y (max (+ (rectangle-y rect) (rectangle-height rect)) max-y)))

    (let ((max (get-world-to-screen-2d (vec2 max-x max-y) camera))
          (min (get-world-to-screen-2d (vec2 min-x min-y) camera)))
      (when (< (vx max) width) (setf (vx (camera2d-offset camera)) (- width (- (vx max) (/ width 2.0)))))
      (when (< (vy max) height) (setf (vy (camera2d-offset camera)) (- height (- (vy max) (/ height 2.0)))))
      (when (> (vx min) 0) (setf (vx (camera2d-offset camera)) (- (/ width 2.0) (vx min))))
      (when (> (vy min) 0) (setf (vy (camera2d-offset camera)) (- (/ height 2.0) (vy min)))))))

(let ((min-speed 30.0)
      (min-effect-length 10.0)
      (fraction-speed 0.8))
  (defun update-camera-center-smooth-follow (camera player env-items delta width height)
    (declare (ignore env-items))
    (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
    (let* ((diff (vector2-subtract (player-position player) (camera2d-target camera)))
           (length (vector2-length diff)))
      (when (> length min-effect-length)
        (let ((speed (max (* fraction-speed length) min-speed)))
          (setf (camera2d-target camera)
                (vector2-add (camera2d-target camera) (vector2-scale diff (/ (* speed delta) length)))))))))

(let ((even-out-speed 700.0)
      (evening-out nil)
      (even-out-target 0.0))
  (defun update-camera-even-out-on-landing (camera player env-items delta width height)
    (declare (ignore env-items))
    (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0))
          (vx (camera2d-target camera)) (vx (player-position player)))

    (if evening-out
        (if (> even-out-target (vy (camera2d-target camera)))
            (progn
              (incf (vy (camera2d-target camera)) (* even-out-speed delta))
              (when (> (vy (camera2d-target camera)) even-out-target)
                (setf (vy (camera2d-target camera)) even-out-target
                      evening-out nil)))
            (progn
              (decf (vy (camera2d-target camera)) (* even-out-speed delta))
              (when (< (vy (camera2d-target camera)) even-out-target)
                (setf (vy (camera2d-target camera)) even-out-target
                      evening-out nil))))
        (when (and (player-can-jump player) (= (player-speed player) 0)
                   (/= (vy (player-position player)) (vy (camera2d-target camera))))
          (setf evening-out t
                even-out-target (vy (player-position player)))))))

(let ((bbox (vec2 0.2 0.2)))
  (defun update-camera-player-bounds-push (camera player env-items delta width height)
    (declare (ignore env-items delta))
    (let ((bbox-world-min (get-screen-to-world-2d (vec2 (* (- 1 (vx bbox)) 0.5 width) (* (- 1 (vy bbox)) 0.5 height)) camera))
          (bbox-world-max (get-screen-to-world-2d (vec2 (* (+ 1 (vx bbox)) 0.5 width) (* (+ 1 (vy bbox)) 0.5 height)) camera))
          (p (player-position player)))
      (setf (camera2d-offset camera) (vec2 (* (- 1 (vx bbox)) 0.5 width) (* (- 1 (vy bbox)) 0.5 height)))

      (when (< (vx p) (vx bbox-world-min)) (setf (vx (camera2d-target camera)) (vx p)))
      (when (< (vy p) (vy bbox-world-min)) (setf (vy (camera2d-target camera)) (vy p)))
      (when (> (vx p) (vx bbox-world-max))
        (setf (vx (camera2d-target camera)) (+ (vx bbox-world-min) (- (vx p) (vx bbox-world-max)))))
      (when (> (vy p) (vy bbox-world-max))
        (setf (vy (camera2d-target camera)) (+ (vy bbox-world-min) (- (vy p) (vy bbox-world-max))))))))

(main)
