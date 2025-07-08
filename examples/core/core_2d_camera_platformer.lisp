(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defconstant +g+ 400 "Gravity")
(defconstant +player-jump-spd+ 350.0 "Player jump speed")
(defconstant +player-hor-spd+ 200.0 "Player horizontal speed")

(defstruct player
  position
  speed
  can-jump)

(defstruct env-item
  rect
  blocking
  color)

;; Camera update functions
(defun update-camera-center (camera player env-items delta width height)
  "Follow player center"
  (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
  (setf (camera2d-target camera) (player-position player)))

(defun update-camera-center-inside-map (camera player env-items delta width height)
  "Follow player center, but clamp to map edges"
  (setf (camera2d-target camera) (player-position player))
  (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
  
  (let ((min-x 1000) (min-y 1000) (max-x -1000) (max-y -1000))
    ;; Find map bounds
    (loop for item in env-items do
      (let ((rect (env-item-rect item)))
        (setf min-x (min (rectangle-x rect) min-x))
        (setf max-x (max (+ (rectangle-x rect) (rectangle-width rect)) max-x))
        (setf min-y (min (rectangle-y rect) min-y))
        (setf max-y (max (+ (rectangle-y rect) (rectangle-height rect)) max-y))))
    
    (let ((max-screen (get-world-to-screen-2d (vec2 max-x max-y) camera))
          (min-screen (get-world-to-screen-2d (vec2 min-x min-y) camera)))
      (when (< (vx max-screen) width)
        (setf (vx (camera2d-offset camera)) (- width (- (vx max-screen) (/ width 2)))))
      (when (< (vy max-screen) height)
        (setf (vy (camera2d-offset camera)) (- height (- (vy max-screen) (/ height 2)))))
      (when (> (vx min-screen) 0)
        (setf (vx (camera2d-offset camera)) (- (/ width 2) (vx min-screen))))
      (when (> (vy min-screen) 0)
        (setf (vy (camera2d-offset camera)) (- (/ height 2) (vy min-screen)))))))

(defun update-camera-center-smooth-follow (camera player env-items delta width height)
  "Follow player center; smoothed"
  (let ((min-speed 30)
        (min-effect-length 10)
        (fraction-speed 0.8))
    (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
    (let* ((diff (v- (player-position player) (camera2d-target camera)))
           (length (vlength diff)))
      (when (> length min-effect-length)
        (let ((speed (max (* fraction-speed length) min-speed)))
          (setf (camera2d-target camera) 
                (v+ (camera2d-target camera) 
                    (v* diff (/ (* speed delta) length)))))))))

(defvar *evening-out* nil)
(defvar *even-out-target* 0.0)

(defun update-camera-even-out-on-landing (camera player env-items delta width height)
  "Follow player center horizontally; update player center vertically after landing"
  (let ((even-out-speed 700))
    (setf (camera2d-offset camera) (vec2 (/ width 2.0) (/ height 2.0)))
    (setf (vx (camera2d-target camera)) (vx (player-position player)))
    
    (if *evening-out*
        (if (> *even-out-target* (vy (camera2d-target camera)))
            (progn
              (incf (vy (camera2d-target camera)) (* even-out-speed delta))
              (when (> (vy (camera2d-target camera)) *even-out-target*)
                (setf (vy (camera2d-target camera)) *even-out-target*)
                (setf *evening-out* nil)))
            (progn
              (decf (vy (camera2d-target camera)) (* even-out-speed delta))
              (when (< (vy (camera2d-target camera)) *even-out-target*)
                (setf (vy (camera2d-target camera)) *even-out-target*)
                (setf *evening-out* nil))))
        (when (and (player-can-jump player) 
                   (= (player-speed player) 0) 
                   (/= (vy (player-position player)) (vy (camera2d-target camera))))
          (setf *evening-out* t)
          (setf *even-out-target* (vy (player-position player)))))))

(defun update-camera-player-bounds-push (camera player env-items delta width height)
  "Player push camera on getting too close to screen edge"
  (let ((bbox (vec2 0.2 0.2)))
    (let ((bbox-world-min (get-screen-to-world-2d 
                           (vec2 (* (- 1 (vx bbox)) 0.5 width) 
                                 (* (- 1 (vy bbox)) 0.5 height)) 
                           camera))
          (bbox-world-max (get-screen-to-world-2d 
                           (vec2 (* (+ 1 (vx bbox)) 0.5 width) 
                                 (* (+ 1 (vy bbox)) 0.5 height)) 
                           camera)))
      (setf (camera2d-offset camera) 
            (vec2 (* (- 1 (vx bbox)) 0.5 width) 
                  (* (- 1 (vy bbox)) 0.5 height)))
      
      (when (< (vx (player-position player)) (vx bbox-world-min))
        (setf (vx (camera2d-target camera)) (vx (player-position player))))
      (when (< (vy (player-position player)) (vy bbox-world-min))
        (setf (vy (camera2d-target camera)) (vy (player-position player))))
      (when (> (vx (player-position player)) (vx bbox-world-max))
        (setf (vx (camera2d-target camera)) 
              (+ (vx bbox-world-min) (- (vx (player-position player)) (vx bbox-world-max)))))
      (when (> (vy (player-position player)) (vy bbox-world-max))
        (setf (vy (camera2d-target camera)) 
              (+ (vy bbox-world-min) (- (vy (player-position player)) (vy bbox-world-max))))))))

(defun update-player (player env-items delta)
  "Update player physics and input"
  (when (is-key-down :key-left)
    (decf (vx (player-position player)) (* +player-hor-spd+ delta)))
  (when (is-key-down :key-right)
    (incf (vx (player-position player)) (* +player-hor-spd+ delta)))
  (when (and (is-key-down :key-space) (player-can-jump player))
    (setf (player-speed player) (- +player-jump-spd+))
    (setf (player-can-jump player) nil))
  
  (let ((hit-obstacle nil))
    (loop for item in env-items do
      (let ((ei-rect (env-item-rect item))
            (p (player-position player)))
        (when (and (env-item-blocking item)
                   (<= (rectangle-x ei-rect) (vx p))
                   (>= (+ (rectangle-x ei-rect) (rectangle-width ei-rect)) (vx p))
                   (>= (rectangle-y ei-rect) (vy p))
                   (<= (rectangle-y ei-rect) (+ (vy p) (* (player-speed player) delta))))
          (setf hit-obstacle t)
          (setf (player-speed player) 0.0)
          (setf (vy (player-position player)) (rectangle-y ei-rect))
          (return))))
    
    (if (not hit-obstacle)
        (progn
          (incf (vy (player-position player)) (* (player-speed player) delta))
          (incf (player-speed player) (* +g+ delta))
          (setf (player-can-jump player) nil))
        (setf (player-can-jump player) t))))

(defun main ()
  "raylib [core] example - 2D Camera platformer"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [core] example - 2d camera")
      (set-target-fps 60)

      (let ((player (make-player :position (vec2 400 280)
                                 :speed 0
                                 :can-jump nil))
            (env-items (list
                        (make-env-item :rect (make-rectangle :x 0 :y 0 :width 1000 :height 400)
                                       :blocking nil :color :lightgray)
                        (make-env-item :rect (make-rectangle :x 0 :y 400 :width 1000 :height 200)
                                       :blocking t :color :gray)
                        (make-env-item :rect (make-rectangle :x 300 :y 200 :width 400 :height 10)
                                       :blocking t :color :gray)
                        (make-env-item :rect (make-rectangle :x 250 :y 300 :width 100 :height 10)
                                       :blocking t :color :gray)
                        (make-env-item :rect (make-rectangle :x 650 :y 300 :width 100 :height 10)
                                       :blocking t :color :gray)))
            (camera (make-camera2d :target (vec2 400 280)
                                   :offset (vec2 (/ screen-width 2.0) (/ screen-height 2.0))
                                   :rotation 0.0
                                   :zoom 1.0))
            (camera-option 0)
            (camera-updaters (list #'update-camera-center
                                   #'update-camera-center-inside-map
                                   #'update-camera-center-smooth-follow
                                   #'update-camera-even-out-on-landing
                                   #'update-camera-player-bounds-push))
            (camera-descriptions (list
                                  "Follow player center"
                                  "Follow player center, but clamp to map edges"
                                  "Follow player center; smoothed"
                                  "Follow player center horizontally; update player center vertically after landing"
                                  "Player push camera on getting too close to screen edge")))

        (loop
          until (window-should-close)
          do (progn
               ;; Update
               (let ((delta-time (get-frame-time)))
                 (update-player player env-items delta-time)
                 
                 ;; Handle zoom
                 (incf (camera2d-zoom camera) (* (get-mouse-wheel-move) 0.05))
                 (setf (camera2d-zoom camera) (max 0.25 (min 3.0 (camera2d-zoom camera))))
                 
                 ;; Reset
                 (when (is-key-pressed :key-r)
                   (setf (camera2d-zoom camera) 1.0)
                   (setf (player-position player) (vec2 400 280)))
                 
                 ;; Change camera mode
                 (when (is-key-pressed :key-c)
                   (setf camera-option (mod (1+ camera-option) (length camera-updaters))))
                 
                 ;; Update camera
                 (funcall (nth camera-option camera-updaters) 
                          camera player env-items delta-time screen-width screen-height))

               ;; Draw
               (with-drawing
                 (clear-background :lightgray)

                 (with-mode-2d (camera)
                   ;; Draw environment
                   (loop for item in env-items do
                     (draw-rectangle-rec (env-item-rect item) (env-item-color item)))

                   ;; Draw player
                   (let ((player-rect (make-rectangle :x (- (vx (player-position player)) 20)
                                                      :y (- (vy (player-position player)) 40)
                                                      :width 40.0
                                                      :height 40.0)))
                     (draw-rectangle-rec player-rect :red))
                   
                   (draw-circle-v (player-position player) 5.0 :gold))

                 ;; Draw UI
                 (draw-text "Controls:" 20 20 10 :black)
                 (draw-text "- Right/Left to move" 40 40 10 :darkgray)
                 (draw-text "- Space to jump" 40 60 10 :darkgray)
                 (draw-text "- Mouse Wheel to Zoom in-out, R to reset zoom" 40 80 10 :darkgray)
                 (draw-text "- C to change camera mode" 40 100 10 :darkgray)
                 (draw-text "Current camera mode:" 20 120 10 :black)
                 (draw-text (nth camera-option camera-descriptions) 40 140 10 :darkgray))))))))

(main)