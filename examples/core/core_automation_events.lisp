;;;; raylib [core] example - automation events
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example based on 2d_camera_platformer example by arvyy (@arvyy)
;;;;
;;;; Copyright (c) 2023-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_automation_events.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-automation-events
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-automation-events)

(defconstant +gravity+ 400)
(defconstant +player-jump-spd+ 350.0)
(defconstant +player-hor-spd+ 200.0)

(defconstant +max-environment-elements+ 5)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct player
  (position (vec2 0.0 0.0))
  (speed 0.0)
  (can-jump nil))

(defstruct env-element
  rect
  blocking
  color)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - automation events")

    (let* (;; Define player
           (player (make-player :position (vec2 400.0 280.0) :speed 0.0 :can-jump nil))
           ;; Define environment elements (platforms)
           (env-elements (vector (make-env-element :rect (make-rectangle :x 0.0 :y 0.0 :width 1000.0 :height 400.0) :blocking 0 :color +lightgray+)
                                 (make-env-element :rect (make-rectangle :x 0.0 :y 400.0 :width 1000.0 :height 200.0) :blocking 1 :color +gray+)
                                 (make-env-element :rect (make-rectangle :x 300.0 :y 200.0 :width 400.0 :height 10.0) :blocking 1 :color +gray+)
                                 (make-env-element :rect (make-rectangle :x 250.0 :y 300.0 :width 100.0 :height 10.0) :blocking 1 :color +gray+)
                                 (make-env-element :rect (make-rectangle :x 650.0 :y 300.0 :width 100.0 :height 10.0) :blocking 1 :color +gray+)))
           ;; Define camera
           (camera (make-camera2d :target (vcopy (player-position player))
                                  :offset (vec2 (/ screen-width 2.0) (/ screen-height 2.0))
                                  :rotation 0.0
                                  :zoom 1.0))
           ;; Automation events
           (aelist (load-automation-event-list nil)) ; Initialize list of automation events to record new events
           (event-recording nil)
           (event-playing nil)
           (frame-counter 0)
           (play-frame-counter 0)
           (current-play-frame 0))

      (set-automation-event-list aelist)

      (flet ((reset-scene ()
               (setf (player-position player) (vec2 400.0 280.0)
                     (player-speed player) 0.0
                     (player-can-jump player) nil
                     (camera2d-target camera) (vcopy (player-position player))
                     (camera2d-offset camera) (vec2 (/ screen-width 2.0) (/ screen-height 2.0))
                     (camera2d-rotation camera) 0.0
                     (camera2d-zoom camera) 1.0)))

        (set-target-fps 60)
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close)
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (let ((delta-time 0.015))      ;GetFrameTime();

                   ;; Dropped files logic
                   ;;----------------------------------------------------------------------------------
                   (when (is-file-dropped)
                     (let ((dropped-files (load-dropped-files)))

                       ;; Supports loading .rgs style files (text or binary) and .png style palette images
                       (when (is-file-extension (elt (file-path-list-paths dropped-files) 0) ".txt;.rae")
                         (unload-automation-event-list aelist)
                         (setf aelist (load-automation-event-list (elt (file-path-list-paths dropped-files) 0)))

                         (setf event-recording nil)

                         ;; Reset scene state to play
                         (setf event-playing t
                               play-frame-counter 0
                               current-play-frame 0)

                         (reset-scene))

                       (unload-dropped-files dropped-files))) ; Unload filepaths from memory
                   ;;----------------------------------------------------------------------------------

                   ;; Update player
                   ;;----------------------------------------------------------------------------------
                   (when (is-key-down +key-left+) (decf (vx (player-position player)) (* +player-hor-spd+ delta-time)))
                   (when (is-key-down +key-right+) (incf (vx (player-position player)) (* +player-hor-spd+ delta-time)))
                   (when (and (is-key-down +key-space+) (player-can-jump player))
                     (setf (player-speed player) (- +player-jump-spd+)
                           (player-can-jump player) nil))

                   (let ((hit-obstacle nil)
                         (p (player-position player)))
                     (loop for element across env-elements
                           for rect = (env-element-rect element)
                           do (when (and (/= (env-element-blocking element) 0)
                                         (<= (rectangle-x rect) (vx p))
                                         (>= (+ (rectangle-x rect) (rectangle-width rect)) (vx p))
                                         (>= (rectangle-y rect) (vy p))
                                         (<= (rectangle-y rect) (+ (vy p) (* (player-speed player) delta-time))))
                                (setf hit-obstacle t
                                      (player-speed player) 0.0
                                      (vy p) (rectangle-y rect))))

                     (if (not hit-obstacle)
                         (progn
                           (incf (vy (player-position player)) (* (player-speed player) delta-time))
                           (incf (player-speed player) (* +gravity+ delta-time))
                           (setf (player-can-jump player) nil))
                         (setf (player-can-jump player) t)))

                   (when (is-key-pressed +key-r+)
                     ;; Reset game state
                     (reset-scene))
                   ;;----------------------------------------------------------------------------------

                   ;; Events playing
                   ;; NOTE: Logic must be before Camera update because it depends on mouse-wheel value,
                   ;; that can be set by the played event... but some other inputs could be affected
                   ;;----------------------------------------------------------------------------------
                   (when event-playing
                     ;; NOTE: Multiple events could be executed in a single frame
                     (loop while (= play-frame-counter
                                    (automation-event-frame (aref (automation-event-list-events aelist) current-play-frame)))
                           do (play-automation-event (aref (automation-event-list-events aelist) current-play-frame))
                              (incf current-play-frame)

                              (when (= current-play-frame (automation-event-list-count aelist))
                                (setf event-playing nil
                                      current-play-frame 0
                                      play-frame-counter 0)

                                (trace-log +log-info+ "FINISH PLAYING!")
                                (return)))

                     (incf play-frame-counter))
                   ;;----------------------------------------------------------------------------------

                   ;; Update camera
                   ;;----------------------------------------------------------------------------------
                   (setf (camera2d-target camera) (vcopy (player-position player))
                         (camera2d-offset camera) (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))
                   (let ((min-x 1000.0) (min-y 1000.0) (max-x -1000.0) (max-y -1000.0))

                     ;; WARNING: On event replay, mouse-wheel internal value is set
                     (incf (camera2d-zoom camera) (* (get-mouse-wheel-move) 0.05))
                     (cond ((> (camera2d-zoom camera) 3.0) (setf (camera2d-zoom camera) 3.0))
                           ((< (camera2d-zoom camera) 0.25) (setf (camera2d-zoom camera) 0.25)))

                     (loop for element across env-elements
                           for rect = (env-element-rect element)
                           do (setf min-x (min (rectangle-x rect) min-x)
                                    max-x (max (+ (rectangle-x rect) (rectangle-width rect)) max-x)
                                    min-y (min (rectangle-y rect) min-y)
                                    max-y (max (+ (rectangle-y rect) (rectangle-height rect)) max-y)))

                     (let ((max (get-world-to-screen-2d (vec2 max-x max-y) camera))
                           (min (get-world-to-screen-2d (vec2 min-x min-y) camera)))
                       (when (< (vx max) screen-width) (setf (vx (camera2d-offset camera)) (- screen-width (- (vx max) (/ screen-width 2.0)))))
                       (when (< (vy max) screen-height) (setf (vy (camera2d-offset camera)) (- screen-height (- (vy max) (/ screen-height 2.0)))))
                       (when (> (vx min) 0) (setf (vx (camera2d-offset camera)) (- (/ screen-width 2.0) (vx min))))
                       (when (> (vy min) 0) (setf (vy (camera2d-offset camera)) (- (/ screen-height 2.0) (vy min))))))
                   ;;----------------------------------------------------------------------------------

                   ;; Events management
                   (cond ((is-key-pressed +key-s+) ; Toggle events recording
                          (unless event-playing
                            (if event-recording
                                (progn
                                  (stop-automation-event-recording)
                                  (setf event-recording nil)

                                  (export-automation-event-list aelist "automation.rae")

                                  (trace-log +log-info+ "RECORDED FRAMES: ~d" (automation-event-list-count aelist)))
                                (progn
                                  (set-automation-event-base-frame 180)
                                  (start-automation-event-recording)
                                  (setf event-recording t)))))
                         ((is-key-pressed +key-a+) ; Toggle events playing (WARNING: Starts next frame)
                          (when (and (not event-recording) (> (automation-event-list-count aelist) 0))
                            ;; Reset scene state to play
                            (setf event-playing t
                                  play-frame-counter 0
                                  current-play-frame 0)

                            (reset-scene))))

                   (if (or event-recording event-playing) (incf frame-counter) (setf frame-counter 0)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +lightgray+)

                 (begin-mode-2d camera)

                 ;; Draw environment elements
                 (loop for element across env-elements
                       do (draw-rectangle-rec (env-element-rect element) (env-element-color element)))

                 ;; Draw player rectangle
                 (draw-rectangle-rec (make-rectangle :x (- (vx (player-position player)) 20) :y (- (vy (player-position player)) 40)
                                                     :width 40.0 :height 40.0)
                                     +red+)

                 (end-mode-2d)

                 ;; Draw game controls
                 (draw-rectangle 10 10 290 145 (fade +skyblue+ 0.5))
                 (draw-rectangle-lines 10 10 290 145 (fade +blue+ 0.8))

                 (draw-text "Controls:" 20 20 10 +black+)
                 (draw-text "- RIGHT | LEFT: Player movement" 30 40 10 +darkgray+)
                 (draw-text "- SPACE: Player jump" 30 60 10 +darkgray+)
                 (draw-text "- R: Reset game state" 30 80 10 +darkgray+)

                 (draw-text "- S: START/STOP RECORDING INPUT EVENTS" 30 110 10 +black+)
                 (draw-text "- A: REPLAY LAST RECORDED INPUT EVENTS" 30 130 10 +black+)

                 ;; Draw automation events recording indicator
                 (cond (event-recording
                        (draw-rectangle 10 160 290 30 (fade +red+ 0.3))
                        (draw-rectangle-lines 10 160 290 30 (fade +maroon+ 0.8))
                        (draw-circle 30 175 10.0 +maroon+)

                        (when (= (mod (floor frame-counter 15) 2) 1)
                          (draw-text (text-format "RECORDING EVENTS... [%i]" (automation-event-list-count aelist)) 50 170 10 +maroon+)))
                       (event-playing
                        (draw-rectangle 10 160 290 30 (fade +lime+ 0.3))
                        (draw-rectangle-lines 10 160 290 30 (fade +darkgreen+ 0.8))
                        (draw-triangle (vec2 20.0 (+ 155.0 10)) (vec2 20.0 (+ 155.0 30)) (vec2 40.0 (+ 155.0 20)) +darkgreen+)

                        (when (= (mod (floor frame-counter 15) 2) 1)
                          (draw-text (text-format "PLAYING RECORDED EVENTS... [%i]" current-play-frame) 50 170 10 +darkgreen+))))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
