;;;; raylib [shapes] example - ball physics
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by David Buzatto (@davidbuzatto) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 David Buzatto (@davidbuzatto)
;;;; Common Lisp port of raylib/examples/shapes/shapes_ball_physics.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-ball-physics
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-ball-physics)

(defconstant +max-balls+ 5000)          ; Maximum quantity of balls

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Ball data type
(defstruct ball
  (position (vec2 0.0 0.0))
  (speed (vec2 0.0 0.0))
  (prev-position (vec2 0.0 0.0))
  (radius 0.0 :type single-float)
  (friction 0.0 :type single-float)
  (elasticity 0.0 :type single-float)
  (color +blank+)
  (grabbed nil))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - ball physics")

    (let ((balls (make-array +max-balls+ :initial-element nil))
          (ball-count 1)
          (grabbed-ball nil)            ; The current ball that is grabbed
          (press-offset (vec2 0.0 0.0)) ; Mouse press offset relative to the ball that grabbedd
          (gravity 100.0)               ; World gravity
          (window-position (get-window-position)))

      ;; Init first ball in the array
      (setf (aref balls 0) (make-ball :position (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0))
                                      :speed (vec2 200.0 200.0)
                                      :prev-position (vec2 0.0 0.0)
                                      :radius 40.0
                                      :friction 0.99
                                      :elasticity 0.9
                                      :color +blue+
                                      :grabbed nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((delta (get-frame-time))
                     (mouse-pos (get-mouse-position)))

                 ;; Checks if a ball was grabbed
                 (when (is-mouse-button-pressed +mouse-button-left+)
                   (loop for i from (1- ball-count) downto 0
                         for ball = (aref balls i)
                         do (setf (vx press-offset) (- (vx mouse-pos) (vx (ball-position ball)))
                                  (vy press-offset) (- (vy mouse-pos) (vy (ball-position ball))))

                            ;; If the distance between the ball position and the mouse press position
                            ;; is less than or equal to the ball radius, the event occurred inside the ball
                            (when (<= (let ((x (float (vx press-offset) 1d0)) (y (float (vy press-offset) 1d0)))
                                        (sqrt (+ (* x x) (* y y)))) ; hypot()
                                      (ball-radius ball))
                              (setf (ball-grabbed ball) t
                                    grabbed-ball ball)
                              (return))))

                 ;; Releases any ball the was grabbed
                 (when (is-mouse-button-released +mouse-button-left+)
                   (when grabbed-ball
                     (setf (ball-grabbed grabbed-ball) nil
                           grabbed-ball nil)))

                 ;; Creates a new ball
                 (when (or (is-mouse-button-pressed +mouse-button-right+)
                           (and (is-key-down +key-left-control+) (is-mouse-button-down +mouse-button-right+)))
                   (when (< ball-count +max-balls+)
                     (setf (aref balls ball-count)
                           (make-ball :position mouse-pos
                                      :speed (let* ((x (float (get-random-value -300 300)))
                                                    (y (float (get-random-value -300 300))))
                                               (vec2 x y))
                                      :prev-position (vec2 0.0 0.0)
                                      :radius (+ 20.0 (float (get-random-value 0 30)))
                                      :friction 0.99
                                      :elasticity 0.9
                                      :color (let* ((r (get-random-value 0 255))
                                                    (g (get-random-value 0 255))
                                                    (b (get-random-value 0 255)))
                                               (list r g b 255))
                                      :grabbed nil))
                     (incf ball-count)))

                 ;; Get window position change for shaking
                 (let ((window-position-delta (vector2-subtract window-position (get-window-position))))
                   (when (> (vector2-length window-position-delta) 5.0)
                     (dotimes (i ball-count)
                       (let ((ball (aref balls i)))
                         (unless (ball-grabbed ball)
                           (setf (ball-speed ball) (vector2-add (ball-speed ball) (vector2-scale window-position-delta 10.0))))))))

                 ;; Shake balls
                 (when (is-mouse-button-pressed +mouse-button-middle+)
                   (dotimes (i ball-count)
                     (let ((ball (aref balls i)))
                       (unless (ball-grabbed ball)
                         (setf (ball-speed ball) (let* ((x (float (get-random-value -2000 2000)))
                                                        (y (float (get-random-value -2000 2000))))
                                                   (vec2 x y)))))))

                 ;; Changes gravity
                 (incf gravity (* (get-mouse-wheel-move) 5))

                 ;; Updates each ball state
                 (dotimes (i ball-count)
                   (let* ((ball (aref balls i))
                          (position (ball-position ball))
                          (speed (ball-speed ball)))

                     ;; The ball is not grabbed
                     (if (not (ball-grabbed ball))
                         (progn
                           ;; Ball repositioning using the velocity
                           (incf (vx position) (* (vx speed) delta))
                           (incf (vy position) (* (vy speed) delta))

                           ;; Does the ball hit the screen right boundary?
                           (cond ((>= (+ (vx position) (ball-radius ball)) screen-width)
                                  (setf (vx position) (- screen-width (ball-radius ball))) ; Ball repositioning
                                  (setf (vx speed) (* (- (vx speed)) (ball-elasticity ball)))) ; Elasticity makes the ball lose 10% of its velocity on hit
                                 ;; Does the ball hit the screen left boundary?
                                 ((<= (- (vx position) (ball-radius ball)) 0)
                                  (setf (vx position) (ball-radius ball))
                                  (setf (vx speed) (* (- (vx speed)) (ball-elasticity ball)))))

                           ;; The same for y axis
                           (cond ((>= (+ (vy position) (ball-radius ball)) screen-height)
                                  (setf (vy position) (- screen-height (ball-radius ball)))
                                  (setf (vy speed) (* (- (vy speed)) (ball-elasticity ball))))
                                 ((<= (- (vy position) (ball-radius ball)) 0)
                                  (setf (vy position) (ball-radius ball))
                                  (setf (vy speed) (* (- (vy speed)) (ball-elasticity ball)))))

                           ;; Friction makes the ball lose 1% of its velocity each frame
                           (setf (vx speed) (* (vx speed) (ball-friction ball)))
                           ;; Gravity affects only the y axis
                           (setf (vy speed) (+ (* (vy speed) (ball-friction ball)) gravity)))
                         (progn
                           ;; Ball repositioning using the mouse position
                           (setf (vx position) (- (vx mouse-pos) (vx press-offset))
                                 (vy position) (- (vy mouse-pos) (vy press-offset)))

                           ;; While the ball is grabbed, recalculates its velocity
                           (setf (vx speed) (/ (- (vx position) (vx (ball-prev-position ball))) delta)
                                 (vy speed) (/ (- (vy position) (vy (ball-prev-position ball))) delta))
                           (setf (ball-prev-position ball) (vcopy position))))))

                 (setf window-position (get-window-position)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i ball-count)
                 (let ((ball (aref balls i)))
                   (draw-circle-v (ball-position ball) (ball-radius ball) (ball-color ball))
                   (draw-circle-lines-v (ball-position ball) (ball-radius ball) +black+)))

               (draw-text "grab a ball by pressing with the mouse and throw it by releasing" 10 10 10 +darkgray+)
               (draw-text "right click to create new balls (keep left control pressed to create a lot)" 10 30 10 +darkgray+)
               (draw-text "use mouse wheel to change gravity" 10 50 10 +darkgray+)
               (draw-text "middle click to shake" 10 70 10 +darkgray+)
               (draw-text (text-format "BALL COUNT: %d" ball-count) 10 (- (get-screen-height) 70) 20 +black+)
               (draw-text (text-format "GRAVITY: %.2f" gravity) 10 (- (get-screen-height) 40) 20 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
