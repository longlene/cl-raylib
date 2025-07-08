(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib :3d-vectors))

(in-package :raylib-user)

(defconstant +rad2deg+ (/ 180.0 pi))

(defun vector2-angle (v1 v2)
  "Calculate angle between two vectors in radians"
  (acos (max -1.0 (min 1.0 (vdot v1 v2)))))

(defun vector2-line-angle (start end)
  "Calculate angle of a line from start to end in radians, relative to positive X axis"
  (atan (- (vy end) (vy start)) (- (vx end) (vx start))))

(defun vector2-normalize (v)
  "Normalize vector to unit length"
  (let ((length (vlength v)))
    (if (> length 0.0)
        (v/ v length)
        (vec2 0.0 0.0))))

(defun vector2-subtract (v1 v2)
  "Subtract v2 from v1"
  (v- v1 v2))

(defun vector2-add (v1 v2)
  "Add v1 and v2"
  (v+ v1 v2))

(defun main ()
  "raylib [math] example - vector angle"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [math] example - vector angle")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      (let ((v0 (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))
            (v1 (vector2-add (vec2 (/ screen-width 2.0) (/ screen-height 2.0)) 
                            (vec2 100.0 80.0)))
            (v2 (vec2 0.0 0.0))
            (angle 0.0)
            (angle-mode 0)) ; 0-Vector2Angle(), 1-Vector2LineAngle()

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (let ((start-angle 0.0))
                 (when (= angle-mode 0) 
                   (setf start-angle (- (* (vector2-line-angle v0 v1) +rad2deg+))))
                 (when (= angle-mode 1) 
                   (setf start-angle 0.0))

                 (setf v2 (get-mouse-position))

                 (when (is-key-pressed :key-space) 
                   (setf angle-mode (if (= angle-mode 0) 1 0)))

                 (when (and (= angle-mode 0) (is-mouse-button-down :mouse-button-right))
                   (setf v1 (get-mouse-position)))

                 (cond
                   ((= angle-mode 0)
                    ;; Calculate angle between two vectors, considering a common origin (v0)
                    (let ((v1-normal (vector2-normalize (vector2-subtract v1 v0)))
                          (v2-normal (vector2-normalize (vector2-subtract v2 v0))))
                      (setf angle (* (vector2-angle v1-normal v2-normal) +rad2deg+))))
                   ((= angle-mode 1)
                    ;; Calculate angle defined by a two vectors line, in reference to horizontal line
                    (setf angle (* (vector2-line-angle v0 v2) +rad2deg+))))

                 ;; Draw
                 (with-drawing
                   (clear-background :raywhite)

                   (cond
                     ((= angle-mode 0)
                      (draw-text "MODE 0: Angle between V1 and V2" 10 10 20 :black)
                      (draw-text "Right Click to Move V2" 10 30 20 :darkgray)

                      (draw-line-ex v0 v1 2.0 :black)
                      (draw-line-ex v0 v2 2.0 :red)

                      (draw-circle-sector v0 40.0 start-angle (+ start-angle angle) 32 (fade :green 0.6)))
                     ((= angle-mode 1)
                      (draw-text "MODE 1: Angle formed by line V1 to V2" 10 10 20 :black)

                      (draw-line 0 (floor (/ screen-height 2)) screen-width (floor (/ screen-height 2)) :lightgray)
                      (draw-line-ex v0 v2 2.0 :red)

                      (draw-circle-sector v0 40.0 start-angle (- start-angle angle) 32 (fade :green 0.6))))

                   (draw-text "v0" (floor (vx v0)) (floor (vy v0)) 10 :darkgray)

                   ;; If the line from v0 to v1 would overlap the text, move it's position up 10
                   (when (= angle-mode 0)
                     (if (> (vy (vector2-subtract v0 v1)) 0.0)
                         (draw-text "v1" (floor (vx v1)) (floor (- (vy v1) 10.0)) 10 :darkgray)
                         (draw-text "v1" (floor (vx v1)) (floor (vy v1)) 10 :darkgray)))

                   ;; If angle mode 1, use v1 to emphasize the horizontal line
                   (when (= angle-mode 1)
                     (draw-text "v1" (floor (+ (vx v0) 40.0)) (floor (vy v0)) 10 :darkgray))

                   ;; position adjusted by -10 so it isn't hidden by cursor
                   (draw-text "v2" (floor (- (vx v2) 10.0)) (floor (- (vy v2) 10.0)) 10 :darkgray)

                   (draw-text "Press SPACE to change MODE" 460 10 20 :darkgray)
                   (draw-text (format nil "ANGLE: ~5,2f" angle) 10 70 20 :lime))))))))

(main)