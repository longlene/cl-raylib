;;;; shapes_following_eyes.lisp - Following eyes
;;;; Translated from raylib/examples/shapes/shapes_following_eyes.c

(require :cl-raylib)

(defpackage :shapes-following-eyes
  (:use :cl :cl-raylib))

(in-package :shapes-following-eyes)

(defun main ()
  "Main function - following eyes"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - following eyes")

    (let ((sclera-left-position (vec2 (- (/ screen-width 2.0) 100.0) (/ screen-height 2.0)))
          (sclera-right-position (vec2 (+ (/ screen-width 2.0) 100.0) (/ screen-height 2.0)))
          (sclera-radius 80.0)
          (iris-left-position (vec2 (- (/ screen-width 2.0) 100.0) (/ screen-height 2.0)))
          (iris-right-position (vec2 (+ (/ screen-width 2.0) 100.0) (/ screen-height 2.0)))
          (iris-radius 24.0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (setf iris-left-position (get-mouse-position))
        (setf iris-right-position (get-mouse-position))

        ;; Check not inside the left eye sclera
        (unless (check-collision-point-circle iris-left-position sclera-left-position (- sclera-radius iris-radius))
          (let* ((dx (- (vx iris-left-position) (vx sclera-left-position)))
                 (dy (- (vy iris-left-position) (vy sclera-left-position)))
                 (angle (atan dy dx))
                 (dxx (* (- sclera-radius iris-radius) (cos angle)))
                 (dyy (* (- sclera-radius iris-radius) (sin angle))))
            (setf iris-left-position (vec2 (+ (vx sclera-left-position) dxx)
                                          (+ (vy sclera-left-position) dyy)))))

        ;; Check not inside the right eye sclera
        (unless (check-collision-point-circle iris-right-position sclera-right-position (- sclera-radius iris-radius))
          (let* ((dx (- (vx iris-right-position) (vx sclera-right-position)))
                 (dy (- (vy iris-right-position) (vy sclera-right-position)))
                 (angle (atan dy dx))
                 (dxx (* (- sclera-radius iris-radius) (cos angle)))
                 (dyy (* (- sclera-radius iris-radius) (sin angle))))
            (setf iris-right-position (vec2 (+ (vx sclera-right-position) dxx)
                                           (+ (vy sclera-right-position) dyy)))))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-circle-v sclera-left-position sclera-radius +lightgray+)
          (draw-circle-v iris-left-position iris-radius +brown+)
          (draw-circle-v iris-left-position 10.0 +black+)

          (draw-circle-v sclera-right-position sclera-radius +lightgray+)
          (draw-circle-v iris-right-position iris-radius +darkgreen+)
          (draw-circle-v iris-right-position 10.0 +black+)

          (draw-fps 10 10)

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)