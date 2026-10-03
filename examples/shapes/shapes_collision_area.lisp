;;;; shapes_collision_area.lisp - Collision area detection
;;;; Translated from raylib/examples/shapes/shapes_collision_area.c

(require :cl-raylib)

(defpackage :shapes-collision-area
  (:use :cl :cl-raylib))

(in-package :shapes-collision-area)

(defun main ()
  "Main function - collision area detection"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - collision area")

    ;; Box A: Moving box
    (let ((box-a (make-rectangle :x 10.0 :y (- (/ screen-height 2.0) 50.0) :width 200.0 :height 100.0))
          (box-a-speed-x 4.0)
          ;; Box B: Mouse moved box
          (box-b (make-rectangle :x (- (/ screen-width 2.0) 30.0) 
                                :y (- (/ screen-height 2.0) 30.0) 
                                :width 60.0 :height 60.0))
          (box-collision (make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0)) ; Collision rectangle
          (screen-upper-limit 40) ; Top menu limits
          (pause nil) ; Movement pause
          (collision nil)) ; Collision detection

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        ;; Move box if not paused
        (unless pause
          (incf (rectangle-x box-a) box-a-speed-x))

        ;; Bounce box on x screen limits
        (when (or (>= (+ (rectangle-x box-a) (rectangle-width box-a)) screen-width)
                  (<= (rectangle-x box-a) 0))
          (setf box-a-speed-x (* box-a-speed-x -1)))

        ;; Update player-controlled-box (box B)
        (setf (rectangle-x box-b) (- (get-mouse-x) (/ (rectangle-width box-b) 2)))
        (setf (rectangle-y box-b) (- (get-mouse-y) (/ (rectangle-height box-b) 2)))

        ;; Make sure Box B does not go out of move area limits
        (when (>= (+ (rectangle-x box-b) (rectangle-width box-b)) screen-width)
          (setf (rectangle-x box-b) (- screen-width (rectangle-width box-b))))
        (when (<= (rectangle-x box-b) 0)
          (setf (rectangle-x box-b) 0.0))

        (when (>= (+ (rectangle-y box-b) (rectangle-height box-b)) screen-height)
          (setf (rectangle-y box-b) (- screen-height (rectangle-height box-b))))
        (when (<= (rectangle-y box-b) screen-upper-limit)
          (setf (rectangle-y box-b) (float screen-upper-limit)))

        ;; Check boxes collision
        (setf collision (check-collision-recs box-a box-b))

        ;; Get collision rectangle (only on collision)
        (when collision
          (setf box-collision (get-collision-rec box-a box-b)))

        ;; Pause Box A movement
        (when (is-key-pressed +key-space+)
          (setf pause (not pause)))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-rectangle 0 0 screen-width screen-upper-limit (if collision +red+ +black+))

          (draw-rectangle-rec box-a +gold+)
          (draw-rectangle-rec box-b +blue+)

          (when collision
            ;; Draw collision area
            (draw-rectangle-rec box-collision +lime+)

            ;; Draw collision message
            (let ((collision-text "COLLISION!")
                  (text-width (measure-text "COLLISION!" 20)))
              (draw-text collision-text 
                        (- (/ screen-width 2) (/ text-width 2)) 
                        (- (/ screen-upper-limit 2) 10) 
                        20 +black+))

            ;; Draw collision area
            (let ((area-text (format nil "Collision Area: ~d" 
                                    (truncate (* (rectangle-width box-collision) 
                                               (rectangle-height box-collision))))))
              (draw-text area-text (- (/ screen-width 2) 100) (+ screen-upper-limit 10) 20 +black+)))

          ;; Draw help instructions
          (draw-text "Press SPACE to PAUSE/RESUME" 20 (- screen-height 35) 20 +lightgray+)

          (draw-fps 10 10)

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)