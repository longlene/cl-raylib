;;;; shapes_easings_ball_anim.lisp - Easings ball animation
;;;; Translated from raylib/examples/shapes/shapes_easings_ball_anim.c

(require :cl-raylib)

(defpackage :shapes-easings-ball-anim
  (:use :cl :cl-raylib))

(in-package :shapes-easings-ball-anim)

;; Simple easing functions implementation
(defun ease-elastic-out (t b c d)
  "Elastic ease out function"
  (let ((t (/ t d)))
    (if (= t 0)
        b
        (if (= t 1)
            (+ b c)
            (let* ((p (* d 0.3))
                   (s (/ p 4)))
              (+ (* c (expt 2 (* -10 t)) (sin (/ (* (- t s) (* 2 pi)) p))) c b))))))

(defun ease-elastic-in (t b c d)
  "Elastic ease in function"
  (let ((t (/ t d)))
    (if (= t 0)
        b
        (if (= t 1)
            (+ b c)
            (let* ((p (* d 0.3))
                   (s (/ p 4)))
              (+ (* (- c) (expt 2 (* 10 (decf t))) (sin (/ (* (- t s) (* 2 pi)) p))) b))))))

(defun ease-cubic-out (t b c d)
  "Cubic ease out function"
  (let ((t (1- (/ t d))))
    (+ (* c (1+ (* t t t))) b)))

(defun main ()
  "Main function - easings ball animation"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [shapes] example - easings ball anim")

    ;; Ball variable value to be animated with easings
    (let ((ball-position-x -100)
          (ball-radius 20)
          (ball-alpha 0.0)
          (state 0)
          (frames-counter 0))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (cond
          ((= state 0) ; Move ball position X with easing
           (incf frames-counter)
           (setf ball-position-x (truncate (ease-elastic-out frames-counter -100 (+ (/ screen-width 2.0) 100) 120)))

           (when (>= frames-counter 120)
             (setf frames-counter 0)
             (setf state 1)))

          ((= state 1) ; Increase ball radius with easing
           (incf frames-counter)
           (setf ball-radius (truncate (ease-elastic-in frames-counter 20 500 200)))

           (when (>= frames-counter 200)
             (setf frames-counter 0)
             (setf state 2)))

          ((= state 2) ; Change ball alpha with easing (background color blending)
           (incf frames-counter)
           (setf ball-alpha (ease-cubic-out frames-counter 0.0 1.0 200))

           (when (>= frames-counter 200)
             (setf frames-counter 0)
             (setf state 3)))

          ((= state 3) ; Reset state to play again
           (when (is-key-pressed +key-enter+)
             ;; Reset required variables to play again
             (setf ball-position-x -100)
             (setf ball-radius 20)
             (setf ball-alpha 0.0)
             (setf state 0))))

        (when (is-key-pressed +key-r+)
          (setf frames-counter 0))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (when (>= state 2)
            (draw-rectangle 0 0 screen-width screen-height +green+))
          (draw-circle ball-position-x 200 (float ball-radius) (fade +red+ (- 1.0 ball-alpha)))

          (when (= state 3)
            (draw-text "PRESS [ENTER] TO PLAY AGAIN!" 240 200 20 +black+))

        (end-drawing)))

    ;; Close window
    (close-window)))

;; Run the example
(main)