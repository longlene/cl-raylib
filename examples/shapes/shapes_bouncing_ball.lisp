;;;; shapes_bouncing_ball.lisp
;;;; 
;;;; cl-raylib [shapes] example - bouncing ball
;;;;
;;;; Translation of raylib's shapes_bouncing_ball.c example
;;;; This example demonstrates basic physics simulation with collision detection
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-shapes-bouncing-ball
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-shapes-bouncing-ball)

(defun shapes-bouncing-ball ()
  "Bouncing ball example - equivalent to raylib's shapes_bouncing_ball"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [shapes] example - bouncing ball")
      (let ((ball-position (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))
            (ball-speed (vec2 5.0 4.0))
            (ball-radius 20)
            (pause nil)
            (frames-counter 0))
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (when (is-key-pressed +key-space+) 
            (setf pause (not pause)))
          
          (if (not pause)
              (progn
                ;; Update ball position
                (incf (vx2 ball-position) (vx2 ball-speed))
                (incf (vy2 ball-position) (vy2 ball-speed))
                
                ;; Check walls collision for bouncing
                (when (or (>= (vx2 ball-position) (- (get-screen-width) ball-radius))
                          (<= (vx2 ball-position) ball-radius))
                  (setf (vx2 ball-speed) (* -1.0 (vx2 ball-speed))))
                
                (when (or (>= (vy2 ball-position) (- (get-screen-height) ball-radius))
                          (<= (vy2 ball-position) ball-radius))
                  (setf (vy2 ball-speed) (* -1.0 (vy2 ball-speed)))))
              (incf frames-counter))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (draw-circle-v ball-position ball-radius +maroon+)
            (draw-text "PRESS SPACE to PAUSE BALL MOVEMENT" 10 (- (get-screen-height) 25) 20 +lightgray+)
            
            ;; On pause, we draw a blinking message
            (when (and pause (= (mod (floor frames-counter 30) 2) 1))
              (draw-text "PAUSED" 350 200 30 +gray+))
            
            (draw-fps 10 10)))))))

;; Run the example
(shapes-bouncing-ball)