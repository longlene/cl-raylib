;;;; raylib [core] example - 2d camera
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.5, last time updated with raylib 3.0
;;;;
;;;; Copyright (c) 2016-2026 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_2d_camera.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-2d-camera
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-2d-camera)

(defconstant +max-buildings+ 100)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 2d camera")

    (let ((player (make-rectangle :x 400.0 :y 280.0 :width 40.0 :height 40.0))
          (buildings (make-array +max-buildings+))
          (build-colors (make-array +max-buildings+))
          (spacing 0))

      (dotimes (i +max-buildings+)
        (let* ((width (float (get-random-value 50 200)))
               (height (float (get-random-value 100 800))))
          (setf (aref buildings i) (make-rectangle :x (+ -6000.0 spacing) :y (- screen-height 130.0 height)
                                                   :width width :height height))
          (incf spacing (truncate width))
          (setf (aref build-colors i) (list (get-random-value 200 240)
                                            (get-random-value 200 240)
                                            (get-random-value 200 250)
                                            255))))

      (let ((camera (make-camera2d :target (vec2 (+ (rectangle-x player) 20.0) (+ (rectangle-y player) 20.0))
                                   :offset (vec2 (/ screen-width 2.0) (/ screen-height 2.0))
                                   :rotation 0.0
                                   :zoom 1.0)))

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Player movement
                 (cond ((is-key-down +key-right+) (incf (rectangle-x player) 2))
                       ((is-key-down +key-left+) (decf (rectangle-x player) 2)))

                 ;; Camera target follows player
                 (setf (camera2d-target camera) (vec2 (+ (rectangle-x player) 20) (+ (rectangle-y player) 20)))

                 ;; Camera rotation controls
                 (cond ((is-key-down +key-a+) (decf (camera2d-rotation camera)))
                       ((is-key-down +key-s+) (incf (camera2d-rotation camera))))

                 ;; Limit camera rotation to 80 degrees (-40 to 40)
                 (cond ((> (camera2d-rotation camera) 40) (setf (camera2d-rotation camera) 40.0))
                       ((< (camera2d-rotation camera) -40) (setf (camera2d-rotation camera) -40.0)))

                 ;; Camera zoom controls
                 ;; Uses log scaling to provide consistent zoom speed
                 (setf (camera2d-zoom camera) (exp (+ (log (camera2d-zoom camera)) (* (get-mouse-wheel-move) 0.1))))

                 (cond ((> (camera2d-zoom camera) 3.0) (setf (camera2d-zoom camera) 3.0))
                       ((< (camera2d-zoom camera) 0.1) (setf (camera2d-zoom camera) 0.1)))

                 ;; Camera reset (zoom and rotation)
                 (when (is-key-pressed +key-r+)
                   (setf (camera2d-zoom camera) 1.0
                         (camera2d-rotation camera) 0.0))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-2d camera)

                 (draw-rectangle -6000 320 13000 8000 +darkgray+)

                 (dotimes (i +max-buildings+) (draw-rectangle-rec (aref buildings i) (aref build-colors i)))

                 (draw-rectangle-rec player +red+)

                 (draw-line (truncate (vx (camera2d-target camera))) (* (- screen-height) 10)
                            (truncate (vx (camera2d-target camera))) (* screen-height 10) +green+)
                 (draw-line (* (- screen-width) 10) (truncate (vy (camera2d-target camera)))
                            (* screen-width 10) (truncate (vy (camera2d-target camera))) +green+)

                 (end-mode-2d)

                 (draw-text "SCREEN AREA" 640 10 20 +red+)

                 (draw-rectangle 0 0 screen-width 5 +red+)
                 (draw-rectangle 0 5 5 (- screen-height 10) +red+)
                 (draw-rectangle (- screen-width 5) 5 5 (- screen-height 10) +red+)
                 (draw-rectangle 0 (- screen-height 5) screen-width 5 +red+)

                 (draw-rectangle 10 10 250 113 (fade +skyblue+ 0.5))
                 (draw-rectangle-lines 10 10 250 113 +blue+)

                 (draw-text "Free 2D camera controls:" 20 20 10 +black+)
                 (draw-text "- Right/Left to move player" 40 40 10 +darkgray+)
                 (draw-text "- Mouse Wheel to Zoom in-out" 40 60 10 +darkgray+)
                 (draw-text "- A / S to Rotate" 40 80 10 +darkgray+)
                 (draw-text "- R to reset Zoom and Rotation" 40 100 10 +darkgray+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (close-window)))))              ; Close window and OpenGL context

(main)
