;;;; core_2d_camera.lisp
;;;; 
;;;; cl-raylib [core] example - 2D Camera system
;;;;
;;;; Translation of raylib's core_2d_camera.c example
;;;; This example demonstrates 2D camera functionality with building generation
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-2d-camera
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-2d-camera)

(defconstant +max-buildings+ 100)

(defun core-2d-camera ()
  "2D Camera system example - equivalent to raylib's core_2d_camera"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 2d camera")
      (let ((player (make-rectangle :x 400.0 :y 280.0 :width 40.0 :height 40.0))
            (buildings (make-array +max-buildings+))
            (build-colors (make-array +max-buildings+))
            (spacing 0.0))
        
        ;; Generate random buildings
        (dotimes (i +max-buildings+)
          (let ((width (float (get-random-value 50 200)))
                (height (float (get-random-value 100 800))))
            (setf (aref buildings i) 
                  (make-rectangle :x (+ -6000.0 spacing)
                                  :y (- (float screen-height) 130.0 height)
                                  :width width
                                  :height height))
            (incf spacing width)
            (setf (aref build-colors i)
                  (make-color (get-random-value 200 240)
                              (get-random-value 200 240)
                              (get-random-value 200 250)
                              255))))
        
        ;; Initialize camera
        (let ((camera (make-camera2d :target (vec2 (+ (rectangle-x player) 20.0)
                                                    (+ (rectangle-y player) 20.0))
                                     :offset (vec2 (/ (float screen-width) 2.0)
                                                   (/ (float screen-height) 2.0))
                                     :rotation 0.0
                                     :zoom 1.0)))
          
          (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
          
          ;; Main game loop
          (loop until (window-should-close) do  ; Detect window close button or ESC key
            
            ;; Update
            ;; Player movement
            (cond
              ((is-key-down :key-right) (incf (rectangle-x player) 2.0))
              ((is-key-down :key-left) (incf (rectangle-x player) -2.0)))
            
            ;; Camera target follows player
            (setf (camera2d-target camera) 
                  (vec2 (+ (rectangle-x player) 20.0)
                        (+ (rectangle-y player) 20.0)))
            
            ;; Camera rotation controls
            (cond
              ((is-key-down :key-a) (incf (camera2d-rotation camera) -1.0))
              ((is-key-down :key-s) (incf (camera2d-rotation camera) 1.0)))
            
            ;; Limit camera rotation to 80 degrees (-40 to 40)
            (when (> (camera2d-rotation camera) 40.0)
              (setf (camera2d-rotation camera) 40.0))
            (when (< (camera2d-rotation camera) -40.0)
              (setf (camera2d-rotation camera) -40.0))
            
            ;; Camera zoom controls
            ;; Uses log scaling to provide consistent zoom speed
            (let ((wheel-move (get-mouse-wheel-move)))
              (when (/= wheel-move 0)
                (setf (camera2d-zoom camera) 
                      (exp (+ (log (camera2d-zoom camera)) (* wheel-move 0.1))))))
            
            ;; Limit zoom
            (when (> (camera2d-zoom camera) 3.0)
              (setf (camera2d-zoom camera) 3.0))
            (when (< (camera2d-zoom camera) 0.1)
              (setf (camera2d-zoom camera) 0.1))
            
            ;; Camera reset (zoom and rotation)
            (when (is-key-pressed :key-r)
              (setf (camera2d-zoom camera) 1.0)
              (setf (camera2d-rotation camera) 0.0))
            
            ;; Draw
            (with-drawing
              (clear-background +raywhite+)
              
              (with-mode-2d (camera)
                ;; Ground
                (draw-rectangle -6000 320 13000 8000 +darkgray+)
                
                ;; Buildings
                (dotimes (i +max-buildings+)
                  (draw-rectangle-rec (aref buildings i) (aref build-colors i)))
                
                ;; Player
                (draw-rectangle-rec player +red+)
                
                ;; Camera crosshair
                (draw-line (floor (vx2 (camera2d-target camera))) 
                           (- (* screen-height 10))
                           (floor (vx2 (camera2d-target camera)))
                           (* screen-height 10)
                           +green+)
                (draw-line (- (* screen-width 10))
                           (floor (vy2 (camera2d-target camera)))
                           (* screen-width 10)
                           (floor (vy2 (camera2d-target camera)))
                           +green+))
              
              ;; UI
              (draw-text "SCREEN AREA" 640 10 20 +red+)
              
              ;; Screen border
              (draw-rectangle 0 0 screen-width 5 +red+)
              (draw-rectangle 0 5 5 (- screen-height 10) +red+)
              (draw-rectangle (- screen-width 5) 5 5 (- screen-height 10) +red+)
              (draw-rectangle 0 (- screen-height 5) screen-width 5 +red+)
              
              ;; Info panel
              (draw-rectangle 10 10 250 113 (color-fade +skyblue+ 0.5))
              (draw-rectangle-lines 10 10 250 113 +blue+)
              
              (draw-text "Free 2d camera controls:" 20 20 10 +black+)
              (draw-text "- Right/Left to move Offset" 40 40 10 +darkgray+)
              (draw-text "- Mouse Wheel to Zoom in-out" 40 60 10 +darkgray+)
              (draw-text "- A / S to Rotate" 40 80 10 +darkgray+)
              (draw-text "- R to reset Zoom and Rotation" 40 100 10 +darkgray+))))))))

;; Run the example
(core-2d-camera)