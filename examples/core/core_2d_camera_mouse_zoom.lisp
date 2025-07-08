;;;; core_2d_camera_mouse_zoom.lisp
;;;; 
;;;; cl-raylib [core] example - 2d camera mouse zoom
;;;;
;;;; Translation of raylib's core_2d_camera_mouse_zoom.c example
;;;; This example demonstrates 2D camera mouse zoom functionality
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

(defpackage :cl-raylib-example-core-2d-camera-mouse-zoom
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-2d-camera-mouse-zoom)

(defun core-2d-camera-mouse-zoom ()
  "2D Camera mouse zoom example - equivalent to raylib's core_2d_camera_mouse_zoom"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 2d camera mouse zoom")
      (let ((camera (make-camera2d :offset (vec2 0 0)
                                   :target (vec2 0 0)
                                   :rotation 0.0
                                   :zoom 1.0))
            (zoom-mode 0))  ; 0-Mouse Wheel, 1-Mouse Move
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (when (is-key-pressed :key-one)
            (setf zoom-mode 0))
          (when (is-key-pressed :key-two)
            (setf zoom-mode 1))
          
          ;; Translate based on mouse left click
          (when (is-mouse-button-down :mouse-button-left)
            (let* ((delta (get-mouse-delta))
                   (scaled-delta (3d-vectors:v* delta (/ -1.0 (camera2d-zoom camera))))
                   (new-target (3d-vectors:v+ (camera2d-target camera) scaled-delta)))
              (setf (camera2d-target camera) new-target)))
          
          (if (= zoom-mode 0)
            ;; Zoom based on mouse wheel
            (let ((wheel (get-mouse-wheel-move)))
              (when (/= wheel 0)
                ;; Get the world point that is under the mouse
                (let* ((mouse-pos (get-mouse-position))
                       (mouse-world-pos (get-screen-to-world-2d mouse-pos camera)))
                  ;; Set the offset to where the mouse is
                  (setf (camera2d-offset camera) mouse-pos)
                  
                  ;; Set the target to match, so that the camera maps the world space point 
                  ;; under the cursor to the screen space point under the cursor at any zoom
                  (setf (camera2d-target camera) mouse-world-pos)
                  
                  ;; Zoom increment
                  ;; Uses log scaling to provide consistent zoom speed
                  (let* ((scale (* 0.2 wheel))
                         (new-zoom (exp (+ (log (camera2d-zoom camera)) scale))))
                    (setf (camera2d-zoom camera) (clamp new-zoom 0.125 64.0))))))
            
            ;; Zoom based on mouse right click
            (progn
              (when (is-mouse-button-pressed :mouse-button-right)
                ;; Get the world point that is under the mouse
                (let* ((mouse-pos (get-mouse-position))
                       (mouse-world-pos (get-screen-to-world-2d mouse-pos camera)))
                  ;; Set the offset to where the mouse is
                  (setf (camera2d-offset camera) mouse-pos)
                  
                  ;; Set the target to match, so that the camera maps the world space point 
                  ;; under the cursor to the screen space point under the cursor at any zoom
                  (setf (camera2d-target camera) mouse-world-pos)))
              
              (when (is-mouse-button-down :mouse-button-right)
                ;; Zoom increment
                ;; Uses log scaling to provide consistent zoom speed
                (let* ((mouse-delta (get-mouse-delta))
                       (delta-x (3d-vectors:vx mouse-delta))
                       (scale (* 0.005 delta-x))
                       (new-zoom (exp (+ (log (camera2d-zoom camera)) scale))))
                  (setf (camera2d-zoom camera) (clamp new-zoom 0.125 64.0))))))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (with-mode-2d (camera)
              ;; Draw simple shapes for testing
              ;; Draw a grid pattern
              (loop for x from -1000 to 1000 by 100 do
                (draw-line x -1000 x 1000 +lightgray+))
              (loop for y from -1000 to 1000 by 100 do
                (draw-line -1000 y 1000 y +lightgray+))
              
              ;; Draw coordinate axes
              (draw-line -1000 0 1000 0 +red+)   ; X axis
              (draw-line 0 -1000 0 1000 +green+) ; Y axis
              
              ;; Draw some test shapes
              (draw-rectangle -50 -50 100 100 +blue+)
              (draw-circle 100 100 30 +maroon+)
              (draw-rectangle 200 200 100 100 +purple+))
            
            ;; Draw mouse reference
            (let* ((mouse-pos (get-mouse-position))
                   (x (3d-vectors:vx mouse-pos))
                   (y (3d-vectors:vy mouse-pos)))
              (draw-circle x y 4 +darkgray+)
              (draw-text (text-format "[%d, %d]" (round x) (round y))
                        (round (+ x -44)) (round (+ y -24)) 20 +black+))
            
            (draw-text "[1][2] Select mouse zoom mode (Wheel or Move)" 20 20 20 +darkgray+)
            (if (= zoom-mode 0)
              (draw-text "Mouse left button drag to move, mouse wheel to zoom" 20 50 20 +darkgray+)
              (draw-text "Mouse left button drag to move, mouse press and move to zoom" 20 50 20 +darkgray+))))))))

;; Run the example
(core-2d-camera-mouse-zoom)
