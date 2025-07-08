;;;; core_3d_camera_free.lisp
;;;; 
;;;; cl-raylib [core] example - Initialize 3d camera free
;;;;
;;;; Translation of raylib's core_3d_camera_free.c example
;;;; This example demonstrates free 3D camera movement
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

(defpackage :cl-raylib-example-core-3d-camera-free
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-3d-camera-free)

(defun core-3d-camera-free ()
  "3D Camera free example - equivalent to raylib's core_3d_camera_free"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 3d camera free")
      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec3 10.0 10.0 10.0)  ; Camera position
                                   :target (vec3 0.0 0.0 0.0)       ; Camera looking at point
                                   :up (vec3 0.0 1.0 0.0)           ; Camera up vector (rotation towards target)
                                   :fovy 45.0                       ; Camera field-of-view Y
                                   :projection :camera-perspective)) ; Camera projection type
            (cube-position (vec3 0.0 0.0 0.0)))
        
        ;; (disable-cursor)  ; Don't disable cursor for testing
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          
          ;; Update
          (update-camera camera :camera-free)
          
          ;; Debug: Print camera position when keys are pressed
          (when (is-key-down :key-w)
            (format t "W pressed - Camera pos: ~A~%" (camera3d-position camera)))
          (when (is-key-down :key-s)
            (format t "S pressed - Camera pos: ~A~%" (camera3d-position camera)))
          
          ;; Reset camera target to center when 'Z' is pressed
          (when (is-key-pressed :key-z)
            (setf (camera3d-target camera) (vec3 0.0 0.0 0.0)))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (with-mode-3d (camera)
              (draw-cube cube-position 2.0 2.0 2.0 +red+)
              (draw-cube-wires cube-position 2.0 2.0 2.0 +maroon+)
              
              (draw-grid 10 1.0))
            
            ;; UI
            (draw-rectangle 10 10 320 93 (color-fade +skyblue+ 0.5))
            (draw-rectangle-lines 10 10 320 93 +blue+)
            
            (draw-text "Free camera default controls:" 20 20 10 +black+)
            (draw-text "- Mouse Wheel to Zoom in-out" 40 40 10 +darkgray+)
            (draw-text "- Mouse Wheel Pressed to Pan" 40 60 10 +darkgray+)
            (draw-text "- Z to zoom to (0, 0, 0)" 40 80 10 +darkgray+)))))))

;; Run the example
(core-3d-camera-free)