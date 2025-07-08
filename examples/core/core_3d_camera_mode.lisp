;;;; core_3d_camera_mode.lisp
;;;; 
;;;; cl-raylib [core] example - Initialize 3d camera mode
;;;;
;;;; Translation of raylib's core_3d_camera_mode.c example
;;;; This example demonstrates basic 3D camera setup and rendering
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

(defpackage :cl-raylib-example-core-3d-camera-mode
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-3d-camera-mode)

(defun core-3d-camera-mode ()
  "3D Camera mode example - equivalent to raylib's core_3d_camera_mode"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 3d camera mode")
      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec3 0.0 10.0 10.0)  ; Camera position
                                   :target (vec3 0.0 0.0 0.0)      ; Camera looking at point
                                   :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                   :fovy 45.0                      ; Camera field-of-view Y
                                   :projection :camera-perspective)) ; Camera mode type
            (cube-position (vec3 0.0 0.0 0.0)))
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          ;; TODO: Update your variables here
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (with-mode-3d (camera)
              (draw-cube cube-position 2.0 2.0 2.0 +red+)
              (draw-cube-wires cube-position 2.0 2.0 2.0 +maroon+)
              
              (draw-grid 10 1.0))
            
            (draw-text "Welcome to the third dimension!" 10 40 20 +darkgray+)
            
            (draw-fps 10 10)))))))

;; Run the example
(core-3d-camera-mode)