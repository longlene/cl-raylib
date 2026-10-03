;;;; 3D Graphics Demo for cl-raylib
;;;; This demonstrates comprehensive 3D rendering capabilities

(require :cl-raylib)

(defpackage :cl-raylib-3d-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-3d-demo)

(defun 3d-demo ()
  "Comprehensive 3D graphics demo"
  (let ((screen-width 800)
        (screen-height 600))
    
    ;; Set window flags for better 3D performance
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+ +flag-msaa-4x-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [3D] - 3D Graphics Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      
      ;; Create 3D camera
      (let* ((camera (camera3d-default))
             (cube-position (vec3 0.0 0.0 0.0))
             (sphere-position (vec3 -4.0 0.0 0.0))
             (cylinder-position (vec3 4.0 0.0 0.0))
             (plane-position (vec3 0.0 -2.0 0.0))
             
             ;; Animation variables
             (rotation 0.0)
             (camera-angle 0.0)
             (wave-time 0.0)
             
             ;; Camera control variables
             (camera-distance 15.0)
             (camera-height 5.0))
        
        ;; Set initial camera position
        (camera3d-set-position camera (vec3 0.0 camera-height camera-distance))
        (camera3d-set-target camera (vec3 0.0 0.0 0.0))
        (set-camera-mode camera +camera-orbital+)
        
        (loop until (window-should-close) do
          ;; Update animation
          (incf rotation 1.0)
          (incf camera-angle 0.5)
          (incf wave-time 0.1)
          
          ;; Update camera
          (update-camera camera)
          
          ;; Handle camera control
          (when (is-key-down +key-up+)
            (setf camera-distance (max 5.0 (- camera-distance 0.2))))
          (when (is-key-down +key-down+)
            (setf camera-distance (min 50.0 (+ camera-distance 0.2))))
          (when (is-key-down +key-left+)
            (setf camera-height (+ camera-height 0.1)))
          (when (is-key-down +key-right+)
            (setf camera-height (- camera-height 0.1)))
          
          ;; Reset camera
          (when (is-key-pressed +key-r+)
            (setf camera-distance 15.0)
            (setf camera-height 5.0)
            (camera3d-set-position camera (vec3 0.0 camera-height camera-distance))
            (camera3d-set-target camera (vec3 0.0 0.0 0.0)))
          
          ;; Switch camera modes
          (when (is-key-pressed +key-one+)
            (set-camera-mode camera +camera-orbital+))
          (when (is-key-pressed +key-two+)
            (set-camera-mode camera +camera-free+))
          (when (is-key-pressed +key-three+)
            (set-camera-mode camera +camera-first-person+))
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; 3D drawing
            (with-mode-3d camera
              ;; Draw ground grid
              (draw-grid 20 1.0)
              
              ;; Draw animated cube
              (with-matrix
                (translate-3d (first cube-position) (second cube-position) (third cube-position))
                (rotate-3d rotation 1.0 1.0 0.0)
                (draw-cube-v (vec3 0 0 0) (vec3 2.0 2.0 2.0) +red+)
                (draw-cube-wires-v (vec3 0 0 0) (vec3 2.0 2.0 2.0) +maroon+))
              
              ;; Draw animated sphere
              (let ((animated-y (+ (second sphere-position) (* 1.5 (sin wave-time)))))
                (draw-sphere (vec3 (first sphere-position) animated-y (third sphere-position))
                            1.5 +blue+)
                (draw-sphere-wires (vec3 (first sphere-position) animated-y (third sphere-position))
                                  1.5 8 16 +darkblue+))
              
              ;; Draw animated cylinder
              (with-matrix
                (translate-3d (first cylinder-position) (second cylinder-position) (third cylinder-position))
                (rotate-3d (* rotation 2) 0.0 1.0 0.0)
                (draw-cylinder (vec3 0 0 0) 1.0 0.5 3.0 8 +green+))
              
              ;; Draw textured plane
              (draw-plane plane-position (vec2 20.0 20.0) +lightgray+)
              
              ;; Draw some additional shapes for variety
              (with-matrix
                (translate-3d 2.0 1.0 -2.0)
                (rotate-3d (* rotation 0.5) 0.0 0.0 1.0)
                (draw-cube-wires-v (vec3 0 0 0) (vec3 1.0 3.0 1.0) +purple+))
              
              (with-matrix
                (translate-3d -2.0 1.0 2.0)
                (rotate-3d (* rotation -0.7) 1.0 0.0 1.0)
                (draw-sphere (vec3 0 0 0) 0.8 +orange+))
              
              ;; Draw some 3D lines and points
              (draw-line-3d (vec3 -5.0 0.0 -5.0) (vec3 5.0 5.0 5.0) +yellow+)
              (draw-point-3d (vec3 0.0 3.0 0.0) +magenta+)
              
              ;; Draw camera ray if mouse is pressed
              (when (is-mouse-button-down +mouse-button-left+)
                (let* ((mouse-pos (get-mouse-position))
                       (ray (get-mouse-ray mouse-pos camera)))
                  (draw-ray ray +cyan+))))
            
            ;; 2D UI overlay
            (draw-text "3D Graphics Demo" 10 10 20 +darkgray+)
            (draw-text "Controls:" 10 40 16 +darkgray+)
            (draw-text "- Mouse: Rotate camera (orbital mode)" 10 60 12 +gray+)
            (draw-text "- WASD: Move camera (free/first-person modes)" 10 75 12 +gray+)
            (draw-text "- Arrow Keys: Adjust camera distance/height" 10 90 12 +gray+)
            (draw-text "- 1/2/3: Switch camera modes (Orbital/Free/First-person)" 10 105 12 +gray+)
            (draw-text "- R: Reset camera" 10 120 12 +gray+)
            (draw-text "- Left Mouse: Show camera ray" 10 135 12 +gray+)
            (draw-text "- ESC: Exit" 10 150 12 +gray+)
            
            ;; Display 3D info
            (let ((info-y 180))
              (draw-text (format nil "Camera Mode: ~a" 
                                (case *camera-mode*
                                  (+camera-orbital+ "Orbital")
                                  (+camera-free+ "Free")
                                  (+camera-first-person+ "First Person")
                                  (t "Custom"))) 
                        10 info-y 12 +blue+)
              (draw-text (format nil "Camera Position: ~,1f, ~,1f, ~,1f" 
                                (first (camera3d-position camera))
                                (second (camera3d-position camera))
                                (third (camera3d-position camera))) 
                        10 (+ info-y 15) 12 +blue+)
              (draw-text (format nil "Camera Target: ~,1f, ~,1f, ~,1f" 
                                (first (camera3d-target camera))
                                (second (camera3d-target camera))
                                (third (camera3d-target camera))) 
                        10 (+ info-y 30) 12 +blue+)
              (draw-text (format nil "Distance: ~,1f" camera-distance) 10 (+ info-y 45) 12 +blue+)
              (draw-text (format nil "Height: ~,1f" camera-height) 10 (+ info-y 60) 12 +blue+))
            
            ;; Draw performance info
            (draw-text (format nil "FPS: ~d" 60) ; Placeholder FPS
                      (- screen-width 80) 10 16 +lime+)
            
            ;; Draw 3D coordinate system indicator (mini axes)
            (let ((axes-size 50)
                  (axes-x (- screen-width 70))
                  (axes-y (- screen-height 70)))
              (draw-line axes-x axes-y (+ axes-x axes-size) axes-y +red+)     ; X axis
              (draw-line axes-x axes-y axes-x (- axes-y axes-size) +green+)   ; Y axis  
              (draw-text "X" (+ axes-x axes-size 5) (- axes-y 5) 12 +red+)
              (draw-text "Y" (- axes-x 15) (- axes-y axes-size 5) 12 +green+)
              (draw-text "Z" (- axes-x 15) (+ axes-y 15) 12 +blue+)))
        
        ;; Cleanup
        (cleanup-texture-system)))))

;; Run the demo
(3d-demo)