;;;; core_3d_camera_first_person.lisp
;;;; 
;;;; cl-raylib [core] example - 3d camera first person
;;;;
;;;; Translation of raylib's core_3d_camera_first_person.c example
;;;; This example demonstrates first person 3D camera controls
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

(defpackage :cl-raylib-example-core-3d-camera-first-person
  (:use :cl :cl-raylib))

;; Temporary fix: define draw-plane if it's not available
(unless (fboundp 'cl-raylib:draw-plane)
  (defun cl-raylib:draw-plane (center-pos size color)
    "Draw a plane (horizontal by default)"
    (cl-raylib::set-gl-color color)
    (let* ((cx (if (cl-raylib::vec3-p center-pos) (cl-raylib::vx3 center-pos) (first center-pos)))
           (cy (if (cl-raylib::vec3-p center-pos) (cl-raylib::vy3 center-pos) (second center-pos)))
           (cz (if (cl-raylib::vec3-p center-pos) (cl-raylib::vz3 center-pos) (third center-pos)))
           (sx (if (cl-raylib::vec2-p size) (cl-raylib::vx2 size) (first size)))
           (sz (if (cl-raylib::vec2-p size) (cl-raylib::vy2 size) (second size)))
           (half-x (/ sx 2.0))
           (half-z (/ sz 2.0)))
      (gl:with-primitive :triangles
        (gl:normal 0.0 1.0 0.0)
        
        ;; Triangle 1
        (gl:vertex (- cx half-x) cy (- cz half-z))
        (gl:vertex (+ cx half-x) cy (- cz half-z))
        (gl:vertex (+ cx half-x) cy (+ cz half-z))
        
        ;; Triangle 2
        (gl:vertex (- cx half-x) cy (- cz half-z))
        (gl:vertex (+ cx half-x) cy (+ cz half-z))
        (gl:vertex (- cx half-x) cy (+ cz half-z))))))

(in-package :cl-raylib-example-core-3d-camera-first-person)

(defconstant +max-columns+ 20)

(defun core-3d-camera-first-person ()
  "3D Camera first person example - equivalent to raylib's core_3d_camera_first_person"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 3d camera first person")
      ;; Define the camera to look into our 3d world
      (let ((camera (make-camera3d :position (vec3 0.0 2.0 4.0)    ; Camera position
                                   :target (vec3 0.0 2.0 0.0)      ; Camera looking at point
                                   :up (vec3 0.0 1.0 0.0)          ; Camera up vector (rotation towards target)
                                   :fovy 60.0                      ; Camera field-of-view Y
                                   :projection :camera-perspective)) ; Camera projection type
            (camera-mode :camera-first-person)
            (heights (make-array +max-columns+ :element-type 'single-float))
            (positions (make-array +max-columns+))
            (colors (make-array +max-columns+)))
        
        ;; Generate some random columns
        (loop for i from 0 below +max-columns+ do
          (setf (aref heights i) (float (get-random-value 1 12)))
          (setf (aref positions i) (vec3 (float (get-random-value -15 15))
                                         (/ (aref heights i) 2.0)
                                         (float (get-random-value -15 15))))
          (setf (aref colors i) (make-color (get-random-value 20 255)
                                            (get-random-value 10 55)
                                            30
                                            255)))
        
        (disable-cursor)  ; Limit cursor to relative movement inside the window
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          ;; Update
          ;; Switch camera mode
          (when (is-key-pressed :key-one)
            (setf camera-mode :camera-free)
            (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll
          
          (when (is-key-pressed :key-two)
            (setf camera-mode :camera-first-person)
            (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll
          
          (when (is-key-pressed :key-three)
            (setf camera-mode :camera-third-person)
            (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll
          
          (when (is-key-pressed :key-four)
            (setf camera-mode :camera-orbital)
            (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))) ; Reset roll
          
          ;; Switch camera projection
          (when (is-key-pressed :key-p)
            (if (eq (camera3d-projection camera) +camera-perspective+)
              ;; Create isometric view
              (progn
                (setf camera-mode :camera-third-person)
                ;; Note: The target distance is related to the render distance in the orthographic projection
                (setf (camera3d-position camera) (vec3 0.0 2.0 -100.0))
                (setf (camera3d-target camera) (vec3 0.0 2.0 0.0))
                (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))
                (setf (camera3d-projection camera) +camera-orthographic+)
                (setf (camera3d-fovy camera) 20.0) ; near plane width in CAMERA_ORTHOGRAPHIC
                (camera3d-rotate-yaw camera (* -135 (degrees-to-radians 1.0)) t)
                (camera3d-rotate-pitch camera (* -45 (degrees-to-radians 1.0)) t t nil))
              ;; Reset to default view
              (progn
                (setf camera-mode :camera-third-person)
                (setf (camera3d-position camera) (vec3 0.0 2.0 10.0))
                (setf (camera3d-target camera) (vec3 0.0 2.0 0.0))
                (setf (camera3d-up camera) (vec3 0.0 1.0 0.0))
                (setf (camera3d-projection camera) +camera-perspective+)
                (setf (camera3d-fovy camera) 60.0))))
          
          ;; Update camera computes movement internally depending on the camera mode
          ;; Some default standard keyboard/mouse inputs are hardcoded to simplify use
          ;; For advanced camera controls, it's recommended to compute camera movement manually
          (update-camera camera camera-mode)  ; Update camera
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (with-mode-3d (camera)
              (draw-plane (vec3 0.0 0.0 0.0) (vec2 32.0 32.0) +lightgray+) ; Draw ground
              (draw-cube (vec3 -16.0 2.5 0.0) 1.0 5.0 32.0 +blue+)     ; Draw a blue wall
              (draw-cube (vec3 16.0 2.5 0.0) 1.0 5.0 32.0 +lime+)      ; Draw a green wall
              (draw-cube (vec3 0.0 2.5 16.0) 32.0 5.0 1.0 +gold+)      ; Draw a yellow wall
              
              ;; Draw some cubes around
              (loop for i from 0 below +max-columns+ do
                (draw-cube (aref positions i) 2.0 (aref heights i) 2.0 (aref colors i))
                (draw-cube-wires (aref positions i) 2.0 (aref heights i) 2.0 +maroon+))
              
              ;; Draw player cube
              (when (eq camera-mode :camera-third-person)
                (draw-cube (camera3d-target camera) 0.5 0.5 0.5 +purple+)
                (draw-cube-wires (camera3d-target camera) 0.5 0.5 0.5 +darkpurple+)))
            
            ;; Draw info boxes
            (draw-rectangle 5 5 330 100 (color-fade +skyblue+ 0.5))
            (draw-rectangle-lines 5 5 330 100 +blue+)
            
            (draw-text "Camera controls:" 15 15 10 +black+)
            (draw-text "- Move keys: W, A, S, D, Space, Left-Ctrl" 15 30 10 +black+)
            (draw-text "- Look around: arrow keys or mouse" 15 45 10 +black+)
            (draw-text "- Camera mode keys: 1, 2, 3, 4" 15 60 10 +black+)
            (draw-text "- Zoom keys: num-plus, num-minus or mouse scroll" 15 75 10 +black+)
            (draw-text "- Camera projection key: P" 15 90 10 +black+)
            
            (draw-rectangle 600 5 195 100 (color-fade +skyblue+ 0.5))
            (draw-rectangle-lines 600 5 195 100 +blue+)
            
            (draw-text "Camera status:" 610 15 10 +black+)
            (let ((mode-text (case camera-mode
                               (:camera-free "FREE")
                               (:camera-first-person "FIRST_PERSON")
                               (:camera-third-person "THIRD_PERSON")
                               (:camera-orbital "ORBITAL")
                               (t "CUSTOM")))
                  (proj-text (if (eq (camera3d-projection camera) +camera-perspective+)
                               "PERSPECTIVE"
                               "ORTHOGRAPHIC"))
                  (pos (camera3d-position camera))
                  (target (camera3d-target camera))
                  (up (camera3d-up camera)))
              (draw-text (text-format "- Mode: %s" mode-text) 610 30 10 +black+)
              (draw-text (text-format "- Projection: %s" proj-text) 610 45 10 +black+)
              (draw-text (text-format "- Position: (%06.3f, %06.3f, %06.3f)" 
                                     (3d-vectors:vx pos) (3d-vectors:vy pos) (3d-vectors:vz pos)) 610 60 10 +black+)
              (draw-text (text-format "- Target: (%06.3f, %06.3f, %06.3f)" 
                                     (3d-vectors:vx target) (3d-vectors:vy target) (3d-vectors:vz target)) 610 75 10 +black+)
              (draw-text (text-format "- Up: (%06.3f, %06.3f, %06.3f)" 
                                     (3d-vectors:vx up) (3d-vectors:vy up) (3d-vectors:vz up)) 610 90 10 +black+))))))))

;; Run the example
(core-3d-camera-first-person)
