;;;; core_2d_camera_split_screen.lisp
;;;; 
;;;; cl-raylib [core] example - 2d camera split screen
;;;;
;;;; Translation of raylib's core_2d_camera_split_screen.c example
;;;; This example demonstrates split screen rendering with two 2D cameras
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Adapted from the core_3d_camera_split_screen example
;;;; 
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-2d-camera-split-screen
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-2d-camera-split-screen)

(defconstant +player-size+ 40)

(defun core-2d-camera-split-screen ()
  "2D Camera split screen example - equivalent to raylib's core_2d_camera_split_screen"
  (let ((screen-width 800)
        (screen-height 440))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - 2d camera split screen")
      (let ((player1 (make-rectangle :x 200.0 :y 200.0 :width (float +player-size+) :height (float +player-size+)))
            (player2 (make-rectangle :x 250.0 :y 200.0 :width (float +player-size+) :height (float +player-size+)))
            
            ;; Create cameras for each player
            (camera1 (make-camera2d :target (vec2 200 200)
                                    :offset (vec2 200.0 200.0)
                                    :rotation 0.0
                                    :zoom 1.0))
            (camera2 (make-camera2d :target (vec2 250 200)
                                    :offset (vec2 200.0 200.0)
                                    :rotation 0.0
                                    :zoom 1.0)))
        
        ;; Create render textures for split screen
        (let ((screen-camera1 (load-render-texture (/ screen-width 2) screen-height))
              (screen-camera2 (load-render-texture (/ screen-width 2) screen-height)))
          
          ;; Build a flipped rectangle the size of the split view to use for drawing later
          (let ((split-screen-rect (make-rectangle :x 0.0 
                                                   :y 0.0 
                                                   :width (float (texture-width (render-texture-texture screen-camera1)))
                                                   :height (- (float (texture-height (render-texture-texture screen-camera1)))))))
            
            (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
            
            ;; Main game loop
            (loop until (window-should-close) do  ; Detect window close button or ESC key
              
              ;; Update
              ;; Player 1 movement (WASD)
              (when (is-key-down :key-s)
                (incf (rectangle-y player1) 3.0))
              (when (is-key-down :key-w)
                (decf (rectangle-y player1) 3.0))
              (when (is-key-down :key-d)
                (incf (rectangle-x player1) 3.0))
              (when (is-key-down :key-a)
                (decf (rectangle-x player1) 3.0))
              
              ;; Player 2 movement (Arrow keys)
              (when (is-key-down :key-up)
                (decf (rectangle-y player2) 3.0))
              (when (is-key-down :key-down)
                (incf (rectangle-y player2) 3.0))
              (when (is-key-down :key-right)
                (incf (rectangle-x player2) 3.0))
              (when (is-key-down :key-left)
                (decf (rectangle-x player2) 3.0))
              
              ;; Update camera targets to follow players
              (setf (camera2d-target camera1) (vec2 (rectangle-x player1) (rectangle-y player1)))
              (setf (camera2d-target camera2) (vec2 (rectangle-x player2) (rectangle-y player2)))
              
              ;; Draw
              ;; Render first camera view
              (begin-texture-mode screen-camera1)
              (clear-background +raywhite+)
              
              (with-mode-2d (camera1)
                ;; Draw full scene with first camera
                ;; Draw vertical grid lines
                (loop for i from 0 to (/ screen-width +player-size+) do
                  (draw-line-v (vec2 (* +player-size+ i) 0) 
                               (vec2 (* +player-size+ i) screen-height) 
                               +lightgray+))
                
                ;; Draw horizontal grid lines
                (loop for i from 0 to (/ screen-height +player-size+) do
                  (draw-line-v (vec2 0 (* +player-size+ i))
                               (vec2 screen-width (* +player-size+ i))
                               +lightgray+))
                
                ;; Draw grid coordinates
                (loop for i from 0 below (/ screen-width +player-size+) do
                  (loop for j from 0 below (/ screen-height +player-size+) do
                    (draw-text (text-format "[%d,%d]" i j)
                              (+ 10 (* +player-size+ i))
                              (+ 15 (* +player-size+ j))
                              10 +lightgray+)))
                
                ;; Draw players
                (draw-rectangle-rec player1 +red+)
                (draw-rectangle-rec player2 +blue+))
              
              ;; Draw UI for player 1
              (draw-rectangle 0 0 (/ (get-screen-width) 2) 30 (color-fade +raywhite+ 0.6))
              (draw-text "PLAYER1: W/S/A/D to move" 10 10 10 +maroon+)
              
              (end-texture-mode)
              
              ;; Render second camera view
              (begin-texture-mode screen-camera2)
              (clear-background +raywhite+)
              
              (with-mode-2d (camera2)
                ;; Draw full scene with second camera
                ;; Draw vertical grid lines
                (loop for i from 0 to (/ screen-width +player-size+) do
                  (draw-line-v (vec2 (* +player-size+ i) 0)
                               (vec2 (* +player-size+ i) screen-height)
                               +lightgray+))
                
                ;; Draw horizontal grid lines
                (loop for i from 0 to (/ screen-height +player-size+) do
                  (draw-line-v (vec2 0 (* +player-size+ i))
                               (vec2 screen-width (* +player-size+ i))
                               +lightgray+))
                
                ;; Draw grid coordinates
                (loop for i from 0 below (/ screen-width +player-size+) do
                  (loop for j from 0 below (/ screen-height +player-size+) do
                    (draw-text (text-format "[%d,%d]" i j)
                              (+ 10 (* +player-size+ i))
                              (+ 15 (* +player-size+ j))
                              10 +lightgray+)))
                
                ;; Draw players
                (draw-rectangle-rec player1 +red+)
                (draw-rectangle-rec player2 +blue+))
              
              ;; Draw UI for player 2
              (draw-rectangle 0 0 (/ (get-screen-width) 2) 30 (color-fade +raywhite+ 0.6))
              (draw-text "PLAYER2: UP/DOWN/LEFT/RIGHT to move" 10 10 10 +darkblue+)
              
              (end-texture-mode)
              
              ;; Draw both views render textures to the screen side by side
              (with-drawing
                (clear-background +black+)
                
                ;; Draw left view (camera1)
                (draw-texture-rec (render-texture-texture screen-camera1) 
                                  split-screen-rect 
                                  (vec2 0 0) 
                                  +white+)
                
                ;; Draw right view (camera2) 
                (draw-texture-rec (render-texture-texture screen-camera2)
                                  split-screen-rect
                                  (vec2 (/ screen-width 2.0) 0)
                                  +white+)
                
                ;; Draw separator line
                (draw-rectangle (- (/ (get-screen-width) 2) 2) 0 4 (get-screen-height) +lightgray+)))
            
            ;; Cleanup
            (unload-render-texture screen-camera1)
            (unload-render-texture screen-camera2)))))))

;; Run the example
(core-2d-camera-split-screen)