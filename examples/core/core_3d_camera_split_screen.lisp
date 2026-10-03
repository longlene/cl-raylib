;;;; raylib [core] example - 3d camera split screen
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 3.7, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2021-2025 Jeffery Myers (@JeffM2501)
;;;; Common Lisp port of raylib/examples/core/core_3d_camera_split_screen.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-3d-camera-split-screen
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-3d-camera-split-screen)

(defun draw-scene (camera-player1 camera-player2 count spacing)
  ;; Draw scene: grid of cube trees on a plane to make a "world"
  (draw-plane (vec3 0.0 0.0 0.0) (vec2 50.0 50.0) +beige+) ; Simple world plane

  (loop for x = (* (- count) spacing) then (+ x spacing)
        while (<= x (* count spacing))
        do (loop for z = (* (- count) spacing) then (+ z spacing)
                 while (<= z (* count spacing))
                 do (draw-cube (vec3 x 1.5 z) 1.0 1.0 1.0 +lime+)
                    (draw-cube (vec3 x 0.5 z) 0.25 1.0 0.25 +brown+)))

  ;; Draw a cube at each player's position
  (draw-cube (camera3d-position camera-player1) 1.0 1.0 1.0 +red+)
  (draw-cube (camera3d-position camera-player2) 1.0 1.0 1.0 +blue+))

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 3d camera split screen")

    ;; Setup player 1 camera and screen
    (let* ((camera-player1 (make-camera3d :fovy 45.0 :up (vec3 0.0 1.0 0.0) :target (vec3 0.0 1.0 0.0)
                                          :position (vec3 0.0 1.0 -3.0)))
           (screen-player1 (load-render-texture (floor screen-width 2) screen-height))
           ;; Setup player two camera and screen
           (camera-player2 (make-camera3d :fovy 45.0 :up (vec3 0.0 1.0 0.0) :target (vec3 0.0 3.0 0.0)
                                          :position (vec3 -3.0 3.0 0.0)))
           (screen-player2 (load-render-texture (floor screen-width 2) screen-height))
           ;; Build a flipped rectangle the size of the split view to use for drawing later
           (split-screen-rect (make-rectangle :x 0.0 :y 0.0
                                              :width (float (texture-width (render-texture-texture screen-player1)))
                                              :height (float (- (texture-height (render-texture-texture screen-player1))))))
           ;; Grid data
           (count 5)
           (spacing 4.0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; If anyone moves this frame, how far will they move based on the time since the last frame
               ;; this moves things at 10 world units per second, regardless of the actual FPS
               (let ((offset-this-frame (* 10.0 (get-frame-time))))

                 ;; Move Player1 forward and backwards (no turning)
                 (cond ((is-key-down +key-w+)
                        (incf (vz (camera3d-position camera-player1)) offset-this-frame)
                        (incf (vz (camera3d-target camera-player1)) offset-this-frame))
                       ((is-key-down +key-s+)
                        (decf (vz (camera3d-position camera-player1)) offset-this-frame)
                        (decf (vz (camera3d-target camera-player1)) offset-this-frame)))

                 ;; Move Player2 forward and backwards (no turning)
                 (cond ((is-key-down +key-up+)
                        (incf (vx (camera3d-position camera-player2)) offset-this-frame)
                        (incf (vx (camera3d-target camera-player2)) offset-this-frame))
                       ((is-key-down +key-down+)
                        (decf (vx (camera3d-position camera-player2)) offset-this-frame)
                        (decf (vx (camera3d-target camera-player2)) offset-this-frame))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               ;; Draw Player1 view to the render texture
               (begin-texture-mode screen-player1)
               (clear-background +skyblue+)

               (begin-mode-3d camera-player1)
               (draw-scene camera-player1 camera-player2 count spacing)
               (end-mode-3d)

               (draw-rectangle 0 0 (floor (get-screen-width) 2) 40 (fade +raywhite+ 0.8))
               (draw-text "PLAYER1: W/S to move" 10 10 20 +maroon+)

               (end-texture-mode)

               ;; Draw Player2 view to the render texture
               (begin-texture-mode screen-player2)
               (clear-background +skyblue+)

               (begin-mode-3d camera-player2)
               (draw-scene camera-player1 camera-player2 count spacing)
               (end-mode-3d)

               (draw-rectangle 0 0 (floor (get-screen-width) 2) 40 (fade +raywhite+ 0.8))
               (draw-text "PLAYER2: UP/DOWN to move" 10 10 20 +darkblue+)

               (end-texture-mode)

               ;; Draw both views render textures to the screen side by side
               (begin-drawing)
               (clear-background +black+)

               (draw-texture-rec (render-texture-texture screen-player1) split-screen-rect (vec2 0.0 0.0) +white+)
               (draw-texture-rec (render-texture-texture screen-player2) split-screen-rect (vec2 (/ screen-width 2.0) 0.0) +white+)

               (draw-rectangle (- (floor (get-screen-width) 2) 2) 0 4 (get-screen-height) +lightgray+)
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture screen-player1) ; Unload render texture
      (unload-render-texture screen-player2) ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
