;;;; raylib [core] example - 2d camera split screen
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Addapted from the core_3d_camera_split_screen example:
;;;;     https://github.com/raysan5/raylib/blob/master/examples/core/core_3d_camera_split_screen.c
;;;;
;;;; Example originally created with raylib 4.5, last time updated with raylib 4.5
;;;;
;;;; Example contributed by Gabriel dos Santos Sanches (@gabrielssanches) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2023-2025 Gabriel dos Santos Sanches (@gabrielssanches)
;;;; Common Lisp port of raylib/examples/core/core_2d_camera_split_screen.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-2d-camera-split-screen
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-2d-camera-split-screen)

(defconstant +player-size+ 40)

(defun draw-scene (player1 player2 screen-width screen-height)
  ;; Draw full scene
  (dotimes (i (1+ (floor screen-width +player-size+)))
    (draw-line-v (vec2 (float (* +player-size+ i)) 0.0) (vec2 (float (* +player-size+ i)) (float screen-height)) +lightgray+))

  (dotimes (i (1+ (floor screen-height +player-size+)))
    (draw-line-v (vec2 0.0 (float (* +player-size+ i))) (vec2 (float screen-width) (float (* +player-size+ i))) +lightgray+))

  (dotimes (i (floor screen-width +player-size+))
    (dotimes (j (floor screen-height +player-size+))
      (draw-text (text-format "[%i,%i]" i j) (+ 10 (* +player-size+ i)) (+ 15 (* +player-size+ j)) 10 +lightgray+)))

  (draw-rectangle-rec player1 +red+)
  (draw-rectangle-rec player2 +blue+))

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 440))

    (init-window screen-width screen-height "raylib [core] example - 2d camera split screen")

    (let* ((player1 (make-rectangle :x 200.0 :y 200.0 :width (float +player-size+) :height (float +player-size+)))
           (player2 (make-rectangle :x 250.0 :y 200.0 :width (float +player-size+) :height (float +player-size+)))
           (camera1 (make-camera2d :target (vec2 (rectangle-x player1) (rectangle-y player1))
                                   :offset (vec2 200.0 200.0) :rotation 0.0 :zoom 1.0))
           (camera2 (make-camera2d :target (vec2 (rectangle-x player2) (rectangle-y player2))
                                   :offset (vec2 200.0 200.0) :rotation 0.0 :zoom 1.0))
           (screen-camera1 (load-render-texture (floor screen-width 2) screen-height))
           (screen-camera2 (load-render-texture (floor screen-width 2) screen-height))
           ;; Build a flipped rectangle the size of the split view to use for drawing later
           (split-screen-rect (make-rectangle :x 0.0 :y 0.0
                                              :width (float (texture-width (render-texture-texture screen-camera1)))
                                              :height (float (- (texture-height (render-texture-texture screen-camera1)))))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (cond ((is-key-down +key-s+) (incf (rectangle-y player1) 3.0))
                     ((is-key-down +key-w+) (decf (rectangle-y player1) 3.0)))
               (cond ((is-key-down +key-d+) (incf (rectangle-x player1) 3.0))
                     ((is-key-down +key-a+) (decf (rectangle-x player1) 3.0)))

               (cond ((is-key-down +key-up+) (decf (rectangle-y player2) 3.0))
                     ((is-key-down +key-down+) (incf (rectangle-y player2) 3.0)))
               (cond ((is-key-down +key-right+) (incf (rectangle-x player2) 3.0))
                     ((is-key-down +key-left+) (decf (rectangle-x player2) 3.0)))

               (setf (camera2d-target camera1) (vec2 (rectangle-x player1) (rectangle-y player1))
                     (camera2d-target camera2) (vec2 (rectangle-x player2) (rectangle-y player2)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-texture-mode screen-camera1)
               (clear-background +raywhite+)

               (begin-mode-2d camera1)
               ;; Draw full scene with first camera
               (draw-scene player1 player2 screen-width screen-height)
               (end-mode-2d)

               (draw-rectangle 0 0 (floor (get-screen-width) 2) 30 (fade +raywhite+ 0.6))
               (draw-text "PLAYER1: W/S/A/D to move" 10 10 10 +maroon+)

               (end-texture-mode)

               (begin-texture-mode screen-camera2)
               (clear-background +raywhite+)

               (begin-mode-2d camera2)
               ;; Draw full scene with second camera
               (draw-scene player1 player2 screen-width screen-height)
               (end-mode-2d)

               (draw-rectangle 0 0 (floor (get-screen-width) 2) 30 (fade +raywhite+ 0.6))
               (draw-text "PLAYER2: UP/DOWN/LEFT/RIGHT to move" 10 10 10 +darkblue+)

               (end-texture-mode)

               ;; Draw both views render textures to the screen side by side
               (begin-drawing)
               (clear-background +black+)

               (draw-texture-rec (render-texture-texture screen-camera1) split-screen-rect (vec2 0.0 0.0) +white+)
               (draw-texture-rec (render-texture-texture screen-camera2) split-screen-rect (vec2 (/ screen-width 2.0) 0.0) +white+)

               (draw-rectangle (- (floor (get-screen-width) 2) 2) 0 4 (get-screen-height) +lightgray+)
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture screen-camera1) ; Unload render texture
      (unload-render-texture screen-camera2) ; Unload render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
