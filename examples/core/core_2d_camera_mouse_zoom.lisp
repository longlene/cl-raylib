;;;; raylib [core] example - 2d camera mouse zoom
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2022-2026 Jeffery Myers (@JeffM2501)
;;;; Common Lisp port of raylib/examples/core/core_2d_camera_mouse_zoom.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-2d-camera-mouse-zoom
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-2d-camera-mouse-zoom)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - 2d camera mouse zoom")

    (let ((camera (make-camera2d :zoom 1.0))
          (zoom-mode 0))                ; 0-Mouse Wheel, 1-Mouse Move

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (cond ((is-key-pressed +key-one+) (setf zoom-mode 0))
                     ((is-key-pressed +key-two+) (setf zoom-mode 1)))

               ;; Translate based on mouse right click
               (when (is-mouse-button-down +mouse-button-left+)
                 (let ((delta (vector2-scale (get-mouse-delta) (/ -1.0 (camera2d-zoom camera)))))
                   (setf (camera2d-target camera) (vector2-add (camera2d-target camera) delta))))

               (if (= zoom-mode 0)
                   ;; Zoom based on mouse wheel
                   (let ((wheel (get-mouse-wheel-move)))
                     (when (/= wheel 0)
                       ;; Get the world point that is under the mouse
                       (let ((mouse-world-pos (get-screen-to-world-2d (get-mouse-position) camera)))
                         ;; Set the offset to where the mouse is
                         (setf (camera2d-offset camera) (get-mouse-position))

                         ;; Set the target to match, so that the camera maps the world space point
                         ;; under the cursor to the screen space point under the cursor at any zoom
                         (setf (camera2d-target camera) mouse-world-pos)

                         ;; Zoom increment
                         ;; Uses log scaling to provide consistent zoom speed
                         (let ((scale (* 0.2 wheel)))
                           (setf (camera2d-zoom camera) (clamp (exp (+ (log (camera2d-zoom camera)) scale)) 0.125 64.0))))))
                   (progn
                     ;; Zoom based on mouse right click
                     (when (is-mouse-button-pressed +mouse-button-right+)
                       ;; Get the world point that is under the mouse
                       (let ((mouse-world-pos (get-screen-to-world-2d (get-mouse-position) camera)))
                         ;; Set the offset to where the mouse is
                         (setf (camera2d-offset camera) (get-mouse-position))

                         ;; Set the target to match, so that the camera maps the world space point
                         ;; under the cursor to the screen space point under the cursor at any zoom
                         (setf (camera2d-target camera) mouse-world-pos)))

                     (when (is-mouse-button-down +mouse-button-right+)
                       ;; Zoom increment
                       ;; Uses log scaling to provide consistent zoom speed
                       (let* ((delta-x (vx (get-mouse-delta)))
                              (scale (* 0.005 delta-x)))
                         (setf (camera2d-zoom camera) (clamp (exp (+ (log (camera2d-zoom camera)) scale)) 0.125 64.0))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               (begin-mode-2d camera)
               ;; Draw the 3d grid, rotated 90 degrees and centered around 0,0
               ;; just so we have something in the XY plane
               (rl-push-matrix)
               (rl-translatef 0.0 (* 25.0 50.0) 0.0)
               (rl-rotatef 90.0 1.0 0.0 0.0)
               (draw-grid 100 50.0)
               (rl-pop-matrix)

               ;; Draw a reference circle
               (draw-circle (floor (get-screen-width) 2) (floor (get-screen-height) 2) 50.0 +maroon+)
               (end-mode-2d)

               ;; Draw mouse reference
               (draw-circle-v (get-mouse-position) 4.0 +darkgray+)
               (draw-text-ex (get-font-default) (text-format "[%i, %i]" (get-mouse-x) (get-mouse-y))
                             (vector2-add (get-mouse-position) (vec2 -44.0 -24.0)) 20.0 2.0 +black+)

               (draw-text "[1][2] Select mouse zoom mode (Wheel or Move)" 20 20 20 +darkgray+)
               (if (= zoom-mode 0)
                   (draw-text "Mouse left button drag to move, mouse wheel to zoom" 20 50 20 +darkgray+)
                   (draw-text "Mouse left button drag to move, mouse press and move to zoom" 20 50 20 +darkgray+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
