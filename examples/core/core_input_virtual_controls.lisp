;;;; raylib [core] example - input virtual controls
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example contributed by GreenSnakeLinux (@GreenSnakeLinux),
;;;; reviewed by Ramon Santamaria (@raysan5), oblerion (@oblerion) and danilwhale (@danilwhale)
;;;;
;;;; Copyright (c) 2024-2025 GreenSnakeLinux (@GreenSnakeLinux) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_virtual_controls.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-virtual-controls
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-virtual-controls)

;; PadButton
(defconstant +button-none+ -1)
(defconstant +button-up+ 0)
(defconstant +button-left+ 1)
(defconstant +button-right+ 2)
(defconstant +button-down+ 3)
(defconstant +button-max+ 4)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input virtual controls")

    (let* ((pad-position (vec2 100.0 350.0))
           (button-radius 30.0)
           (button-positions (vector (vec2 (vx pad-position) (- (vy pad-position) (* button-radius 1.5))) ; Up
                                     (vec2 (- (vx pad-position) (* button-radius 1.5)) (vy pad-position)) ; Left
                                     (vec2 (+ (vx pad-position) (* button-radius 1.5)) (vy pad-position)) ; Right
                                     (vec2 (vx pad-position) (+ (vy pad-position) (* button-radius 1.5))))) ; Down
           (arrow-tris (flet ((p (i dx dy) (vec2 (+ (vx (aref button-positions i)) dx) (+ (vy (aref button-positions i)) dy))))
                         (vector (vector (p 0 0 -12) (p 0 -9 9) (p 0 9 9))       ; Up
                                 (vector (p 1 9 -9) (p 1 -12 0) (p 1 9 9))       ; Left
                                 (vector (p 2 12 0) (p 2 -9 -9) (p 2 -9 9))      ; Right
                                 (vector (p 3 -9 -9) (p 3 0 12) (p 3 9 -9)))))   ; Down
           (button-label-colors (vector +yellow+ ; Up
                                        +blue+   ; Left
                                        +red+    ; Right
                                        +green+)) ; Down
           (pressed-button +button-none+)
           (input-position (vec2 0.0 0.0))
           (player-position (vec2 (/ screen-width 2.0) (/ screen-height 2.0)))
           (player-speed 75.0))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;--------------------------------------------------------------------------
               (if (> (get-touch-point-count) 0)
                   (setf input-position (get-touch-position 0)) ; Use touch position
                   (setf input-position (get-mouse-position))) ; Use mouse position

               ;; Reset pressed button to none
               (setf pressed-button +button-none+)

               ;; Make sure user is pressing left mouse button if they're from desktop
               (when (or (> (get-touch-point-count) 0)
                         (and (= (get-touch-point-count) 0) (is-mouse-button-down +mouse-button-left+)))
                 ;; Find nearest D-Pad button to the input position
                 (dotimes (i +button-max+)
                   (let ((dist-x (abs (- (vx (aref button-positions i)) (vx input-position))))
                         (dist-y (abs (- (vy (aref button-positions i)) (vy input-position)))))
                     (when (< (+ dist-x dist-y) button-radius)
                       (setf pressed-button i)
                       (return)))))

               ;; Move player according to pressed button
               (case pressed-button
                 (#.+button-up+ (decf (vy player-position) (* player-speed (get-frame-time))))
                 (#.+button-left+ (decf (vx player-position) (* player-speed (get-frame-time))))
                 (#.+button-right+ (incf (vx player-position) (* player-speed (get-frame-time))))
                 (#.+button-down+ (incf (vy player-position) (* player-speed (get-frame-time)))))
               ;;--------------------------------------------------------------------------

               ;; Draw
               ;;--------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw world
               (draw-circle-v player-position 50.0 +maroon+)

               ;; Draw GUI
               (dotimes (i +button-max+)
                 (draw-circle-v (aref button-positions i) button-radius (if (= i pressed-button) +darkgray+ +black+))

                 (draw-triangle (aref (aref arrow-tris i) 0)
                                (aref (aref arrow-tris i) 1)
                                (aref (aref arrow-tris i) 2)
                                (aref button-label-colors i)))

               (draw-text "move the player with D-Pad buttons" 10 10 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
