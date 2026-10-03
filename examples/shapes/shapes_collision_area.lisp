;;;; raylib [shapes] example - collision area
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2013-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_collision_area.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-collision-area
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-collision-area)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;---------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - collision area")

    (let (;; Box A: Moving box
          (box-a (make-rectangle :x 10.0 :y (- (/ (get-screen-height) 2.0) 50) :width 200.0 :height 100.0))
          (box-a-speed-x 4)
          ;; Box B: Mouse moved box
          (box-b (make-rectangle :x (- (/ (get-screen-width) 2.0) 30) :y (- (/ (get-screen-height) 2.0) 30)
                                 :width 60.0 :height 60.0))
          (box-collision (make-rectangle))   ; Collision rectangle
          (screen-upper-limit 40)            ; Top menu limits
          (pause nil)                        ; Movement pause
          (collision nil))                   ; Collision detection

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;----------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;-----------------------------------------------------
               ;; Move box if not paused
               (unless pause (incf (rectangle-x box-a) box-a-speed-x))

               ;; Bounce box on x screen limits
               (when (or (>= (+ (rectangle-x box-a) (rectangle-width box-a)) (get-screen-width)) (<= (rectangle-x box-a) 0))
                 (setf box-a-speed-x (* box-a-speed-x -1)))

               ;; Update player-controlled-box (box02)
               (setf (rectangle-x box-b) (- (get-mouse-x) (/ (rectangle-width box-b) 2))
                     (rectangle-y box-b) (- (get-mouse-y) (/ (rectangle-height box-b) 2)))

               ;; Make sure Box B does not go out of move area limits
               (cond ((>= (+ (rectangle-x box-b) (rectangle-width box-b)) (get-screen-width))
                      (setf (rectangle-x box-b) (- (get-screen-width) (rectangle-width box-b))))
                     ((<= (rectangle-x box-b) 0) (setf (rectangle-x box-b) 0.0)))

               (cond ((>= (+ (rectangle-y box-b) (rectangle-height box-b)) (get-screen-height))
                      (setf (rectangle-y box-b) (- (get-screen-height) (rectangle-height box-b))))
                     ((<= (rectangle-y box-b) screen-upper-limit) (setf (rectangle-y box-b) (float screen-upper-limit))))

               ;; Check boxes collision
               (setf collision (check-collision-recs box-a box-b))

               ;; Get collision rectangle (only on collision)
               (when collision (setf box-collision (get-collision-rec box-a box-b)))

               ;; Pause Box A movement
               (when (is-key-pressed +key-space+) (setf pause (not pause)))
               ;;-----------------------------------------------------

               ;; Draw
               ;;-----------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-rectangle 0 0 screen-width screen-upper-limit (if collision +red+ +black+))

               (draw-rectangle-rec box-a +gold+)
               (draw-rectangle-rec box-b +blue+)

               (when collision
                 ;; Draw collision area
                 (draw-rectangle-rec box-collision +lime+)

                 ;; Draw collision message
                 (draw-text "COLLISION!" (- (truncate (get-screen-width) 2) (truncate (measure-text "COLLISION!" 20) 2))
                            (- (truncate screen-upper-limit 2) 10) 20 +black+)

                 ;; Draw collision area
                 (draw-text (text-format "Collision Area: %i" (* (truncate (rectangle-width box-collision))
                                                                 (truncate (rectangle-height box-collision))))
                            (- (truncate (get-screen-width) 2) 100) (+ screen-upper-limit 10) 20 +black+))

               ;; Draw help instructions
               (draw-text "Press SPACE to PAUSE/RESUME" 20 (- screen-height 35) 20 +lightgray+)

               (draw-fps 10 10)

               (end-drawing))
      ;;-----------------------------------------------------

      ;; De-Initialization
      ;;---------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
