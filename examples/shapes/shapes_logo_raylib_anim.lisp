;;;; raylib [shapes] example - logo raylib anim
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_logo_raylib_anim.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-logo-raylib-anim
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-logo-raylib-anim)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - logo raylib anim")

    (let ((logo-position-x (- (truncate screen-width 2) 128))
          (logo-position-y (- (truncate screen-height 2) 128))
          (frames-counter 0)
          (letters-count 0)
          (top-side-rec-width 16)
          (left-side-rec-height 16)
          (bottom-side-rec-width 16)
          (right-side-rec-height 16)
          (state 0)                     ; Tracking animation states (State Machine)
          (alpha 1.0))                  ; Useful for fading

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (case state
                 (0                     ; State 0: Small box blinking
                  (incf frames-counter)

                  (when (= frames-counter 120)
                    (setf state 1
                          frames-counter 0))) ; Reset counter... will be used later...
                 (1                     ; State 1: Top and left bars growing
                  (incf top-side-rec-width 4)
                  (incf left-side-rec-height 4)

                  (when (= top-side-rec-width 256) (setf state 2)))
                 (2                     ; State 2: Bottom and right bars growing
                  (incf bottom-side-rec-width 4)
                  (incf right-side-rec-height 4)

                  (when (= bottom-side-rec-width 256) (setf state 3)))
                 (3                     ; State 3: Letters appearing (one by one)
                  (incf frames-counter)

                  (when (/= (truncate frames-counter 12) 0) ; Every 12 frames, one more letter!
                    (incf letters-count)
                    (setf frames-counter 0))

                  (when (>= letters-count 10) ; When all letters have appeared, just fade out everything
                    (decf alpha 0.02)

                    (when (<= alpha 0.0)
                      (setf alpha 0.0
                            state 4))))
                 (4                     ; State 4: Reset and Replay
                  (when (is-key-pressed +key-r+)
                    (setf frames-counter 0
                          letters-count 0
                          top-side-rec-width 16
                          left-side-rec-height 16
                          bottom-side-rec-width 16
                          right-side-rec-height 16
                          alpha 1.0
                          state 0))))   ; Return to State 0
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (case state
                 (0
                  (when (oddp (truncate frames-counter 15)) (draw-rectangle logo-position-x logo-position-y 16 16 +black+)))
                 (1
                  (draw-rectangle logo-position-x logo-position-y top-side-rec-width 16 +black+)
                  (draw-rectangle logo-position-x logo-position-y 16 left-side-rec-height +black+))
                 (2
                  (draw-rectangle logo-position-x logo-position-y top-side-rec-width 16 +black+)
                  (draw-rectangle logo-position-x logo-position-y 16 left-side-rec-height +black+)

                  (draw-rectangle (+ logo-position-x 240) logo-position-y 16 right-side-rec-height +black+)
                  (draw-rectangle logo-position-x (+ logo-position-y 240) bottom-side-rec-width 16 +black+))
                 (3
                  (draw-rectangle logo-position-x logo-position-y top-side-rec-width 16 (fade +black+ alpha))
                  (draw-rectangle logo-position-x (+ logo-position-y 16) 16 (- left-side-rec-height 32) (fade +black+ alpha))

                  (draw-rectangle (+ logo-position-x 240) (+ logo-position-y 16) 16 (- right-side-rec-height 32) (fade +black+ alpha))
                  (draw-rectangle logo-position-x (+ logo-position-y 240) bottom-side-rec-width 16 (fade +black+ alpha))

                  (draw-rectangle (- (truncate (get-screen-width) 2) 112) (- (truncate (get-screen-height) 2) 112) 224 224 (fade +raywhite+ alpha))

                  (draw-text (text-subtext "raylib" 0 letters-count) (- (truncate (get-screen-width) 2) 44) (+ (truncate (get-screen-height) 2) 48) 50 (fade +black+ alpha)))
                 (4
                  (draw-text "[R] REPLAY" 340 200 20 +gray+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
