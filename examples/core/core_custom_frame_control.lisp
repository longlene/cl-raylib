;;;; raylib [core] example - custom frame control
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; NOTE: WARNING: This is an example for advanced users willing to have full control over
;;;; the frame processes. By default, EndDrawing() calls the following processes:
;;;;     1. Draw remaining batch data: rlDrawRenderBatchActive()
;;;;     2. SwapScreenBuffer()
;;;;     3. Frame time control: WaitTime()
;;;;     4. PollInputEvents()
;;;;
;;;; To avoid steps 2, 3 and 4, flag SUPPORT_CUSTOM_FRAME_CONTROL can be enabled in
;;;; config.h (it requires recompiling raylib). This way those steps are up to the user.
;;;;
;;;; Note that enabling this flag invalidates some functions:
;;;;     - GetFrameTime()
;;;;     - SetTargetFPS()
;;;;     - GetFPS()
;;;;
;;;; Example originally created with raylib 4.0, last time updated with raylib 4.0
;;;;
;;;; Copyright (c) 2021-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_custom_frame_control.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-custom-frame-control
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-custom-frame-control)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - custom frame control")

    ;; Custom timming variables
    (let ((previous-time (get-time))    ; Previous time measure
          (current-time 0d0)            ; Current time measure
          (update-draw-time 0d0)        ; Update + Draw time
          (wait-time 0d0)               ; Wait time (if target fps required)
          (delta-time 0.0)              ; Frame time (Update + Draw + Wait time)
          (time-counter 0.0)            ; Accumulative time counter (seconds)
          (position 0.0)                ; Circle position
          (pause nil)                   ; Pause control flag
          (target-fps 60))              ; Our initial target fps
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (poll-input-events)      ; Poll input events (SUPPORT_CUSTOM_FRAME_CONTROL)

               (when (is-key-pressed +key-space+) (setf pause (not pause)))

               (cond ((is-key-pressed +key-up+) (incf target-fps 20))
                     ((is-key-pressed +key-down+) (decf target-fps 20)))

               (when (< target-fps 0) (setf target-fps 0))

               (unless pause
                 (incf position (* 200 delta-time)) ; We move at 200 pixels per second
                 (when (>= position (get-screen-width)) (setf position 0.0))
                 (incf time-counter delta-time))    ; We count time (seconds)
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i (floor (get-screen-width) 200)) (draw-rectangle (* 200 i) 0 1 (get-screen-height) +skyblue+))

               (draw-circle (truncate position) (- (floor (get-screen-height) 2) 25) 50.0 +red+)

               (draw-text (text-format "%03.0f ms" (* time-counter 1000.0)) (- (truncate position) 40) (- (floor (get-screen-height) 2) 100) 20 +maroon+)
               (draw-text (text-format "PosX: %03.0f" position) (- (truncate position) 50) (+ (floor (get-screen-height) 2) 40) 20 +black+)

               (draw-text (format nil "Circle is moving at a constant 200 pixels/sec,~%independently of the frame rate.") 10 10 20 +darkgray+)
               (draw-text "PRESS SPACE to PAUSE MOVEMENT" 10 (- (get-screen-height) 60) 20 +gray+)
               (draw-text "PRESS UP | DOWN to CHANGE TARGET FPS" 10 (- (get-screen-height) 30) 20 +gray+)
               (draw-text (text-format "TARGET FPS: %i" target-fps) (- (get-screen-width) 220) 10 20 +lime+)
               (when (/= delta-time 0)
                 (draw-text (text-format "CURRENT FPS: %i" (truncate (/ 1.0 delta-time))) (- (get-screen-width) 220) 40 20 +green+))

               (end-drawing)

               ;; NOTE: In case raylib is configured to SUPPORT_CUSTOM_FRAME_CONTROL,
               ;; Events polling, screen buffer swap and frame time control must be managed by the user

               (swap-screen-buffer)     ; Flip the back buffer to screen (front buffer)

               (setf current-time (get-time)
                     update-draw-time (- current-time previous-time))

               (if (> target-fps 0)     ; We want a fixed frame rate
                   (progn
                     (setf wait-time (- (/ 1.0 (float target-fps)) update-draw-time))
                     (when (> wait-time 0d0)
                       (wait-time (float wait-time 1.0))
                       (setf current-time (get-time)
                             delta-time (float (- current-time previous-time) 1.0))))
                   (setf delta-time (float update-draw-time 1.0))) ; Framerate could be variable

               (setf previous-time current-time))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
