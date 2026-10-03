;;;; raylib [core] example - input gamepad
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; NOTE: This example requires a Gamepad connected to the system
;;;;       raylib is configured to work with the following gamepads:
;;;;              - Xbox 360 Controller (Xbox 360, Xbox One)
;;;;              - PLAYSTATION(R)3 Controller
;;;;       Check raylib.h for buttons configuration
;;;;
;;;; Example originally created with raylib 1.1, last time updated with raylib 4.2
;;;;
;;;; Copyright (c) 2013-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_input_gamepad.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-gamepad
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-gamepad)

;; NOTE: Gamepad name ID depends on drivers and OS
(defparameter +xbox-alias-1+ "xbox")
(defparameter +xbox-alias-2+ "x-box")
(defparameter +ps-alias-1+ "playstation")
(defparameter +ps-alias-2+ "sony")

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+) ; Set MSAA 4X hint before windows creation

    (init-window screen-width screen-height "raylib [core] example - input gamepad")

    (let ((tex-ps3-pad (load-texture "resources/ps3.png"))
          (tex-xbox-pad (load-texture "resources/xbox.png"))
          ;; Set axis deadzones
          (left-stick-deadzone-x 0.1)
          (left-stick-deadzone-y 0.1)
          (right-stick-deadzone-x 0.1)
          (right-stick-deadzone-y 0.1)
          (left-trigger-deadzone -0.9)
          (right-trigger-deadzone -0.9)
          (vibrate-button (make-rectangle))
          (gamepad 0))                  ; which gamepad to display

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (and (is-key-pressed +key-left+) (> gamepad 0)) (decf gamepad))
               (when (is-key-pressed +key-right+) (incf gamepad))
               (let ((mouse-position (get-mouse-position)))

                 (setf vibrate-button (make-rectangle :x 10.0 :y (+ 70.0 (* 20 (get-gamepad-axis-count gamepad)) 20)
                                                      :width 75.0 :height 24.0))
                 (when (and (is-mouse-button-pressed +mouse-button-left+) (check-collision-point-rec mouse-position vibrate-button))
                   (set-gamepad-vibration gamepad 1.0 1.0 1.0)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (flet ((down (button) (is-gamepad-button-down gamepad button)))
                 (if (is-gamepad-available gamepad)
                     (progn
                       (draw-text (text-format "GP%d: %s" gamepad (get-gamepad-name gamepad)) 10 10 10 +black+)

                       ;; Get axis values
                       (let ((left-stick-x (get-gamepad-axis-movement gamepad +gamepad-axis-left-x+))
                             (left-stick-y (get-gamepad-axis-movement gamepad +gamepad-axis-left-y+))
                             (right-stick-x (get-gamepad-axis-movement gamepad +gamepad-axis-right-x+))
                             (right-stick-y (get-gamepad-axis-movement gamepad +gamepad-axis-right-y+))
                             (left-trigger (get-gamepad-axis-movement gamepad +gamepad-axis-left-trigger+))
                             (right-trigger (get-gamepad-axis-movement gamepad +gamepad-axis-right-trigger+))
                             (name (text-to-lower (get-gamepad-name gamepad))))

                         ;; Calculate deadzones
                         (when (and (> left-stick-x (- left-stick-deadzone-x)) (< left-stick-x left-stick-deadzone-x)) (setf left-stick-x 0.0))
                         (when (and (> left-stick-y (- left-stick-deadzone-y)) (< left-stick-y left-stick-deadzone-y)) (setf left-stick-y 0.0))
                         (when (and (> right-stick-x (- right-stick-deadzone-x)) (< right-stick-x right-stick-deadzone-x)) (setf right-stick-x 0.0))
                         (when (and (> right-stick-y (- right-stick-deadzone-y)) (< right-stick-y right-stick-deadzone-y)) (setf right-stick-y 0.0))
                         (when (< left-trigger left-trigger-deadzone) (setf left-trigger -1.0))
                         (when (< right-trigger right-trigger-deadzone) (setf right-trigger -1.0))

                         (cond
                           ((or (> (text-find-index name +xbox-alias-1+) -1)
                                (> (text-find-index name +xbox-alias-2+) -1))
                            (draw-texture tex-xbox-pad 0 0 +darkgray+)

                            ;; Draw buttons: xbox home
                            (when (down +gamepad-button-middle+) (draw-circle 394 89 19.0 +red+))

                            ;; Draw buttons: basic
                            (when (down +gamepad-button-middle-right+) (draw-circle 436 150 9.0 +red+))
                            (when (down +gamepad-button-middle-left+) (draw-circle 352 150 9.0 +red+))
                            (when (down +gamepad-button-right-face-left+) (draw-circle 501 151 15.0 +blue+))
                            (when (down +gamepad-button-right-face-down+) (draw-circle 536 187 15.0 +lime+))
                            (when (down +gamepad-button-right-face-right+) (draw-circle 572 151 15.0 +maroon+))
                            (when (down +gamepad-button-right-face-up+) (draw-circle 536 115 15.0 +gold+))

                            ;; Draw buttons: d-pad
                            (draw-rectangle 317 202 19 71 +black+)
                            (draw-rectangle 293 228 69 19 +black+)
                            (when (down +gamepad-button-left-face-up+) (draw-rectangle 317 202 19 26 +red+))
                            (when (down +gamepad-button-left-face-down+) (draw-rectangle 317 (+ 202 45) 19 26 +red+))
                            (when (down +gamepad-button-left-face-left+) (draw-rectangle 292 228 25 19 +red+))
                            (when (down +gamepad-button-left-face-right+) (draw-rectangle (+ 292 44) 228 26 19 +red+))

                            ;; Draw buttons: left-right back
                            (when (down +gamepad-button-left-trigger-1+) (draw-circle 259 61 20.0 +red+))
                            (when (down +gamepad-button-right-trigger-1+) (draw-circle 536 61 20.0 +red+))

                            ;; Draw axis: left joystick
                            (let ((left-gamepad-color (if (down +gamepad-button-left-thumb+) +red+ +black+)))
                              (draw-circle 259 152 39.0 +black+)
                              (draw-circle 259 152 34.0 +lightgray+)
                              (draw-circle (+ 259 (truncate (* left-stick-x 20))) (+ 152 (truncate (* left-stick-y 20))) 25.0 left-gamepad-color))

                            ;; Draw axis: right joystick
                            (let ((right-gamepad-color (if (down +gamepad-button-right-thumb+) +red+ +black+)))
                              (draw-circle 461 237 38.0 +black+)
                              (draw-circle 461 237 33.0 +lightgray+)
                              (draw-circle (+ 461 (truncate (* right-stick-x 20))) (+ 237 (truncate (* right-stick-y 20))) 25.0 right-gamepad-color))

                            ;; Draw axis: left-right triggers
                            (draw-rectangle 170 30 15 70 +gray+)
                            (draw-rectangle 604 30 15 70 +gray+)
                            (draw-rectangle 170 30 15 (truncate (* (/ (+ 1 left-trigger) 2) 70)) +red+)
                            (draw-rectangle 604 30 15 (truncate (* (/ (+ 1 right-trigger) 2) 70)) +red+))

                           ((or (> (text-find-index name +ps-alias-1+) -1)
                                (> (text-find-index name +ps-alias-2+) -1))
                            (draw-texture tex-ps3-pad 0 0 +darkgray+)

                            ;; Draw buttons: ps
                            (when (down +gamepad-button-middle+) (draw-circle 396 222 13.0 +red+))

                            ;; Draw buttons: basic
                            (when (down +gamepad-button-middle-left+) (draw-rectangle 328 170 32 13 +red+))
                            (when (down +gamepad-button-middle-right+) (draw-triangle (vec2 436.0 168.0) (vec2 436.0 185.0) (vec2 464.0 177.0) +red+))
                            (when (down +gamepad-button-right-face-up+) (draw-circle 557 144 13.0 +lime+))
                            (when (down +gamepad-button-right-face-right+) (draw-circle 586 173 13.0 +red+))
                            (when (down +gamepad-button-right-face-down+) (draw-circle 557 203 13.0 +violet+))
                            (when (down +gamepad-button-right-face-left+) (draw-circle 527 173 13.0 +pink+))

                            ;; Draw buttons: d-pad
                            (draw-rectangle 225 132 24 84 +black+)
                            (draw-rectangle 195 161 84 25 +black+)
                            (when (down +gamepad-button-left-face-up+) (draw-rectangle 225 132 24 29 +red+))
                            (when (down +gamepad-button-left-face-down+) (draw-rectangle 225 (+ 132 54) 24 30 +red+))
                            (when (down +gamepad-button-left-face-left+) (draw-rectangle 195 161 30 25 +red+))
                            (when (down +gamepad-button-left-face-right+) (draw-rectangle (+ 195 54) 161 30 25 +red+))

                            ;; Draw buttons: left-right back buttons
                            (when (down +gamepad-button-left-trigger-1+) (draw-circle 239 82 20.0 +red+))
                            (when (down +gamepad-button-right-trigger-1+) (draw-circle 557 82 20.0 +red+))

                            ;; Draw axis: left joystick
                            (let ((left-gamepad-color (if (down +gamepad-button-left-thumb+) +red+ +black+)))
                              (draw-circle 319 255 35.0 +black+)
                              (draw-circle 319 255 31.0 +lightgray+)
                              (draw-circle (+ 319 (truncate (* left-stick-x 20))) (+ 255 (truncate (* left-stick-y 20))) 25.0 left-gamepad-color))

                            ;; Draw axis: right joystick
                            (let ((right-gamepad-color (if (down +gamepad-button-right-thumb+) +red+ +black+)))
                              (draw-circle 475 255 35.0 +black+)
                              (draw-circle 475 255 31.0 +lightgray+)
                              (draw-circle (+ 475 (truncate (* right-stick-x 20))) (+ 255 (truncate (* right-stick-y 20))) 25.0 right-gamepad-color))

                            ;; Draw axis: left-right triggers
                            (draw-rectangle 169 48 15 70 +gray+)
                            (draw-rectangle 611 48 15 70 +gray+)
                            (draw-rectangle 169 48 15 (truncate (* (/ (+ 1 left-trigger) 2) 70)) +red+)
                            (draw-rectangle 611 48 15 (truncate (* (/ (+ 1 right-trigger) 2) 70)) +red+))

                           (t
                            ;; Draw background: generic
                            (draw-rectangle-rounded (make-rectangle :x 175.0 :y 110.0 :width 460.0 :height 220.0) 0.3 16 +darkgray+)

                            ;; Draw buttons: basic
                            (draw-circle 365 170 12.0 +raywhite+)
                            (draw-circle 405 170 12.0 +raywhite+)
                            (draw-circle 445 170 12.0 +raywhite+)
                            (draw-circle 516 191 17.0 +raywhite+)
                            (draw-circle 551 227 17.0 +raywhite+)
                            (draw-circle 587 191 17.0 +raywhite+)
                            (draw-circle 551 155 17.0 +raywhite+)
                            (when (down +gamepad-button-middle-left+) (draw-circle 365 170 10.0 +red+))
                            (when (down +gamepad-button-middle+) (draw-circle 405 170 10.0 +green+))
                            (when (down +gamepad-button-middle-right+) (draw-circle 445 170 10.0 +blue+))
                            (when (down +gamepad-button-right-face-left+) (draw-circle 516 191 15.0 +gold+))
                            (when (down +gamepad-button-right-face-down+) (draw-circle 551 227 15.0 +blue+))
                            (when (down +gamepad-button-right-face-right+) (draw-circle 587 191 15.0 +green+))
                            (when (down +gamepad-button-right-face-up+) (draw-circle 551 155 15.0 +red+))

                            ;; Draw buttons: d-pad
                            (draw-rectangle 245 145 28 88 +raywhite+)
                            (draw-rectangle 215 174 88 29 +raywhite+)
                            (draw-rectangle 247 147 24 84 +black+)
                            (draw-rectangle 217 176 84 25 +black+)
                            (when (down +gamepad-button-left-face-up+) (draw-rectangle 247 147 24 29 +red+))
                            (when (down +gamepad-button-left-face-down+) (draw-rectangle 247 (+ 147 54) 24 30 +red+))
                            (when (down +gamepad-button-left-face-left+) (draw-rectangle 217 176 30 25 +red+))
                            (when (down +gamepad-button-left-face-right+) (draw-rectangle (+ 217 54) 176 30 25 +red+))

                            ;; Draw buttons: left-right back
                            (draw-rectangle-rounded (make-rectangle :x 215.0 :y 98.0 :width 100.0 :height 10.0) 0.5 16 +darkgray+)
                            (draw-rectangle-rounded (make-rectangle :x 495.0 :y 98.0 :width 100.0 :height 10.0) 0.5 16 +darkgray+)
                            (when (down +gamepad-button-left-trigger-1+)
                              (draw-rectangle-rounded (make-rectangle :x 215.0 :y 98.0 :width 100.0 :height 10.0) 0.5 16 +red+))
                            (when (down +gamepad-button-right-trigger-1+)
                              (draw-rectangle-rounded (make-rectangle :x 495.0 :y 98.0 :width 100.0 :height 10.0) 0.5 16 +red+))

                            ;; Draw axis: left joystick
                            (let ((left-gamepad-color (if (down +gamepad-button-left-thumb+) +red+ +black+)))
                              (draw-circle 345 260 40.0 +black+)
                              (draw-circle 345 260 35.0 +lightgray+)
                              (draw-circle (+ 345 (truncate (* left-stick-x 20))) (+ 260 (truncate (* left-stick-y 20))) 25.0 left-gamepad-color))

                            ;; Draw axis: right joystick
                            (let ((right-gamepad-color (if (down +gamepad-button-right-thumb+) +red+ +black+)))
                              (draw-circle 465 260 40.0 +black+)
                              (draw-circle 465 260 35.0 +lightgray+)
                              (draw-circle (+ 465 (truncate (* right-stick-x 20))) (+ 260 (truncate (* right-stick-y 20))) 25.0 right-gamepad-color))

                            ;; Draw axis: left-right triggers
                            (draw-rectangle 151 110 15 70 +gray+)
                            (draw-rectangle 644 110 15 70 +gray+)
                            (draw-rectangle 151 110 15 (truncate (* (/ (+ 1 left-trigger) 2) 70)) +red+)
                            (draw-rectangle 644 110 15 (truncate (* (/ (+ 1 right-trigger) 2) 70)) +red+))))

                       (draw-text (text-format "DETECTED AXIS [%i]:" (get-gamepad-axis-count gamepad)) 10 50 10 +maroon+)

                       (dotimes (i (get-gamepad-axis-count gamepad))
                         (draw-text (text-format "AXIS %i: %.02f" i (get-gamepad-axis-movement gamepad i)) 20 (+ 70 (* 20 i)) 10 +darkgray+))

                       ;; Draw vibrate button
                       (draw-rectangle-rec vibrate-button +skyblue+)
                       (draw-text "VIBRATE" (truncate (+ (rectangle-x vibrate-button) 14)) (truncate (+ (rectangle-y vibrate-button) 1)) 10 +darkgray+)

                       (if (/= (get-gamepad-button-pressed) +gamepad-button-unknown+)
                           (draw-text (text-format "DETECTED BUTTON: %i" (get-gamepad-button-pressed)) 10 430 10 +red+)
                           (draw-text "DETECTED BUTTON: NONE" 10 430 10 +gray+)))
                     (progn
                       (draw-text (text-format "GP%d: NOT DETECTED" gamepad) 10 10 10 +gray+)

                       (draw-texture tex-xbox-pad 0 0 +lightgray+))))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture tex-ps3-pad)
      (unload-texture tex-xbox-pad)

      (close-window))))                 ; Close window and OpenGL context

(main)
