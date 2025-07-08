(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  "raylib [core] example - Gamepad input"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Set MSAA 4X hint before window creation
    (set-config-flags +flag-msaa-4x-hint+)
    
    (with-window (screen-width screen-height "raylib [core] example - gamepad input")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      ;; Load gamepad textures
      (let ((tex-ps3-pad (load-texture "resources/ps3.png"))
            (tex-xbox-pad (load-texture "resources/xbox.png"))
            (gamepad 0)) ; which gamepad to display

        ;; Set axis deadzones
        (let ((left-stick-deadzone-x 0.1)
              (left-stick-deadzone-y 0.1)
              (right-stick-deadzone-x 0.1)
              (right-stick-deadzone-y 0.1)
              (left-trigger-deadzone -0.9)
              (right-trigger-deadzone -0.9))

          (loop
            until (window-should-close) ; Detect window close button or ESC key
            do (progn
                 ;; Update gamepad selection
                 (when (and (is-key-pressed :key-left) (> gamepad 0))
                   (decf gamepad))
                 (when (is-key-pressed :key-right)
                   (incf gamepad))

                 ;; Draw
                 (with-drawing
                   (clear-background :raywhite)

                   (if (is-gamepad-available gamepad)
                       (progn
                         (draw-text (text-format "GP~d: ~a" gamepad (get-gamepad-name gamepad)) 10 10 10 :black)

                         ;; Get axis values
                         (let ((left-stick-x (get-gamepad-axis-movement gamepad :gamepad-axis-left-x))
                               (left-stick-y (get-gamepad-axis-movement gamepad :gamepad-axis-left-y))
                               (right-stick-x (get-gamepad-axis-movement gamepad :gamepad-axis-right-x))
                               (right-stick-y (get-gamepad-axis-movement gamepad :gamepad-axis-right-y))
                               (left-trigger (get-gamepad-axis-movement gamepad :gamepad-axis-left-trigger))
                               (right-trigger (get-gamepad-axis-movement gamepad :gamepad-axis-right-trigger)))

                           ;; Apply deadzones
                           (when (and (> left-stick-x (- left-stick-deadzone-x)) 
                                      (< left-stick-x left-stick-deadzone-x))
                             (setf left-stick-x 0.0))
                           (when (and (> left-stick-y (- left-stick-deadzone-y)) 
                                      (< left-stick-y left-stick-deadzone-y))
                             (setf left-stick-y 0.0))
                           (when (and (> right-stick-x (- right-stick-deadzone-x)) 
                                      (< right-stick-x right-stick-deadzone-x))
                             (setf right-stick-x 0.0))
                           (when (and (> right-stick-y (- right-stick-deadzone-y)) 
                                      (< right-stick-y right-stick-deadzone-y))
                             (setf right-stick-y 0.0))
                           (when (< left-trigger left-trigger-deadzone)
                             (setf left-trigger -1.0))
                           (when (< right-trigger right-trigger-deadzone)
                             (setf right-trigger -1.0))

                           ;; Check gamepad type and draw appropriate controller
                           (let ((gamepad-name (string-downcase (get-gamepad-name gamepad))))
                             (cond
                               ;; Xbox controller
                               ((or (search "xbox" gamepad-name) (search "x-box" gamepad-name))
                                (draw-texture tex-xbox-pad 0 0 :darkgray)

                                ;; Draw buttons: xbox home
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle)
                                  (draw-circle 394 89 19 :red))

                                ;; Draw buttons: basic
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle-right)
                                  (draw-circle 436 150 9 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle-left)
                                  (draw-circle 352 150 9 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-left)
                                  (draw-circle 501 151 15 :blue))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-down)
                                  (draw-circle 536 187 15 :lime))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-right)
                                  (draw-circle 572 151 15 :maroon))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-up)
                                  (draw-circle 536 115 15 :gold))

                                ;; Draw buttons: d-pad
                                (draw-rectangle 317 202 19 71 :black)
                                (draw-rectangle 293 228 69 19 :black)
                                (when (is-gamepad-button-down gamepad :gamepad-button-left-face-up)
                                  (draw-rectangle 317 202 19 26 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-left-face-down)
                                  (draw-rectangle 317 247 19 26 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-left-face-left)
                                  (draw-rectangle 292 228 25 19 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-left-face-right)
                                  (draw-rectangle 336 228 26 19 :red))

                                ;; Draw buttons: left-right back
                                (when (is-gamepad-button-down gamepad :gamepad-button-left-trigger-1)
                                  (draw-circle 259 61 20 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-trigger-1)
                                  (draw-circle 536 61 20 :red))

                                ;; Draw axis: left joystick
                                (let ((left-gamepad-color (if (is-gamepad-button-down gamepad :gamepad-button-left-thumb) :red :black)))
                                  (draw-circle 259 152 39 :black)
                                  (draw-circle 259 152 34 :lightgray)
                                  (draw-circle (+ 259 (floor (* left-stick-x 20)))
                                               (+ 152 (floor (* left-stick-y 20))) 25 left-gamepad-color))

                                ;; Draw axis: right joystick
                                (let ((right-gamepad-color (if (is-gamepad-button-down gamepad :gamepad-button-right-thumb) :red :black)))
                                  (draw-circle 461 237 38 :black)
                                  (draw-circle 461 237 33 :lightgray)
                                  (draw-circle (+ 461 (floor (* right-stick-x 20)))
                                               (+ 237 (floor (* right-stick-y 20))) 25 right-gamepad-color))

                                ;; Draw axis: left-right triggers
                                (draw-rectangle 170 30 15 70 :gray)
                                (draw-rectangle 604 30 15 70 :gray)
                                (draw-rectangle 170 30 15 (floor (* (/ (+ 1 left-trigger) 2) 70)) :red)
                                (draw-rectangle 604 30 15 (floor (* (/ (+ 1 right-trigger) 2) 70)) :red))

                               ;; PlayStation controller
                               ((search "playstation" gamepad-name)
                                (draw-texture tex-ps3-pad 0 0 :darkgray)

                                ;; Draw buttons: ps
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle)
                                  (draw-circle 396 222 13 :red))

                                ;; Draw buttons: basic
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle-left)
                                  (draw-rectangle 328 170 32 13 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-middle-right)
                                  (draw-triangle (vec2 436 168) (vec2 436 185) (vec2 464 177) :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-up)
                                  (draw-circle 557 144 13 :lime))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-right)
                                  (draw-circle 586 173 13 :red))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-down)
                                  (draw-circle 557 203 13 :violet))
                                (when (is-gamepad-button-down gamepad :gamepad-button-right-face-left)
                                  (draw-circle 527 173 13 :pink))

                                ;; Additional PS3 controller drawing code...
                                ;; (Similar pattern for d-pad, joysticks, etc.)
                                )

                               ;; Generic controller
                               (t
                                (draw-rectangle-rounded (make-rectangle :x 175 :y 110 :width 460 :height 220) 0.3 16 :darkgray)
                                ;; Generic controller drawing code...
                                )))

                         ;; Display axis information
                         (draw-text (text-format "DETECTED AXIS [~d]:" (get-gamepad-axis-count 0)) 10 50 10 :maroon)
                         (loop for i from 0 below (get-gamepad-axis-count 0) do
                           (draw-text (text-format "AXIS ~d: ~4,2f" i (get-gamepad-axis-movement 0 i)) 
                                      20 (+ 70 (* 20 i)) 10 :darkgray))

                         ;; Display button pressed
                         (let ((button-pressed (get-gamepad-button-pressed)))
                           (if (not (eq button-pressed :gamepad-button-unknown))
                               (draw-text (text-format "DETECTED BUTTON: ~a" button-pressed) 10 430 10 :red)
                               (draw-text "DETECTED BUTTON: NONE" 10 430 10 :gray))))

                       ;; No gamepad detected
                       (progn
                         (draw-text (text-format "GP~d: NOT DETECTED" gamepad) 10 10 10 :gray)
                         (draw-texture tex-xbox-pad 0 0 :lightgray))))))

        ;; Cleanup
        (unload-texture tex-ps3-pad)
        (unload-texture tex-xbox-pad)))))

(main)