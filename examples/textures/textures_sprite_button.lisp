;;;; textures_sprite_button.lisp - Sprite button example
;;;; Translated from raylib/examples/textures/textures_sprite_button.c

(require :cl-raylib)

(defpackage :textures-sprite-button
  (:use :cl :cl-raylib))

(in-package :textures-sprite-button)

(defconstant +num-frames+ 3) ; Number of frames (rectangles) for the button sprite texture

(defun main ()
  "Main function - sprite button example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - sprite button")

    (init-audio-device) ; Initialize audio device

    (let ((fx-button (load-sound "examples/textures/resources/buttonfx.wav")) ; Load button sound
          (button (load-texture "examples/textures/resources/button.png"))) ; Load button texture

      ;; Define frame rectangle for drawing
      (let ((frame-height (/ (texture-height button) +num-frames+))
            (source-rec (make-rectangle :x 0.0 :y 0.0 
                                       :width (float (texture-width button)) 
                                       :height (/ (texture-height button) +num-frames+)))
            ;; Define button bounds on screen
            (btn-bounds (make-rectangle :x (- (/ screen-width 2.0) (/ (texture-width button) 2.0))
                                       :y (- (/ screen-height 2.0) (/ (texture-height button) +num-frames+ 2.0))
                                       :width (float (texture-width button))
                                       :height (/ (texture-height button) +num-frames+)))
            (btn-state 0) ; Button state: 0-NORMAL, 1-MOUSE_HOVER, 2-PRESSED
            (btn-action nil)) ; Button action should be activated

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (let ((mouse-point (get-mouse-position)))
            (setf btn-action nil)

            ;; Check button state
            (if (check-collision-point-rec mouse-point btn-bounds)
                (progn
                  (if (is-mouse-button-down +mouse-button-left+)
                      (setf btn-state 2)
                      (setf btn-state 1))

                  (when (is-mouse-button-released +mouse-button-left+)
                    (setf btn-action t)))
                (setf btn-state 0))

            (when btn-action
              (play-sound fx-button)
              ;; TODO: Any desired action
              )

            ;; Calculate button frame rectangle to draw depending on button state
            (setf (rectangle-y source-rec) (* btn-state frame-height)))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            (draw-texture-rec button source-rec 
                             (vec2 (rectangle-x btn-bounds) (rectangle-y btn-bounds)) 
                             +white+) ; Draw button frame

          (end-drawing))

        ;; De-Initialization
        (unload-texture button) ; Unload button texture
        (unload-sound fx-button)) ; Unload sound

      (close-audio-device)) ; Close audio device

    ;; Close window
    (close-window)))

;; Run the example
(main)