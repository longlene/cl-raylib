;;;; textures_sprite_explosion.lisp - Sprite explosion animation
;;;; Translated from raylib/examples/textures/textures_sprite_explosion.c

(require :cl-raylib)

(defpackage :textures-sprite-explosion
  (:use :cl :cl-raylib))

(in-package :textures-sprite-explosion)

(defconstant +num-frames-per-line+ 5)
(defconstant +num-lines+ 5)

(defun main ()
  "Main function - sprite explosion animation"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - sprite explosion")

    (init-audio-device)

    ;; Load explosion sound
    (let ((fx-boom (load-sound "examples/textures/resources/boom.wav"))
          ;; Load explosion texture
          (explosion (load-texture "examples/textures/resources/explosion.png")))

      ;; Init variables for animation
      (let* ((frame-width (/ (texture-width explosion) +num-frames-per-line+)) ; Sprite one frame rectangle width
             (frame-height (/ (texture-height explosion) +num-lines+))        ; Sprite one frame rectangle height
             (current-frame 0)
             (current-line 0)
             (frame-rec (make-rectangle :x 0.0 :y 0.0 :width frame-width :height frame-height))
             (position (vec2 0.0 0.0))
             (active nil)
             (frames-counter 0))

        (set-target-fps 60) ; Set game to run at 60 frames-per-second

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          
          ;; Check for mouse button pressed and activate explosion (if not active)
          (when (and (is-mouse-button-pressed +mouse-button-left+) (not active))
            (setf position (get-mouse-position))
            (setf active t)

            (decf (vx position) (/ frame-width 2.0))
            (decf (vy position) (/ frame-height 2.0))

            (play-sound fx-boom))

          ;; Compute explosion animation frames
          (when active
            (incf frames-counter)

            (when (> frames-counter 2)
              (incf current-frame)

              (when (>= current-frame +num-frames-per-line+)
                (setf current-frame 0)
                (incf current-line)

                (when (>= current-line +num-lines+)
                  (setf current-line 0)
                  (setf active nil)))

              (setf frames-counter 0)))

          (setf (rectangle-x frame-rec) (* frame-width current-frame))
          (setf (rectangle-y frame-rec) (* frame-height current-line))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw explosion required frame rectangle
            (when active
              (draw-texture-rec explosion frame-rec position +white+))

          (end-drawing))

        ;; De-Initialization
        (unload-texture explosion) ; Unload texture
        (unload-sound fx-boom))   ; Unload sound

    (close-audio-device))

    ;; Close window
    (close-window)))

;; Run the example
(main)