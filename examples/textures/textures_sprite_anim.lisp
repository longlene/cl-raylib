;;;; textures_sprite_anim.lisp - Sprite animation example
;;;; Translated from raylib/examples/textures/textures_sprite_anim.c

(require :cl-raylib)

(defpackage :textures-sprite-anim
  (:use :cl :cl-raylib))

(in-package :textures-sprite-anim)

(defconstant +max-frame-speed+ 15)
(defconstant +min-frame-speed+ 1)

(defun main ()
  "Main function - sprite animation example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [texture] example - sprite anim")

    ;; Load texture (MUST be loaded after window initialization)
    (let ((scarfy (load-texture "examples/textures/resources/scarfy.png")))
      
      (let ((position (vec2 350.0 280.0))
            (frame-rec (make-rectangle :x 0.0 :y 0.0 
                                       :width (/ (texture-width scarfy) 6.0)
                                       :height (float (texture-height scarfy))))
            (current-frame 0)
            (frames-counter 0)
            (frames-speed 8))  ; Number of spritesheet frames shown by second

        (set-target-fps 60) ; Set game to run at 60 frames-per-second

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (incf frames-counter)

          ;; Update sprite frame based on time
          (when (>= frames-counter (/ 60 frames-speed))
            (setf frames-counter 0)
            (incf current-frame)
            
            (when (> current-frame 5)
              (setf current-frame 0))
            
            ;; Update frame rectangle x position
            (setf (rectangle-x frame-rec) 
                  (* current-frame (/ (texture-width scarfy) 6.0))))

          ;; Control frames speed with keyboard
          (when (is-key-pressed +key-right+)
            (incf frames-speed))
          (when (is-key-pressed +key-left+)
            (decf frames-speed))

          ;; Clamp frames speed
          (when (> frames-speed +max-frame-speed+)
            (setf frames-speed +max-frame-speed+))
          (when (< frames-speed +min-frame-speed+)
            (setf frames-speed +min-frame-speed+))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw full texture at top left
            (draw-texture scarfy 15 40 +white+)
            (draw-rectangle-lines 15 40 (texture-width scarfy) (texture-height scarfy) +lime+)
            
            ;; Draw current frame indicator
            (draw-rectangle-lines (+ 15 (truncate (rectangle-x frame-rec)))
                                  (+ 40 (truncate (rectangle-y frame-rec)))
                                  (truncate (rectangle-width frame-rec))
                                  (truncate (rectangle-height frame-rec))
                                  +red+)

            ;; Draw speed info
            (draw-text "FRAME SPEED: " 165 210 10 +darkgray+)
            (draw-text (text-format "%02d FPS" frames-speed) 575 210 10 +darkgray+)
            (draw-text "PRESS RIGHT/LEFT KEYS to CHANGE SPEED!" 290 240 10 +darkgray+)

            ;; Draw speed indicator bars
            (loop for i from 0 below +max-frame-speed+ do
              (when (< i frames-speed)
                (draw-rectangle (+ 250 (* 21 i)) 205 20 20 +red+))
              (draw-rectangle-lines (+ 250 (* 21 i)) 205 20 20 +maroon+))

            ;; Draw animated sprite
            (draw-texture-rec scarfy frame-rec position +white+)

            ;; Draw credits
            (draw-text "(c) Scarfy sprite by Eiden Marsal" 
                       (- screen-width 200) (- screen-height 20) 10 +gray+)

          (end-drawing))

        ;; De-Initialization
        (unload-texture scarfy)))

    ;; Close window
    (close-window)))

;; Run the example
(main)