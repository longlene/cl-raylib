;;;; raylib [textures] example - sprite animation
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 1.3, last time updated with raylib 1.3
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_sprite_animation.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-sprite-animation
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-sprite-animation)

(defconstant +max-frame-speed+ 15)
(defconstant +min-frame-speed+ 1)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - sprite animation")

    ;; NOTE: Textures MUST be loaded after Window initialization (OpenGL context is required)
    (let* ((scarfy (load-texture "resources/scarfy.png")) ; Texture loading

           (position (vec2 350.0 280.0))
           (frame-rec (make-rectangle :x 0.0 :y 0.0 :width (/ (float (texture-width scarfy)) 6) :height (float (texture-height scarfy))))
           (current-frame 0)

           (frames-counter 0)
           (frames-speed 8))            ; Number of spritesheet frames shown by second

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf frames-counter)

               (when (>= frames-counter (truncate 60 frames-speed))
                 (setf frames-counter 0)
                 (incf current-frame)

                 (when (> current-frame 5) (setf current-frame 0))

                 (setf (rectangle-x frame-rec) (/ (* (float current-frame) (float (texture-width scarfy))) 6)))

               ;; Control frames speed
               (cond ((is-key-pressed +key-right+) (incf frames-speed))
                     ((is-key-pressed +key-left+) (decf frames-speed)))

               (cond ((> frames-speed +max-frame-speed+) (setf frames-speed +max-frame-speed+))
                     ((< frames-speed +min-frame-speed+) (setf frames-speed +min-frame-speed+)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-texture scarfy 15 40 +white+)
               (draw-rectangle-lines 15 40 (texture-width scarfy) (texture-height scarfy) +lime+)
               (draw-rectangle-lines (+ 15 (truncate (rectangle-x frame-rec))) (+ 40 (truncate (rectangle-y frame-rec)))
                                     (truncate (rectangle-width frame-rec)) (truncate (rectangle-height frame-rec)) +red+)

               (draw-text "FRAME SPEED: " 165 210 10 +darkgray+)
               (draw-text (text-format "%02i FPS" frames-speed) 575 210 10 +darkgray+)
               (draw-text "PRESS RIGHT/LEFT KEYS to CHANGE SPEED!" 290 240 10 +darkgray+)

               (dotimes (i +max-frame-speed+)
                 (when (< i frames-speed) (draw-rectangle (+ 250 (* 21 i)) 205 20 20 +red+))
                 (draw-rectangle-lines (+ 250 (* 21 i)) 205 20 20 +maroon+))

               (draw-texture-rec scarfy frame-rec position +white+) ; Draw part of the texture

               (draw-text "(c) Scarfy sprite by Eiden Marsal" (- screen-width 200) (- screen-height 20) 10 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture scarfy)           ; Texture unloading

      (close-window))))                 ; Close window and OpenGL context

(main)
