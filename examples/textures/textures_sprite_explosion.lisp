;;;; raylib [textures] example - sprite explosion
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_sprite_explosion.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-sprite-explosion
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-sprite-explosion)

(defconstant +num-frames-per-line+ 5)
(defconstant +num-lines+ 5)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - sprite explosion")

    (init-audio-device)

    (let* (;; Load explosion sound
           (fx-boom (load-sound "resources/boom.wav"))

           ;; Load explosion texture
           (explosion (load-texture "resources/explosion.png"))

           ;; Init variables for animation
           (frame-width (/ (float (texture-width explosion)) +num-frames-per-line+)) ; Sprite one frame rectangle width
           (frame-height (/ (float (texture-height explosion)) +num-lines+))         ; Sprite one frame rectangle height
           (current-frame 0)
           (current-line 0)

           (frame-rec (make-rectangle :x 0.0 :y 0.0 :width frame-width :height frame-height))
           (position (vec2 0.0 0.0))

           (active nil)
           (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Check for mouse button pressed and activate explosion (if not active)
               (when (and (is-mouse-button-pressed +mouse-button-left+) (not active))
                 (setf position (get-mouse-position)
                       active t)

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
                       (setf current-line 0
                             active nil)))

                   (setf frames-counter 0)))

               (setf (rectangle-x frame-rec) (* frame-width current-frame)
                     (rectangle-y frame-rec) (* frame-height current-line))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw explosion required frame rectangle
               (when active (draw-texture-rec explosion frame-rec position +white+))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture explosion)        ; Unload texture
      (unload-sound fx-boom)            ; Unload sound

      (close-audio-device)

      (close-window))))                 ; Close window and OpenGL context

(main)
