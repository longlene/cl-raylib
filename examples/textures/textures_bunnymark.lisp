;;;; raylib [textures] example - bunnymark
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 1.6, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_bunnymark.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-bunnymark
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-bunnymark)

(defconstant +max-bunnies+ 80000)       ; 80K bunnies limit

;; This is the maximum amount of elements (quads) per batch
;; NOTE: This value is defined in [rlgl] module and can be changed there
(defconstant +max-batch-elements+ 8192)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct bunny
  (position (vec2 0.0 0.0))
  (speed (vec2 0.0 0.0))
  (color +blank+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - bunnymark")

    ;; Load bunny texture
    (let ((tex-bunny (load-texture "resources/raybunny.png"))
          (bunnies (make-array +max-bunnies+ :initial-element nil)) ; Bunnies array
          (bunnies-count 0)             ; Bunnies counter
          (paused nil))

      (set-target-fps 0)                ; Set our game to run with an uncapped framerate
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-mouse-button-down +mouse-button-left+)
                 ;; Create more bunnies
                 (dotimes (i 100)
                   (when (< bunnies-count +max-bunnies+)
                     (setf (aref bunnies bunnies-count)
                           (let* ((position (get-mouse-position))
                                  (speed-x (float (get-random-value -250 250)))
                                  (speed-y (float (get-random-value -250 250)))
                                  (r (get-random-value 50 240))
                                  (g (get-random-value 80 240))
                                  (b (get-random-value 100 240)))
                             (make-bunny :position position :speed (vec2 speed-x speed-y) :color (list r g b 255))))
                     (incf bunnies-count))))

               (when (is-key-pressed +key-p+) (setf paused (not paused)))

               (unless paused
                 ;; Update bunnies
                 (dotimes (i bunnies-count)
                   (let* ((bunny (aref bunnies i))
                          (position (bunny-position bunny))
                          (speed (bunny-speed bunny)))
                     (incf (vx position) (* (vx speed) (get-frame-time)))
                     (incf (vy position) (* (vy speed) (get-frame-time)))

                     (when (or (> (+ (vx position) (/ (float (texture-width tex-bunny)) 2)) (get-screen-width))
                               (< (+ (vx position) (/ (float (texture-width tex-bunny)) 2)) 0))
                       (setf (vx speed) (* (vx speed) -1)))
                     (when (or (> (+ (vy position) (/ (float (texture-height tex-bunny)) 2)) (get-screen-height))
                               (< (- (+ (vy position) (/ (float (texture-height tex-bunny)) 2)) 40) 0))
                       (setf (vy speed) (* (vy speed) -1))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i bunnies-count)
                 ;; NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
                 ;; a draw call is launched and buffer starts being filled again;
                 ;; before issuing a draw call, updated vertex data from internal CPU buffer is send to GPU...
                 ;; Process of sending data is costly and it could happen that GPU data has not been completely
                 ;; processed for drawing while new data is tried to be sent (updating current in-use buffers)
                 ;; it could generates a stall and consequently a frame drop, limiting the number of drawn bunnies
                 (let ((bunny (aref bunnies i)))
                   (draw-texture tex-bunny (truncate (vx (bunny-position bunny))) (truncate (vy (bunny-position bunny))) (bunny-color bunny))))

               (draw-rectangle 0 0 screen-width 40 +black+)
               (draw-text (text-format "bunnies: %i" bunnies-count) 120 10 20 +green+)
               (draw-text (text-format "batched draw calls: %i" (+ 1 (truncate bunnies-count +max-batch-elements+))) 320 10 20 +maroon+)

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture tex-bunny)        ; Unload bunny texture

      (close-window))))                 ; Close window and OpenGL context

(main)
