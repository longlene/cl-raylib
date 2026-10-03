;;;; textures_bunnymark.lisp - Bunnymark performance test
;;;; Translated from raylib/examples/textures/textures_bunnymark.c

(require :cl-raylib)

(defpackage :textures-bunnymark
  (:use :cl :cl-raylib))

(in-package :textures-bunnymark)

(defconstant +max-bunnies+ 50000) ; 50K bunnies limit

;; This is the maximum amount of elements (quads) per batch
;; NOTE: This value is defined in [rlgl] module and can be changed there
(defconstant +max-batch-elements+ 8192)

(defstruct bunny
  "Bunny structure with position, speed, and color"
  (position (vec2 0.0 0.0) :type vec2)
  (speed (vec2 0.0 0.0) :type vec2)
  (color +white+ :type color))

(defun main ()
  "Main function - bunnymark performance test"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - bunnymark")

    ;; Load bunny texture
    (let ((tex-bunny (load-texture "examples/textures/resources/wabbit_alpha.png")))

      ;; Bunnies array
      (let ((bunnies (make-array +max-bunnies+ :element-type 'bunny :initial-element (make-bunny)))
            (bunnies-count 0))

        (set-target-fps 60) ; Set game to run at 60 frames-per-second

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (when (is-mouse-button-down +mouse-button-left+)
            ;; Create more bunnies
            (loop for i from 0 below 100 do
              (when (< bunnies-count +max-bunnies+)
                (let ((mouse-pos (get-mouse-position)))
                  (setf (bunny-position (aref bunnies bunnies-count)) mouse-pos)
                  (setf (bunny-speed (aref bunnies bunnies-count))
                        (vec2 (/ (get-random-value -250 250) 60.0)
                              (/ (get-random-value -250 250) 60.0)))
                  (setf (bunny-color (aref bunnies bunnies-count))
                        (make-color (get-random-value 50 240)
                                   (get-random-value 80 240)
                                   (get-random-value 100 240)
                                   255))
                  (incf bunnies-count)))))

          ;; Update bunnies
          (loop for i from 0 below bunnies-count do
            (let ((bunny (aref bunnies i)))
              ;; Update position
              (setf (vx (bunny-position bunny))
                    (+ (vx (bunny-position bunny)) (vx (bunny-speed bunny))))
              (setf (vy (bunny-position bunny))
                    (+ (vy (bunny-position bunny)) (vy (bunny-speed bunny))))

              ;; Bounce off screen edges
              (when (or (> (+ (vx (bunny-position bunny)) (/ (texture-width tex-bunny) 2)) screen-width)
                        (< (+ (vx (bunny-position bunny)) (/ (texture-width tex-bunny) 2)) 0))
                (setf (vx (bunny-speed bunny)) (* (vx (bunny-speed bunny)) -1)))
              (when (or (> (+ (vy (bunny-position bunny)) (/ (texture-height tex-bunny) 2)) screen-height)
                        (< (+ (vy (bunny-position bunny)) (/ (texture-height tex-bunny) 2) -40) 0))
                (setf (vy (bunny-speed bunny)) (* (vy (bunny-speed bunny)) -1)))))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            ;; Draw all bunnies
            (loop for i from 0 below bunnies-count do
              (let ((bunny (aref bunnies i)))
                ;; NOTE: When internal batch buffer limit is reached (MAX_BATCH_ELEMENTS),
                ;; a draw call is launched and buffer starts being filled again;
                ;; before issuing a draw call, updated vertex data from internal CPU buffer is sent to GPU...
                ;; Process of sending data is costly and it could happen that GPU data has not been completely
                ;; processed for drawing while new data is tried to be sent (updating current in-use buffers)
                ;; it could generates a stall and consequently a frame drop, limiting the number of drawn bunnies
                (draw-texture tex-bunny
                             (truncate (vx (bunny-position bunny)))
                             (truncate (vy (bunny-position bunny)))
                             (bunny-color bunny))))

            ;; Draw info
            (draw-rectangle 0 0 screen-width 40 +black+)
            (draw-text (format nil "bunnies: ~d" bunnies-count) 120 10 20 +green+)
            (draw-text (format nil "batched draw calls: ~d" (1+ (truncate bunnies-count +max-batch-elements+))) 320 10 20 +maroon+)

            (draw-fps 10 10)

          (end-drawing))

        ;; De-Initialization
        (unload-texture tex-bunny)))

    ;; Close window
    (close-window)))

;; Run the example
(main)