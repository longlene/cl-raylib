;;;; raylib [textures] example - gif player
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 4.2
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2021-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_gif_player.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-gif-player
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-gif-player)

(defconstant +max-frame-delay+ 20)
(defconstant +min-frame-delay+ 1)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - gif player")

    ;; Load all GIF animation frames into a single Image
    ;; NOTE: GIF data is always loaded as RGBA (32bit) by default
    ;; NOTE: Frames are just appended one after another in image.data memory
    (multiple-value-bind (im-scarfy-anim anim-frames) (load-image-anim "resources/scarfy_run.gif")

      ;; Load texture from image
      ;; NOTE: We will update this texture when required with next frame data
      ;; WARNING: It's not recommended to use this technique for sprites animation,
      ;; use spritesheets instead, like illustrated in textures_sprite_anim example
      (let ((tex-scarfy-anim (load-texture-from-image im-scarfy-anim))
            (frame-size (* (image-width im-scarfy-anim) (image-height im-scarfy-anim) 4))

            (next-frame-data-offset 0)  ; Current byte offset to next frame in image.data

            (current-anim-frame 0)      ; Current animation frame to load and draw
            (frame-delay 8)             ; Frame delay to switch between animation frames
            (frame-counter 0))          ; General frames counter

        (set-target-fps 60)             ; Set our game to run at 60 frames-per-second
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 (incf frame-counter)
                 (when (>= frame-counter frame-delay)
                   ;; Move to next frame
                   ;; NOTE: If final frame is reached we return to first frame
                   (incf current-anim-frame)
                   (when (>= current-anim-frame anim-frames) (setf current-anim-frame 0))

                   ;; Get memory offset position for next frame data in image.data
                   (setf next-frame-data-offset (* frame-size current-anim-frame))

                   ;; Update GPU texture data with next frame image data
                   ;; WARNING: Data size (frame size) and pixel format must match already created texture
                   (update-texture tex-scarfy-anim (subseq (image-data im-scarfy-anim) next-frame-data-offset
                                                           (+ next-frame-data-offset frame-size)))

                   (setf frame-counter 0))

                 ;; Control frames delay
                 (cond ((is-key-pressed +key-right+) (incf frame-delay))
                       ((is-key-pressed +key-left+) (decf frame-delay)))

                 (cond ((> frame-delay +max-frame-delay+) (setf frame-delay +max-frame-delay+))
                       ((< frame-delay +min-frame-delay+) (setf frame-delay +min-frame-delay+)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-text (text-format "TOTAL GIF FRAMES:  %02i" anim-frames) 50 30 20 +lightgray+)
                 (draw-text (text-format "CURRENT FRAME: %02i" current-anim-frame) 50 60 20 +gray+)
                 (draw-text (text-format "CURRENT FRAME IMAGE.DATA OFFSET: %02i" next-frame-data-offset) 50 90 20 +gray+)

                 (draw-text "FRAMES DELAY: " 100 305 10 +darkgray+)
                 (draw-text (text-format "%02i frames" frame-delay) 620 305 10 +darkgray+)
                 (draw-text "PRESS RIGHT/LEFT KEYS to CHANGE SPEED!" 290 350 10 +darkgray+)

                 (dotimes (i +max-frame-delay+)
                   (when (< i frame-delay) (draw-rectangle (+ 190 (* 21 i)) 300 20 20 +red+))
                   (draw-rectangle-lines (+ 190 (* 21 i)) 300 20 20 +maroon+))

                 (draw-texture tex-scarfy-anim (- (truncate (get-screen-width) 2) (truncate (texture-width tex-scarfy-anim) 2)) 140 +white+)

                 (draw-text "(c) Scarfy sprite by Eiden Marsal" (- screen-width 200) (- screen-height 20) 10 +gray+)

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture tex-scarfy-anim) ; Unload texture
        (unload-image im-scarfy-anim)   ; Unload image (contains all frames)

        (close-window)))))              ; Close window and OpenGL context

(main)
