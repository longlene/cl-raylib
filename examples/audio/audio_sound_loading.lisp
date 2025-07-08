(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  "raylib [audio] example - Sound loading and playing"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [audio] example - sound loading and playing")
      (set-target-fps 60) ; Set our game to run at 60 FPS

      (init-audio-device) ; Initialize audio device

      (let ((fx-wav nil)
            (fx-ogg nil))

        ;; Try to load sound files
        (handler-case
            (setf fx-wav (load-sound "resources/sound.wav"))
          (error ()
            (setf fx-wav nil)))

        (handler-case
            (setf fx-ogg (load-sound "resources/target.ogg"))
          (error ()
            (setf fx-ogg nil)))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (when (and (is-key-pressed :key-space) fx-wav)
                 (play-sound fx-wav)) ; Play WAV sound
               (when (and (is-key-pressed :key-enter) fx-ogg)
                 (play-sound fx-ogg)) ; Play OGG sound

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (if fx-wav
                     (draw-text "Press SPACE to PLAY the WAV sound!" 200 180 20 :lightgray)
                     (draw-text "WAV sound file not found" 200 180 20 :red))
                 
                 (if fx-ogg
                     (draw-text "Press ENTER to PLAY the OGG sound!" 200 220 20 :lightgray)
                     (draw-text "OGG sound file not found" 200 220 20 :red)))))

        ;; Cleanup
        (when fx-wav (unload-sound fx-wav))
        (when fx-ogg (unload-sound fx-ogg))
        (close-audio-device)))))

(main)