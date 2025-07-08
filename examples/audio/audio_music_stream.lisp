(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  "raylib [audio] example - Music playing (streaming)"
  (let ((screen-width 800)
        (screen-height 450))
    (with-window (screen-width screen-height "raylib [audio] example - music playing (streaming)")
      (set-target-fps 30) ; Set our game to run at 30 FPS

      (init-audio-device) ; Initialize audio device

      (let ((music nil)
            (time-played 0.0)
            (pause nil))

        ;; Try to load music file
        (handler-case
            (progn
              (setf music (load-music-stream "resources/country.mp3"))
              (play-music-stream music))
          (error ()
            (setf music nil)))

        (loop
          until (window-should-close) ; Detect window close button or ESC key
          do (progn
               ;; Update
               (when music
                 (update-music-stream music) ; Update music buffer with new stream data

                 ;; Restart music playing (stop and play)
                 (when (is-key-pressed :key-space)
                   (stop-music-stream music)
                   (play-music-stream music))

                 ;; Pause/Resume music playing
                 (when (is-key-pressed :key-p)
                   (setf pause (not pause))
                   (if pause
                       (pause-music-stream music)
                       (resume-music-stream music)))

                 ;; Get normalized time played for current music stream
                 (let ((time-length (get-music-time-length music)))
                   (when (> time-length 0)
                     (setf time-played (/ (get-music-time-played music) time-length))
                     (when (> time-played 1.0) (setf time-played 1.0)))))

               ;; Draw
               (with-drawing
                 (clear-background :raywhite)

                 (if music
                     (progn
                       (draw-text "MUSIC SHOULD BE PLAYING!" 255 150 20 :lightgray)

                       (draw-rectangle 200 200 400 12 :lightgray)
                       (draw-rectangle 200 200 (floor (* time-played 400.0)) 12 :maroon)
                       (draw-rectangle-lines 200 200 400 12 :gray)

                       (draw-text "PRESS SPACE TO RESTART MUSIC" 215 250 20 :lightgray)
                       (draw-text "PRESS P TO PAUSE/RESUME MUSIC" 208 280 20 :lightgray))
                     (progn
                       (draw-text "MUSIC FILE NOT FOUND!" 280 150 20 :red)
                       (draw-text "Place 'country.mp3' in resources/ folder" 200 200 20 :gray))))))

        ;; Cleanup
        (when music (unload-music-stream music))
        (close-audio-device)))))

(main)