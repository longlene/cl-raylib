(require :cl-raylib)

(defpackage :raylib-user
  (:use :cl :raylib))

(in-package :raylib-user)

(defun main ()
  (let ((screen-width 800)
        (screen-height 450)
	(res-path (uiop:native-namestring (asdf:system-relative-pathname 'cl-raylib  "examples/audio/resources/"))))
    (set-target-fps 60) ; Set our game to run at 60 FPS
    (with-window (screen-width screen-height "raylib [audio] example - sound loading and playing")
      (with-audio-device
        (with-sound (fx-wav (concatenate 'string res-path "sound.wav"))
	  (with-sound (fx-ogg (concatenate 'string res-path "target.ogg"))
	    (loop
	      until (window-should-close) ; detect window close button or ESC key
	      do
		 (if (is-key-pressed :key-space) (play-sound fx-wav))
		 (if (is-key-pressed :key-enter) (play-sound fx-ogg))
		 (with-drawing
		   (clear-background :raywhite)
		   (draw-text "Press SPACE to PLAY the WAV sound!" 200 180 20 :lightgray)
		   (draw-text "Press ENTER to PLAY the OGG sound!" 200 220 20 :lightgray)))))))))

(main)
