;;;; audio_sound_multi.lisp - Playing sound multiple times
;;;; Translated from raylib/examples/audio/audio_sound_multi.c

(require :cl-raylib)

(defpackage :audio-sound-multi
  (:use :cl :cl-raylib))

(in-package :audio-sound-multi)

(defconstant +max-sounds+ 10)

(defun main ()
  "Main function - playing sound multiple times"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [audio] example - playing sound multiple times")

    (init-audio-device) ; Initialize audio device

    ;; Load the sound list
    (let ((sound-array (make-array +max-sounds+))
          (current-sound 0))

      ;; Load WAV audio file into the first slot as the 'source' sound
      ;; this sound owns the sample data
      (setf (aref sound-array 0) (load-sound "resources/sound.wav"))

      ;; Load an alias of the sound into slots 1-9. These do not own the sound data, but can be played
      (loop for i from 1 below +max-sounds+ do
        (setf (aref sound-array i) (load-sound-alias (aref sound-array 0))))

      (set-target-fps 60) ; Set game to run at 60 frames-per-second

      ;; Main game loop
      (loop until (window-should-close) do
        ;; Update
        (when (is-key-pressed +key-space+)
          (play-sound (aref sound-array current-sound)) ; play the next open sound slot
          (incf current-sound) ; increment the sound slot
          (when (>= current-sound +max-sounds+) ; if the sound slot is out of bounds, go back to 0
            (setf current-sound 0))

          ;; Note: a better way would be to look at the list for the first sound that is not playing and use that slot
          )

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          (draw-text "Press SPACE to PLAY a WAV sound!" 200 180 20 +lightgray+)

        (end-drawing))

      ;; De-Initialization
      (loop for i from 1 below +max-sounds+ do
        (unload-sound-alias (aref sound-array i))) ; Unload sound aliases
      (unload-sound (aref sound-array 0)) ; Unload source sound data

      (close-audio-device)) ; Close audio device

    ;; Close window
    (close-window)))

;; Run the example
(main)