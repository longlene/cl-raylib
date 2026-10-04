;;;; raylib [audio] example - sound multi
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.0
;;;;
;;;; Example contributed by Jeffery Myers (@JeffM2501) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2023-2025 Jeffery Myers (@JeffM2501)
;;;; Common Lisp port of raylib/examples/audio/audio_sound_multi.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-sound-multi
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-sound-multi)

(defconstant +max-sounds+ 10)

(defvar *sound-array* (make-array +max-sounds+ :initial-element nil))
(defvar *current-sound* 0)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - sound multi")

    (init-audio-device)                 ; Initialize audio device

    ;; Load audio file into the first slot as the 'source' sound,
    ;; this sound owns the sample data
    (setf (aref *sound-array* 0) (load-sound "resources/sound.wav"))

    ;; Load an alias of the sound into slots 1-9. These do not own the sound data, but can be played
    (loop for i from 1 below +max-sounds+ do (setf (aref *sound-array* i) (load-sound-alias (aref *sound-array* 0))))

    (setf *current-sound* 0)            ; Set the sound list to the start

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             (when (is-key-pressed +key-space+)
               (play-sound (aref *sound-array* *current-sound*)) ; Play the next open sound slot
               (incf *current-sound*)                             ; Increment the sound slot

               ;; If the sound slot is out of bounds, go back to 0
               (when (>= *current-sound* +max-sounds+) (setf *current-sound* 0)))

             ;; NOTE: Another approach would be to look at the list for the first sound
             ;; that is not playing and use that slot
             ;;----------------------------------------------------------------------------------

             ;; Draw
             ;;----------------------------------------------------------------------------------
             (begin-drawing)

             (clear-background +raywhite+)

             (draw-text "Press SPACE to PLAY a WAV sound!" 200 180 20 +lightgray+)

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (loop for i from 1 below +max-sounds+ do (unload-sound-alias (aref *sound-array* i))) ; Unload sound aliases
    (unload-sound (aref *sound-array* 0)) ; Unload source sound data

    (close-audio-device)                ; Close audio device

    (close-window)))                    ; Close window and OpenGL context

(main)
