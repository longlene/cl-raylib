;;;; raylib [audio] example - sound loading
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.1, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/audio/audio_sound_loading.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-sound-loading
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-sound-loading)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - sound loading")

    (init-audio-device)                 ; Initialize audio device

    (let ((fx-wav (load-sound "resources/sound.wav"))   ; Load WAV audio file
          (fx-ogg (load-sound "resources/target.ogg"))) ; Load OGG audio file

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+) (play-sound fx-wav)) ; Play WAV sound
               (when (is-key-pressed +key-enter+) (play-sound fx-ogg)) ; Play OGG sound
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "Press SPACE to PLAY the WAV sound!" 200 180 20 +lightgray+)
               (draw-text "Press ENTER to PLAY the OGG sound!" 200 220 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-sound fx-wav)             ; Unload sound data
      (unload-sound fx-ogg)             ; Unload sound data

      (close-audio-device)              ; Close audio device

      (close-window))))                 ; Close window and OpenGL context

(main)
