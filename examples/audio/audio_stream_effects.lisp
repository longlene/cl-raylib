;;;; raylib [audio] example - stream effects
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 4.2, last time updated with raylib 5.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2022-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/audio/audio_stream_effects.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-stream-effects
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-stream-effects)

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defvar *delay-buffer* nil)
(defvar *delay-buffer-size* 0)
(defvar *delay-read-index* 2)
(defvar *delay-write-index* 0)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; NOTE: BUFFER holds the frames as interleaved stereo floats

;; Audio effect: lowpass filter
(let ((low (make-array 2 :element-type 'single-float :initial-element 0.0)) ; C static local
      (cutoff (/ 70.0 44100.0)))       ; 70 Hz lowpass filter
  (defun audio-process-effect-lpf (buffer frames)
    (let ((k (/ cutoff (+ cutoff 0.1591549431)))) ; RC filter formula

      ;; Converts the buffer data before using it
      (loop for i from 0 below (* frames 2) by 2
            do (let ((l (aref buffer i))
                     (r (aref buffer (1+ i))))
                 (incf (aref low 0) (* k (- l (aref low 0))))
                 (incf (aref low 1) (* k (- r (aref low 1))))
                 (setf (aref buffer i) (aref low 0)
                       (aref buffer (1+ i)) (aref low 1)))))))

;; Audio effect: delay
(defun audio-process-effect-delay (buffer frames)
  (loop for i from 0 below (* frames 2) by 2
        do (let ((left-delay (aref *delay-buffer* (shiftf *delay-read-index* (1+ *delay-read-index*)))) ; ERROR: Reading buffer -> WHY??? Maybe thread related???
                 (right-delay (aref *delay-buffer* (shiftf *delay-read-index* (1+ *delay-read-index*)))))

             (when (= *delay-read-index* *delay-buffer-size*) (setf *delay-read-index* 0))

             (setf (aref buffer i) (+ (* 0.5 (aref buffer i)) (* 0.5 left-delay))
                   (aref buffer (1+ i)) (+ (* 0.5 (aref buffer (1+ i))) (* 0.5 right-delay)))

             (setf (aref *delay-buffer* (shiftf *delay-write-index* (1+ *delay-write-index*))) (aref buffer i))
             (setf (aref *delay-buffer* (shiftf *delay-write-index* (1+ *delay-write-index*))) (aref buffer (1+ i)))
             (when (= *delay-write-index* *delay-buffer-size*) (setf *delay-write-index* 0)))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - stream effects")

    (init-audio-device)                 ; Initialize audio device

    (let ((music (load-music-stream "resources/country.mp3"))
          (time-played 0.0)             ; Time played normalized [0.0f..1.0f]
          (pause nil)                   ; Music playing paused

          (enable-effect-lpf nil)       ; Enable effect low-pass-filter
          (enable-effect-delay nil))    ; Enable effect delay (1 second)

      ;; Allocate buffer for the delay effect
      (setf *delay-buffer-size* (* 48000 2)) ; 1 second delay (device sampleRate*channels)
      (setf *delay-buffer* (make-array *delay-buffer-size* :element-type 'single-float :initial-element 0.0))

      (play-music-stream music)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-music-stream music) ; Update music buffer with new stream data

               ;; Restart music playing (stop and play)
               (when (is-key-pressed +key-space+)
                 (stop-music-stream music)
                 (play-music-stream music))

               ;; Pause/Resume music playing
               (when (is-key-pressed +key-p+)
                 (setf pause (not pause))

                 (if pause
                     (pause-music-stream music)
                     (resume-music-stream music)))

               ;; Add/Remove effect: lowpass filter
               (when (is-key-pressed +key-f+)
                 (setf enable-effect-lpf (not enable-effect-lpf))
                 (if enable-effect-lpf
                     (attach-audio-stream-processor (music-stream music) #'audio-process-effect-lpf)
                     (detach-audio-stream-processor (music-stream music) #'audio-process-effect-lpf)))

               ;; Add/Remove effect: delay
               (when (is-key-pressed +key-d+)
                 (setf enable-effect-delay (not enable-effect-delay))
                 (if enable-effect-delay
                     (attach-audio-stream-processor (music-stream music) #'audio-process-effect-delay)
                     (detach-audio-stream-processor (music-stream music) #'audio-process-effect-delay)))

               ;; Get normalized time played for current music stream
               (setf time-played (/ (get-music-time-played music) (get-music-time-length music)))

               (when (> time-played 1.0) (setf time-played 1.0)) ; Make sure time played is no longer than music
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-text "MUSIC SHOULD BE PLAYING!" 245 150 20 +lightgray+)

               (draw-rectangle 200 180 400 12 +lightgray+)
               (draw-rectangle 200 180 (truncate (* time-played 400.0)) 12 +maroon+)
               (draw-rectangle-lines 200 180 400 12 +gray+)

               (draw-text "PRESS SPACE TO RESTART MUSIC" 215 230 20 +lightgray+)
               (draw-text "PRESS P TO PAUSE/RESUME MUSIC" 208 260 20 +lightgray+)

               (draw-text (text-format "PRESS F TO TOGGLE LPF EFFECT: %s" (if enable-effect-lpf "ON" "OFF")) 200 320 20 +gray+)
               (draw-text (text-format "PRESS D TO TOGGLE DELAY EFFECT: %s" (if enable-effect-delay "ON" "OFF")) 180 350 20 +gray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-music-stream music)       ; Unload music stream buffers from RAM

      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (setf *delay-buffer* nil)         ; Free delay buffer

      (close-window))))                 ; Close window and OpenGL context

(main)
