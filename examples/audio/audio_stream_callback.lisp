;;;; raylib [audio] example - stream callback
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example created by Dan Hoang (@dan-hoang) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; NOTE: Example sends a wave to the audio device,
;;;;   user gets the choice of four waves: sine, square, triangle, and sawtooth
;;;;   A stream is set up to play to the audio device; stream is hooked to a callback that
;;;;   generates a wave, that is determined by user choice
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Dan Hoang (@dan-hoang)
;;;; Common Lisp port of raylib/examples/audio/audio_stream_callback.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-stream-callback
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-stream-callback)

(defconstant +buffer-size+ 4096)
(defconstant +sample-rate+ 44100)

;; Wave type
(defconstant +sine+ 0)
(defconstant +square+ 1)
(defconstant +triangle+ 2)
(defconstant +sawtooth+ 3)

(defvar *wave-frequency* 440)
(defvar *new-wave-frequency* 440)
(defvar *wave-index* 0)

;; Buffer to keep the last second of uploaded audio,
;; part of which will be drawn on the screen
(defvar *buffer* (make-array +sample-rate+ :element-type 'single-float :initial-element 0.0))
(defvar *wave-types-as-string* #("sine" "square" "triangle" "sawtooth"))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; NOTE: FRAMES-OUT is the stream internal buffer (single-float, mono)

;; Save the synthesized samples for later drawing
(defun save-samples (frames-out frame-count)
  (replace *buffer* *buffer* :start1 0 :start2 frame-count)
  (replace *buffer* frames-out :start1 (- +sample-rate+ frame-count) :end2 frame-count))

(defun next-wave-index (wavelength)
  (incf *wave-index*)

  (when (>= *wave-index* wavelength)
    (setf *wave-frequency* *new-wave-frequency*)
    (setf *wave-index* 0)))

(defun sine-callback (frames-out frame-count)
  (let ((wavelength (floor +sample-rate+ *wave-frequency*)))

    ;; Synthesize the sine wave
    (dotimes (i frame-count)
      (setf (aref frames-out i) (sin (/ (* 2 +pi+ *wave-index*) wavelength)))
      (next-wave-index wavelength)))

  (save-samples frames-out frame-count))

(defun square-callback (frames-out frame-count)
  (let ((wavelength (floor +sample-rate+ *wave-frequency*)))

    ;; Synthesize the square wave
    (dotimes (i frame-count)
      (setf (aref frames-out i) (if (< *wave-index* (floor wavelength 2)) 1.0 -1.0))
      (next-wave-index wavelength)))

  (save-samples frames-out frame-count))

(defun triangle-callback (frames-out frame-count)
  (let* ((wavelength (floor +sample-rate+ *wave-frequency*))
         (half (floor wavelength 2)))

    ;; Synthesize the triangle wave
    (dotimes (i frame-count)
      (setf (aref frames-out i) (if (< *wave-index* half)
                                    (+ -1 (/ (* 2.0 *wave-index*) half))
                                    (- 1 (/ (* 2.0 (- *wave-index* half)) half))))
      (next-wave-index wavelength)))

  (save-samples frames-out frame-count))

(defun sawtooth-callback (frames-out frame-count)
  (let ((wavelength (floor +sample-rate+ *wave-frequency*)))

    ;; Synthesize the sawtooth wave
    (dotimes (i frame-count)
      (setf (aref frames-out i) (+ -1 (/ (* 2.0 *wave-index*) wavelength)))
      (next-wave-index wavelength)))

  (save-samples frames-out frame-count))

(defvar *wave-callbacks* (vector #'sine-callback #'square-callback #'triangle-callback #'sawtooth-callback))

;; NOTE: C reads buffer[SAMPLE_RATE] (one past the end) for the last line point, read as 0 here
(defun buffer-sample (i)
  (if (< i +sample-rate+) (aref *buffer* i) 0.0))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - stream callback")

    (init-audio-device)

    ;; Set the number of samples the stream will keep in memory at a time to BUFFER_SIZE
    (set-audio-stream-buffer-size-default +buffer-size+)

    ;; Init raw audio stream (sample rate: 44100, sample size: 32bit-float, channels: 1-mono)
    (let ((stream (load-audio-stream +sample-rate+ 32 1))
          (wave-type +sine+))
      (play-audio-stream stream)

      ;; Configure it so that waveCallbacks[waveType] is called whenever stream is out of samples
      (set-audio-stream-callback stream (aref *wave-callbacks* wave-type))

      (set-target-fps 30)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-down +key-up+)
                 (incf *new-wave-frequency* 10)
                 (when (> *new-wave-frequency* 12500) (setf *new-wave-frequency* 12500)))

               (when (is-key-down +key-down+)
                 (decf *new-wave-frequency* 10)
                 (when (< *new-wave-frequency* 20) (setf *new-wave-frequency* 20)))

               (when (is-key-pressed +key-left+)
                 (setf wave-type (cond ((= wave-type +sine+) +sawtooth+)
                                       ((= wave-type +square+) +sine+)
                                       ((= wave-type +triangle+) +square+)
                                       (t +triangle+)))

                 (set-audio-stream-callback stream (aref *wave-callbacks* wave-type)))

               (when (is-key-pressed +key-right+)
                 (setf wave-type (cond ((= wave-type +sine+) +square+)
                                       ((= wave-type +square+) +triangle+)
                                       ((= wave-type +triangle+) +sawtooth+)
                                       (t +sine+)))

                 (set-audio-stream-callback stream (aref *wave-callbacks* wave-type)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)
               (draw-text (text-format "frequency: %i" *new-wave-frequency*) (- screen-width 220) 10 20 +red+)
               (draw-text (text-format "wave type: %s" (aref *wave-types-as-string* wave-type)) (- screen-width 220) 30 20 +red+)
               (draw-text "Up/down to change frequency" 10 10 20 +darkgray+)
               (draw-text "Left/right to change wave type" 10 30 20 +darkgray+)

               ;; Draw the last 10 ms of uploaded audio
               (let ((start (- +sample-rate+ (floor +sample-rate+ 100))))
                 (dotimes (i screen-width)
                   (let ((start-pos (vec2 (float i) (- 250 (* 50 (buffer-sample (+ start (floor (floor (* i +sample-rate+) 100) screen-width)))))))
                         (end-pos (vec2 (float (1+ i)) (- 250 (* 50 (buffer-sample (+ start (floor (floor (* (1+ i) +sample-rate+) 100) screen-width))))))))
                     (draw-line-v start-pos end-pos +red+))))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-audio-stream stream)      ; Close raw audio stream and delete buffers from RAM
      (close-audio-device)              ; Close audio device (music streaming is automatically stopped)

      (close-window))))                 ; Close window and OpenGL context

(main)
