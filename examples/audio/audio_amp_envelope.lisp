;;;; raylib [audio] example - amp envelope
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Arbinda Rizki Muhammad (@arbipink) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2026 Arbinda Rizki Muhammad (@arbipink)
;;;; Common Lisp port of raylib/examples/audio/audio_amp_envelope.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-amp-envelope
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/audio-amp-envelope)

(defconstant +buffer-size+ 4096)
(defconstant +sample-rate+ 44100)

;; Wave state
(defconstant +idle+ 0)
(defconstant +attack+ 1)
(defconstant +decay+ 2)
(defconstant +sustain+ 3)
(defconstant +release+ 4)

;; Grouping all ADSR parameters and state into a struct
(defstruct envelope
  (attack-time 0.0 :type single-float)
  (decay-time 0.0 :type single-float)
  (sustain-level 0.0 :type single-float)
  (release-time 0.0 :type single-float)
  (current-value 0.0 :type single-float)
  (state +idle+ :type fixnum))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; NOTE: C passes audioTime by pointer, here the updated audio time is returned
(defun fill-audio-buffer (i buffer envelope-value audio-time)
  (let ((frequency 440))
    (setf (aref buffer i) (* envelope-value (sin (* 2.0 +pi+ frequency audio-time))))
    (+ audio-time (/ 1.0 +sample-rate+))))

(defun update-envelope (env)
  ;; Calculate the time delta for ONE sample (1/44100)
  (let ((sample-time (/ 1.0 +sample-rate+)))
    (case (envelope-state env)
      (#.+attack+
       (incf (envelope-current-value env) (* (/ 1.0 (envelope-attack-time env)) sample-time))
       (when (>= (envelope-current-value env) 1.0)
         (setf (envelope-current-value env) 1.0
               (envelope-state env) +decay+)))
      (#.+decay+
       (decf (envelope-current-value env) (* (/ (- 1.0 (envelope-sustain-level env)) (envelope-decay-time env)) sample-time))
       (when (<= (envelope-current-value env) (envelope-sustain-level env))
         (setf (envelope-current-value env) (envelope-sustain-level env)
               (envelope-state env) +sustain+)))
      (#.+sustain+
       (setf (envelope-current-value env) (envelope-sustain-level env)))
      (#.+release+
       (decf (envelope-current-value env) (* (/ (envelope-sustain-level env) (envelope-release-time env)) sample-time))
       (when (<= (envelope-current-value env) 0.001) ; Use a small threshold to avoid infinite tail
         (setf (envelope-current-value env) 0.0
               (envelope-state env) +idle+))))))

(defun draw-adsr-graph (env bounds)
  (draw-rectangle-rec bounds (fade +lightgray+ 0.3))
  (draw-rectangle-lines-ex bounds 1.0 +gray+)

  ;; Fixed visual width for sustain stage since it's an amplitude not a time value
  (let* ((sustain-width 1.0)

         ;; Total time to visualize (sum of A, D, R + a padding for Sustain)
         (total-time (+ (envelope-attack-time env) (envelope-decay-time env) sustain-width (envelope-release-time env)))

         (scale-x (/ (rectangle-width bounds) total-time))
         (scale-y (rectangle-height bounds))

         (start (vec2 (rectangle-x bounds) (+ (rectangle-y bounds) (rectangle-height bounds))))
         (peak (vec2 (+ (vx start) (* (envelope-attack-time env) scale-x)) (rectangle-y bounds)))
         (sustain (vec2 (+ (vx peak) (* (envelope-decay-time env) scale-x)) (+ (rectangle-y bounds) (* (- 1.0 (envelope-sustain-level env)) scale-y))))
         (rel (vec2 (+ (vx sustain) (* sustain-width scale-x)) (vy sustain)))
         (end (vec2 (+ (vx rel) (* (envelope-release-time env) scale-x)) (+ (rectangle-y bounds) (rectangle-height bounds)))))

    (draw-line-v start peak +skyblue+)
    (draw-line-v peak sustain +blue+)
    (draw-line-v sustain rel +darkblue+)
    (draw-line-v rel end +orange+)

    (draw-text "ADSR Visualizer" (truncate (rectangle-x bounds)) (truncate (- (rectangle-y bounds) 20)) 10 +darkgray+)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - amp envelope")

    (init-audio-device)

    ;; Set the number of samples the stream will keep in memory at a time to BUFFER_SIZE
    (set-audio-stream-buffer-size-default +buffer-size+)
    (let* ((buffer (make-array +buffer-size+ :element-type 'single-float :initial-element 0.0))

           ;; Init raw audio stream (sample rate: 44100, sample size: 32bit-float, channels: 1-mono)
           (stream (load-audio-stream +sample-rate+ 32 1))

           ;; Init Phase
           (audio-time 0.0)

           ;; Initialize the struct
           (env (make-envelope :attack-time 1.0
                               :decay-time 1.0
                               :sustain-level 0.5
                               :release-time 1.0
                               :current-value 0.0
                               :state +idle+)))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+) (setf (envelope-state env) +attack+))

               (when (and (is-key-released +key-space+) (/= (envelope-state env) +idle+)) (setf (envelope-state env) +release+))

               (when (is-audio-stream-processed stream)
                 (if (or (/= (envelope-state env) +idle+) (> (envelope-current-value env) 0.0))
                     (dotimes (i +buffer-size+)
                       (update-envelope env)
                       (setf audio-time (fill-audio-buffer i buffer (envelope-current-value env) audio-time)))
                     (progn
                       ;; Clear buffer if silent to avoid looping noise
                       (fill buffer 0.0)
                       (setf audio-time 0.0)))

                 (update-audio-stream stream buffer +buffer-size+))

               (unless (is-audio-stream-playing stream) (play-audio-stream stream))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (setf (envelope-attack-time env) (nth-value 1 (gui-slider-bar (make-rectangle :x 100.0 :y 60.0 :width 400.0 :height 30.0) "Attack (s)" (text-format "%2.2fs" (envelope-attack-time env)) (envelope-attack-time env) 0.1 3.0)))
               (setf (envelope-decay-time env) (nth-value 1 (gui-slider-bar (make-rectangle :x 100.0 :y 100.0 :width 400.0 :height 30.0) "Decay (s)" (text-format "%2.2fs" (envelope-decay-time env)) (envelope-decay-time env) 0.1 3.0)))
               (setf (envelope-sustain-level env) (nth-value 1 (gui-slider-bar (make-rectangle :x 100.0 :y 140.0 :width 400.0 :height 30.0) "Sustain" (text-format "%2.2f" (envelope-sustain-level env)) (envelope-sustain-level env) 0.0 1.0)))
               (setf (envelope-release-time env) (nth-value 1 (gui-slider-bar (make-rectangle :x 100.0 :y 180.0 :width 400.0 :height 30.0) "Release (s)" (text-format "%2.2fs" (envelope-release-time env)) (envelope-release-time env) 0.1 3.0)))

               (draw-adsr-graph env (make-rectangle :x 100.0 :y 250.0 :width 400.0 :height 100.0))

               (draw-circle-v (vec2 520.0 (- 350 (* (envelope-current-value env) 100))) 5 +maroon+)
               (draw-text (text-format "Current Gain: %2.2f" (envelope-current-value env)) 535 (truncate (- 345 (* (envelope-current-value env) 100))) 10 +maroon+)

               (draw-text "Press SPACE to PLAY the sound!" 200 400 20 +lightgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-audio-stream stream)
      (close-audio-device)

      (close-window))))
      ;;--------------------------------------------------------------------------------------

(main)
