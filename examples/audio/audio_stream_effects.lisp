;;;; audio_stream_effects.lisp - Music stream processing effects
;;;; Translated from raylib/examples/audio/audio_stream_effects.c

(require :cl-raylib)

(defpackage :audio-stream-effects
  (:use :cl :cl-raylib))

(in-package :audio-stream-effects)

;; Required delay effect variables
(defparameter *delay-buffer* nil)
(defparameter *delay-buffer-size* 0)
(defparameter *delay-read-index* 2)
(defparameter *delay-write-index* 0)

;; Low-pass filter state
(defparameter *low* (vector 0.0 0.0))

(defun audio-process-effect-lpf (buffer frames)
  "Audio effect: lowpass filter"
  (let* ((cutoff (/ 70.0 44100.0))  ; 70 Hz lowpass filter
         (k (/ cutoff (+ cutoff 0.1591549431))))  ; RC filter formula
    
    ;; Process stereo audio data
    (loop for i from 0 below (* frames 2) by 2 do
      (let ((l (aref buffer i))
            (r (aref buffer (1+ i))))
        
        (incf (aref *low* 0) (* k (- l (aref *low* 0))))
        (incf (aref *low* 1) (* k (- r (aref *low* 1))))
        
        (setf (aref buffer i) (aref *low* 0))
        (setf (aref buffer (1+ i)) (aref *low* 1))))))

(defun audio-process-effect-delay (buffer frames)
  "Audio effect: delay"
  (when *delay-buffer*
    (loop for i from 0 below (* frames 2) by 2 do
      ;; Read from delay buffer
      (let ((left-delay (aref *delay-buffer* *delay-read-index*))
            (right-delay (aref *delay-buffer* (1+ *delay-read-index*))))
        
        (incf *delay-read-index* 2)
        (when (>= *delay-read-index* *delay-buffer-size*)
          (setf *delay-read-index* 0))
        
        ;; Mix original with delayed signal
        (let ((original-left (aref buffer i))
              (original-right (aref buffer (1+ i))))
          
          (setf (aref buffer i) (+ (* 0.5 original-left) (* 0.5 left-delay)))
          (setf (aref buffer (1+ i)) (+ (* 0.5 original-right) (* 0.5 right-delay)))
          
          ;; Write to delay buffer
          (setf (aref *delay-buffer* *delay-write-index*) (aref buffer i))
          (setf (aref *delay-buffer* (1+ *delay-write-index*)) (aref buffer (1+ i)))
          
          (incf *delay-write-index* 2)
          (when (>= *delay-write-index* *delay-buffer-size*)
            (setf *delay-write-index* 0)))))))

(defun main ()
  "Main function - stream effects example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [audio] example - stream effects")
    (init-audio-device)

    ;; Load music
    (let ((music (load-music-stream "resources/country.mp3")))
      
      ;; Allocate buffer for the delay effect (1 second delay)
      (setf *delay-buffer-size* (* 48000 2))  ; 48kHz * 2 channels
      (setf *delay-buffer* (make-array *delay-buffer-size* :initial-element 0.0 :element-type 'single-float))
      
      (play-music-stream music)
      
      (let ((time-played 0.0)
            (pause nil)
            (enable-effect-lpf nil)
            (enable-effect-delay nil))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          (update-music-stream music)

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
          (let ((time-length (get-music-time-length music)))
            (when (> time-length 0.0)
              (setf time-played (/ (get-music-time-played music) time-length))
              (when (> time-played 1.0)
                (setf time-played 1.0))))

          ;; Draw
          (begin-drawing)
            (clear-background +raywhite+)

            (draw-text "MUSIC SHOULD BE PLAYING!" 245 150 20 +lightgray+)

            ;; Draw progress bar
            (draw-rectangle 200 180 400 12 +lightgray+)
            (draw-rectangle 200 180 (truncate (* time-played 400.0)) 12 +maroon+)
            (draw-rectangle-lines 200 180 400 12 +gray+)

            (draw-text "PRESS SPACE TO RESTART MUSIC" 215 230 20 +lightgray+)
            (draw-text "PRESS P TO PAUSE/RESUME MUSIC" 208 260 20 +lightgray+)

            (draw-text (format nil "PRESS F TO TOGGLE LPF EFFECT: ~a" 
                              (if enable-effect-lpf "ON" "OFF")) 
                      200 320 20 +gray+)
            (draw-text (format nil "PRESS D TO TOGGLE DELAY EFFECT: ~a" 
                              (if enable-effect-delay "ON" "OFF")) 
                      180 350 20 +gray+)

          (end-drawing))

        ;; De-Initialization
        (unload-music-stream music))

      ;; Free delay buffer
      (setf *delay-buffer* nil))

    (close-audio-device)
    (close-window)))

;; Run the example
(main)