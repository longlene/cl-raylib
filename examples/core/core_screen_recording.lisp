;;;; raylib [core] example - screen recording
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_screen_recording.c

(require :cl-raylib)

;; Using msf_gif library to record frames into GIF
(load (merge-pathnames "msf_gif.lisp" *load-truename*)) ; GIF recording functionality

(defpackage #:raylib-examples/core-screen-recording
  (:use #:cl #:raylib #:msf-gif))
(in-package #:raylib-examples/core-screen-recording)

(defconstant +gif-record-framerate+ 5)  ; Record framerate, we get a frame every N frames

(defconstant +max-sinewave-points+ 256)

;; libm sinf(), as used by the C example
(defun sinf (x) (cffi:foreign-funcall "sinf" :float (float x 1.0) :float))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - screen recording")

    (let ((gif-recording nil)           ; GIF recording state
          (gif-frame-counter 0)         ; GIF frames counter
          (gif-state (make-msf-gif-state)) ; MSGIF context state

          (circle-position (vec2 0.0 (/ screen-height 2.0)))
          (time-counter 0.0)

          ;; Get sine wave points for line drawing
          (sine-points (make-array +max-sinewave-points+)))
      (dotimes (i +max-sinewave-points+)
        (setf (aref sine-points i)
              (vec2 (/ (* i (get-screen-width)) 180.0)
                    (+ (/ screen-height 2.0) (* 150 (sinf (* (/ (* 2 +pi+) 1.5) (/ 1.0 60.0) (float i)))))))) ; Calculate for 60 fps

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Update circle sinusoidal movement
               (incf time-counter (get-frame-time))
               (incf (vx circle-position) (/ (get-screen-width) 180.0))
               (setf (vy circle-position) (+ (/ screen-height 2.0) (* 150 (sinf (* (/ (* 2 +pi+) 1.5) time-counter)))))
               (when (> (vx circle-position) screen-width)
                 (setf (vx circle-position) 0.0)
                 (setf (vy circle-position) (/ screen-height 2.0))
                 (setf time-counter 0.0))

               ;; Start-Stop GIF recording on CTRL+R
               (when (and (is-key-down +key-left-control+) (is-key-pressed +key-r+))
                 (if gif-recording
                     (progn
                       ;; Stop current recording and save file
                       (setf gif-recording nil)
                       (let ((result (msf-gif-end gif-state)))
                         (save-file-data (text-format "%s/screenrecording.gif" (get-application-directory))
                                         (msf-gif-result-data result) (msf-gif-result-data-size result))
                         (msf-gif-free result))

                       (trace-log +log-info+ "Finish animated GIF recording"))
                     (progn
                       ;; Start a new recording
                       (setf gif-recording t)
                       (setf gif-frame-counter 0)
                       (msf-gif-begin gif-state (get-render-width) (get-render-height))

                       (trace-log +log-info+ "Start animated GIF recording"))))

               (when gif-recording
                 (incf gif-frame-counter)

                 ;; NOTE: We record one gif frame depending on the desired gif framerate
                 (when (> gif-frame-counter +gif-record-framerate+)
                   ;; Get image data for the current frame (from backbuffer)
                   ;; WARNING: This process is quite slow, it can generate stuttering
                   (let ((im-screen (load-image-from-screen)))

                     ;; Add the frame to the gif recording, providing and "estimated" time for display in centiseconds
                     (msf-gif-frame gif-state (image-data im-screen) (floor (truncate (* (/ 1.0 60.0) +gif-record-framerate+)) 10) 16 (* (image-width im-screen) 4))
                     (setf gif-frame-counter 0)

                     (unload-image im-screen)))) ; Free image data
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i (1- +max-sinewave-points+))
                 (draw-line-v (aref sine-points i) (aref sine-points (1+ i)) +maroon+)
                 (draw-circle-v (aref sine-points i) 3 +maroon+))

               (draw-circle-v circle-position 30 +red+)

               (draw-fps 10 10)

               #|
               ;; Draw record indicator
               ;; WARNING: If drawn here, it will appear in the recorded image,
               ;; use a render texture instead for the recording and (load-image-from-texture (render-texture-texture rt))
               (when gif-recording
                 ;; Display the recording indicator every half-second
                 (when (= (mod (truncate (/ (get-time) 0.5)) 2) 1)
                   (draw-circle 30 (- (get-screen-height) 20) 10 +maroon+)
                   (draw-text "GIF RECORDING" 50 (- (get-screen-height) 25) 10 +red+)))
               |#
               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      ;; If still recording a GIF on close window, just finish
      (when gif-recording
        (let ((result (msf-gif-end gif-state)))
          (msf-gif-free result))
        (setf gif-recording nil))

      (close-window))))                 ; Close window and OpenGL context

(main)
