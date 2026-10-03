;;;; raylib [core] example - custom logging
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Pablo Marcos Oltra (@pamarcos) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2018-2025 Pablo Marcos Oltra (@pamarcos) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_custom_logging.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-custom-logging
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-custom-logging)

;; Custom logging function
;; NOTE: The callback receives the already formatted message text
(defun custom-trace-log (msg-type text)
  (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
    (format t "[~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d] " year month day hour min sec))

  (case msg-type
    (#.+log-info+ (format t "[INFO] : "))
    (#.+log-error+ (format t "[ERROR]: "))
    (#.+log-warning+ (format t "[WARN] : "))
    (#.+log-debug+ (format t "[DEBUG]: ")))

  (format t "~a~%" text))

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; Set custom logger
    (set-trace-log-callback #'custom-trace-log)

    (init-window screen-width screen-height "raylib [core] example - custom logging")

    (set-target-fps 60)                 ; Set our game to run at 60 frames-per-second
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             ;; TODO: Update your variables here
             ;;----------------------------------------------------------------------------------

             ;; Draw
             ;;----------------------------------------------------------------------------------
             (begin-drawing)

             (clear-background +raywhite+)

             (draw-text "Check out the console output to see the custom logger in action!" 60 200 20 +lightgray+)

             (end-drawing))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (close-window)))                    ; Close window and OpenGL context

(main)
