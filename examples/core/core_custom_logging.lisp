;;;; core_custom_logging.lisp
;;;; 
;;;; cl-raylib [core] example - Custom logging
;;;;
;;;; Translation of raylib's core_custom_logging.c example
;;;; This example demonstrates custom logging functionality
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-custom-logging
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-custom-logging)

;; Custom logging function
(defun custom-log (msg-type text &rest args)
  "Custom logging function with timestamp and formatted output"
  (multiple-value-bind (second minute hour date month year)
      (get-decoded-time)
    (let ((time-str (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
                           year month date hour minute second)))
      (format t "[~a] " time-str)
      
      ;; Print log level
      (case msg-type
        (:log-info (format t "[INFO] : "))
        (:log-error (format t "[ERROR]: "))
        (:log-warning (format t "[WARN] : "))
        (:log-debug (format t "[DEBUG]: "))
        (t (format t "[UNKNOWN]: ")))
      
      ;; Print the message with arguments
      (apply #'format t text args)
      (format t "~%")
      (force-output))))

(defun core-custom-logging ()
  "Custom logging example - equivalent to raylib's core_custom_logging"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    ;; Set custom logger (in cl-raylib, we can override the trace-log function)
    (setf *trace-log-callback* #'custom-log)
    
    (with-window (screen-width screen-height "cl-raylib [core] example - custom logging")
      (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
      
      ;; Test the custom logging
      (trace-log :log-info "Custom logging system initialized")
      (trace-log :log-warning "This is a warning message")
      (trace-log :log-debug "Debug information: Frame rate target set to ~d FPS" 60)
      
      ;; Main game loop
      (loop until (window-should-close) do  ; Detect window close button or ESC key
        
        ;; Update
        ;; Add some periodic logging for demonstration
        (when (= (mod (get-time) 120) 0)  ; Log every 2 seconds at 60 FPS
          (trace-log :log-info "Game running... FPS: ~d" (get-fps)))
        
        ;; Draw
        (with-drawing
          (clear-background +raywhite+)
          
          (draw-text "Check out the console output to see the custom logger in action!" 60 200 20 +lightgray+)
          (draw-text "Custom log messages are being generated with timestamps" 60 230 16 +darkgray+)
          (draw-text "Press ESC or close window to exit" 60 260 14 +gray+))))))

;; Run the example
(core-custom-logging)