;;;; simple_shader_test.lisp
;;;; 
;;;; Simple shader test - debug window close issue
;;;;

(require :cl-raylib)

(defpackage :cl-raylib-simple-shader-test
  (:use :cl :cl-raylib))

(in-package :cl-raylib-simple-shader-test)

(defun simple-shader-test ()
  "Simple shader test to debug window close issue"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [shaders] simple test")
      (format t "Window initialized, starting main loop~%")
      (format t "Press ESC to exit~%")
      
      (set-target-fps 60)
      
      ;; Main game loop with explicit debug
      (loop 
        do (progn
             ;; Check exit conditions explicitly
             (when (window-should-close)
               (format t "Window should close flag is true~%")
               (return))
             
             ;; Check ESC key explicitly
             (when (is-key-pressed +key-escape+)
               (format t "ESC key pressed, exiting~%")
               (return))
             
             ;; Draw
             (with-drawing
               (clear-background +raywhite+)
               (draw-text "Simple Shader Test" 20 20 20 +black+)
               (draw-text "Press ESC to exit" 20 50 16 +darkgray+)
               (draw-circle 400 225 50 +red+))))
      
      (format t "Exiting main loop~%"))))

;; Run the test
(simple-shader-test)