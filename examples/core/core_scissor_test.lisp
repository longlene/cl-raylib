;;;; core_scissor_test.lisp
;;;; 
;;;; cl-raylib [core] example - Scissor test
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Translation of raylib's core_scissor_test.c example
;;;; This example demonstrates OpenGL scissor test functionality
;;;; for clipping rendering to a specific rectangular area
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-scissor-test
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-scissor-test)

(defun core-scissor-test ()
  "Scissor test example - demonstrate OpenGL scissor functionality"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - scissor test")
      (let ((scissor-area (make-rectangle :x 0.0 :y 0.0 :width 300.0 :height 300.0))
            (scissor-mode t))
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          ;; Toggle scissor mode with S key
          (when (is-key-pressed :key-s)
            (setf scissor-mode (not scissor-mode)))
          
          ;; Center the scissor area around the mouse position
          (let ((mouse-x (get-mouse-x))
                (mouse-y (get-mouse-y)))
            (setf (rectangle-x scissor-area) (- mouse-x (/ (rectangle-width scissor-area) 2.0)))
            (setf (rectangle-y scissor-area) (- mouse-y (/ (rectangle-height scissor-area) 2.0))))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            ;; Enable scissor mode if active
            (when scissor-mode
              (begin-scissor-mode (round (rectangle-x scissor-area))
                                 (round (rectangle-y scissor-area)) 
                                 (round (rectangle-width scissor-area))
                                 (round (rectangle-height scissor-area))))
            
            ;; Draw full screen rectangle and some text
            ;; NOTE: Only part defined by scissor area will be rendered
            (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +red+)
            (draw-text "Move the mouse around to reveal this text!" 190 200 20 +lightgray+)
            
            ;; Disable scissor mode if it was active
            (when scissor-mode
              (end-scissor-mode))
            
            ;; Draw scissor area outline and instructions
            (draw-rectangle-lines-ex scissor-area 1.0 +black+)
            (draw-text "Press S to toggle scissor test" 10 10 20 +black+)))))))

;; Run the example
(core-scissor-test)
