;;;; core_window_should_close.lisp
;;;; 
;;;; cl-raylib [core] example - Window should close
;;;;
;;;; Translation of raylib's core_window_should_close.c example
;;;; This example demonstrates custom window close handling with confirmation
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-window-should-close
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-window-should-close)

(defun core-window-should-close ()
  "Window should close example - equivalent to raylib's core_window_should_close"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - window should close")
      (set-exit-key +key-null+)  ; Disable KEY_ESCAPE to close window, X-button still works
      
      (let ((exit-window-requested nil)  ; Flag to request window to exit
            (exit-window nil))           ; Flag to set window to exit
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until exit-window do
          
          ;; Update
          ;; Detect if X-button or KEY_ESCAPE have been pressed to close window
          (when (or (window-should-close) (is-key-pressed +key-escape+))
            (setf exit-window-requested t))
          
          (when exit-window-requested
            ;; A request for close window has been issued, we can save data before closing
            ;; or just show a message asking for confirmation
            (cond
              ((is-key-pressed +key-y+) (setf exit-window t))
              ((is-key-pressed +key-n+) (setf exit-window-requested nil))))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (if exit-window-requested
                (progn
                  (draw-rectangle 0 100 screen-width 200 +black+)
                  (draw-text "Are you sure you want to exit program? [Y/N]" 40 180 30 +white+))
                (draw-text "Try to close the window to get confirmation message!" 120 200 20 +lightgray+))))))))

;; Run the example
(core-window-should-close)