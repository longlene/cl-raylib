;;;; text_format_text.lisp
;;;; 
;;;; cl-raylib [text] example - Text formatting
;;;;
;;;; Translation of raylib's text_format_text.c example
;;;; This example demonstrates various text formatting capabilities
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

(defpackage :cl-raylib-example-text-format-text
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-text-format-text)

(defun text-format-text ()
  "Text formatting example - equivalent to raylib's text_format_text"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [text] example - text formatting")
      (let ((score 100020)
            (hiscore 200450)
            (lives 5))
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          ;; TODO: Update your variables here
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (draw-text (text-format "Score: ~8,'0d" score) 200 80 20 +red+)
            
            (draw-text (text-format "HiScore: ~8,'0d" hiscore) 200 120 20 +green+)
            
            (draw-text (text-format "Lives: ~2,'0d" lives) 200 160 40 +blue+)
            
            (draw-text (text-format "Elapsed Time: ~6,2f ms" (* (get-frame-time) 1000)) 200 220 20 +black+)))))))

;; Run the example
(text-format-text)