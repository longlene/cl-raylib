;;;; core_input_mouse_wheel.lisp
;;;; 
;;;; cl-raylib [core] example - Mouse wheel input
;;;;
;;;; Translation of raylib's core_input_mouse_wheel.c example
;;;; This example demonstrates mouse wheel input handling
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

(defpackage :cl-raylib-example-core-input-mouse-wheel
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-input-mouse-wheel)

(defun core-input-mouse-wheel ()
  "Mouse wheel input example - equivalent to raylib's core_input_mouse_wheel"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - input mouse wheel")
      (let ((box-position-y (- (/ screen-height 2) 40))
            (scroll-speed 4))  ; Scrolling speed in pixels
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (decf box-position-y (round (* (get-mouse-wheel-move) scroll-speed)))
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (draw-rectangle (- (/ screen-width 2) 40) box-position-y 80 80 +maroon+)
            
            (draw-text "Use mouse wheel to move the cube up and down!" 10 10 20 +gray+)
            (draw-text (text-format "Box position Y: ~3,'0d" box-position-y) 10 40 20 +lightgray+)))))))

;; Run the example
(core-input-mouse-wheel)