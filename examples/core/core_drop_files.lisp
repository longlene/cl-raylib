;;;; core_drop_files.lisp
;;;; 
;;;; cl-raylib [core] example - Windows drop files
;;;;
;;;; Translation of raylib's core_drop_files.c example
;;;; This example demonstrates drag & drop file functionality
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; NOTE: This example only works on platforms that support drag & drop (Windows, Linux, OSX)
;;;;
;;;; This example has been created using cl-raylib
;;;; cl-raylib is a Common Lisp implementation of raylib
;;;; https://github.com/longlene/cl-raylib
;;;;
;;;; Copyright (c) 2025 cl-raylib contributors
;;;; Licensed under the unmodified zlib/libpng license

(require :cl-raylib)

(defpackage :cl-raylib-example-core-drop-files
  (:use :cl :cl-raylib))

(in-package :cl-raylib-example-core-drop-files)

(defconstant +max-filepath-recorded+ 4096)
(defconstant +max-filepath-size+ 2048)

(defun core-drop-files ()
  "Drop files example - equivalent to raylib's core_drop_files"
  (let ((screen-width 800)
        (screen-height 450))
    
    ;; Initialization
    (with-window (screen-width screen-height "cl-raylib [core] example - drop files")
      (let ((file-path-counter 0)
            (file-paths (make-array +max-filepath-recorded+ :initial-element "")))
        
        (set-target-fps 60)  ; Set our game to run at 60 frames-per-second
        
        ;; Main game loop
        (loop until (window-should-close) do  ; Detect window close button or ESC key
          
          ;; Update
          (when (is-file-dropped)
            (let ((dropped-files (load-dropped-files)))
              (loop for i from 0 below (file-path-list-count dropped-files) do
                (when (< file-path-counter (1- +max-filepath-recorded+))
                  (setf (aref file-paths (+ file-path-counter i))
                        (file-path-list-path dropped-files i))
                  (incf file-path-counter)))
              (unload-dropped-files dropped-files)))  ; Unload filepaths from memory
          
          ;; Draw
          (with-drawing
            (clear-background +raywhite+)
            
            (if (= file-path-counter 0)
              (draw-text "Drop your files to this window!" 100 40 20 +darkgray+)
              (progn
                (draw-text "Dropped files:" 100 40 20 +darkgray+)
                
                (loop for i from 0 below file-path-counter do
                  (if (evenp i)
                    (draw-rectangle 0 (+ 85 (* 40 i)) screen-width 40 (color-fade +lightgray+ 0.5))
                    (draw-rectangle 0 (+ 85 (* 40 i)) screen-width 40 (color-fade +lightgray+ 0.3)))
                  
                  (draw-text (aref file-paths i) 120 (+ 100 (* 40 i)) 10 +gray+))
                
                (draw-text "Drop new files..." 100 (+ 110 (* 40 file-path-counter)) 20 +darkgray+)))))))))

;; Run the example
(core-drop-files)
