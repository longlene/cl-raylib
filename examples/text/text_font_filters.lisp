;;;; text_font_filters.lisp - Font filters example
;;;; Translated from raylib/examples/text/text_font_filters.c

(require :cl-raylib)

(defpackage :text-font-filters
  (:use :cl :cl-raylib))

(in-package :text-font-filters)

(defun main ()
  "Main function - font filters example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - font filters")

    (let ((msg "Loaded Font"))
      
      ;; TTF Font loading with custom generation parameters
      (let ((font (load-font-ex "resources/KAISG.ttf" 96 nil)))
        
        ;; Generate mipmap levels to use trilinear filtering
        ;; NOTE: On 2D drawing it won't be noticeable, it looks like FILTER_BILINEAR
        (gen-texture-mipmaps (font-texture font))
        
        (let ((font-size (float (font-base-size font)))
              (font-position (vec2 40.0 (- (/ screen-height 2.0) 80.0)))
              (text-size (vec2 0.0 0.0))
              (current-font-filter 0))  ; TEXTURE_FILTER_POINT
          
          ;; Setup texture scaling filter
          (set-texture-filter (font-texture font) +texture-filter-point+)
          
          (set-target-fps 60)
          
          ;; Main game loop
          (loop until (window-should-close) do
            ;; Update
            (incf font-size (* (get-mouse-wheel-move) 4.0))
            
            ;; Choose font texture filter method
            (cond
              ((is-key-pressed +key-one+)
               (set-texture-filter (font-texture font) +texture-filter-point+)
               (setf current-font-filter 0))
              ((is-key-pressed +key-two+)
               (set-texture-filter (font-texture font) +texture-filter-bilinear+)
               (setf current-font-filter 1))
              ((is-key-pressed +key-three+)
               ;; NOTE: Trilinear filter won't be noticed on 2D drawing
               (set-texture-filter (font-texture font) +texture-filter-trilinear+)
               (setf current-font-filter 2)))
            
            (setf text-size (measure-text-ex font msg font-size 0))
            
            (cond
              ((is-key-down +key-left+) (decf (vx font-position) 10))
              ((is-key-down +key-right+) (incf (vx font-position) 10)))
            
            ;; Load a dropped TTF file dynamically (at current fontSize)
            (when (is-file-dropped)
              (let ((dropped-files (load-dropped-files)))
                ;; NOTE: We only support first ttf file dropped
                (when (is-file-extension (aref (file-path-list-paths dropped-files) 0) ".ttf")
                  (unload-font font)
                  (setf font (load-font-ex (aref (file-path-list-paths dropped-files) 0) 
                                          (truncate font-size) nil)))
                (unload-dropped-files dropped-files)))
            
            ;; Draw
            (begin-drawing)
              (clear-background +raywhite+)
              
              (draw-text "Use mouse wheel to change font size" 20 20 10 +gray+)
              (draw-text "Use KEY_RIGHT and KEY_LEFT to move text" 20 40 10 +gray+)
              (draw-text "Use 1, 2, 3 to change texture filter" 20 60 10 +gray+)
              (draw-text "Drop a new TTF font for dynamic loading" 20 80 10 +darkgray+)
              
              (draw-text-ex font msg font-position font-size 0 +black+)
              
              (draw-rectangle 0 (- screen-height 80) screen-width 80 +lightgray+)
              (draw-text (format nil "Font size: ~,2f" font-size) 20 (- screen-height 50) 10 +darkgray+)
              (draw-text (format nil "Text size: [~,2f, ~,2f]" (vx text-size) (vy text-size)) 
                        20 (- screen-height 30) 10 +darkgray+)
              (draw-text "CURRENT TEXTURE FILTER:" 250 400 20 +gray+)
              
              (cond
                ((= current-font-filter 0) (draw-text "POINT" 570 400 20 +black+))
                ((= current-font-filter 1) (draw-text "BILINEAR" 570 400 20 +black+))
                ((= current-font-filter 2) (draw-text "TRILINEAR" 570 400 20 +black+)))
              
            (end-drawing)))
        
        ;; De-Initialization
        (unload-font font)))

    ;; Close window
    (close-window)))

;; Run the example
(main)