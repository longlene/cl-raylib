;;;; text_font_sdf.lisp - Font SDF loading example
;;;; Translated from raylib/examples/text/text_font_sdf.c

(require :cl-raylib)

(defpackage :text-font-sdf
  (:use :cl :cl-raylib))

(in-package :text-font-sdf)

(defconstant +glsl-version+ 330)  ; Desktop OpenGL version

(defun main ()
  "Main function - SDF fonts example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [text] example - SDF fonts")

    (let ((msg "Signed Distance Fields"))
      
      ;; Loading file to memory
      (let ((file-data (load-file-data "resources/anonymous_pro_bold.ttf")))
        
        ;; Default font generation from TTF font
        (let ((font-default (make-font :base-size 16 :glyph-count 95)))
          
          ;; Loading font data from memory data
          (setf (font-glyphs font-default) (load-font-data file-data 16 nil 95 +font-default+))
          (let ((atlas (gen-image-font-atlas (font-glyphs font-default) (font-recs font-default) 95 16 4 0)))
            (setf (font-texture font-default) (load-texture-from-image atlas))
            (unload-image atlas))
          
          ;; SDF font generation from TTF font
          (let ((font-sdf (make-font :base-size 16 :glyph-count 95)))
            
            (setf (font-glyphs font-sdf) (load-font-data file-data 16 nil 0 +font-sdf+))
            (let ((atlas (gen-image-font-atlas (font-glyphs font-sdf) (font-recs font-sdf) 95 16 0 1)))
              (setf (font-texture font-sdf) (load-texture-from-image atlas))
              (unload-image atlas))
            
            (unload-file-data file-data)  ; Free memory from loaded file
            
            ;; Load SDF required shader (we use default vertex shader)
            (let ((shader (load-shader nil (format nil "resources/shaders/glsl~a/sdf.fs" +glsl-version+))))
              (set-texture-filter (font-texture font-sdf) +texture-filter-bilinear+)  ; Required for SDF font
              
              (let ((font-position (vec2 40.0 (- (/ screen-height 2.0) 50.0)))
                    (text-size (vec2 0.0 0.0))
                    (font-size 16.0)
                    (current-font 0))  ; 0 - fontDefault, 1 - fontSDF
                
                (set-target-fps 60)
                
                ;; Main game loop
                (loop until (window-should-close) do
                  ;; Update
                  (incf font-size (* (get-mouse-wheel-move) 8.0))
                  
                  (when (< font-size 6) (setf font-size 6))
                  
                  (setf current-font (if (is-key-down +key-space+) 1 0))
                  
                  (setf text-size (if (= current-font 0)
                                     (measure-text-ex font-default msg font-size 0)
                                     (measure-text-ex font-sdf msg font-size 0)))
                  
                  (setf (vx font-position) (- (/ (get-screen-width) 2.0) (/ (vx text-size) 2.0)))
                  (setf (vy font-position) (+ (- (/ (get-screen-height) 2.0) (/ (vy text-size) 2.0)) 80.0))
                  
                  ;; Draw
                  (begin-drawing)
                    (clear-background +raywhite+)
                    
                    (if (= current-font 0)
                        (draw-text-ex font-default msg font-position font-size 0 +black+)
                        (progn
                          (begin-shader-mode shader)
                          (draw-text-ex font-sdf msg font-position font-size 0 +black+)
                          (end-shader-mode)))
                    
                    (draw-texture (font-texture font-default) 10 10 +black+)
                    (draw-texture (font-texture font-sdf) 10 (+ 10 (texture-height (font-texture font-default)) 10) +black+)
                    
                    (draw-text "FONT TYPE:" 10 (+ 10 (texture-height (font-texture font-default)) 10 (texture-height (font-texture font-sdf)) 10) 20 +black+)
                    
                    (if (= current-font 0)
                        (draw-text "DEFAULT" 30 (+ 10 (texture-height (font-texture font-default)) 10 (texture-height (font-texture font-sdf)) 10 25) 20 +lime+)
                        (draw-text "SDF" 30 (+ 10 (texture-height (font-texture font-default)) 10 (texture-height (font-texture font-sdf)) 10 25) 20 +red+))
                    
                    (draw-text "Use MOUSE WHEEL to SCALE TEXT!" 10 20 10 +red+)
                    (draw-text "HOLD SPACE to use SDF FONT version!" 10 30 10 +red+)
                    
                    (draw-text (format nil "FONT SIZE: ~,1f" font-size) 10 (- (get-screen-height) 30) 10 +darkblue+)
                    (draw-text (format nil "RENDER SIZE: ~,1f x ~,1f" (vx text-size) (vy text-size)) 10 (- (get-screen-height) 10) 10 +darkblue+)
                    
                  (end-drawing))
                
                ;; De-Initialization
                (unload-shader shader))
              
              (unload-font font-sdf))
            
            (unload-font font-default))))

    ;; Close window
    (close-window)))

;; Run the example
(main)