;;;; Advanced Text Rendering Demo for cl-raylib
;;;; This demonstrates the advanced text rendering features including atlas system

(require :cl-raylib)

(defpackage :cl-raylib-advanced-text-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-advanced-text-demo)

(defun advanced-text-demo ()
  "Demonstrate advanced text rendering features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    ;; Initialize logging system
    (set-trace-log-level +log-info+)
    (trace-log-info "Starting Advanced Text Rendering Demo")
    
    ;; Set window flags
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [advanced] - Advanced Text Rendering Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      (init-text-system)
      
      (let* (;; Demo state
             (demo-mode 0) ; 0=atlas, 1=effects, 2=alignment, 3=input
             (mode-names '("Font Atlas" "Text Effects" "Text Alignment" "Text Input"))
             
             ;; Font and text variables
             (default-font (get-font-default))
             (sample-texts '("Hello, World!" 
                            "Advanced Text Rendering"
                            "Font Atlas System"
                            "Pure Common Lisp Raylib"
                            "The quick brown fox jumps over the lazy dog"))
             (current-text-index 0)
             
             ;; Effect variables
             (shadow-offset (vec2 2 2))
             (outline-size 1)
             (gradient-progress 0.0)
             
             ;; Input system
             (text-input (create-text-input 128))
             (input-active nil)
             
             ;; Animation variables
             (time-counter 0.0)
             (pulse-scale 1.0))
        
        ;; Enable text input
        (setf (text-input-active text-input) t)
        (setf (text-input-text text-input) "Type here...")
        
        (trace-log-info "Advanced text demo initialized")
        
        (loop until (window-should-close) do
          ;; Update
          (incf time-counter 0.016)
          (setf pulse-scale (+ 1.0 (* 0.2 (sin (* time-counter 2.0)))))
          (setf gradient-progress (/ (+ 1.0 (sin time-counter)) 2.0))
          
          ;; Handle input
          (when (is-key-pressed +key-tab+)
            (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
          
          (when (is-key-pressed +key-space+)
            (setf current-text-index (mod (1+ current-text-index) (length sample-texts))))
          
          (when (is-key-pressed +key-enter+)
            (setf input-active (not input-active)))
          
          ;; Handle text input for input demo
          (when (and input-active (= demo-mode 3))
            (let ((key (get-char-pressed)))
              (when (and key (> key 0) (< key 127))
                (text-input-insert text-input (string (code-char key)))))
            
            (when (is-key-pressed +key-backspace+)
              (text-input-delete-char text-input))
            
            (when (is-key-pressed +key-left+)
              (text-input-move-cursor text-input -1))
            
            (when (is-key-pressed +key-right+)
              (text-input-move-cursor text-input 1)))
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; Draw title
            (draw-text "PURE-RAYLIB ADVANCED TEXT RENDERING DEMO" 20 20 24 +darkblue+)
            (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
            
            ;; Draw mode-specific content
            (case demo-mode
              (0 (draw-atlas-demo default-font))
              (1 (draw-effects-demo default-font (nth current-text-index sample-texts) 
                                   shadow-offset outline-size gradient-progress pulse-scale))
              (2 (draw-alignment-demo default-font (nth current-text-index sample-texts)))
              (3 (draw-input-demo text-input input-active)))
            
            ;; Draw common UI
            (draw-common-controls)
            
            ;; Draw performance info
            (let ((info-x (- screen-width 300))
                  (info-y 20))
              (draw-text "Performance:" info-x info-y 16 +darkgreen+)
              (draw-text (format nil "FPS: ~d" (get-fps)) info-x (+ info-y 25) 14 +green+)
              (draw-text (format nil "Frame Time: ~,2fms" (* (get-frame-time) 1000)) info-x (+ info-y 45) 14 +green+)
              (when default-font
                (draw-text (get-font-atlas-info default-font) info-x (+ info-y 70) 10 +gray+)))
            
            ;; Draw sample text info
            (draw-text "SPACE: Change sample text" 20 (- screen-height 100) 12 +gray+)
            (draw-text (format nil "Current: \"~a\"" (nth current-text-index sample-texts)) 20 (- screen-height 80) 12 +blue+)))
        
        ;; Cleanup
        (trace-log-info "Advanced text demo completed")
        (cleanup-text-system)
        (cleanup-texture-system))))

(defun draw-atlas-demo (font)
  "Draw font atlas demonstration"
  (let ((y-offset 100))
    (draw-text "FONT ATLAS SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Font information
    (draw-text (format nil "Font Base Size: ~d" (font-base-size font)) 20 (+ y-offset 40) 16 +black+)
    (draw-text (format nil "Glyph Count: ~d" (font-glyph-count font)) 20 (+ y-offset 60) 16 +black+)
    (draw-text (format nil "Atlas Info: ~a" (get-font-atlas-info font)) 20 (+ y-offset 80) 12 +darkgreen+)
    
    ;; Sample text at different sizes
    (draw-text "Sample Text at Different Sizes:" 20 (+ y-offset 120) 16 +darkgreen+)
    (draw-text "Size 10" 20 (+ y-offset 150) 10 +black+)
    (draw-text "Size 16" 20 (+ y-offset 170) 16 +black+)
    (draw-text "Size 24" 20 (+ y-offset 200) 24 +black+)
    (draw-text "Size 32" 20 (+ y-offset 240) 32 +black+)
    
    ;; Character samples
    (draw-text "Character Samples:" 20 (+ y-offset 290) 16 +darkgreen+)
    (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          (start-x 20)
          (start-y (+ y-offset 320)))
      (loop for i from 0 below (length chars) do
        (let ((x (+ start-x (* i 18)))
              (y start-y))
          (when (< x 1000) ; Stay within screen bounds
            (draw-text (string (char chars i)) x y 16 +blue+)))))
    
    ;; Numbers and symbols
    (let ((symbols "0123456789!@#$%^&*()_+-=[]{}|;:,.<>?")
          (start-x 20)
          (start-y (+ y-offset 350)))
      (loop for i from 0 below (length symbols) do
        (let ((x (+ start-x (* i 16)))
              (y start-y))
          (when (< x 1000) ; Stay within screen bounds
            (draw-text (string (char symbols i)) x y 14 +purple+)))))
    
    ;; Draw atlas texture (scaled down)
    (draw-text "Font Atlas Texture (scaled):" 20 (+ y-offset 390) 14 +darkgreen+)
    (debug-draw-font-atlas font (vec2 20 (+ y-offset 410)) 0.3)))

(defun draw-effects-demo (font text shadow-offset outline-size gradient-progress pulse-scale)
  "Draw text effects demonstration"
  (let ((y-offset 100))
    (draw-text "TEXT EFFECTS SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Shadow effect
    (draw-text "Drop Shadow:" 20 (+ y-offset 50) 16 +darkgreen+)
    (draw-text-with-shadow text 20 (+ y-offset 80) 20 +black+ +gray+ shadow-offset)
    
    ;; Outlined text
    (draw-text "Outlined Text:" 20 (+ y-offset 130) 16 +darkgreen+)
    (draw-text-outlined font text (vec2 20 (+ y-offset 160)) 20 1.0 outline-size +white+ +black+)
    
    ;; Gradient text (horizontal)
    (draw-text "Gradient Text (Horizontal):" 20 (+ y-offset 210) 16 +darkgreen+)
    (draw-text-gradient font text (vec2 20 (+ y-offset 240)) 20 1.0 +red+ +blue+ :horizontal)
    
    ;; Pulsing text
    (draw-text "Pulsing Text:" 20 (+ y-offset 290) 16 +darkgreen+)
    (draw-text-ex font text (vec2 20 (+ y-offset 320)) (* 20 pulse-scale) 1.0 +purple+)
    
    ;; Animated color
    (let ((animated-color (list (round (* 255 (/ (+ 1.0 (sin gradient-progress)) 2.0)))
                               (round (* 255 (/ (+ 1.0 (cos gradient-progress)) 2.0)))
                               128
                               255)))
      (draw-text "Animated Color:" 20 (+ y-offset 370) 16 +darkgreen+)
      (draw-text-ex font text (vec2 20 (+ y-offset 400)) 20 1.0 animated-color))
    
    ;; Multiple effects combined
    (draw-text "Combined Effects:" 20 (+ y-offset 450) 16 +darkgreen+)
    (draw-text-with-shadow text 22 (+ y-offset 482) (* 18 pulse-scale) +white+ +darkgray+ (vec2 2 2))
    (draw-text-outlined font text (vec2 20 (+ y-offset 480)) (* 18 pulse-scale) 1.0 1 +yellow+ +red+)))

(defun draw-alignment-demo (font text)
  "Draw text alignment demonstration"
  (let ((y-offset 100)
        (center-x 500)
        (right-x 900))
    (draw-text "TEXT ALIGNMENT SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Left aligned (default)
    (draw-text "Left Aligned:" 20 (+ y-offset 50) 16 +darkgreen+)
    (draw-line 20 (+ y-offset 80) 400 (+ y-offset 80) +lightgray+)
    (draw-text-ex font text (vec2 20 (+ y-offset 85)) 16 1.0 +black+)
    
    ;; Center aligned
    (draw-text "Center Aligned:" 20 (+ y-offset 130) 16 +darkgreen+)
    (draw-line (- center-x 200) (+ y-offset 160) (+ center-x 200) (+ y-offset 160) +lightgray+)
    (draw-circle center-x (+ y-offset 160) 3 +red+) ; Center point
    (draw-text-centered font text (vec2 center-x (+ y-offset 165)) 16 1.0 +black+)
    
    ;; Right aligned
    (draw-text "Right Aligned:" 20 (+ y-offset 210) 16 +darkgreen+)
    (draw-line (- right-x 400) (+ y-offset 240) right-x (+ y-offset 240) +lightgray+)
    (draw-circle right-x (+ y-offset 240) 3 +red+) ; Right point
    (draw-text-right-aligned font text (vec2 right-x (+ y-offset 245)) 16 1.0 +black+)
    
    ;; Word wrapping
    (draw-text "Word Wrapped Text:" 20 (+ y-offset 290) 16 +darkgreen+)
    (let* ((wrap-width 300)
           (wrap-rect (make-rectangle :x 20 :y (+ y-offset 320) :width wrap-width :height 120))
           (long-text "This is a long text that demonstrates word wrapping functionality. The text will automatically wrap to the next line when it exceeds the specified maximum width. This is very useful for creating text boxes and paragraphs."))
      (draw-rectangle-lines (rectangle-x wrap-rect) (rectangle-y wrap-rect) 
                           (rectangle-width wrap-rect) (rectangle-height wrap-rect) +gray+)
      (draw-text-wrapped font long-text (vec2 (+ (rectangle-x wrap-rect) 5) (+ (rectangle-y wrap-rect) 5)) 
                        12 1.0 (- wrap-width 10) +black+))
    
    ;; Text in box
    (draw-text "Text in Box:" 400 (+ y-offset 290) 16 +darkgreen+)
    (let* ((box-width 250)
           (box-height 100)
           (box-rect (make-rectangle :x 400 :y (+ y-offset 320) :width box-width :height box-height)))
      (draw-rectangle-rec box-rect +lightgray+)
      (draw-rectangle-lines (rectangle-x box-rect) (rectangle-y box-rect) 
                           (rectangle-width box-rect) (rectangle-height box-rect) +black+)
      (draw-text-box font "This text is contained within a bounded box area with automatic wrapping." 
                    (vec2 (+ (rectangle-x box-rect) 5) (+ (rectangle-y box-rect) 5)) 
                    12 1.0 (- box-width 10) +darkblue+))))

(defun draw-input-demo (text-input input-active)
  "Draw text input demonstration"
  (let ((y-offset 100))
    (draw-text "TEXT INPUT SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press ENTER to toggle input mode" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Input status
    (draw-text (format nil "Input Active: ~a" (if input-active "YES" "NO")) 20 (+ y-offset 60) 16 
               (if input-active +green+ +red+))
    
    ;; Text input box
    (let* ((input-rect (make-rectangle :x 20 :y (+ y-offset 100) :width 600 :height 40))
           (input-text (text-input-text text-input))
           (cursor-pos (text-input-cursor-pos text-input)))
      
      ;; Draw input box
      (draw-rectangle-rec input-rect (if input-active +white+ +lightgray+))
      (draw-rectangle-lines (rectangle-x input-rect) (rectangle-y input-rect) 
                           (rectangle-width input-rect) (rectangle-height input-rect) 
                           (if input-active +blue+ +gray+))
      
      ;; Draw text
      (draw-text input-text (+ (rectangle-x input-rect) 5) (+ (rectangle-y input-rect) 10) 16 +black+)
      
      ;; Draw cursor if active
      (when input-active
        (let* ((cursor-text (subseq input-text 0 (min cursor-pos (length input-text))))
               (cursor-x (+ (rectangle-x input-rect) 5 (first (measure-text cursor-text 16))))
               (cursor-y (+ (rectangle-y input-rect) 8)))
          (when (= (mod (floor (* (get-time) 2)) 2) 0) ; Blinking cursor
            (draw-line cursor-x cursor-y cursor-x (+ cursor-y 20) +black+)))))
    
    ;; Input information
    (draw-text "Input Information:" 20 (+ y-offset 160) 16 +darkgreen+)
    (draw-text (format nil "Text Length: ~d" (length (text-input-text text-input))) 20 (+ y-offset 185) 14 +black+)
    (draw-text (format nil "Cursor Position: ~d" (text-input-cursor-pos text-input)) 20 (+ y-offset 205) 14 +black+)
    (draw-text (format nil "Max Length: ~d" (text-input-max-length text-input)) 20 (+ y-offset 225) 14 +black+)
    
    ;; Sample text processing
    (draw-text "Text Processing:" 20 (+ y-offset 265) 16 +darkgreen+)
    (let ((sample-text (text-input-text text-input)))
      (draw-text (format nil "Uppercase: ~a" (text-to-upper sample-text)) 20 (+ y-offset 290) 12 +black+)
      (draw-text (format nil "Lowercase: ~a" (text-to-lower sample-text)) 20 (+ y-offset 310) 12 +black+)
      (draw-text (format nil "Length: ~d characters" (text-length sample-text)) 20 (+ y-offset 330) 12 +black+)
      (when (> (length sample-text) 10)
        (draw-text (format nil "First 10 chars: ~a" (text-subtext sample-text 0 10)) 20 (+ y-offset 350) 12 +black+)))
    
    ;; Instructions
    (draw-text "Instructions:" 20 (+ y-offset 390) 16 +darkgreen+)
    (draw-text "• Type to add text" 20 (+ y-offset 415) 12 +blue+)
    (draw-text "• BACKSPACE to delete" 20 (+ y-offset 430) 12 +blue+)
    (draw-text "• LEFT/RIGHT arrows to move cursor" 20 (+ y-offset 445) 12 +blue+)
    (draw-text "• ENTER to toggle input mode" 20 (+ y-offset 460) 12 +blue+)))

(defun draw-common-controls ()
  "Draw common control instructions"
  (let ((controls-y 750))
    (draw-text "Controls:" 20 controls-y 14 +darkblue+)
    (draw-text "TAB: Switch modes  SPACE: Change text  ENTER: Toggle input  ESC: Exit" 
              20 (+ controls-y 20) 12 +gray+)))

;; Run the demo
(advanced-text-demo)