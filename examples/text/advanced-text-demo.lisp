;;;; Advanced Text Rendering Demo for cl-raylib
;;;; This demonstrates basic text rendering features

(require :cl-raylib)

(defpackage :cl-raylib-advanced-text-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-advanced-text-demo)

;; Trace logging functions for debugging and demonstration
(defun trace-log-info (format-string &rest args)
  "Log information message - simulates raylib's TraceLog(LOG_INFO, ...)"
  (format t "[INFO] ~a~%" (apply #'format nil format-string args)))

(defun trace-log-warning (format-string &rest args)
  "Log warning message - simulates raylib's TraceLog(LOG_WARNING, ...)"
  (format t "[WARNING] ~a~%" (apply #'format nil format-string args)))

(defun trace-log-error (format-string &rest args)
  "Log error message - simulates raylib's TraceLog(LOG_ERROR, ...)"
  (format t "[ERROR] ~a~%" (apply #'format nil format-string args)))

(defun trace-log-debug (format-string &rest args)
  "Log debug message - simulates raylib's TraceLog(LOG_DEBUG, ...)"
  (format t "[DEBUG] ~a~%" (apply #'format nil format-string args)))

;; Advanced text input structure - based on raylib's text input implementation
(defstruct text-input
  text
  cursor-pos
  max-length
  active
  selection-start
  selection-end
  multiline
  focused)

(defun create-text-input (max-length &key (multiline nil) (initial-text ""))
  "Create a text input with advanced features based on raylib implementation"
  (make-text-input :text initial-text
                   :cursor-pos (length initial-text)
                   :max-length max-length
                   :active nil
                   :selection-start 0
                   :selection-end 0
                   :multiline multiline
                   :focused nil))

(defun text-input-process-char (input char-code)
  "Process a single character input - based on raylib's character processing"
  (when (and (text-input-active input)
             (< (length (text-input-text input)) (text-input-max-length input))
             (>= char-code 32)  ; Only printable characters
             (<= char-code 126)) ; Standard ASCII range
    (let* ((text (text-input-text input))
           (pos (text-input-cursor-pos input))
           (char-str (string (code-char char-code)))
           (new-text (concatenate 'string
                                 (subseq text 0 pos)
                                 char-str
                                 (subseq text pos))))
      (setf (text-input-text input) new-text)
      (incf (text-input-cursor-pos input)))))

(defun text-input-process-chars (input)
  "Process multiple characters from input queue - based on raylib's GetCharPressed loop"
  (when (text-input-active input)
    (loop for char-code = (get-char-pressed)
          while (> char-code 0)
          do (text-input-process-char input char-code))))

(defun text-input-insert (input char-string)
  "Insert string at cursor position - legacy function for compatibility"
  (when (and (text-input-active input)
             (< (+ (length (text-input-text input)) (length char-string)) (text-input-max-length input)))
    (let* ((text (text-input-text input))
           (pos (text-input-cursor-pos input))
           (new-text (concatenate 'string
                                 (subseq text 0 pos)
                                 char-string
                                 (subseq text pos))))
      (setf (text-input-text input) new-text)
      (incf (text-input-cursor-pos input) (length char-string)))))

(defun text-input-delete-char (input)
  "Delete character before cursor"
  (when (and (text-input-active input)
             (> (text-input-cursor-pos input) 0))
    (let* ((text (text-input-text input))
           (pos (text-input-cursor-pos input))
           (new-text (concatenate 'string
                                 (subseq text 0 (1- pos))
                                 (subseq text pos))))
      (setf (text-input-text input) new-text)
      (decf (text-input-cursor-pos input)))))

(defun text-input-move-cursor (input direction)
  "Move cursor left (-1) or right (1)"
  (let ((new-pos (+ (text-input-cursor-pos input) direction)))
    (setf (text-input-cursor-pos input)
          (max 0 (min new-pos (length (text-input-text input)))))))

(defun text-input-select-all (input)
  "Select all text in the input"
  (setf (text-input-selection-start input) 0)
  (setf (text-input-selection-end input) (length (text-input-text input))))

(defun text-input-clear-selection (input)
  "Clear text selection"
  (setf (text-input-selection-start input) 0)
  (setf (text-input-selection-end input) 0))

(defun text-input-has-selection (input)
  "Check if text input has active selection"
  (not (= (text-input-selection-start input) (text-input-selection-end input))))

(defun text-input-get-selected-text (input)
  "Get the currently selected text"
  (if (text-input-has-selection input)
      (let ((start (min (text-input-selection-start input) (text-input-selection-end input)))
            (end (max (text-input-selection-start input) (text-input-selection-end input))))
        (subseq (text-input-text input) start end))
      ""))

(defun text-input-delete-selection (input)
  "Delete the currently selected text"
  (when (text-input-has-selection input)
    (let* ((start (min (text-input-selection-start input) (text-input-selection-end input)))
           (end (max (text-input-selection-start input) (text-input-selection-end input)))
           (text (text-input-text input))
           (new-text (concatenate 'string
                                 (subseq text 0 start)
                                 (subseq text end))))
      (setf (text-input-text input) new-text)
      (setf (text-input-cursor-pos input) start)
      (text-input-clear-selection input))))

(defun text-input-word-boundaries (input)
  "Find word boundaries in the text - useful for Ctrl+Left/Right navigation"
  (let ((text (text-input-text input))
        (boundaries '()))
    (loop for i from 0 below (length text) do
      (when (and (> i 0)
                 (or (char= (char text (1- i)) #\Space)
                     (char= (char text (1- i)) #\Tab)
                     (char= (char text (1- i)) #\Newline))
                 (not (or (char= (char text i) #\Space)
                          (char= (char text i) #\Tab)
                          (char= (char text i) #\Newline))))
        (push i boundaries)))
    (nreverse (cons 0 (cons (length text) boundaries)))))

(defun advanced-text-demo ()
  "Demonstrate basic text rendering features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    (init-window screen-width screen-height "cl-raylib [advanced] - Text Rendering Demo")
    (set-target-fps 60)
      
    (let* (;; Demo state
           (demo-mode 0) ; 0=basic, 1=effects, 2=alignment, 3=input
           (mode-names '("Basic Text" "Text Effects" "Text Alignment" "Text Input"))
           
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
      
      (loop until (window-should-close) do
        ;; Update
        (incf time-counter (get-frame-time))
        (setf pulse-scale (+ 1.0 (* 0.2 (sin (* time-counter 2.0)))))
        (setf gradient-progress (/ (+ 1.0 (sin time-counter)) 2.0))
        
        ;; Handle input
        (when (is-key-pressed +key-tab+)
          (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
        
        (when (is-key-pressed +key-space+)
          (setf current-text-index (mod (1+ current-text-index) (length sample-texts))))
        
        (when (is-key-pressed +key-enter+)
          (setf input-active (not input-active)))
        
        ;; Handle text input for input demo - enhanced raylib-style processing
        (when (and input-active (= demo-mode 3))
          ;; Process all characters in the input queue
          (text-input-process-chars text-input)
          
          ;; Handle special keys
          (when (is-key-pressed +key-backspace+)
            (text-input-delete-char text-input))
          
          (when (is-key-pressed +key-left+)
            (text-input-move-cursor text-input -1))
          
          (when (is-key-pressed +key-right+)
            (text-input-move-cursor text-input 1))
          
          ;; Handle Ctrl+A (select all)
          (when (and (is-key-down +key-left-control+) (is-key-pressed +key-a+))
            (setf (text-input-selection-start text-input) 0)
            (setf (text-input-selection-end text-input) (length (text-input-text text-input))))
          
          ;; Handle Home/End keys
          (when (is-key-pressed cl-raylib:+key-home+)
            (setf (text-input-cursor-pos text-input) 0)
            (text-input-clear-selection text-input))
          
          (when (is-key-pressed cl-raylib:+key-end+)
            (setf (text-input-cursor-pos text-input) (length (text-input-text text-input)))
            (text-input-clear-selection text-input))
          
          ;; Handle Delete key
          (when (is-key-pressed cl-raylib:+key-delete+)
            (if (text-input-has-selection text-input)
                (text-input-delete-selection text-input)
                (when (< (text-input-cursor-pos text-input) (length (text-input-text text-input)))
                  (let* ((text (text-input-text text-input))
                         (pos (text-input-cursor-pos text-input))
                         (new-text (concatenate 'string
                                               (subseq text 0 pos)
                                               (subseq text (1+ pos)))))
                    (setf (text-input-text text-input) new-text)))))
          
          ;; Handle Ctrl+V (paste) - simplified version
          (when (and (is-key-down +key-left-control+) (is-key-pressed +key-v+))
            (text-input-insert text-input "[Pasted Text]"))
          
          ;; Handle Ctrl+X (cut) - simplified version
          (when (and (is-key-down +key-left-control+) (is-key-pressed +key-x+))
            (when (text-input-has-selection text-input)
              (text-input-delete-selection text-input)))
          
          ;; Handle Ctrl+C (copy) - simplified version
          (when (and (is-key-down +key-left-control+) (is-key-pressed +key-c+))
            (when (text-input-has-selection text-input)
              (trace-log-info "Copied: ~a" (text-input-get-selected-text text-input)))))
        
        ;; Drawing
        (begin-drawing)
          (clear-background +raywhite+)
          
          ;; Draw title
          (draw-text "PURE-RAYLIB TEXT RENDERING DEMO" 20 20 24 +darkblue+)
          (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
          
          ;; Draw mode-specific content
          (case demo-mode
            (0 (draw-basic-demo default-font))
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
            (draw-text (format nil "Frame Time: ~,2fms" (* (get-frame-time) 1000)) info-x (+ info-y 45) 14 +green+))
          
          ;; Draw sample text info
          (draw-text "SPACE: Change sample text" 20 (- screen-height 100) 12 +gray+)
          (draw-text (format nil "Current: \"~a\"" (nth current-text-index sample-texts)) 20 (- screen-height 80) 12 +blue+)
        
        (end-drawing))
      
      ;; Cleanup
      (close-window))))

(defun draw-basic-demo (font)
  "Draw basic text demonstration"
  (let ((y-offset 100))
    (draw-text "BASIC TEXT SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Font information
    (draw-text (format nil "Font Base Size: ~d" (font-base-size font)) 20 (+ y-offset 40) 16 +black+)
    (draw-text (format nil "Glyph Count: ~d" (font-glyph-count font)) 20 (+ y-offset 60) 16 +black+)
    
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
      (loop for i from 0 below (min (length chars) 40) do
        (let ((x (+ start-x (* i 18)))
              (y start-y))
          (when (< x 1000) ; Stay within screen bounds
            (draw-text (string (char chars i)) x y 16 +blue+)))))
    
    ;; Numbers and symbols
    (let ((symbols "0123456789!@#$%^&*()_+-=[]{}|;:,.<>?")
          (start-x 20)
          (start-y (+ y-offset 350)))
      (loop for i from 0 below (min (length symbols) 30) do
        (let ((x (+ start-x (* i 16)))
              (y start-y))
          (when (< x 1000) ; Stay within screen bounds
            (draw-text (string (char symbols i)) x y 14 +purple+)))))
    
    ;; Draw font texture info
    (draw-text "Font Texture Information:" 20 (+ y-offset 390) 14 +darkgreen+)
    (draw-text (format nil "Texture ID: ~d" (texture-id (font-texture font))) 20 (+ y-offset 410) 12 +black+)
    (draw-text (format nil "Texture Size: ~dx~d" 
                      (texture-width (font-texture font))
                      (texture-height (font-texture font))) 20 (+ y-offset 430) 12 +black+)))

(defun draw-effects-demo (font text shadow-offset outline-size gradient-progress pulse-scale)
  "Draw text effects demonstration"
  (declare (ignore outline-size))
  (let ((y-offset 100))
    (draw-text "TEXT EFFECTS SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Shadow effect (simple)
    (draw-text "Drop Shadow (Simple):" 20 (+ y-offset 50) 16 +darkgreen+)
    (draw-text text (+ 20 (truncate (vx shadow-offset))) (+ y-offset 80 (truncate (vy shadow-offset))) 20 +gray+)
    (draw-text text 20 (+ y-offset 80) 20 +black+)
    
    ;; Pulsing text
    (draw-text "Pulsing Text:" 20 (+ y-offset 130) 16 +darkgreen+)
    (draw-text-ex font text (vec2 20.0 (+ y-offset 160)) (* 20 pulse-scale) 1.0 +purple+)
    
    ;; Animated color
    (let ((red-val (truncate (* 255 (/ (+ 1.0 (sin gradient-progress)) 2.0))))
          (green-val (truncate (* 255 (/ (+ 1.0 (cos gradient-progress)) 2.0))))
          (blue-val 128)
          (alpha-val 255))
      (draw-text "Animated Color:" 20 (+ y-offset 210) 16 +darkgreen+)
      (draw-text-ex font text (vec2 20.0 (+ y-offset 240)) 20 1.0 
                    (make-color red-val green-val blue-val alpha-val)))
    
    ;; Different sizes
    (draw-text "Different Sizes:" 20 (+ y-offset 290) 16 +darkgreen+)
    (draw-text text 20 (+ y-offset 320) 12 +blue+)
    (draw-text text 20 (+ y-offset 340) 16 +green+)
    (draw-text text 20 (+ y-offset 365) 20 +red+)
    (draw-text text 20 (+ y-offset 395) 24 +orange+)
    
    ;; Multiple colors
    (draw-text "Multiple Colors:" 20 (+ y-offset 430) 16 +darkgreen+)
    (let ((colors (list +red+ +orange+ +yellow+ +green+ +blue+ +purple+)))
      (loop for i from 0 below (min (length text) (length colors)) do
        (let ((char (string (char text i)))
              (color (nth i colors))
              (x-pos (+ 20 (* i 15))))
          (draw-text char x-pos (+ y-offset 460) 20 color))))))

(defun draw-alignment-demo (font text)
  "Draw text alignment demonstration"
  (let ((y-offset 100)
        (center-x 500)
        (right-x 900))
    (draw-text "TEXT ALIGNMENT SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Left aligned (default)
    (draw-text "Left Aligned:" 20 (+ y-offset 50) 16 +darkgreen+)
    (draw-line 20 (+ y-offset 80) 400 (+ y-offset 80) +lightgray+)
    (draw-text-ex font text (vec2 20.0 (+ y-offset 85)) 16 1.0 +black+)
    
    ;; Center aligned (manual calculation)
    (draw-text "Center Aligned:" 20 (+ y-offset 130) 16 +darkgreen+)
    (draw-line (- center-x 200) (+ y-offset 160) (+ center-x 200) (+ y-offset 160) +lightgray+)
    (draw-circle center-x (+ y-offset 160) 3 +red+) ; Center point
    (let* ((text-size (measure-text-ex font text 16 1.0))
           (text-x (- center-x (/ (vx text-size) 2))))
      (draw-text-ex font text (vec2 text-x (+ y-offset 165)) 16 1.0 +black+))
    
    ;; Right aligned (manual calculation)
    (draw-text "Right Aligned:" 20 (+ y-offset 210) 16 +darkgreen+)
    (draw-line (- right-x 400) (+ y-offset 240) right-x (+ y-offset 240) +lightgray+)
    (draw-circle right-x (+ y-offset 240) 3 +red+) ; Right point
    (let* ((text-size (measure-text-ex font text 16 1.0))
           (text-x (- right-x (vx text-size))))
      (draw-text-ex font text (vec2 text-x (+ y-offset 245)) 16 1.0 +black+))
    
    ;; Simple text wrapping simulation
    (draw-text "Text in Boxes:" 20 (+ y-offset 290) 16 +darkgreen+)
    (let* ((box-width 300.0)
           (box-height 60.0)
           (box-rect (make-rectangle :x 20.0 :y (+ y-offset 320) :width box-width :height box-height))
           (wrapped-text1 "This is the first line of text.")
           (wrapped-text2 "This is the second line."))
      (draw-rectangle-rec box-rect +lightgray+)
      (draw-rectangle-lines (rectangle-x box-rect) (rectangle-y box-rect) 
                           (rectangle-width box-rect) (rectangle-height box-rect) +black+)
      (draw-text wrapped-text1 (+ (rectangle-x box-rect) 5) (+ (rectangle-y box-rect) 5) 12 +darkblue+)
      (draw-text wrapped-text2 (+ (rectangle-x box-rect) 5) (+ (rectangle-y box-rect) 20) 12 +darkblue+))
    
    ;; Multiple text samples
    (draw-text "Multiple Text Samples:" 20 (+ y-offset 400) 16 +darkgreen+)
    (let ((sample-texts '("Short text" "Medium length text here" "This is a longer text sample")))
      (loop for i from 0 below (length sample-texts) do
        (let ((sample (nth i sample-texts))
              (y-pos (+ y-offset 430 (* i 20))))
          (draw-text sample 20 y-pos 14 (nth i (list +blue+ +green+ +purple+))))))))

(defun draw-input-demo (text-input input-active)
  "Draw text input demonstration"
  (let ((y-offset 100))
    (draw-text "TEXT INPUT SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press ENTER to toggle input mode" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Input status
    (draw-text (format nil "Input Active: ~a" (if input-active "YES" "NO")) 20 (+ y-offset 60) 16 
               (if input-active +green+ +red+))
    
    ;; Text input box
    (let* ((input-rect (make-rectangle :x 20.0 :y (+ y-offset 100) :width 600.0 :height 40.0))
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
               (cursor-x (+ (rectangle-x input-rect) 5 (measure-text cursor-text 16)))
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
      (draw-text (format nil "Uppercase: ~a" (string-upcase sample-text)) 20 (+ y-offset 290) 12 +black+)
      (draw-text (format nil "Lowercase: ~a" (string-downcase sample-text)) 20 (+ y-offset 310) 12 +black+)
      (draw-text (format nil "Length: ~d characters" (length sample-text)) 20 (+ y-offset 330) 12 +black+)
      (when (> (length sample-text) 10)
        (draw-text (format nil "First 10 chars: ~a" (subseq sample-text 0 10)) 20 (+ y-offset 350) 12 +black+)))
    
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

(defun main ()
  "Main entry point"
  (advanced-text-demo))

;; Run the demo
(main)
