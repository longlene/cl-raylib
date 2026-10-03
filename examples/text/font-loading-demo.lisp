;;;; Font Loading Demo for cl-raylib
;;;; This demonstrates basic font loading capabilities

(require :cl-raylib)

(defpackage :cl-raylib-font-loading-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-font-loading-demo)

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

;; Font loader configuration structure - based on raylib's font loading system
(defstruct font-loader-config
  base-size
  atlas-size
  padding
  filter
  antialias
  codepoint-ranges
  fallback-char
  generate-mipmaps)

(defun create-font-loader-config (&key (base-size 16) (atlas-size 512) (padding 4) 
                                       (filter :bilinear) (antialias t) 
                                       (codepoint-ranges nil) (fallback-char 63) ; '?' character
                                       (generate-mipmaps nil))
  "Create a font loader configuration based on raylib's LoadFontEx parameters"
  (make-font-loader-config :base-size base-size
                          :atlas-size atlas-size
                          :padding padding
                          :filter filter
                          :antialias antialias
                          :codepoint-ranges codepoint-ranges
                          :fallback-char fallback-char
                          :generate-mipmaps generate-mipmaps))

(defun load-font-with-config (filename config)
  "Load a font with the given configuration - simulates raylib's LoadFontEx"
  (declare (ignore config))
  ;; For now, just return the default font since we don't have full font loading
  ;; In a complete implementation, this would:
  ;; 1. Parse the font file format (TTF, OTF, FNT, etc.)
  ;; 2. Generate texture atlas based on config settings
  ;; 3. Create glyph information for specified codepoint ranges
  ;; 4. Apply filtering and antialiasing settings
  (get-font-default))

(defun validate-font-config (config)
  "Validate font configuration parameters"
  (and (> (font-loader-config-base-size config) 0)
       (> (font-loader-config-atlas-size config) 0)
       (>= (font-loader-config-padding config) 0)
       (member (font-loader-config-filter config) '(:point :bilinear :trilinear))
       (typep (font-loader-config-antialias config) 'boolean)))

(defun get-font-format-from-extension (filename)
  "Determine font format from file extension - based on raylib's file detection"
  (let ((ext (string-downcase (pathname-type (pathname filename)))))
    (cond
      ((string= ext "ttf") :truetype)
      ((string= ext "otf") :opentype)
      ((string= ext "fnt") :bitmap)
      ((member ext '("png" "bmp" "jpg" "jpeg") :test #'string=) :image)
      (t :unknown))))

(defun get-default-codepoint-ranges ()
  "Get default Unicode codepoint ranges for font loading"
  ;; Basic Latin + Latin-1 Supplement (most common characters)
  (list (cons 32 126)   ; Basic ASCII printable characters
        (cons 160 255)  ; Latin-1 Supplement
        (cons 8192 8303) ; General Punctuation
        (cons 8364 8364) ; Euro sign
        (cons 8482 8482))) ; Trademark sign

(defun font-loading-demo ()
  "Demonstrate basic font loading features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    (init-window screen-width screen-height "cl-raylib [fonts] - Font Loading Demo")
    (set-target-fps 60)
      
    (let* (;; Demo state
           (demo-mode 0) ; 0=formats, 1=loading, 2=info, 3=samples
           (mode-names '("Font Formats" "Font Loading" "Font Info" "Font Samples"))
           
           ;; Font variables
           (loaded-fonts (list (list "Default" (get-font-default))))
           (current-font-index 0)
           (sample-text "The quick brown fox jumps over the lazy dog")
           
           ;; Font loading test
           (test-font-configs nil)
           (config-index 0)
           
           ;; Animation
           (time-counter 0.0))
      
      ;; Create test configurations
      (setf test-font-configs
            (list (create-font-loader-config :base-size 12 :atlas-size 256)
                  (create-font-loader-config :base-size 16 :atlas-size 512)
                  (create-font-loader-config :base-size 24 :atlas-size 512)
                  (create-font-loader-config :base-size 32 :atlas-size 1024)))
      
      (loop until (window-should-close) do
        ;; Update
        (incf time-counter (get-frame-time))
        
        ;; Handle input
        (when (is-key-pressed +key-tab+)
          (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
        
        (when (is-key-pressed +key-space+)
          (case demo-mode
            (1 ; Font loading mode
             (setf config-index (mod (1+ config-index) (length test-font-configs))))
            (3 ; Font samples mode
             (setf current-font-index (mod (1+ current-font-index) (length loaded-fonts))))))
        
        (when (is-key-pressed +key-l+)
          (setf loaded-fonts (simulate-font-loading loaded-fonts)))
        
        (when (is-key-pressed +key-r+)
          (simulate-font-reloading loaded-fonts))
        
        (when (is-key-pressed +key-c+)
          (cleanup-font-resources loaded-fonts)
          (setf loaded-fonts (list (list "Default" (get-font-default)))))
        
        ;; Drawing
        (begin-drawing)
          (clear-background +raywhite+)
          
          ;; Draw title
          (draw-text "RAYLIB FONT LOADING DEMO" 20 20 24 +darkblue+)
          (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
          
          ;; Draw mode-specific content
          (case demo-mode
            (0 (draw-formats-demo))
            (1 (draw-loading-demo test-font-configs config-index))
            (2 (draw-info-demo))
            (3 (draw-samples-demo loaded-fonts current-font-index sample-text)))
          
          ;; Draw common controls
          (draw-common-font-controls demo-mode)
          
          ;; Draw performance info
          (let ((info-x (- screen-width 350))
                (info-y 20))
            (draw-text "System Info:" info-x info-y 16 +darkgreen+)
            (draw-text (format nil "FPS: ~d" (get-fps)) info-x (+ info-y 25) 14 +green+)
            (draw-text (format nil "Loaded Fonts: ~d" (length loaded-fonts)) info-x (+ info-y 45) 14 +green+))
        
        (end-drawing))
      
      ;; Cleanup
      (close-window))))

(defun draw-formats-demo ()
  "Draw font formats information"
  (let ((y-offset 100))
    (draw-text "FONT FORMATS & SUPPORT" 20 y-offset 20 +darkblue+)
    
    ;; Supported formats
    (draw-text "Supported Font Formats:" 20 (+ y-offset 50) 16 +darkgreen+)
    
    (let ((formats '(("FNT" "Bitmap font with atlas" "✓ Supported")
                    ("PNG/BMP/JPG" "Image-based character grid" "✓ Supported")
                    ("TTF/OTF" "TrueType/OpenType fonts" "✓ Supported")))
          (start-y (+ y-offset 80)))
      
      (loop for (ext desc status) in formats
            for i from 0 do
        (let ((y (+ start-y (* i 40))))
          (draw-text (format nil "• ~a" ext) 40 y 14 +black+)
          (draw-text desc 40 (+ y 15) 12 +gray+)
          (draw-text status 40 (+ y 28) 12 +green+))))
    
    ;; Format details
    (draw-text "Format Details:" 20 (+ y-offset 250) 16 +darkgreen+)
    
    ;; Bitmap font details
    (draw-text "Bitmap Font (.fnt):" 40 (+ y-offset 280) 14 +darkblue+)
    (let ((details '("• Text-based format with image atlas"
                    "• Precise glyph positioning and metrics"
                    "• Supports custom character sets"
                    "• Generated by tools like BMFont"))
          (start-y (+ y-offset 300)))
      (loop for detail in details
            for i from 0 do
        (draw-text detail 60 (+ start-y (* i 18)) 12 +black+)))
    
    ;; Image font details
    (draw-text "Image Font (.png/.bmp/.jpg):" 40 (+ y-offset 380) 14 +darkblue+)
    (let ((details '("• Single image with character grid"
                    "• 16x6 character layout (96 characters)"
                    "• Fixed-width character spacing"
                    "• Simple to create and use"))
          (start-y (+ y-offset 400)))
      (loop for detail in details
            for i from 0 do
        (draw-text detail 60 (+ start-y (* i 18)) 12 +black+)))
    
    ;; File detection
    (draw-text "Automatic Format Detection:" 20 (+ y-offset 480) 16 +darkgreen+)
    (draw-text "The system automatically detects font format from file extension" 40 (+ y-offset 510) 12 +black+)
    (draw-text "Example: font.fnt → Bitmap Font, font.png → Image Font" 40 (+ y-offset 525) 12 +gray+)))

(defun draw-loading-demo (configs config-index)
  "Draw font loading demonstration"
  (let ((y-offset 100))
    (draw-text "FONT LOADING SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press L to simulate loading, SPACE to change config" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Current configuration
    (let ((config (nth config-index configs)))
      (draw-text "Font Loading Configuration:" 20 (+ y-offset 70) 16 +darkgreen+)
      (draw-text (format nil "Base Size: ~d pixels" (font-loader-config-base-size config)) 40 (+ y-offset 100) 14 +black+)
      (draw-text (format nil "Atlas Size: ~dx~d" (font-loader-config-atlas-size config) (font-loader-config-atlas-size config)) 40 (+ y-offset 120) 14 +black+)
      (draw-text (format nil "Padding: ~d pixels" (font-loader-config-padding config)) 40 (+ y-offset 140) 14 +black+)
      (draw-text (format nil "Filter: ~a" (font-loader-config-filter config)) 40 (+ y-offset 160) 14 +black+)
      (draw-text (format nil "Antialias: ~a" (font-loader-config-antialias config)) 40 (+ y-offset 180) 14 +black+))
    
    ;; Loading process
    (draw-text "Font Loading Process:" 20 (+ y-offset 220) 16 +darkgreen+)
    (let ((steps '("1. Detect font format from file extension"
                  "2. Parse font metadata and glyph information"
                  "3. Load or generate font atlas texture"
                  "4. Create glyph mapping and metrics"
                  "5. Register font in system"
                  "6. Return ready-to-use font object"))
          (start-y (+ y-offset 250)))
      (loop for step in steps
            for i from 0 do
        (draw-text step 40 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Example usage
    (draw-text "Example Usage:" 20 (+ y-offset 400) 16 +darkgreen+)
    (let ((code-lines '("(let ((font (load-font \"myfont.fnt\")))"
                       "  (draw-text-ex font \"Hello!\" (vec2 100 100) 24 1.0 +black+))"
                       "  (unload-font font))"))
          (start-y (+ y-offset 430)))
      (loop for line in code-lines
            for i from 0 do
        (draw-text line 40 (+ start-y (* i 18)) 11 +darkblue+)))
    
    ;; Configuration showcase
    (draw-text "Size Comparison:" 400 (+ y-offset 70) 16 +darkgreen+)
    (loop for config in configs
          for i from 0 do
      (let ((y (+ y-offset 100 (* i 60)))
            (size (font-loader-config-base-size config))
            (current (= i config-index)))
        (draw-text (format nil "Size ~d:" size) 420 y 14 (if current +red+ +black+))
        (draw-text "Sample Text" 420 (+ y 20) size (if current +red+ +black+))))))

(defun draw-info-demo ()
  "Draw font info demonstration"
  (let ((y-offset 100))
    (draw-text "FONT INFORMATION SYSTEM" 20 y-offset 20 +darkblue+)
    
    ;; Current font info
    (let ((default-font (get-font-default)))
      (draw-text "Default Font Information:" 20 (+ y-offset 70) 16 +darkgreen+)
      (draw-text (format nil "Base Size: ~d pixels" (font-base-size default-font)) 40 (+ y-offset 100) 14 +black+)
      (draw-text (format nil "Glyph Count: ~d" (font-glyph-count default-font)) 40 (+ y-offset 120) 14 +black+)
      (draw-text (format nil "Texture ID: ~d" (texture-id (font-texture default-font))) 40 (+ y-offset 140) 14 +black+)
      (draw-text (format nil "Texture Size: ~dx~d" 
                        (texture-width (font-texture default-font))
                        (texture-height (font-texture default-font))) 40 (+ y-offset 160) 14 +black+))
    
    ;; Font features
    (draw-text "Font System Features:" 20 (+ y-offset 220) 16 +darkgreen+)
    (let ((features '("• Unicode text support"
                     "• Multiple font format support"
                     "• Glyph atlas management"
                     "• Texture filtering options"
                     "• Font scaling and spacing"
                     "• Text measurement functions"))
          (start-y (+ y-offset 250)))
      (loop for feature in features
            for i from 0 do
        (draw-text feature 40 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Font metrics
    (draw-text "Font Metrics:" 20 (+ y-offset 400) 16 +darkgreen+)
    (let ((sample-text "Sample Text"))
      (draw-text (format nil "Text Width: ~d pixels" (measure-text sample-text 16)) 40 (+ y-offset 430) 12 +black+)
      (draw-text (format nil "Font Height: ~d pixels" 16) 40 (+ y-offset 450) 12 +black+)
      (draw-text sample-text 40 (+ y-offset 480) 16 +blue+))))

(defun draw-samples-demo (loaded-fonts current-index sample-text)
  "Draw font samples demonstration"
  (let ((y-offset 100))
    (draw-text "FONT SAMPLES SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press SPACE to cycle fonts" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Current font info
    (when loaded-fonts
      (let* ((current-font-info (nth current-index loaded-fonts))
             (font-name (first current-font-info))
             (font (second current-font-info)))
        
        (draw-text "Current Font:" 20 (+ y-offset 70) 16 +darkgreen+)
        (draw-text (format nil "Name: ~a" font-name) 40 (+ y-offset 100) 14 +black+)
        (draw-text (format nil "Valid: ~a" (not (null font))) 40 (+ y-offset 120) 14 +black+)
        
        ;; Font preview
        (draw-text "Font Preview:" 20 (+ y-offset 180) 16 +darkgreen+)
        (when font
          (draw-text-ex font sample-text (vec2 40.0 (+ y-offset 210)) 16 1.0 +black+)
          (draw-text-ex font sample-text (vec2 40.0 (+ y-offset 235)) 20 1.0 +darkblue+)
          (draw-text-ex font sample-text (vec2 40.0 (+ y-offset 265)) 24 1.0 +purple+))))
    
    ;; Loaded fonts list
    (draw-text "Loaded Fonts:" 20 (+ y-offset 320) 16 +darkgreen+)
    (loop for font-info in loaded-fonts
          for i from 0 do
      (let ((y (+ y-offset 350 (* i 20)))
            (name (first font-info))
            (current (= i current-index)))
        (draw-text (format nil "~d. ~a~a" (1+ i) name (if current " ← Current" "")) 
                  40 y 12 (if current +red+ +black+))))
    
    ;; Sample text variations
    (draw-text "Text Variations:" 400 (+ y-offset 70) 16 +darkgreen+)
    (let ((variations '("UPPERCASE TEXT"
                       "lowercase text"
                       "Mixed Case Text"
                       "Numbers: 0123456789"
                       "Symbols: !@#$%^&*()"
                       "Special: áéíóú çñü"))
          (start-y (+ y-offset 100)))
      (loop for variation in variations
            for i from 0 do
        (draw-text variation 420 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Font statistics
    (draw-text "Statistics:" 400 (+ y-offset 250) 16 +darkgreen+)
    (let ((stats (get-font-loading-stats)))
      (draw-text (format nil "Total Fonts: ~d" (length loaded-fonts)) 420 (+ y-offset 280) 12 +black+)
      (draw-text (format nil "Total Glyphs: ~d" (getf stats :total-glyphs-cached)) 420 (+ y-offset 300) 12 +black+)
      (draw-text (format nil "Atlas Memory: ~d bytes" (getf stats :atlas-memory-used)) 420 (+ y-offset 320) 12 +black+)
      (draw-text (format nil "Default Font: ~a" (not (null (get-font-default)))) 420 (+ y-offset 340) 12 +black+))))

(defun draw-common-font-controls (demo-mode)
  "Draw common control instructions"
  (let ((controls-y 750))
    (draw-text "Controls:" 20 controls-y 14 +darkblue+)
    (let ((controls (case demo-mode
                     (1 "TAB: Switch modes  SPACE: Change config  L: Load font  R: Reload  C: Clear")
                     (2 "TAB: Switch modes  C: Clear cache  R: Reload fonts")
                     (3 "TAB: Switch modes  SPACE: Cycle fonts  R: Reload  C: Clear")
                     (t "TAB: Switch modes  L: Load font  R: Reload  C: Clear"))))
      (draw-text controls 20 (+ controls-y 20) 12 +gray+))))

(defun simulate-font-loading (loaded-fonts)
  "Simulate font loading operation based on raylib's LoadFont/LoadFontEx"
  (let* ((font-name (format nil "TestFont~d" (length loaded-fonts)))
         (base-size (+ 12 (* 4 (mod (length loaded-fonts) 6))))
         (config (create-font-loader-config :base-size base-size
                                           :atlas-size (if (> base-size 20) 1024 512)
                                           :padding (if (> base-size 24) 6 4)
                                           :antialias (> base-size 16)
                                           :codepoint-ranges (get-default-codepoint-ranges))))
    (trace-log-info "Simulating font load: ~a (size: ~d)" font-name base-size)
    
    ;; Validate configuration
    (unless (validate-font-config config)
      (trace-log-warning "Invalid font configuration for ~a" font-name)
      (return-from simulate-font-loading loaded-fonts))
    
    ;; Simulate loading process
    (trace-log-info "  1. Detecting font format...")
    (trace-log-info "  2. Parsing font metadata...")
    (trace-log-info "  3. Generating ~dx~d atlas texture..." 
                   (font-loader-config-atlas-size config)
                   (font-loader-config-atlas-size config))
    (trace-log-info "  4. Rasterizing glyphs...")
    (trace-log-info "  5. Creating glyph mappings...")
    
    ;; In a real scenario, this would load from a file using the config
    ;; For demo purposes, we'll use the default font
    (let ((loaded-font (load-font-with-config font-name config)))
      (push (list font-name loaded-font) loaded-fonts)
      (trace-log-info "Font loaded successfully: ~a" font-name))
    
    loaded-fonts))

(defun simulate-font-reloading (loaded-fonts)
  "Simulate font reloading operation with caching support"
  (when loaded-fonts
    (let* ((font-info (first loaded-fonts))
           (font-name (first font-info)))
      (trace-log-info "Simulating font reload: ~a" font-name)
      
      ;; Simulate reloading process
      (trace-log-info "  1. Checking font file timestamp...")
      (trace-log-info "  2. Clearing cached glyphs...")
      (trace-log-info "  3. Reloading font data...")
      (trace-log-info "  4. Regenerating atlas texture...")
      (trace-log-info "  5. Updating glyph mappings...")
      
      ;; In a real scenario, this would reload from file and update the font
      (trace-log-info "Font reloaded successfully: ~a" font-name))))

(defun get-font-loading-stats ()
  "Get font loading statistics - useful for debugging"
  (list :total-fonts-loaded 1  ; Default font
        :total-glyphs-cached (font-glyph-count (get-font-default))
        :atlas-memory-used (* 128 128 2) ; Default font atlas size
        :average-load-time-ms 0.0))

(defun cleanup-font-resources (loaded-fonts)
  "Clean up font resources - simulates raylib's UnloadFont"
  (loop for font-info in loaded-fonts do
    (let ((font-name (first font-info))
          (font (second font-info)))
      (trace-log-info "Cleaning up font resources: ~a" font-name)
      ;; In a real implementation, this would:
      ;; 1. Free texture atlas memory
      ;; 2. Free glyph data arrays
      ;; 3. Free font structure
      (declare (ignore font))))
  (trace-log-info "All font resources cleaned up"))

;; Run the demo
(font-loading-demo)
