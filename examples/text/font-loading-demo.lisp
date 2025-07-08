;;;; Font Loading Demo for cl-raylib
;;;; This demonstrates the font loading and caching system

(require :cl-raylib)

(defpackage :cl-raylib-font-loading-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-font-loading-demo)

(defun font-loading-demo ()
  "Demonstrate font loading and management features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    ;; Initialize logging system
    (set-trace-log-level +log-info+)
    (trace-log-info "Starting Font Loading Demo")
    
    ;; Set window flags
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [fonts] - Font Loading & Management Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      (init-text-system)
      
      (let* (;; Demo state
             (demo-mode 0) ; 0=formats, 1=loading, 2=cache, 3=management
             (mode-names '("Font Formats" "Font Loading" "Font Cache" "Font Management"))
             
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
        
        (trace-log-info "Font loading demo initialized")
        
        (loop until (window-should-close) do
          ;; Update
          (incf time-counter 0.016)
          
          ;; Handle input
          (when (is-key-pressed +key-tab+)
            (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
          
          (when (is-key-pressed +key-space+)
            (case demo-mode
              (1 ; Font loading mode
               (setf config-index (mod (1+ config-index) (length test-font-configs))))
              (3 ; Font management mode
               (setf current-font-index (mod (1+ current-font-index) (length loaded-fonts))))))
          
          (when (is-key-pressed +key-l+)
            (simulate-font-loading loaded-fonts))
          
          (when (is-key-pressed +key-c+)
            (clear-font-cache)
            (trace-log-info "Font cache cleared"))
          
          (when (is-key-pressed +key-r+)
            (simulate-font-reloading loaded-fonts))
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; Draw title
            (draw-text "PURE-RAYLIB FONT LOADING & MANAGEMENT DEMO" 20 20 24 +darkblue+)
            (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
            
            ;; Draw mode-specific content
            (case demo-mode
              (0 (draw-formats-demo))
              (1 (draw-loading-demo test-font-configs config-index))
              (2 (draw-cache-demo))
              (3 (draw-management-demo loaded-fonts current-font-index sample-text)))
            
            ;; Draw common controls
            (draw-common-font-controls demo-mode)
            
            ;; Draw performance info
            (let ((info-x (- screen-width 350))
                  (info-y 20))
              (draw-text "System Info:" info-x info-y 16 +darkgreen+)
              (draw-text (format nil "FPS: ~d" (get-fps)) info-x (+ info-y 25) 14 +green+)
              (draw-text (format nil "Loaded Fonts: ~d" (length loaded-fonts)) info-x (+ info-y 45) 14 +green+)
              (draw-text (get-font-cache-info) info-x (+ info-y 65) 12 +gray+)
              (draw-text (get-font-format-info) info-x (+ info-y 85) 10 +gray+)))
        
        ;; Cleanup
        (trace-log-info "Font loading demo completed")
        (cleanup-text-system)
        (cleanup-texture-system))))

(defun draw-formats-demo ()
  "Draw font formats information"
  (let ((y-offset 100))
    (draw-text "FONT FORMATS & SUPPORT" 20 y-offset 20 +darkblue+)
    
    ;; Supported formats
    (draw-text "Supported Font Formats:" 20 (+ y-offset 50) 16 +darkgreen+)
    
    (let ((formats '((:bitmap-font "FNT" "Bitmap font with atlas" "✓ Supported")
                    (:image-font "PNG/BMP/JPG" "Image-based character grid" "✓ Supported")
                    (:truetype "TTF/OTF" "Vector-based fonts" "⚠ Planned")))
          (start-y (+ y-offset 80)))
      
      (loop for (format ext desc status) in formats
            for i from 0 do
        (let ((y (+ start-y (* i 40)))
              (status-color (if (is-font-format-supported format) +green+ +orange+)))
          (draw-text (format nil "• ~a (~a)" (string-capitalize (string format)) ext) 40 y 14 +black+)
          (draw-text desc 40 (+ y 15) 12 +gray+)
          (draw-text status 40 (+ y 28) 12 status-color))))
    
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
                  "5. Register font in system cache"
                  "6. Return ready-to-use font object"))
          (start-y (+ y-offset 250)))
      (loop for step in steps
            for i from 0 do
        (draw-text step 40 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Example usage
    (draw-text "Example Usage:" 20 (+ y-offset 400) 16 +darkgreen+)
    (let ((code-lines '("(let ((font (load-font \"myfont.fnt\")))"
                       "      (config (create-font-loader-config :base-size 24)))"
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

(defun draw-cache-demo ()
  "Draw font cache demonstration"
  (let ((y-offset 100))
    (draw-text "FONT CACHE SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press C to clear cache" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Cache statistics
    (draw-text "Cache Statistics:" 20 (+ y-offset 70) 16 +darkgreen+)
    (draw-text (get-font-cache-info) 40 (+ y-offset 100) 14 +black+)
    
    ;; Cache mechanism
    (draw-text "Caching Mechanism:" 20 (+ y-offset 140) 16 +darkgreen+)
    (let ((features '("• Automatic caching of loaded fonts"
                     "• LRU (Least Recently Used) eviction policy"
                     "• Reference counting for memory management"
                     "• Configurable cache size limit"
                     "• Cache key based on filename and configuration"
                     "• Fast lookup for frequently used fonts"))
          (start-y (+ y-offset 170)))
      (loop for feature in features
            for i from 0 do
        (draw-text feature 40 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Cache benefits
    (draw-text "Benefits:" 20 (+ y-offset 320) 16 +darkgreen+)
    (let ((benefits '("✓ Reduced memory usage through sharing"
                     "✓ Faster font access for repeated loads"
                     "✓ Automatic cleanup of unused fonts"
                     "✓ Better performance in font-heavy applications"))
          (start-y (+ y-offset 350)))
      (loop for benefit in benefits
            for i from 0 do
        (draw-text benefit 40 (+ start-y (* i 25)) 12 +darkgreen+)))
    
    ;; Cache visualization
    (draw-text "Cache Visualization:" 400 (+ y-offset 70) 16 +darkgreen+)
    (let ((cache-rect (make-rectangle :x 420 :y (+ y-offset 100) :width 300 :height 200)))
      (draw-rectangle-rec cache-rect +lightgray+)
      (draw-rectangle-lines (rectangle-x cache-rect) (rectangle-y cache-rect) 
                           (rectangle-width cache-rect) (rectangle-height cache-rect) +black+)
      (draw-text "Font Cache" (+ (rectangle-x cache-rect) 10) (+ (rectangle-y cache-rect) 10) 14 +black+)
      
      ;; Simulate cache entries
      (loop for i from 0 below 6 do
        (let ((entry-y (+ (rectangle-y cache-rect) 40 (* i 25))))
          (when (< entry-y (+ (rectangle-y cache-rect) (rectangle-height cache-rect) -10))
            (draw-text (format nil "Font ~d: default.fnt (~d refs)" (1+ i) (1+ (mod i 3))) 
                      (+ (rectangle-x cache-rect) 10) entry-y 10 +darkblue+)))))))

(defun draw-management-demo (loaded-fonts current-index sample-text)
  "Draw font management demonstration"
  (let ((y-offset 100))
    (draw-text "FONT MANAGEMENT SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "Press SPACE to cycle fonts, R to reload" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Current font info
    (when loaded-fonts
      (let* ((current-font-info (nth current-index loaded-fonts))
             (font-name (first current-font-info))
             (font (second current-font-info)))
        
        (draw-text "Current Font:" 20 (+ y-offset 70) 16 +darkgreen+)
        (draw-text (format nil "Name: ~a" font-name) 40 (+ y-offset 100) 14 +black+)
        (draw-text (format nil "Valid: ~a" (validate-font font)) 40 (+ y-offset 120) 14 +black+)
        (when font
          (draw-text (get-font-info font) 40 (+ y-offset 140) 12 +gray+))
        
        ;; Font preview
        (draw-text "Font Preview:" 20 (+ y-offset 180) 16 +darkgreen+)
        (when (validate-font font)
          (draw-text-ex font sample-text (vec2 40 (+ y-offset 210)) 16 1.0 +black+)
          (draw-text-ex font sample-text (vec2 40 (+ y-offset 235)) 20 1.0 +darkblue+)
          (draw-text-ex font sample-text (vec2 40 (+ y-offset 265)) 24 1.0 +purple+))))
    
    ;; Loaded fonts list
    (draw-text "Loaded Fonts:" 20 (+ y-offset 320) 16 +darkgreen+)
    (loop for font-info in loaded-fonts
          for i from 0 do
      (let ((y (+ y-offset 350 (* i 20)))
            (name (first font-info))
            (current (= i current-index)))
        (draw-text (format nil "~d. ~a~a" (1+ i) name (if current " ← Current" "")) 
                  40 y 12 (if current +red+ +black+))))
    
    ;; Management operations
    (draw-text "Management Operations:" 400 (+ y-offset 70) 16 +darkgreen+)
    (let ((operations '("• Font validation and integrity checking"
                       "• Font information and metadata display"
                       "• Font reloading and hot-swapping"
                       "• Memory usage monitoring"
                       "• Font registry and lookup"
                       "• Automatic cleanup and garbage collection"))
          (start-y (+ y-offset 100)))
      (loop for op in operations
            for i from 0 do
        (draw-text op 420 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Font statistics
    (draw-text "System Statistics:" 400 (+ y-offset 250) 16 +darkgreen+)
    (draw-text (format nil "Total Fonts Loaded: ~d" (length loaded-fonts)) 420 (+ y-offset 280) 12 +black+)
    (draw-text (format nil "Default Font Active: ~a" (not (null (get-font-default)))) 420 (+ y-offset 300) 12 +black+)
    (draw-text (format nil "Font Registry Size: ~d" (length (get-loaded-fonts))) 420 (+ y-offset 320) 12 +black+)))

(defun draw-common-font-controls (demo-mode)
  "Draw common control instructions"
  (let ((controls-y 750))
    (draw-text "Controls:" 20 controls-y 14 +darkblue+)
    (let ((controls (case demo-mode
                     (1 "TAB: Switch modes  SPACE: Change config  L: Load font")
                     (2 "TAB: Switch modes  C: Clear cache")
                     (3 "TAB: Switch modes  SPACE: Cycle fonts  R: Reload")
                     (t "TAB: Switch modes"))))
      (draw-text controls 20 (+ controls-y 20) 12 +gray+))))

(defun simulate-font-loading (loaded-fonts)
  "Simulate font loading operation"
  (let ((font-name (format nil "TestFont~d" (length loaded-fonts)))
        (config (create-font-loader-config :base-size (+ 12 (* 4 (mod (length loaded-fonts) 6))))))
    (trace-log-info "Simulating font load: ~a" font-name)
    ;; In a real scenario, this would load from a file
    ;; For demo purposes, we'll use the default font
    (push (list font-name (get-font-default)) loaded-fonts)
    (trace-log-info "Font loaded successfully: ~a" font-name)))

(defun simulate-font-reloading (loaded-fonts)
  "Simulate font reloading operation"
  (when loaded-fonts
    (let* ((font-info (first loaded-fonts))
           (font-name (first font-info)))
      (trace-log-info "Simulating font reload: ~a" font-name)
      ;; In a real scenario, this would reload from file
      (trace-log-info "Font reloaded successfully: ~a" font-name))))

;; Run the demo
(font-loading-demo)