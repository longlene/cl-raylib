;;;; Text Performance Optimization Demo
;;;; Comprehensive demonstration of text rendering performance optimizations

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun demo-text-performance-stats ()
  "Demonstrate text performance statistics tracking"
  (format t "~%=== Text Performance Statistics Demo ===~%")
  
  ;; Reset stats for clean demonstration
  (reset-text-perf-stats)
  
  (format t "Initial performance statistics:~%")
  (let ((stats (get-text-perf-stats)))
    (format t "- Draw calls: ~d~%" (text-perf-stats-draw-calls stats))
    (format t "- Characters rendered: ~d~%" (text-perf-stats-characters-rendered stats))
    (format t "- Cache hits: ~d~%" (text-perf-stats-cache-hits stats))
    (format t "- Cache misses: ~d~%" (text-perf-stats-cache-misses stats)))
  
  ;; Simulate some text rendering operations
  (format t "~%Simulating text rendering operations...~%")
  (dotimes (i 5)
    (let ((text (format nil "Test text ~d" i)))
      (draw-text-optimized (get-font-default) text (vec2 10.0 (* i 20.0)) 16 1.0 +black+)))
  
  (format t "~%Final performance statistics:~%")
  (let ((stats (get-text-perf-stats)))
    (format t "- Draw calls: ~d~%" (text-perf-stats-draw-calls stats))
    (format t "- Characters rendered: ~d~%" (text-perf-stats-characters-rendered stats))
    (format t "- Batches used: ~d~%" (text-perf-stats-batches-used stats))
    (format t "- Cache hits: ~d~%" (text-perf-stats-cache-hits stats))
    (format t "- Cache misses: ~d~%" (text-perf-stats-cache-misses stats))
    (format t "- Total render time: ~,2f ms~%" (text-perf-stats-render-time stats)))
  
  (format t "Performance statistics demo completed.~%"))

(defun demo-text-batching ()
  "Demonstrate text batching system"
  (format t "~%=== Text Batching Demo ===~%")
  
  ;; Enable batching
  (enable-text-optimizations :batching t :caching nil)
  (reset-text-perf-stats)
  
  (format t "Testing batched text rendering:~%")
  (format t "Rendering 10 text strings with batching enabled...~%")
  
  ;; Begin batch
  (begin-text-batch)
  
  ;; Add multiple text strings to batch
  (dotimes (i 10)
    (let ((text (format nil "Batched text line ~d" i)))
      (draw-text-optimized (get-font-default) text (vec2 10.0 (* i 16.0)) 12 1.0 +blue+)))
  
  ;; End batch (flush)
  (end-text-batch)
  
  (let ((stats (get-text-perf-stats)))
    (format t "Batching results:~%")
    (format t "- Batches used: ~d~%" (text-perf-stats-batches-used stats))
    (format t "- Characters rendered: ~d~%" (text-perf-stats-characters-rendered stats))
    (format t "- Draw calls: ~d~%" (text-perf-stats-draw-calls stats)))
  
  ;; Compare with non-batched rendering
  (format t "~%Testing non-batched rendering for comparison:~%")
  (disable-text-optimizations)
  (reset-text-perf-stats)
  
  (dotimes (i 10)
    (let ((text (format nil "Non-batched text line ~d" i)))
      (draw-text-optimized (get-font-default) text (vec2 10.0 (* i 16.0)) 12 1.0 +red+)))
  
  (let ((stats (get-text-perf-stats)))
    (format t "Non-batched results:~%")
    (format t "- Draw calls: ~d~%" (text-perf-stats-draw-calls stats))
    (format t "- Characters rendered: ~d~%" (text-perf-stats-characters-rendered stats)))
  
  (format t "Text batching demo completed.~%"))

(defun demo-text-caching ()
  "Demonstrate text caching system"
  (format t "~%=== Text Caching Demo ===~%")
  
  ;; Enable caching
  (enable-text-optimizations :batching nil :caching t)
  (clear-text-cache)
  (reset-text-perf-stats)
  
  (format t "Testing text caching system:~%")
  (let ((test-texts '("Hello World" "Cached Text" "Performance Test" "Hello World")))
    
    (format t "First pass - populating cache:~%")
    (dolist (text test-texts)
      (draw-text-optimized (get-font-default) text (vec2 10.0 20.0) 16 1.0 +green+)
      (format t "  Rendered: '~a'~%" text))
    
    (let ((stats (get-text-perf-stats)))
      (format t "Cache statistics after first pass:~%")
      (format t "- Cache hits: ~d~%" (text-perf-stats-cache-hits stats))
      (format t "- Cache misses: ~d~%" (text-perf-stats-cache-misses stats)))
    
    ;; Reset stats but keep cache
    (setf (text-perf-stats-cache-hits *text-perf-stats*) 0)
    (setf (text-perf-stats-cache-misses *text-perf-stats*) 0)
    
    (format t "~%Second pass - using cache:~%")
    (dolist (text test-texts)
      (draw-text-optimized (get-font-default) text (vec2 10.0 20.0) 16 1.0 +green+)
      (format t "  Rendered: '~a'~%" text))
    
    (let ((stats (get-text-perf-stats)))
      (format t "Cache statistics after second pass:~%")
      (format t "- Cache hits: ~d~%" (text-perf-stats-cache-hits stats))
      (format t "- Cache misses: ~d~%" (text-perf-stats-cache-misses stats))
      (let ((total-attempts (+ (text-perf-stats-cache-hits stats) 
                              (text-perf-stats-cache-misses stats))))
        (when (> total-attempts 0)
          (format t "- Cache hit ratio: ~,2f~%" 
                  (/ (text-perf-stats-cache-hits stats) total-attempts))))))
  
  (format t "Text caching demo completed.~%"))

(defun demo-gpu-font-atlas ()
  "Demonstrate GPU font atlas optimization"
  (format t "~%=== GPU Font Atlas Demo ===~%")
  
  (let ((default-font (get-font-default)))
    (format t "Creating GPU font atlas for default font...~%")
    
    ;; Create GPU atlas
    (let ((atlas (create-gpu-font-atlas default-font 256)))
      (format t "GPU Atlas created:~%")
      (format t "- Size: ~dx~d~%" (gpu-font-atlas-width atlas) (gpu-font-atlas-height atlas))
      (format t "- Glyph cache entries: ~d~%" 
              (hash-table-count (gpu-font-atlas-glyph-cache atlas)))
      (format t "- Last update: ~,2f~%" (gpu-font-atlas-last-update atlas))
      
      ;; Test glyph lookup
      (format t "~%Testing glyph cache lookups:~%")
      (format t "Char | Found~%")
      (format t "-----|------~%")
      (loop for char across "HELLO" do
        (let ((rect (gethash char (gpu-font-atlas-glyph-cache atlas))))
          (format t " ~c   | ~a~%" char (if rect "Yes" "No"))))
      
      ;; Get atlas through system
      (let ((cached-atlas (get-gpu-font-atlas default-font)))
        (format t "~%Atlas retrieval test:~%")
        (format t "- Same atlas returned: ~a~%" (eq atlas cached-atlas))))
    
    (format t "GPU font atlas demo completed.~%")))

(defun demo-performance-analysis ()
  "Demonstrate performance analysis and recommendations"
  (format t "~%=== Performance Analysis Demo ===~%")
  
  ;; Set up different scenarios for analysis
  (enable-text-optimizations :batching t :caching t)
  (reset-text-perf-stats)
  
  ;; Scenario 1: Many small text draws (should recommend batching)
  (format t "Scenario 1: Many small text draws~%")
  (dotimes (i 20)
    (draw-text-optimized (get-font-default) "Hi" (vec2 (* i 10.0) 10.0) 12 1.0 +black+))
  
  (format t "~%~a~%" (analyze-text-performance))
  
  ;; Scenario 2: Repeated text (should show good cache performance)
  (format t "~%Scenario 2: Repeated text rendering~%")
  (reset-text-perf-stats)
  (dotimes (i 10)
    (draw-text-optimized (get-font-default) "Repeated Text" (vec2 10.0 10.0) 16 1.0 +blue+))
  
  (format t "~%~a~%" (analyze-text-performance))
  
  ;; Scenario 3: Large text blocks (should show good batching)
  (format t "~%Scenario 3: Large text blocks~%")
  (reset-text-perf-stats)
  (begin-text-batch)
  (dotimes (i 5)
    (let ((long-text "This is a longer text string that contains many characters for testing"))
      (draw-text-optimized (get-font-default) long-text (vec2 10.0 (* i 20.0)) 14 1.0 +green+)))
  (end-text-batch)
  
  (format t "~%~a~%" (analyze-text-performance))
  
  (format t "Performance analysis demo completed.~%"))

(defun demo-optimization-settings ()
  "Demonstrate optimization settings and configuration"
  (format t "~%=== Optimization Settings Demo ===~%")
  
  (format t "Current optimization info:~%")
  (format t "~a~%" (get-text-optimization-info))
  
  ;; Test different optimization combinations
  (format t "~%Testing different optimization combinations:~%")
  
  (format t "~%1. All optimizations enabled:~%")
  (enable-text-optimizations :batching t :caching t :gpu-atlas t)
  (format t "~a~%" (get-text-optimization-info))
  
  (format t "~%2. Only batching enabled:~%")
  (enable-text-optimizations :batching t :caching nil :gpu-atlas nil)
  (format t "~a~%" (get-text-optimization-info))
  
  (format t "~%3. Only caching enabled:~%")
  (enable-text-optimizations :batching nil :caching t :gpu-atlas nil)
  (format t "~a~%" (get-text-optimization-info))
  
  (format t "~%4. All optimizations disabled:~%")
  (disable-text-optimizations)
  (format t "~a~%" (get-text-optimization-info))
  
  ;; Restore optimal settings
  (format t "~%Restoring optimal settings:~%")
  (enable-text-optimizations :batching t :caching t :gpu-atlas t)
  (format t "~a~%" (get-text-optimization-info))
  
  (format t "Optimization settings demo completed.~%"))

(defun demo-memory-and-cleanup ()
  "Demonstrate memory management and cleanup"
  (format t "~%=== Memory Management Demo ===~%")
  
  ;; Create some cached text entries
  (enable-text-optimizations :caching t)
  (clear-text-cache)
  
  (format t "Creating text cache entries...~%")
  (dotimes (i 10)
    (let ((text (format nil "Cache entry ~d" i)))
      (draw-text-optimized (get-font-default) text (vec2 10.0 10.0) 16 1.0 +red+)))
  
  (format t "Cache entries created: ~d~%" (hash-table-count *text-cache*))
  
  ;; Create GPU atlases
  (format t "~%Creating GPU font atlases...~%")
  (let ((atlas1 (create-gpu-font-atlas (get-font-default) 256))
        (atlas2 (create-gpu-font-atlas (get-font-default) 512)))
    (format t "GPU atlases created: ~d~%" (hash-table-count *gpu-font-atlases*)))
  
  ;; Show memory usage stats
  (let ((stats (get-text-perf-stats)))
    (format t "~%Memory statistics:~%")
    (format t "- GPU memory used: ~,1f KB~%" (/ (text-perf-stats-gpu-memory-used stats) 1024.0))
    (format t "- Cache entries: ~d~%" (hash-table-count *text-cache*))
    (format t "- GPU atlases: ~d~%" (hash-table-count *gpu-font-atlases*)))
  
  ;; Cleanup
  (format t "~%Performing cleanup...~%")
  (cleanup-text-optimizations)
  
  (format t "After cleanup:~%")
  (format t "- Cache entries: ~d~%" (hash-table-count *text-cache*))
  (format t "- GPU atlases: ~d~%" (hash-table-count *gpu-font-atlases*))
  
  (format t "Memory management demo completed.~%"))

(defun demo-text-batch-structures ()
  "Demonstrate text batch data structures"
  (format t "~%=== Text Batch Structures Demo ===~%")
  
  ;; Create batch vertex
  (let ((vertex (make-text-batch-vertex :position (vec2 100.0 200.0)
                                       :texcoord (vec2 0.5 0.5)
                                       :color +yellow+)))
    (format t "Text Batch Vertex:~%")
    (format t "- Position: (~,1f, ~,1f)~%" 
            (first (text-batch-vertex-position vertex))
            (second (text-batch-vertex-position vertex)))
    (format t "- Texcoord: (~,2f, ~,2f)~%" 
            (first (text-batch-vertex-texcoord vertex))
            (second (text-batch-vertex-texcoord vertex)))
    (format t "- Color: ~a~%" (text-batch-vertex-color vertex)))
  
  ;; Create render batch
  (let ((batch (make-text-render-batch :capacity 500)))
    (format t "~%Text Render Batch:~%")
    (format t "- Capacity: ~d~%" (text-render-batch-capacity batch))
    (format t "- Vertex count: ~d~%" (text-render-batch-vertex-count batch))
    (format t "- Triangle count: ~d~%" (text-render-batch-triangle-count batch))
    (format t "- Has texture: ~a~%" (if (text-render-batch-texture batch) "Yes" "No")))
  
  ;; Create cache entry
  (let ((cache-entry (make-text-cache-entry :text "Sample Text"
                                           :font-size 16
                                           :spacing 1.0
                                           :color +white+
                                           :last-used (get-time)
                                           :access-count 5)))
    (format t "~%Text Cache Entry:~%")
    (format t "- Text: '~a'~%" (text-cache-entry-text cache-entry))
    (format t "- Font size: ~d~%" (text-cache-entry-font-size cache-entry))
    (format t "- Spacing: ~,1f~%" (text-cache-entry-spacing cache-entry))
    (format t "- Access count: ~d~%" (text-cache-entry-access-count cache-entry))
    (format t "- Last used: ~,2f~%" (text-cache-entry-last-used cache-entry)))
  
  (format t "Text batch structures demo completed.~%"))

(defun run-all-text-optimization-demos ()
  "Run all text optimization demos in sequence"
  (format t "==========================================~%")
  (format t "Pure-Raylib Text Optimization Demo Suite~%")
  (format t "==========================================~%")
  
  ;; Initialize text system
  (init-text-system)
  
  ;; Run all demos
  (demo-text-performance-stats)
  (demo-text-batching)
  (demo-text-caching)
  (demo-gpu-font-atlas)
  (demo-performance-analysis)
  (demo-optimization-settings)
  (demo-memory-and-cleanup)
  (demo-text-batch-structures)
  
  (format t "~%==========================================~%")
  (format t "All text optimization demos completed!~%")
  (format t "~%Key features demonstrated:~%")
  (format t "- Performance statistics tracking~%")
  (format t "- Text batching for reduced draw calls~%")
  (format t "- Text caching for repeated content~%")
  (format t "- GPU font atlas optimization~%")
  (format t "- Performance analysis and recommendations~%")
  (format t "- Configurable optimization settings~%")
  (format t "- Memory management and cleanup~%")
  (format t "- Comprehensive data structures~%")
  (format t "==========================================~%"))

;; Auto-run when loaded
(eval-when (:load-toplevel :execute)
  (format t "Text Optimization Demo loaded. Run (run-all-text-optimization-demos) to see all demos.~%"))