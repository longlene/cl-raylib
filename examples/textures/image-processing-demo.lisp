;;;; Advanced Image Processing Demo
;;;; Comprehensive demonstration of opticl integration and advanced image processing

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun demo-image-format-detection ()
  "Demonstrate image format detection and verification"
  (format t "~%=== Image Format Detection Demo ===~%")
  
  (let ((test-files '("test.png" "photo.jpg" "image.bmp" "animation.gif" "texture.tga" "unknown.xyz")))
    (format t "Testing format detection:~%")
    (format t "Filename        | Detected Format | Magic Verified~%")
    (format t "----------------|-----------------|---------------~%")
    
    (dolist (filename test-files)
      (let* ((format (detect-image-format filename))
             (verified (if (uiop:file-exists-p filename)
                         (verify-image-format filename format)
                         "File not found")))
        (format t "~15a | ~15a | ~a~%" filename format verified))))
  
  (format t "~%Magic bytes analysis:~%")
  ;; Create test files with magic bytes for demonstration
  (let ((test-data (list 
                    (list "PNG" (vector #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A))
                    (list "JPEG" (vector #xFF #xD8 #xFF #xE0))
                    (list "BMP" (vector #x42 #x4D))
                    (list "GIF87a" (vector #x47 #x49 #x46 #x38 #x37 #x61))
                    (list "GIF89a" (vector #x47 #x49 #x46 #x38 #x39 #x61)))))
    
    (format t "Format  | Magic Bytes~%")
    (format t "--------|------------~%")
    (dolist (entry test-data)
      (format t "~7a | ~{~2,'0X ~}~%" (first entry) (coerce (second entry) 'list))))
  
  (format t "Format detection demo completed.~%"))

(defun demo-opticl-integration ()
  "Demonstrate opticl integration and fallback handling"
  (format t "~%=== Opticl Integration Demo ===~%")
  
  (format t "System status:~%")
  (format t "- Image processing enabled: ~a~%" *image-processing-enabled*)
  (format t "- Opticl package available: ~a~%" (if (find-package :opticl) "Yes" "No"))
  (format t "- Supported formats: ~{~a~^, ~}~%" (get-supported-image-formats))
  
  ;; Test image creation and conversion
  (format t "~%Creating test images for conversion:~%")
  (let ((test-images (list
                      (create-mock-image "test-gray.png" 1)
                      (create-mock-image "test-rgb.png" 3)
                      (create-mock-image "test-rgba.png" 4))))
    
    (dolist (image test-images)
      (format t "Image: ~dx~d, ~d channels, format: ~d~%"
              (advanced-image-width image)
              (advanced-image-height image)
              (advanced-image-channels image)
              (advanced-image-format image))
      
      ;; Test opticl conversion (if available)
      (when (find-package :opticl)
        (let ((opticl-img (convert-to-opticl-image image)))
          (if opticl-img
            (let ((converted-back (convert-from-opticl-image opticl-img "converted.png")))
              (format t "  -> Opticl conversion: Success (~dx~d)~%"
                      (advanced-image-width converted-back)
                      (advanced-image-height converted-back)))
            (format t "  -> Opticl conversion: Failed~%"))))))
  
  (format t "Opticl integration demo completed.~%"))

(defun demo-image-loading-and-saving ()
  "Demonstrate advanced image loading and saving"
  (format t "~%=== Image Loading and Saving Demo ===~%")
  
  ;; Test loading with different methods
  (let ((test-filename "demo-image.png"))
    (format t "Testing image loading methods:~%")
    
    ;; Create a test image
    (let ((test-image (create-mock-image test-filename 4)))
      (format t "Created test image: ~dx~d, ~d channels~%"
              (advanced-image-width test-image)
              (advanced-image-height test-image)
              (advanced-image-channels test-image))
      
      ;; Test saving in different formats
      (format t "~%Testing save formats:~%")
      (let ((formats '(:png :jpeg :bmp :tga)))
        (dolist (format formats)
          (let ((filename (format nil "test-output.~a" (string-downcase format))))
            (if (save-image-advanced test-image filename :format format :quality 90)
              (format t "  ~a: Saved successfully~%" format)
              (format t "  ~a: Save failed~%" format)))))
      
      ;; Test loading back
      (format t "~%Testing load with advanced loader:~%")
      (let ((loaded-image (load-image-advanced "test-output.png")))
        (if loaded-image
          (format t "  Loaded image: ~dx~d, ~d channels~%"
                  (advanced-image-width loaded-image)
                  (advanced-image-height loaded-image)
                  (advanced-image-channels loaded-image))
          (format t "  Failed to load image~%")))))
  
  (format t "Image loading and saving demo completed.~%"))

(defun demo-image-processing-filters ()
  "Demonstrate various image processing filters"
  (format t "~%=== Image Processing Filters Demo ===~%")
  
  ;; Create test image
  (let ((original-image (create-mock-image "filter-test.png" 3)))
    (format t "Created test image: ~dx~d for filter testing~%"
            (advanced-image-width original-image)
            (advanced-image-height original-image))
    
    ;; Test different filters
    (let ((filters '((:grayscale)
                    (:blur 2)
                    (:sharpen 1.5)
                    (:brightness 0.2)
                    (:contrast 1.3)
                    (:sepia)
                    (:horizontal-flip)
                    (:vertical-flip))))
      
      (format t "~%Applying filters:~%")
      (format t "Filter           | Processing Time | Result~%")
      (format t "-----------------|-----------------|-------~%")
      
      (dolist (filter-spec filters)
        (let* ((filter-name (first filter-spec))
               (filter-args (rest filter-spec))
               (start-time (get-time))
               (filtered-image (apply #'apply-image-filter original-image filter-name filter-args))
               (processing-time (* (- (get-time) start-time) 1000)))
          
          (if filtered-image
            (format t "~16a | ~13,1f ms | Success (~dx~d)~%"
                    filter-name processing-time
                    (advanced-image-width filtered-image)
                    (advanced-image-height filtered-image))
            (format t "~16a | ~13,1f ms | Failed~%"
                    filter-name processing-time)))))
    
    ;; Test chained filters
    (format t "~%Testing filter chaining:~%")
    (let* ((step1 (apply-image-filter original-image :blur 1))
           (step2 (apply-image-filter step1 :sharpen 1.2))
           (final (apply-image-filter step2 :contrast 1.1)))
      (format t "Blur -> Sharpen -> Contrast: ~a~%"
              (if final "Success" "Failed"))))
  
  (format t "Image processing filters demo completed.~%"))

(defun demo-image-resizing ()
  "Demonstrate image resizing with different algorithms"
  (format t "~%=== Image Resizing Demo ===~%")
  
  (let ((original-image (create-mock-image "resize-test.png" 4)))
    (format t "Original image: ~dx~d~%"
            (advanced-image-width original-image)
            (advanced-image-height original-image))
    
    ;; Test different resize operations
    (let ((resize-tests '((64 64 :nearest)
                         (256 256 :bilinear)
                         (512 256 :bicubic)
                         (32 32 :lanczos))))
      
      (format t "~%Resize operations:~%")
      (format t "Target Size    | Algorithm | Processing Time | Result~%")
      (format t "---------------|-----------|-----------------|-------~%")
      
      (dolist (test resize-tests)
        (let* ((width (first test))
               (height (second test))
               (algorithm (third test))
               (start-time (get-time))
               (resized-image (resize-image-advanced original-image width height algorithm))
               (processing-time (* (- (get-time) start-time) 1000)))
          
          (if resized-image
            (format t "~6dx~6d  | ~9a | ~13,1f ms | Success~%"
                    width height algorithm processing-time)
            (format t "~6dx~6d  | ~9a | ~13,1f ms | Failed~%"
                    width height algorithm processing-time)))))
    
    ;; Test aspect ratio preservation
    (format t "~%Aspect ratio tests:~%")
    (let ((original-width (advanced-image-width original-image))
          (original-height (advanced-image-height original-image)))
      (format t "Original aspect ratio: ~,3f~%"
              (/ original-width original-height))
      
      (let ((resized (resize-image-advanced original-image 200 200)))
        (when resized
          (format t "Square resize: ~,3f~%"
                  (/ (advanced-image-width resized)
                     (advanced-image-height resized)))))))
  
  (format t "Image resizing demo completed.~%"))

(defun demo-metadata-and-exif ()
  "Demonstrate metadata and EXIF data handling"
  (format t "~%=== Metadata and EXIF Demo ===~%")
  
  (let ((test-image (create-mock-image "metadata-test.jpg" 3)))
    ;; Extract metadata (simulated)
    (extract-exif-data test-image "metadata-test.jpg")
    
    (format t "EXIF data extracted:~%")
    (format t "- Camera make: ~a~%" (get-image-metadata test-image "camera-make"))
    (format t "- Camera model: ~a~%" (get-image-metadata test-image "camera-model"))
    (format t "- Creation date: ~,2f~%" (get-image-metadata test-image "creation-date"))
    (format t "- Orientation: ~a~%" (get-image-metadata test-image "orientation"))
    (format t "- Flash used: ~a~%" (get-image-metadata test-image "flash-used"))
    
    ;; Add custom metadata
    (format t "~%Adding custom metadata:~%")
    (set-image-metadata test-image "processed-by" "cl-raylib")
    (set-image-metadata test-image "processing-date" (get-time))
    (set-image-metadata test-image "filter-applied" "none")
    
    (format t "- Processed by: ~a~%" (get-image-metadata test-image "processed-by"))
    (format t "- Processing date: ~,2f~%" (get-image-metadata test-image "processing-date"))
    (format t "- Filter applied: ~a~%" (get-image-metadata test-image "filter-applied"))
    
    ;; Test metadata preservation through processing
    (let ((filtered-image (apply-image-filter test-image :grayscale)))
      (when filtered-image
        (set-image-metadata filtered-image "filter-applied" "grayscale")
        (format t "~%After grayscale filter:~%")
        (format t "- Filter applied: ~a~%" (get-image-metadata filtered-image "filter-applied")))))
  
  (format t "Metadata and EXIF demo completed.~%"))

(defun demo-performance-comparison ()
  "Demonstrate performance comparison between opticl and fallback"
  (format t "~%=== Performance Comparison Demo ===~%")
  
  (let ((test-image (create-mock-image "perf-test.png" 3))
        (iterations 10))
    
    (format t "Performance test with ~d iterations:~%" iterations)
    (format t "Operation        | Opticl Time | Fallback Time | Speedup~%")
    (format t "-----------------|-------------|---------------|--------~%")
    
    ;; Test resize performance
    (let ((opticl-times nil)
          (fallback-times nil))
      
      ;; Test with opticl (if available)
      (when (find-package :opticl)
        (dotimes (i iterations)
          (let ((start (get-time)))
            (resize-image-with-opticl test-image 64 64 :bilinear)
            (push (* (- (get-time) start) 1000) opticl-times))))
      
      ;; Test with fallback
      (dotimes (i iterations)
        (let ((start (get-time)))
          (resize-image-fallback test-image 64 64)
          (push (* (- (get-time) start) 1000) fallback-times)))
      
      (let ((avg-opticl (if opticl-times (/ (reduce #'+ opticl-times) (length opticl-times)) 0))
            (avg-fallback (/ (reduce #'+ fallback-times) (length fallback-times))))
        (format t "Resize (64x64)   | ~9,1f ms | ~11,1f ms | ~,1fx~%"
                avg-opticl avg-fallback 
                (if (> avg-opticl 0) (/ avg-fallback avg-opticl) 0))))
    
    ;; Test filter performance
    (let ((filter-tests '(:blur :sharpen :grayscale)))
      (dolist (filter filter-tests)
        (let ((fallback-times nil))
          (dotimes (i 5)
            (let ((start (get-time)))
              (apply-filter-fallback test-image filter nil)
              (push (* (- (get-time) start) 1000) fallback-times)))
          
          (let ((avg-time (/ (reduce #'+ fallback-times) (length fallback-times))))
            (format t "~16a | ~9s | ~11,1f ms | ~s~%"
                    filter "N/A" avg-time "N/A"))))))
  
  (format t "Performance comparison demo completed.~%"))

(defun demo-batch-processing ()
  "Demonstrate batch image processing"
  (format t "~%=== Batch Image Processing Demo ===~%")
  
  ;; Create multiple test images
  (let ((test-images (loop for i from 1 to 5 collect
                          (create-mock-image (format nil "batch-~d.png" i) 3))))
    
    (format t "Created ~d test images for batch processing~%" (length test-images))
    
    ;; Batch resize
    (format t "~%Batch resizing to 64x64:~%")
    (let ((start-time (get-time))
          (processed-count 0))
      (dolist (image test-images)
        (let ((resized (resize-image-advanced image 64 64)))
          (when resized
            (incf processed-count))))
      
      (let ((total-time (* (- (get-time) start-time) 1000)))
        (format t "Processed ~d/~d images in ~,1f ms (~,1f ms per image)~%"
                processed-count (length test-images) total-time
                (/ total-time (length test-images)))))
    
    ;; Batch filter application
    (format t "~%Batch filter application (grayscale):~%")
    (let ((start-time (get-time))
          (processed-count 0))
      (dolist (image test-images)
        (let ((filtered (apply-image-filter image :grayscale)))
          (when filtered
            (incf processed-count))))
      
      (let ((total-time (* (- (get-time) start-time) 1000)))
        (format t "Processed ~d/~d images in ~,1f ms (~,1f ms per image)~%"
                processed-count (length test-images) total-time
                (/ total-time (length test-images))))))
  
  (format t "Batch processing demo completed.~%"))

(defun run-all-image-processing-demos ()
  "Run all image processing demos in sequence"
  (format t "==============================================~%")
  (format t "Pure-Raylib Advanced Image Processing Demo Suite~%")
  (format t "==============================================~%")
  
  ;; Initialize image processing system
  (init-image-processing-system)
  
  ;; Run all demos
  (demo-image-format-detection)
  (demo-opticl-integration)
  (demo-image-loading-and-saving)
  (demo-image-processing-filters)
  (demo-image-resizing)
  (demo-metadata-and-exif)
  (demo-performance-comparison)
  (demo-batch-processing)
  
  (format t "~%==============================================~%")
  (format t "All image processing demos completed successfully!~%")
  (format t "~%Key features demonstrated:~%")
  (format t "- Automatic image format detection and verification~%")
  (format t "- Seamless opticl integration with fallback support~%")
  (format t "- Comprehensive image loading and saving~%")
  (format t "- Advanced image processing filters~%")
  (format t "- High-quality image resizing algorithms~%")
  (format t "- EXIF metadata extraction and management~%")
  (format t "- Performance optimization and comparison~%")
  (format t "- Efficient batch processing capabilities~%")
  (format t "==============================================~%"))

;; Auto-run when loaded
(eval-when (:load-toplevel :execute)
  (format t "Image Processing Demo loaded. Run (run-all-image-processing-demos) to see all demos.~%"))