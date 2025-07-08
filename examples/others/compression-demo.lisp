;;;; Compression System Demonstration
;;;; This demo shows how to use the compression system in cl-raylib

(require 'cl-raylib)
(in-package :cl-raylib)

(defun compression-demo ()
  "Demonstrate the compression system capabilities"
  (format t "~%=== Pure-Raylib Compression System Demo ===~%~%")
  
  ;; Show compression system info
  (format t "~a~%~%" (get-compression-system-info))
  
  ;; Show supported formats
  (format t "Supported Compression Formats: ~{~a~^, ~}~%~%" (get-supported-compression-formats))
  
  ;; Demo format detection
  (format t "Format Detection Examples:~%")
  (let ((test-data-gzip (make-array 10 :element-type '(unsigned-byte 8) 
                                   :initial-contents '(#x1F #x8B #x08 #x00 #x00 #x00 #x00 #x00 #x00 #xFF)))
        (test-data-zlib (make-array 10 :element-type '(unsigned-byte 8)
                                   :initial-contents '(#x78 #x9C #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)))
        (test-data-bzip2 (make-array 10 :element-type '(unsigned-byte 8)
                                    :initial-contents '(#x42 #x5A #x68 #x39 #x31 #x41 #x59 #x26 #x53 #x59))))
    
    (format t "  GZIP magic bytes -> ~a~%" (detect-compression-format test-data-gzip))
    (format t "  ZLIB magic bytes -> ~a~%" (detect-compression-format test-data-zlib))
    (format t "  BZIP2 magic bytes -> ~a~%" (detect-compression-format test-data-bzip2))
    (format t "  Unknown data -> ~a~%" (detect-compression-format #(#x00 #x01 #x02 #x03))))
  
  (format t "~%"))

(defun test-compression-operations ()
  "Test compression and decompression operations"
  (format t "=== Testing Compression Operations ===~%~%")
  
  ;; Create test data
  (let* ((test-string "Hello, World! This is a test string for compression. It should compress well due to repetitive patterns. Hello, World! Hello, World!")
         (test-data (map 'vector #'char-code test-string)))
    
    (format t "Original data: ~d bytes~%" (length test-data))
    (format t "Original text: \"~a\"~%~%" (subseq test-string 0 (min 50 (length test-string))))
    
    ;; Test each compression format
    (dolist (format '("DEFLATE" "ZLIB" "GZIP"))
      (format t "Testing ~a compression:~%" format)
      
      (let ((compressed (compress-data test-data :format format :level 6)))
        (if compressed
          (progn
            (format t "  Compressed: ~d bytes (~,1f%% of original)~%"
                    (length compressed)
                    (* 100.0 (/ (length compressed) (length test-data))))
            
            ;; Test decompression
            (let ((decompressed (decompress-data compressed)))
              (if (and decompressed (equalp decompressed test-data))
                (format t "  Decompression: SUCCESS~%")
                (format t "  Decompression: FAILED~%"))))
          (format t "  Compression: NOT AVAILABLE~%")))
      (format t "~%"))))

(defun test-library-integration ()
  "Test integration with Common Lisp compression libraries"
  (format t "=== Testing Library Integration ===~%~%")
  
  ;; Check available libraries
  (format t "Library Availability:~%")
  (format t "  chipz: ~a~%" 
          (if (find-package :chipz) "Available" "Not loaded"))
  (format t "  salza2: ~a~%" 
          (if (find-package :salza2) "Available" "Not loaded"))
  (format t "  decompress: ~a~%" 
          (if (find-package :decompress) "Available" "Not loaded"))
  
  (format t "~%")
  
  ;; Test with libraries if available
  (when (find-package :salza2)
    (format t "Testing with salza2 library:~%")
    (let* ((test-data #(72 101 108 108 111 44 32 87 111 114 108 100 33))
           (compressed (compress-data test-data :format "ZLIB")))
      (when compressed
        (format t "  ZLIB compression successful: ~d -> ~d bytes~%" 
                (length test-data) (length compressed)))))
  
  (when (find-package :chipz)
    (format t "Testing with chipz library:~%")
    (format t "  Decompression support available~%"))
  
  (when (find-package :decompress)
    (format t "Testing with decompress library:~%")
    (format t "  Fast decompression support available~%"))
  
  (format t "~%"))

(defun demo-compression-caching ()
  "Demonstrate compression caching system"
  (format t "=== Compression Caching Demo ===~%~%")
  
  (format t "Initial: ~a~%~%" (get-compression-cache-info))
  
  ;; Create test data
  (let ((test-data #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
    
    ;; First compression (should cache)
    (format t "First compression (should cache result):~%")
    (let ((start-time (get-time)))
      (compress-data test-data :format "DEFLATE")
      (format t "  Time: ~,3f seconds~%" (- (get-time) start-time)))
    
    (format t "After first compression: ~a~%~%" (get-compression-cache-info))
    
    ;; Second compression (should use cache)
    (format t "Second compression (should use cache):~%")
    (let ((start-time (get-time)))
      (compress-data test-data :format "DEFLATE")
      (format t "  Time: ~,3f seconds~%" (- (get-time) start-time)))
    
    ;; Clear cache
    (clear-compression-cache)
    (format t "After clearing cache: ~a~%~%" (get-compression-cache-info))))

(defun test-file-compression ()
  "Test file compression and decompression"
  (format t "=== File Compression Test ===~%~%")
  
  ;; Create a test file
  (let* ((test-filename "/tmp/test-compression.txt")
         (compressed-filename "/tmp/test-compression.deflate")
         (decompressed-filename "/tmp/test-compression-restored.txt")
         (test-content "This is a test file for compression.\nIt contains multiple lines.\nAnd some repetitive text.\nRepetitive text is good for compression.\nRepetitive text compresses well."))
    
    ;; Write test file
    (save-file-text test-filename test-content)
    (format t "Created test file: ~a (~d bytes)~%" test-filename (length test-content))
    
    ;; Compress file
    (compress-file test-filename compressed-filename :format "DEFLATE" :level 6)
    
    ;; Check if compression worked
    (when (uiop:file-exists-p compressed-filename)
      (let ((compressed-size (get-file-length compressed-filename)))
        (format t "Compressed file: ~a (~d bytes)~%" compressed-filename compressed-size)
        
        ;; Decompress file
        (decompress-file compressed-filename decompressed-filename)
        
        ;; Check if decompression worked
        (when (uiop:file-exists-p decompressed-filename)
          (let ((restored-content (load-file-text decompressed-filename)))
            (if (string= test-content restored-content)
              (format t "File compression/decompression: SUCCESS~%")
              (format t "File compression/decompression: FAILED - content mismatch~%"))))
        
        ;; Cleanup
        (delete-file-safe test-filename)
        (delete-file-safe compressed-filename)
        (delete-file-safe decompressed-filename)))
    
    (format t "~%")))

(defun show-compression-format-details ()
  "Show detailed information about compression formats"
  (format t "=== Compression Format Details ===~%~%")
  
  (loop for name being the hash-keys of *compression-formats*
        for format being the hash-values of *compression-formats*
        do (format t "~a Format:~%" name)
           (format t "  Extensions: ~{~a~^, ~}~%" (compression-format-info-extensions format))
           (format t "  MIME Types: ~{~a~^, ~}~%" (compression-format-info-mime-types format))
           (format t "  Library: ~a~%" (compression-format-info-library format))
           (format t "  Compression: ~a~%" 
                   (if (compression-format-info-compressor format) "Supported" "Not supported"))
           (format t "  Decompression: ~a~%" 
                   (if (compression-format-info-decompressor format) "Supported" "Not supported"))
           (format t "  Level Support: ~a~%" 
                   (if (compression-format-info-compression-level-support format) "Yes" "No"))
           (format t "  Magic Bytes: ~a~%" 
                   (if (compression-format-info-magic-bytes format)
                     (format nil "~{~2,'0X~^ ~}" (coerce (compression-format-info-magic-bytes format) 'list))
                     "None"))
           (format t "  Enabled: ~a~%~%" (if (is-compression-format-enabled name) "Yes" "No")))
  
  (format t "=== End Format Details ===~%"))

(defun run-complete-compression-demo ()
  "Run complete compression system demonstration"
  (compression-demo)
  (test-compression-operations)
  (test-library-integration)
  (demo-compression-caching)
  (test-file-compression)
  (show-compression-format-details)
  
  (format t "~%=== Complete Compression Demo Finished ===~%")
  (format t "Note: Full functionality requires installing Common Lisp compression libraries:~%")
  (format t "  - (ql:quickload :chipz) for decompression~%")
  (format t "  - (ql:quickload :salza2) for compression~%")
  (format t "  - (ql:quickload :decompress) for fast decompression~%"))

;; Run demonstration when file is loaded
(eval-when (:load-toplevel :execute)
  (format t "~%Loading compression demo...~%")
  (format t "Run (cl-raylib:run-complete-compression-demo) to see the demonstration~%"))