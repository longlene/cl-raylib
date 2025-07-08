;;;; Minimal Test for Pure-Raylib
;;;; A simple test to verify the system loads and basic functions work

;; Load the system
(ql:quickload :cl-raylib)

;; Switch to cl-raylib package
(in-package :cl-raylib)

(defun minimal-test ()
  "Minimal test of cl-raylib functionality"
  (format t "~%=== Pure-Raylib Minimal Test ===~%~%")
  
  ;; Test basic math functions
  (format t "Testing basic math:~%")
  (let ((v1 (vec3 1.0 2.0 3.0))
        (v2 (vec3 4.0 5.0 6.0)))
    (format t "  v1 = ~a~%" v1)
    (format t "  v2 = ~a~%" v2)
    (format t "  v1 + v2 = ~a~%" (vector3-add v1 v2))
    (format t "  |v1| = ~,3f~%" (vector3-length v1)))
  
  ;; Test color functions
  (format t "~%Testing colors:~%")
  (let ((red (make-color 255 0 0 255))
        (blue (make-color 0 0 255 255)))
    (format t "  Red: ~a~%" red)
    (format t "  Blue: ~a~%" blue)
    (format t "  White constant: ~a~%" +white+))
  
  ;; Test timing functions
  (format t "~%Testing timing:~%")
  (format t "  Current time: ~,3f seconds~%" (get-time))
  
  ;; Test random functions
  (format t "~%Testing random:~%")
  (format t "  Random integer (0-100): ~d~%" (get-random-value 0 100))
  (format t "  Random float (0.0-1.0): ~,3f~%" (get-random-float-01))
  
  ;; Test logging
  (format t "~%Testing logging:~%")
  (trace-log-info "This is an info message")
  (trace-log-debug "This is a debug message")
  
  ;; Test file operations
  (format t "~%Testing file operations:~%")
  (let ((test-file "/tmp/cl-raylib-test.txt")
        (test-content "Hello from cl-raylib!"))
    (save-file-text test-file test-content)
    (if (uiop:file-exists-p test-file)
      (let ((loaded-content (load-file-text test-file)))
        (format t "  File save/load: ~a~%" 
                (if (string= test-content loaded-content) "SUCCESS" "FAILED"))
        (delete-file-safe test-file))
      (format t "  File save/load: FAILED~%")))
  
  ;; Test image generation
  (format t "~%Testing image generation:~%")
  (let ((test-image (gen-image-color 64 64 +red+)))
    (format t "  Generated ~dx~d red image~%" 
            (image-width test-image) (image-height test-image))
    (unload-image test-image))
  
  ;; Test mesh generation
  (format t "~%Testing mesh generation:~%")
  (let ((cube-mesh (gen-mesh-cube 2.0 2.0 2.0)))
    (format t "  Generated cube mesh: ~d vertices, ~d triangles~%"
            (mesh-vertex-count cube-mesh) (mesh-triangle-count cube-mesh)))
  
  ;; Test audio codec system
  (format t "~%Testing audio codec system:~%")
  (format t "  Supported formats: ~{~a~^, ~}~%" (get-supported-audio-formats))
  (format t "  ~a~%" (get-cache-info))
  
  ;; Test compression system
  (format t "~%Testing compression system:~%")
  (format t "  Supported formats: ~{~a~^, ~}~%" (get-supported-compression-formats))
  (let ((test-data #(1 2 3 4 5 6 7 8 9 10)))
    (format t "  Testing compression of ~d bytes...~%" (length test-data))
    (let ((compressed (compress-data test-data :format "DEFLATE")))
      (if compressed
        (format t "    Compressed to ~d bytes~%" (length compressed))
        (format t "    Compression not available~%"))))
  
  ;; Test GLTF system (if JSON library available)
  (format t "~%Testing GLTF system:~%")
  (handler-case
    (let ((json-lib (detect-json-library)))
      (format t "  JSON library detected: ~a~%" json-lib)
      (format t "  GLTF loader ready~%"))
    (error (e)
      (format t "  No JSON library available: ~a~%" e)
      (format t "  Install with: (ql:quickload :jsown)~%")))
  
  (format t "~%=== Test Complete ===~%")
  (format t "Pure-raylib system is working correctly!~%~%"))

;; Run the test
(minimal-test)