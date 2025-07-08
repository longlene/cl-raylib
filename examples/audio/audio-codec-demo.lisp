;;;; Audio Codec System Demonstration
;;;; This demo shows how to use the new audio codec system in cl-raylib

(require 'cl-raylib)
(in-package :cl-raylib)

(defun audio-codec-demo ()
  "Demonstrate the audio codec system capabilities"
  (format t "~%=== Pure-Raylib Audio Codec System Demo ===~%~%")
  
  ;; Show codec registration info
  (format t "Audio Codec Information:~%")
  (format t "~a~%~%" (get-audio-codec-info))
  
  ;; Show supported formats
  (format t "Supported Audio Formats: ~{~a~^, ~}~%~%" (get-supported-audio-formats))
  
  ;; Show cache info
  (format t "~a~%~%" (get-cache-info))
  
  ;; Demo codec detection by extension
  (format t "Codec Detection Examples:~%")
  (dolist (ext '(".flac" ".wav" ".mp3" ".ogg" ".unknown"))
    (let ((codec (get-codec-by-extension ext)))
      (if codec
        (format t "  ~a -> ~a (~a)~%" 
                ext 
                (audio-codec-info-name codec)
                (if (is-codec-enabled (audio-codec-info-name codec)) "enabled" "disabled"))
        (format t "  ~a -> No codec found~%" ext))))
  
  (format t "~%")
  
  ;; Demo format detection
  (format t "Format Detection Examples:~%")
  (dolist (filename '("test.flac" "music.wav" "song.mp3" "audio.ogg" "unknown.xyz"))
    (format t "  ~a -> ~a~%" filename (detect-audio-format filename)))
  
  (format t "~%")
  
  ;; Demo fallback audio generation
  (format t "Testing fallback audio generation...~%")
  (let ((dummy-audio (create-dummy-audio-data "test.wav" 44100 2 16 8820))) ; 0.2 seconds
    (when dummy-audio
      (format t "Generated dummy audio:~%")
      (format t "  Sample Rate: ~d Hz~%" (decoded-audio-data-sample-rate dummy-audio))
      (format t "  Channels: ~d~%" (decoded-audio-data-channels dummy-audio))
      (format t "  Bits per Sample: ~d~%" (decoded-audio-data-bits-per-sample dummy-audio))
      (format t "  Total Samples: ~d~%" (decoded-audio-data-total-samples dummy-audio))
      (format t "  Data Size: ~d bytes~%" (length (decoded-audio-data-data dummy-audio)))
      (format t "  Duration: ~,2f seconds~%" 
              (/ (decoded-audio-data-total-samples dummy-audio)
                 (decoded-audio-data-sample-rate dummy-audio)))))
  
  (format t "~%")
  
  ;; Show library integration status
  (format t "Library Integration Status:~%")
  (format t "  easy-audio.flac: ~a~%" 
          (if (find-package :easy-audio.flac) "Available" "Not loaded"))
  (format t "  easy-audio.wav: ~a~%" 
          (if (find-package :easy-audio.wav) "Available" "Not loaded"))
  (format t "  easy-audio.ogg: ~a~%" 
          (if (find-package :easy-audio.ogg) "Available" "Not loaded"))
  (format t "  cl-mpg123: ~a~%" 
          (if (find-package :cl-mpg123) "Available" "Not loaded"))
  
  (format t "~%=== Demo Complete ===~%"))

(defun test-audio-codec-file-operations ()
  "Test audio codec operations with real files (if available)"
  (format t "~%=== Testing Audio Codec File Operations ===~%~%")
  
  ;; Test with dummy files to show the decode flow
  (dolist (test-file '("test.flac" "test.wav" "test.mp3" "test.ogg"))
    (format t "Testing decode of ~a...~%" test-file)
    (handler-case
      (let ((decoded (decode-audio-file test-file)))
        (if decoded
          (format t "  Success: ~dx~d, ~d Hz, ~d samples~%"
                  (decoded-audio-data-channels decoded)
                  (decoded-audio-data-bits-per-sample decoded)
                  (decoded-audio-data-sample-rate decoded)
                  (decoded-audio-data-total-samples decoded))
          (format t "  Failed: No decoder available or file not found~%")))
      (error (e)
        (format t "  Error: ~a~%" e))))
  
  (format t "~%"))

(defun show-codec-capabilities ()
  "Show detailed codec capabilities"
  (format t "~%=== Codec Capabilities ===~%~%")
  
  (loop for name being the hash-keys of *audio-codecs*
        for codec being the hash-values of *audio-codecs*
        do (format t "~a Codec:~%" name)
           (format t "  Extensions: ~{~a~^, ~}~%" (audio-codec-info-extensions codec))
           (format t "  MIME Types: ~{~a~^, ~}~%" (audio-codec-info-mime-types codec))
           (format t "  Lossy: ~a~%" (if (audio-codec-info-lossy codec) "Yes" "No"))
           (format t "  Sample Rates: ~{~d~^ ~}~%" (audio-codec-info-supported-sample-rates codec))
           (format t "  Bit Depths: ~{~d~^ ~}~%" (audio-codec-info-supported-bit-depths codec))
           (format t "  Max Channels: ~d~%" (audio-codec-info-max-channels codec))
           (format t "  Library: ~a~%" (audio-codec-info-library codec))
           (format t "  Enabled: ~a~%~%" (if (is-codec-enabled name) "Yes" "No")))
  
  (format t "=== End Capabilities ===~%"))

(defun run-complete-audio-demo ()
  "Run complete audio codec demonstration"
  (audio-codec-demo)
  (test-audio-codec-file-operations)
  (show-codec-capabilities)
  
  ;; Show cache operations
  (format t "~%=== Cache Operations ===~%")
  (format t "Initial: ~a~%" (get-cache-info))
  
  ;; Simulate caching some audio
  (cache-audio-data "test1.wav" (create-dummy-audio-data "test1.wav" 44100 2 16 4410))
  (cache-audio-data "test2.flac" (create-dummy-audio-data "test2.flac" 48000 2 24 4800))
  
  (format t "After caching: ~a~%" (get-cache-info))
  
  (clear-audio-cache)
  (format t "After clearing: ~a~%" (get-cache-info))
  
  (format t "~%=== Complete Demo Finished ===~%"))

;; Run the demo when this file is loaded
(eval-when (:load-toplevel :execute)
  (format t "~%Loading audio codec demo...~%")
  (format t "Run (cl-raylib:run-complete-audio-demo) to see the demonstration~%"))