;;;; Simple Audio Test
;;;; Test basic audio functionality in cl-raylib

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun simple-audio-test ()
  "Simple test of cl-raylib audio functionality"
  (format t "~%=== Simple Audio Test ===~%")
  
  ;; Initialize audio device
  (init-audio-device)
  (format t "Audio device ready: ~a~%" (is-audio-device-ready))
  
  ;; Test master volume
  (set-master-volume 0.8)
  (format t "Master volume: ~,2f~%" (get-master-volume))
  
  ;; Create a dummy wave and test sound loading
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave)))
    
    (format t "Wave valid: ~a~%" (is-wave-valid wave))
    (format t "Sound valid: ~a~%" (is-sound-valid sound))
    
    ;; Test sound properties
    (set-sound-volume sound 0.5)
    (set-sound-pitch sound 1.2)
    (set-sound-pan sound -0.3)
    
    ;; Test playback functions (no actual audio, just logging)
    (play-sound sound)
    (pause-sound sound)
    (resume-sound sound)
    (stop-sound sound)
    
    ;; Test 3D audio system
    (init-3d-audio-system)
    (format t "~%3D Audio Info: ~a~%" (get-3d-audio-info))
    
    ;; Create 3D positioned sound
    (let ((sound-3d (create-sound-3d sound (vec3 5.0 0.0 0.0))))
      (format t "3D Sound created~%")
      
      ;; Test listener positioning
      (set-listener-position (vec3 0.0 0.0 0.0))
      (set-listener-orientation (vec3 0.0 0.0 -1.0) (vec3 0.0 1.0 0.0))
      
      ;; Test 3D sound positioning
      (set-sound-3d-position sound-3d (vec3 10.0 0.0 0.0))
      (format t "3D Sound volume: ~,3f~%" (sound-3d-calculated-volume sound-3d))
      (format t "3D Sound pan: ~,3f~%" (sound-3d-calculated-pan sound-3d))
      
      ;; Test distance models
      (set-3d-audio-distance-model :linear)
      (format t "Distance model set to: ~a~%" *distance-model*)
      
      ;; Test Doppler effect
      (set-sound-3d-velocity sound-3d (vec3 10.0 0.0 0.0))
      (let ((doppler (calculate-doppler-effect sound-3d)))
        (format t "Doppler factor: ~,3f~%" doppler))
      
      ;; Test reverb zone
      (let ((reverb-zone (create-reverb-zone (vec3 0.0 0.0 0.0) (vec3 10.0 10.0 10.0) 0.5 1.5)))
        (format t "Reverb effect at origin: ~,3f~%" (calculate-reverb-effect (vec3 0.0 0.0 0.0)))
        (remove-reverb-zone reverb-zone))
      
      ;; Cleanup
      (remove-sound-3d sound-3d))
    
    ;; Cleanup
    (unload-sound sound)
    (unload-wave wave))
  
  ;; Cleanup audio systems
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "~%Audio test completed successfully!~%"))

;; Run the test
(simple-audio-test)