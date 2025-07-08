;;;; 3D Audio System Demo
;;;; Comprehensive demonstration of 3D spatial audio capabilities

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun demo-3d-audio-basic ()
  "Basic 3D audio positioning demo"
  (format t "~%=== 3D Audio Basic Demo ===~%")
  
  ;; Initialize audio system
  (init-audio-device)
  (init-3d-audio-system)
  
  ;; Create a test sound
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave))
         (sound-3d (create-sound-3d sound (vec3 5.0 0.0 0.0))))
    
    ;; Set up audio listener
    (set-listener-position (vec3 0.0 0.0 0.0))
    (set-listener-orientation (vec3 0.0 0.0 -1.0) (vec3 0.0 1.0 0.0))
    
    (format t "Sound positioned at (5, 0, 0), listener at origin~%")
    (format t "Initial 3D parameters:~%")
    (format t "  Calculated volume: ~,3f~%" (sound-3d-calculated-volume sound-3d))
    (format t "  Calculated pan: ~,3f~%" (sound-3d-calculated-pan sound-3d))
    
    ;; Test different positions
    (format t "~%Moving sound to different positions:~%")
    (dolist (pos (list (vec3 -5.0 0.0 0.0) (vec3 0.0 5.0 0.0) (vec3 0.0 0.0 5.0)))
      (set-sound-3d-position sound-3d pos)
      (format t "Position (~,1f, ~,1f, ~,1f): Volume=~,3f, Pan=~,3f~%"
              (vx pos) (vy pos) (vz pos)
              (sound-3d-calculated-volume sound-3d)
              (sound-3d-calculated-pan sound-3d)))
    
    ;; Cleanup
    (remove-sound-3d sound-3d)
    (unload-sound sound)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Basic 3D audio demo completed.~%"))

(defun demo-3d-audio-distance-models ()
  "Demonstrate different distance attenuation models"
  (format t "~%=== 3D Audio Distance Models Demo ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave))
         (sound-3d (create-sound-3d sound (vec3 1.0 0.0 0.0))))
    
    ;; Set distance parameters
    (set-sound-3d-distance-model sound-3d 1.0 10.0 1.0)
    (set-listener-position (vec3 0.0 0.0 0.0))
    
    ;; Test different distance models
    (dolist (model '(:linear :inverse-distance :exponential-distance))
      (set-3d-audio-distance-model model)
      (format t "~%Distance model: ~a~%" model)
      (format t "Distance | Volume~%")
      (format t "---------|-------~%")
      
      (loop for distance from 0.0 to 10.0 by 1.0 do
        (set-sound-3d-position sound-3d (vec3 distance 0.0 0.0))
        (format t "~8,1f | ~,3f~%" distance (sound-3d-calculated-volume sound-3d))))
    
    ;; Cleanup
    (remove-sound-3d sound-3d)
    (unload-sound sound)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Distance models demo completed.~%"))

(defun demo-3d-audio-directional ()
  "Demonstrate directional sound sources"
  (format t "~%=== 3D Audio Directional Demo ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave))
         (sound-3d (create-sound-3d sound (vec3 1.0 0.0 0.0))))
    
    ;; Set up directional cone (45° inner, 90° outer)
    (set-sound-3d-cone sound-3d 45.0 90.0 0.1 (vec3 1.0 0.0 0.0))
    (set-listener-position (vec3 5.0 0.0 0.0))
    
    (format t "Directional sound pointing towards +X~%")
    (format t "Listener positions around sound source:~%")
    (format t "Position | Angle | Gain~%")
    (format t "---------|-------|-----~%")
    
    ;; Test listener positions in a circle around the sound
    (loop for angle from 0.0 to 359.0 by 30.0 do
      (let* ((rad (degrees-to-radians angle))
             (x (* 5.0 (cos rad)))
             (z (* 5.0 (sin rad)))
             (pos (vec3 x 0.0 z)))
        (set-listener-position pos)
        (calculate-3d-audio-parameters sound-3d)
        (format t "(~4,1f,~4,1f) | ~3,0f° | ~,3f~%" 
                x z angle (sound-3d-calculated-volume sound-3d))))
    
    ;; Cleanup
    (remove-sound-3d sound-3d)
    (unload-sound sound)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Directional audio demo completed.~%"))

(defun demo-3d-audio-doppler ()
  "Demonstrate Doppler effect calculations"
  (format t "~%=== 3D Audio Doppler Effect Demo ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  (set-doppler-factor 1.0)
  
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave))
         (sound-3d (create-sound-3d sound (vec3 2.0 0.0 0.0))))
    
    (set-listener-position (vec3 0.0 0.0 0.0))
    (set-listener-velocity (vec3 0.0 0.0 0.0))
    
    (format t "Doppler effect with different sound velocities:~%")
    (format t "Sound Velocity | Doppler Factor~%")
    (format t "---------------|---------------~%")
    
    ;; Test different velocities
    (dolist (velocity (list (vec3 0.0 0.0 0.0)     ; Stationary
                           (vec3 10.0 0.0 0.0)     ; Moving towards
                           (vec3 -10.0 0.0 0.0)    ; Moving away
                           (vec3 0.0 10.0 0.0)     ; Moving perpendicular
                           (vec3 50.0 0.0 0.0)))   ; Fast approach
      (set-sound-3d-velocity sound-3d velocity)
      (let ((doppler (calculate-doppler-effect sound-3d)))
        (format t "(~4,1f,~4,1f,~4,1f) | ~,3f~%" 
                (vx velocity) (vy velocity) (vz velocity) doppler)))
    
    ;; Test with moving listener
    (format t "~%With moving listener (sound stationary):~%")
    (set-sound-3d-velocity sound-3d (vec3 0.0 0.0 0.0))
    (dolist (listener-vel (list (vec3 10.0 0.0 0.0) (vec3 -10.0 0.0 0.0)))
      (set-listener-velocity listener-vel)
      (let ((doppler (calculate-doppler-effect sound-3d)))
        (format t "Listener velocity (~4,1f,~4,1f,~4,1f) | ~,3f~%"
                (vx listener-vel) (vy listener-vel) (vz listener-vel) doppler)))
    
    ;; Cleanup
    (remove-sound-3d sound-3d)
    (unload-sound sound)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Doppler effect demo completed.~%"))

(defun demo-3d-audio-reverb ()
  "Demonstrate reverb zones and environmental audio"
  (format t "~%=== 3D Audio Reverb Zones Demo ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  
  ;; Create multiple reverb zones
  (let ((cave-zone (create-reverb-zone (vec3 0.0 0.0 0.0) (vec3 10.0 5.0 10.0) 0.8 2.5))
        (hall-zone (create-reverb-zone (vec3 20.0 0.0 0.0) (vec3 15.0 8.0 15.0) 0.6 1.8))
        (small-room (create-reverb-zone (vec3 -15.0 0.0 0.0) (vec3 5.0 3.0 5.0) 0.3 0.8)))
    
    (format t "Created reverb zones:~%")
    (format t "- Cave: High reverb (0.8), long decay (2.5s)~%")
    (format t "- Hall: Medium reverb (0.6), medium decay (1.8s)~%")
    (format t "- Small room: Low reverb (0.3), short decay (0.8s)~%")
    (format t "~%Listener movement through zones:~%")
    (format t "Position | Reverb Level~%")
    (format t "---------|-------------~%")
    
    ;; Test listener positions
    (dolist (pos (list (vec3 0.0 0.0 0.0)     ; Cave center
                      (vec3 5.0 0.0 0.0)      ; Cave edge
                      (vec3 12.0 0.0 0.0)     ; Between zones
                      (vec3 20.0 0.0 0.0)     ; Hall center
                      (vec3 -15.0 0.0 0.0)    ; Small room
                      (vec3 -30.0 0.0 0.0)))  ; Outside all zones
      (let ((reverb-level (calculate-reverb-effect pos)))
        (format t "(~4,1f,~4,1f,~4,1f) | ~,3f~%"
                (vx pos) (vy pos) (vz pos) reverb-level)))
    
    ;; Cleanup
    (remove-reverb-zone cave-zone)
    (remove-reverb-zone hall-zone)
    (remove-reverb-zone small-room))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Reverb zones demo completed.~%"))

(defun demo-3d-audio-camera-integration ()
  "Demonstrate integration with camera system"
  (format t "~%=== 3D Audio Camera Integration Demo ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  
  ;; Create a camera and sounds
  (let* ((camera (make-camera3d :position (vec3 0.0 5.0 10.0)
                                :target (vec3 0.0 0.0 0.0)
                                :up (vec3 0.0 1.0 0.0)
                                :fovy 45.0))
         (wave (create-dummy-wave))
         (sound1 (load-sound-from-wave wave))
         (sound2 (load-sound-from-wave wave))
         (sound-3d-1 (create-sound-3d sound1 (vec3 -5.0 0.0 0.0)))
         (sound-3d-2 (create-sound-3d sound2 (vec3 5.0 0.0 0.0))))
    
    (format t "Camera-based audio listener demo~%")
    (format t "Camera position: (~,1f, ~,1f, ~,1f)~%"
            (vx (camera3d-position camera))
            (vy (camera3d-position camera))
            (vz (camera3d-position camera)))
    
    ;; Update listener from camera
    (update-listener-from-camera camera)
    
    (format t "~%Audio positioning after camera update:~%")
    (format t "Sound 1 (left): Volume=~,3f, Pan=~,3f~%"
            (sound-3d-calculated-volume sound-3d-1)
            (sound-3d-calculated-pan sound-3d-1))
    (format t "Sound 2 (right): Volume=~,3f, Pan=~,3f~%"
            (sound-3d-calculated-volume sound-3d-2)
            (sound-3d-calculated-pan sound-3d-2))
    
    ;; Simulate camera movement
    (format t "~%Simulating camera movement:~%")
    (loop for z from 10.0 downto 0.0 by 2.0 do
      (setf (camera3d-position camera) (vec3 0.0 5.0 z))
      (update-listener-from-camera camera)
      (format t "Camera Z=~,1f: Sound1 Vol=~,3f, Sound2 Vol=~,3f~%"
              z
              (sound-3d-calculated-volume sound-3d-1)
              (sound-3d-calculated-volume sound-3d-2)))
    
    ;; Cleanup
    (remove-sound-3d sound-3d-1)
    (remove-sound-3d sound-3d-2)
    (unload-sound sound1)
    (unload-sound sound2)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "Camera integration demo completed.~%"))

(defun demo-3d-audio-system-info ()
  "Display 3D audio system information and capabilities"
  (format t "~%=== 3D Audio System Information ===~%")
  
  (init-audio-device)
  (init-3d-audio-system)
  
  ;; Create some test objects
  (let* ((wave (create-dummy-wave))
         (sound (load-sound-from-wave wave))
         (sound-3d (create-sound-3d sound (vec3 1.0 0.0 0.0)))
         (reverb-zone (create-reverb-zone (vec3 0.0 0.0 0.0) (vec3 5.0 5.0 5.0) 0.5 1.0)))
    
    (format t "System Information:~%")
    (format t "- Audio device ready: ~a~%" (is-audio-device-ready))
    (format t "- Master volume: ~,2f~%" (get-master-volume))
    (format t "- ~a~%" (get-3d-audio-info))
    (format t "- Distance model: ~a~%" *distance-model*)
    (format t "- Doppler factor: ~,2f~%" *doppler-factor*)
    (format t "- Speed of sound: ~,1f m/s~%" *speed-of-sound*)
    
    (format t "~%Available distance models:~%")
    (format t "- :linear~%")
    (format t "- :inverse-distance~%")
    (format t "- :exponential-distance~%")
    
    (format t "~%Supported features:~%")
    (format t "- 3D positioned sounds with distance attenuation~%")
    (format t "- Directional sound cones~%")
    (format t "- Doppler effect calculations~%")
    (format t "- Environmental reverb zones~%")
    (format t "- Camera integration for listener updates~%")
    (format t "- Real-time parameter updates~%")
    
    ;; Cleanup
    (remove-sound-3d sound-3d)
    (remove-reverb-zone reverb-zone)
    (unload-sound sound)
    (unload-wave wave))
  
  (cleanup-3d-audio-system)
  (close-audio-device)
  (format t "System information display completed.~%"))

(defun run-all-3d-audio-demos ()
  "Run all 3D audio demos in sequence"
  (format t "========================================~%")
  (format t "Pure-Raylib 3D Audio System Demo Suite~%")
  (format t "========================================~%")
  
  ;; Run all demos
  (demo-3d-audio-system-info)
  (demo-3d-audio-basic)
  (demo-3d-audio-distance-models)
  (demo-3d-audio-directional)
  (demo-3d-audio-doppler)
  (demo-3d-audio-reverb)
  (demo-3d-audio-camera-integration)
  
  (format t "~%========================================~%")
  (format t "All 3D Audio demos completed successfully!~%")
  (format t "========================================~%"))

;; Auto-run when loaded
(eval-when (:load-toplevel :execute)
           (run-all-3d-audio-demos))
