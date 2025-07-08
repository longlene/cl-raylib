;;;; Audio System Demo for cl-raylib
;;;; This demonstrates the audio system functionality

(require :cl-raylib)

(defpackage :cl-raylib-audio-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-audio-demo)

(defun audio-demo ()
  "Demonstrate audio system features"
  (let ((screen-width 1200)
        (screen-height 800))
    
    ;; Initialize logging system
    (set-trace-log-level +log-info+)
    (trace-log-info "Starting Audio System Demo")
    
    ;; Set window flags
    (set-window-state (logior +flag-window-resizable+ +flag-vsync-hint+))
    
    (with-window (screen-width screen-height "cl-raylib [audio] - Audio System Demo")
      (set-target-fps 60)
      
      ;; Initialize systems
      (init-texture-system)
      (init-text-system)
      (init-audio-device)
      
      (let* (;; Demo state
             (demo-mode 0) ; 0=device, 1=sounds, 2=music, 3=streams
             (mode-names '("Audio Device" "Sound Effects" "Music Streaming" "Audio Streams"))
             
             ;; Audio objects
             (test-sounds nil)
             (test-music nil)
             (test-stream nil)
             
             ;; Audio controls
             (master-volume 1.0)
             (sound-volume 1.0)
             (music-volume 1.0)
             (sound-pitch 1.0)
             (music-pitch 1.0)
             (sound-pan 0.0)
             
             ;; Visual elements
             (volume-bars nil)
             (waveform-points nil)
             (spectrum-bars nil)
             
             ;; Animation
             (time-counter 0.0)
             (beat-pulse 1.0))
        
        ;; Initialize audio content
        (setf test-sounds (list (create-dummy-wave) (create-dummy-wave) (create-dummy-wave)))
        (setf test-music (make-music :time-length 180.0 ; 3 minutes
                                    :volume 0.7
                                    :looping t))
        (setf test-stream (load-audio-stream 44100 16 2))
        
        ;; Initialize visual elements
        (setf volume-bars (loop for i from 0 to 9 collect (get-random-float 0.2 1.0)))
        (setf waveform-points (loop for i from 0 to 99 collect 
                               (* 50 (sin (* i 0.1)))))
        (setf spectrum-bars (loop for i from 0 to 15 collect (get-random-float 0.1 0.8)))
        
        (trace-log-info "Audio demo initialized")
        
        (loop until (window-should-close) do
          ;; Update
          (incf time-counter 0.016)
          (setf beat-pulse (+ 1.0 (* 0.2 (sin (* time-counter 4.0)))))
          
          ;; Update audio system
          (update-audio-system)
          
          ;; Handle input
          (when (is-key-pressed +key-tab+)
            (setf demo-mode (mod (1+ demo-mode) (length mode-names))))
          
          ;; Master volume controls
          (when (is-key-down +key-up+)
            (setf master-volume (clamp (+ master-volume 0.01) 0.0 1.0))
            (set-master-volume master-volume))
          
          (when (is-key-down +key-down+)
            (setf master-volume (clamp (- master-volume 0.01) 0.0 1.0))
            (set-master-volume master-volume))
          
          ;; Mode-specific controls
          (case demo-mode
            (1 (handle-sound-controls test-sounds))
            (2 (handle-music-controls test-music))
            (3 (handle-stream-controls test-stream)))
          
          ;; Update visual elements
          (update-visual-elements volume-bars waveform-points spectrum-bars time-counter)
          
          ;; Drawing
          (with-drawing
            (clear-background +raywhite+)
            
            ;; Draw title
            (draw-text "PURE-RAYLIB AUDIO SYSTEM DEMO" 20 20 24 +darkblue+)
            (draw-text (format nil "Mode: ~a (TAB to switch)" (nth demo-mode mode-names)) 20 50 16 +darkgray+)
            
            ;; Draw mode-specific content
            (case demo-mode
              (0 (draw-device-demo))
              (1 (draw-sounds-demo test-sounds volume-bars beat-pulse))
              (2 (draw-music-demo test-music waveform-points beat-pulse))
              (3 (draw-streams-demo test-stream spectrum-bars)))
            
            ;; Draw common controls and info
            (draw-audio-controls demo-mode)
            (draw-audio-visualizer volume-bars waveform-points spectrum-bars time-counter)
            
            ;; Draw system info
            (let ((info-x (- screen-width 350))
                  (info-y 20))
              (draw-text "Audio System:" info-x info-y 16 +darkgreen+)
              (draw-text (format nil "Device Ready: ~a" (is-audio-device-ready)) info-x (+ info-y 25) 12 +black+)
              (draw-text (format nil "Master Volume: ~,2f" (get-master-volume)) info-x (+ info-y 45) 12 +black+)
              (draw-text (format nil "Buffer Size: ~d samples" (get-audio-buffer-size)) info-x (+ info-y 65) 12 +black+)
              (draw-text (get-audio-system-info) info-x (+ info-y 85) 10 +gray+)
              (draw-text (format nil "FPS: ~d" (get-fps)) info-x (+ info-y 105) 12 +green+)))
        
        ;; Cleanup
        (when test-stream (unload-audio-stream test-stream))
        (cleanup-audio-system)
        (trace-log-info "Audio demo completed")
        (cleanup-text-system)
        (cleanup-texture-system))))

(defun handle-sound-controls (test-sounds)
  "Handle sound effect controls"
  (when (is-key-pressed +key-space+)
    (let ((sound (load-sound-from-wave (first test-sounds))))
      (when sound
        (play-sound sound)
        (trace-log-info "Playing test sound"))))
  
  (when (is-key-pressed +key-s+)
    (stop-all-sounds)
    (trace-log-info "All sounds stopped"))
  
  ;; Volume control with left/right arrows
  (when (is-key-down +key-left+)
    (setf sound-volume (clamp (- sound-volume 0.01) 0.0 1.0)))
  
  (when (is-key-down +key-right+)
    (setf sound-volume (clamp (+ sound-volume 0.01) 0.0 1.0))))

(defun handle-music-controls (test-music)
  "Handle music streaming controls"
  (when (is-key-pressed +key-p+)
    (if (is-music-stream-playing test-music)
      (pause-music-stream test-music)
      (play-music-stream test-music)))
  
  (when (is-key-pressed +key-s+)
    (stop-music-stream test-music))
  
  (when (is-key-pressed +key-l+)
    (set-music-looping test-music (not (music-looping test-music))))
  
  ;; Seek with left/right arrows
  (when (is-key-down +key-left+)
    (let ((new-time (max 0.0 (- (get-music-time-played test-music) 1.0))))
      (seek-music-stream test-music new-time)))
  
  (when (is-key-down +key-right+)
    (let ((new-time (min (get-music-time-length test-music) 
                        (+ (get-music-time-played test-music) 1.0))))
      (seek-music-stream test-music new-time))))

(defun handle-stream-controls (test-stream)
  "Handle audio stream controls"
  (when (is-key-pressed +key-p+)
    (if (is-audio-stream-playing test-stream)
      (pause-audio-stream test-stream)
      (play-audio-stream test-stream)))
  
  (when (is-key-pressed +key-s+)
    (stop-audio-stream test-stream))
  
  ;; Stream would need buffer updates here
  (when (is-audio-stream-processed test-stream)
    ;; Generate new audio data and update stream
    (update-audio-stream test-stream nil 1024)))

(defun update-visual-elements (volume-bars waveform-points spectrum-bars time-counter)
  "Update visual audio elements"
  ;; Update volume bars with animation
  (loop for i from 0 below (length volume-bars) do
    (let ((target (+ 0.5 (* 0.3 (sin (+ time-counter (* i 0.5))))))
          (current (nth i volume-bars))
          (speed 0.1))
      (setf (nth i volume-bars) (+ current (* speed (- target current))))))
  
  ;; Update waveform
  (loop for i from 0 below (length waveform-points) do
    (setf (nth i waveform-points) 
          (* 30 (sin (+ (* i 0.2) (* time-counter 3.0))))))
  
  ;; Update spectrum bars
  (loop for i from 0 below (length spectrum-bars) do
    (let ((target (+ 0.3 (* 0.5 (sin (+ time-counter (* i 0.8))))))
          (current (nth i spectrum-bars))
          (speed 0.05))
      (setf (nth i spectrum-bars) (+ current (* speed (- target current)))))))

(defun draw-device-demo ()
  "Draw audio device information"
  (let ((y-offset 100))
    (draw-text "AUDIO DEVICE MANAGEMENT" 20 y-offset 20 +darkblue+)
    
    ;; Device status
    (draw-text "Device Status:" 20 (+ y-offset 50) 16 +darkgreen+)
    (draw-text (format nil "Initialized: ~a" (is-audio-device-ready)) 40 (+ y-offset 80) 14 +black+)
    (draw-text (format nil "Backend: Pure Common Lisp" ) 40 (+ y-offset 100) 14 +black+)
    (draw-text (format nil "Sample Rate: 44100 Hz") 40 (+ y-offset 120) 14 +black+)
    (draw-text (format nil "Buffer Size: ~d samples" (get-audio-buffer-size)) 40 (+ y-offset 140) 14 +black+)
    (draw-text (format nil "Format: 16-bit Stereo") 40 (+ y-offset 160) 14 +black+)
    
    ;; Volume control
    (draw-text "Master Volume Control:" 20 (+ y-offset 200) 16 +darkgreen+)
    (let* ((volume (get-master-volume))
           (bar-x 40)
           (bar-y (+ y-offset 230))
           (bar-width 300)
           (bar-height 20)
           (fill-width (* bar-width volume)))
      
      (draw-rectangle bar-x bar-y bar-width bar-height +lightgray+)
      (draw-rectangle bar-x bar-y (round fill-width) bar-height +green+)
      (draw-rectangle-lines bar-x bar-y bar-width bar-height +black+)
      (draw-text (format nil "~,0f%" (* volume 100)) (+ bar-x bar-width 10) (+ bar-y 2) 14 +black+))
    
    ;; Audio architecture info
    (draw-text "Pure-Raylib Audio Architecture:" 20 (+ y-offset 280) 16 +darkgreen+)
    (let ((info-lines '("• Pure Common Lisp implementation"
                       "• Cross-platform audio abstraction"
                       "• Wave/Sound/Music/Stream support"
                       "• Multiple audio format compatibility"
                       "• Real-time audio processing framework"
                       "• 3D spatial audio capabilities (planned)"))
          (start-y (+ y-offset 310)))
      (loop for line in info-lines
            for i from 0 do
        (draw-text line 40 (+ start-y (* i 25)) 12 +black+)))
    
    ;; Comparison with raylib
    (draw-text "Compared to Native Raylib:" 20 (+ y-offset 480) 16 +darkgreen+)
    (let ((comparison '("✓ Same API interface and function names"
                       "✓ Compatible data structures and workflows"
                       "⚠ Simplified audio backend (no miniaudio dependency)"
                       "⚠ Audio formats loaded via abstraction layer"
                       "• Future: External audio library integration"))
          (start-y (+ y-offset 510)))
      (loop for line in comparison
            for i from 0 do
        (draw-text line 40 (+ start-y (* i 20)) 12 +darkblue+)))))

(defun draw-sounds-demo (test-sounds volume-bars beat-pulse)
  "Draw sound effects demonstration"
  (let ((y-offset 100))
    (draw-text "SOUND EFFECTS SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "SPACE: Play sound  S: Stop all  LEFT/RIGHT: Volume" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Sound slots
    (draw-text "Loaded Sounds:" 20 (+ y-offset 70) 16 +darkgreen+)
    (loop for sound in test-sounds
          for i from 0 do
      (let ((slot-y (+ y-offset 100 (* i 40)))
            (slot-color (if (= (mod i 2) 0) +lightgray+ +gray+)))
        (draw-rectangle 40 slot-y 300 30 slot-color)
        (draw-rectangle-lines 40 slot-y 300 30 +black+)
        (draw-text (format nil "Sound ~d: ~d frames, ~d Hz" 
                          (1+ i) (wave-frame-count sound) (wave-sample-rate sound))
                  45 (+ slot-y 8) 12 +black+)))
    
    ;; Volume visualization
    (draw-text "Volume Levels:" 20 (+ y-offset 250) 16 +darkgreen+)
    (loop for volume in volume-bars
          for i from 0 do
      (let* ((bar-x (+ 40 (* i 35)))
             (bar-y (+ y-offset 350))
             (bar-height (* volume 80 beat-pulse))
             (bar-color (if (> volume 0.7) +red+ (if (> volume 0.4) +orange+ +green+))))
        (draw-rectangle bar-x (- bar-y bar-height) 25 bar-height bar-color)
        (draw-rectangle-lines bar-x (- bar-y 80) 25 80 +black+)
        (draw-text (format nil "~d" i) (+ bar-x 8) (+ bar-y 10) 10 +black+)))
    
    ;; Sound properties
    (draw-text "Sound Properties:" 20 (+ y-offset 400) 16 +darkgreen+)
    (draw-text (format nil "Volume: ~,2f" sound-volume) 40 (+ y-offset 430) 14 +black+)
    (draw-text (format nil "Pitch: ~,2f" sound-pitch) 40 (+ y-offset 450) 14 +black+)
    (draw-text (format nil "Pan: ~,2f" sound-pan) 40 (+ y-offset 470) 14 +black+)
    
    ;; Sound features
    (draw-text "Features:" 400 (+ y-offset 70) 16 +darkgreen+)
    (let ((features '("• Multiple simultaneous sound playback"
                     "• Individual volume, pitch, and pan controls"
                     "• Sound caching and management"
                     "• WAV/OGG/MP3/FLAC format support"
                     "• Real-time parameter adjustment"
                     "• Memory efficient sound loading"))
          (start-y (+ y-offset 100)))
      (loop for feature in features
            for i from 0 do
        (draw-text feature 420 (+ start-y (* i 25)) 12 +black+)))))

(defun draw-music-demo (test-music waveform-points beat-pulse)
  "Draw music streaming demonstration"
  (let ((y-offset 100))
    (draw-text "MUSIC STREAMING SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "P: Play/Pause  S: Stop  L: Toggle loop  LEFT/RIGHT: Seek" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Music info
    (draw-text "Current Music:" 20 (+ y-offset 70) 16 +darkgreen+)
    (draw-rectangle 40 (+ y-offset 100) 500 80 +lightgray+)
    (draw-rectangle-lines 40 (+ y-offset 100) 500 80 +black+)
    (draw-text "Demo Music Track" 50 (+ y-offset 110) 16 +black+)
    (draw-text (format nil "Format: ~a" (music-ctx-type test-music)) 50 (+ y-offset 130) 12 +darkgray+)
    (draw-text (format nil "Status: ~a" (if (is-music-stream-playing test-music) "Playing" "Stopped")) 
              50 (+ y-offset 150) 12 +darkgray+)
    (draw-text (format nil "Looping: ~a" (music-looping test-music)) 50 (+ y-offset 165) 12 +darkgray+)
    
    ;; Progress bar
    (draw-text "Playback Progress:" 20 (+ y-offset 200) 16 +darkgreen+)
    (let* ((progress (/ (get-music-time-played test-music) (get-music-time-length test-music)))
           (bar-x 40)
           (bar-y (+ y-offset 230))
           (bar-width 500)
           (bar-height 20)
           (fill-width (* bar-width progress)))
      
      (draw-rectangle bar-x bar-y bar-width bar-height +lightgray+)
      (draw-rectangle bar-x bar-y (round fill-width) bar-height +blue+)
      (draw-rectangle-lines bar-x bar-y bar-width bar-height +black+)
      
      ;; Time display
      (draw-text (format nil "~,1fs / ~,1fs" 
                        (get-music-time-played test-music) 
                        (get-music-time-length test-music))
                (+ bar-x bar-width 10) (+ bar-y 2) 12 +black+))
    
    ;; Waveform visualization
    (draw-text "Waveform Visualization:" 20 (+ y-offset 280) 16 +darkgreen+)
    (let ((wave-center-y (+ y-offset 350))
          (wave-start-x 40))
      (draw-line wave-start-x wave-center-y (+ wave-start-x 600) wave-center-y +gray+)
      
      (loop for i from 0 below (1- (length waveform-points)) do
        (let* ((x1 (+ wave-start-x (* i 6)))
               (y1 (+ wave-center-y (nth i waveform-points)))
               (x2 (+ wave-start-x (* (1+ i) 6)))
               (y2 (+ wave-center-y (nth (1+ i) waveform-points)))
               (intensity (abs (/ (nth i waveform-points) 30.0)))
               (color (cond
                       ((> intensity 0.8) +red+)
                       ((> intensity 0.5) +orange+)
                       (t +lime+))))
          (draw-line x1 y1 x2 y2 color))))
    
    ;; Music controls
    (draw-text "Music Controls:" 20 (+ y-offset 400) 16 +darkgreen+)
    (draw-text (format nil "Volume: ~,2f" (music-volume test-music)) 40 (+ y-offset 430) 14 +black+)
    (draw-text (format nil "Pitch: ~,2f" (music-pitch test-music)) 40 (+ y-offset 450) 14 +black+)
    
    ;; Beat visualization
    (when (is-music-stream-playing test-music)
      (draw-circle 700 (+ y-offset 150) (* 30 beat-pulse) (color-alpha +cyan+ 0.5))
      (draw-text "♪" 695 (+ y-offset 145) 20 +blue+))))

(defun draw-streams-demo (test-stream spectrum-bars)
  "Draw audio streams demonstration"
  (let ((y-offset 100))
    (draw-text "AUDIO STREAMS SYSTEM" 20 y-offset 20 +darkblue+)
    (draw-text "P: Play/Pause  S: Stop" 20 (+ y-offset 30) 14 +gray+)
    
    ;; Stream info
    (draw-text "Audio Stream:" 20 (+ y-offset 70) 16 +darkgreen+)
    (draw-rectangle 40 (+ y-offset 100) 400 60 +lightgray+)
    (draw-rectangle-lines 40 (+ y-offset 100) 400 60 +black+)
    (draw-text (format nil "Sample Rate: ~d Hz" (audio-stream-sample-rate test-stream)) 50 (+ y-offset 110) 12 +black+)
    (draw-text (format nil "Channels: ~d" (audio-stream-channels test-stream)) 50 (+ y-offset 125) 12 +black+)
    (draw-text (format nil "Bit Depth: ~d-bit" (audio-stream-sample-size test-stream)) 50 (+ y-offset 140) 12 +black+)
    (draw-text (format nil "Status: ~a" (if (is-audio-stream-playing test-stream) "Streaming" "Stopped")) 
              250 (+ y-offset 110) 12 +darkgray+)
    
    ;; Spectrum analyzer
    (draw-text "Frequency Spectrum:" 20 (+ y-offset 180) 16 +darkgreen+)
    (loop for amplitude in spectrum-bars
          for i from 0 do
      (let* ((bar-x (+ 40 (* i 40)))
             (bar-y (+ y-offset 350))
             (bar-height (* amplitude 120))
             (frequency (* (1+ i) 1000)) ; Simulate frequency bands
             (bar-color (cond
                         ((< frequency 500) +red+)
                         ((< frequency 2000) +orange+)
                         ((< frequency 8000) +yellow+)
                         (t +lime+))))
        (draw-rectangle bar-x (- bar-y bar-height) 30 bar-height bar-color)
        (draw-rectangle-lines bar-x (- bar-y 120) 30 120 +black+)
        (draw-text (format nil "~,1fk" (/ frequency 1000.0)) (+ bar-x 5) (+ bar-y 10) 8 +black+)))
    
    ;; Stream features
    (draw-text "Stream Features:" 20 (+ y-offset 380) 16 +darkgreen+)
    (let ((features '("• Real-time audio buffer management"
                     "• Low-latency audio processing"
                     "• Custom audio data streaming"
                     "• Procedural audio generation"
                     "• Audio effects processing pipeline"))
          (start-y (+ y-offset 410)))
      (loop for feature in features
            for i from 0 do
        (draw-text feature 40 (+ start-y (* i 20)) 12 +black+)))
    
    ;; Buffer status
    (draw-text "Buffer Status:" 500 (+ y-offset 180) 16 +darkgreen+)
    (let ((buffer-bars 8))
      (loop for i from 0 below buffer-bars do
        (let* ((bar-x (+ 520 (* i 30)))
               (bar-y (+ y-offset 220))
               (filled (< i 6)) ; Simulate buffer fill
               (bar-color (if filled +green+ +red+)))
          (draw-rectangle bar-x bar-y 20 15 bar-color)
          (draw-rectangle-lines bar-x bar-y 20 15 +black+))))
    
    (draw-text "Buffer Fill Level" 520 (+ y-offset 250) 12 +gray+)))

(defun draw-audio-controls (demo-mode)
  "Draw audio control instructions"
  (let ((controls-y 720))
    (draw-text "Controls:" 20 controls-y 14 +darkblue+)
    (let ((controls (case demo-mode
                     (0 "TAB: Switch modes  UP/DOWN: Master volume")
                     (1 "TAB: Switch modes  SPACE: Play sound  S: Stop  LEFT/RIGHT: Volume")
                     (2 "TAB: Switch modes  P: Play/Pause  S: Stop  L: Loop  LEFT/RIGHT: Seek")
                     (3 "TAB: Switch modes  P: Play/Pause  S: Stop"))))
      (draw-text controls 20 (+ controls-y 20) 12 +gray+))))

(defun draw-audio-visualizer (volume-bars waveform-points spectrum-bars time-counter)
  "Draw audio visualization elements"
  (let ((viz-x 750)
        (viz-y 400))
    
    ;; Mini spectrum
    (draw-text "Audio Visualizer" viz-x viz-y 12 +darkblue+)
    (loop for i from 0 to 7 do
      (let* ((bar-height (* 30 (nth (mod i (length spectrum-bars)) spectrum-bars)))
             (x (+ viz-x (* i 15)))
             (y (+ viz-y 50)))
        (draw-rectangle x (- y bar-height) 10 bar-height +cyan+)
        (draw-rectangle-lines x (- y 30) 10 30 +black+)))
    
    ;; Mini waveform
    (let ((wave-y (+ viz-y 120)))
      (loop for i from 0 to 19 do
        (let* ((sample (nth (mod (* i 5) (length waveform-points)) waveform-points))
               (x (+ viz-x (* i 6)))
               (y (+ wave-y sample)))
          (draw-circle x y 2 +lime+))))))

;; Run the demo
(audio-demo)