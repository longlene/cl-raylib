(in-package #:cl-raylib)

;;;; Audio System Implementation
;;;; Based on raylib's raudio.c module

;;; Audio device state
(defvar *audio-device-ready* nil "Audio device initialization state")

;;; Audio Device Management Functions (matches raylib InitAudioDevice/CloseAudioDevice)

(defun init-audio-device ()
  "Initialize audio device and context (matches raylib InitAudioDevice)"
  ;; For now, we'll use a simple flag-based audio device simulation
  ;; In a full implementation, this would initialize miniaudio or similar
  (setf *audio-device-ready* t)
  (trace-log-info "AUDIO: Device initialized successfully"))

(defun close-audio-device ()
  "Close the audio device and context (matches raylib CloseAudioDevice)"
  (setf *audio-device-ready* nil)
  (trace-log-info "AUDIO: Device closed"))

(defun is-audio-device-ready ()
  "Check if audio device has been initialized successfully (matches raylib IsAudioDeviceReady)"
  *audio-device-ready*)

;;; Master Volume Control

(defvar *master-volume* 1.0 "Master audio volume (0.0 to 1.0)")

(defun set-master-volume (volume)
  "Set master volume (listener) (matches raylib SetMasterVolume)"
  (setf *master-volume* (clamp volume 0.0 1.0)))

(defun get-master-volume ()
  "Get master volume (listener) (matches raylib GetMasterVolume)"
  *master-volume*)

;;; Wave Functions

(defun create-dummy-wave ()
  "Create a dummy wave for testing purposes"
  (make-wave :frame-count 44100      ; 1 second at 44.1kHz
             :sample-rate 44100
             :sample-size 16          ; 16-bit
             :channels 1              ; Mono
             :data (make-array (* 44100 2) :element-type '(unsigned-byte 8) :initial-element 0)))

(defun load-wave (filename)
  "Load wave data from file (matches raylib LoadWave)"
  (declare (ignore filename))
  ;; Placeholder implementation - would load actual audio file
  (when *audio-device-ready*
    (trace-log-info "AUDIO: Wave loaded from file (placeholder): ~a" filename)
    (create-dummy-wave)))

(defun load-wave-from-memory (file-type file-data data-size)
  "Load wave from memory buffer (matches raylib LoadWaveFromMemory)"
  (declare (ignore file-type file-data data-size))
  ;; Placeholder implementation
  (when *audio-device-ready*
    (trace-log-info "AUDIO: Wave loaded from memory (placeholder)")
    (create-dummy-wave)))

(defun is-wave-valid (wave)
  "Checks if wave data is valid (matches raylib IsWaveValid)"
  (and wave
       (wave-data wave)
       (> (wave-frame-count wave) 0)
       (> (wave-sample-rate wave) 0)
       (> (wave-channels wave) 0)))

(defun unload-wave (wave)
  "Unload wave data (matches raylib UnloadWave)"
  (when (and wave (wave-data wave))
    (trace-log-info "AUDIO: Wave unloaded")
    ;; In a real implementation, would free the audio data
    (setf (wave-data wave) nil)))

;;; Sound Functions

(defun load-sound (filename)
  "Load sound from file (matches raylib LoadSound)"
  (declare (ignore filename))
  (when *audio-device-ready*
    (let ((wave (create-dummy-wave)))
      (when wave
        (trace-log-info "AUDIO: Sound loaded from file (placeholder): ~a" filename)
        (make-sound :stream nil ; Placeholder for audio stream
                    :frame-count (wave-frame-count wave))))))

(defun load-sound-from-wave (wave)
  "Load sound from wave data (matches raylib LoadSoundFromWave)"
  (when (and *audio-device-ready* (is-wave-valid wave))
    (trace-log-info "AUDIO: Sound loaded from wave data")
    (make-sound :stream nil ; Placeholder for audio stream
                :frame-count (wave-frame-count wave))))

(defun is-sound-valid (sound)
  "Checks if a sound is valid (matches raylib IsSoundValid)"
  (and sound
       (> (sound-frame-count sound) 0)))

(defun unload-sound (sound)
  "Unload sound (matches raylib UnloadSound)"
  (when sound
    (trace-log-info "AUDIO: Sound unloaded")
    ;; In a real implementation, would free audio buffers
    (setf (sound-stream sound) nil)))

(defun play-sound (sound)
  "Play a sound (matches raylib PlaySound)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (trace-log-info "AUDIO: Playing sound")))

(defun stop-sound (sound)
  "Stop playing a sound (matches raylib StopSound)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (trace-log-info "AUDIO: Stopping sound")))

(defun pause-sound (sound)
  "Pause a sound (matches raylib PauseSound)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (trace-log-info "AUDIO: Pausing sound")))

(defun resume-sound (sound)
  "Resume a paused sound (matches raylib ResumeSound)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (trace-log-info "AUDIO: Resuming sound")))

(defun is-sound-playing (sound)
  "Check if a sound is currently playing (matches raylib IsSoundPlaying)"
  (declare (ignore sound))
  ;; Placeholder - would check actual playback state
  nil)

(defun set-sound-volume (sound volume)
  "Set volume for a sound (matches raylib SetSoundVolume)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (let ((clamped-volume (clamp volume 0.0 1.0)))
      (trace-log-info "AUDIO: Sound volume set to ~,2f" clamped-volume))))

(defun set-sound-pitch (sound pitch)
  "Set pitch for a sound (matches raylib SetSoundPitch)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (trace-log-info "AUDIO: Sound pitch set to ~,2f" pitch)))

(defun set-sound-pan (sound pan)
  "Set pan for a sound (matches raylib SetSoundPan)"
  (when (and *audio-device-ready* (is-sound-valid sound))
    (let ((clamped-pan (clamp pan -1.0 1.0)))
      (trace-log-info "AUDIO: Sound pan set to ~,2f" clamped-pan))))

;;; Music Functions

(defun load-music-stream (filename)
  "Load music stream from file (matches raylib LoadMusicStream)"
  (declare (ignore filename))
  (when *audio-device-ready*
    (trace-log-info "AUDIO: Music stream loaded from file (placeholder): ~a" filename)
    (make-music :stream nil ; Placeholder for audio stream
                :frame-count 0
                :looping nil
                :ctx-type 0
                :ctx-data nil)))

(defun is-music-valid (music)
  "Checks if a music stream is valid (matches raylib IsMusicValid)"
  (and music
       (music-stream music)))

(defun unload-music-stream (music)
  "Unload music stream (matches raylib UnloadMusicStream)"
  (when music
    (trace-log-info "AUDIO: Music stream unloaded")
    (setf (music-stream music) nil)))

(defun play-music-stream (music)
  "Start music playing (matches raylib PlayMusicStream)"
  (when (and *audio-device-ready* (is-music-valid music))
    (trace-log-info "AUDIO: Playing music stream")))

(defun is-music-stream-playing (music)
  "Check if music is playing (matches raylib IsMusicStreamPlaying)"
  (declare (ignore music))
  ;; Placeholder - would check actual playback state
  nil)

(defun update-music-stream (music)
  "Updates buffers for music streaming (matches raylib UpdateMusicStream)"
  (when (and *audio-device-ready* (is-music-valid music))
    ;; Placeholder - would update streaming buffers
    nil))

(defun stop-music-stream (music)
  "Stop music playing (matches raylib StopMusicStream)"
  (when (and *audio-device-ready* (is-music-valid music))
    (trace-log-info "AUDIO: Stopping music stream")))

(defun pause-music-stream (music)
  "Pause music playing (matches raylib PauseMusicStream)"
  (when (and *audio-device-ready* (is-music-valid music))
    (trace-log-info "AUDIO: Pausing music stream")))

(defun resume-music-stream (music)
  "Resume playing paused music (matches raylib ResumeMusicStream)"
  (when (and *audio-device-ready* (is-music-valid music))
    (trace-log-info "AUDIO: Resuming music stream")))

(defun set-music-volume (music volume)
  "Set volume for music (matches raylib SetMusicVolume)"
  (when (and *audio-device-ready* (is-music-valid music))
    (let ((clamped-volume (clamp volume 0.0 1.0)))
      (trace-log-info "AUDIO: Music volume set to ~,2f" clamped-volume))))

(defun set-music-pitch (music pitch)
  "Set pitch for a music (matches raylib SetMusicPitch)"
  (when (and *audio-device-ready* (is-music-valid music))
    (trace-log-info "AUDIO: Music pitch set to ~,2f" pitch)))

(defun set-music-pan (music pan)
  "Set pan for a music (matches raylib SetMusicPan)"
  (when (and *audio-device-ready* (is-music-valid music))
    (let ((clamped-pan (clamp pan -1.0 1.0)))
      (trace-log-info "AUDIO: Music pan set to ~,2f" clamped-pan))))

(defun get-music-time-length (music)
  "Get music time length in seconds (matches raylib GetMusicTimeLength)"
  (declare (ignore music))
  ;; Placeholder - would return actual music length
  0.0)

(defun get-music-time-played (music)
  "Get current music time played in seconds (matches raylib GetMusicTimePlayed)"
  (declare (ignore music))
  ;; Placeholder - would return actual playback position
  0.0)

;;; 3D Audio System Implementation

;;; 3D Audio global state
(defvar *3d-audio-initialized* nil "3D audio system initialization state")
(defvar *listener-position* (vec3 0.0 0.0 0.0) "3D listener position")
(defvar *listener-forward* (vec3 0.0 0.0 -1.0) "3D listener forward direction") 
(defvar *listener-up* (vec3 0.0 1.0 0.0) "3D listener up direction")
(defvar *listener-velocity* (vec3 0.0 0.0 0.0) "3D listener velocity for Doppler effect")
(defvar *distance-model* :inverse-distance "Distance attenuation model")
(defvar *doppler-factor* 1.0 "Doppler effect factor")
(defvar *speed-of-sound* 343.3 "Speed of sound in m/s")
(defvar *3d-sounds* (make-hash-table :test 'equal) "Active 3D sounds")

;;; Sound-3D structure (extends Sound with 3D properties)
(defstruct sound-3d
  "3D positioned sound with spatial audio properties"
  sound                     ; Base Sound object
  position                  ; vec3 - 3D position
  velocity                  ; vec3 - velocity for Doppler effect
  min-distance             ; float - minimum distance for attenuation
  max-distance             ; float - maximum distance (volume = 0)
  rolloff-factor           ; float - rolloff rate for distance attenuation
  cone-inner-angle         ; float - inner cone angle in degrees
  cone-outer-angle         ; float - outer cone angle in degrees
  cone-outer-gain          ; float - gain outside the outer cone
  cone-direction           ; vec3 - cone direction
  calculated-volume        ; float - calculated volume based on position
  calculated-pan)          ; float - calculated pan based on position

;;; 3D Audio System Management

(defun init-3d-audio-system ()
  "Initialize 3D audio system"
  (when *audio-device-ready*
    (setf *3d-audio-initialized* t)
    (setf *listener-position* (vec3 0.0 0.0 0.0))
    (setf *listener-forward* (vec3 0.0 0.0 -1.0))
    (setf *listener-up* (vec3 0.0 1.0 0.0))
    (setf *listener-velocity* (vec3 0.0 0.0 0.0))
    (clrhash *3d-sounds*)
    (trace-log-info "AUDIO: 3D audio system initialized")))

(defun cleanup-3d-audio-system ()
  "Cleanup 3D audio system"
  (when *3d-audio-initialized*
    (clrhash *3d-sounds*)
    (setf *3d-audio-initialized* nil)
    (trace-log-info "AUDIO: 3D audio system cleaned up")))

(defun get-3d-audio-info ()
  "Get 3D audio system information"
  (if *3d-audio-initialized*
    (format nil "3D Audio System: ACTIVE (~d sounds active, listener at ~a)"
            (hash-table-count *3d-sounds*)
            *listener-position*)
    "3D Audio System: INACTIVE"))

;;; Listener Functions

(defun set-listener-position (position)
  "Set 3D audio listener position"
  (when *3d-audio-initialized*
    (setf *listener-position* (vcopy3 position))
    (update-all-3d-audio-parameters)))

(defun set-listener-orientation (forward up)
  "Set 3D audio listener orientation"
  (when *3d-audio-initialized*
    (setf *listener-forward* (vunit forward))
    (setf *listener-up* (vunit up))
    (update-all-3d-audio-parameters)))

(defun set-listener-velocity (velocity)
  "Set 3D audio listener velocity for Doppler effect"
  (when *3d-audio-initialized*
    (setf *listener-velocity* (vcopy3 velocity))))

(defun update-listener-from-camera (camera)
  "Update listener position and orientation from camera"
  (when *3d-audio-initialized*
    (setf *listener-position* (camera3d-position camera))
    (let ((target (camera3d-target camera)))
      (setf *listener-forward* (vunit (v- target *listener-position*))))
    (setf *listener-up* (camera3d-up camera))
    (update-all-3d-audio-parameters)))

;;; 3D Sound Management

(defun create-sound-3d (sound position)
  "Create a 3D positioned sound"
  (when (and *3d-audio-initialized* (is-sound-valid sound))
    (let ((sound-3d (make-sound-3d :sound sound
                                   :position (vcopy3 position)
                                   :velocity (vec3 0.0 0.0 0.0)
                                   :min-distance 1.0
                                   :max-distance 10.0
                                   :rolloff-factor 1.0
                                   :cone-inner-angle 360.0
                                   :cone-outer-angle 360.0
                                   :cone-outer-gain 1.0
                                   :cone-direction (vec3 0.0 0.0 -1.0)
                                   :calculated-volume 1.0
                                   :calculated-pan 0.0))
          (sound-id (format nil "sound-~a" (random 100000))))
      (setf (gethash sound-id *3d-sounds*) sound-3d)
      (calculate-3d-audio-parameters sound-3d)
      (trace-log-info "AUDIO: 3D sound created at position ~a" position)
      sound-3d)))

(defun remove-sound-3d (sound-3d)
  "Remove a 3D sound from the system"
  (when (and *3d-audio-initialized* sound-3d)
    (loop for key being the hash-keys of *3d-sounds*
          for value being the hash-values of *3d-sounds*
          when (eq value sound-3d)
          do (remhash key *3d-sounds*)
             (trace-log-info "AUDIO: 3D sound removed")
             (return))))

(defun set-sound-3d-position (sound-3d position)
  "Set 3D sound position"
  (when (and *3d-audio-initialized* sound-3d)
    (setf (sound-3d-position sound-3d) (vcopy3 position))
    (calculate-3d-audio-parameters sound-3d)))

(defun set-sound-3d-velocity (sound-3d velocity)
  "Set 3D sound velocity for Doppler effect"
  (when (and *3d-audio-initialized* sound-3d)
    (setf (sound-3d-velocity sound-3d) (vcopy3 velocity))))

(defun set-sound-3d-distance-model (sound-3d min-distance max-distance rolloff-factor)
  "Set distance parameters for 3D sound"
  (when (and *3d-audio-initialized* sound-3d)
    (setf (sound-3d-min-distance sound-3d) min-distance)
    (setf (sound-3d-max-distance sound-3d) max-distance)
    (setf (sound-3d-rolloff-factor sound-3d) rolloff-factor)
    (calculate-3d-audio-parameters sound-3d)))

(defun set-sound-3d-cone (sound-3d inner-angle outer-angle outer-gain direction)
  "Set directional cone parameters for 3D sound"
  (when (and *3d-audio-initialized* sound-3d)
    (setf (sound-3d-cone-inner-angle sound-3d) inner-angle)
    (setf (sound-3d-cone-outer-angle sound-3d) outer-angle)
    (setf (sound-3d-cone-outer-gain sound-3d) outer-gain)
    (setf (sound-3d-cone-direction sound-3d) (vunit direction))
    (calculate-3d-audio-parameters sound-3d)))

;;; 3D Audio Calculation Functions

(defun calculate-3d-audio-parameters (sound-3d)
  "Calculate volume and pan for 3D sound based on listener position"
  (when (and *3d-audio-initialized* sound-3d)
    (let* ((sound-pos (sound-3d-position sound-3d))
           (listener-pos *listener-position*)
           (distance (vlength (v- sound-pos listener-pos))))
      
      ;; Handle case where sound is at listener position
      (if (< distance 0.001) ; Use small epsilon to avoid division by zero
          (progn
            (setf (sound-3d-calculated-volume sound-3d) 
                  (calculate-distance-attenuation distance sound-3d))
            (setf (sound-3d-calculated-pan sound-3d) 0.0)) ; Centered pan
          (let* ((direction (vunit (v- sound-pos listener-pos)))
                 (volume (calculate-distance-attenuation distance sound-3d))
                 (cone-gain (calculate-cone-attenuation sound-3d direction))
                 (pan (calculate-stereo-pan direction)))
            
            (setf (sound-3d-calculated-volume sound-3d) (* volume cone-gain))
            (setf (sound-3d-calculated-pan sound-3d) pan))))))

(defun calculate-distance-attenuation (distance sound-3d)
  "Calculate volume attenuation based on distance"
  (let ((min-dist (sound-3d-min-distance sound-3d))
        (max-dist (sound-3d-max-distance sound-3d))
        (rolloff (sound-3d-rolloff-factor sound-3d)))
    (cond
      ((<= distance min-dist) 1.0)
      ((>= distance max-dist) 0.0)
      (t (case *distance-model*
           (:linear
            (- 1.0 (* rolloff (/ (- distance min-dist) (- max-dist min-dist)))))
           (:inverse-distance
            (/ min-dist (+ min-dist (* rolloff (- distance min-dist)))))
           (:exponential-distance
            (expt (/ distance min-dist) (- rolloff)))
           (t 1.0))))))

(defun calculate-cone-attenuation (sound-3d direction)
  "Calculate volume attenuation based on directional cone"
  (let* ((cone-dir (sound-3d-cone-direction sound-3d))
         (inner-angle (sound-3d-cone-inner-angle sound-3d))
         (outer-angle (sound-3d-cone-outer-angle sound-3d))
         (outer-gain (sound-3d-cone-outer-gain sound-3d))
         (angle (radians-to-degrees (acos (v. cone-dir direction)))))
    (cond
      ((<= angle (/ inner-angle 2.0)) 1.0)
      ((>= angle (/ outer-angle 2.0)) outer-gain)
      (t (+ outer-gain 
            (* (- 1.0 outer-gain)
               (/ (- (/ outer-angle 2.0) angle)
                  (- (/ outer-angle 2.0) (/ inner-angle 2.0)))))))))

(defun calculate-stereo-pan (direction)
  "Calculate stereo pan based on direction relative to listener"
  (let* ((right (vc *listener-forward* *listener-up*))
         (pan-factor (v. direction right)))
    (clamp pan-factor -1.0 1.0)))

(defun update-all-3d-audio-parameters ()
  "Update audio parameters for all active 3D sounds"
  (when *3d-audio-initialized*
    (loop for sound-3d being the hash-values of *3d-sounds*
          do (calculate-3d-audio-parameters sound-3d))))

;;; Distance Model Management

(defun set-3d-audio-distance-model (model)
  "Set global distance attenuation model"
  (when *3d-audio-initialized*
    (setf *distance-model* model)
    (update-all-3d-audio-parameters)))

;;; Doppler Effect

(defun set-doppler-factor (factor)
  "Set Doppler effect factor"
  (when *3d-audio-initialized*
    (setf *doppler-factor* factor)))

(defun calculate-doppler-effect (sound-3d)
  "Calculate Doppler effect for 3D sound"
  (when (and *3d-audio-initialized* sound-3d)
    (let* ((sound-pos (sound-3d-position sound-3d))
           (sound-vel (sound-3d-velocity sound-3d))
           (listener-pos *listener-position*)
           (listener-vel *listener-velocity*)
           (distance (vlength (v- sound-pos listener-pos))))
      
      ;; Handle case where sound is at listener position
      (if (< distance 0.001)
          1.0 ; No Doppler effect at same position
          (let* ((direction (vunit (v- sound-pos listener-pos)))
                 (sound-speed (v. sound-vel direction))
                 (listener-speed (v. listener-vel direction))
                 (relative-speed (- sound-speed listener-speed))
                 (denominator (+ *speed-of-sound* relative-speed)))
            ;; Avoid division by zero
            (if (< (abs denominator) 0.001)
                1.0 ; Return unity factor when denominator is near zero
                (* *doppler-factor*
                   (/ (+ *speed-of-sound* listener-speed)
                      denominator))))))))

;;; Reverb System (simplified)

(defstruct reverb-zone
  "3D reverb zone for environmental audio"
  position         ; vec3 - center position
  size            ; vec3 - zone dimensions
  reverb-level    ; float - reverb amount (0.0 to 1.0)
  decay-time)     ; float - reverb decay time in seconds

(defvar *reverb-zones* (make-hash-table :test 'equal) "Active reverb zones")

(defun create-reverb-zone (position size reverb-level decay-time)
  "Create a reverb zone"
  (when *3d-audio-initialized*
    (let ((zone (make-reverb-zone :position (vcopy3 position)
                                  :size (vcopy3 size)
                                  :reverb-level reverb-level
                                  :decay-time decay-time))
          (zone-id (format nil "reverb-~a" (random 100000))))
      (setf (gethash zone-id *reverb-zones*) zone)
      (trace-log-info "AUDIO: Reverb zone created at ~a" position)
      zone)))

(defun remove-reverb-zone (zone)
  "Remove a reverb zone"
  (when (and *3d-audio-initialized* zone)
    (loop for key being the hash-keys of *reverb-zones*
          for value being the hash-values of *reverb-zones*
          when (eq value zone)
          do (remhash key *reverb-zones*)
             (trace-log-info "AUDIO: Reverb zone removed")
             (return))))

(defun calculate-reverb-effect (listener-pos)
  "Calculate reverb effect based on listener position"
  (let ((max-reverb 0.0))
    (loop for zone being the hash-values of *reverb-zones*
          for zone-pos = (reverb-zone-position zone)
          for zone-size = (reverb-zone-size zone)
          for distance = (vlength (v- listener-pos zone-pos))
          for max-distance = (* 0.5 (vlength zone-size))
          when (<= distance max-distance)
          do (let ((influence (- 1.0 (/ distance max-distance))))
               (setf max-reverb (max max-reverb 
                                    (* (reverb-zone-reverb-level zone) influence)))))
    max-reverb))

;;; Note: sound-3d-calculated-volume and sound-3d-calculated-pan are automatically
;;; generated by defstruct and don't need to be defined manually
