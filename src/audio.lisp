(in-package #:cl-raylib)

;;;===================================================================================
;;; raudio - A simple and easy-to-use audio library based on miniaudio
;;; Port of raylib/src/raudio.c
;;;
;;; FEATURES:
;;;   - Manage audio device (init/close)
;;;   - Manage raw audio context
;;;   - Manage mixing channels
;;;   - Load and unload audio files
;;;   - Format wave data (sample rate, size, channels)
;;;   - Play/Stop/Pause/Resume loaded audio
;;;
;;; Supported file formats: WAV (wav.lisp), OGG (vorbis.lisp), QOA (qoa.lisp), FLAC (flac.lisp)
;;; NOTE: SUPPORT_FILEFORMAT_FLAC is enabled (disabled by default in raylib config.h)
;;; NOTE: MP3, XM and MOD formats are not supported yet
;;; NOTE: Playback device uses PulseAudio through libpulse-simple (see miniaudio.lisp)
;;;
;;; NOTE: Sample data is stored in typed arrays: u8 -> (unsigned-byte 8), s16 -> (signed-byte 16),
;;; f32 -> single-float. Functions receiving raw sample data (UpdateSound(), UpdateAudioStream())
;;; accept a typed array, a byte vector (copied as raw bytes) or a foreign pointer
;;; NOTE: AudioCallback functions are Lisp functions called as (funcall callback buffer frames),
;;; BUFFER is a typed array in the stream internal format (stream callback) or a single-float
;;; array (processors) with FRAMES frames starting at index 0
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(defconstant +audio-device-format+ +ma-format-f32+ "Device output format (float-32bit)")
(defconstant +audio-device-channels+ 2 "Device output channels: stereo")
(defconstant +audio-device-sample-rate+ 0 "Device output sample rate")
(defconstant +audio-device-period-size-in-frames+ 0 "Device latency. 0 uses the backend default")
(defconstant +max-audio-buffer-pool-channels+ 16 "Audio pool channels")
(defconstant +audio-buffer-residual-capacity+ 8 "In PCM frames, for resampling and pitch shifting")

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

;; Music context type
;; NOTE: Depends on data structure provided by the library in charge of loading
(defconstant +music-audio-none+ 0 "No audio context loaded")
(defconstant +music-audio-wav+ 1 "WAV audio context")
(defconstant +music-audio-ogg+ 2 "OGG audio context")
(defconstant +music-audio-flac+ 3 "FLAC audio context")
(defconstant +music-audio-mp3+ 4 "MP3 audio context")
(defconstant +music-audio-qoa+ 5 "QOA audio context")
(defconstant +music-module-xm+ 6 "XM module audio context")
(defconstant +music-module-mod+ 7 "MOD module audio context")

;; Audio buffer usage
(defconstant +audio-buffer-usage-static+ 0)
(defconstant +audio-buffer-usage-stream+ 1)

;; Audio buffer struct
(defstruct (r-audio-buffer (:constructor %make-r-audio-buffer))
  (converter nil)                               ; Audio data converter
  (converter-residual nil)                      ; Cached residual input frames for use by the converter
  (converter-residual-count 0 :type fixnum)     ; The number of valid frames sitting in converterResidual
  (callback nil)                                ; Audio buffer callback for buffer filling on audio threads
  (processor nil :type list)                    ; Audio processors (in order)
  (volume 1f0 :type single-float)               ; Audio buffer volume
  (pitch 1f0 :type single-float)                ; Audio buffer pitch
  (pan 0f0 :type single-float)                  ; Audio buffer pan (-1.0f to 1.0f)
  (playing nil)                                 ; Audio buffer state: AUDIO_PLAYING
  (paused nil)                                  ; Audio buffer state: AUDIO_PAUSED
  (looping nil)                                 ; Audio buffer looping, default to true for AudioStreams
  (usage 0 :type fixnum)                        ; Audio buffer usage mode: STATIC or STREAM
  (is-sub-buffer-processed (make-array 2 :initial-element t))  ; SubBuffer processed (virtual double buffer)
  (size-in-frames 0 :type fixnum)               ; Total buffer size in frames
  (frame-cursor-pos 0 :type fixnum)             ; Frame cursor position
  (frames-processed 0 :type fixnum)             ; Total frames processed in this buffer (required for play timing)
  (data nil)                                    ; Data buffer, on music stream keeps filling
  (input-buffer nil)                            ; Temp buffer to read frames in internal format (mixing)
  (next nil)                                    ; Next audio buffer on the list
  (prev nil))                                   ; Previous audio buffer on the list

;; Audio data context
(defstruct (audio-data (:constructor %make-audio-data))
  ;; System
  (device nil)                                  ; miniaudio device
  (lock (bt:make-lock "raylib audio"))          ; miniaudio mutex lock
  (is-ready nil)                                ; Check if audio device is ready
  (pcm-buffer-size 0 :type fixnum)              ; Pre-allocated buffer size (in samples)
  (pcm-buffer nil)                              ; Pre-allocated buffer to read audio data from file/memory
  (master-volume 0f0 :type single-float)        ; Master volume while no device is initialized
  ;; Buffer
  (first nil)                                   ; Pointer to first AudioBuffer in the list
  (last nil)                                    ; Pointer to last AudioBuffer in the list
  (default-size 0 :type fixnum)                 ; Default audio buffer size for audio streams
  (mixed-processor nil :type list)              ; Audio processors applied to the mixed output
  ;; Mixing temp buffer (frames for stereo)
  (temp-buffer (make-array 1024 :element-type 'single-float :initial-element 0f0)))

;;;----------------------------------------------------------------------------------
;;; Global Variables Definition
;;;----------------------------------------------------------------------------------
(defvar *audio* (%make-audio-data) "Global AUDIO context")

(defmacro %with-audio-lock (&body body)
  `(bt:with-lock-held ((audio-data-lock *audio*))
     ,@body))

(declaim (inline %audio-device-sample-rate %audio-device-channels))
(defun %audio-device-sample-rate ()
  (let ((device (audio-data-device *audio*)))
    (if device (ma-device-sample-rate device) 0)))

(defun %audio-device-channels ()
  (let ((device (audio-data-device *audio*)))
    (if device (ma-device-channels device) 0)))

(defun %audio-format-from-sample-size (sample-size)
  (cond ((= sample-size 8) +ma-format-u8+)
        ((= sample-size 16) +ma-format-s16+)
        (t +ma-format-f32+)))

;; Get the raw bytes of sample data (native little-endian layout)
(defun %audio-data-bytes (data &optional (sample-count (length data)))
  (etypecase data
    ((simple-array (unsigned-byte 8) (*)) (subseq data 0 sample-count))
    ((simple-array (signed-byte 16) (*))
     (let ((bytes (make-array (* sample-count 2) :element-type '(unsigned-byte 8))))
       (dotimes (i sample-count bytes)
         (let ((x (logand (aref data i) #xffff)))
           (setf (aref bytes (* i 2)) (ldb (byte 8 0) x)
                 (aref bytes (+ (* i 2) 1)) (ldb (byte 8 8) x))))))
    ((simple-array single-float (*))
     (let ((bytes (make-array (* sample-count 4) :element-type '(unsigned-byte 8))))
       (dotimes (i sample-count bytes)
         (let ((x (ieee-floats:encode-float32 (aref data i))))
           (dotimes (k 4)
             (setf (aref bytes (+ (* i 4) k)) (ldb (byte 8 (* k 8)) x)))))))))

;; Copy SAMPLE-COUNT samples into DST (typed array) at DST-START, memcpy() equivalent
;; SRC can be a typed array of the same format, any other array (copied as raw bytes
;; when specialized, or element by element), a list or a foreign pointer
(defun %audio-copy-samples (dst dst-start src sample-count)
  (let ((format (%ma-buffer-format dst)))
    (cond
      ((cffi:pointerp src)
       (let ((type (ecase format (1 :uint8) (2 :int16) (5 :float))))
         (dotimes (i sample-count)
           (setf (aref dst (+ dst-start i)) (cffi:mem-aref src type i)))))
      ((and (typep src '%ma-buffer) (= (%ma-buffer-format src) format))
       (replace dst src :start1 dst-start :end2 (min (length src) sample-count)))
      ((typep src '%ma-buffer)
       ;; Raw byte copy between different formats
       (let* ((bytes (%audio-data-bytes src))
              (bytes-per-sample (ma-get-bytes-per-sample format)))
         (dotimes (i (min sample-count (floor (length bytes) bytes-per-sample)))
           (let ((b (* i bytes-per-sample)))
             (setf (aref dst (+ dst-start i))
                   (ecase format
                     (1 (aref bytes b))
                     (2 (%s16 (%u16le bytes b)))
                     (5 (ieee-floats:decode-float32 (%u32le bytes b)))))))))
      (t
       (let ((i 0))
         (map nil (lambda (x)
                    (when (< i sample-count)
                      (setf (aref dst (+ dst-start i))
                            (ecase format
                              (1 (logand (round x) #xff))
                              (2 (%i16 (round x)))
                              (5 (coerce x 'single-float))))
                      (incf i)))
              src))))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Audio Device initialization and Closing
;;;----------------------------------------------------------------------------------

;; Initialize audio device
(defun init-audio-device ()
  "Initialize audio device and context"
  ;; Init audio device
  ;; NOTE: Using the default device. Format is floating point because it simplifies mixing
  (let ((device (ma-device-init :format +audio-device-format+
                                :channels +audio-device-channels+
                                :sample-rate +audio-device-sample-rate+
                                :period-size-in-frames +audio-device-period-size-in-frames+
                                :data-callback #'%on-send-audio-data-to-device)))
    (unless device
      (trace-log +log-warning+ "AUDIO: Failed to initialize playback device")
      (return-from init-audio-device nil))
    (setf (audio-data-device *audio*) device)
    ;; Keep the device running the whole time
    (unless (ma-device-start device)
      (trace-log +log-warning+ "AUDIO: Failed to start playback device")
      (ma-device-uninit device)
      (setf (audio-data-device *audio*) nil)
      (return-from init-audio-device nil))
    (trace-log +log-info+ "AUDIO: Device initialized successfully")
    (trace-log +log-info+ "    > Backend:       PulseAudio (libpulse-simple)")
    (trace-log +log-info+ "    > Format:        ~a -> ~a" (ma-get-format-name (ma-device-format device))
               (ma-get-format-name (ma-device-internal-format device)))
    (trace-log +log-info+ "    > Channels:      ~d -> ~d" (ma-device-channels device) (ma-device-internal-channels device))
    (trace-log +log-info+ "    > Sample rate:   ~d -> ~d" (ma-device-sample-rate device) (ma-device-internal-sample-rate device))
    (trace-log +log-info+ "    > Periods size:  ~d" (* (ma-device-internal-period-size-in-frames device)
                                                       (ma-device-internal-periods device)))
    (setf (audio-data-is-ready *audio*) t)))

;; Close the audio device for all contexts
(defun close-audio-device ()
  "Close the audio device and context"
  (if (audio-data-is-ready *audio*)
      (progn
        ;; Stop the device first (joins the callback thread)
        (ma-device-uninit (audio-data-device *audio*))
        (setf (audio-data-device *audio*) nil
              (audio-data-is-ready *audio*) nil
              (audio-data-pcm-buffer *audio*) nil
              (audio-data-pcm-buffer-size *audio*) 0)
        (trace-log +log-info+ "AUDIO: Device closed successfully"))
      (trace-log +log-warning+ "AUDIO: Device could not be closed, not currently initialized")))

;; Check if device has been initialized successfully
(defun is-audio-device-ready ()
  "Check if audio device has been initialized successfully"
  (audio-data-is-ready *audio*))

;; Set master volume (listener)
(defun set-master-volume (volume)
  "Set master volume (listener)"
  (let ((device (audio-data-device *audio*)))
    (if device
        (ma-device-set-master-volume device volume)
        (when (>= volume 0) (setf (audio-data-master-volume *audio*) (coerce volume 'single-float))))))

;; Get master volume (listener)
(defun get-master-volume ()
  "Get master volume (listener)"
  (let ((device (audio-data-device *audio*)))
    (if device
        (ma-device-get-master-volume device)
        (audio-data-master-volume *audio*))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Audio Buffer management
;;;----------------------------------------------------------------------------------

;; Initialize a new audio buffer (filled with silence)
(defun load-audio-buffer (format channels sample-rate size-in-frames usage)
  (let ((audio-buffer (%make-r-audio-buffer)))
    (when (> size-in-frames 0)
      (setf (r-audio-buffer-data audio-buffer) (%ma-make-buffer format (* size-in-frames channels))))
    ;; Audio data runs through a format converter
    (let ((converter (ma-data-converter-init format +audio-device-format+ channels +audio-device-channels+
                                             sample-rate (%audio-device-sample-rate)
                                             :allow-dynamic-sample-rate t)))
      (unless converter
        (trace-log +log-warning+ "AUDIO: Failed to create data conversion pipeline")
        (return-from load-audio-buffer nil))
      (setf (r-audio-buffer-converter audio-buffer) converter))
    ;; A cache for use by the converter is necessary when resampling because
    ;; when generating output frames a different number of input frames will
    ;; be consumed. Any residual input frames need to be kept track of to
    ;; ensure there are no discontinuities
    (setf (r-audio-buffer-converter-residual-count audio-buffer) 0
          (r-audio-buffer-converter-residual audio-buffer)
          (%ma-make-buffer format (* +audio-buffer-residual-capacity+ channels)))
    ;; Init audio buffer values
    (setf (r-audio-buffer-volume audio-buffer) 1f0
          (r-audio-buffer-pitch audio-buffer) 1f0
          (r-audio-buffer-pan audio-buffer) 0f0 ; Center
          (r-audio-buffer-callback audio-buffer) nil
          (r-audio-buffer-processor audio-buffer) nil
          (r-audio-buffer-playing audio-buffer) nil
          (r-audio-buffer-paused audio-buffer) nil
          (r-audio-buffer-looping audio-buffer) nil
          (r-audio-buffer-usage audio-buffer) usage
          (r-audio-buffer-frame-cursor-pos audio-buffer) 0
          (r-audio-buffer-frames-processed audio-buffer) 0
          (r-audio-buffer-size-in-frames audio-buffer) size-in-frames)
    ;; Buffers should be marked as processed by default so that a call to
    ;; UpdateAudioStream() immediately after initialization works correctly
    (setf (aref (r-audio-buffer-is-sub-buffer-processed audio-buffer) 0) t
          (aref (r-audio-buffer-is-sub-buffer-processed audio-buffer) 1) t)
    ;; Track audio buffer to linked list next position
    (track-audio-buffer audio-buffer)
    audio-buffer))

;; Delete an audio buffer
(defun unload-audio-buffer (buffer)
  (when buffer
    (untrack-audio-buffer buffer)
    (setf (r-audio-buffer-converter buffer) nil
          (r-audio-buffer-converter-residual buffer) nil
          (r-audio-buffer-data buffer) nil)))

;; Check if an audio buffer is playing from a program state without lock
(defun is-audio-buffer-playing (buffer)
  (%with-audio-lock
    (%is-audio-buffer-playing-in-locked-state buffer)))

;; Play an audio buffer
;; NOTE: Buffer is restarted to the start
;; Use PauseAudioBuffer() and ResumeAudioBuffer() if the playback position should be maintained
(defun play-audio-buffer (buffer)
  (when buffer
    (%with-audio-lock
      (setf (r-audio-buffer-playing buffer) t
            (r-audio-buffer-paused buffer) nil
            (r-audio-buffer-frame-cursor-pos buffer) 0
            (r-audio-buffer-frames-processed buffer) 0
            (aref (r-audio-buffer-is-sub-buffer-processed buffer) 0) t
            (aref (r-audio-buffer-is-sub-buffer-processed buffer) 1) t))))

;; Stop an audio buffer from a program state without lock
(defun stop-audio-buffer (buffer)
  (%with-audio-lock
    (%stop-audio-buffer-in-locked-state buffer)))

;; Pause an audio buffer
(defun pause-audio-buffer (buffer)
  (when buffer
    (%with-audio-lock
      (setf (r-audio-buffer-paused buffer) t))))

;; Resume an audio buffer
(defun resume-audio-buffer (buffer)
  (when buffer
    (%with-audio-lock
      (setf (r-audio-buffer-paused buffer) nil))))

;; Set volume for an audio buffer
(defun set-audio-buffer-volume (buffer volume)
  (when buffer
    (%with-audio-lock
      (setf (r-audio-buffer-volume buffer) (coerce volume 'single-float)))))

;; Set pitch for an audio buffer
(defun set-audio-buffer-pitch (buffer pitch)
  (let ((pitch (coerce pitch 'single-float)))
    (when (and buffer (> pitch 0f0))
      (%with-audio-lock
        ;; Pitching is an adjustment of the sample rate
        ;; Note that this changes the duration of the sound:
        ;;  - higher pitches will make the sound faster
        ;;  - lower pitches make it slower
        (let ((output-sample-rate (truncate (/ (float (%audio-device-sample-rate) 1f0) pitch)))
              (converter (r-audio-buffer-converter buffer)))
          (ma-data-converter-set-rate converter (ma-data-converter-sample-rate-in converter) output-sample-rate))
        (setf (r-audio-buffer-pitch buffer) pitch)))))

;; Set pan for an audio buffer
(defun set-audio-buffer-pan (buffer pan)
  (let ((pan (coerce pan 'single-float)))
    (cond ((< pan -1f0) (setf pan -1f0))
          ((> pan 1f0) (setf pan 1f0)))
    (when buffer
      (%with-audio-lock
        (setf (r-audio-buffer-pan buffer) pan)))))

;; Track audio buffer to linked list next position
(defun track-audio-buffer (buffer)
  (%with-audio-lock
    (if (null (audio-data-first *audio*))
        (setf (audio-data-first *audio*) buffer)
        (setf (r-audio-buffer-next (audio-data-last *audio*)) buffer
              (r-audio-buffer-prev buffer) (audio-data-last *audio*)))
    (setf (audio-data-last *audio*) buffer)))

;; Untrack audio buffer from linked list
(defun untrack-audio-buffer (buffer)
  (%with-audio-lock
    (if (null (r-audio-buffer-prev buffer))
        (setf (audio-data-first *audio*) (r-audio-buffer-next buffer))
        (setf (r-audio-buffer-next (r-audio-buffer-prev buffer)) (r-audio-buffer-next buffer)))
    (if (null (r-audio-buffer-next buffer))
        (setf (audio-data-last *audio*) (r-audio-buffer-prev buffer))
        (setf (r-audio-buffer-prev (r-audio-buffer-next buffer)) (r-audio-buffer-prev buffer)))
    (setf (r-audio-buffer-prev buffer) nil
          (r-audio-buffer-next buffer) nil)))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Sounds loading and playing (.WAV)
;;;----------------------------------------------------------------------------------

;; Load wave data from file
(defun load-wave (file-name)
  "Load wave data from file"
  (let ((wave (make-wave)))
    ;; Loading file to memory
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      ;; Loading wave from memory data
      (when file-data
        (setf wave (load-wave-from-memory (or (get-file-extension file-name) "") file-data data-size))))
    wave))

;; Load wave from memory buffer, fileType refers to extension: i.e. ".wav"
;; WARNING: File extension must be provided in lower-case
(defun load-wave-from-memory (file-type file-data data-size)
  "Load wave from memory buffer, fileType refers to extension: i.e. '.wav'"
  (let ((wave (make-wave)))
    (flet ((type-p (&rest types) (member file-type types :test #'string=)))
      (cond
        ((type-p ".wav" ".WAV")
         (let ((wav (drwav-init-memory file-data data-size)))
           (if wav
               (progn
                 (setf (wave-frame-count wave) (drwav-total-pcm-frame-count wav)
                       (wave-sample-rate wave) (drwav-sample-rate wav)
                       (wave-sample-size wave) 16
                       (wave-channels wave) (drwav-channels wav)
                       (wave-data wave) (make-array (* (wave-frame-count wave) (wave-channels wave))
                                                    :element-type '(signed-byte 16) :initial-element 0))
                 ;; NOTE: Forcing conversion to 16bit sample size on reading
                 (drwav-read-pcm-frames-s16 wav (wave-frame-count wave) (wave-data wave)))
               (trace-log +log-warning+ "WAVE: Failed to load WAV data"))
           (when wav (drwav-uninit wav))))
        ((type-p ".ogg" ".OGG")
         (let ((ogg-data (stb-vorbis-open-memory file-data data-size)))
           (if ogg-data
               (multiple-value-bind (channels sample-rate) (stb-vorbis-get-info ogg-data)
                 (setf (wave-sample-rate wave) sample-rate
                       (wave-sample-size wave) 16      ; By default, ogg data is 16 bit per sample (short)
                       (wave-channels wave) channels
                       (wave-frame-count wave) (stb-vorbis-stream-length-in-samples ogg-data)   ; NOTE: It returns frames!
                       (wave-data wave) (make-array (* (wave-frame-count wave) channels)
                                                    :element-type '(signed-byte 16) :initial-element 0))
                 ;; NOTE: Get the number of samples to process (be careful! asking for number of shorts, not bytes!)
                 (stb-vorbis-get-samples-short-interleaved ogg-data channels (wave-data wave)
                                                           (* (wave-frame-count wave) channels))
                 (stb-vorbis-close ogg-data))
               (trace-log +log-warning+ "WAVE: Failed to load OGG data"))))
        ((type-p ".qoa" ".QOA")
         (let* ((qoa (make-qoa-desc))
                (data (qoa-decode file-data data-size qoa)))
           ;; NOTE: Returned sample data is always 16 bit
           (setf (wave-data wave) data
                 (wave-sample-size wave) 16)
           (if data
               (setf (wave-channels wave) (qoa-desc-channels qoa)
                     (wave-sample-rate wave) (qoa-desc-samplerate qoa)
                     (wave-frame-count wave) (qoa-desc-samples qoa))
               (trace-log +log-warning+ "WAVE: Failed to load QOA data"))))
        ((type-p ".flac" ".FLAC")
         (multiple-value-bind (data channels sample-rate total-frame-count)
             (drflac-open-memory-and-read-pcm-frames-s16 file-data data-size)
           ;; NOTE: Forcing conversion to 16bit sample size on reading
           (setf (wave-data wave) data
                 (wave-sample-size wave) 16)
           (if data
               (setf (wave-channels wave) channels
                     (wave-sample-rate wave) sample-rate
                     (wave-frame-count wave) total-frame-count)
               (trace-log +log-warning+ "WAVE: Failed to load FLAC data"))))
        (t (trace-log +log-warning+ "WAVE: Data format not supported"))))
    (trace-log +log-info+ "WAVE: Data loaded successfully (~d Hz, ~d bit, ~d channels)"
               (wave-sample-rate wave) (wave-sample-size wave) (wave-channels wave))
    wave))

;; Check if wave data is valid (data loaded and parameters)
(defun is-wave-valid (wave)
  "Checks if wave data is valid (data loaded and parameters)"
  (and (wave-data wave)                 ; Validate wave data available
       (> (wave-frame-count wave) 0)     ; Validate frame count
       (> (wave-sample-rate wave) 0)     ; Validate sample rate is supported
       (> (wave-sample-size wave) 0)     ; Validate sample size is supported
       (> (wave-channels wave) 0)        ; Validate number of channels supported
       t))

;; Load sound from file
;; NOTE: The entire file is loaded to memory to be played (no-streaming)
(defun load-sound (file-name)
  "Load sound from file"
  (let* ((wave (load-wave file-name))
         (sound (load-sound-from-wave wave)))
    (unload-wave wave)                  ; Sound is loaded, wave can be unloaded
    sound))

;; Load sound from wave data
;; NOTE: Wave data must be unallocated manually
(defun load-sound-from-wave (wave)
  "Load sound from wave data"
  (let ((sound (make-sound)))
    (when (wave-data wave)
      ;; When using miniaudio mixing needs to b done manually
      ;; To simplify this, the format of each sound needs to be converted to be consistent with
      ;; the format used to open the playback AUDIO.System.device
      ;; Format conversion is done on the loading stage
      (let* ((format-in (%audio-format-from-sample-size (wave-sample-size wave)))
             (frame-count-in (wave-frame-count wave))
             (frame-count (ma-convert-frames nil 0 +audio-device-format+ +audio-device-channels+ (%audio-device-sample-rate)
                                             nil frame-count-in format-in (wave-channels wave) (wave-sample-rate wave))))
        (when (= frame-count 0) (trace-log +log-warning+ "SOUND: Failed to get frame count for format conversion"))
        (let ((audio-buffer (load-audio-buffer +audio-device-format+ +audio-device-channels+ (%audio-device-sample-rate)
                                               frame-count +audio-buffer-usage-static+)))
          (if audio-buffer
              (progn
                (setf frame-count (ma-convert-frames (r-audio-buffer-data audio-buffer) frame-count
                                                     +audio-device-format+ +audio-device-channels+ (%audio-device-sample-rate)
                                                     (wave-data wave) frame-count-in format-in (wave-channels wave)
                                                     (wave-sample-rate wave)))
                (when (= frame-count 0) (trace-log +log-warning+ "SOUND: Failed format conversion"))
                (setf (sound-frame-count sound) frame-count
                      (sound-stream sound) (make-audio-stream :sample-rate (%audio-device-sample-rate)
                                                              :sample-size 32
                                                              :channels +audio-device-channels+
                                                              :buffer audio-buffer)))
              (trace-log +log-warning+ "SOUND: Failed to create buffer")))))
    sound))

;; Load sound alias, clone sound from existing sound data, clone does not own wave data
;; NOTE: Wave data must be unallocated manually and will be shared across all clones
(defun load-sound-alias (source)
  "Create a new sound that shares the same sample data as the source sound, does not own the sound data"
  (let ((sound (make-sound))
        (source-buffer (audio-stream-buffer (sound-stream source))))
    (when (and source-buffer (r-audio-buffer-data source-buffer))
      (let ((audio-buffer (load-audio-buffer +audio-device-format+ +audio-device-channels+ (%audio-device-sample-rate)
                                             0 +audio-buffer-usage-static+)))
        (if audio-buffer
            (progn
              (setf (r-audio-buffer-size-in-frames audio-buffer) (r-audio-buffer-size-in-frames source-buffer)
                    (r-audio-buffer-data audio-buffer) (r-audio-buffer-data source-buffer))
              ;; Initalize the buffer as if it was new
              (setf (r-audio-buffer-volume audio-buffer) 1f0
                    (r-audio-buffer-pitch audio-buffer) 1f0
                    (r-audio-buffer-pan audio-buffer) 0f0)
              (setf (sound-frame-count sound) (sound-frame-count source)
                    (sound-stream sound) (make-audio-stream :sample-rate (%audio-device-sample-rate)
                                                            :sample-size 32
                                                            :channels +audio-device-channels+
                                                            :buffer audio-buffer)))
            (trace-log +log-warning+ "SOUND: Failed to create buffer"))))
    sound))

;; Check if sound is valid (data loaded and buffers initialized)
(defun is-sound-valid (sound)
  "Checks if a sound is valid (data loaded and buffers initialized)"
  (let ((stream (sound-stream sound)))
    (and (> (sound-frame-count sound) 0)          ; Validate frame count
         (audio-stream-buffer stream)             ; Validate stream buffer
         (> (audio-stream-sample-rate stream) 0)  ; Validate sample rate is supported
         (> (audio-stream-sample-size stream) 0)  ; Validate sample size is supported
         (> (audio-stream-channels stream) 0)     ; Validate number of channels supported
         t)))

;; Unload wave data
(defun unload-wave (wave)
  "Unload wave data"
  (setf (wave-data wave) nil))

;; Unload sound
(defun unload-sound (sound)
  "Unload sound"
  (unload-audio-buffer (audio-stream-buffer (sound-stream sound))))

(defun unload-sound-alias (alias)
  "Unload a sound alias (does not deallocate sample data)"
  ;; Untrack and unload the sound buffer, not the sample data, it is shared with the source for the alias
  (let ((buffer (audio-stream-buffer (sound-stream alias))))
    (when buffer
      (untrack-audio-buffer buffer)
      (setf (r-audio-buffer-converter buffer) nil
            (r-audio-buffer-converter-residual buffer) nil))))

;; Update sound buffer with new data
;; PARAMS: [data], format must match sound.stream.sampleSize, default 32 bit float - stereo
;; PARAMS: [frameCount] must not exceed sound.frameCount
(defun update-sound (sound data frame-count)
  "Update sound buffer with new data (default data format: 32 bit float, stereo)"
  (let ((buffer (audio-stream-buffer (sound-stream sound))))
    (when buffer
      (stop-audio-buffer buffer)
      (%audio-copy-samples (r-audio-buffer-data buffer) 0 data
                           (* frame-count (ma-data-converter-channels-in (r-audio-buffer-converter buffer)))))))

;; Export wave data to file
(defun export-wave (wave file-name)
  "Export wave data to file, returns true on success"
  (let ((result nil))
    (cond
      ((is-file-extension file-name ".wav")
       (let* ((format-tag (if (= (wave-sample-size wave) 32) +dr-wave-format-ieee-float+ +dr-wave-format-pcm+))
              (sample-count (* (wave-frame-count wave) (wave-channels wave)))
              (file-data (drwav-write-memory format-tag (wave-channels wave) (wave-sample-rate wave)
                                             (wave-sample-size wave) (wave-frame-count wave)
                                             (%audio-data-bytes (wave-data wave) sample-count))))
         ;; NOTE: In raudio.c the result of SaveFileData() is assigned to a shadowing variable,
         ;; the returned result is the number of frames written
         (setf result (> (wave-frame-count wave) 0))
         (save-file-data file-name file-data (length file-data))))
      ((is-file-extension file-name ".qoa")
       (if (= (wave-sample-size wave) 16)
           (let ((qoa (make-qoa-desc :channels (wave-channels wave)
                                     :samplerate (wave-sample-rate wave)
                                     :samples (wave-frame-count wave))))
             (when (> (qoa-write file-name (wave-data wave) qoa) 0) (setf result t)))
           (trace-log +log-warning+ "AUDIO: Wave data must be 16 bit per sample for QOA format export")))
      ((is-file-extension file-name ".raw")
       ;; Export raw sample data (without header)
       ;; NOTE: It's up to the user to track wave parameters
       (let ((bytes (%audio-data-bytes (wave-data wave) (* (wave-frame-count wave) (wave-channels wave)))))
         (setf result (save-file-data file-name bytes (length bytes))))))
    (if result
        (trace-log +log-info+ "FILEIO: [~a] Wave data exported successfully" file-name)
        (trace-log +log-warning+ "FILEIO: [~a] Failed to export wave data" file-name))
    result))

;; Export wave sample data to code (.h)
(defun export-wave-as-code (wave file-name)
  "Export wave sample data to code (.h), returns true on success"
  (let* ((text-bytes-per-line 20)
         (wave-data-size (floor (* (wave-frame-count wave) (wave-channels wave) (wave-sample-size wave)) 8))
         ;; Get file name from path and convert variable name to uppercase
         (var-file-name (map 'string (lambda (c) (if (char<= #\a c #\z) (char-upcase c) c))
                             (get-file-name-without-ext file-name)))
         (txt-data
           (with-output-to-string (out)
             (write-string (format nil "~%//////////////////////////////////////////////////////////////////////////////////~%") out)
             (write-string (format nil "//                                                                              //~%") out)
             (write-string (format nil "// WaveAsCode exporter v1.1 - Wave data exported as an array of bytes           //~%") out)
             (write-string (format nil "//                                                                              //~%") out)
             (write-string (format nil "// more info and bugs-report:  github.com/raysan5/raylib                        //~%") out)
             (write-string (format nil "// feedback and support:       ray[at]raylib.com                                //~%") out)
             (write-string (format nil "//                                                                              //~%") out)
             (write-string (format nil "// Copyright (c) 2018-2026 Ramon Santamaria (@raysan5)                          //~%") out)
             (write-string (format nil "//                                                                              //~%") out)
             (write-string (format nil "//////////////////////////////////////////////////////////////////////////////////~%~%") out)
             ;; Add wave information
             (write-string (format nil "// Wave data information~%") out)
             (write-string (%sprintf (format nil "#define %s_FRAME_COUNT      %u~%") var-file-name (wave-frame-count wave)) out)
             (write-string (%sprintf (format nil "#define %s_SAMPLE_RATE      %u~%") var-file-name (wave-sample-rate wave)) out)
             (write-string (%sprintf (format nil "#define %s_SAMPLE_SIZE      %u~%") var-file-name (wave-sample-size wave)) out)
             (write-string (%sprintf (format nil "#define %s_CHANNELS         %u~%~%") var-file-name (wave-channels wave)) out)
             ;; Write wave data as an array of values
             ;; Wave data is exported as byte array for 8/16bit and float array for 32bit float data
             ;; NOTE: Frame data exported is channel-interlaced: frame01[sampleChannel1, sampleChannel2, ...], frame02[], frame03[]
             (if (= (wave-sample-size wave) 32)
                 (let ((data (wave-data wave))
                       (n (floor wave-data-size 4)))
                   (write-string (%sprintf (format nil "static float %s_DATA[%i] = {~%") var-file-name n) out)
                   (loop for i from 1 below n
                         do (write-string (%sprintf (if (= (mod i text-bytes-per-line) 0) (format nil "%.4ff,~%    ") "%.4ff, ")
                                                    (float (aref data (- i 1)) 1d0))
                                          out))
                   (write-string (%sprintf (format nil "%.4ff };~%") (float (aref data (- n 1)) 1d0)) out))
                 (let ((bytes (%audio-data-bytes (wave-data wave))))
                   (write-string (%sprintf "static unsigned char %s_DATA[%i] = { " var-file-name wave-data-size) out)
                   (loop for i from 1 below wave-data-size
                         do (write-string (%sprintf (if (= (mod i text-bytes-per-line) 0) (format nil "0x%x,~%    ") "0x%x, ")
                                                    (aref bytes (- i 1)))
                                          out))
                   (write-string (%sprintf (format nil "0x%x };~%") (aref bytes (- wave-data-size 1))) out)))))
         ;; NOTE: Text data length exported is determined by '\0' (NULL) character
         (result (save-file-text file-name txt-data)))
    (if result
        (trace-log +log-info+ "FILEIO: [~a] Wave as code exported successfully" file-name)
        (trace-log +log-warning+ "FILEIO: [~a] Failed to export wave as code" file-name))
    result))

;; Play a sound
(defun play-sound (sound)
  "Play a sound"
  (play-audio-buffer (audio-stream-buffer (sound-stream sound))))

;; Pause a sound
(defun pause-sound (sound)
  "Pause a sound"
  (pause-audio-buffer (audio-stream-buffer (sound-stream sound))))

;; Resume a paused sound
(defun resume-sound (sound)
  "Resume a paused sound"
  (resume-audio-buffer (audio-stream-buffer (sound-stream sound))))

;; Stop reproducing a sound
(defun stop-sound (sound)
  "Stop playing a sound"
  (stop-audio-buffer (audio-stream-buffer (sound-stream sound))))

;; Check if sound is playing
(defun is-sound-playing (sound)
  "Check if a sound is currently playing"
  (and (is-audio-buffer-playing (audio-stream-buffer (sound-stream sound))) t))

;; Set volume for a sound
(defun set-sound-volume (sound volume)
  "Set volume for a sound (1.0 is max level)"
  (set-audio-buffer-volume (audio-stream-buffer (sound-stream sound)) volume))

;; Set pitch for a sound
(defun set-sound-pitch (sound pitch)
  "Set pitch for a sound (1.0 is base level)"
  (set-audio-buffer-pitch (audio-stream-buffer (sound-stream sound)) pitch))

;; Set pan for a sound
(defun set-sound-pan (sound pan)
  "Set pan for a sound (-1.0 left, 0.0 center, 1.0 right)"
  (set-audio-buffer-pan (audio-stream-buffer (sound-stream sound)) pan))

;; Convert wave data to desired format
(defun wave-format (wave sample-rate sample-size channels)
  "Convert wave data to desired format"
  (let* ((format-in (%audio-format-from-sample-size (wave-sample-size wave)))
         (format-out (%audio-format-from-sample-size sample-size))
         (frame-count-in (wave-frame-count wave))
         (frame-count (ma-convert-frames nil 0 format-out channels sample-rate
                                         nil frame-count-in format-in (wave-channels wave) (wave-sample-rate wave))))
    (when (= frame-count 0)
      (trace-log +log-warning+ "WAVE: Failed to get frame count for format conversion")
      (return-from wave-format nil))
    (let ((data (%ma-make-buffer format-out (* frame-count channels))))
      (setf frame-count (ma-convert-frames data frame-count format-out channels sample-rate
                                           (wave-data wave) frame-count-in format-in (wave-channels wave)
                                           (wave-sample-rate wave)))
      (when (= frame-count 0)
        (trace-log +log-warning+ "WAVE: Failed format conversion")
        (return-from wave-format nil))
      (setf (wave-frame-count wave) frame-count
            (wave-sample-size wave) sample-size
            (wave-sample-rate wave) sample-rate
            (wave-channels wave) channels
            (wave-data wave) data)
      wave)))

;; Copy a wave to a new wave
(defun wave-copy (wave)
  "Copy a wave to a new wave"
  (let ((new-wave (make-wave)))
    (when (wave-data wave)
      (setf (wave-data new-wave) (subseq (wave-data wave) 0 (* (wave-frame-count wave) (wave-channels wave)))
            (wave-frame-count new-wave) (wave-frame-count wave)
            (wave-sample-rate new-wave) (wave-sample-rate wave)
            (wave-sample-size new-wave) (wave-sample-size wave)
            (wave-channels new-wave) (wave-channels wave)))
    new-wave))

;; Crop a wave to defined frames range
;; NOTE: Security check in case of out-of-range
(defun wave-crop (wave init-frame final-frame)
  "Crop a wave to defined frames range"
  (if (and (>= init-frame 0) (< init-frame final-frame) (<= final-frame (wave-frame-count wave)))
      (let ((channels (wave-channels wave)))
        (setf (wave-data wave) (subseq (wave-data wave) (* init-frame channels) (* final-frame channels))
              (wave-frame-count wave) (- final-frame init-frame)))
      (trace-log +log-warning+ "WAVE: Crop range out of bounds"))
  wave)

;; Load samples data from wave as a floats array
;; NOTE 1: Returned sample values are normalized to range [-1..1]
;; NOTE 2: Sample data allocated should be freed with UnloadWaveSamples()
(defun load-wave-samples (wave)
  "Load samples data from wave as a 32bit float data array"
  (let* ((count (* (wave-frame-count wave) (wave-channels wave)))
         (samples (make-array count :element-type 'single-float :initial-element 0f0))
         (data (wave-data wave)))
    ;; NOTE: sampleCount is the total number of interlaced samples (including channels)
    (dotimes (i count samples)
      (case (wave-sample-size wave)
        (8 (setf (aref samples i) (/ (float (- (aref data i) 128) 1f0) 128f0)))
        (16 (setf (aref samples i) (/ (float (aref data i) 1f0) 32768f0)))
        (32 (setf (aref samples i) (aref data i)))))))

;; Unload samples data loaded with LoadWaveSamples()
(defun unload-wave-samples (samples)
  "Unload samples data loaded with LoadWaveSamples()"
  (declare (ignore samples))
  nil)

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Music loading and stream playing
;;;----------------------------------------------------------------------------------

(defun %print-music-info (music)
  (trace-log +log-info+ "    > Sample rate:   ~d Hz" (audio-stream-sample-rate (music-stream music)))
  (trace-log +log-info+ "    > Sample size:   ~d bits" (audio-stream-sample-size (music-stream music)))
  (trace-log +log-info+ "    > Channels:      ~d (~a)" (audio-stream-channels (music-stream music))
             (case (audio-stream-channels (music-stream music)) (1 "Mono") (2 "Stereo") (t "Multi")))
  (trace-log +log-info+ "    > Total frames:  ~d" (music-frame-count music)))

;; Init music context from file data in memory, returns T on success
;; NOTE: Files are loaded to memory before decoding
(defun %load-music-context (music type data data-size)
  (case type
    (:wav
     (let ((ctx-wav (drwav-init-memory data data-size)))
       (when ctx-wav
         (let ((sample-size (drwav-bits-per-sample ctx-wav)))
           (when (= sample-size 24) (setf sample-size 16))   ; Forcing conversion to s16 on UpdateMusicStream()
           (setf (music-ctx-type music) +music-audio-wav+
                 (music-ctx-data music) ctx-wav
                 (music-stream music) (load-audio-stream (drwav-sample-rate ctx-wav) sample-size (drwav-channels ctx-wav))
                 (music-frame-count music) (drwav-total-pcm-frame-count ctx-wav)
                 (music-looping music) t)   ; Looping enabled by default
           t))))
    (:ogg
     ;; Open ogg audio stream
     (let ((ctx-ogg (stb-vorbis-open-memory data data-size)))
       (when ctx-ogg
         (multiple-value-bind (channels sample-rate) (stb-vorbis-get-info ctx-ogg)
           ;; OGG bit rate defaults to 16 bit, it's enough for compressed format
           (setf (music-ctx-type music) +music-audio-ogg+
                 (music-ctx-data music) ctx-ogg
                 (music-stream music) (load-audio-stream sample-rate 16 channels)
                 ;; WARNING: It seems this function returns length in frames, not samples, so multiply by channels
                 (music-frame-count music) (stb-vorbis-stream-length-in-samples ctx-ogg)
                 (music-looping music) t)
           t))))
    (:qoa
     (let ((ctx-qoa (when (and data (> data-size 0)) (qoaplay-open-memory data data-size))))
       (when ctx-qoa
         ;; NOTE: Loading samples as 32bit float normalized data, so,
         ;; configure the output audio stream to also use float 32bit
         (setf (music-ctx-type music) +music-audio-qoa+
               (music-ctx-data music) ctx-qoa
               (music-stream music) (load-audio-stream (qoa-desc-samplerate (qoaplay-desc-info ctx-qoa)) 32
                                                       (qoa-desc-channels (qoaplay-desc-info ctx-qoa)))
               (music-frame-count music) (qoa-desc-samples (qoaplay-desc-info ctx-qoa))
               (music-looping music) t)
         t)))
    (:flac
     (let ((ctx-flac (drflac-open-memory data data-size)))
       (when ctx-flac
         (let ((sample-size (drflac-bits-per-sample ctx-flac)))
           (when (= sample-size 24) (setf sample-size 16))   ; Forcing conversion to s16 on UpdateMusicStream()
           (setf (music-ctx-type music) +music-audio-flac+
                 (music-ctx-data music) ctx-flac
                 (music-stream music) (load-audio-stream (drflac-sample-rate ctx-flac) sample-size (drflac-channels ctx-flac))
                 (music-frame-count music) (drflac-total-pcm-frame-count ctx-flac)
                 (music-looping music) t)
           t))))))

;; Load music stream from file
(defun load-music-stream (file-name)
  "Load music stream from file"
  (let* ((music (make-music))
         (type (cond ((is-file-extension file-name ".wav") :wav)
                     ((is-file-extension file-name ".ogg") :ogg)
                     ((is-file-extension file-name ".qoa") :qoa)
                     ((is-file-extension file-name ".flac") :flac)))
         (music-loaded (when type
                         (multiple-value-bind (data size) (load-file-data file-name)
                           (and data (%load-music-context music type data size))))))
    (unless type
      (trace-log +log-warning+ "STREAM: [~a] File format not supported" file-name))
    (if (not music-loaded)
        (trace-log +log-warning+ "FILEIO: [~a] Music file could not be opened" file-name)
        (progn
          ;; Show some music stream info
          (trace-log +log-info+ "FILEIO: [~a] Music file loaded successfully" file-name)
          (%print-music-info music)))
    music))

;; Load music stream from memory buffer, fileType refers to extension: i.e. ".wav"
;; WARNING: File extension must be provided in lower-case
(defun load-music-stream-from-memory (file-type data data-size)
  "Load music stream from data"
  (let* ((music (make-music))
         (type (flet ((type-p (&rest types) (member file-type types :test #'string=)))
                 (cond ((type-p ".wav" ".WAV") :wav)
                       ((type-p ".ogg" ".OGG") :ogg)
                       ((type-p ".qoa" ".QOA") :qoa)
                       ((type-p ".flac" ".FLAC") :flac))))
         (music-loaded (when type (%load-music-context music type data data-size))))
    (unless type
      (trace-log +log-warning+ "STREAM: Data format not supported"))
    (if (not music-loaded)
        (trace-log +log-warning+ "FILEIO: Music data could not be loaded")
        (progn
          (trace-log +log-info+ "FILEIO: Music data loaded successfully")
          (%print-music-info music)))
    music))

;; Check if music stream is valid (context and buffers initialized)
(defun is-music-valid (music)
  "Checks if a music stream is valid (context and buffers initialized)"
  (let ((stream (music-stream music)))
    (and (music-ctx-data music)                   ; Validate context loaded
         (> (music-frame-count music) 0)          ; Validate audio frame count
         (> (audio-stream-sample-rate stream) 0)  ; Validate sample rate is supported
         (> (audio-stream-sample-size stream) 0)  ; Validate sample size is supported
         (> (audio-stream-channels stream) 0)     ; Validate number of channels supported
         t)))

;; Unload music stream
(defun unload-music-stream (music)
  "Unload music stream"
  (when (is-music-stream-playing music) (stop-music-stream music))
  (unload-audio-stream (music-stream music))
  (when (music-ctx-data music)
    (cond ((= (music-ctx-type music) +music-audio-wav+) (drwav-uninit (music-ctx-data music)))
          ((= (music-ctx-type music) +music-audio-ogg+) (stb-vorbis-close (music-ctx-data music)))
          ((= (music-ctx-type music) +music-audio-qoa+) (qoaplay-close (music-ctx-data music)))
          ((= (music-ctx-type music) +music-audio-flac+) (drflac-close (music-ctx-data music))))))

;; Start music playing (open stream) from beginning
(defun play-music-stream (music)
  "Start music playing"
  (play-audio-stream (music-stream music)))

;; Pause music playing
(defun pause-music-stream (music)
  "Pause music playing"
  (pause-audio-stream (music-stream music)))

;; Resume music playing
(defun resume-music-stream (music)
  "Resume playing paused music"
  (resume-audio-stream (music-stream music)))

;; Stop music playing (close stream)
(defun stop-music-stream (music)
  "Stop music playing"
  (stop-audio-stream (music-stream music))
  (let ((ctx (music-ctx-data music)))
    (case (music-ctx-type music)
      (#.+music-audio-wav+ (drwav-seek-to-first-pcm-frame ctx))
      (#.+music-audio-ogg+ (stb-vorbis-seek-start ctx))
      (#.+music-audio-qoa+ (qoaplay-rewind ctx))
      (#.+music-audio-flac+ (drflac-seek-to-first-frame ctx)))))

;; Seek music to a certain position (in seconds)
(defun seek-music-stream (music position)
  "Seek music to a position (in seconds)"
  ;; Seeking is not supported in module formats
  (when (or (= (music-ctx-type music) +music-module-xm+) (= (music-ctx-type music) +music-module-mod+))
    (return-from seek-music-stream nil))
  (let ((position-in-frames (truncate (* (coerce position 'single-float)
                                         (float (audio-stream-sample-rate (music-stream music)) 1f0))))
        (ctx (music-ctx-data music)))
    (case (music-ctx-type music)
      (#.+music-audio-wav+ (drwav-seek-to-pcm-frame ctx position-in-frames))
      (#.+music-audio-ogg+ (stb-vorbis-seek-frame ctx position-in-frames))
      (#.+music-audio-qoa+
       (let ((qoa-frame (floor position-in-frames +qoa-frame-len+)))
         (qoaplay-seek-frame ctx qoa-frame) ; Seeks to QOA frame, not PCM frame
         ;; Compute QOA frame number and update positionInFrames
         (setf position-in-frames (qoaplay-desc-sample-position ctx))))
      (#.+music-audio-flac+ (drflac-seek-to-pcm-frame ctx position-in-frames)))
    (let ((buffer (audio-stream-buffer (music-stream music))))
      (%with-audio-lock
        (setf (r-audio-buffer-frames-processed buffer) position-in-frames
              (aref (r-audio-buffer-is-sub-buffer-processed buffer) 0) t
              (aref (r-audio-buffer-is-sub-buffer-processed buffer) 1) t)))))

;; Update (re-fill) music buffers if data already processed
(defun update-music-stream (music)
  "Updates buffers for music streaming"
  (let* ((stream (music-stream music))
         (buffer (audio-stream-buffer stream)))
    (when (or (null buffer) (not (r-audio-buffer-playing buffer)))
      (return-from update-music-stream nil))
    (bt:acquire-lock (audio-data-lock *audio*))
    (let* ((sub-buffer-size-in-frames (floor (r-audio-buffer-size-in-frames buffer) 2))
           (channels (audio-stream-channels stream))
           ;; On first call of this function, lazily pre-allocated a temp buffer to read audio files/memory data in
           ;; NOTE: The temp buffer format is the format provided by the decoder
           (pcm-format (cond ((= (music-ctx-type music) +music-audio-qoa+) +ma-format-f32+)
                             ((and (= (music-ctx-type music) +music-audio-wav+) (= (audio-stream-sample-size stream) 32))
                              +ma-format-f32+)
                             (t +ma-format-s16+)))
           (pcm-size (* sub-buffer-size-in-frames channels))
           (ctx (music-ctx-data music)))
      (when (or (< (audio-data-pcm-buffer-size *audio*) pcm-size)
                (/= (%ma-buffer-format (audio-data-pcm-buffer *audio*)) pcm-format))
        (setf (audio-data-pcm-buffer *audio*) (%ma-make-buffer pcm-format pcm-size)
              (audio-data-pcm-buffer-size *audio*) pcm-size))
      (let ((pcm-buffer (audio-data-pcm-buffer *audio*)))
        ;; Check both sub-buffers to check if they require refilling
        (dotimes (i 2)
          (let* ((frames-left (%u32 (- (music-frame-count music) (r-audio-buffer-frames-processed buffer))))  ; Frames left to be processed
                 (frames-to-stream (if (or (>= frames-left sub-buffer-size-in-frames) (music-looping music))
                                       sub-buffer-size-in-frames
                                       frames-left)))   ; Total frames to be streamed
            (when (= frames-to-stream 0)
              ;; Check if both buffers have been processed
              (let ((processed (r-audio-buffer-is-sub-buffer-processed buffer)))
                (bt:release-lock (audio-data-lock *audio*))
                (when (and (aref processed 0) (aref processed 1))
                  (stop-music-stream music))
                (return-from update-music-stream nil)))
            (when (aref (r-audio-buffer-is-sub-buffer-processed buffer) i)   ; No refilling required, move to next sub-buffer
              (let ((frame-count-still-needed frames-to-stream)
                    (frame-count-read-total 0))
                (case (music-ctx-type music)
                  (#.+music-audio-wav+
                   (when (or (= (audio-stream-sample-size stream) 16) (= (audio-stream-sample-size stream) 32))
                     (loop
                       (let ((frame-count-read (if (= (audio-stream-sample-size stream) 16)
                                                   (drwav-read-pcm-frames-s16 ctx frame-count-still-needed pcm-buffer
                                                                              (* frame-count-read-total channels))
                                                   (drwav-read-pcm-frames-f32 ctx frame-count-still-needed pcm-buffer
                                                                              (* frame-count-read-total channels)))))
                         (incf frame-count-read-total frame-count-read)
                         (decf frame-count-still-needed frame-count-read)
                         (if (= frame-count-still-needed 0)
                             (return)
                             (drwav-seek-to-first-pcm-frame ctx))))))
                  (#.+music-audio-ogg+
                   (loop
                     (let ((frame-count-read (stb-vorbis-get-samples-short-interleaved
                                              ctx channels pcm-buffer (* frame-count-still-needed channels)
                                              (* frame-count-read-total channels))))
                       (incf frame-count-read-total frame-count-read)
                       (decf frame-count-still-needed frame-count-read)
                       (if (= frame-count-still-needed 0)
                           (return)
                           (stb-vorbis-seek-start ctx)))))
                  (#.+music-audio-qoa+
                   (incf frame-count-read-total (qoaplay-decode ctx pcm-buffer frames-to-stream)))
                  (#.+music-audio-flac+
                   (loop
                     (let ((frame-count-read (drflac-read-pcm-frames-s16 ctx frame-count-still-needed pcm-buffer
                                                                         (* frame-count-read-total channels))))
                       (incf frame-count-read-total frame-count-read)
                       (decf frame-count-still-needed frame-count-read)
                       (if (= frame-count-still-needed 0)
                           (return)
                           (drflac-seek-to-first-frame ctx))))))
                (%update-audio-stream-in-locked-state stream pcm-buffer frames-to-stream))))))
      (bt:release-lock (audio-data-lock *audio*)))))

;; Check if any music is playing
(defun is-music-stream-playing (music)
  "Check if music is playing"
  (is-audio-stream-playing (music-stream music)))

;; Set volume for music
(defun set-music-volume (music volume)
  "Set volume for music (1.0 is max level)"
  (set-audio-stream-volume (music-stream music) volume))

;; Set pitch for music
(defun set-music-pitch (music pitch)
  "Set pitch for a music (1.0 is base level)"
  (set-audio-buffer-pitch (audio-stream-buffer (music-stream music)) pitch))

;; Set pan for music
(defun set-music-pan (music pan)
  "Set pan for a music (-1.0 left, 0.0 center, 1.0 right)"
  (set-audio-buffer-pan (audio-stream-buffer (music-stream music)) pan))

;; Get music time length (in seconds)
(defun get-music-time-length (music)
  "Get music time length (in seconds)"
  (let ((sample-rate (audio-stream-sample-rate (music-stream music))))
    (if (= sample-rate 0)
        0f0
        (/ (float (music-frame-count music) 1f0) (float sample-rate 1f0)))))

;; Get current music time played (in seconds)
(defun get-music-time-played (music)
  "Get current music time played (in seconds)"
  (let ((seconds-played 0f0)
        (stream (music-stream music)))
    (when (and (audio-stream-buffer stream) (> (music-frame-count music) 0))
      (%with-audio-lock
        (let* ((buffer (audio-stream-buffer stream))
               (frames-processed (r-audio-buffer-frames-processed buffer))
               (sub-buffer-size (floor (r-audio-buffer-size-in-frames buffer) 2))
               (frames-in-first-buffer (if (aref (r-audio-buffer-is-sub-buffer-processed buffer) 0) 0 sub-buffer-size))
               (frames-in-second-buffer (if (aref (r-audio-buffer-is-sub-buffer-processed buffer) 1) 0 sub-buffer-size))
               (frames-in-buffers (+ frames-in-first-buffer frames-in-second-buffer)))
          (when (and (> frames-in-buffers (music-frame-count music)) (not (music-looping music)))
            (setf frames-in-buffers (music-frame-count music)))
          (let* ((frames-sent-to-mix (if (> sub-buffer-size 0) (rem (r-audio-buffer-frame-cursor-pos buffer) sub-buffer-size) 0))
                 (frames-played (rem (+ (- frames-processed frames-in-buffers) frames-sent-to-mix) (music-frame-count music))))
            (when (< frames-played 0) (incf frames-played (music-frame-count music)))
            (setf seconds-played (/ (float frames-played 1f0) (float (audio-stream-sample-rate stream) 1f0)))))))
    seconds-played))

;; Load audio stream (to stream audio pcm data)
(defun load-audio-stream (sample-rate sample-size channels)
  "Load audio stream (to stream raw audio pcm data)"
  (let* ((stream (make-audio-stream :sample-rate sample-rate :sample-size sample-size :channels channels))
         (format-in (%audio-format-from-sample-size sample-size))
         (device (audio-data-device *audio*))
         ;; The size of a streaming buffer must be at least double the size of a period
         (period-size (if device (ma-device-internal-period-size-in-frames device) 0))
         ;; If the buffer is not set, compute one that would give us a buffer good enough for a decent frame rate at the device bit size/rate
         (device-bits-per-sample (* (min (if device (ma-device-format device) 0) 4) (%audio-device-channels)))
         (sub-buffer-size (if (= (audio-data-default-size *audio*) 0)
                              (* (floor (%audio-device-sample-rate) 30) device-bits-per-sample)
                              (audio-data-default-size *audio*))))
    (when (< sub-buffer-size period-size) (setf sub-buffer-size period-size))
    ;; Create a double audio buffer of defined size
    (setf (audio-stream-buffer stream)
          (load-audio-buffer format-in channels sample-rate (* sub-buffer-size 2) +audio-buffer-usage-stream+))
    (if (audio-stream-buffer stream)
        (progn
          (setf (r-audio-buffer-looping (audio-stream-buffer stream)) t)   ; Always loop for streaming buffers
          (trace-log +log-info+ "STREAM: Initialized successfully (~d Hz, ~d bit, ~a)" sample-rate sample-size
                     (if (= channels 1) "Mono" "Stereo")))
        (trace-log +log-warning+ "STREAM: Failed to load audio buffer, stream could not be created"))
    stream))

;; Check if an audio stream is valid (buffers initialized)
(defun is-audio-stream-valid (stream)
  "Checks if an audio stream is valid (buffers initialized)"
  (and (audio-stream-buffer stream)              ; Validate stream buffer
       (> (audio-stream-sample-rate stream) 0)   ; Validate sample rate is supported
       (> (audio-stream-sample-size stream) 0)   ; Validate sample size is supported
       (> (audio-stream-channels stream) 0)      ; Validate number of channels supported
       t))

;; Unload audio stream and free memory
(defun unload-audio-stream (stream)
  "Unload audio stream and free memory"
  (unload-audio-buffer (audio-stream-buffer stream))
  (trace-log +log-info+ "STREAM: Unloaded audio stream data from RAM"))

;; Update audio stream buffers with data
;; NOTE 1: Only updates one buffer of the stream source: dequeue -> update -> queue
;; NOTE 2: To dequeue a buffer it needs to be processed: IsAudioStreamProcessed()
(defun update-audio-stream (stream data frame-count)
  "Update audio stream buffers with data"
  (%with-audio-lock
    (%update-audio-stream-in-locked-state stream data frame-count)))

;; Check if any audio stream buffers requires refill
(defun is-audio-stream-processed (stream)
  "Check if any audio stream buffers requires refill"
  (let ((buffer (audio-stream-buffer stream)))
    (when buffer
      (%with-audio-lock
        (or (aref (r-audio-buffer-is-sub-buffer-processed buffer) 0)
            (aref (r-audio-buffer-is-sub-buffer-processed buffer) 1))))))

;; Play audio stream
(defun play-audio-stream (stream)
  "Play audio stream"
  (play-audio-buffer (audio-stream-buffer stream)))

;; Play audio stream
(defun pause-audio-stream (stream)
  "Pause audio stream"
  (pause-audio-buffer (audio-stream-buffer stream)))

;; Resume audio stream playing
(defun resume-audio-stream (stream)
  "Resume audio stream"
  (resume-audio-buffer (audio-stream-buffer stream)))

;; Check if audio stream is playing
(defun is-audio-stream-playing (stream)
  "Check if audio stream is playing"
  (and (is-audio-buffer-playing (audio-stream-buffer stream)) t))

;; Stop audio stream
(defun stop-audio-stream (stream)
  "Stop audio stream"
  (stop-audio-buffer (audio-stream-buffer stream)))

;; Set volume for audio stream (1.0 is max level)
(defun set-audio-stream-volume (stream volume)
  "Set volume for audio stream (1.0 is max level)"
  (set-audio-buffer-volume (audio-stream-buffer stream) volume))

;; Set pitch for audio stream (1.0 is base level)
(defun set-audio-stream-pitch (stream pitch)
  "Set pitch for audio stream (1.0 is base level)"
  (set-audio-buffer-pitch (audio-stream-buffer stream) pitch))

;; Set pan for audio stream
(defun set-audio-stream-pan (stream pan)
  "Set pan for audio stream (0.5 is centered)"
  (set-audio-buffer-pan (audio-stream-buffer stream) pan))

;; Default size for new audio streams
(defun set-audio-stream-buffer-size-default (size)
  "Default size for new audio streams"
  (setf (audio-data-default-size *audio*) size))

;; Audio thread callback to request new data
(defun set-audio-stream-callback (stream callback)
  "Audio thread callback to request new data"
  (let ((buffer (audio-stream-buffer stream)))
    (when buffer
      (%with-audio-lock
        (setf (r-audio-buffer-callback buffer) callback)))))

;; Add processor to audio stream. Contrary to buffers, the order of processors is important
;; The new processor must be added at the end
(defun attach-audio-stream-processor (stream process)
  "Attach audio stream processor to stream, receives frames x 2 samples as 'float' (stereo)"
  (%with-audio-lock
    (let ((buffer (audio-stream-buffer stream)))
      (setf (r-audio-buffer-processor buffer) (append (r-audio-buffer-processor buffer) (list process))))))

;; Remove processor from audio stream
(defun detach-audio-stream-processor (stream process)
  "Detach audio stream processor from stream"
  (%with-audio-lock
    (let ((buffer (audio-stream-buffer stream)))
      (setf (r-audio-buffer-processor buffer) (remove process (r-audio-buffer-processor buffer))))))

;; Add processor to audio pipeline. Order of processors is important
;; Works the same way as {Attach,Detach}AudioStreamProcessor() functions, except
;; these two work on the already mixed output before sending it to the sound hardware
(defun attach-audio-mixed-processor (process)
  "Attach audio stream processor to the entire audio pipeline, receives frames x 2 samples as 'float' (stereo)"
  (%with-audio-lock
    (setf (audio-data-mixed-processor *audio*) (append (audio-data-mixed-processor *audio*) (list process)))))

;; Remove processor from audio pipeline
(defun detach-audio-mixed-processor (process)
  "Detach audio stream processor from the entire audio pipeline"
  (%with-audio-lock
    (setf (audio-data-mixed-processor *audio*) (remove process (audio-data-mixed-processor *audio*)))))

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions Definition
;;;----------------------------------------------------------------------------------

;; Reads audio data from an AudioBuffer object in internal format
(defun %read-audio-buffer-frames-in-internal-format (audio-buffer frames-out frame-count)
  ;; Don't read anything if the sound is not playing
  (unless (r-audio-buffer-playing audio-buffer)
    (return-from %read-audio-buffer-frames-in-internal-format 0))
  ;; Using audio buffer callback
  (when (r-audio-buffer-callback audio-buffer)
    (funcall (r-audio-buffer-callback audio-buffer) frames-out frame-count)
    (incf (r-audio-buffer-frames-processed audio-buffer) frame-count)
    (return-from %read-audio-buffer-frames-in-internal-format frame-count))
  (when (= (r-audio-buffer-size-in-frames audio-buffer) 0)
    (return-from %read-audio-buffer-frames-in-internal-format 0))
  (let* ((size-in-frames (r-audio-buffer-size-in-frames audio-buffer))
         (sub-buffer-size-in-frames (if (> size-in-frames 1) (floor size-in-frames 2) size-in-frames))
         (current-sub-buffer-index (floor (r-audio-buffer-frame-cursor-pos audio-buffer) sub-buffer-size-in-frames)))
    (when (> current-sub-buffer-index 1)
      (return-from %read-audio-buffer-frames-in-internal-format 0))
    ;; Another thread can update the processed state of buffers, so
    ;; take a copy here to try and avoid potential synchronization problems
    (let ((is-sub-buffer-processed (copy-seq (r-audio-buffer-is-sub-buffer-processed audio-buffer)))
          (channels (ma-data-converter-channels-in (r-audio-buffer-converter audio-buffer)))
          (static-p (= (r-audio-buffer-usage audio-buffer) +audio-buffer-usage-static+))
          (data (r-audio-buffer-data audio-buffer))
          (frames-read 0))
      ;; Fill out every frame until a buffer that's marked as processed is found, then fill the remainder with 0
      (loop
        ;; Break from this loop differently depending on the buffer's usage
        ;;  - For static buffers, simply fill as much data as possible
        ;;  - For streaming buffers, only fill half of the buffer that are processed
        ;;    Unprocessed halves must keep their audio data in-tact
        (if static-p
            (when (>= frames-read frame-count) (return))
            (when (aref is-sub-buffer-processed current-sub-buffer-index) (return)))
        (let ((total-frames-remaining (- frame-count frames-read)))
          (when (= total-frames-remaining 0) (return))
          (let* ((frames-remaining-in-output-buffer
                   (if static-p
                       (- size-in-frames (r-audio-buffer-frame-cursor-pos audio-buffer))
                       (- sub-buffer-size-in-frames (- (r-audio-buffer-frame-cursor-pos audio-buffer)
                                                       (* sub-buffer-size-in-frames current-sub-buffer-index)))))
                 (frames-to-read (min total-frames-remaining frames-remaining-in-output-buffer))
                 (cursor (r-audio-buffer-frame-cursor-pos audio-buffer)))
            (replace frames-out data :start1 (* frames-read channels)
                                     :start2 (* cursor channels) :end2 (* (+ cursor frames-to-read) channels))
            (setf (r-audio-buffer-frame-cursor-pos audio-buffer) (mod (+ cursor frames-to-read) size-in-frames))
            (incf frames-read frames-to-read)
            ;; If the end of the buffer is read, mark it as processed
            (when (= frames-to-read frames-remaining-in-output-buffer)
              (setf (aref (r-audio-buffer-is-sub-buffer-processed audio-buffer) current-sub-buffer-index) t
                    (aref is-sub-buffer-processed current-sub-buffer-index) t
                    current-sub-buffer-index (mod (+ current-sub-buffer-index 1) 2))
              ;; Break from this loop if looping not enabled
              (unless (r-audio-buffer-looping audio-buffer)
                (%stop-audio-buffer-in-locked-state audio-buffer)
                (return))))))
      ;; Zero-fill excess
      (let ((total-frames-remaining (- frame-count frames-read)))
        (when (> total-frames-remaining 0)
          (fill frames-out (if (typep frames-out '(simple-array single-float (*))) 0f0 0)
                :start (* frames-read channels) :end (* frame-count channels))
          ;; For static buffers, fill the remaining frames with silence for safety, but don't report those
          ;; frames as "read"; The reason for this is that the caller uses the return value
          ;; to know whether a non-looping sound has finished playback
          (unless static-p (incf frames-read total-frames-remaining))))
      frames-read)))

;; Reads audio data from an AudioBuffer object in device format, returned data will be in a format appropriate for mixing
(defun %read-audio-buffer-frames-in-mixing-format (audio-buffer frames-out frame-count)
  ;; NOTE: Continuously converting data from the AudioBuffer's internal format to the mixing format,
  ;; which should be defined by the output format of the data converter
  ;; This is done until frameCount frames have been output
  (let* ((converter (r-audio-buffer-converter audio-buffer))
         (format-in (ma-data-converter-format-in converter))
         (channels-in (ma-data-converter-channels-in converter))
         (bpf (ma-get-bytes-per-frame format-in channels-in))
         (input-buffer-frame-cap (floor 4096 bpf))
         (input-buffer (let ((b (r-audio-buffer-input-buffer audio-buffer)))
                         (if b
                             (progn (fill b (if (= format-in +ma-format-f32+) 0f0 0)) b)
                             (setf (r-audio-buffer-input-buffer audio-buffer)
                                   (%ma-make-buffer format-in (* input-buffer-frame-cap channels-in))))))
         (residual (r-audio-buffer-converter-residual audio-buffer))
         (resampler (ma-data-converter-resampler converter))
         (total-output-frames-processed 0))
    (loop while (< total-output-frames-processed frame-count)
          do (let ((output-frames-to-process-this-iteration (- frame-count total-output-frames-processed))
                   (out-start total-output-frames-processed))
               (if (> (r-audio-buffer-converter-residual-count audio-buffer) 0)
                   ;; Process any residual input frames from the previous read first
                   (multiple-value-bind (input-frames-processed output-frames-processed)
                       (ma-data-converter-process-pcm-frames converter residual 0 (r-audio-buffer-converter-residual-count audio-buffer)
                                                             frames-out out-start output-frames-to-process-this-iteration)
                     ;; Make sure the data in the cache is consumed
                     (replace residual residual :start2 (* input-frames-processed channels-in))
                     (decf (r-audio-buffer-converter-residual-count audio-buffer) input-frames-processed)
                     (incf total-output-frames-processed output-frames-processed))
                   ;; Getting here means there are no residual frames from the previous read
                   ;; Fresh data can now be pulled from the AudioBuffer and processed
                   ;; A best guess needs to be used made to determine how many input frames to pull from the buffer
                   (let ((estimated-input-frame-count
                           (truncate (* (/ (float (ma-resampler-sample-rate-in resampler) 1f0)
                                           (float (ma-resampler-sample-rate-out resampler) 1f0))
                                        (float output-frames-to-process-this-iteration 1f0)))))
                     (when (= estimated-input-frame-count 0) (setf estimated-input-frame-count 1))   ; Make sure at least one input frame is read
                     (when (> estimated-input-frame-count input-buffer-frame-cap) (setf estimated-input-frame-count input-buffer-frame-cap))
                     (let ((input-frames-in-internal-format-count
                             (%read-audio-buffer-frames-in-internal-format audio-buffer input-buffer estimated-input-frame-count)))
                       (multiple-value-bind (input-frames-processed output-frames-processed)
                           (ma-data-converter-process-pcm-frames converter input-buffer 0 input-frames-in-internal-format-count
                                                                 frames-out out-start output-frames-to-process-this-iteration)
                         (incf total-output-frames-processed output-frames-processed)
                         (when (> input-frames-in-internal-format-count input-frames-processed)
                           ;; Getting here means the estimated input frame count was overestimated
                           ;; The residual needs be stored for later use
                           (let ((residual-frame-count (min (- input-frames-in-internal-format-count input-frames-processed)
                                                            +audio-buffer-residual-capacity+)))
                             (replace residual input-buffer :start2 (* input-frames-processed channels-in)
                                                            :end2 (* (+ input-frames-processed residual-frame-count) channels-in))
                             (setf (r-audio-buffer-converter-residual-count audio-buffer) residual-frame-count)))
                         (when (< input-frames-in-internal-format-count estimated-input-frame-count)
                           (return))))))))   ; Reached the end of the sound
    total-output-frames-processed))

;; Sending audio data to device callback function
;; This function will be called when miniaudio needs more data
;; NOTE: All the mixing takes place here
(defun %on-send-audio-data-to-device (device frames-out frame-count)
  (declare (type (simple-array single-float (*)) frames-out))
  ;; Mixing is basically an accumulation, need to initialize the output buffer to 0
  (fill frames-out 0f0 :end (* frame-count (ma-device-channels device)))
  ;; Using a mutex here for thread-safety which makes things not real-time
  (%with-audio-lock
    (let ((temp-buffer (audio-data-temp-buffer *audio*))
          (temp-buffer-frames (floor 1024 +audio-device-channels+)))
      (do ((audio-buffer (audio-data-first *audio*) (r-audio-buffer-next audio-buffer)))
          ((null audio-buffer))
        ;; Ignore stopped or paused sounds
        (when (and (r-audio-buffer-playing audio-buffer) (not (r-audio-buffer-paused audio-buffer)))
          (let ((frames-read 0))
            (loop
              (when (>= frames-read frame-count) (return))
              ;; Read as much data as possible from the stream
              (let ((frames-to-read (- frame-count frames-read)))
                (loop while (> frames-to-read 0)
                      do (fill temp-buffer 0f0)   ; Frames for stereo
                         (let* ((frames-to-read-right-now (min frames-to-read temp-buffer-frames))
                                (frames-just-read (%read-audio-buffer-frames-in-mixing-format audio-buffer temp-buffer
                                                                                              frames-to-read-right-now)))
                           (when (> frames-just-read 0)
                             ;; Apply processors chain if defined
                             (dolist (processor (r-audio-buffer-processor audio-buffer))
                               (funcall processor temp-buffer frames-just-read))
                             (%mix-audio-frames frames-out (* frames-read (ma-device-channels device))
                                                temp-buffer frames-just-read audio-buffer)
                             (decf frames-to-read frames-just-read)
                             (incf frames-read frames-just-read))
                           (unless (r-audio-buffer-playing audio-buffer)
                             (setf frames-read frame-count)
                             (return))
                           ;; If all the frames requested can't be read, break
                           (when (< frames-just-read frames-to-read-right-now)
                             (if (not (r-audio-buffer-looping audio-buffer))
                                 (progn
                                   (%stop-audio-buffer-in-locked-state audio-buffer)
                                   (return))
                                 ;; Should never get here, but for safety,
                                 ;; move the cursor position back to the start and continue the loop
                                 (setf (r-audio-buffer-frame-cursor-pos audio-buffer) 0)))))
                ;; If for some reason is not possible to read every frame, the loop needs to be broken
                (when (> frames-to-read 0) (return)))))))
      (dolist (processor (audio-data-mixed-processor *audio*))
        (funcall processor frames-out frame-count)))))

;; Main mixing function, pretty simple in this project, only an accumulation
;; NOTE: framesOut is both an input and an output, it is initially filled with zeros outside of this function
(defun %mix-audio-frames (frames-out out-start frames-in frame-count buffer)
  (declare (type (simple-array single-float (*)) frames-out frames-in)
           (type fixnum out-start frame-count)
           (optimize speed (safety 0)))
  (let ((local-volume (r-audio-buffer-volume buffer))
        (channels (%audio-device-channels)))
    (declare (type single-float local-volume)
             (type fixnum channels))
    (if (= channels 2)              ; Consider panning
        (let* ((right (/ (+ (r-audio-buffer-pan buffer) 1f0) 2f0))   ; Normalize: [-1..1] -> [0..1]
               (left (- 1f0 right))
               ;; Fast sine approximation in [0..1] for pan law: y = 0.5f*x*(3 - x*x);
               (level0 (* local-volume 0.5f0 left (- 3f0 (* left left))))
               (level1 (* local-volume 0.5f0 right (- 3f0 (* right right)))))
          (declare (type single-float right left level0 level1))
          (dotimes (frame frame-count)
            (let ((o (+ out-start (* frame 2)))
                  (i (* frame 2)))
              (setf (aref frames-out o) (+ (aref frames-out o) (* (aref frames-in i) level0))
                    (aref frames-out (+ o 1)) (+ (aref frames-out (+ o 1)) (* (aref frames-in (+ i 1)) level1))))))
        ;; Do not consider panning
        (dotimes (frame frame-count)
          (dotimes (c channels)
            (let ((o (+ out-start (* frame channels) c))
                  (i (+ (* frame channels) c)))
              ;; Output accumulates input multiplied by volume to provided output (usually 0)
              (setf (aref frames-out o) (+ (aref frames-out o) (* (aref frames-in i) local-volume)))))))))

;; Check if an audio buffer is playing, assuming the audio system mutex has been locked
(defun %is-audio-buffer-playing-in-locked-state (buffer)
  (and buffer (r-audio-buffer-playing buffer) (not (r-audio-buffer-paused buffer))))

;; Stop an audio buffer, assuming the audio system mutex has been locked
(defun %stop-audio-buffer-in-locked-state (buffer)
  (when (and buffer (%is-audio-buffer-playing-in-locked-state buffer))
    (setf (r-audio-buffer-playing buffer) nil
          (r-audio-buffer-paused buffer) nil
          (r-audio-buffer-frame-cursor-pos buffer) 0
          (r-audio-buffer-frames-processed buffer) 0
          (aref (r-audio-buffer-is-sub-buffer-processed buffer) 0) t
          (aref (r-audio-buffer-is-sub-buffer-processed buffer) 1) t)))

;; Update audio stream, assuming the audio system mutex has been locked
(defun %update-audio-stream-in-locked-state (stream data frame-count)
  (let ((buffer (audio-stream-buffer stream)))
    (when buffer
      (let ((processed (r-audio-buffer-is-sub-buffer-processed buffer)))
        (if (or (aref processed 0) (aref processed 1))
            (let ((sub-buffer-to-update 0))
              (if (and (aref processed 0) (aref processed 1))
                  ;; Both buffers are available for updating
                  ;; Update the first one and make sure the cursor is moved back to the front
                  (setf sub-buffer-to-update 0
                        (r-audio-buffer-frame-cursor-pos buffer) 0)
                  ;; Update whichever sub-buffer is processed
                  (setf sub-buffer-to-update (if (aref processed 0) 0 1)))
              (let* ((sub-buffer-size-in-frames (floor (r-audio-buffer-size-in-frames buffer) 2))
                     (channels (audio-stream-channels stream))
                     (sub-buffer-start (* sub-buffer-size-in-frames channels sub-buffer-to-update)))
                (incf (r-audio-buffer-frames-processed buffer) frame-count)
                ;; Does this API expect a whole buffer to be updated in one go?
                ;; Assuming so, but if not will need to change this logic
                (if (>= sub-buffer-size-in-frames frame-count)
                    (let ((data-buffer (r-audio-buffer-data buffer)))
                      (%audio-copy-samples data-buffer sub-buffer-start data (* frame-count channels))
                      ;; Any leftover frames should be filled with zeros
                      (when (> (- sub-buffer-size-in-frames frame-count) 0)
                        (fill data-buffer (if (typep data-buffer '(simple-array single-float (*))) 0f0 0)
                              :start (+ sub-buffer-start (* frame-count channels))
                              :end (+ sub-buffer-start (* sub-buffer-size-in-frames channels))))
                      (setf (aref processed sub-buffer-to-update) nil))
                    (trace-log +log-warning+ "STREAM: Attempting to write too many frames to buffer"))))
            (trace-log +log-warning+ "STREAM: Buffer not available for updating"))))))
