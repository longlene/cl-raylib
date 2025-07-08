(in-package #:cl-raylib)

;;; Core functionality and utilities

;;; GLFW action constants
(defconstant +glfw-release+ 0)
(defconstant +glfw-press+ 1)
(defconstant +glfw-repeat+ 2)

;;; Maximum values constants (matching raylib)
(defconstant +max-keyboard-keys+ 512)
(defconstant +max-mouse-buttons+ 8)
(defconstant +max-gamepads+ 4)
(defconstant +max-gamepad-axes+ 8)
(defconstant +max-gamepad-buttons+ 32)
(defconstant +max-touch-points+ 8)
(defconstant +max-key-pressed-queue+ 16)
(defconstant +max-char-pressed-queue+ 16)

;;; Core global state context data structure (matching raylib CoreData)
(defstruct core-data
  "Core global core state context data"
  ;; Window subsystem
  (window-title "" :type string)
  (window-flags 0 :type fixnum)
  (window-ready nil :type boolean)
  (window-fullscreen nil :type boolean)
  (window-should-close nil :type boolean)
  (window-resized-last-frame nil :type boolean)
  (window-event-waiting nil :type boolean)
  (window-using-fbo nil :type boolean)
  (window-position-x 0 :type fixnum)
  (window-position-y 0 :type fixnum)
  (window-previous-position-x 0 :type fixnum)
  (window-previous-position-y 0 :type fixnum)
  (window-display-width 0 :type fixnum)
  (window-display-height 0 :type fixnum)
  (window-screen-width 800 :type fixnum)
  (window-screen-height 600 :type fixnum)
  (window-previous-screen-width 0 :type fixnum)
  (window-previous-screen-height 0 :type fixnum)
  (window-current-fbo-width 0 :type fixnum)
  (window-current-fbo-height 0 :type fixnum)
  (window-render-width 0 :type fixnum)
  (window-render-height 0 :type fixnum)
  (window-render-offset-x 0 :type fixnum)
  (window-render-offset-y 0 :type fixnum)
  (window-screen-min-width 0 :type fixnum)
  (window-screen-min-height 0 :type fixnum)
  (window-screen-max-width 0 :type fixnum)
  (window-screen-max-height 0 :type fixnum)
  (window-screen-scale (meye 4) :type mat4)
  (window-drop-filepaths nil :type list)
  (window-drop-file-count 0 :type fixnum)
  
  ;; Storage subsystem
  (storage-base-path "" :type string)
  
  ;; Input subsystem - Keyboard
  (input-keyboard-exit-key +key-escape+ :type fixnum)
  (input-keyboard-current-state (make-array +max-keyboard-keys+ :initial-element nil) :type simple-vector)
  (input-keyboard-previous-state (make-array +max-keyboard-keys+ :initial-element nil) :type simple-vector)
  (input-keyboard-key-repeat-in-frame (make-array +max-keyboard-keys+ :initial-element nil) :type simple-vector)
  (input-keyboard-key-pressed-queue (make-array +max-key-pressed-queue+ :initial-element 0) :type simple-vector)
  (input-keyboard-key-pressed-queue-count 0 :type fixnum)
  (input-keyboard-char-pressed-queue (make-array +max-char-pressed-queue+ :initial-element 0) :type simple-vector)
  (input-keyboard-char-pressed-queue-count 0 :type fixnum)
  
  ;; Input subsystem - Mouse
  (input-mouse-offset (vec2) :type vec2)
  (input-mouse-scale (vec2 1.0 1.0) :type vec2)
  (input-mouse-current-position (vec2) :type vec2)
  (input-mouse-previous-position (vec2) :type vec2)
  (input-mouse-cursor 0 :type fixnum)
  (input-mouse-cursor-hidden nil :type boolean)
  (input-mouse-cursor-on-screen nil :type boolean)
  (input-mouse-current-button-state (make-array +max-mouse-buttons+ :initial-element nil) :type simple-vector)
  (input-mouse-previous-button-state (make-array +max-mouse-buttons+ :initial-element nil) :type simple-vector)
  (input-mouse-current-wheel-move (vec2) :type vec2)
  (input-mouse-previous-wheel-move (vec2) :type vec2)
  
  ;; Input subsystem - Touch
  (input-touch-point-count 0 :type fixnum)
  (input-touch-point-id (make-array +max-touch-points+ :initial-element 0) :type simple-vector)
  (input-touch-position-x (make-array +max-touch-points+ :initial-element 0.0 :element-type 'single-float) :type (simple-array single-float (*)))
  (input-touch-position-y (make-array +max-touch-points+ :initial-element 0.0 :element-type 'single-float) :type (simple-array single-float (*)))
  (input-touch-current-touch-state (make-array +max-touch-points+ :initial-element nil) :type simple-vector)
  (input-touch-previous-touch-state (make-array +max-touch-points+ :initial-element nil) :type simple-vector)
  
  ;; Input subsystem - Gamepad
  (input-gamepad-last-button-pressed 0 :type fixnum)
  (input-gamepad-axis-count (make-array +max-gamepads+ :initial-element 0) :type simple-vector)
  (input-gamepad-ready (make-array +max-gamepads+ :initial-element nil) :type simple-vector)
  (input-gamepad-name (make-array +max-gamepads+ :initial-element "") :type simple-vector)
  (input-gamepad-current-button-state (make-array (list +max-gamepads+ +max-gamepad-buttons+) :initial-element nil) :type simple-array)
  (input-gamepad-previous-button-state (make-array (list +max-gamepads+ +max-gamepad-buttons+) :initial-element nil) :type simple-array)
  (input-gamepad-axis-state (make-array (list +max-gamepads+ +max-gamepad-axes+) :initial-element 0.0 :element-type 'single-float) :type (simple-array single-float (* *)))
  
  ;; Time subsystem
  (time-current 0.0d0 :type double-float)
  (time-previous 0.0d0 :type double-float)
  (time-update 0.0d0 :type double-float)
  (time-draw 0.0d0 :type double-float)
  (time-frame 0.0d0 :type double-float)
  (time-target 0.0d0 :type double-float)
  (time-base 0 :type fixnum)
  (time-frame-counter 0 :type fixnum))

;;; Global CORE state context (matching raylib CORE global variable)
(defvar *core* (make-core-data) "Global core state context")

;;; Performance optimizations
(declaim (optimize (speed 3) (safety 1) (space 0) (debug 1)))

;;; Initialize window and OpenGL context
(declaim (ftype (function (fixnum fixnum string) (values)) init-window))
(defun init-window (width height title)
  "Initialize window and OpenGL context - matches raylib InitWindow logic"
  (trace-log-info "Initializing raylib 5.5")
  (trace-log-info "Platform backend: DESKTOP (GLFW)")
  (trace-log-info "Supported raylib modules:")
  (trace-log-info "    > core:..... loaded (mandatory)")
  (trace-log-info "    > gl:....... loaded (mandatory)")
  (trace-log-info "    > shapes:... loaded (optional)")
  (trace-log-info "    > textures:. loaded (optional)")
  (trace-log-info "    > text:..... loaded (optional)")
  (trace-log-info "    > models:... loaded (optional)")
  (trace-log-info "    > audio:.... loaded (optional)")
  
  ;; Initialize core window data
  (setf (core-data-window-screen-width *core*) width)
  (setf (core-data-window-screen-height *core*) height)
  (setf (core-data-window-event-waiting *core*) nil)
  (setf (core-data-window-screen-scale *core*) (meye 4)) ; Identity matrix
  (setf (core-data-window-title *core*) title)
  (setf (core-data-window-ready *core*) nil)
  
  ;; Initialize global input state (matching C version CORE.Input reset)
  (init-input-system)
  
  ;; Initialize platform (window, OpenGL context, input callbacks)
  (init-platform)
  
  ;; Note: setup-framebuffer is now called from init-platform at the correct time
  
  ;; Initialize rlgl default data (buffers and shaders) - matches raylib rlglInit
  ;; NOTE: CORE.Window.currentFbo.width and CORE.Window.currentFbo.height not used, just stored as globals in rlgl
  (rlgl-init (core-data-window-current-fbo-width *core*) (core-data-window-current-fbo-height *core*))
  
  ;; Setup default viewport - matches raylib SetupViewport  
  (setup-viewport (core-data-window-current-fbo-width *core*) (core-data-window-current-fbo-height *core*))
  
  ;; Load default font - matches raylib LoadFontDefault
  (load-font-default)
  
  ;; Initialize timer system (matches raylib InitTimer call)
  (init-timer)
  
  ;; Initialize game loop state (matching C version)
  (setf (core-data-time-frame-counter *core*) 0)
  (setf (core-data-window-should-close *core*) nil)
  (set-random-seed (get-universal-time))
  
  ;; Set window ready flag
  (setf (core-data-window-ready *core*) t)
  
  ;; Log working directory (matching C version)
  (trace-log-info "SYSTEM: Working Directory: ~a" (get-working-directory)))

;;; Initialize input system (matching raylib CORE.Input initialization)
(defun init-input-system ()
  "Initialize global input state to match raylib CORE.Input reset"
  ;; Reset keyboard state
  (setf (core-data-input-keyboard-exit-key *core*) +key-escape+)
  (fill (core-data-input-keyboard-current-state *core*) nil)
  (fill (core-data-input-keyboard-previous-state *core*) nil)
  (fill (core-data-input-keyboard-key-repeat-in-frame *core*) nil)
  (fill (core-data-input-keyboard-key-pressed-queue *core*) 0)
  (setf (core-data-input-keyboard-key-pressed-queue-count *core*) 0)
  (fill (core-data-input-keyboard-char-pressed-queue *core*) 0)
  (setf (core-data-input-keyboard-char-pressed-queue-count *core*) 0)
  
  ;; Reset mouse state
  (setf (core-data-input-mouse-offset *core*) (vec2 0.0 0.0))
  (setf (core-data-input-mouse-scale *core*) (vec2 1.0 1.0))
  (setf (core-data-input-mouse-current-position *core*) (vec2 0.0 0.0))
  (setf (core-data-input-mouse-previous-position *core*) (vec2 0.0 0.0))
  (setf (core-data-input-mouse-cursor *core*) 0) ; MOUSE_CURSOR_ARROW
  (setf (core-data-input-mouse-cursor-hidden *core*) nil)
  (setf (core-data-input-mouse-cursor-on-screen *core*) nil)
  (fill (core-data-input-mouse-current-button-state *core*) nil)
  (fill (core-data-input-mouse-previous-button-state *core*) nil)
  (setf (core-data-input-mouse-current-wheel-move *core*) (vec2 0.0 0.0))
  (setf (core-data-input-mouse-previous-wheel-move *core*) (vec2 0.0 0.0))
  
  ;; Reset touch state
  (setf (core-data-input-touch-point-count *core*) 0)
  (fill (core-data-input-touch-point-id *core*) 0)
  (fill (core-data-input-touch-position-x *core*) 0.0)
  (fill (core-data-input-touch-position-y *core*) 0.0)
  (fill (core-data-input-touch-current-touch-state *core*) nil)
  (fill (core-data-input-touch-previous-touch-state *core*) nil)
  
  ;; Reset gamepad state
  (setf (core-data-input-gamepad-last-button-pressed *core*) 0) ; GAMEPAD_BUTTON_UNKNOWN
  (fill (core-data-input-gamepad-axis-count *core*) 0)
  (fill (core-data-input-gamepad-ready *core*) nil)
  (fill (core-data-input-gamepad-name *core*) "")
  (loop for i from 0 below +max-gamepads+
        do (loop for j from 0 below +max-gamepad-buttons+
                 do (setf (aref (core-data-input-gamepad-current-button-state *core*) i j) nil)
                    (setf (aref (core-data-input-gamepad-previous-button-state *core*) i j) nil))
           (loop for j from 0 below +max-gamepad-axes+
                 do (setf (aref (core-data-input-gamepad-axis-state *core*) i j) 0.0))))

(declaim (ftype (function () (values)) close-window))
(defun close-window ()
  "Close window and terminate GLFW using high-level API"
  (when (platform-data-handle *platform*)
    (glfw:destroy (platform-data-handle *platform*))
    (setf (platform-data-handle *platform*) nil))
  (glfw:shutdown)
  (setf (core-data-window-ready *core*) nil))

;;; Window state query functions
(declaim (ftype (function () boolean) is-window-ready))
(defun is-window-ready ()
  "Check if window has been initialized successfully"
  (core-data-window-ready *core*))

(defun is-window-fullscreen ()
  "Check if window is currently fullscreen"
  (and (platform-data-handle *platform*)
       (glfw:monitor (platform-data-handle *platform*))))


;;; Screen and monitor functions
(declaim (ftype (function () fixnum) get-screen-width))
(defun get-screen-width ()
  "Get current screen width"
  (core-data-window-screen-width *core*))

(declaim (ftype (function () fixnum) get-screen-height))
(defun get-screen-height ()
  "Get current screen height"
  (core-data-window-screen-height *core*))

(declaim (ftype (function () fixnum) get-render-width))
(defun get-render-width ()
  "Get current render width (considers HiDPI)"
  ;; Use stored render width if available, otherwise fall back to screen width
  (let ((render-width (core-data-window-render-width *core*)))
    (if (> render-width 0)
        render-width
        (core-data-window-screen-width *core*))))

(declaim (ftype (function () fixnum) get-render-height))
(defun get-render-height ()
  "Get current render height (considers HiDPI)"
  ;; Use stored render height if available, otherwise fall back to screen height
  (let ((render-height (core-data-window-render-height *core*)))
    (if (> render-height 0)
        render-height
        (core-data-window-screen-height *core*))))

(declaim (ftype (function () single-float) get-pixel-scale))
(defun get-pixel-scale ()
  "Get pixel scaling factor for HiDPI displays - matches raylib GetWindowScaleDPI"
  (if (platform-data-handle *platform*)
      (let ((render-width (get-render-width))
            (screen-width (get-screen-width)))
        (if (> screen-width 0)
            (/ (float render-width) (float screen-width))
            1.0))
      1.0))

(declaim (ftype (function () boolean) window-should-close))
(defun window-should-close ()
  "Check if window should close"
  ;; First check if ESC key is currently pressed (direct polling)
  (when (and (platform-data-handle *platform*)
             (eq (glfw:key-state 256 (platform-data-handle *platform*)) :press))
    (setf (core-data-window-should-close *core*) t)
    (setf (glfw:should-close-p (platform-data-handle *platform*)) t))
  
  ;; Return true if any close condition is met
  (or (core-data-window-should-close *core*)
      (when (platform-data-handle *platform*)
        (glfw:should-close-p (platform-data-handle *platform*)))))

;;; Drawing management
(declaim (ftype (function () (values)) begin-drawing))
(defun begin-drawing ()
  "Setup to start drawing with timing integration - matches raylib BeginDrawing"
  (begin-frame-timing)
  ;; Update input state before drawing (matches raylib PollInputEvents)
  (update-input)
  ;; Clear both color and depth buffers for 3D rendering support
  (%gl:clear (logior +gl-color-buffer-bit+ +gl-depth-buffer-bit+))
  
  ;; Enable texture support for text rendering (matches raylib BeginDrawing)
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  
  ;; Apply matrix transformations (matches raylib BeginDrawing)
  (%gl:matrix-mode +gl-modelview+)
  (%gl:load-identity)
  
  ;; No additional matrix transformations needed
  ;; The mixed viewport/projection approach handles coordinate scaling automatically
  )

(declaim (ftype (function () (values)) end-drawing))
(defun end-drawing ()
  "End drawing and swap buffers using high-level API with timing"
  (when (platform-data-handle *platform*)
    (glfw:swap-buffers (platform-data-handle *platform*)))
  (end-frame-timing))

;;; Background and clearing
(declaim (ftype (function (t) (values)) clear-background))
(defun clear-background (color)
  "Clear background with specified color"
  (let* ((actual-color (keyword-to-color color))
         (normalized (color-normalize actual-color)))
    (%gl:clear-color (first normalized) (second normalized)
                     (third normalized) (fourth normalized))
    (%gl:clear +gl-color-buffer-bit+)))

;;; FPS calculation parameters (matches raylib FPS macro definitions)
(defconstant +fps-capture-frames-count+ 30
  "Number of historical frames for FPS calculation")

(defconstant +fps-average-time-seconds+ 0.5
  "FPS average calculation time window (seconds)")

(defconstant +fps-step+ (/ +fps-average-time-seconds+ +fps-capture-frames-count+)
  "FPS update step interval")

;;; Global timing state variables
(defvar *fps-history* (make-array +fps-capture-frames-count+ :initial-element 0.0)
  "FPS historical frame time array")

(defvar *fps-index* 0
  "Current index in FPS history array")

(defvar *fps-average* 0.0
  "FPS average frame time")

(defvar *fps-last-update* 0.0
  "Last FPS update time")

(declaim (ftype (function () (values)) init-timer))
(defun init-timer ()
  "Initialize hi-resolution timer - matches raylib InitTimer exactly"
  ;; Set base time for hi-res timer (using internal-real-time as base)
  (setf (core-data-time-base *core*) (get-internal-real-time))
  ;; Initialize previous time using GetTime (matches raylib: CORE.Time.previous = GetTime())
  (setf (core-data-time-previous *core*) (get-time)))

(declaim (ftype (function () double-float) get-frame-time))
(defun get-frame-time ()
  "Get time in seconds for last frame drawn (delta time) - matches raylib GetFrameTime"
  (float (core-data-time-frame *core*)))

(declaim (ftype (function () fixnum) get-target-fps))
(defun get-target-fps ()
  "Get target FPS"
  (if (> (core-data-time-target *core*) 0.0)
      (round (/ 1.0 (core-data-time-target *core*)))
      0))

;;; Frame timing management (matches raylib BeginDrawing/EndDrawing timing parts)
(defun begin-frame-timing ()
  "Begin frame timing statistics - called in BeginDrawing (matches raylib)"
  (setf (core-data-time-current *core*) (get-time))
  (setf (core-data-time-update *core*) 
        (- (core-data-time-current *core*) (core-data-time-previous *core*)))
  (setf (core-data-time-previous *core*) (core-data-time-current *core*)))

(defun end-frame-timing ()
  "End frame timing statistics - called in EndDrawing (matches raylib)"
  (setf (core-data-time-current *core*) (get-time))
  (setf (core-data-time-draw *core*) 
        (- (core-data-time-current *core*) (core-data-time-previous *core*)))
  (setf (core-data-time-previous *core*) (core-data-time-current *core*))
  
  ;; Calculate total frame time (update + draw)
  (setf (core-data-time-frame *core*) 
        (+ (core-data-time-update *core*) (core-data-time-draw *core*)))
  
  ;; Increment frame counter (matching raylib)
  (incf (core-data-time-frame-counter *core*))
  
  ;; Frame rate control (matches raylib timing logic)
  (when (> (core-data-time-target *core*) 0.0)
    (when (< (core-data-time-frame *core*) (core-data-time-target *core*))
      (let ((wait-time (- (core-data-time-target *core*) (core-data-time-frame *core*))))
        (when (> wait-time 0.0)
          (sleep wait-time))
        
        ;; Recalculate time including wait time
        (setf (core-data-time-current *core*) (get-time))
        (let ((actual-wait-time (- (core-data-time-current *core*) (core-data-time-previous *core*))))
          (setf (core-data-time-previous *core*) (core-data-time-current *core*))
          (incf (core-data-time-frame *core*) actual-wait-time))))))

;;; Performance timer structures
(defstruct performance-timer
  "Performance timer structure"
  (start-time 0.0d0 :type double-float)
  (end-time 0.0d0 :type double-float)
  (running nil :type boolean))

(defun create-timer ()
  "Create performance timer"
  (make-performance-timer))

(defun start-timer (timer)
  "Start timing"
  (setf (performance-timer-start-time timer) (get-time))
  (setf (performance-timer-running timer) t))

(defun stop-timer (timer)
  "Stop timing"
  (setf (performance-timer-end-time timer) (get-time))
  (setf (performance-timer-running timer) nil))

(defun get-timer-elapsed (timer)
  "Get timer elapsed time"
  (if (performance-timer-running timer)
      (- (get-time) (performance-timer-start-time timer))
      (- (performance-timer-end-time timer) (performance-timer-start-time timer))))

;;; Convenience macros
(defmacro with-timer (timer-var &body body)
  "Time execution of code block"
  `(let ((,timer-var (create-timer)))
     (start-timer ,timer-var)
     (unwind-protect
         (progn ,@body)
       (stop-timer ,timer-var))))

(defmacro time-execution (&body body)
  "Measure code execution time and return result and time"
  (let ((timer (gensym "TIMER"))
        (result (gensym "RESULT")))
    `(let ((,timer (create-timer))
           ,result)
       (start-timer ,timer)
       (setf ,result (progn ,@body))
       (stop-timer ,timer)
       (values ,result (get-timer-elapsed ,timer)))))

;;; Debug and monitoring functions
(defun get-timing-info ()
  "Get detailed timing information for debugging"
  (list :current-time (core-data-time-current *core*)
        :frame-time (core-data-time-frame *core*)
        :update-time (core-data-time-update *core*)
        :draw-time (core-data-time-draw *core*)
        :target-fps (get-target-fps)
        :current-fps (get-fps)
        :frame-counter (core-data-time-frame-counter *core*)))

(defun reset-timing ()
  "Reset timing statistics"
  (setf (core-data-time-frame-counter *core*) 0)
  (init-timer))

;;; FPS functions (consolidated from rcore.c timing functionality)
(declaim (ftype (function (fixnum) (values)) set-target-fps))
(defun set-target-fps (fps)
  "Set target FPS (maximum)"
  (if (< fps 1)
      (progn
        (setf (core-data-time-target *core*) 0.0d0)
        (when (platform-data-handle *platform*)
          (setf (glfw:swap-interval (platform-data-handle *platform*)) 0)))
      (progn
        (setf (core-data-time-target *core*) (/ 1.0d0 fps))
        ;; Set vertical synchronization
        (when (platform-data-handle *platform*)
          (setf (glfw:swap-interval (platform-data-handle *platform*)) 1))))
  
  ;; Add same log output as raylib
  (trace-log-info "TIMER: Target time per frame: ~6,3f milliseconds" 
                  (* (core-data-time-target *core*) 1000.0)))

(declaim (ftype (function () fixnum) get-fps))
(defun get-fps ()
  "Get current FPS - matches raylib GetFPS implementation using CORE.Time fields"
  (let ((fps 0))
    ;; Get frame time from CORE.Time.frame (via get-frame-time)
    (let ((fps-frame (get-frame-time)))
      
      ;; Initialize FPS tracking variables on first call (lazy initialization)
      (when (= (core-data-time-frame-counter *core*) 0)
        (setf *fps-average* 0.0)
        (setf *fps-last-update* 0.0)
        (setf *fps-index* 0)
        (fill *fps-history* 0.0))
      
      ;; If frame time is 0, return 0
      (when (= fps-frame 0.0)
        (return-from get-fps 0))
      
      ;; Check if we need to update FPS (using get-time for elapsed time)
      (when (> (- (get-time) *fps-last-update*) +fps-step+)
        (setf *fps-last-update* (get-time))
        (setf *fps-index* (mod (1+ *fps-index*) +fps-capture-frames-count+))
        
        ;; Update sliding window average
        (decf *fps-average* (aref *fps-history* *fps-index*))
        (setf (aref *fps-history* *fps-index*) 
              (/ fps-frame +fps-capture-frames-count+))
        (incf *fps-average* (aref *fps-history* *fps-index*)))
      
      ;; Calculate and return FPS as 1.0/average (matching raylib)
      (setf fps (if (> *fps-average* 0.0)
                    (round (/ 1.0 *fps-average*))
                    0)))
    fps))

(defun wait-time (seconds)
  "Wait for some time"
  (when (> seconds 0.0)
    (sleep seconds)))

;;; Random value generation (from rcore.c)

(defvar *cl-raylib-random-state* (make-random-state t) "cl-raylib random state")

(defun set-random-seed (seed)
  "Set random seed for reproducible sequences"
  (setf *cl-raylib-random-state* (make-random-state nil))
  ;; Initialize with seed by calling random multiple times
  (let ((*random-state* *cl-raylib-random-state*))
    (loop repeat (mod seed 10000) do (random 1.0))))

(defun get-random-value (min max)
  "Get random integer between min and max (inclusive) - matches raylib GetRandomValue"
  (when (> min max)
    ;; Swap if min > max (raylib behavior)
    (rotatef min max))
  
  (let ((*random-state* *cl-raylib-random-state*))
    (+ min (random (1+ (- max min))))))

;;; Scissor mode functions
(defun begin-scissor-mode (x y width height)
  "Begin scissor mode for clipping rendering to a rectangular area (matches raylib BeginScissorMode)"
  ;; Enable scissor test
  (gl:enable :scissor-test)
  ;; Apply Y coordinate flip to match raylib behavior
  ;; In OpenGL, (0,0) is bottom-left, but raylib uses top-left
  (let ((flipped-y (- (core-data-window-screen-height *core*) (+ y height))))
    (gl:scissor x flipped-y width height)))

(defun end-scissor-mode ()
  "End scissor mode (matches raylib EndScissorMode)"
  (gl:disable :scissor-test))

(defun get-working-directory ()
  "Get current working directory - matches raylib GetWorkingDirectory"
  (namestring (uiop:getcwd)))

(defun get-application-directory ()
  "Get application directory - matches raylib GetApplicationDirectory"
  ;; For now, return the directory containing the executable
  ;; This is platform-specific and simplified
  (get-working-directory))

;;; Note: setup-framebuffer function moved to glfw.lisp to resolve load order dependency

(defun setup-viewport (width height)
  "Set viewport for provided width and height - matches raylib SetupViewport exactly"
  (declare (type fixnum width height))
  
  ;; Check if we need to use actual framebuffer size for viewport
  ;; This handles the case where render size == screen size but framebuffer is larger
  (let* ((screen-width (core-data-window-screen-width *core*))      ; Logical screen size
         (screen-height (core-data-window-screen-height *core*))    ; Logical screen size
         (render-width (core-data-window-render-width *core*))      ; Render size (may equal screen size)
         (render-height (core-data-window-render-height *core*))    ; Render size (may equal screen size)
         ;; Get actual framebuffer size for viewport
         (actual-fb-width render-width)
         (actual-fb-height render-height))
    
    ;; On HiDPI displays, even if render size equals screen size, we might need actual framebuffer
    (when (and (platform-data-handle *platform*)
               (= render-width screen-width)  ; If render size was set to screen size
               (= render-height screen-height))
      ;; Get actual framebuffer size from GLFW
      (cffi:with-foreign-objects ((fb-w :int) (fb-h :int))
        (let ((window-ptr (glfw:pointer (platform-data-handle *platform*))))
          (%glfw:get-framebuffer-size window-ptr fb-w fb-h)
          (setf actual-fb-width (cffi:mem-ref fb-w :int))
          (setf actual-fb-height (cffi:mem-ref fb-h :int)))))
    
    ;; Set viewport to actual framebuffer size for proper full-window rendering
    (%gl:viewport (truncate (/ (core-data-window-render-offset-x *core*) 2))
                  (truncate (/ (core-data-window-render-offset-y *core*) 2))
                  actual-fb-width actual-fb-height)
    
    ;; Set orthographic projection to LOGICAL screen size (this is the key!)
    ;; This allows code to use logical coordinates while viewport uses full framebuffer
    (%gl:matrix-mode +gl-projection+)
    (%gl:load-identity)
    (%gl:ortho 0.0d0 (coerce screen-width 'double-float) 
               (coerce screen-height 'double-float) 0.0d0 0.0d0 1.0d0)
    
    ;; Switch back to modelview matrix
    (%gl:matrix-mode +gl-modelview+)
    (%gl:load-identity)
    
    (trace-log-info "DISPLAY: SetupViewport ~dx~d (actual framebuffer), projection ~dx~d (logical)"
                   actual-fb-width actual-fb-height screen-width screen-height)))

;;; Text formatting (from rcore.c TextFormat)

(defun text-format (format-string &rest args)
  "Format text with variables (raylib TextFormat equivalent)"
  ;; Handle raylib-style format specifiers
  (let ((cl-format-string (convert-raylib-format-to-cl format-string)))
    (apply #'format nil cl-format-string args)))

(defun convert-raylib-format-to-cl (raylib-format)
  "Convert raylib format specifiers to Common Lisp format"
  ;; Simple string replacement without regex for basic cases
  (let ((result raylib-format))
    ;; Convert %i to ~d (integer)
    (setf result (substitute-string result "%i" "~d"))
    ;; Convert %d to ~d (integer)  
    (setf result (substitute-string result "%d" "~d"))
    ;; Convert %f to ~f (float)
    (setf result (substitute-string result "%f" "~f"))
    ;; Convert %s to ~a (string)
    (setf result (substitute-string result "%s" "~a"))
    ;; Convert %c to ~c (character)
    (setf result (substitute-string result "%c" "~c"))
    result))

(defun substitute-string (string old new)
  "Replace all occurrences of OLD with NEW in STRING"
  (let ((result string)
        (old-len (length old)))
    (loop for pos = (search old result)
          while pos
          do (setf result (concatenate 'string
                                       (subseq result 0 pos)
                                       new
                                       (subseq result (+ pos old-len)))))
    result))
