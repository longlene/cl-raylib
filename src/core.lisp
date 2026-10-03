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
  (input-gamepad-current-button-state (make-array (list +max-gamepads+ +max-gamepad-buttons+) :initial-element nil :element-type 'boolean) :type (simple-array boolean (* *)))
  (input-gamepad-previous-button-state (make-array (list +max-gamepads+ +max-gamepad-buttons+) :initial-element nil :element-type 'boolean) :type (simple-array boolean (* *)))
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
  (set-random-seed (- (get-universal-time) 2208988800)) ; Unix time like time(NULL)
  
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

(declaim (ftype (function () (values boolean &optional)) window-should-close))
(defun window-should-close ()
  "Check if window should close"
  ;; First check if ESC key is currently pressed (direct polling)
  (when (and (platform-data-handle *platform*)
             (eq (glfw:key-state 256 (platform-data-handle *platform*)) :press))
    (setf (core-data-window-should-close *core*) t)
    (setf (glfw:should-close-p (platform-data-handle *platform*)) t))
  
  ;; Return true if any close condition is met
  (values (or (core-data-window-should-close *core*)
              (when (platform-data-handle *platform*)
                (glfw:should-close-p (platform-data-handle *platform*))))))

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
    (%gl:clear-color (vx normalized) (vy normalized)
                     (vz normalized) (vw normalized))
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

;;; Random values generation functions (from rcore.c)
;;; NOTE: Port of rprand.h (SUPPORT_RPRAND_GENERATOR): Xoshiro128** generator seeded by SplitMix64

(defvar *cl-raylib-random-state* (make-random-state t) "Random state used by the non-raylib random helpers")

(defvar *rprand-seed* #xAABBCCDD "SplitMix64 default seed (aligned to rprand_state)")
(defvar *rprand-state* (make-array 4 :element-type '(unsigned-byte 32)
                                     :initial-contents '(#x96ea83c1 #x218b21e5 #xaa91febd #x976414d4))
  "Xoshiro128** state, initialized by SplitMix64")

(defun %rprand-splitmix64 ()
  "SplitMix64 generator (uses seed to generate rprand_state)"
  (let ((z (setf *rprand-seed* (logand (+ *rprand-seed* #x9e3779b97f4a7c15) #xffffffffffffffff))))
    (setf z (logand (* (logxor z (ash z -30)) #xbf58476d1ce4e5b9) #xffffffffffffffff))
    (setf z (logand (* (logxor z (ash z -27)) #x94d049bb133111eb) #xffffffffffffffff))
    (logxor z (ash z -31))))

(defun %rprand-xoshiro ()
  "Xoshiro128** generator (uses global rprand_state)"
  (flet ((rotl (x k) (logand (logior (ash x k) (ash x (- k 32))) #xffffffff)))
    (let* ((s *rprand-state*)
           (result (logand (* (rotl (logand (* (aref s 1) 5) #xffffffff) 7) 9) #xffffffff))
           (tt (logand (ash (aref s 1) 9) #xffffffff)))
      (setf (aref s 2) (logxor (aref s 2) (aref s 0))
            (aref s 3) (logxor (aref s 3) (aref s 1))
            (aref s 1) (logxor (aref s 1) (aref s 2))
            (aref s 0) (logxor (aref s 0) (aref s 3))
            (aref s 2) (logxor (aref s 2) tt)
            (aref s 3) (rotl (aref s 3) 11))
      result)))

(defun set-random-seed (seed)
  "Set the seed for the random number generator"
  (setf *rprand-seed* (logand seed #xffffffffffffffff))
  ;; To generate the Xoshiro128** state, we use SplitMix64 generator first
  ;; We generate 4 pseudo-random 64bit numbers that we combine using their LSB|MSB
  (setf (aref *rprand-state* 0) (ldb (byte 32 0) (%rprand-splitmix64))
        (aref *rprand-state* 1) (ldb (byte 32 32) (%rprand-splitmix64))
        (aref *rprand-state* 2) (ldb (byte 32 0) (%rprand-splitmix64))
        (aref *rprand-state* 3) (ldb (byte 32 32) (%rprand-splitmix64)))
  (setf *cl-raylib-random-state* #+sbcl (sb-ext:seed-random-state (logand seed #xffffffff)) #-sbcl (make-random-state t))
  nil)

(defun get-random-value (min max)
  "Get a random value between min and max (both included)"
  (when (> min max) (rotatef min max))
  (+ (mod (%rprand-xoshiro) (1+ (abs (- max min)))) min))

;; NOTE: Returns a vector of COUNT values, NIL if COUNT is greater than the range
(defun load-random-sequence (count min max)
  "Load random values sequence, no values repeated"
  (if (> count (1+ (abs (- max min))))
      (progn (trace-log-warning "Sequence count required is greater than range provided")
             nil)
      (let ((sequence (make-array count :initial-element 0))
            (i 0))
        (loop while (< i count)
              do (let ((value (+ (mod (%rprand-xoshiro) (1+ (abs (- max min)))) min)))
                   (unless (find value sequence :end i)
                     (setf (aref sequence i) value)
                     (incf i))))
        sequence)))

(defun unload-random-sequence (sequence)
  "Unload random values sequence"
  (declare (ignore sequence))
  nil)

;;; Hashing functions (from rcore.c)

(defun compute-crc32 (data data-size)
  "Compute CRC32 hash code"
  (let ((crc #xffffffff))
    (dotimes (i data-size)
      (setf crc (logxor (ash crc -8)
                        (let ((c (logxor (aref data i) (logand crc #xff))))
                          (dotimes (k 8 c)
                            (setf c (if (logbitp 0 c) (logxor #xedb88320 (ash c -1)) (ash c -1))))))))
    (logxor crc #xffffffff)))

(defun %u32+ (&rest values) (logand (reduce #'+ values) #xffffffff))
(defun %rotl32 (x c) (logand (logior (ash x c) (ash x (- c 32))) #xffffffff))
(defun %rotr32 (x c) (logand (logior (ash x (- c)) (ash x (- 32 c))) #xffffffff))

(defun %hash-message (data data-size big-endian-length)
  "MD5/SHA message padding: data, 0x80, zeros and the 64bit bit length"
  (let* ((new-size (* 64 (1+ (floor (+ data-size 8) 64))))
         (msg (make-array new-size :element-type '(unsigned-byte 8) :initial-element 0))
         (bits (* 8 data-size)))
    (replace msg data :end2 data-size)
    (setf (aref msg data-size) 128)
    (dotimes (k 8)
      (setf (aref msg (if big-endian-length (- new-size 1 k) (+ (- new-size 8) k))) (ldb (byte 8 (* 8 k)) bits)))
    msg))

;; NOTE: Returns a vector of 4 unsigned 32bit words
(defun compute-md5 (data data-size)
  "Compute MD5 hash code, returns static int[4] (16 bytes)"
  (let ((r #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22 5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
             4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23 6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))
        (k (let ((v (make-array 64)))
             (dotimes (i 64 v) (setf (aref v i) (floor (* (abs (sin (float (1+ i) 1d0))) (expt 2 32)))))))
        (hash (vector #x67452301 #xefcdab89 #x98badcfe #x10325476))
        (msg (%hash-message data data-size nil)))
    (loop for offset from 0 below (length msg) by 64
          do (let ((w (make-array 16))
                   (a (aref hash 0)) (b (aref hash 1)) (c (aref hash 2)) (d (aref hash 3)))
               (dotimes (i 16) (setf (aref w i) (%u32-le msg (+ offset (* i 4)))))
               (dotimes (i 64)
                 (multiple-value-bind (f g)
                     (cond ((< i 16) (values (logior (logand b c) (logand (lognot b) d)) i))
                           ((< i 32) (values (logior (logand d b) (logand (lognot d) c)) (mod (1+ (* 5 i)) 16)))
                           ((< i 48) (values (logxor b c d) (mod (+ (* 3 i) 5) 16)))
                           (t (values (logxor c (logior b (lognot d))) (mod (* 7 i) 16))))
                   (let ((temp d))
                     (setf d c
                           c b
                           b (%u32+ b (%rotl32 (%u32+ a (logand f #xffffffff) (aref k i) (aref w g)) (aref r i)))
                           a temp))))
               (setf (aref hash 0) (%u32+ (aref hash 0) a) (aref hash 1) (%u32+ (aref hash 1) b)
                     (aref hash 2) (%u32+ (aref hash 2) c) (aref hash 3) (%u32+ (aref hash 3) d))))
    hash))

;; NOTE: Returns a vector of 5 unsigned 32bit words
(defun compute-sha1 (data data-size)
  "Compute SHA1 hash code, returns static int[5] (20 bytes)"
  (let ((hash (vector #x67452301 #xEFCDAB89 #x98BADCFE #x10325476 #xC3D2E1F0))
        (msg (%hash-message data data-size t)))
    (loop for offset from 0 below (length msg) by 64
          do (let ((w (make-array 80)))
               (dotimes (i 16) (setf (aref w i) (%u32-be msg (+ offset (* i 4)))))
               (loop for i from 16 below 80
                     do (setf (aref w i) (%rotl32 (logxor (aref w (- i 3)) (aref w (- i 8))
                                                          (aref w (- i 14)) (aref w (- i 16)))
                                                  1)))
               (let ((a (aref hash 0)) (b (aref hash 1)) (c (aref hash 2)) (d (aref hash 3)) (e (aref hash 4)))
                 (dotimes (i 80)
                   (multiple-value-bind (f k)
                       (cond ((< i 20) (values (logior (logand b c) (logand (lognot b) d)) #x5A827999))
                             ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                             ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
                             (t (values (logxor b c d) #xCA62C1D6)))
                     (let ((temp (%u32+ (%rotl32 a 5) (logand f #xffffffff) e k (aref w i))))
                       (setf e d d c c (%rotl32 b 30) b a a temp))))
                 (setf (aref hash 0) (%u32+ (aref hash 0) a) (aref hash 1) (%u32+ (aref hash 1) b)
                       (aref hash 2) (%u32+ (aref hash 2) c) (aref hash 3) (%u32+ (aref hash 3) d)
                       (aref hash 4) (%u32+ (aref hash 4) e)))))
    hash))

;; NOTE: Returns a vector of 8 unsigned 32bit words
(defun compute-sha256 (data data-size)
  "Compute SHA256 hash code, returns static int[8] (32 bytes)"
  (let ((k #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
             #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
             #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
             #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
             #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
             #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
             #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
             #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
        (hash (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
        (msg (%hash-message data data-size t)))
    (loop for offset from 0 below (length msg) by 64
          do (let ((w (make-array 64)))
               (dotimes (i 16) (setf (aref w i) (%u32-be msg (+ offset (* i 4)))))
               (loop for i from 16 below 64
                     do (let ((s0 (logxor (%rotr32 (aref w (- i 15)) 7) (%rotr32 (aref w (- i 15)) 18) (ash (aref w (- i 15)) -3)))
                              (s1 (logxor (%rotr32 (aref w (- i 2)) 17) (%rotr32 (aref w (- i 2)) 19) (ash (aref w (- i 2)) -10))))
                          (setf (aref w i) (%u32+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
               (let ((a (aref hash 0)) (b (aref hash 1)) (c (aref hash 2)) (d (aref hash 3))
                     (e (aref hash 4)) (f (aref hash 5)) (g (aref hash 6)) (h (aref hash 7)))
                 (dotimes (i 64)
                   (let* ((s1 (logxor (%rotr32 e 6) (%rotr32 e 11) (%rotr32 e 25)))
                          (ch (logxor (logand e f) (logand (logxor e #xffffffff) g)))
                          (temp1 (%u32+ h s1 ch (aref k i) (aref w i)))
                          (s0 (logxor (%rotr32 a 2) (%rotr32 a 13) (%rotr32 a 22)))
                          (maj (logxor (logand a b) (logand a c) (logand b c)))
                          (temp2 (%u32+ s0 maj)))
                     (setf h g g f f e e (%u32+ d temp1) d c c b b a a (%u32+ temp1 temp2))))
                 (loop for v in (list a b c d e f g h)
                       for idx from 0
                       do (setf (aref hash idx) (%u32+ (aref hash idx) v))))))
    hash))

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
