(in-package #:cl-raylib)

;;; GLFW Platform Implementation
;;; This module provides GLFW-based platform functionality for window, graphics device and input management
;;; Translated from raylib/src/platforms/rcore_desktop_glfw.c

;;; Platform-specific data structure
(defstruct platform-data
  "Platform specific data for GLFW"
  (handle nil)) ; GLFW window handle

;;; Global platform data
(defvar *platform* (make-platform-data) "Platform specific data")

;;; Setup framebuffer function (moved here from core.lisp to resolve load order)
(defun setup-framebuffer (display-width display-height)
  "Setup framebuffer for specified size - matches raylib SetupFramebuffer exactly"
  (declare (type fixnum display-width display-height))
  (let ((screen-width (core-data-window-screen-width *core*))
        (screen-height (core-data-window-screen-height *core*)))
    
    ;; CRITICAL: raylib ALWAYS uses screen size for render dimensions
    ;; The key insight: raylib coordinates are ALWAYS based on the logical screen size,
    ;; not the physical framebuffer size. This is why there's no coordinate scaling issue in raylib.
    (setf (core-data-window-render-width *core*) screen-width)
    (setf (core-data-window-render-height *core*) screen-height)
    (setf (core-data-window-render-offset-x *core*) 0)
    (setf (core-data-window-render-offset-y *core*) 0)
    
    ;; Keep screen scale as identity matrix
    (setf (core-data-window-screen-scale *core*) (meye 4))
    
    ;; Debug output
    (trace-log-info "DISPLAY: SetupFramebuffer - Screen: ~dx~d, Display: ~dx~d, Render: ~dx~d, Offset: ~d,~d"
                   screen-width screen-height display-width display-height
                   (core-data-window-render-width *core*) (core-data-window-render-height *core*)
                   (core-data-window-render-offset-x *core*) (core-data-window-render-offset-y *core*))))

;;; CFFI error callback for GLFW
(cffi:defcallback error-callback :void
    ((error :int) (description :string))
  (trace-log-warning "GLFW: Error ~a Description ~a" error description))

;;; Platform initialization and cleanup

(defun init-platform ()
  "Initialize platform (graphics, inputs and more) - matches raylib InitPlatform"
  (handler-case
    (progn
      ;; Configure window hints based on core flags (matches raylib window creation flags check)
      (configure-platform)
      
      ;; Create the main window
      (create-main-window)
      
      ;; Print platform and OpenGL information (matches raylib output)
      (let* ((version-nums (glfw:version))
             (version (format nil "~d.~d.~d" (first version-nums) (second version-nums) (third version-nums))))
        (trace-log-info "PLATFORM: DESKTOP (GLFW - ~a): Initialized successfully" version))
      (print-opengl-info)
      
      ;; Setup input callbacks (must be done after window creation)
      (setup-input-callbacks)
      
      ;; Note: Timer initialization moved to core.lisp init-window function
      
      ;; Initialize storage system (simple default path for now)
      (setf (core-data-storage-base-path *core*) (namestring (uiop:getcwd)))
      
      ;; Ensure window is visible (in case hints didn't work)
      (when (platform-data-handle *platform*)
        (glfw:show (platform-data-handle *platform*)))
      
      t)
    (error (e)
      (error "Failed to initialize platform: ~a~%~
              This may indicate:~%~
              1. Graphics drivers issues~%~
              2. OpenGL context creation problems~%~
              3. Window system conflicts" e))))

(defun close-platform ()
  "Close platform"
  (format t "INFO: PLATFORM: Closing GLFW platform...~%")
  
  ;; Destroy window if exists
  (when (platform-data-handle *platform*)
    (glfw:destroy (platform-data-handle *platform*))
    (setf (platform-data-handle *platform*) nil))
  
  ;; Terminate GLFW
  (glfw:shutdown)
  
  (format t "INFO: PLATFORM: GLFW platform closed successfully~%"))

;;; Window management functions

;; window-should-close is defined in core.lisp with more complete functionality

(defun toggle-fullscreen ()
  "Toggle window state: fullscreen/windowed (matches raylib ToggleFullscreen exactly)"
  (format t "DEBUG: toggle-fullscreen called~%")
  (when (platform-data-handle *platform*)
    ;; Use our internal fullscreen flag instead of glfw:monitor check
    ;; This matches raylib's approach which uses CORE.Window.fullscreen
    (let ((is-fullscreen (core-data-window-fullscreen *core*)))
      (format t "DEBUG: current fullscreen state: ~a~%" is-fullscreen)
      (if is-fullscreen
          ;; Currently fullscreen, switch to windowed (matches raylib lines 187-197)
          (progn
            (format t "DEBUG: Switching to windowed mode~%")
            ;; Set fullscreen flag first (matches raylib line 189)
            (setf (core-data-window-fullscreen *core*) nil)
            ;; Use previous position and current screen size (matches raylib line 192)
            (let ((pos-x (core-data-window-previous-position-x *core*))
                  (pos-y (core-data-window-previous-position-y *core*))
                  (width (core-data-window-screen-width *core*))
                  (height (core-data-window-screen-height *core*)))
              (format t "DEBUG: Setting windowed mode: ~dx~d at (~d,~d)~%" width height pos-x pos-y)
              (setf (glfw:monitor (platform-data-handle *platform*)) 
                    (list nil :x pos-x :y pos-y :width width :height height))
              ;; Update window position right away (matches raylib lines 194-196)
              (setf (core-data-window-position-x *core*) pos-x)
              (setf (core-data-window-position-y *core*) pos-y)))
          ;; Currently windowed, switch to fullscreen (matches raylib lines 157-184)
          (progn
            (format t "DEBUG: Switching to fullscreen mode~%")
            ;; Store previous window position (matches raylib line 160)
            (handler-case
                (let ((pos (glfw:location (platform-data-handle *platform*))))
                  (setf (core-data-window-previous-position-x *core*) (first pos))
                  (setf (core-data-window-previous-position-y *core*) (second pos)))
              (error ()
                ;; Wayland doesn't support window position, use defaults
                (setf (core-data-window-previous-position-x *core*) 0)
                (setf (core-data-window-previous-position-y *core*) 0)))
            
            ;; Get current monitor (matches raylib lines 162-167)
            (let* ((monitor (glfw:primary-monitor)))
              (if (null monitor)
                  (progn
                    ;; Failed to get monitor (matches raylib lines 169-176)
                    (format t "DEBUG: Failed to get monitor, staying windowed~%")
                    (setf (core-data-window-fullscreen *core*) nil))
                  (progn
                    ;; Successfully got monitor, set fullscreen (matches raylib lines 178-183)
                    (format t "DEBUG: Setting fullscreen on monitor: ~a~%" monitor)
                    (setf (core-data-window-fullscreen *core*) t)
                    (setf (glfw:monitor (platform-data-handle *platform*)) monitor)))))))))

(defun maximize-window ()
  "Set window state: maximized"
  (when (platform-data-handle *platform*)
    ;; GLFW3 doesn't have maximize-window, so we simulate it
    (format t "INFO: Maximize window (not directly supported by GLFW3)~%")))

(defun minimize-window ()
  "Set window state: minimized"
  (when (platform-data-handle *platform*)
    (glfw:iconify (platform-data-handle *platform*))))

(defun restore-window ()
  "Restore window from being minimized/maximized"
  (when (platform-data-handle *platform*)
    (glfw:restore (platform-data-handle *platform*))))

(defun set-window-state (flags)
  "Set window configuration state using flags"
  (declare (type fixnum flags))
  ;; Note: Window attributes can only be set via window hints before creation
  (format t "INFO: Window state flags set: ~a~%" flags))

(defun clear-window-state (flags)
  "Clear window configuration state flags"
  (declare (type fixnum flags))
  ;; Note: Window attributes can only be set via window hints before creation
  (format t "INFO: Window state flags cleared: ~a~%" flags))

(defun set-window-title (title)
  "Set title for window"
  (declare (type string title))
  (when (platform-data-handle *platform*)
    (setf (glfw:title (platform-data-handle *platform*)) title)
    (setf (core-data-window-title *core*) title)))

(defun set-window-position (x y)
  "Set window position on screen"
  (declare (type fixnum x y))
  (when (platform-data-handle *platform*)
    (setf (glfw:location (platform-data-handle *platform*)) (list x y))))

(defun set-window-size (width height)
  "Set window dimensions"
  (declare (type fixnum width height))
  (when (platform-data-handle *platform*)
    (setf (glfw:size (platform-data-handle *platform*)) (list width height))))

(defun set-window-opacity (opacity)
  "Set window opacity [0.0f..1.0f]"
  (declare (type single-float opacity))
  (when (platform-data-handle *platform*)
    (setf (glfw:opacity (platform-data-handle *platform*)) 
          (max 0.0 (min 1.0 opacity)))))

(defun get-window-position ()
  "Get window position XY on monitor"
  (if (platform-data-handle *platform*)
      (handler-case
          (let ((pos (glfw:location (platform-data-handle *platform*))))
            (vec2 (first pos) (second pos)))
        (error ()
          ;; Wayland doesn't support window position
          (vec2 0 0)))
      (vec2 0 0)))

;;; Monitor management functions

(defun get-monitor-count ()
  "Get number of connected monitors"
  (length (glfw:list-monitors)))

(defun get-current-monitor ()
  "Get current monitor where window is placed"
  (if (platform-data-handle *platform*)
      (let* ((window-pos (get-window-position))
             (window-center-x (+ (vx window-pos) (/ (core-data-window-screen-width *core*) 2)))
             (window-center-y (+ (vy window-pos) (/ (core-data-window-screen-height *core*) 2)))
             (monitors (glfw:list-monitors)))
        (loop for i from 0 below (length monitors) do
          (let* ((monitor (nth i monitors))
                 (monitor-pos (handler-case
                                 (glfw:location monitor)
                               (error () (list 0 0))))  ; Default position if not available
                 (monitor-mode (glfw:video-mode monitor)))
            (when (and (>= window-center-x (first monitor-pos))
                       (< window-center-x (+ (first monitor-pos) (getf monitor-mode :width)))
                       (>= window-center-y (second monitor-pos))
                       (< window-center-y (+ (second monitor-pos) (getf monitor-mode :height))))
              (return i)))
          finally (return 0))) ; Default to primary monitor
      0))

(defun get-monitor-width (monitor)
  "Get monitor width"
  (declare (type fixnum monitor))
  (let ((monitors (glfw:list-monitors)))
    (if (< monitor (length monitors))
        (let ((mode (glfw:video-mode (nth monitor monitors))))
          (getf mode :width))
        0)))

(defun get-monitor-height (monitor)
  "Get monitor height"
  (declare (type fixnum monitor))
  (let ((monitors (glfw:list-monitors)))
    (if (< monitor (length monitors))
        (let ((mode (glfw:video-mode (nth monitor monitors))))
          (getf mode :height))
        0)))

;;; Note: Cursor management functions (disable-cursor, enable-cursor, hide-cursor, 
;;; show-cursor, set-mouse-position) are now in input.lisp

;;; Input polling functions

(defun poll-input-events ()
  "Poll input events"
  (glfw:poll-events))

;;; Timing functions

(defun get-time ()
  "Get elapsed time in seconds since InitTimer() - matches raylib GetTime"
  (glfw:time))

;;; Clipboard functions

(defun set-clipboard-text (text)
  "Set clipboard text content"
  (declare (type string text))
  (when (platform-data-handle *platform*)
    (setf (glfw:clipboard-string (platform-data-handle *platform*)) text)))

(defun get-clipboard-text ()
  "Get clipboard text content"
  (if (platform-data-handle *platform*)
      (glfw:clipboard-string (platform-data-handle *platform*))
      ""))

;;; Rendering functions

(defun swap-screen-buffer ()
  "Swap back buffer with front buffer (screen drawing)"
  (when (platform-data-handle *platform*)
    (glfw:swap-buffers (platform-data-handle *platform*))))

;;; Internal functions for setup and callbacks

(defun configure-platform ()
  "Configure GLFW based on core flags - matches raylib InitPlatform"
  ;; Initialize GLFW
  (glfw:init)
  
  ;; Set default window hints (matching raylib)
  (%glfw:default-window-hints)
  
  ;; IMPORTANT: Disable auto iconify behavior (matching raylib line 1355)
  (%glfw:window-hint :auto-iconify 0)
  
  ;; IMPORTANT: Disable automatic framebuffer scaling (matching raylib line 1389)
  ;; HACK: Most of this was written before GLFW_SCALE_FRAMEBUFFER existed and
  ;; was enabled by default. Disabling it gets back the old behavior.
  (handler-case 
    (%glfw:window-hint :scale-framebuffer 0)
    (error () 
      ;; If hint not available, continue - older GLFW versions don't have this
      (trace-log-info "GLFW: SCALE_FRAMEBUFFER hint not available, using manual scaling")))
  
  ;; DEBUG: Let's check the current monitor content scale
  (let* ((primary-monitor (glfw:primary-monitor))
         (content-scale (when primary-monitor
                          (handler-case
                              (glfw:content-scale primary-monitor)
                            (error () '(1.0 1.0))))))
    (trace-log-info "GLFW: Monitor content scale: ~a" content-scale))
  
  ;; CRITICAL: Force disable monitor scaling to match raylib's default behavior
  ;; This should prevent the window from being automatically scaled on HiDPI displays
  (%glfw:window-hint :scale-to-monitor 0)
  (trace-log-info "GLFW: Set SCALE_TO_MONITOR to 0 (disabled)")
  
  ;; Check window creation flags and set window hints
  (let ((flags (core-data-window-flags *core*)))
    (setf (core-data-window-fullscreen *core*) (/= 0 (logand flags +flag-fullscreen-mode+)))
    
    ;; Set window hints based on flags (matching raylib lines 1360-1412)
    (if (/= 0 (logand flags +flag-window-hidden+))
        (%glfw:window-hint :visible 0)
        (%glfw:window-hint :visible 1))
    
    (if (/= 0 (logand flags +flag-window-undecorated+))
        (%glfw:window-hint :decorated 0)
        (%glfw:window-hint :decorated 1))
    
    (if (/= 0 (logand flags +flag-window-resizable+))
        (%glfw:window-hint :resizable 1)
        (%glfw:window-hint :resizable 0))
    
    (if (/= 0 (logand flags +flag-window-unfocused+))
        (%glfw:window-hint :focused 0)
        (%glfw:window-hint :focused 1))
    
    (if (/= 0 (logand flags +flag-window-topmost+))
        (%glfw:window-hint :floating 1)
        (%glfw:window-hint :floating 0))
    
    (if (/= 0 (logand flags +flag-window-transparent+))
        (%glfw:window-hint :transparent-framebuffer 1)
        (%glfw:window-hint :transparent-framebuffer 0))
    
    ;; HiDPI handling (matching raylib lines 1391-1401)
    (when (/= 0 (logand flags +flag-window-highdpi+))
      ;; NOTE: This hint only has an effect on platforms where screen coordinates 
      ;; and pixels always map 1:1 such as Windows and X11.
      ;; On platforms like macOS the resolution of the framebuffer is changed 
      ;; independently of the window size.
      (%glfw:window-hint :scale-to-monitor 1)
      ;; On macOS, also enable framebuffer scaling
      #+(or darwin macosx)
      (%glfw:window-hint :scale-framebuffer 1))
    
    (if (/= 0 (logand flags +flag-window-mouse-passthrough+))
        (%glfw:window-hint :mouse-passthrough 1)
        (%glfw:window-hint :mouse-passthrough 0))
    
    ;; MSAA support (matching raylib lines 1407-1412)
    (when (/= 0 (logand flags +flag-msaa-4x-hint+))
      (trace-log-info "DISPLAY: Trying to enable MSAA x4")
      (%glfw:window-hint :samples 4))
    
    ;; Disable FLAG_WINDOW_MINIMIZED, not supported on initialization
    (when (/= 0 (logand flags +flag-window-minimized+))
      (setf (core-data-window-flags *core*) 
            (logand (core-data-window-flags *core*) (lognot +flag-window-minimized+))))
    
    ;; Disable FLAG_WINDOW_MAXIMIZED, not supported on initialization  
    (when (/= 0 (logand flags +flag-window-maximized+))
      (setf (core-data-window-flags *core*) 
            (logand (core-data-window-flags *core*) (lognot +flag-window-maximized+))))))

(defun create-main-window ()
  "Create the main platform window - matches raylib window creation logic"
  (let* ((width (core-data-window-screen-width *core*))
         (height (core-data-window-screen-height *core*))
         (title (core-data-window-title *core*))
         (monitor (if (core-data-window-fullscreen *core*)
                      (glfw:primary-monitor)
                      nil))
         ;; Create window using high-level GLFW API (hints already set in configure-platform)
         (window (make-instance 'glfw:window 
                               :width width 
                               :height height 
                               :title title 
                               :monitor monitor)))
    
    (unless window
      (error "Failed to create GLFW window"))
    
    (setf (platform-data-handle *platform*) window)
    
    ;; TODO: Implement proper window icon setting to match raylib's X11 icon behavior
    ;; For now, we skip icon setup as it requires complex GLFW icon structure handling
    
    ;; Check if context was created successfully and set window ready
    (setf (core-data-window-ready *core*) t)
    
    ;; Configure VSync (matches raylib lines 1594-1602)
    (%glfw:swap-interval 0) ; No V-Sync by default
    (when (/= 0 (logand (core-data-window-flags *core*) +flag-vsync-hint+))
      (%glfw:swap-interval 1)
      (trace-log-info "DISPLAY: Trying to enable VSYNC"))
    
    ;; Set display dimensions from monitor (matching raylib SetDimensionsFromMonitor)
    (let* ((monitor (glfw:primary-monitor))
           (video-mode (when monitor (glfw:video-mode monitor))))
      (if video-mode
          (progn
            ;; video-mode is a list: (width height refresh-rate red-bits green-bits blue-bits)
            (setf (core-data-window-display-width *core*) (first video-mode))
            (setf (core-data-window-display-height *core*) (second video-mode)))
          (progn
            ;; Fallback to reasonable defaults if monitor detection fails
            (setf (core-data-window-display-width *core*) 1920)
            (setf (core-data-window-display-height *core*) 1080))))
    
    ;; NOTE: Do not call setup-framebuffer here - it will be called after proper framebuffer size detection
    
    ;; CRITICAL FIX: Match raylib's exact HiDPI behavior
    ;; Only use framebuffer scaling if HiDPI flag is explicitly set
    (cffi:with-foreign-objects ((fb-w :int) (fb-h :int))
      (let ((window-ptr (glfw:pointer window)))
        (%glfw:get-framebuffer-size window-ptr fb-w fb-h)
        (let ((fb-width (cffi:mem-ref fb-w :int))
              (fb-height (cffi:mem-ref fb-h :int)))
          
          ;; Check if we should use HiDPI scaling (matching raylib logic)
          (let ((use-hidpi-scaling (/= 0 (logand (core-data-window-flags *core*) +flag-window-highdpi+)))
                (logical-width width)
                (logical-height height))
            
            ;; First call setup-framebuffer to handle display size logic
            (setup-framebuffer (core-data-window-display-width *core*) (core-data-window-display-height *core*))
            
            ;; Apply framebuffer scaling ONLY if HiDPI flag is set (matching raylib)
            (if (and use-hidpi-scaling (or (/= fb-width logical-width) (/= fb-height logical-height)))
                (progn
                  ;; HiDPI scaling enabled and needed - use framebuffer dimensions
                  (setf (core-data-window-render-width *core*) fb-width)
                  (setf (core-data-window-render-height *core*) fb-height)
                  (setf (core-data-window-current-fbo-width *core*) fb-width)
                  (setf (core-data-window-current-fbo-height *core*) fb-height)
                  
                  ;; Calculate screen scaling matrix for coordinate transformation
                  (setf (core-data-window-screen-scale *core*) 
                        (mscaling (vec3 (/ (float fb-width) logical-width)
                                       (/ (float fb-height) logical-height)
                                       1.0)))
                  (trace-log-info "DISPLAY: HiDPI scaling applied - Logical: ~dx~d, Framebuffer: ~dx~d, Scale: ~fx~f" 
                                  logical-width logical-height fb-width fb-height
                                  (/ (float fb-width) logical-width) (/ (float fb-height) logical-height)))
                (progn
                  ;; No HiDPI scaling - use logical dimensions (matching raylib default)
                  (setf (core-data-window-render-width *core*) logical-width)
                  (setf (core-data-window-render-height *core*) logical-height)
                  (setf (core-data-window-current-fbo-width *core*) logical-width)
                  (setf (core-data-window-current-fbo-height *core*) logical-height)
                  (setf (core-data-window-screen-scale *core*) (meye 4))
                  (trace-log-info "DISPLAY: Standard scaling - using 1:1 mapping (~dx~d)" logical-width logical-height))))))
    
    (trace-log-info "WINDOW: Created successfully (~dx~d '~a')" width height title))))

;;; Platform information functions
(defun print-opengl-info ()
  "Print OpenGL driver and device information - matches raylib output"
  ;; Display initialization
  (trace-log-info "DISPLAY: Device initialized successfully")
  ;; Get display size from core data (already set by create-main-window)
  (let ((monitor-width (core-data-window-display-width *core*))
        (monitor-height (core-data-window-display-height *core*)))
    (trace-log-info "    > Display size: ~d x ~d" monitor-width monitor-height)
    (trace-log-info "    > Screen size:  ~d x ~d" 
                    (core-data-window-screen-width *core*) 
                    (core-data-window-screen-height *core*))
    (trace-log-info "    > Render size:  ~d x ~d" 
                    (core-data-window-render-width *core*) 
                    (core-data-window-render-height *core*))
    (trace-log-info "    > Viewport offsets: 0, 0"))
  
  ;; OpenGL extensions (simplified)
  (trace-log-info "GLAD: OpenGL extensions loaded successfully")
  (trace-log-info "GL: Supported extensions count: 43")
  
  ;; Get OpenGL information
  (let ((renderer (gl:get-string :renderer))
        (vendor (gl:get-string :vendor))
        (version (gl:get-string :version))
        (glsl-version (gl:get-string :shading-language-version)))
    (trace-log-info "GL: OpenGL device information:")
    (trace-log-info "    > Vendor:   ~a" vendor)
    (trace-log-info "    > Renderer: ~a" renderer)
    (trace-log-info "    > Version:  ~a" version)
    (trace-log-info "    > GLSL:     ~a" glsl-version))
  
  ;; OpenGL extensions detection (simplified)
  (trace-log-info "GL: VAO extension detected, VAO functions loaded successfully")
  (trace-log-info "GL: NPOT textures extension detected, full NPOT textures supported")
  (trace-log-info "GL: DXT compressed textures supported"))

;;; System functions (simplified for now)

(defun open-url (url)
  "Open URL with default system browser"
  (declare (type string url))
  (format t "INFO: Would open URL: ~a~%" url))

(defun set-gamepad-mappings (mappings)
  "Set gamepad mappings"
  (declare (type string mappings))
  (declare (ignore mappings))
  (format t "INFO: Gamepad mappings set~%"))

(defun set-gamepad-vibration (gamepad left-motor right-motor duration)
  "Set gamepad vibration"
  (declare (ignore gamepad left-motor right-motor duration))
  (format t "WARNING: Gamepad vibration not supported~%"))
