(in-package #:cl-raylib)

;;;===================================================================================
;;; rcore - Window/display management, Graphic device/context management and input management
;;; Port of raylib/src/rcore.c
;;; NOTE: Functions with a platform-specific implementation are in glfw.lisp
;;; (port of platforms/rcore_desktop_glfw.c)
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(alexandria:define-constant +raylib-version+ "6.1-dev" :test #'string=)

(defconstant +max-filepath-capacity+ 8192 "Maximum file paths capacity")
(defconstant +max-filepath-length+ 4096 "Maximum length for filepaths (Linux PATH_MAX default value)")
(defconstant +max-keyboard-keys+ 512 "Maximum number of keyboard keys supported")
(defconstant +max-mouse-buttons+ 8 "Maximum number of mouse buttons supported")
(defconstant +max-gamepads+ 4 "Maximum number of gamepads supported")
(defconstant +max-gamepad-axes+ 8 "Maximum number of axes supported (per gamepad)")
(defconstant +max-gamepad-buttons+ 32 "Maximum number of buttons supported (per gamepad)")
(defconstant +max-gamepad-vibration-time+ 2.0 "Maximum vibration time in seconds")
(defconstant +max-touch-points+ 8 "Maximum number of touch points supported")
(defconstant +max-key-pressed-queue+ 16 "Maximum number of keys in the key input queue")
(defconstant +max-char-pressed-queue+ 16 "Maximum number of characters in the char input queue")
(defconstant +max-automation-events+ 16384 "Maximum number of automation events to record")

;; Flags bitwise operation macros
(defmacro %flag-set (place f) `(setf ,place (logior ,place ,f)))
(defmacro %flag-clear (place f) `(setf ,place (logand ,place (lognot ,f))))
(declaim (inline %flag-is-set))
(defun %flag-is-set (n f) (= (logand n f) f))

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

(defun %u8-array (&rest dims)
  (make-array dims :element-type '(unsigned-byte 8) :initial-element 0))

(defun %vec2-array (n)
  (let ((array (make-array n)))
    (dotimes (i n array) (setf (aref array i) (vec2 0.0 0.0)))))

;;; Core global state context data
(defstruct core-data
  ;; Window
  (window-title nil)                                  ; Window text title
  (window-flags 0 :type (unsigned-byte 32))           ; Configuration flags (bit based), keeps window state
  (window-ready nil)                                  ; Check if window has been initialized successfully
  (window-should-close nil)                           ; Check if window set for closing
  (window-resized-last-frame nil)                     ; Check if window has been resized last frame
  (window-event-waiting nil)                          ; Wait for events before ending frame
  (window-using-fbo nil)                              ; Using FBO (RenderTexture) for rendering instead of default framebuffer
  (window-display-width 0 :type fixnum)               ; Display width and height (monitor, device-screen, LCD, ...)
  (window-display-height 0 :type fixnum)
  (window-screen-width 0 :type fixnum)                ; Screen current width and height
  (window-screen-height 0 :type fixnum)
  (window-position-x 0 :type fixnum)                  ; Window current position
  (window-position-y 0 :type fixnum)
  (window-previous-screen-width 0 :type fixnum)       ; Screen previous width and height (required on fullscreen/borderless-windowed toggle)
  (window-previous-screen-height 0 :type fixnum)
  (window-previous-position-x 0 :type fixnum)         ; Window previous position (required on fullscreen/borderless-windowed toggle)
  (window-previous-position-y 0 :type fixnum)
  (window-render-width 0 :type fixnum)                ; Screen framebuffer width and height
  (window-render-height 0 :type fixnum)
  (window-render-offset-x 0 :type fixnum)             ; Screen framebuffer render offset (Not required anymore?)
  (window-render-offset-y 0 :type fixnum)
  (window-current-fbo-width 0 :type fixnum)           ; Current framebuffer render width and height (depends on active render texture)
  (window-current-fbo-height 0 :type fixnum)
  (window-screen-min-width 0 :type fixnum)            ; Screen minimum width and height (for resizable window)
  (window-screen-min-height 0 :type fixnum)
  (window-screen-max-width 0 :type fixnum)            ; Screen maximum width and height (for resizable window)
  (window-screen-max-height 0 :type fixnum)
  (window-screen-scale (matrix-identity))             ; Matrix to scale screen (framebuffer rendering)
  (window-drop-filepaths nil :type list)              ; Store dropped files paths
  (window-drop-file-count 0 :type fixnum)             ; Count dropped files strings
  ;; Storage
  (storage-base-path "")                              ; Base path for data storage
  ;; Input: Keyboard
  (input-keyboard-exit-key 0 :type fixnum)            ; Default exit key
  (input-keyboard-current-key-state (%u8-array +max-keyboard-keys+))  ; Registers current frame key state
  (input-keyboard-previous-key-state (%u8-array +max-keyboard-keys+)) ; Registers previous frame key state
  ;; NOTE: Since key press logic involves comparing previous vs current key state,
  ;; key repeats needs to be handled specially
  (input-keyboard-key-repeat-in-frame (%u8-array +max-keyboard-keys+)) ; Registers key repeats for current frame
  (input-keyboard-key-pressed-queue (make-array +max-key-pressed-queue+ :initial-element 0)) ; Input keys queue
  (input-keyboard-key-pressed-queue-count 0 :type fixnum)      ; Input keys queue count
  (input-keyboard-char-pressed-queue (make-array +max-char-pressed-queue+ :initial-element 0)) ; Input characters queue (unicode)
  (input-keyboard-char-pressed-queue-count 0 :type fixnum)     ; Input characters queue count
  ;; Input: Mouse
  (input-mouse-offset (vec2 0.0 0.0))                 ; Mouse offset
  (input-mouse-scale (vec2 0.0 0.0))                  ; Mouse scaling
  (input-mouse-current-position (vec2 0.0 0.0))       ; Mouse position on screen
  (input-mouse-previous-position (vec2 0.0 0.0))      ; Previous mouse position
  (input-mouse-locked-position (vec2 0.0 0.0))        ; Mouse position when locked
  (input-mouse-cursor 0 :type fixnum)                 ; Tracks current mouse cursor
  (input-mouse-cursor-hidden nil)                     ; Track if cursor is hidden
  (input-mouse-cursor-locked nil)                     ; Track if cursor is locked (disabled)
  (input-mouse-cursor-on-screen nil)                  ; Tracks if cursor is inside client area
  (input-mouse-current-button-state (%u8-array +max-mouse-buttons+))  ; Registers current mouse button state
  (input-mouse-previous-button-state (%u8-array +max-mouse-buttons+)) ; Registers previous mouse button state
  (input-mouse-current-wheel-move (vec2 0.0 0.0))     ; Registers current mouse wheel variation
  (input-mouse-previous-wheel-move (vec2 0.0 0.0))    ; Registers previous mouse wheel variation
  ;; Input: Touch
  (input-touch-point-count 0 :type fixnum)            ; Number of touch points active
  (input-touch-point-id (make-array +max-touch-points+ :initial-element 0)) ; Point identifiers
  (input-touch-position (%vec2-array +max-touch-points+))          ; Touch position on screen
  (input-touch-previous-position (%vec2-array +max-touch-points+)) ; Previous touch position on screen
  (input-touch-current-touch-state (%u8-array +max-touch-points+)) ; Registers current touch state
  (input-touch-previous-touch-state (%u8-array +max-touch-points+)) ; Registers previous touch state
  ;; Input: Gamepad
  (input-gamepad-last-button-pressed 0 :type fixnum)  ; Register last gamepad button pressed
  (input-gamepad-axis-count (make-array +max-gamepads+ :initial-element 0)) ; Register number of available gamepad axes
  (input-gamepad-ready (make-array +max-gamepads+ :initial-element nil))    ; Flag to know if gamepad is ready
  (input-gamepad-name (make-array +max-gamepads+ :initial-element ""))      ; Gamepad name holder
  (input-gamepad-current-button-state (%u8-array +max-gamepads+ +max-gamepad-buttons+))  ; Current gamepad buttons state
  (input-gamepad-previous-button-state (%u8-array +max-gamepads+ +max-gamepad-buttons+)) ; Previous gamepad buttons state
  (input-gamepad-axis-state (make-array (list +max-gamepads+ +max-gamepad-axes+)
                                        :element-type 'single-float :initial-element 0.0)) ; Gamepad axes state
  ;; Time
  (time-current 0.0d0 :type double-float)             ; Current time measure (seconds)
  (time-previous 0.0d0 :type double-float)            ; Previous time measure (seconds)
  (time-update 0.0d0 :type double-float)              ; Time measure for frame update (seconds)
  (time-draw 0.0d0 :type double-float)                ; Time measure for frame draw (seconds)
  (time-frame 0.0d0 :type double-float)               ; Time measure for one frame (seconds)
  (time-target 0.0d0 :type double-float)              ; Desired time for one frame, if 0 not applied (seconds)
  (time-base 0 :type (unsigned-byte 64))              ; Base time measure for hi-res timer (ticks or nanoseconds)
  (time-frame-counter 0 :type (unsigned-byte 32)))    ; Frame counter (frames)

(defun %reset-core-input (core)
  "memset(&CORE.Input, 0, sizeof(CORE.Input))"
  (let ((fresh (make-core-data)))
    (macrolet ((reset (&rest accessors)
                 `(setf ,@(loop for accessor in accessors
                                append `((,accessor core) (,accessor fresh))))))
      (reset core-data-input-keyboard-exit-key
             core-data-input-keyboard-current-key-state
             core-data-input-keyboard-previous-key-state
             core-data-input-keyboard-key-repeat-in-frame
             core-data-input-keyboard-key-pressed-queue
             core-data-input-keyboard-key-pressed-queue-count
             core-data-input-keyboard-char-pressed-queue
             core-data-input-keyboard-char-pressed-queue-count
             core-data-input-mouse-offset
             core-data-input-mouse-scale
             core-data-input-mouse-current-position
             core-data-input-mouse-previous-position
             core-data-input-mouse-locked-position
             core-data-input-mouse-cursor
             core-data-input-mouse-cursor-hidden
             core-data-input-mouse-cursor-locked
             core-data-input-mouse-cursor-on-screen
             core-data-input-mouse-current-button-state
             core-data-input-mouse-previous-button-state
             core-data-input-mouse-current-wheel-move
             core-data-input-mouse-previous-wheel-move
             core-data-input-touch-point-count
             core-data-input-touch-point-id
             core-data-input-touch-position
             core-data-input-touch-previous-position
             core-data-input-touch-current-touch-state
             core-data-input-touch-previous-touch-state
             core-data-input-gamepad-last-button-pressed
             core-data-input-gamepad-axis-count
             core-data-input-gamepad-ready
             core-data-input-gamepad-name
             core-data-input-gamepad-current-button-state
             core-data-input-gamepad-previous-button-state
             core-data-input-gamepad-axis-state))))

;;;----------------------------------------------------------------------------------
;;; Global Variables Definition
;;;----------------------------------------------------------------------------------
(defvar *core* (make-core-data) "Global CORE state context")

;;; Automation events
(defvar *current-event-list* nil "Current automation events list, set by user, keep internal pointer")
(defvar *automation-event-recording* nil "Recording automation events flag")

(defvar *screenshot-counter* 0 "Screenshots counter")

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Window and Graphics Device
;;;----------------------------------------------------------------------------------

(defun init-window (width height title)
  "Initialize window and OpenGL context"
  (trace-log-info "Initializing raylib ~a" +raylib-version+)
  (trace-log-info "Platform backend: DESKTOP (GLFW)")
  (trace-log-info "Supported raylib modules:")
  (trace-log-info "    > rcore:..... loaded (mandatory)")
  (trace-log-info "    > rlgl:...... loaded (mandatory)")
  (trace-log-info "    > rshapes:... loaded (optional)")
  (trace-log-info "    > rtextures:. loaded (optional)")
  (trace-log-info "    > rtext:..... loaded (optional)")
  (trace-log-info "    > rmodels:... loaded (optional)")
  (trace-log-info "    > raudio:.... loaded (optional)")

  ;; Initialize window data
  (setf (core-data-window-screen-width *core*) width
        (core-data-window-screen-height *core*) height
        (core-data-window-event-waiting *core*) nil
        (core-data-window-screen-scale *core*) (matrix-identity)) ; No draw scaling required by default
  (when (and title (plusp (length title))) (setf (core-data-window-title *core*) title))

  ;; Initialize global input state
  (%reset-core-input *core*)            ; Reset CORE.Input structure to 0
  (setf (core-data-input-keyboard-exit-key *core*) +key-escape+
        (core-data-input-mouse-scale *core*) (vec2 1.0 1.0)
        (core-data-input-mouse-cursor *core*) +mouse-cursor-arrow+
        (core-data-input-gamepad-last-button-pressed *core*) +gamepad-button-unknown+)

  ;; Initialize platform
  ;;--------------------------------------------------------------
  (let ((result (init-platform)))
    (when (/= result 0)
      (trace-log-warning "SYSTEM: Failed to initialize platform")
      (return-from init-window)))

  ;; Initialize render dimensions for embedded platforms
  ;; NOTE: On desktop platforms (GLFW, SDL, etc.), CORE.Window.render.width/height are set during window creation
  (when (or (= (core-data-window-render-width *core*) 0) (= (core-data-window-render-height *core*) 0))
    (setf (core-data-window-render-width *core*) (core-data-window-screen-width *core*)
          (core-data-window-render-height *core*) (core-data-window-screen-height *core*)))
  ;;--------------------------------------------------------------

  ;; Initialize rlgl default data (buffers and shaders)
  ;; NOTE: Current fbo size stored as globals in rlgl for convenience
  (rlgl-init (core-data-window-render-width *core*) (core-data-window-render-height *core*))

  ;; Setup default viewport
  (setup-viewport (core-data-window-render-width *core*) (core-data-window-render-height *core*))

  ;; Load default font
  ;; WARNING: External function: Module required: rtext
  (load-font-default)
  ;; Set font white rectangle for shapes drawing, so shapes and text can be batched together
  ;; WARNING: rshapes module is required, if not available, default internal white rectangle is used
  (let ((rec (aref (font-recs (get-font-default)) 95)))
    (if (%flag-is-set (core-data-window-flags *core*) +flag-msaa-4x-hint+)
        ;; NOTE: Try to maximize rec padding to avoid pixel bleeding on MSAA filtering
        (set-shapes-texture (font-texture (get-font-default))
                            (make-rectangle :x (+ (rectangle-x rec) 2) :y (+ (rectangle-y rec) 2)
                                            :width 1.0 :height 1.0))
        ;; NOTE: Set up a 1px padding on char rectangle to avoid pixel bleeding
        (set-shapes-texture (font-texture (get-font-default))
                            (make-rectangle :x (+ (rectangle-x rec) 1) :y (+ (rectangle-y rec) 1)
                                            :width (- (rectangle-width rec) 2)
                                            :height (- (rectangle-height rec) 2)))))

  (setf (core-data-time-frame-counter *core*) 0
        (core-data-window-should-close *core*) nil)

  ;; Initialize random seed
  (set-random-seed (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0)))

  (trace-log-info "SYSTEM: Working Directory: ~a" (get-working-directory))
  (values))

(defun close-window ()
  "Close window and unload OpenGL context"
  (unload-font-default)                 ; WARNING: Module required: rtext
  (rlgl-close)                          ; De-init rlgl
  ;; De-initialize platform
  ;;--------------------------------------------------------------
  (close-platform)
  ;;--------------------------------------------------------------
  (setf (core-data-window-ready *core*) nil)
  (trace-log-info "Window closed successfully")
  (values))

(defun is-window-ready ()
  "Check if window has been initialized successfully"
  (core-data-window-ready *core*))

(defun is-window-fullscreen ()
  "Check if window is currently fullscreen"
  (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+))

(defun is-window-hidden ()
  "Check if window is currently hidden"
  (%flag-is-set (core-data-window-flags *core*) +flag-window-hidden+))

(defun is-window-minimized ()
  "Check if window has been minimized"
  (%flag-is-set (core-data-window-flags *core*) +flag-window-minimized+))

(defun is-window-maximized ()
  "Check if window has been maximized"
  (%flag-is-set (core-data-window-flags *core*) +flag-window-maximized+))

(defun is-window-focused ()
  "Check if window has the focus"
  (not (%flag-is-set (core-data-window-flags *core*) +flag-window-unfocused+)))

(defun is-window-resized ()
  "Check if window has been resizedLastFrame"
  (core-data-window-resized-last-frame *core*))

(defun is-window-state (flag)
  "Check if one specific window flag is enabled"
  (%flag-is-set (core-data-window-flags *core*) (%flags flag)))

(defun get-screen-width ()
  "Get current screen width"
  (core-data-window-screen-width *core*))

(defun get-screen-height ()
  "Get current screen height"
  (core-data-window-screen-height *core*))

(defun get-render-width ()
  "Get current render width which is equal to screen width*dpi scale"
  (if (core-data-window-using-fbo *core*)
      (core-data-window-current-fbo-width *core*)
      (core-data-window-render-width *core*)))

(defun get-render-height ()
  "Get current screen height which is equal to screen height*dpi scale"
  (if (core-data-window-using-fbo *core*)
      (core-data-window-current-fbo-height *core*)
      (core-data-window-render-height *core*)))

(defun enable-event-waiting ()
  "Enable waiting for events on EndDrawing(), no automatic event polling"
  (setf (core-data-window-event-waiting *core*) t))

(defun disable-event-waiting ()
  "Disable waiting for events on EndDrawing(), automatic events polling"
  (setf (core-data-window-event-waiting *core*) nil))

(defun is-cursor-hidden ()
  "Check if cursor is not visible"
  (core-data-input-mouse-cursor-hidden *core*))

(defun is-cursor-on-screen ()
  "Check if cursor is on the current screen"
  (core-data-input-mouse-cursor-on-screen *core*))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Screen Drawing
;;;----------------------------------------------------------------------------------

(defun clear-background (color)
  "Clear background (framebuffer) to color"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (rl-clear-color r g b a))           ; Set clear color
  (rl-clear-screen-buffers)             ; Clear current framebuffers
  (values))

(defun begin-drawing ()
  "Begin canvas (framebuffer) drawing"
  ;; WARNING: Previously to BeginDrawing() other render textures drawing could happen,
  ;; consequently the measure for update vs draw is not accurate (only the total frame time is accurate)
  (setf (core-data-time-current *core*) (get-time)) ; Number of elapsed seconds since InitTimer()
  (setf (core-data-time-update *core*) (- (core-data-time-current *core*) (core-data-time-previous *core*)))
  (setf (core-data-time-previous *core*) (core-data-time-current *core*))
  (rl-load-identity)                    ; Reset current matrix (modelview)
  (rl-mult-matrixf (matrix-to-float-v (core-data-window-screen-scale *core*))) ; Apply screen scaling
  (values))

(defun end-drawing ()
  "End canvas (framebuffer) drawing and swap buffers (double buffering)"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (when *automation-event-recording*
    (record-automation-event))          ; Event recording
  (swap-screen-buffer)                  ; Copy back buffer to front buffer (screen)
  ;; Frame time control system
  (setf (core-data-time-current *core*) (get-time))
  (setf (core-data-time-draw *core*) (- (core-data-time-current *core*) (core-data-time-previous *core*)))
  (setf (core-data-time-previous *core*) (core-data-time-current *core*))
  (setf (core-data-time-frame *core*) (+ (core-data-time-update *core*) (core-data-time-draw *core*)))
  ;; Wait for some milliseconds...
  (when (< (core-data-time-frame *core*) (core-data-time-target *core*))
    (wait-time (- (core-data-time-target *core*) (core-data-time-frame *core*)))
    (setf (core-data-time-current *core*) (get-time))
    (let ((wait-time (- (core-data-time-current *core*) (core-data-time-previous *core*))))
      (setf (core-data-time-previous *core*) (core-data-time-current *core*))
      (incf (core-data-time-frame *core*) wait-time))) ; Total frame time: update + draw + wait
  (poll-input-events)                   ; Poll user events (before next frame update)
  ;; SUPPORT_SCREEN_CAPTURE
  (when (is-key-pressed +key-f12+)
    (take-screenshot (format nil "screenshot~3,'0d.png" *screenshot-counter*))
    (incf *screenshot-counter*))
  (setf (core-data-time-frame-counter *core*)
        (logand (1+ (core-data-time-frame-counter *core*)) #xffffffff))
  (values))

;;; 2D/3D modes (from rcore.c)

(defun begin-mode-2d (camera)
  "Begin 2D mode with custom camera (2D)"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-load-identity)                    ; Reset current matrix (modelview)
  ;; Apply 2d camera transformation to modelview
  (rl-mult-matrixf (matrix-to-float-v (get-camera-matrix-2d camera))))

(defun end-mode-2d ()
  "Ends 2D mode with custom camera"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-load-identity)                    ; Reset current matrix (modelview)
  (when (= (rl-get-active-framebuffer) 0)
    ;; Apply screen scaling if required
    (rl-mult-matrixf (matrix-to-float-v (core-data-window-screen-scale *core*)))))

(defun %camera-projection (camera)
  "Camera projection as CameraProjection value (accepts the cl-raylib.cffi keywords)"
  (let ((projection (camera3d-projection camera)))
    (case projection
      (:camera-perspective +camera-perspective+)
      (:camera-orthographic +camera-orthographic+)
      (t projection))))

(defun begin-mode-3d (camera)
  "Begin 3D mode with custom camera (3D)"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-matrix-mode +rl-projection+)      ; Switch to projection matrix
  (rl-push-matrix)                      ; Save previous matrix, which contains the settings for the 2d ortho projection
  (rl-load-identity)                    ; Reset current matrix (projection)
  (let ((aspect (/ (float (core-data-window-current-fbo-width *core*) 1.0)
                   (float (core-data-window-current-fbo-height *core*) 1.0))))
    ;; NOTE: zNear and zFar values are important when computing depth buffer values
    (case (%camera-projection camera)
      (#.+camera-perspective+
       ;; Setup perspective projection
       (let* ((top (* (rl-get-cull-distance-near)
                      (tan (* (camera3d-fovy camera) 0.5d0 +deg2rad+))))
              (right (* top aspect)))
         (rl-frustum (- right) right (- top) top (rl-get-cull-distance-near) (rl-get-cull-distance-far))))
      (#.+camera-orthographic+
       ;; Setup orthographic projection
       (let* ((top (/ (camera3d-fovy camera) 2.0d0))
              (right (* top aspect)))
         (rl-ortho (- right) right (- top) top (rl-get-cull-distance-near) (rl-get-cull-distance-far))))))
  (rl-matrix-mode +rl-modelview+)       ; Switch back to modelview matrix
  (rl-load-identity)                    ; Reset current matrix (modelview)
  ;; Setup Camera view
  (let ((mat-view (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera))))
    (rl-mult-matrixf (matrix-to-float-v mat-view))) ; Multiply modelview matrix by view matrix (camera)
  (rl-enable-depth-test))               ; Enable DEPTH_TEST for 3D

(defun end-mode-3d ()
  "Ends 3D mode and returns to default 2D orthographic mode"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-matrix-mode +rl-projection+)      ; Switch to projection matrix
  (rl-pop-matrix)                       ; Restore previous matrix (projection) from matrix stack
  (rl-matrix-mode +rl-modelview+)       ; Switch back to modelview matrix
  (rl-load-identity)                    ; Reset current matrix (modelview)
  (when (= (rl-get-active-framebuffer) 0)
    ;; Apply screen scaling if required
    (rl-mult-matrixf (matrix-to-float-v (core-data-window-screen-scale *core*))))
  (rl-disable-depth-test))              ; Disable DEPTH_TEST for 2D

;;; Texture mode, blend mode and scissor mode (from rcore.c)

(defun begin-texture-mode (target)
  "Begin drawing to render texture"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-enable-framebuffer (render-texture-id target)) ; Enable render target
  (let ((width (texture-width (render-texture-texture target)))
        (height (texture-height (render-texture-texture target))))
    ;; Set viewport and RLGL internal framebuffer size
    (rl-viewport 0 0 width height)
    (rl-set-framebuffer-width width)
    (rl-set-framebuffer-height height)
    (rl-matrix-mode +rl-projection+)    ; Switch to projection matrix
    (rl-load-identity)                  ; Reset current matrix (projection)
    ;; Set orthographic projection to current framebuffer size
    ;; NOTE: Configured top-left corner as (0, 0)
    (rl-ortho 0 width height 0 0.0 1.0)
    (rl-matrix-mode +rl-modelview+)     ; Switch back to modelview matrix
    (rl-load-identity)                  ; Reset current matrix (modelview)
    ;; Setup current width/height for proper aspect ratio
    ;; calculation when using BeginMode3D()
    (setf (core-data-window-current-fbo-width *core*) width
          (core-data-window-current-fbo-height *core*) height
          (core-data-window-using-fbo *core*) t)))

(defun end-texture-mode ()
  "Ends drawing to render texture"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-disable-framebuffer)              ; Disable render target (fbo)
  ;; Set viewport to default framebuffer size
  (setup-viewport (core-data-window-render-width *core*) (core-data-window-render-height *core*))
  ;; Go back to the modelview state from BeginDrawing since we are back to the default FBO
  (rl-matrix-mode +rl-modelview+)       ; Switch back to modelview matrix
  (rl-load-identity)                    ; Reset current matrix (modelview)
  (rl-mult-matrixf (matrix-to-float-v (core-data-window-screen-scale *core*))) ; Apply screen scaling if required
  ;; Reset current fbo to screen size
  (setf (core-data-window-current-fbo-width *core*) (core-data-window-render-width *core*)
        (core-data-window-current-fbo-height *core*) (core-data-window-render-height *core*)
        (core-data-window-using-fbo *core*) nil))

;; Begin custom shader mode
(defun begin-shader-mode (shader)
  "Begin custom shader drawing"
  (rl-set-shader (shader-id shader) (shader-locs shader)))

;; End custom shader mode (returns to default shader)
(defun end-shader-mode ()
  "End custom shader drawing (use default shader)"
  (rl-set-shader (rl-get-shader-id-default) (rl-get-shader-locs-default)))

(defun begin-blend-mode (mode)
  "Begin blending mode (alpha, additive, multiplied, subtract, custom)"
  (rl-set-blend-mode mode))

(defun end-blend-mode ()
  "End blending mode (reset to default: alpha blending)"
  (rl-set-blend-mode +blend-alpha+))

(defun begin-scissor-mode (x y width height)
  "Begin scissor mode (define screen area for following drawing)"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-enable-scissor-test)
  (if (and (not (core-data-window-using-fbo *core*))
           (logtest (core-data-window-flags *core*) +flag-window-highdpi+))
      (let ((scale (get-window-scale-dpi)))
        (rl-scissor (truncate (* x (vx scale)))
                    (truncate (- (core-data-window-current-fbo-height *core*) (* (+ y height) (vy scale))))
                    (truncate (* width (vx scale))) (truncate (* height (vy scale)))))
      (rl-scissor x (- (core-data-window-current-fbo-height *core*) (+ y height)) width height)))

(defun end-scissor-mode ()
  "End scissor mode"
  (rl-draw-render-batch-active)         ; Update and draw internal render batch
  (rl-disable-scissor-test))

;;; VR Stereo Rendering (from rcore.c)

;; Begin VR drawing configuration
(defun begin-vr-stereo-mode (config)
  "Begin stereo rendering (requires VR simulator)"
  (rl-enable-stereo-render)
  ;; Set stereo render matrices
  (rl-set-matrix-projection-stereo (aref (vr-stereo-config-projection config) 0) (aref (vr-stereo-config-projection config) 1))
  (rl-set-matrix-view-offset-stereo (aref (vr-stereo-config-view-offset config) 0) (aref (vr-stereo-config-view-offset config) 1)))

;; End VR drawing process (and desktop mirror)
(defun end-vr-stereo-mode ()
  "End stereo rendering (requires VR simulator)"
  (rl-disable-stereo-render))

;; Load VR stereo config for VR simulator device parameters
(defun load-vr-stereo-config (device)
  "Load VR stereo config for VR simulator device parameters"
  (let ((config (make-vr-stereo-config)))
    (if (/= (rl-get-version) +rl-opengl-11+)
        (let* (;; Compute aspect ratio
               (aspect (/ (* (float (vr-device-info-h-resolution device) 1.0) 0.5)
                          (float (vr-device-info-v-resolution device) 1.0)))
               ;; Compute lens parameters
               (h-screen-size (vr-device-info-h-screen-size device))
               (lens-shift (/ (- (* h-screen-size 0.25) (* (vr-device-info-lens-separation-distance device) 0.5))
                              h-screen-size)))
          (setf (aref (vr-stereo-config-left-lens-center config) 0) (+ 0.25 lens-shift)
                (aref (vr-stereo-config-left-lens-center config) 1) 0.5
                (aref (vr-stereo-config-right-lens-center config) 0) (- 0.75 lens-shift)
                (aref (vr-stereo-config-right-lens-center config) 1) 0.5
                (aref (vr-stereo-config-left-screen-center config) 0) 0.25
                (aref (vr-stereo-config-left-screen-center config) 1) 0.5
                (aref (vr-stereo-config-right-screen-center config) 0) 0.75
                (aref (vr-stereo-config-right-screen-center config) 1) 0.5)
          ;; Compute distortion scale parameters
          ;; NOTE: To get lens max radius, lensShift must be normalized to [-1..1]
          (let* ((lens-radius (abs (- -1.0 (* 4.0 lens-shift))))
                 (lens-radius-sq (* lens-radius lens-radius))
                 (k (vr-device-info-lens-distortion-values device))
                 (distortion-scale (+ (aref k 0)
                                      (* (aref k 1) lens-radius-sq)
                                      (* (aref k 2) lens-radius-sq lens-radius-sq)
                                      (* (aref k 3) lens-radius-sq lens-radius-sq lens-radius-sq)))
                 (norm-screen-width 0.5)
                 (norm-screen-height 1.0))
            (setf (aref (vr-stereo-config-scale-in config) 0) (/ 2.0 norm-screen-width)
                  (aref (vr-stereo-config-scale-in config) 1) (/ (/ 2.0 norm-screen-height) aspect)
                  (aref (vr-stereo-config-scale config) 0) (/ (* norm-screen-width 0.5) distortion-scale)
                  (aref (vr-stereo-config-scale config) 1) (/ (* norm-screen-height 0.5 aspect) distortion-scale))
            ;; Fovy is normally computed with: 2*atan2f(device.vScreenSize, 2*device.eyeToScreenDistance)
            ;; ...but with lens distortion it is increased (see Oculus SDK Documentation)
            (let* ((fovy (* 2.0 (atan (* (vr-device-info-v-screen-size device) 0.5 distortion-scale)
                                      (vr-device-info-eye-to-screen-distance device)))) ; Really need distortionScale?
                   ;; Compute camera projection matrices
                   (proj-offset (* 4.0 lens-shift)) ; Scaled to projection space coordinates [-1..1]
                   (proj (matrix-perspective fovy aspect (rl-get-cull-distance-near) (rl-get-cull-distance-far)))
                   (ipd (vr-device-info-interpupillary-distance device)))
              (setf (aref (vr-stereo-config-projection config) 0) (matrix-multiply proj (matrix-translate proj-offset 0.0 0.0))
                    (aref (vr-stereo-config-projection config) 1) (matrix-multiply proj (matrix-translate (- proj-offset) 0.0 0.0)))
              ;; Compute camera transformation matrices
              ;; NOTE: Camera movement might seem more natural if modelling the head
              ;; Axis of rotation is the base of the head, so adding some y (base of head to eye level
              ;; and -z (center of head to eye protrusion) to the camera positions
              (setf (aref (vr-stereo-config-view-offset config) 0) (matrix-translate (* ipd 0.5) 0.075 0.045)
                    (aref (vr-stereo-config-view-offset config) 1) (matrix-translate (* (- ipd) 0.5) 0.075 0.045)))))
        (trace-log +log-warning+ "RLGL: VR Simulator not supported on OpenGL 1.1"))
    config))

;; Unload VR stereo config properties
(defun unload-vr-stereo-config (config)
  "Unload VR stereo config"
  (declare (ignore config))
  (trace-log +log-info+ "UnloadVrStereoConfig not implemented in rcore.c"))

;;; Shaders Management (from rcore.c)

;; Load shader from files and bind default locations
;; NOTE: If shader filename is NULL, using default vertex/fragment shaders
(defun load-shader (vs-file-name fs-file-name)
  "Load shader from files and bind default locations"
  (let ((v-shader-str (when vs-file-name (load-file-text vs-file-name)))
        (f-shader-str (when fs-file-name (load-file-text fs-file-name))))
    (when (and (null v-shader-str) (null f-shader-str))
      (trace-log +log-warning+ "SHADER: Shader files provided are not valid, using default shader"))
    (load-shader-from-memory v-shader-str f-shader-str)))

;; Load shader from code strings and bind default locations
(defun load-shader-from-memory (vs-code fs-code)
  "Load shader from code strings and bind default locations"
  (let ((shader (make-shader)))
    (setf (shader-id shader) (rl-load-shader-program vs-code fs-code))
    (cond ((= (shader-id shader) 0)
           ;; Shader could not be loaded but still loading the location points to avoid potential crashes
           ;; NOTE: All locations set to -1 (no location found)
           (setf (shader-locs shader) (make-array +rl-max-shader-locations+ :initial-element -1)))
          ((= (shader-id shader) (rl-get-shader-id-default))
           (setf (shader-locs shader) (rl-get-shader-locs-default)))
          ((> (shader-id shader) 0)
           ;; After custom shader loading, trying to set default location names
           ;; Default shader attribute locations have been binded before linking:
           ;;  - vertex position location    = 0
           ;;  - vertex texcoord location    = 1
           ;;  - vertex normal location      = 2
           ;;  - vertex color location       = 3
           ;;  - vertex tangent location     = 4
           ;;  - vertex texcoord2 location   = 5
           ;;  - vertex boneIndices location = 6
           ;;  - vertex boneWeights location = 7

           ;; NOTE: If any location is not found, loc point becomes -1

           ;; Load shader locations array
           ;; NOTE: All locations set to -1 (no location)
           (let ((locs (make-array +rl-max-shader-locations+ :initial-element -1))
                 (id (shader-id shader)))
             ;; Get handles to GLSL input attribute locations
             (setf (aref locs +shader-loc-vertex-position+) (rl-get-location-attrib id +rl-default-shader-attrib-name-position+)
                   (aref locs +shader-loc-vertex-texcoord01+) (rl-get-location-attrib id +rl-default-shader-attrib-name-texcoord+)
                   (aref locs +shader-loc-vertex-texcoord02+) (rl-get-location-attrib id +rl-default-shader-attrib-name-texcoord2+)
                   (aref locs +shader-loc-vertex-normal+) (rl-get-location-attrib id +rl-default-shader-attrib-name-normal+)
                   (aref locs +shader-loc-vertex-tangent+) (rl-get-location-attrib id +rl-default-shader-attrib-name-tangent+)
                   (aref locs +shader-loc-vertex-color+) (rl-get-location-attrib id +rl-default-shader-attrib-name-color+)
                   (aref locs +shader-loc-vertex-boneids+) (rl-get-location-attrib id +rl-default-shader-attrib-name-boneindices+)
                   (aref locs +shader-loc-vertex-boneweights+) (rl-get-location-attrib id +rl-default-shader-attrib-name-boneweights+)
                   (aref locs +shader-loc-vertex-instancetransform+) (rl-get-location-attrib id +rl-default-shader-attrib-name-instancetransform+))

             ;; Get handles to GLSL uniform locations (vertex shader)
             (setf (aref locs +shader-loc-matrix-mvp+) (rl-get-location-uniform id +rl-default-shader-uniform-name-mvp+)
                   (aref locs +shader-loc-matrix-view+) (rl-get-location-uniform id +rl-default-shader-uniform-name-view+)
                   (aref locs +shader-loc-matrix-projection+) (rl-get-location-uniform id +rl-default-shader-uniform-name-projection+)
                   (aref locs +shader-loc-matrix-model+) (rl-get-location-uniform id +rl-default-shader-uniform-name-model+)
                   (aref locs +shader-loc-matrix-normal+) (rl-get-location-uniform id +rl-default-shader-uniform-name-normal+)
                   (aref locs +shader-loc-matrix-bonetransforms+) (rl-get-location-uniform id +rl-default-shader-uniform-name-bonematrices+))

             ;; Get handles to GLSL uniform locations (fragment shader)
             (setf (aref locs +shader-loc-color-diffuse+) (rl-get-location-uniform id +rl-default-shader-uniform-name-color+)
                   (aref locs +shader-loc-map-diffuse+) (rl-get-location-uniform id +rl-default-shader-sampler2d-name-texture0+) ; SHADER_LOC_MAP_ALBEDO
                   (aref locs +shader-loc-map-specular+) (rl-get-location-uniform id +rl-default-shader-sampler2d-name-texture1+) ; SHADER_LOC_MAP_METALNESS
                   (aref locs +shader-loc-map-normal+) (rl-get-location-uniform id +rl-default-shader-sampler2d-name-texture2+))
             (setf (shader-locs shader) locs))))
    shader))

;; Check if shader is valid (loaded on GPU)
(defun is-shader-valid (shader)
  "Check if a shader is valid (loaded on GPU)"
  (and (> (shader-id shader) 0)         ; Validate shader id (GPU loaded successfully)
       (not (null (shader-locs shader))))) ; Validate memory has been allocated for default shader locations

;; Unload shader from GPU memory (VRAM)
(defun unload-shader (shader)
  "Unload shader from GPU memory (VRAM)"
  (when (/= (shader-id shader) (rl-get-shader-id-default))
    (rl-unload-shader-program (shader-id shader))
    ;; NOTE: If shader loading failed, it should be 0
    (setf (shader-locs shader) nil)))

;; Get shader uniform location
(defun get-shader-location (shader uniform-name)
  "Get shader uniform location"
  (rl-get-location-uniform (shader-id shader) uniform-name))

;; Get shader attribute location
(defun get-shader-location-attrib (shader attrib-name)
  "Get shader attribute location"
  (rl-get-location-attrib (shader-id shader) attrib-name))

;; Set shader uniform value
;; NOTE: VALUE is a number, vec2/vec3/vec4, sequence, specialized vector or foreign pointer
(defun set-shader-value (shader loc-index value uniform-type)
  "Set shader uniform value"
  (set-shader-value-v shader loc-index value uniform-type 1))

;; Set shader uniform value vector
(defun set-shader-value-v (shader loc-index value uniform-type count)
  "Set shader uniform value vector"
  (when (> loc-index -1)
    (rl-enable-shader (shader-id shader))
    (rl-set-uniform loc-index value uniform-type count)))
    ;;rlDisableShader();      // Avoid resetting current shader program, in case other uniforms are set

;; Set shader uniform value (matrix 4x4)
(defun set-shader-value-matrix (shader loc-index mat)
  "Set shader uniform value (matrix 4x4)"
  (when (> loc-index -1)
    (rl-enable-shader (shader-id shader))
    (rl-set-uniform-matrix loc-index mat)))

;; Set shader uniform value for texture
(defun set-shader-value-texture (shader loc-index texture)
  "Set shader uniform value for texture (sampler2d)"
  (when (> loc-index -1)
    (rl-enable-shader (shader-id shader))
    (rl-set-uniform-sampler loc-index (texture-id texture))))

;;; Screen-space-related functions (from rcore.c)

(defun get-screen-to-world-ray (position camera)
  "Get a ray trace from screen position (i.e mouse)"
  (get-screen-to-world-ray-ex position camera (get-screen-width) (get-screen-height)))

(defun get-mouse-ray (position camera)
  "Get a ray trace from mouse position (old raylib name, kept for cl-raylib.cffi compatibility)"
  (get-screen-to-world-ray position camera))

(defun %camera-projection-matrix (camera width height)
  "Projection matrix used by GetScreenToWorldRayEx() and GetWorldToScreenEx()"
  (case (%camera-projection camera)
    (#.+camera-perspective+
     ;; Calculate projection matrix from perspective
     (matrix-perspective (* (camera3d-fovy camera) +deg2rad+)
                         (/ (float width 1d0) (float height 1d0))
                         (rl-get-cull-distance-near) (rl-get-cull-distance-far)))
    (#.+camera-orthographic+
     (let* ((aspect (/ (float width 1d0) (float height 1d0)))
            (top (/ (camera3d-fovy camera) 2.0d0))
            (right (* top aspect)))
       ;; Calculate projection matrix from orthographic
       (matrix-ortho (- right) right (- top) top (rl-get-cull-distance-near) (rl-get-cull-distance-far))))
    (t (matrix-identity))))

(defun get-screen-to-world-ray-ex (position camera width height)
  "Get a ray trace from the screen position (i.e mouse) within a specific section of the screen"
  ;; Calculate normalized device coordinates
  ;; NOTE: y value is negative
  (let* ((x (- (/ (* 2.0 (vx position)) (float width 1.0)) 1.0))
         (y (- 1.0 (/ (* 2.0 (vy position)) (float height 1.0))))
         ;; Calculate view matrix from camera look at
         (mat-view (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera)))
         (mat-proj (%camera-projection-matrix camera width height))
         ;; Unproject far/near points
         (near-point (vector3-unproject (vec3 x y 0.0) mat-proj mat-view))
         (far-point (vector3-unproject (vec3 x y 1.0) mat-proj mat-view))
         ;; Unproject the mouse cursor in the near plane
         ;; It is needed as the source position because orthographic projects,
         ;; compared to perspective doesn't have a convergence point,
         ;; meaning that the "eye" of the camera is more like a plane than a point
         (camera-plane-pointer-pos (vector3-unproject (vec3 x y -1.0) mat-proj mat-view))
         ;; Calculate normalized direction vector
         (direction (vector3-normalize (vector3-subtract far-point near-point))))
    (make-ray :position (case (%camera-projection camera)
                          (#.+camera-perspective+ (camera3d-position camera))
                          (#.+camera-orthographic+ camera-plane-pointer-pos)
                          (t (vec3 0.0 0.0 0.0)))
              :direction direction)))

(defun get-camera-matrix (camera)
  "Get transform matrix for camera"
  (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera)))

(defun get-camera-matrix-2d (camera)
  "Get camera 2d transform matrix"
  ;; The camera in world-space is set by
  ;;   1. Move it to target
  ;;   2. Rotate by -rotation and scale by (1/zoom)
  ;;      When setting higher scale, it's more intuitive for the world to become bigger (= camera become smaller),
  ;;      not for the camera getting bigger, hence the invert. Same deal with rotation
  ;;   3. Move it by (-offset);
  ;;      Offset defines target transform relative to screen, but since effectively "moving" screen (camera)
  ;;      it needs to be moved into opposite direction (inverse transform)
  ;; Having camera transform in world-space, inverse of it gives the modelview transform
  ;; Since (A*B*C)' = C'*B'*A', the modelview is
  ;;   1. Move to offset
  ;;   2. Rotate and Scale
  ;;   3. Move by -target
  (let ((mat-origin (matrix-translate (- (vx (camera2d-target camera))) (- (vy (camera2d-target camera))) 0.0))
        (mat-rotation (matrix-rotate (vec3 0.0 0.0 1.0) (* (camera2d-rotation camera) +deg2rad+)))
        (mat-scale (matrix-scale (camera2d-zoom camera) (camera2d-zoom camera) 1.0))
        (mat-translation (matrix-translate (vx (camera2d-offset camera)) (vy (camera2d-offset camera)) 0.0)))
    (matrix-multiply (matrix-multiply mat-origin (matrix-multiply mat-scale mat-rotation)) mat-translation)))

(defun get-world-to-screen (position camera)
  "Get the screen space position from a 3d world space position"
  (get-world-to-screen-ex position camera (get-screen-width) (get-screen-height)))

(defun get-world-to-screen-ex (position camera width height)
  "Get size position for a 3d world space position (useful for texture drawing)"
  ;; Calculate projection matrix (from perspective instead of frustum
  (let* ((mat-proj (%camera-projection-matrix camera width height))
         ;; Calculate view matrix from camera look at (and transpose it)
         (mat-view (matrix-look-at (camera3d-position camera) (camera3d-target camera) (camera3d-up camera)))
         ;; Convert world position vector to quaternion
         (world-pos (vec4 (vx position) (vy position) (vz position) 1.0)))
    ;; Transform world position to view
    (setf world-pos (quaternion-transform world-pos mat-view))
    ;; Transform result to projection (clip space position)
    (setf world-pos (quaternion-transform world-pos mat-proj))
    ;; Calculate normalized device coordinates (inverted y)
    (let ((ndc-x (/ (vx world-pos) (vw world-pos)))
          (ndc-y (/ (- (vy world-pos)) (vw world-pos))))
      ;; Calculate 2d screen position vector
      (vec2 (* (/ (+ ndc-x 1.0) 2.0) (float width 1.0))
            (* (/ (+ ndc-y 1.0) 2.0) (float height 1.0))))))

(defun get-world-to-screen-2d (position camera)
  "Get the screen space position for a 2d camera world space position"
  (let* ((mat-camera (get-camera-matrix-2d camera))
         (transform (vector3-transform (vec3 (vx position) (vy position) 0.0) mat-camera)))
    (vec2 (vx transform) (vy transform))))

(defun get-screen-to-world-2d (position camera)
  "Get the world space position for a 2d camera screen space position"
  (let* ((inv-mat-camera (matrix-invert (get-camera-matrix-2d camera)))
         (transform (vector3-transform (vec3 (vx position) (vy position) 0.0) inv-mat-camera)))
    (vec2 (vx transform) (vy transform))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Timing
;;;----------------------------------------------------------------------------------

;; NOTE: Functions with a platform-specific implementation on glfw.lisp
;;(defun get-time ())

(defun set-target-fps (fps)
  "Set target FPS (maximum)"
  (if (< fps 1)
      (setf (core-data-time-target *core*) 0.0d0)
      (setf (core-data-time-target *core*) (/ 1.0d0 fps)))
  (trace-log-info "TIMER: Target time per frame: ~,3f milliseconds"
                  (* (float (core-data-time-target *core*) 1.0) 1000.0))
  (values))

;; FPS_CAPTURE_FRAMES_COUNT, FPS_AVERAGE_TIME_SECONDS, FPS_STEP
(defconstant +fps-capture-frames-count+ 30 "30 captures")
(defconstant +fps-average-time-seconds+ 0.5 "500 milliseconds")
(defconstant +fps-step+ (/ +fps-average-time-seconds+ +fps-capture-frames-count+))

(defvar *fps-index* 0)
(defvar *fps-history* (make-array +fps-capture-frames-count+ :element-type 'single-float :initial-element 0.0))
(defvar *fps-average* 0.0)
(defvar *fps-last* 0.0)

(defun get-fps ()
  "Get current FPS
NOTE: Calculating an average framerate"
  (let ((fps 0)
        (fps-frame (get-frame-time)))
    ;; If reseting the window, reset the FPS info
    (when (= (core-data-time-frame-counter *core*) 0)
      (setf *fps-average* 0.0
            *fps-last* 0.0
            *fps-index* 0)
      (fill *fps-history* 0.0))
    (if (/= fps-frame 0)
        (progn
          (when (> (- (get-time) *fps-last*) +fps-step+)
            (setf *fps-last* (float (get-time) 1.0))
            (setf *fps-index* (mod (1+ *fps-index*) +fps-capture-frames-count+))
            (decf *fps-average* (aref *fps-history* *fps-index*))
            (setf (aref *fps-history* *fps-index*) (/ fps-frame +fps-capture-frames-count+))
            (incf *fps-average* (aref *fps-history* *fps-index*)))
          (setf fps (round (/ 1.0 *fps-average*))))
        (setf fps 0))
    fps))

(defun get-frame-time ()
  "Get time in seconds for last frame drawn (delta time)"
  (float (core-data-time-frame *core*) 1.0))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Custom frame control
;;;----------------------------------------------------------------------------------

;; NOTE: Functions with a platform-specific implementation on glfw.lisp
;;(defun swap-screen-buffer ())
;;(defun poll-input-events ())

(defun wait-time (seconds)
  "Wait for some time (stop program execution)
NOTE: Sleep() granularity could be around 10 ms, it means, Sleep() could
take longer than expected... for that reason a partial busy wait loop is used
(SUPPORT_PARTIALBUSY_WAIT_LOOP)"
  (when (< seconds 0) (return-from wait-time))     ; Security check
  (let* ((destination-time (+ (get-time) seconds))
         ;; NOTE: Reserve a percentage of the time for busy waiting
         (sleep-seconds (- seconds (* seconds 0.05d0))))
    ;; System halt functions
    (when (> sleep-seconds 0) (sleep sleep-seconds))
    (loop while (< (get-time) destination-time)))
  (values))

;;; cl-raylib extras: simple performance timers built on get-time

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

(defun get-timing-info ()
  "Get detailed timing information for debugging"
  (list :current-time (core-data-time-current *core*)
        :frame-time (core-data-time-frame *core*)
        :update-time (core-data-time-update *core*)
        :draw-time (core-data-time-draw *core*)
        :target-fps (if (> (core-data-time-target *core*) 0)
                        (round (/ 1.0d0 (core-data-time-target *core*)))
                        0)
        :current-fps (get-fps)
        :frame-counter (core-data-time-frame-counter *core*)))

(defun reset-timing ()
  "Reset timing statistics"
  (setf (core-data-time-frame-counter *core*) 0)
  (init-timer))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Misc
;;;----------------------------------------------------------------------------------

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

(defun take-screenshot (file-name)
  "Takes a screenshot of current screen (saved a .png)"
  ;; Security check to (partially) avoid malicious code
  (when (find #\' file-name)
    (trace-log-warning "SYSTEM: Provided fileName could be potentially malicious, avoid [\\'] character")
    (return-from take-screenshot))
  ;; Apply content scaling if required
  (let* ((scale (if (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+)
                    (get-window-scale-dpi)
                    (vec2 1.0 1.0)))
         (width (truncate (* (float (core-data-window-render-width *core*) 1.0) (vx scale))))
         (height (truncate (* (float (core-data-window-render-height *core*) 1.0) (vy scale))))
         (img-data (rl-read-screen-pixels width height))
         (image (make-image :data img-data :width width :height height :mipmaps 1
                            :format +pixelformat-uncompressed-r8g8b8a8+))
         (path (if (not (is-path-absolute file-name))
                   (format nil "~a/~a" (core-data-storage-base-path *core*) file-name)
                   file-name)))
    (export-image image path)           ; WARNING: Module required: rtextures
    (if (file-exists path)
        (trace-log-info "SYSTEM: [~a] Screenshot taken successfully" path)
        (trace-log-warning "SYSTEM: [~a] Screenshot could not be saved" path)))
  (values))

(defun set-config-flags (flags)
  "Set up window configuration flags (view FLAGS)
NOTE: This function is expected to be called before window creation,
because it sets up some flags for the window creation process
To configure window states after creation, use SetWindowState()"
  (when (core-data-window-ready *core*)
    (trace-log-warning "WINDOW: SetConfigFlags called after window initialization, Use \"SetWindowState\" to set flags instead"))
  ;; Selected flags are set but not evaluated at this point,
  ;; flag evaluation happens at InitWindow() or SetWindowState()
  (%flag-set (core-data-window-flags *core*) (%flags flags))
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: File system
;;; NOTE: Most file system functions are in utils.lisp
;;;----------------------------------------------------------------------------------

(defun get-working-directory ()
  "Get current working directory (without trailing separator)"
  (let ((path (namestring (uiop:getcwd))))
    (if (and (> (length path) 1) (char= (char path (1- (length path))) #\/))
        (subseq path 0 (1- (length path)))
        path)))

(defun get-application-directory ()
  "Get the directory of the running application (with trailing separator)"
  (let ((exe #+sbcl sb-ext:*runtime-pathname* #-sbcl nil))
    (if exe
        (directory-namestring (truename exe))
        "./")))

(defun is-file-dropped ()
  "Check if a file has been dropped into window"
  (> (core-data-window-drop-file-count *core*) 0))

(defun load-dropped-files ()
  "Load dropped filepaths"
  (make-file-path-list :capacity (core-data-window-drop-file-count *core*)
                       :count (core-data-window-drop-file-count *core*)
                       :paths (core-data-window-drop-filepaths *core*)))

(defun file-path-list-path (file-path-list index)
  "Get a specific path from a FilePathList by index (cl-raylib helper for FilePathList.paths[index])"
  (when (and (>= index 0) (< index (file-path-list-count file-path-list)))
    (nth index (file-path-list-paths file-path-list))))

(defun unload-dropped-files (files)
  "Unload dropped filepaths"
  ;; WARNING: files pointers are the same as internal ones
  (when (> (file-path-list-count files) 0)
    (setf (core-data-window-drop-file-count *core*) 0
          (core-data-window-drop-filepaths *core*) nil))
  (values))

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

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Automation Events Recording and Playing
;;;----------------------------------------------------------------------------------

;;; Automation events type (AutomationEventType)
(defconstant +event-none+ 0)
;; Input events
(defconstant +input-key-up+ 1 "param[0]: key")
(defconstant +input-key-down+ 2 "param[0]: key")
(defconstant +input-key-pressed+ 3 "param[0]: key")
(defconstant +input-key-released+ 4 "param[0]: key")
(defconstant +input-mouse-button-up+ 5 "param[0]: button")
(defconstant +input-mouse-button-down+ 6 "param[0]: button")
(defconstant +input-mouse-position+ 7 "param[0]: x, param[1]: y")
(defconstant +input-mouse-wheel-motion+ 8 "param[0]: x delta, param[1]: y delta")
(defconstant +input-gamepad-connect+ 9 "param[0]: gamepad")
(defconstant +input-gamepad-disconnect+ 10 "param[0]: gamepad")
(defconstant +input-gamepad-button-up+ 11 "param[0]: button")
(defconstant +input-gamepad-button-down+ 12 "param[0]: button")
(defconstant +input-gamepad-axis-motion+ 13 "param[0]: axis, param[1]: delta")
(defconstant +input-touch-up+ 14 "param[0]: id")
(defconstant +input-touch-down+ 15 "param[0]: id")
(defconstant +input-touch-position+ 16 "param[0]: x, param[1]: y")
(defconstant +input-gesture+ 17 "param[0]: gesture")
;; Window events
(defconstant +window-close+ 18 "no params")
(defconstant +window-maximize+ 19 "no params")
(defconstant +window-minimize+ 20 "no params")
(defconstant +window-resize+ 21 "param[0]: width, param[1]: height")
;; Custom events
(defconstant +action-take-screenshot+ 22 "no params")
(defconstant +action-settargetfps+ 23 "param[0]: fps")

;; Event type name strings, required for export
(defparameter *auto-event-type-name*
  #("EVENT_NONE" "INPUT_KEY_UP" "INPUT_KEY_DOWN" "INPUT_KEY_PRESSED" "INPUT_KEY_RELEASED"
    "INPUT_MOUSE_BUTTON_UP" "INPUT_MOUSE_BUTTON_DOWN" "INPUT_MOUSE_POSITION" "INPUT_MOUSE_WHEEL_MOTION"
    "INPUT_GAMEPAD_CONNECT" "INPUT_GAMEPAD_DISCONNECT" "INPUT_GAMEPAD_BUTTON_UP" "INPUT_GAMEPAD_BUTTON_DOWN"
    "INPUT_GAMEPAD_AXIS_MOTION" "INPUT_TOUCH_UP" "INPUT_TOUCH_DOWN" "INPUT_TOUCH_POSITION" "INPUT_GESTURE"
    "WINDOW_CLOSE" "WINDOW_MAXIMIZE" "WINDOW_MINIMIZE" "WINDOW_RESIZE"
    "ACTION_TAKE_SCREENSHOT" "ACTION_SETTARGETFPS"))

(defun %make-automation-events (count)
  (let ((events (make-array count)))
    (dotimes (i count events) (setf (aref events i) (make-automation-event)))))

(defun %parse-ints (line start count)
  "sscanf() helper: read COUNT integers from LINE starting at START"
  (let ((pos start) (values '()))
    (dotimes (i count)
      (multiple-value-bind (value next) (parse-integer line :start pos :junk-allowed t)
        (unless value (return))
        (push value values)
        (setf pos next)))
    (nreverse values)))

(defun load-automation-event-list (file-name)
  "Load automation events list from file, NIL for empty list, capacity = MAX_AUTOMATION_EVENTS"
  ;; Allocate and empty automation event list, ready to record new events
  (let ((list (make-automation-event-list :capacity +max-automation-events+
                                          :events (%make-automation-events +max-automation-events+))))
    (if (null file-name)
        (trace-log-info "AUTOMATION: New empty events list loaded successfully")
        (progn
          ;; Load events file (text)
          (with-open-file (rae-file file-name :direction :input :if-does-not-exist nil)
            (when rae-file
              (let ((counter 0))
                (loop for buffer = (read-line rae-file nil nil)
                      while buffer
                      do (when (plusp (length buffer))
                           (case (char buffer 0)
                             (#\c (let ((count (first (%parse-ints buffer 1 1))))
                                    (when count (setf (automation-event-list-count list) count))))
                             (#\e (if (< counter (automation-event-list-capacity list))
                                      (let ((values (%parse-ints buffer 1 6))
                                            (event (aref (automation-event-list-events list) counter)))
                                        (when (= (length values) 6)
                                          (setf (automation-event-frame event) (first values)
                                                (automation-event-type event) (second values))
                                          (loop for i from 0 below 4
                                                do (setf (aref (automation-event-params event) i) (nth (+ i 2) values))))
                                        (incf counter))
                                      (trace-log-warning "AUTOMATION: Event goes beyond automated list capacity (MAX: ~d): ~a"
                                                         (automation-event-list-capacity list) buffer))))))
                (when (/= counter (automation-event-list-count list))
                  (trace-log-warning "AUTOMATION: Events read from file [~d] do not mach event count specified [~d]"
                                     counter (automation-event-list-count list))
                  (setf (automation-event-list-count list) counter))
                (trace-log-info "AUTOMATION: Events file loaded successfully"))))
          (trace-log-info "AUTOMATION: Events loaded from file: ~d" (automation-event-list-count list))))
    list))

(defun unload-automation-event-list (list)
  "Unload automation events list from file"
  (setf (automation-event-list-events list) #()
        (automation-event-list-count list) 0
        (automation-event-list-capacity list) 0)
  (values))

(defun export-automation-event-list (list file-name)
  "Export automation events list as text file"
  ;; Export events as text
  ;; NOTE: Save to memory buffer and SaveFileText()
  (let ((txt-data
          (with-output-to-string (out)
            (format out "#~%")
            (format out "# Automation events exporter v1.0 - raylib automation events list~%")
            (format out "#~%")
            (format out "#    c <events_count>~%")
            (format out "#    e <frame> <event_type> <param0> <param1> <param2> <param3> // <event_type_name>~%")
            (format out "#~%")
            (format out "# more info and bugs-report:  github.com/raysan5/raylib~%")
            (format out "# feedback and support:       ray[at]raylib.com~%")
            (format out "#~%")
            (format out "# Copyright (c) 2023-2026 Ramon Santamaria (@raysan5)~%")
            (format out "#~%~%")
            ;; Add events data
            (format out "c ~d~%" (automation-event-list-count list))
            (loop for i from 0 below (automation-event-list-count list)
                  for event = (aref (automation-event-list-events list) i)
                  for params = (automation-event-params event)
                  do (format out "e ~d ~d ~d ~d ~d ~d // Event: ~a~%"
                             (automation-event-frame event) (automation-event-type event)
                             (aref params 0) (aref params 1) (aref params 2) (aref params 3)
                             (aref *auto-event-type-name* (automation-event-type event)))))))
    ;; NOTE: Text data size exported is determined by '\0' (NULL) character
    (save-file-text file-name txt-data)))

(defun set-automation-event-list (list)
  "Setup automation event list to record to"
  (setf *current-event-list* list)
  (values))

(defun set-automation-event-base-frame (frame)
  "Set automation event internal base frame to start recording"
  (setf (core-data-time-frame-counter *core*) frame)
  (values))

(defun start-automation-event-recording ()
  "Start recording automation events (AutomationEventList must be set)"
  (setf *automation-event-recording* t)
  (values))

(defun stop-automation-event-recording ()
  "Stop recording automation events"
  (setf *automation-event-recording* nil)
  (values))

(defun play-automation-event (event)
  "Play a recorded automation event"
  ;; WARNING: When should event be played? After/before/replace PollInputEvents()? -> Up to the user!
  (unless *automation-event-recording*
    (let ((params (automation-event-params event)))
      (flet ((param (i) (aref params i)))
        (case (automation-event-type event)
          ;; Input event
          (#.+input-key-up+                 ; param[0]: key
           (setf (aref (core-data-input-keyboard-current-key-state *core*) (param 0)) 0))
          (#.+input-key-down+               ; param[0]: key
           (setf (aref (core-data-input-keyboard-current-key-state *core*) (param 0)) 1)
           (when (= (aref (core-data-input-keyboard-previous-key-state *core*) (param 0)) 0)
             (when (< (core-data-input-keyboard-key-pressed-queue-count *core*) +max-key-pressed-queue+)
               ;; Add character to the queue
               (setf (aref (core-data-input-keyboard-key-pressed-queue *core*)
                           (core-data-input-keyboard-key-pressed-queue-count *core*))
                     (param 0))
               (incf (core-data-input-keyboard-key-pressed-queue-count *core*)))))
          (#.+input-mouse-button-up+        ; param[0]: key
           (setf (aref (core-data-input-mouse-current-button-state *core*) (param 0)) 0))
          (#.+input-mouse-button-down+      ; param[0]: key
           (setf (aref (core-data-input-mouse-current-button-state *core*) (param 0)) 1))
          (#.+input-mouse-position+         ; param[0]: x, param[1]: y
           (setf (core-data-input-mouse-current-position *core*)
                 (vec2 (float (param 0) 1.0) (float (param 1) 1.0))))
          (#.+input-mouse-wheel-motion+     ; param[0]: x delta, param[1]: y delta
           (setf (core-data-input-mouse-current-wheel-move *core*)
                 (vec2 (float (param 0) 1.0) (float (param 1) 1.0))))
          (#.+input-touch-up+               ; param[0]: id
           (setf (aref (core-data-input-touch-current-touch-state *core*) (param 0)) 0))
          (#.+input-touch-down+             ; param[0]: id
           (setf (aref (core-data-input-touch-current-touch-state *core*) (param 0)) 1))
          (#.+input-touch-position+         ; param[0]: id, param[1]: x, param[2]: y
           (setf (aref (core-data-input-touch-position *core*) (param 0))
                 (vec2 (float (param 1) 1.0) (float (param 2) 1.0))))
          (#.+input-gamepad-connect+        ; param[0]: gamepad
           (setf (aref (core-data-input-gamepad-ready *core*) (param 0)) t))
          (#.+input-gamepad-disconnect+     ; param[0]: gamepad
           (setf (aref (core-data-input-gamepad-ready *core*) (param 0)) nil))
          (#.+input-gamepad-button-up+      ; param[0]: gamepad, param[1]: button
           (setf (aref (core-data-input-gamepad-current-button-state *core*) (param 0) (param 1)) 0))
          (#.+input-gamepad-button-down+    ; param[0]: gamepad, param[1]: button
           (setf (aref (core-data-input-gamepad-current-button-state *core*) (param 0) (param 1)) 1))
          (#.+input-gamepad-axis-motion+    ; param[0]: gamepad, param[1]: axis, param[2]: delta
           (setf (aref (core-data-input-gamepad-axis-state *core*) (param 0) (param 1))
                 (/ (float (param 2) 1.0) 32768.0)))
          (#.+input-gesture+                ; param[0]: gesture (enum Gesture) -> rgestures.h: GESTURES.current
           (setf (gestures-data-current *gestures*) (param 0)))
          ;; Window event
          (#.+window-close+ (setf (core-data-window-should-close *core*) t))
          (#.+window-maximize+ (maximize-window))
          (#.+window-minimize+ (minimize-window))
          (#.+window-resize+ (set-window-size (param 0) (param 1)))
          ;; Custom event
          (#.+action-take-screenshot+
           (take-screenshot (format nil "screenshot~3,'0d.png" *screenshot-counter*))
           (incf *screenshot-counter*))
          (#.+action-settargetfps+ (set-target-fps (param 0))))
        (trace-log-info "AUTOMATION PLAY: Frame: ~d | Event type: ~d | Event parameters: ~d, ~d, ~d"
                        (automation-event-frame event) (automation-event-type event)
                        (param 0) (param 1) (param 2)))))
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Input Handling: Keyboard
;;;----------------------------------------------------------------------------------

(defun is-key-pressed (key)
  "Check if key has been pressed once"
  (let ((key (%key key)))
    (and (> key 0) (< key +max-keyboard-keys+)
         (= (aref (core-data-input-keyboard-previous-key-state *core*) key) 0)
         (= (aref (core-data-input-keyboard-current-key-state *core*) key) 1))))

(defun is-key-pressed-repeat (key)
  "Check if key has been pressed again"
  (let ((key (%key key)))
    (and (> key 0) (< key +max-keyboard-keys+)
         (= (aref (core-data-input-keyboard-key-repeat-in-frame *core*) key) 1))))

(defun is-key-down (key)
  "Check if key is being pressed (key held down)"
  (let ((key (%key key)))
    (and (> key 0) (< key +max-keyboard-keys+)
         (= (aref (core-data-input-keyboard-current-key-state *core*) key) 1))))

(defun is-key-released (key)
  "Check if key has been released once"
  (let ((key (%key key)))
    (and (> key 0) (< key +max-keyboard-keys+)
         (= (aref (core-data-input-keyboard-previous-key-state *core*) key) 1)
         (= (aref (core-data-input-keyboard-current-key-state *core*) key) 0))))

(defun is-key-up (key)
  "Check if key is NOT being pressed (key not held down)"
  (let ((key (%key key)))
    (and (> key 0) (< key +max-keyboard-keys+)
         (= (aref (core-data-input-keyboard-current-key-state *core*) key) 0))))

(defun %queue-pop (queue count-accessor-setter count)
  "Get value from the queue head, shift elements 1 step toward the head"
  (let ((value (aref queue 0)))
    (loop for i from 0 below (1- count)
          do (setf (aref queue i) (aref queue (1+ i))))
    ;; Reset last character in the queue
    (setf (aref queue (1- count)) 0)
    (funcall count-accessor-setter (1- count))
    value))

(defun get-key-pressed ()
  "Get the last key pressed"
  (let ((count (core-data-input-keyboard-key-pressed-queue-count *core*)))
    (if (> count 0)
        (%queue-pop (core-data-input-keyboard-key-pressed-queue *core*)
                    (lambda (n) (setf (core-data-input-keyboard-key-pressed-queue-count *core*) n))
                    count)
        0)))

(defun get-char-pressed ()
  "Get the last char pressed"
  (let ((count (core-data-input-keyboard-char-pressed-queue-count *core*)))
    (if (> count 0)
        (%queue-pop (core-data-input-keyboard-char-pressed-queue *core*)
                    (lambda (n) (setf (core-data-input-keyboard-char-pressed-queue-count *core*) n))
                    count)
        0)))

(defun set-exit-key (key)
  "Set a custom key to exit program
NOTE: default exitKey is set to ESCAPE"
  (setf (core-data-input-keyboard-exit-key *core*) (%key key))
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Input Handling: Gamepad
;;;----------------------------------------------------------------------------------

;; NOTE: Functions with a platform-specific implementation on glfw.lisp
;;(defun set-gamepad-mappings (mappings))

(declaim (inline %gamepad-ready-p))
(defun %gamepad-ready-p (gamepad)
  (and (>= gamepad 0) (< gamepad +max-gamepads+)
       (aref (core-data-input-gamepad-ready *core*) gamepad)))

(defun is-gamepad-available (gamepad)
  "Check if gamepad is available"
  (and (%gamepad-ready-p gamepad) t))

(defun get-gamepad-name (gamepad)
  "Get gamepad internal name id"
  (when (and (>= gamepad 0) (< gamepad +max-gamepads+))
    (aref (core-data-input-gamepad-name *core*) gamepad)))

(defun is-gamepad-button-pressed (gamepad button)
  "Check if gamepad button has been pressed once"
  (let ((button (%gamepad-button button)))
    (and (%gamepad-ready-p gamepad) (< button +max-gamepad-buttons+)
         (= (aref (core-data-input-gamepad-previous-button-state *core*) gamepad button) 0)
         (= (aref (core-data-input-gamepad-current-button-state *core*) gamepad button) 1))))

(defun is-gamepad-button-down (gamepad button)
  "Check if gamepad button is being pressed"
  (let ((button (%gamepad-button button)))
    (and (%gamepad-ready-p gamepad) (< button +max-gamepad-buttons+)
         (= (aref (core-data-input-gamepad-current-button-state *core*) gamepad button) 1))))

(defun is-gamepad-button-released (gamepad button)
  "Check if gamepad button has NOT been pressed once"
  (let ((button (%gamepad-button button)))
    (and (%gamepad-ready-p gamepad) (< button +max-gamepad-buttons+)
         (= (aref (core-data-input-gamepad-previous-button-state *core*) gamepad button) 1)
         (= (aref (core-data-input-gamepad-current-button-state *core*) gamepad button) 0))))

(defun is-gamepad-button-up (gamepad button)
  "Check if gamepad button is NOT being pressed"
  (let ((button (%gamepad-button button)))
    (and (%gamepad-ready-p gamepad) (< button +max-gamepad-buttons+)
         (= (aref (core-data-input-gamepad-current-button-state *core*) gamepad button) 0))))

(defun get-gamepad-button-pressed ()
  "Get the last gamepad button pressed
NOTE: Returns last gamepad button down, down->up change not considered"
  (core-data-input-gamepad-last-button-pressed *core*))

(defun get-gamepad-axis-count (gamepad)
  "Get gamepad axis count"
  (if (and (>= gamepad 0) (< gamepad +max-gamepads+))
      (aref (core-data-input-gamepad-axis-count *core*) gamepad)
      0))

(defun get-gamepad-axis-movement (gamepad axis)
  "Get axis movement vector for a gamepad"
  (let* ((axis (%gamepad-axis axis))
         (value (if (or (= axis +gamepad-axis-left-trigger+) (= axis +gamepad-axis-right-trigger+)) -1.0 0.0)))
    (when (and (%gamepad-ready-p gamepad) (< axis +max-gamepad-axes+))
      (let* ((state (aref (core-data-input-gamepad-axis-state *core*) gamepad axis))
             (movement (if (< value 0.0) state (abs state))))
        (when (> movement value) (setf value state))))
    value))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Input Handling: Mouse
;;;----------------------------------------------------------------------------------

;; NOTE: Functions with a platform-specific implementation on glfw.lisp
;;(defun set-mouse-position (x y))
;;(defun set-mouse-cursor (cursor))

(defun is-mouse-button-pressed (button)
  "Check if mouse button has been pressed once"
  (let ((button (%mouse-button button))
        (pressed nil))
    (when (and (>= button 0) (<= button +mouse-button-back+))
      (when (and (= (aref (core-data-input-mouse-current-button-state *core*) button) 1)
                 (= (aref (core-data-input-mouse-previous-button-state *core*) button) 0))
        (setf pressed t))
      ;; Map touches to mouse buttons checking
      (when (and (= (aref (core-data-input-touch-current-touch-state *core*) button) 1)
                 (= (aref (core-data-input-touch-previous-touch-state *core*) button) 0))
        (setf pressed t)))
    pressed))

(defun is-mouse-button-down (button)
  "Check if mouse button is being pressed"
  (let ((button (%mouse-button button))
        (down nil))
    (when (and (>= button 0) (<= button +mouse-button-back+))
      (when (= (aref (core-data-input-mouse-current-button-state *core*) button) 1) (setf down t))
      ;; NOTE: Touches are considered like mouse buttons
      (when (= (aref (core-data-input-touch-current-touch-state *core*) button) 1) (setf down t)))
    down))

(defun is-mouse-button-released (button)
  "Check if mouse button has been released once"
  (let ((button (%mouse-button button))
        (released nil))
    (when (and (>= button 0) (<= button +mouse-button-back+))
      (when (and (= (aref (core-data-input-mouse-current-button-state *core*) button) 0)
                 (= (aref (core-data-input-mouse-previous-button-state *core*) button) 1))
        (setf released t))
      ;; Map touches to mouse buttons checking
      (when (and (= (aref (core-data-input-touch-current-touch-state *core*) button) 0)
                 (= (aref (core-data-input-touch-previous-touch-state *core*) button) 1))
        (setf released t)))
    released))

(defun is-mouse-button-up (button)
  "Check if mouse button is NOT being pressed"
  (let ((button (%mouse-button button))
        (up nil))
    (when (and (>= button 0) (<= button +mouse-button-back+))
      (when (= (aref (core-data-input-mouse-current-button-state *core*) button) 0) (setf up t))
      ;; NOTE: Touches are considered like mouse buttons
      (when (= (aref (core-data-input-touch-current-touch-state *core*) button) 0) (setf up t)))
    up))

(defun get-mouse-x ()
  "Get mouse position X"
  (truncate (* (+ (vx (core-data-input-mouse-current-position *core*)) (vx (core-data-input-mouse-offset *core*)))
               (vx (core-data-input-mouse-scale *core*)))))

(defun get-mouse-y ()
  "Get mouse position Y"
  (truncate (* (+ (vy (core-data-input-mouse-current-position *core*)) (vy (core-data-input-mouse-offset *core*)))
               (vy (core-data-input-mouse-scale *core*)))))

(defun get-mouse-position ()
  "Get mouse position XY"
  (let ((current (core-data-input-mouse-current-position *core*))
        (offset (core-data-input-mouse-offset *core*))
        (scale (core-data-input-mouse-scale *core*)))
    (vec2 (* (+ (vx current) (vx offset)) (vx scale))
          (* (+ (vy current) (vy offset)) (vy scale)))))

(defun get-mouse-delta ()
  "Get mouse delta between frames"
  (let ((current (core-data-input-mouse-current-position *core*))
        (previous (core-data-input-mouse-previous-position *core*))
        (scale (core-data-input-mouse-scale *core*)))
    (vec2 (* (- (vx current) (vx previous)) (vx scale))
          (* (- (vy current) (vy previous)) (vy scale)))))

(defun set-mouse-offset (offset-x offset-y)
  "Set mouse offset
NOTE: Useful when rendering to different size targets"
  (setf (core-data-input-mouse-offset *core*) (vec2 (float offset-x 1.0) (float offset-y 1.0)))
  (values))

(defun set-mouse-scale (scale-x scale-y)
  "Set mouse scaling
NOTE: Useful when rendering to different size targets"
  (setf (core-data-input-mouse-scale *core*) (vec2 (float scale-x 1.0) (float scale-y 1.0)))
  (values))

(defun get-mouse-wheel-move ()
  "Get mouse wheel movement Y"
  (let ((wheel (core-data-input-mouse-current-wheel-move *core*)))
    (if (> (abs (vx wheel)) (abs (vy wheel)))
        (vx wheel)
        (vy wheel))))

(defun get-mouse-wheel-move-v ()
  "Get mouse wheel movement X/Y as a vector"
  (let ((wheel (core-data-input-mouse-current-wheel-move *core*)))
    (vec2 (vx wheel) (vy wheel))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Input Handling: Touch
;;;----------------------------------------------------------------------------------

(defun get-touch-x ()
  "Get touch position X for touch point 0 (relative to screen size)"
  (truncate (vx (aref (core-data-input-touch-position *core*) 0))))

(defun get-touch-y ()
  "Get touch position Y for touch point 0 (relative to screen size)"
  (truncate (vy (aref (core-data-input-touch-position *core*) 0))))

(defun get-touch-position (index)
  "Get touch position XY for a touch point index (relative to screen size)"
  (if (< index +max-touch-points+)
      (let ((position (aref (core-data-input-touch-position *core*) index)))
        (vec2 (vx position) (vy position)))
      (progn
        (trace-log-warning "INPUT: Required touch point out of range (Max touch points: ~d)" +max-touch-points+)
        (vec2 -1.0 -1.0))))

(defun get-touch-point-id (index)
  "Get touch point identifier for provided index"
  (if (< index +max-touch-points+)
      (aref (core-data-input-touch-point-id *core*) index)
      -1))

(defun get-touch-point-count ()
  "Get number of touch points"
  (core-data-input-touch-point-count *core*))

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions Definition
;;;----------------------------------------------------------------------------------

;; NOTE: Functions with a platform-specific implementation on glfw.lisp
;;(defun init-platform ())
;;(defun close-platform ())

(defun init-timer ()
  "Initialize hi-resolution timer"
  ;; CLOCK_MONOTONIC base time, in nanoseconds
  (setf (core-data-time-base *core*)
        (floor (* (get-internal-real-time) 1000000000) internal-time-units-per-second))
  (setf (core-data-time-previous *core*) (get-time)) ; Get time as double
  (values))

(defun setup-viewport (width height)
  "Set viewport for a provided width and height"
  (setf (core-data-window-render-width *core*) width
        (core-data-window-render-height *core*) height)
  ;; Set viewport width and height
  (rl-viewport (truncate (core-data-window-render-offset-x *core*) 2)
               (truncate (core-data-window-render-offset-y *core*) 2)
               (core-data-window-render-width *core*) (core-data-window-render-height *core*))
  (rl-matrix-mode +rl-projection+)      ; Switch to projection matrix
  (rl-load-identity)                    ; Reset current matrix (projection)
  ;; Set orthographic projection to current framebuffer size
  ;; NOTE: Configured top-left corner as (0, 0)
  (rl-ortho 0 (core-data-window-render-width *core*) (core-data-window-render-height *core*) 0 0.0 1.0)
  (rl-matrix-mode +rl-modelview+)       ; Switch back to modelview matrix
  (rl-load-identity)                    ; Reset current matrix (modelview)
  (values))

(defun record-automation-event ()
  "Automation event recording
Checking events in current frame and save them into currentEventList
NOTE: Recording is by default done at EndDrawing(), before PollInputEvents()"
  (let ((list *current-event-list*)
        (frame (core-data-time-frame-counter *core*)))
    (when (or (null list) (= (automation-event-list-count list) (automation-event-list-capacity list)))
      (return-from record-automation-event))
    (flet ((record (type name p0 p1 p2)
             (let ((event (aref (automation-event-list-events list) (automation-event-list-count list))))
               (setf (automation-event-frame event) frame
                     (automation-event-type event) type)
               (setf (aref (automation-event-params event) 0) p0
                     (aref (automation-event-params event) 1) p1
                     (aref (automation-event-params event) 2) p2)
               (trace-log-info "AUTOMATION: Frame: ~d | Event type: ~a | Event parameters: ~d, ~d, ~d"
                               frame name p0 p1 p2)
               (incf (automation-event-list-count list))))
           (full-p ()
             (= (automation-event-list-count list) (automation-event-list-capacity list))))
      ;; Keyboard input events recording
      ;;-------------------------------------------------------------------------------------
      (let ((current (core-data-input-keyboard-current-key-state *core*))
            (previous (core-data-input-keyboard-previous-key-state *core*)))
        (dotimes (key +max-keyboard-keys+)
          ;; Event type: INPUT_KEY_UP (only saved once)
          (when (and (/= (aref previous key) 0) (= (aref current key) 0))
            (record +input-key-up+ "INPUT_KEY_UP" key 0 0))
          (when (full-p) (return-from record-automation-event)) ; Security check
          ;; Event type: INPUT_KEY_DOWN
          (when (/= (aref current key) 0)
            (record +input-key-down+ "INPUT_KEY_DOWN" key 0 0))
          (when (full-p) (return-from record-automation-event)))) ; Security check
      ;;-------------------------------------------------------------------------------------

      ;; Mouse input currentEventList->events recording
      ;;-------------------------------------------------------------------------------------
      (let ((current (core-data-input-mouse-current-button-state *core*))
            (previous (core-data-input-mouse-previous-button-state *core*)))
        (dotimes (button +max-mouse-buttons+)
          ;; Event type: INPUT_MOUSE_BUTTON_UP
          (when (and (/= (aref previous button) 0) (= (aref current button) 0))
            (record +input-mouse-button-up+ "INPUT_MOUSE_BUTTON_UP" button 0 0))
          (when (full-p) (return-from record-automation-event)) ; Security check
          ;; Event type: INPUT_MOUSE_BUTTON_DOWN
          (when (/= (aref current button) 0)
            (record +input-mouse-button-down+ "INPUT_MOUSE_BUTTON_DOWN" button 0 0))
          (when (full-p) (return-from record-automation-event)))) ; Security check

      ;; Event type: INPUT_MOUSE_POSITION (only saved if changed)
      (let ((current (core-data-input-mouse-current-position *core*))
            (previous (core-data-input-mouse-previous-position *core*)))
        (when (or (/= (truncate (vx current)) (truncate (vx previous)))
                  (/= (truncate (vy current)) (truncate (vy previous))))
          (record +input-mouse-position+ "INPUT_MOUSE_POSITION" (truncate (vx current)) (truncate (vy current)) 0)
          (when (full-p) (return-from record-automation-event)))) ; Security check

      ;; Event type: INPUT_MOUSE_WHEEL_MOTION
      (let ((current (core-data-input-mouse-current-wheel-move *core*))
            (previous (core-data-input-mouse-previous-wheel-move *core*)))
        (when (or (/= (truncate (vx current)) (truncate (vx previous)))
                  (/= (truncate (vy current)) (truncate (vy previous))))
          (record +input-mouse-wheel-motion+ "INPUT_MOUSE_WHEEL_MOTION" (truncate (vx current)) (truncate (vy current)) 0)
          (when (full-p) (return-from record-automation-event)))) ; Security check
      ;;-------------------------------------------------------------------------------------

      ;; Touch input currentEventList->events recording
      ;;-------------------------------------------------------------------------------------
      (let ((current (core-data-input-touch-current-touch-state *core*))
            (previous (core-data-input-touch-previous-touch-state *core*))
            (position (core-data-input-touch-position *core*))
            (previous-position (core-data-input-touch-previous-position *core*)))
        (dotimes (id +max-touch-points+)
          ;; Event type: INPUT_TOUCH_UP
          (when (and (/= (aref previous id) 0) (= (aref current id) 0))
            (record +input-touch-up+ "INPUT_TOUCH_UP" id 0 0))
          (when (full-p) (return-from record-automation-event)) ; Security check
          ;; Event type: INPUT_TOUCH_DOWN
          (when (/= (aref current id) 0)
            (record +input-touch-down+ "INPUT_TOUCH_DOWN" id 0 0))
          (when (full-p) (return-from record-automation-event)) ; Security check
          ;; Event type: INPUT_TOUCH_POSITION
          (let ((p (aref position id))
                (pp (aref previous-position id)))
            (when (or (/= (truncate (vx p)) (truncate (vx pp)))
                      (/= (truncate (vy p)) (truncate (vy pp))))
              (record +input-touch-position+ "INPUT_TOUCH_POSITION" id (truncate (vx p)) (truncate (vy p)))))
          (when (full-p) (return-from record-automation-event)))) ; Security check
      ;;-------------------------------------------------------------------------------------

      ;; Gamepad input currentEventList->events recording
      ;;-------------------------------------------------------------------------------------
      (let ((current (core-data-input-gamepad-current-button-state *core*))
            (previous (core-data-input-gamepad-previous-button-state *core*)))
        (dotimes (gamepad +max-gamepads+)
          ;; Event type: INPUT_GAMEPAD_CONNECT / INPUT_GAMEPAD_DISCONNECT
          ;; TODO in raylib: Automation event: Save gamepad connect/disconnect event
          (dotimes (button +max-gamepad-buttons+)
            ;; Event type: INPUT_GAMEPAD_BUTTON_UP
            (when (and (/= (aref previous gamepad button) 0) (= (aref current gamepad button) 0))
              (record +input-gamepad-button-up+ "INPUT_GAMEPAD_BUTTON_UP" gamepad button 0))
            (when (full-p) (return-from record-automation-event)) ; Security check
            ;; Event type: INPUT_GAMEPAD_BUTTON_DOWN
            (when (/= (aref current gamepad button) 0)
              (record +input-gamepad-button-down+ "INPUT_GAMEPAD_BUTTON_DOWN" gamepad button 0))
            (when (full-p) (return-from record-automation-event))) ; Security check
          (dotimes (axis +max-gamepad-axes+)
            ;; Event type: INPUT_GAMEPAD_AXIS_MOTION
            (let ((default-movement (if (or (= axis +gamepad-axis-left-trigger+) (= axis +gamepad-axis-right-trigger+)) -1.0 0.0)))
              (when (/= (get-gamepad-axis-movement gamepad axis) default-movement)
                (record +input-gamepad-axis-motion+ "INPUT_GAMEPAD_AXIS_MOTION" gamepad axis
                        (truncate (* (aref (core-data-input-gamepad-axis-state *core*) gamepad axis) 32768.0)))))
            (when (full-p) (return-from record-automation-event))))) ; Security check
      ;;-------------------------------------------------------------------------------------

      ;; Gestures input currentEventList->events recording
      ;;-------------------------------------------------------------------------------------
      (when (/= (gestures-data-current *gestures*) +gesture-none+)
        ;; Event type: INPUT_GESTURE
        (record +input-gesture+ "INPUT_GESTURE" (gestures-data-current *gestures*) 0 0))))
  (values))
