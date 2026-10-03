(in-package #:cl-raylib)

;;;===================================================================================
;;; rcore_desktop_glfw - Functions to manage window, graphics device and inputs
;;; Port of raylib/src/platforms/rcore_desktop_glfw.c
;;;
;;; NOTE: Uses the GLFW C API directly through the %glfw CFFI bindings
;;; (org.shirakumo.fraf.glfw.cffi), GLFW enums are passed as raw integers
;;; NOTE: C compile-time checks on _GLFW_X11/_GLFW_WAYLAND are done at runtime
;;; with glfwGetPlatform(), the shared GLFW library supports both backends
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; GLFW defines used by this module (glfw3.h)
;;;----------------------------------------------------------------------------------
(defconstant +glfw-true+ 1)
(defconstant +glfw-false+ 0)
(defconstant +glfw-dont-care+ -1)
(defconstant +glfw-release+ 0)
(defconstant +glfw-press+ 1)
(defconstant +glfw-repeat+ 2)
(defconstant +glfw-mod-caps-lock+ #x0010)
(defconstant +glfw-mod-num-lock+ #x0020)
(defconstant +glfw-focused+ #x00020001)
(defconstant +glfw-resizable+ #x00020003)
(defconstant +glfw-visible+ #x00020004)
(defconstant +glfw-decorated+ #x00020005)
(defconstant +glfw-auto-iconify+ #x00020006)
(defconstant +glfw-floating+ #x00020007)
(defconstant +glfw-transparent-framebuffer+ #x0002000A)
(defconstant +glfw-focus-on-show+ #x0002000C)
(defconstant +glfw-mouse-passthrough+ #x0002000D)
(defconstant +glfw-samples+ #x0002100D)
(defconstant +glfw-context-version-major+ #x00022002)
(defconstant +glfw-context-version-minor+ #x00022003)
(defconstant +glfw-opengl-forward-compat+ #x00022006)
(defconstant +glfw-opengl-profile+ #x00022008)
(defconstant +glfw-scale-to-monitor+ #x0002200C)
(defconstant +glfw-scale-framebuffer+ #x0002200D)
(defconstant +glfw-opengl-core-profile+ #x00032001)
(defconstant +glfw-cursor+ #x00033001)
(defconstant +glfw-lock-key-mods+ #x00033004)
(defconstant +glfw-raw-mouse-motion+ #x00033005)
(defconstant +glfw-cursor-normal+ #x00034001)
(defconstant +glfw-cursor-hidden+ #x00034002)
(defconstant +glfw-cursor-disabled+ #x00034003)
(defconstant +glfw-connected+ #x00040001)
(defconstant +glfw-disconnected+ #x00040002)
(defconstant +glfw-platform-win32+ #x00060001)
(defconstant +glfw-platform-cocoa+ #x00060002)
(defconstant +glfw-platform-wayland+ #x00060003)
(defconstant +glfw-platform-x11+ #x00060004)
(defconstant +glfw-platform-null+ #x00060005)
(defconstant +glfw-no-window-context+ #x0001000A)
(defconstant +glfw-platform-error+ #x00010008)
(defconstant +glfw-gamepad-axis-last+ 5)
;; GLFW gamepad buttons
(defconstant +glfw-gamepad-button-a+ 0)
(defconstant +glfw-gamepad-button-b+ 1)
(defconstant +glfw-gamepad-button-x+ 2)
(defconstant +glfw-gamepad-button-y+ 3)
(defconstant +glfw-gamepad-button-left-bumper+ 4)
(defconstant +glfw-gamepad-button-right-bumper+ 5)
(defconstant +glfw-gamepad-button-back+ 6)
(defconstant +glfw-gamepad-button-start+ 7)
(defconstant +glfw-gamepad-button-guide+ 8)
(defconstant +glfw-gamepad-button-left-thumb+ 9)
(defconstant +glfw-gamepad-button-right-thumb+ 10)
(defconstant +glfw-gamepad-button-dpad-up+ 11)
(defconstant +glfw-gamepad-button-dpad-right+ 12)
(defconstant +glfw-gamepad-button-dpad-down+ 13)
(defconstant +glfw-gamepad-button-dpad-left+ 14)

(defmacro %with-glfw-traps-masked (&body body)
  "GLFW and OpenGL drivers may raise floating point exceptions, mask them as C does"
  `(float-features:with-float-traps-masked t ,@body))

(defun %glfw-enum-int (value enum)
  "Integer value for a %glfw enum result (keyword when declared, integer otherwise)"
  (if (keywordp value) (cffi:foreign-enum-value enum value) value))

(defun %glfw-platform ()
  (%glfw-enum-int (%glfw:get-platform) '%glfw:flag))

(defun %glfw-wayland-p ()
  "Runtime equivalent of defined(_GLFW_WAYLAND) for the active GLFW backend"
  (= (%glfw-platform) +glfw-platform-wayland+))

;; Load the GLFW shared library (statically linked into raylib in the C version)
(unless (cffi:foreign-library-loaded-p '%glfw:libglfw)
  (cffi:load-foreign-library '%glfw:libglfw))

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------
(defstruct platform-data
  (handle (cffi:null-pointer)))         ; GLFW window handle (graphic device)

;;;----------------------------------------------------------------------------------
;;; Global Variables Definition
;;;----------------------------------------------------------------------------------
(defvar *platform* (make-platform-data) "Platform specific data")

(declaim (inline %handle))
(defun %handle () (platform-data-handle *platform*))

(defun %glfw-monitors ()
  "glfwGetMonitors(): returns monitor pointers vector"
  (cffi:with-foreign-object (count :int)
    (let* ((array (%glfw:get-monitors count))
           (n (cffi:mem-ref count :int))
           (monitors (make-array n)))
      (dotimes (i n monitors)
        (setf (aref monitors i) (cffi:mem-aref array :pointer i))))))

(defun %glfw-video-mode (monitor)
  "glfwGetVideoMode(): returns (values width height refresh-rate) or NIL"
  (let ((mode (%glfw:get-video-mode monitor)))
    (unless (cffi:null-pointer-p mode)
      (values (%glfw:video-mode-width mode)
              (%glfw:video-mode-height mode)
              (%glfw:video-mode-refresh-rate mode)))))

(defmacro %with-int-outputs ((&rest vars) &body body)
  "Allocate foreign int output parameters, bind VARs to their values after BODY-call form"
  `(cffi:with-foreign-objects ,(loop for v in vars collect `(,v :int))
     ,@body))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Window and Graphics Device
;;;----------------------------------------------------------------------------------

(defun window-should-close ()
  "Check if application should close
NOTE: By default, if KEY_ESCAPE pressed or window close icon clicked"
  (if (core-data-window-ready *core*)
      (core-data-window-should-close *core*)
      t))

(defun toggle-fullscreen ()
  "Toggle fullscreen mode"
  (if (not (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+))
      (progn
        ;; Store previous screen data (in case exiting fullscreen)
        (setf (core-data-window-previous-position-x *core*) (core-data-window-position-x *core*)
              (core-data-window-previous-position-y *core*) (core-data-window-position-y *core*)
              (core-data-window-previous-screen-width *core*) (core-data-window-screen-width *core*)
              (core-data-window-previous-screen-height *core*) (core-data-window-screen-height *core*))
        ;; Use current monitor the window is on to get fullscreen required size
        (let* ((monitor-index (get-current-monitor))
               (monitors (%glfw-monitors))
               (monitor (when (< monitor-index (length monitors)) (aref monitors monitor-index))))
          (if monitor
              (multiple-value-bind (width height) (%glfw-video-mode monitor)
                ;; Get current monitor video mode
                (setf (core-data-window-display-width *core*) width
                      (core-data-window-display-height *core*) height)
                (setf (core-data-window-position-x *core*) 0
                      (core-data-window-position-y *core*) 0
                      (core-data-window-screen-width *core*) width
                      (core-data-window-screen-height *core*) height)
                ;; Set fullscreen flag to be processed on FramebufferSizeCallback() accordingly
                (%flag-set (core-data-window-flags *core*) +flag-fullscreen-mode+)
                ;; NOTE: X11 requires undecorating the window before switching to
                ;; fullscreen to avoid issues with framebuffer scaling
                #+linux
                (progn
                  (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-false+)
                  (%flag-set (core-data-window-flags *core*) +flag-window-undecorated+))
                ;; WARNING: This function launches FramebufferSizeCallback()
                (%with-glfw-traps-masked
                  (%glfw:set-window-monitor (%handle) monitor 0 0
                                            (core-data-window-screen-width *core*)
                                            (core-data-window-screen-height *core*)
                                            +glfw-dont-care+)))
              (trace-log-warning "GLFW: Failed to get monitor"))))
      (progn
        ;; Restore previous window position and size
        (setf (core-data-window-position-x *core*) (core-data-window-previous-position-x *core*)
              (core-data-window-position-y *core*) (core-data-window-previous-position-y *core*)
              (core-data-window-screen-width *core*) (core-data-window-previous-screen-width *core*)
              (core-data-window-screen-height *core*) (core-data-window-previous-screen-height *core*))
        ;; Set fullscreen flag to be processed on FramebufferSizeCallback() accordingly
        ;; and considered by GetWindowScaleDPI()
        (%flag-clear (core-data-window-flags *core*) +flag-fullscreen-mode+)
        ;; Make sure to restore render size considering HighDPI scaling
        ;; NOTE: On Wayland, GLFW_SCALE_FRAMEBUFFER handles scaling, skip manual resize
        #-darwin
        (when (and (not (%glfw-wayland-p))
                   (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+))
          (let ((scale-dpi (get-window-scale-dpi)))
            (setf (core-data-window-screen-width *core*)
                  (truncate (* (core-data-window-screen-width *core*) (vx scale-dpi)))
                  (core-data-window-screen-height *core*)
                  (truncate (* (core-data-window-screen-height *core*) (vy scale-dpi))))))
        ;; WARNING: This function launches FramebufferSizeCallback()
        (%with-glfw-traps-masked
          (%glfw:set-window-monitor (%handle) (cffi:null-pointer)
                                    (core-data-window-position-x *core*) (core-data-window-position-y *core*)
                                    (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)
                                    +glfw-dont-care+))
        ;; NOTE: X11 requires restoring the decorated window after switching from
        ;; fullscreen to avoid issues with framebuffer scaling
        #+linux
        (progn
          (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-true+)
          (%flag-clear (core-data-window-flags *core*) +flag-window-undecorated+))))
  ;; Try to enable GPU V-Sync, so frames are limited to screen refresh rate (60Hz -> 60 FPS)
  ;; NOTE: V-Sync can be enabled by graphic driver configuration
  (when (%flag-is-set (core-data-window-flags *core*) +flag-vsync-hint+) (%glfw:swap-interval 1))
  (values))

(defun toggle-borderless-windowed ()
  "Toggle borderless windowed mode"
  ;; Leave fullscreen before attempting to set borderless windowed mode
  ;; NOTE: Fullscreen already saves the previous position so it does not need to be set again later
  (when (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+) (toggle-fullscreen))
  (let* ((monitors (%glfw-monitors))
         (monitor (get-current-monitor)))
    (if (and (>= monitor 0) (< monitor (length monitors)))
        (multiple-value-bind (mode-width mode-height mode-refresh-rate) (%glfw-video-mode (aref monitors monitor))
          (if mode-width
              (if (not (%flag-is-set (core-data-window-flags *core*) +flag-borderless-windowed-mode+))
                  (progn
                    ;; Store screen position and size
                    ;; NOTE: If it was on fullscreen, screen position was already stored, so skip setting it here
                    (setf (core-data-window-previous-position-x *core*) (core-data-window-position-x *core*)
                          (core-data-window-previous-position-y *core*) (core-data-window-position-y *core*)
                          (core-data-window-previous-screen-width *core*) (core-data-window-screen-width *core*)
                          (core-data-window-previous-screen-height *core*) (core-data-window-screen-height *core*))
                    ;; Set undecorated flag
                    (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-false+)
                    (%flag-set (core-data-window-flags *core*) +flag-window-undecorated+)
                    ;; Get monitor position and size
                    (%with-int-outputs (x y)
                      (%glfw:get-monitor-pos (aref monitors monitor) x y)
                      (setf (core-data-window-position-x *core*) (cffi:mem-ref x :int)
                            (core-data-window-position-y *core*) (cffi:mem-ref y :int)))
                    (setf (core-data-window-screen-width *core*) mode-width
                          (core-data-window-screen-height *core*) mode-height)
                    ;; Set screen position and size
                    (%with-glfw-traps-masked
                      (%glfw:set-window-monitor (%handle)
                                                #+windows (cffi:null-pointer) #-windows (aref monitors monitor)
                                                (core-data-window-position-x *core*) (core-data-window-position-y *core*)
                                                (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)
                                                mode-refresh-rate))
                    ;; Refocus window
                    (%glfw:focus-window (%handle))
                    (%flag-set (core-data-window-flags *core*) +flag-borderless-windowed-mode+))
                  (progn
                    ;; Restore previous screen values
                    (setf (core-data-window-position-x *core*) (core-data-window-previous-position-x *core*)
                          (core-data-window-position-y *core*) (core-data-window-previous-position-y *core*)
                          (core-data-window-screen-width *core*) (core-data-window-previous-screen-width *core*)
                          (core-data-window-screen-height *core*) (core-data-window-previous-screen-height *core*))
                    ;; Remove undecorated flag
                    (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-true+)
                    (%flag-clear (core-data-window-flags *core*) +flag-window-undecorated+)
                    ;; Make sure to restore size considering HighDPI scaling
                    ;; NOTE: On Wayland, GLFW_SCALE_FRAMEBUFFER handles scaling, skip manual resize
                    #-darwin
                    (when (and (not (%glfw-wayland-p))
                               (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+))
                      (let ((scale-dpi (get-window-scale-dpi)))
                        (setf (core-data-window-screen-width *core*)
                              (truncate (* (core-data-window-screen-width *core*) (vx scale-dpi)))
                              (core-data-window-screen-height *core*)
                              (truncate (* (core-data-window-screen-height *core*) (vy scale-dpi))))))
                    ;; Return to previous screen size and position
                    (%with-glfw-traps-masked
                      (%glfw:set-window-monitor (%handle) (cffi:null-pointer)
                                                (core-data-window-position-x *core*) (core-data-window-position-y *core*)
                                                (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)
                                                mode-refresh-rate))
                    ;; Refocus window
                    (%glfw:focus-window (%handle))
                    (%flag-clear (core-data-window-flags *core*) +flag-borderless-windowed-mode+)))
              (trace-log-warning "GLFW: Failed to find video mode for selected monitor")))
        (trace-log-warning "GLFW: Failed to find selected monitor")))
  (values))

(defun maximize-window ()
  "Set window state: maximized, if resizable"
  (when (= (%glfw:get-window-attrib (%handle) +glfw-resizable+) +glfw-true+)
    (%glfw:maximize-window (%handle))
    (%flag-set (core-data-window-flags *core*) +flag-window-maximized+))
  (values))

(defun minimize-window ()
  "Set window state: minimized"
  ;; NOTE: Following function launches callback that sets appropriate flag!
  (%glfw:iconify-window (%handle))
  (values))

(defun restore-window ()
  "Restore window from being minimized/maximized"
  (when (= (%glfw:get-window-attrib (%handle) +glfw-resizable+) +glfw-true+)
    ;; Restores the specified window if it was previously iconified (minimized) or maximized
    (%glfw:restore-window (%handle))
    (%flag-clear (core-data-window-flags *core*) +flag-window-minimized+)
    (%flag-clear (core-data-window-flags *core*) +flag-window-maximized+))
  (values))

(defun set-window-state (flags)
  "Set window configuration state using flags"
  (let ((flags (%flags flags)))
    (unless (core-data-window-ready *core*)
      (trace-log-warning "WINDOW: SetWindowState does nothing before window initialization, Use \"SetConfigFlags\" instead"))
    ;; Check previous state and requested state to apply required changes
    ;; NOTE: In most cases the functions already change the flags internally
    (flet ((changed-on-p (flag)
             (and (not (eq (%flag-is-set (core-data-window-flags *core*) flag) (%flag-is-set flags flag)))
                  (%flag-is-set flags flag))))
      ;; State change: FLAG_VSYNC_HINT
      (when (changed-on-p +flag-vsync-hint+)
        (%glfw:swap-interval 1)
        (%flag-set (core-data-window-flags *core*) +flag-vsync-hint+))
      ;; State change: FLAG_BORDERLESS_WINDOWED_MODE
      ;; NOTE: This must be handled before FLAG_FULLSCREEN_MODE because ToggleBorderlessWindowed() needs to get some fullscreen values if fullscreen is running
      (when (changed-on-p +flag-borderless-windowed-mode+)
        (toggle-borderless-windowed))   ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_FULLSCREEN_MODE
      (when (changed-on-p +flag-fullscreen-mode+)
        (toggle-fullscreen))            ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_RESIZABLE
      (when (changed-on-p +flag-window-resizable+)
        (%glfw:set-window-attrib (%handle) +glfw-resizable+ +glfw-true+)
        (%flag-set (core-data-window-flags *core*) +flag-window-resizable+))
      ;; State change: FLAG_WINDOW_UNDECORATED
      (when (changed-on-p +flag-window-undecorated+)
        (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-false+)
        (%flag-set (core-data-window-flags *core*) +flag-window-undecorated+))
      ;; State change: FLAG_WINDOW_HIDDEN
      (when (changed-on-p +flag-window-hidden+)
        (%glfw:hide-window (%handle))
        (%flag-set (core-data-window-flags *core*) +flag-window-hidden+))
      ;; State change: FLAG_WINDOW_MINIMIZED
      (when (changed-on-p +flag-window-minimized+)
        (minimize-window))              ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_MAXIMIZED
      (when (changed-on-p +flag-window-maximized+)
        (maximize-window))              ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_UNFOCUSED
      (when (changed-on-p +flag-window-unfocused+)
        (%glfw:set-window-attrib (%handle) +glfw-focus-on-show+ +glfw-false+)
        (%flag-set (core-data-window-flags *core*) +flag-window-unfocused+))
      ;; State change: FLAG_WINDOW_TOPMOST
      (when (changed-on-p +flag-window-topmost+)
        (%glfw:set-window-attrib (%handle) +glfw-floating+ +glfw-true+)
        (%flag-set (core-data-window-flags *core*) +flag-window-topmost+))
      ;; State change: FLAG_WINDOW_ALWAYS_RUN
      (when (changed-on-p +flag-window-always-run+)
        (%flag-set (core-data-window-flags *core*) +flag-window-always-run+))
      ;; The following states can not be changed after window creation
      ;; State change: FLAG_WINDOW_TRANSPARENT
      (when (changed-on-p +flag-window-transparent+)
        (trace-log-warning "WINDOW: Framebuffer transparency can only be configured before window initialization"))
      ;; State change: FLAG_WINDOW_HIGHDPI
      (when (changed-on-p +flag-window-highdpi+)
        (trace-log-warning "WINDOW: High DPI can only be configured before window initialization"))
      ;; State change: FLAG_WINDOW_MOUSE_PASSTHROUGH
      (when (changed-on-p +flag-window-mouse-passthrough+)
        (%glfw:set-window-attrib (%handle) +glfw-mouse-passthrough+ +glfw-true+)
        (%flag-set (core-data-window-flags *core*) +flag-window-mouse-passthrough+))
      ;; State change: FLAG_MSAA_4X_HINT
      (when (changed-on-p +flag-msaa-4x-hint+)
        (trace-log-warning "WINDOW: MSAA can only be configured before window initialization"))
      ;; State change: FLAG_INTERLACED_HINT
      (when (changed-on-p +flag-interlaced-hint+)
        (trace-log-warning "WINDOW: Interlaced mode can only be configured before window initialization"))))
  (values))

(defun clear-window-state (flags)
  "Clear window configuration state flags"
  (let ((flags (%flags flags)))
    ;; Check previous state and requested state to apply required changes
    ;; NOTE: In most cases the functions already change the flags internally
    (flet ((clear-p (flag)
             (and (%flag-is-set (core-data-window-flags *core*) flag) (%flag-is-set flags flag))))
      ;; State change: FLAG_VSYNC_HINT
      (when (clear-p +flag-vsync-hint+)
        (%glfw:swap-interval 0)
        (%flag-clear (core-data-window-flags *core*) +flag-vsync-hint+))
      ;; State change: FLAG_BORDERLESS_WINDOWED_MODE
      ;; NOTE: This must be handled before FLAG_FULLSCREEN_MODE because ToggleBorderlessWindowed() needs to get some fullscreen values if fullscreen is running
      (when (clear-p +flag-borderless-windowed-mode+)
        (toggle-borderless-windowed))   ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_FULLSCREEN_MODE
      (when (clear-p +flag-fullscreen-mode+)
        (toggle-fullscreen))            ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_RESIZABLE
      (when (clear-p +flag-window-resizable+)
        (%glfw:set-window-attrib (%handle) +glfw-resizable+ +glfw-false+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-resizable+))
      ;; State change: FLAG_WINDOW_HIDDEN
      (when (clear-p +flag-window-hidden+)
        (%glfw:show-window (%handle))
        (%flag-clear (core-data-window-flags *core*) +flag-window-hidden+))
      ;; State change: FLAG_WINDOW_MINIMIZED
      (when (clear-p +flag-window-minimized+)
        (restore-window))               ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_MAXIMIZED
      (when (clear-p +flag-window-maximized+)
        (restore-window))               ; NOTE: Window state flag updated inside function
      ;; State change: FLAG_WINDOW_UNDECORATED
      (when (clear-p +flag-window-undecorated+)
        (%glfw:set-window-attrib (%handle) +glfw-decorated+ +glfw-true+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-undecorated+))
      ;; State change: FLAG_WINDOW_UNFOCUSED
      (when (clear-p +flag-window-unfocused+)
        (%glfw:set-window-attrib (%handle) +glfw-focus-on-show+ +glfw-true+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-unfocused+))
      ;; State change: FLAG_WINDOW_TOPMOST
      (when (clear-p +flag-window-topmost+)
        (%glfw:set-window-attrib (%handle) +glfw-floating+ +glfw-false+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-topmost+))
      ;; State change: FLAG_WINDOW_ALWAYS_RUN
      (when (clear-p +flag-window-always-run+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-always-run+))
      ;; The following states can not be changed after window creation
      ;; State change: FLAG_WINDOW_TRANSPARENT
      (when (clear-p +flag-window-transparent+)
        (trace-log-warning "WINDOW: Framebuffer transparency can only be configured before window initialization"))
      ;; State change: FLAG_WINDOW_HIGHDPI
      (when (clear-p +flag-window-highdpi+)
        (trace-log-warning "WINDOW: High DPI can only be configured before window initialization"))
      ;; State change: FLAG_WINDOW_MOUSE_PASSTHROUGH
      (when (clear-p +flag-window-mouse-passthrough+)
        (%glfw:set-window-attrib (%handle) +glfw-mouse-passthrough+ +glfw-false+)
        (%flag-clear (core-data-window-flags *core*) +flag-window-mouse-passthrough+))
      ;; State change: FLAG_MSAA_4X_HINT
      (when (clear-p +flag-msaa-4x-hint+)
        (trace-log-warning "WINDOW: MSAA can only be configured before window initialization"))
      ;; State change: FLAG_INTERLACED_HINT
      (when (clear-p +flag-interlaced-hint+)
        (trace-log-warning "RPI: Interlaced mode can only be configured before window initialization"))))
  (values))

(defun %set-glfw-window-icons (images)
  "glfwSetWindowIcon() with a list of R8G8B8A8 images (copied by GLFW before returning)"
  (let ((count (length images)))
    (if (zerop count)
        (%glfw:set-window-icon (%handle) 0 (cffi:null-pointer))
        (let ((icons (cffi:foreign-alloc '(:struct %glfw:image) :count count))
              (pixels '()))
          (unwind-protect
               (progn
                 (loop for image in images
                       for i from 0
                       for data = (image-data image)
                       for ptr = (cffi:foreign-alloc :uint8 :count (length data))
                       for icon = (cffi:mem-aptr icons '(:struct %glfw:image) i)
                       do (push ptr pixels)
                          (dotimes (j (length data)) (setf (cffi:mem-aref ptr :uint8 j) (aref data j)))
                          (setf (cffi:foreign-slot-value icon '(:struct %glfw:image) '%glfw::width) (image-width image)
                                (cffi:foreign-slot-value icon '(:struct %glfw:image) '%glfw::height) (image-height image)
                                (cffi:foreign-slot-value icon '(:struct %glfw:image) '%glfw::pixels) ptr))
                 ;; NOTE: Images data is copied internally before this function returns
                 (%glfw:set-window-icon (%handle) count icons))
            (mapc #'cffi:foreign-free pixels)
            (cffi:foreign-free icons))))))

(defun set-window-icon (image)
  "Set icon for window
NOTE 1: Image must be in RGBA format, 8bit per channel
NOTE 2: Image is scaled by the OS for all required sizes"
  (cond
    ((or (null image) (null (image-data image)))
     ;; Revert to the default window icon, pass in an empty image array
     (%set-glfw-window-icons '()))
    ((= (image-format image) +pixelformat-uncompressed-r8g8b8a8+)
     ;; NOTE 1: Only one image icon supported
     ;; NOTE 2: The specified image data is copied before this function returns
     (%set-glfw-window-icons (list image)))
    (t (trace-log-warning "GLFW: Window icon image must be in R8G8B8A8 pixel format")))
  (values))

(defun set-window-icons (images &optional (count (length images)))
  "Set icon for window, multiple images
NOTE 1: Images must be in RGBA format, 8bit per channel
NOTE 2: The multiple images are used depending on provided sizes
Standard Windows icon sizes: 256, 128, 96, 64, 48, 32, 24, 16"
  (if (or (null images) (<= count 0))
      ;; Revert to the default window icon, pass in an empty image array
      (%set-glfw-window-icons '())
      (let ((valid '()))
        (loop for i from 0 below count
              for image = (elt images i)
              do (if (= (image-format image) +pixelformat-uncompressed-r8g8b8a8+)
                     (push image valid)
                     (trace-log-warning "GLFW: Window icon image must be in R8G8B8A8 pixel format")))
        (%set-glfw-window-icons (nreverse valid))))
  (values))

(defun set-window-title (title)
  "Set title for window"
  (setf (core-data-window-title *core*) title)
  (%glfw:set-window-title (%handle) title)
  (values))

(defun set-window-position (x y)
  "Set window position on screen (windowed mode)"
  ;; Update CORE.Window.position as well
  (setf (core-data-window-position-x *core*) x
        (core-data-window-position-y *core*) y)
  (%glfw:set-window-pos (%handle) x y)
  (values))

(defun set-window-monitor (monitor)
  "Set monitor for the current window"
  (let ((monitors (%glfw-monitors)))
    (if (and (>= monitor 0) (< monitor (length monitors)))
        (if (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+)
            (progn
              (trace-log-info "GLFW: Selected fullscreen monitor: [~d] ~a" monitor
                              (%glfw:get-monitor-name (aref monitors monitor)))
              (multiple-value-bind (width height refresh-rate) (%glfw-video-mode (aref monitors monitor))
                (%with-glfw-traps-masked
                  (%glfw:set-window-monitor (%handle) (aref monitors monitor) 0 0 width height refresh-rate))))
            (progn
              (trace-log-info "GLFW: Selected monitor: [~d] ~a" monitor
                              (%glfw:get-monitor-name (aref monitors monitor)))
              ;; Here the render width has to be used again in case high dpi flag is enabled
              (let ((screen-width (core-data-window-render-width *core*))
                    (screen-height (core-data-window-render-height *core*)))
                (%with-int-outputs (wx wy ww wh)
                  (%glfw:get-monitor-workarea (aref monitors monitor) wx wy ww wh)
                  (let ((monitor-workarea-x (cffi:mem-ref wx :int))
                        (monitor-workarea-y (cffi:mem-ref wy :int))
                        (monitor-workarea-width (cffi:mem-ref ww :int))
                        (monitor-workarea-height (cffi:mem-ref wh :int)))
                    ;; If the screen size is larger than the monitor workarea, anchor it on the top left corner, otherwise, center it
                    (if (or (>= screen-width monitor-workarea-width) (>= screen-height monitor-workarea-height))
                        (%glfw:set-window-pos (%handle) monitor-workarea-x monitor-workarea-y)
                        (let ((x (- (+ monitor-workarea-x (truncate monitor-workarea-width 2)) (truncate screen-width 2)))
                              (y (- (+ monitor-workarea-y (truncate monitor-workarea-height 2)) (truncate screen-height 2))))
                          (%glfw:set-window-pos (%handle) x y))))))))
        (trace-log-warning "GLFW: Failed to find selected monitor")))
  (values))

(defun %update-window-size-limits ()
  (flet ((limit (value) (if (= value 0) +glfw-dont-care+ value)))
    (%glfw:set-window-size-limits (%handle)
                                  (limit (core-data-window-screen-min-width *core*))
                                  (limit (core-data-window-screen-min-height *core*))
                                  (limit (core-data-window-screen-max-width *core*))
                                  (limit (core-data-window-screen-max-height *core*)))))

(defun set-window-min-size (width height)
  "Set window minimum dimensions (FLAG_WINDOW_RESIZABLE)"
  (setf (core-data-window-screen-min-width *core*) width
        (core-data-window-screen-min-height *core*) height)
  (%update-window-size-limits)
  (values))

(defun set-window-max-size (width height)
  "Set window maximum dimensions (FLAG_WINDOW_RESIZABLE)"
  (setf (core-data-window-screen-max-width *core*) width
        (core-data-window-screen-max-height *core*) height)
  (%update-window-size-limits)
  (values))

(defun set-window-size (width height)
  "Set window dimensions"
  (setf (core-data-window-screen-width *core*) width
        (core-data-window-screen-height *core*) height)
  (%glfw:set-window-size (%handle) width height)
  (values))

(defun set-window-opacity (opacity)
  "Set window opacity, value opacity is between 0.0 and 1.0"
  (let ((opacity (cond ((>= opacity 1.0) 1.0)
                       ((<= opacity 0.0) 0.0)
                       (t (float opacity 1.0)))))
    (%glfw:set-window-opacity (%handle) opacity))
  (values))

(defun set-window-focused ()
  "Set window focused"
  (%glfw:focus-window (%handle))
  (values))

(defun get-window-handle ()
  "Get native window handle
NOTE: On X11 returns the Window (XID) integer instead of a pointer to it"
  (let ((platform (%glfw-platform)))
    (cond ((= platform +glfw-platform-win32+) (%glfw:get-win32-window (%handle)))   ; Type: HWND
          ((= platform +glfw-platform-wayland+) (%glfw:get-wayland-window (%handle))) ; Type: struct wl_surface*
          ((= platform +glfw-platform-x11+) (%glfw:get-x11-window (%handle)))         ; Type: Window (unsigned long)
          ((= platform +glfw-platform-cocoa+) (%glfw:get-cocoa-window (%handle)))     ; Type: NSWindow*
          (t nil))))

(defun get-monitor-count ()
  "Get number of monitors"
  (length (%glfw-monitors)))

(defun get-current-monitor ()
  "Get current monitor where window is placed"
  (let* ((index 0)
         (monitors (%glfw-monitors))
         (monitor-count (length monitors)))
    (when (>= monitor-count 1)
      (if (is-window-fullscreen)
          ;; Get the handle of the monitor that the specified window is in full screen on
          (let ((monitor (%glfw:get-window-monitor (%handle))))
            (loop for i from 0 below monitor-count
                  when (cffi:pointer-eq (aref monitors i) monitor)
                    do (setf index i) (return)))
          ;; In case the window is between two monitors, below logic is used
          ;; to try to detect the "current monitor" for that window, note that
          ;; this is probably an overengineered solution for a side case
          ;; trying to match SDL behaviour
          (let ((closest-dist #xFFFFFFFF)
                (wcx 0) (wcy 0))
            ;; Window center position
            (%with-int-outputs (x y)
              (%glfw:get-window-pos (%handle) x y)
              (setf wcx (cffi:mem-ref x :int) wcy (cffi:mem-ref y :int)))
            (incf wcx (truncate (core-data-window-screen-width *core*) 2))
            (incf wcy (truncate (core-data-window-screen-height *core*) 2))
            (loop for i from 0 below monitor-count
                  for monitor = (aref monitors i)
                  do (let ((mx 0) (my 0))
                       ;; Monitor top-left position
                       (%with-int-outputs (x y)
                         (%glfw:get-monitor-pos monitor x y)
                         (setf mx (cffi:mem-ref x :int) my (cffi:mem-ref y :int)))
                       (multiple-value-bind (mode-width mode-height) (%glfw-video-mode monitor)
                         (if mode-width
                             (let ((right (+ mx mode-width -1))
                                   (bottom (+ my mode-height -1)))
                               (when (and (>= wcx mx) (<= wcx right) (>= wcy my) (<= wcy bottom))
                                 (setf index i)
                                 (return))
                               (let* ((xclosest (cond ((< wcx mx) mx) ((> wcx right) right) (t wcx)))
                                      (yclosest (cond ((< wcy my) my) ((> wcy bottom) bottom) (t wcy)))
                                      (dx (- wcx xclosest))
                                      (dy (- wcy yclosest))
                                      ;; Unsigned to dodge signed overflow UB; (-x)^2 == x^2 mod 2^32 so sign drops out
                                      (ux (logand dx #xFFFFFFFF))
                                      (uy (logand dy #xFFFFFFFF))
                                      (dist (logand (+ (* ux ux) (* uy uy)) #xFFFFFFFF)))
                                 (when (< dist closest-dist)
                                   (setf index i
                                         closest-dist dist))))
                             (trace-log-warning "GLFW: Failed to find video mode for selected monitor"))))))))
    index))

(defun %monitor-or-warn (monitor)
  (let ((monitors (%glfw-monitors)))
    (if (and (>= monitor 0) (< monitor (length monitors)))
        (aref monitors monitor)
        (progn (trace-log-warning "GLFW: Failed to find selected monitor") nil))))

(defun get-monitor-position (monitor)
  "Get selected monitor position"
  (let ((m (%monitor-or-warn monitor)))
    (if m
        (%with-int-outputs (x y)
          (%glfw:get-monitor-pos m x y)
          (vec2 (float (cffi:mem-ref x :int) 1.0) (float (cffi:mem-ref y :int) 1.0)))
        (vec2 0.0 0.0))))

(defun get-monitor-width (monitor)
  "Get selected monitor width (currently used by monitor)"
  (let ((m (%monitor-or-warn monitor)))
    (or (when m
          (or (%glfw-video-mode m)
              (progn (trace-log-warning "GLFW: Failed to find video mode for selected monitor") nil)))
        0)))

(defun get-monitor-height (monitor)
  "Get selected monitor height (currently used by monitor)"
  (let ((m (%monitor-or-warn monitor)))
    (or (when m
          (or (nth-value 1 (%glfw-video-mode m))
              (progn (trace-log-warning "GLFW: Failed to find video mode for selected monitor") nil)))
        0)))

(defun get-monitor-physical-width (monitor)
  "Get selected monitor physical width in millimetres"
  (let ((m (%monitor-or-warn monitor)))
    (if m
        (%with-int-outputs (width)
          (%glfw:get-monitor-physical-size m width (cffi:null-pointer))
          (cffi:mem-ref width :int))
        0)))

(defun get-monitor-physical-height (monitor)
  "Get selected monitor physical height in millimetres"
  (let ((m (%monitor-or-warn monitor)))
    (if m
        (%with-int-outputs (height)
          (%glfw:get-monitor-physical-size m (cffi:null-pointer) height)
          (cffi:mem-ref height :int))
        0)))

(defun get-monitor-refresh-rate (monitor)
  "Get selected monitor refresh rate"
  (let ((m (%monitor-or-warn monitor)))
    (or (when m
          (or (nth-value 2 (%glfw-video-mode m))
              (progn (trace-log-warning "GLFW: Failed to find video mode for selected monitor") nil)))
        0)))

(defun get-monitor-name (monitor)
  "Get the human-readable, UTF-8 encoded name of the selected monitor"
  (let ((m (%monitor-or-warn monitor)))
    (if m (%glfw:get-monitor-name m) "")))

(defun get-window-position ()
  "Get window position XY on monitor"
  (%with-int-outputs (x y)
    (%glfw:get-window-pos (%handle) x y)
    (setf (core-data-window-position-x *core*) (cffi:mem-ref x :int)
          (core-data-window-position-y *core*) (cffi:mem-ref y :int)))
  (vec2 (float (core-data-window-position-x *core*) 1.0)
        (float (core-data-window-position-y *core*) 1.0)))

(defun get-window-scale-dpi ()
  "Get window scale DPI factor for current monitor"
  (let ((scale (vec2 1.0 1.0)))
    (when (and (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+)
               (not (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+)))
      (cffi:with-foreign-objects ((x :float) (y :float))
        (%glfw:get-window-content-scale (%handle) x y)
        (setf scale (vec2 (cffi:mem-ref x :float) (cffi:mem-ref y :float)))))
    scale))

(defun set-clipboard-text (text)
  "Set clipboard text content"
  (%glfw:set-clipboard-string (%handle) text)
  (values))

(defun get-clipboard-text ()
  "Get clipboard text content"
  (%glfw:get-clipboard-string (%handle)))

(defun get-clipboard-image ()
  "Get clipboard image content
NOTE: The C version reads image/png from the X11 selection (or Win32 clipboard), not ported yet"
  (trace-log-warning "GetClipboardImage() not implemented on target platform")
  (make-image :data nil :width 0 :height 0 :mipmaps 0 :format 0))

(defun show-cursor ()
  "Show mouse cursor"
  (%glfw:set-input-mode (%handle) +glfw-cursor+ +glfw-cursor-normal+)
  (setf (core-data-input-mouse-cursor-hidden *core*) nil)
  (values))

(defun hide-cursor ()
  "Hide mouse cursor"
  (%glfw:set-input-mode (%handle) +glfw-cursor+ +glfw-cursor-hidden+)
  (setf (core-data-input-mouse-cursor-hidden *core*) t)
  (values))

(defun enable-cursor ()
  "Enable cursor (unlock cursor)"
  (%glfw:set-input-mode (%handle) +glfw-cursor+ +glfw-cursor-normal+)
  ;; Set cursor position in the middle
  (set-mouse-position (truncate (core-data-window-screen-width *core*) 2)
                      (truncate (core-data-window-screen-height *core*) 2))
  (when (%glfw:raw-mouse-motion-supported)
    (%glfw:set-input-mode (%handle) +glfw-raw-mouse-motion+ +glfw-false+))
  (setf (core-data-input-mouse-cursor-hidden *core*) nil
        (core-data-input-mouse-cursor-locked *core*) nil)
  (values))

(defun disable-cursor ()
  "Disable cursor (lock cursor)"
  ;; Reset mouse position within the window area before disabling cursor
  (set-mouse-position (truncate (core-data-window-screen-width *core*) 2)
                      (truncate (core-data-window-screen-height *core*) 2))
  (%glfw:set-input-mode (%handle) +glfw-cursor+ +glfw-cursor-disabled+)
  (when (%glfw:raw-mouse-motion-supported)
    (%glfw:set-input-mode (%handle) +glfw-raw-mouse-motion+ +glfw-true+))
  (setf (core-data-input-mouse-cursor-hidden *core*) t
        (core-data-input-mouse-cursor-locked *core*) t)
  (values))

(defun swap-screen-buffer ()
  "Swap back buffer with front buffer (screen drawing)"
  (%with-glfw-traps-masked
    (%glfw:swap-buffers (%handle)))
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Misc
;;;----------------------------------------------------------------------------------

(defun get-time ()
  "Get elapsed time measure in seconds since InitTimer()"
  (%glfw:get-time))                     ; Elapsed time since glfwInit()

(defun open-url (url)
  "Open URL with default system browser (if available)
WARNING: This function is only safe to use if you control the URL given,
a user could craft a malicious string to perform and undesired action
NOTE: Some safety checks have been added to mitigate security issues"
  (cond
    ((or (find #\' url) (find #\" url))
     ;; Filter characters: ' and "
     (trace-log-warning "SYSTEM: Provided URL could be potentially malicious, avoid [\\'\\\"] characters"))
    ((not (or (eql 0 (search "http://" url)) (eql 0 (search "https://" url))))
     ;; Only allow URL starting with "http://" or "https://" protocols
     (trace-log-warning "SYSTEM: Provided URL must start with 'http://' or 'https://' protocols"))
    (t
     (let ((cmd #+windows (format nil "explorer \"~a\"" url)
                #+darwin (format nil "open '~a'" url)
                #-(or windows darwin) (format nil "xdg-open '~a'" url)))
       (handler-case (uiop:run-program cmd :ignore-error-status t)
         (error () (trace-log-warning "OpenURL() child process could not be created"))))))
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition: Inputs
;;;----------------------------------------------------------------------------------

(defun set-gamepad-mappings (mappings)
  "Set internal gamepad mappings"
  (if (%glfw:update-gamepad-mappings mappings) 1 0))

(defun set-gamepad-vibration (gamepad left-motor right-motor duration)
  "Set gamepad vibration"
  (declare (ignore gamepad left-motor right-motor duration))
  (trace-log-warning "SetGamepadVibration() not available on target platform")
  (values))

(defun set-mouse-position (x y)
  "Set mouse position XY"
  (setf (core-data-input-mouse-current-position *core*) (vec2 (float x 1.0) (float y 1.0)))
  (%glfw:set-cursor-pos (%handle)
                        (float (vx (core-data-input-mouse-current-position *core*)) 1d0)
                        (float (vy (core-data-input-mouse-current-position *core*)) 1d0))
  (values))

(defun set-mouse-cursor (cursor)
  "Set mouse cursor"
  (let ((cursor (if (integerp cursor) cursor (%enum-value cursor '("MOUSE-CURSOR-")))))
    (setf (core-data-input-mouse-cursor *core*) cursor)
    (if (= cursor +mouse-cursor-default+)
        (%glfw:set-cursor (%handle) (cffi:null-pointer))
        ;; NOTE: Mapping internal GLFW enum values to MouseCursor enum values
        (%glfw:set-cursor (%handle) (cffi:foreign-funcall "glfwCreateStandardCursor"
                                                          :int (+ #x00036000 cursor) :pointer))))
  (values))

(defun get-key-name (key)
  "Get physical key name"
  (let ((key (%key key)))
    (%glfw:get-key-name key (%glfw:get-key-scancode key))))

(defun poll-input-events ()
  "Register all input events"
  ;; NOTE: Gestures update must be called every frame to reset gestures correctly
  ;; because ProcessGestureEvent() is called on an event, not every frame
  (update-gestures)

  ;; Reset keys/chars pressed registered
  (setf (core-data-input-keyboard-key-pressed-queue-count *core*) 0
        (core-data-input-keyboard-char-pressed-queue-count *core*) 0)

  ;; Reset last gamepad button/axis registered state
  (setf (core-data-input-gamepad-last-button-pressed *core*) +gamepad-button-unknown+)

  ;; Keyboard/Mouse input polling (automatically managed by GLFW3 through callback)

  ;; Register previous keys states
  (replace (core-data-input-keyboard-previous-key-state *core*) (core-data-input-keyboard-current-key-state *core*))
  (fill (core-data-input-keyboard-key-repeat-in-frame *core*) 0)

  ;; Register previous mouse states
  (replace (core-data-input-mouse-previous-button-state *core*) (core-data-input-mouse-current-button-state *core*))

  ;; Register previous mouse wheel state
  (setf (core-data-input-mouse-previous-wheel-move *core*) (core-data-input-mouse-current-wheel-move *core*)
        (core-data-input-mouse-current-wheel-move *core*) (vec2 0.0 0.0))

  ;; Register previous mouse position
  (let ((current (core-data-input-mouse-current-position *core*)))
    (setf (core-data-input-mouse-previous-position *core*) (vec2 (vx current) (vy current))))

  ;; Register previous touch states
  (replace (core-data-input-touch-previous-touch-state *core*) (core-data-input-touch-current-touch-state *core*))

  ;; Map touch position to mouse position for convenience
  ;; WARNING: If the target desktop device supports touch screen, this behaviour should be reviewed!
  ;; TODO: GLFW does not support multi-touch input yet
  (let ((current (core-data-input-mouse-current-position *core*)))
    (setf (aref (core-data-input-touch-position *core*) 0) (vec2 (vx current) (vy current))))

  ;; Check if gamepads are ready
  ;; NOTE: Doing it here in case of disconnection
  (dotimes (i +max-gamepads+)
    (setf (aref (core-data-input-gamepad-ready *core*) i) (and (%glfw:joystick-present i) t)))

  ;; Register gamepads buttons events
  (dotimes (i +max-gamepads+)
    (when (aref (core-data-input-gamepad-ready *core*) i) ; Check if gamepad is available
      (let ((current (core-data-input-gamepad-current-button-state *core*))
            (axis-state (core-data-input-gamepad-axis-state *core*)))
        ;; Register previous gamepad states
        (dotimes (k +max-gamepad-buttons+)
          (setf (aref (core-data-input-gamepad-previous-button-state *core*) i k) (aref current i k)))
        ;; Get current gamepad state using internal GLFW mapping, instead of the immediate joystick API
        ;; NOTE: There is no callback available, getting it manually
        (cffi:with-foreign-object (state '(:struct %glfw:gamepad-state))
          (dotimes (k 15) (setf (cffi:mem-aref (cffi:foreign-slot-pointer state '(:struct %glfw:gamepad-state) '%glfw::buttons) :uchar k) 0))
          (dotimes (k 6) (setf (cffi:mem-aref (cffi:foreign-slot-pointer state '(:struct %glfw:gamepad-state) '%glfw::axes) :float k) 0.0))
          ;; This remaps all gamepads so they have their buttons mapped like an xbox controller
          (let ((result (%glfw:get-gamepad-state i state))
                (buttons (cffi:foreign-slot-pointer state '(:struct %glfw:gamepad-state) '%glfw::buttons))
                (axes (cffi:foreign-slot-pointer state '(:struct %glfw:gamepad-state) '%glfw::axes)))
            (unless result          ; No joystick is connected, no gamepad mapping or an error occurred
              ;; Setting axes to expected resting value instead of GLFW 0.0f default when gamepad is not connected
              (setf (cffi:mem-aref axes :float +gamepad-axis-left-trigger+) -1.0
                    (cffi:mem-aref axes :float +gamepad-axis-right-trigger+) -1.0))
            (dotimes (k 15)         ; GLFW_GAMEPAD_BUTTON_LAST + 1 (buttons array size)
              (let ((button (case k  ; GamepadButton enum values assigned
                              (#.+glfw-gamepad-button-y+ +gamepad-button-right-face-up+)
                              (#.+glfw-gamepad-button-b+ +gamepad-button-right-face-right+)
                              (#.+glfw-gamepad-button-a+ +gamepad-button-right-face-down+)
                              (#.+glfw-gamepad-button-x+ +gamepad-button-right-face-left+)
                              (#.+glfw-gamepad-button-left-bumper+ +gamepad-button-left-trigger-1+)
                              (#.+glfw-gamepad-button-right-bumper+ +gamepad-button-right-trigger-1+)
                              (#.+glfw-gamepad-button-back+ +gamepad-button-middle-left+)
                              (#.+glfw-gamepad-button-guide+ +gamepad-button-middle+)
                              (#.+glfw-gamepad-button-start+ +gamepad-button-middle-right+)
                              (#.+glfw-gamepad-button-dpad-up+ +gamepad-button-left-face-up+)
                              (#.+glfw-gamepad-button-dpad-right+ +gamepad-button-left-face-right+)
                              (#.+glfw-gamepad-button-dpad-down+ +gamepad-button-left-face-down+)
                              (#.+glfw-gamepad-button-dpad-left+ +gamepad-button-left-face-left+)
                              (#.+glfw-gamepad-button-left-thumb+ +gamepad-button-left-thumb+)
                              (#.+glfw-gamepad-button-right-thumb+ +gamepad-button-right-thumb+)
                              (t -1))))
                (when (/= button -1)  ; Check for valid button
                  (if (= (cffi:mem-aref buttons :uchar k) +glfw-press+)
                      (setf (aref current i button) 1
                            (core-data-input-gamepad-last-button-pressed *core*) button)
                      (setf (aref current i button) 0)))))
            ;; Get current state of axes
            (dotimes (k (1+ +glfw-gamepad-axis-last+))
              (setf (aref axis-state i k) (cffi:mem-aref axes :float k)))))
        ;; Register buttons for 2nd triggers (because GLFW doesn't count these as buttons but rather as axes)
        (if (> (aref axis-state i +gamepad-axis-left-trigger+) 0.1)
            (setf (aref current i +gamepad-button-left-trigger-2+) 1
                  (core-data-input-gamepad-last-button-pressed *core*) +gamepad-button-left-trigger-2+)
            (setf (aref current i +gamepad-button-left-trigger-2+) 0))
        (if (> (aref axis-state i +gamepad-axis-right-trigger+) 0.1)
            (setf (aref current i +gamepad-button-right-trigger-2+) 1
                  (core-data-input-gamepad-last-button-pressed *core*) +gamepad-button-right-trigger-2+)
            (setf (aref current i +gamepad-button-right-trigger-2+) 0))
        (setf (aref (core-data-input-gamepad-axis-count *core*) i) (1+ +glfw-gamepad-axis-last+)))))

  (setf (core-data-window-resized-last-frame *core*) nil)

  (if (or (core-data-window-event-waiting *core*)
          (and (%flag-is-set (core-data-window-flags *core*) +flag-window-minimized+)
               (not (%flag-is-set (core-data-window-flags *core*) +flag-window-always-run+))))
      (progn
        (%with-glfw-traps-masked (%glfw:wait-events)) ; Wait for in input events before continue (drawing is paused)
        (setf (core-data-time-previous *core*) (get-time)))
      (%with-glfw-traps-masked (%glfw:poll-events))) ; Poll input events: keyboard/mouse/window events (callbacks) -> Update keys state

  (setf (core-data-window-should-close *core*) (%glfw:window-should-close (%handle)))

  ;; Reset close status for next frame
  (%glfw:set-window-should-close (%handle) nil)
  (values))

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions Definition
;;;----------------------------------------------------------------------------------

(defun init-platform ()
  "Initialize platform: graphics, inputs and more"
  (%glfw:set-error-callback (cffi:callback %glfw-error-callback))

  ;; NOTE: glfwInitAllocator() is not used, RL_*ALLOC wrappers are plain malloc/realloc/free

  #+darwin (%glfw:init-hint #x00051001 +glfw-false+) ; GLFW_COCOA_CHDIR_RESOURCES
  ;; Initialize GLFW internal global state
  (let ((result (%with-glfw-traps-masked (%glfw:init))))
    (unless result
      (trace-log-warning "GLFW: Failed to initialize GLFW")
      (return-from init-platform -1)))
  ;; KLUDGE: some GL libraries clobber signals after loading, restore them (as shirakumo glfw does)
  #+(and unix sbcl) (cffi:foreign-funcall "restore_sbcl_signals" :void)

  ;; Initialize graphic device: display/window and graphic context
  ;;----------------------------------------------------------------------------
  (%glfw:default-window-hints)          ; Set default windows hints

  ;; Disable GlFW auto iconify behaviour
  ;; Auto Iconify automatically minimizes (iconifies) the window if the window loses focus
  ;; additionally auto iconify restores the hardware resolution of the monitor if the window that loses focus is a fullscreen window
  (%glfw:window-hint +glfw-auto-iconify+ 0)

  ;; Window flags requested before initialization to be applied after initialization
  (let ((requested-window-flags (core-data-window-flags *core*))
        (flags (core-data-window-flags *core*)))
    (declare (ignorable flags))
    (macrolet ((flag-p (f) `(%flag-is-set (core-data-window-flags *core*) ,f)))
      ;; Check window creation flags
      (if (flag-p +flag-window-hidden+)
          (%glfw:window-hint +glfw-visible+ +glfw-false+) ; Visible window
          (%glfw:window-hint +glfw-visible+ +glfw-true+)) ; Window initially hidden

      (if (flag-p +flag-window-undecorated+)
          (%glfw:window-hint +glfw-decorated+ +glfw-false+) ; Border and buttons on Window
          (%glfw:window-hint +glfw-decorated+ +glfw-true+)) ; Decorated window

      (if (flag-p +flag-window-resizable+)
          (%glfw:window-hint +glfw-resizable+ +glfw-true+) ; Resizable window
          (%glfw:window-hint +glfw-resizable+ +glfw-false+)) ; Avoid window being resizable

      ;; Disable FLAG_WINDOW_MINIMIZED, not supported on initialization
      (when (flag-p +flag-window-minimized+) (%flag-clear (core-data-window-flags *core*) +flag-window-minimized+))

      ;; Disable FLAG_WINDOW_MAXIMIZED, not supported on initialization
      (when (flag-p +flag-window-maximized+) (%flag-clear (core-data-window-flags *core*) +flag-window-maximized+))

      (if (flag-p +flag-window-unfocused+)
          (%glfw:window-hint +glfw-focused+ +glfw-false+)
          (%glfw:window-hint +glfw-focused+ +glfw-true+))

      (if (flag-p +flag-window-topmost+)
          (%glfw:window-hint +glfw-floating+ +glfw-true+)
          (%glfw:window-hint +glfw-floating+ +glfw-false+))

      ;; NOTE: Some GLFW flags are not supported on HTML5
      (if (flag-p +flag-window-transparent+)
          (%glfw:window-hint +glfw-transparent-framebuffer+ +glfw-true+) ; Transparent framebuffer
          (%glfw:window-hint +glfw-transparent-framebuffer+ +glfw-false+)) ; Opaque framebuffer

      (if (flag-p +flag-window-highdpi+)
          (progn
            #+darwin (%glfw:window-hint +glfw-scale-framebuffer+ +glfw-false+)
            ;; Resize window content area based on the monitor content scale
            ;; NOTE: This hint only has an effect on platforms where screen coordinates and
            ;; pixels always map 1:1 such as Windows and X11
            ;; On platforms like macOS the resolution of the framebuffer is changed independently of the window size
            (%glfw:window-hint +glfw-scale-to-monitor+ +glfw-true+)
            #+darwin (%glfw:window-hint +glfw-scale-framebuffer+ +glfw-true+))
          (progn
            (%glfw:window-hint +glfw-scale-to-monitor+ +glfw-false+)
            #+darwin (%glfw:window-hint +glfw-scale-framebuffer+ +glfw-false+)
            ;; GLFW 3.4+ defaults GLFW_SCALE_FRAMEBUFFER to TRUE,
            ;; causing framebuffer/window size mismatch on Wayland with display scaling
            (when (%glfw-wayland-p) (%glfw:window-hint +glfw-scale-framebuffer+ +glfw-false+))))

      ;; Mouse passthrough
      (if (flag-p +flag-window-mouse-passthrough+)
          (%glfw:window-hint +glfw-mouse-passthrough+ +glfw-true+)
          (%glfw:window-hint +glfw-mouse-passthrough+ +glfw-false+))

      (when (flag-p +flag-msaa-4x-hint+)
        ;; NOTE: MSAA is only enabled for main framebuffer, not user-created FBOs
        (trace-log-info "DISPLAY: Trying to enable MSAA x4")
        (%glfw:window-hint +glfw-samples+ 4)) ; Tries to enable multisampling x4 (MSAA), default is 0

      ;; NOTE: When asking for an OpenGL context version, most drivers provide the highest supported version
      ;; with backward compatibility to older OpenGL versions
      ;; For example, if using OpenGL 1.1, driver can provide a 4.3 backwards compatible context
      (let ((version (rl-get-version)))
        (cond ((= version +rl-opengl-21+)
               (%glfw:window-hint +glfw-context-version-major+ 2) ; Choose OpenGL major version (just hint)
               (%glfw:window-hint +glfw-context-version-minor+ 1)) ; Choose OpenGL minor version (just hint)
              ((= version +rl-opengl-33+)
               (%glfw:window-hint +glfw-context-version-major+ 3)
               (%glfw:window-hint +glfw-context-version-minor+ 3)
               (%glfw:window-hint +glfw-opengl-profile+ +glfw-opengl-core-profile+)
               (%glfw:window-hint +glfw-opengl-forward-compat+ #+darwin +glfw-true+ #-darwin +glfw-false+))
              ((= version +rl-opengl-43+)
               (%glfw:window-hint +glfw-context-version-major+ 4)
               (%glfw:window-hint +glfw-context-version-minor+ 3)
               (%glfw:window-hint +glfw-opengl-profile+ +glfw-opengl-core-profile+)
               (%glfw:window-hint +glfw-opengl-forward-compat+ +glfw-false+))))

      ;; NOTE: GLFW 3.4+ defers initialization of the Joystick subsystem on the first call to any Joystick related functions
      ;; Forcing this initialization here avoids doing it on PollInputEvents() called by EndDrawing() after first frame has been drawn
      (%glfw:set-joystick-callback (cffi:null-pointer))

      (when (or (= (core-data-window-screen-width *core*) 0) (= (core-data-window-screen-height *core*) 0))
        (%flag-set (core-data-window-flags *core*) +flag-fullscreen-mode+))

      ;; NOTE: Fullscreen applications default to the primary monitor
      (let ((monitor (%glfw:get-primary-monitor))
            (title (or (core-data-window-title *core*) " ")))
        (when (cffi:null-pointer-p monitor)
          (trace-log-warning "GLFW: Failed to get primary monitor")
          (return-from init-platform -1))

        ;; Init window in fullscreen mode if requested
        ;; NOTE: Keeping original screen size for toggle
        (if (flag-p +flag-fullscreen-mode+)
            (multiple-value-bind (mode-width mode-height) (%glfw-video-mode monitor)
              ;; Default display resolution to that of the current mode
              (setf (core-data-window-display-width *core*) mode-width
                    (core-data-window-display-height *core*) mode-height)
              ;; Check if user requested some screen size
              (if (or (= (core-data-window-screen-width *core*) 0) (= (core-data-window-screen-height *core*) 0))
                  (progn
                    ;; Set some default screen size in case user decides to exit fullscreen mode
                    (setf (core-data-window-previous-screen-width *core*) 800
                          (core-data-window-previous-screen-height *core*) 450
                          (core-data-window-previous-position-x *core*) (- (truncate (core-data-window-display-width *core*) 2) (truncate 800 2))
                          (core-data-window-previous-position-y *core*) (- (truncate (core-data-window-display-height *core*) 2) (truncate 450 2)))
                    ;; Set screen width/height to the display width/height
                    (when (= (core-data-window-screen-width *core*) 0)
                      (setf (core-data-window-screen-width *core*) (core-data-window-display-width *core*)))
                    (when (= (core-data-window-screen-height *core*) 0)
                      (setf (core-data-window-screen-height *core*) (core-data-window-display-height *core*))))
                  (setf (core-data-window-previous-screen-width *core*) (core-data-window-screen-width *core*)
                        (core-data-window-previous-screen-height *core*) (core-data-window-screen-height *core*)
                        (core-data-window-screen-width *core*) (core-data-window-display-width *core*)
                        (core-data-window-screen-height *core*) (core-data-window-display-height *core*)))
              (setf (platform-data-handle *platform*)
                    (%with-glfw-traps-masked
                      (%glfw:create-window (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)
                                           title monitor (cffi:null-pointer))))
              (when (cffi:null-pointer-p (%handle))
                (%glfw:terminate)
                (trace-log-warning "GLFW: Failed to initialize Window")
                (return-from init-platform -1)))
            (progn
              ;; Default to at least one pixel in size, as creation with a zero dimension is not allowed
              (when (= (core-data-window-screen-width *core*) 0) (setf (core-data-window-screen-width *core*) 1))
              (when (= (core-data-window-screen-height *core*) 0) (setf (core-data-window-screen-height *core*) 1))
              (%with-int-outputs (wx wy ww wh)
                (%glfw:get-monitor-workarea monitor wx wy ww wh)
                (let ((work-width (cffi:mem-ref ww :int))
                      (work-height (cffi:mem-ref wh :int)))
                  ;; If the area requested by the user exceeds the maximum workable area, clamp it to that
                  ;; GLFW has a problem where if the window is greater than the workable area (this means
                  ;; the taskbar / dockable areas) it won't show up if the window isn't fullscreen
                  (when (> (core-data-window-screen-width *core*) work-width)
                    (setf (core-data-window-screen-width *core*) work-width))
                  (when (> (core-data-window-screen-height *core*) work-height)
                    (setf (core-data-window-screen-height *core*) work-height))))
              (setf (platform-data-handle *platform*)
                    (%with-glfw-traps-masked
                      (%glfw:create-window (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)
                                           title (cffi:null-pointer) (cffi:null-pointer))))
              (when (cffi:null-pointer-p (%handle))
                (%glfw:terminate)
                (trace-log-warning "GLFW: Failed to initialize Window")
                (return-from init-platform -1))
              ;; After the window was created, determine the monitor that the window manager assigned
              ;; Derive display sizes and, if possible, window size in case it was zero at beginning
              (let ((monitor-index (get-current-monitor))
                    (monitors (%glfw-monitors)))
                (if (< monitor-index (length monitors))
                    (multiple-value-bind (mode-width mode-height) (%glfw-video-mode (aref monitors monitor-index))
                      ;; Default display resolution to that of the current mode
                      (setf (core-data-window-display-width *core*) mode-width
                            (core-data-window-display-height *core*) mode-height)
                      ;; Set screen width/height to the display width/height if they are 0
                      (when (= (core-data-window-screen-width *core*) 0)
                        (setf (core-data-window-screen-width *core*) (core-data-window-display-width *core*)))
                      (when (= (core-data-window-screen-height *core*) 0)
                        (setf (core-data-window-screen-height *core*) (core-data-window-display-height *core*)))
                      (%glfw:set-window-size (%handle) (core-data-window-screen-width *core*) (core-data-window-screen-height *core*)))
                    (progn
                      ;; The monitor for the window-manager-created window can not be determined, so it can not be centered
                      (%glfw:terminate)
                      (trace-log-warning "GLFW: Failed to determine Monitor to center Window")
                      (return-from init-platform -1))))
              #+darwin
              (%with-int-outputs (w h)
                ;; AppKit can constrain the requested window size to the visible work area during creation
                (%glfw:get-window-size (%handle) w h)
                (when (and (> (cffi:mem-ref w :int) 0) (> (cffi:mem-ref h :int) 0))
                  (setf (core-data-window-screen-width *core*) (cffi:mem-ref w :int)
                        (core-data-window-screen-height *core*) (cffi:mem-ref h :int))))
              ;; NOTE: Not considering scale factor now, considered below
              (setf (core-data-window-render-width *core*) (core-data-window-screen-width *core*)
                    (core-data-window-render-height *core*) (core-data-window-screen-height *core*))))))

    (%with-glfw-traps-masked (%glfw:make-context-current (%handle)))
    (let ((result (%glfw-enum-int (%glfw:get-error (cffi:null-pointer)) '%glfw:error)))
      ;; Checking context activation
      (when (and (/= result +glfw-no-window-context+) (/= result +glfw-platform-error+))
        (setf (core-data-window-ready *core*) t)))

    (if (core-data-window-ready *core*)
        (progn
          ;; Setup additional windows configs and register required window size info
          (%glfw:swap-interval 0)       ; No V-Sync by default

          ;; Try to enable GPU V-Sync, so frames are limited to screen refresh rate (60Hz -> 60 FPS)
          ;; NOTE: V-Sync can be enabled by graphic driver configuration, it doesn't need
          ;; to be activated on web platforms since VSync is enforced there
          (when (%flag-is-set (core-data-window-flags *core*) +flag-vsync-hint+)
            ;; WARNING: It seems to hit a critical render path in Intel HD Graphics
            (%glfw:swap-interval 1)
            (trace-log-info "DISPLAY: Trying to enable VSYNC"))

          (if (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+)
              (let ((scale-dpi (get-window-scale-dpi)))
                ;; Set screen size to logical pixel size, considering content scaling
                (setf (core-data-window-render-width *core*) (truncate (* (core-data-window-screen-width *core*) (vx scale-dpi)))
                      (core-data-window-render-height *core*) (truncate (* (core-data-window-screen-height *core*) (vy scale-dpi))))
                ;; Screen scaling matrix is required in case desired screen area is different from display area
                (setf (core-data-window-screen-scale *core*) (matrix-scale (vx scale-dpi) (vy scale-dpi) 1.0))
                ;; NOTE: On APPLE platforms system manage window and input scaling
                ;; Framebuffer scaling is activated with: glfwWindowHint(GLFW_SCALE_FRAMEBUFFER, GLFW_TRUE);
                #-darwin
                (if (%glfw-wayland-p)
                    ;; On Wayland, GLFW_SCALE_FRAMEBUFFER handles scaling; read actual framebuffer size
                    ;; instead of resizing the window (which would double-scale)
                    (%with-int-outputs (fb-width fb-height)
                      (%glfw:get-framebuffer-size (%handle) fb-width fb-height)
                      (setf (core-data-window-render-width *core*) (cffi:mem-ref fb-width :int)
                            (core-data-window-render-height *core*) (cffi:mem-ref fb-height :int)))
                    (progn
                      ;; Mouse input scaling for the new screen size
                      (set-mouse-scale (/ 1.0 (vx scale-dpi)) (/ 1.0 (vy scale-dpi)))
                      ;; Force window size (and framebuffer) refresh
                      (%glfw:set-window-size (%handle) (core-data-window-render-width *core*)
                                             (core-data-window-render-height *core*)))))
              (setf (core-data-window-render-width *core*) (core-data-window-screen-width *core*)
                    (core-data-window-render-height *core*) (core-data-window-screen-height *core*)))

          ;; Current active framebuffer size is main framebuffer size
          (setf (core-data-window-current-fbo-width *core*) (core-data-window-render-width *core*)
                (core-data-window-current-fbo-height *core*) (core-data-window-render-height *core*))

          (trace-log-info "DISPLAY: Device initialized successfully ~a"
                          (if (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+) "(HighDPI)" ""))
          (trace-log-info "    > Display size: ~d x ~d" (core-data-window-display-width *core*) (core-data-window-display-height *core*))
          (trace-log-info "    > Screen size:  ~d x ~d" (core-data-window-screen-width *core*) (core-data-window-screen-height *core*))
          (trace-log-info "    > Render size:  ~d x ~d" (core-data-window-render-width *core*) (core-data-window-render-height *core*))
          (trace-log-info "    > Viewport offsets: ~d, ~d" (core-data-window-render-offset-x *core*) (core-data-window-render-offset-y *core*))

          ;; Try to center window on screen but avoiding window-bar outside of screen
          (let* ((monitor-index (get-current-monitor))
                 (monitor (aref (%glfw-monitors) monitor-index)))
            (%with-int-outputs (mx my mw mh)
              (%glfw:get-monitor-workarea monitor mx my mw mh)
              (let ((monitor-x (cffi:mem-ref mx :int))
                    (monitor-y (cffi:mem-ref my :int))
                    (monitor-width (cffi:mem-ref mw :int))
                    (monitor-height (cffi:mem-ref mh :int)))
                ;; Center window into current monitor
                #+darwin
                (setf (core-data-window-position-x *core*) (+ monitor-x (truncate (- monitor-width (core-data-window-screen-width *core*)) 2))
                      (core-data-window-position-y *core*) (+ monitor-y (truncate (- monitor-height (core-data-window-screen-height *core*)) 2)))
                #-darwin
                (setf (core-data-window-position-x *core*) (+ monitor-x (truncate (- monitor-width (core-data-window-render-width *core*)) 2))
                      (core-data-window-position-y *core*) (+ monitor-y (truncate (- monitor-height (core-data-window-render-height *core*)) 2))))))
          (set-window-position (core-data-window-position-x *core*) (core-data-window-position-y *core*))

          (when (%flag-is-set (core-data-window-flags *core*) +flag-window-minimized+) (minimize-window)))
        (progn
          (trace-log-fatal "PLATFORM: Failed to initialize graphics device")
          (return-from init-platform -1)))

    ;; Apply window flags requested previous to initialization
    (set-window-state requested-window-flags))

  ;; Load OpenGL extensions
  ;; NOTE: GL procedures address loader is required to load extensions
  (rl-load-extensions (cffi:foreign-symbol-pointer "glfwGetProcAddress"))
  ;;----------------------------------------------------------------------------

  ;; Initialize input events callbacks
  ;;----------------------------------------------------------------------------
  ;; Set window callback events
  (%glfw:set-window-size-callback (%handle) (cffi:callback %glfw-window-size-callback)) ; NOTE: Resizing is not enabled by default
  (%glfw:set-framebuffer-size-callback (%handle) (cffi:callback %glfw-framebuffer-size-callback))
  (%glfw:set-window-pos-callback (%handle) (cffi:callback %glfw-window-pos-callback))
  (%glfw:set-window-maximize-callback (%handle) (cffi:callback %glfw-window-maximize-callback))
  (%glfw:set-window-iconify-callback (%handle) (cffi:callback %glfw-window-iconify-callback))
  (%glfw:set-window-focus-callback (%handle) (cffi:callback %glfw-window-focus-callback))
  (%glfw:set-drop-callback (%handle) (cffi:callback %glfw-window-drop-callback))
  (when (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+)
    (%glfw:set-window-content-scale-callback (%handle) (cffi:callback %glfw-window-content-scale-callback)))

  ;; Set input callback events
  (%glfw:set-key-callback (%handle) (cffi:callback %glfw-key-callback))
  (%glfw:set-char-callback (%handle) (cffi:callback %glfw-char-callback))
  (%glfw:set-mouse-button-callback (%handle) (cffi:callback %glfw-mouse-button-callback))
  (%glfw:set-cursor-pos-callback (%handle) (cffi:callback %glfw-mouse-cursor-pos-callback)) ; Track mouse position changes
  (%glfw:set-scroll-callback (%handle) (cffi:callback %glfw-mouse-scroll-callback))
  (%glfw:set-cursor-enter-callback (%handle) (cffi:callback %glfw-cursor-enter-callback))
  (%glfw:set-joystick-callback (cffi:callback %glfw-joystick-callback))
  (%glfw:set-input-mode (%handle) +glfw-lock-key-mods+ +glfw-true+) ; Enable lock keys modifiers (CAPS, NUM)

  ;; Retrieve gamepad names
  (dotimes (i +max-gamepads+)
    (when (%glfw:joystick-present i)
      (setf (aref (core-data-input-gamepad-ready *core*) i) t
            (aref (core-data-input-gamepad-axis-count *core*) i) (1+ +glfw-gamepad-axis-last+)
            (aref (core-data-input-gamepad-name *core*) i) (or (%glfw:get-joystick-name i) ""))))
  ;;----------------------------------------------------------------------------

  ;; Initialize timing system
  ;;----------------------------------------------------------------------------
  (init-timer)
  ;;----------------------------------------------------------------------------

  ;; Initialize storage system
  ;;----------------------------------------------------------------------------
  (setf (core-data-storage-base-path *core*) (get-working-directory))
  ;;----------------------------------------------------------------------------

  (let ((platform (%glfw-platform)))
    (trace-log-info "PLATFORM: DESKTOP (GLFW - ~a): Initialized successfully"
                    (cond ((= platform +glfw-platform-win32+) "Win32")
                          ((= platform +glfw-platform-cocoa+) "Cocoa")
                          ((= platform +glfw-platform-wayland+) "Wayland")
                          ((= platform +glfw-platform-x11+) "X11")
                          ((= platform +glfw-platform-null+) "Null")
                          (t ""))))
  0)

(defun close-platform ()
  "Close platform"
  (%glfw:destroy-window (%handle))
  (setf (platform-data-handle *platform*) (cffi:null-pointer))
  (%glfw:terminate)
  (values))

;;; GLFW3: Error callback, runs on GLFW3 error
(cffi:defcallback %glfw-error-callback :void ((error :int) (description :string))
  (trace-log-warning "GLFW: Error: ~d Description: ~a" error description))

;;; GLFW3: Window size change callback, runs when window is resized
;;; NOTE: Window resizing not enabled by default, use SetConfigFlags()
(cffi:defcallback %glfw-window-size-callback :void ((window :pointer) (width :int) (height :int))
  (declare (ignore window width height))
  ;; Nothing to do for now on window resize...
  )

;;; GLFW3: Framebuffer size change callback, runs when framebuffer is resized
;;; WARNING: If FLAG_WINDOW_HIGHDPI is set, WindowContentScaleCallback() is called before this function
(cffi:defcallback %glfw-framebuffer-size-callback :void ((window :pointer) (width :int) (height :int))
  (declare (ignore window))
  ;; WARNING: On window minimization, callback is called with 0 values,
  ;; but internal screen values should not be changed, it breaks things
  (unless (or (= width 0) (= height 0))
    ;; Reset viewport and projection matrix for new size
    ;; NOTE: Stores current render size: CORE.Window.render
    (setup-viewport width height)
    ;; Set render size
    (setf (core-data-window-render-width *core*) width
          (core-data-window-render-height *core*) height
          (core-data-window-current-fbo-width *core*) width
          (core-data-window-current-fbo-height *core*) height
          (core-data-window-resized-last-frame *core*) t)
    (if (%flag-is-set (core-data-window-flags *core*) +flag-fullscreen-mode+)
        (progn
          ;; On fullscreen mode, strategy is ignoring high-dpi and
          ;; use the all available display size
          ;; Set screen size to render size (physical pixel size)
          (setf (core-data-window-screen-width *core*) width
                (core-data-window-screen-height *core*) height
                (core-data-window-screen-scale *core*) (matrix-scale 1.0 1.0 1.0))
          (set-mouse-scale 1.0 1.0)
          ;; On Wayland with GLFW_SCALE_FRAMEBUFFER, the framebuffer is still scaled in fullscreen,
          ;; use logical window size as screen and apply screenScale
          (when (and (%glfw-wayland-p) (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+))
            (%with-int-outputs (w h)
              (%glfw:get-window-size (%handle) w h)
              (let ((win-width (cffi:mem-ref w :int))
                    (win-height (cffi:mem-ref h :int)))
                (when (or (/= win-width width) (/= win-height height))
                  (setf (core-data-window-screen-width *core*) win-width
                        (core-data-window-screen-height *core*) win-height
                        (core-data-window-screen-scale *core*)
                        (matrix-scale (/ (float width 1.0) win-width) (/ (float height 1.0) win-height) 1.0)))))))
        ;; Window mode (including borderless window)
        ;; Check if render size was actually scaled for high-dpi
        (if (%flag-is-set (core-data-window-flags *core*) +flag-window-highdpi+)
            (let ((scale-dpi (get-window-scale-dpi)))
              ;; Set screen size to logical pixel size, considering content scaling
              (setf (core-data-window-screen-width *core*) (truncate (/ (float width 1.0) (vx scale-dpi)))
                    (core-data-window-screen-height *core*) (truncate (/ (float height 1.0) (vy scale-dpi)))
                    (core-data-window-screen-scale *core*) (matrix-scale (vx scale-dpi) (vy scale-dpi) 1.0))
              ;; On macOS and Linux-Wayland, mouse coords are already in logical space
              #-darwin
              (unless (%glfw-wayland-p)
                (set-mouse-scale (/ 1.0 (vx scale-dpi)) (/ 1.0 (vy scale-dpi)))))
            ;; Set screen size to render size (physical pixel size)
            (setf (core-data-window-screen-width *core*) width
                  (core-data-window-screen-height *core*) height)))
    ;; WARNING: If using a render texture, it is not scaled to new size
    ))

;;; GLFW3: Window content scale callback, runs on monitor content scale change detected
;;; WARNING: If FLAG_WINDOW_HIGHDPI is not set, this function is not called
(cffi:defcallback %glfw-window-content-scale-callback :void ((window :pointer) (scalex :float) (scaley :float))
  (declare (ignore window))
  (setf (core-data-window-render-width *core*) (truncate (* (float (core-data-window-screen-width *core*) 1.0) scalex))
        (core-data-window-render-height *core*) (truncate (* (float (core-data-window-screen-height *core*) 1.0) scaley))
        (core-data-window-current-fbo-width *core*) (core-data-window-render-width *core*)
        (core-data-window-current-fbo-height *core*) (core-data-window-render-height *core*))
  ;; NOTE: On APPLE platforms system should manage window/input scaling and also framebuffer scaling
  ;; Framebuffer scaling is activated with: glfwWindowHint(GLFW_SCALE_FRAMEBUFFER, GLFW_TRUE);
  (setf (core-data-window-screen-scale *core*) (matrix-scale scalex scaley 1.0))
  ;; On macOS and Linux-Wayland, mouse coords are already in logical space
  #-darwin
  (unless (%glfw-wayland-p)
    (set-mouse-scale (/ 1.0 scalex) (/ 1.0 scaley))))

;;; GLFW3: Window position callback, runs when window position changes
(cffi:defcallback %glfw-window-pos-callback :void ((window :pointer) (x :int) (y :int))
  (declare (ignore window))
  ;; Set current window position
  (setf (core-data-window-position-x *core*) x
        (core-data-window-position-y *core*) y))

;;; GLFW3: Window iconify callback, runs when window is minimized/restored
(cffi:defcallback %glfw-window-iconify-callback :void ((window :pointer) (iconified :int))
  (declare (ignore window))
  (if (/= iconified 0)
      (%flag-set (core-data-window-flags *core*) +flag-window-minimized+)     ; The window was iconified
      (%flag-clear (core-data-window-flags *core*) +flag-window-minimized+))) ; The window was restored

;;; GLFW3: Window maximize callback, runs when window is maximized/restored
(cffi:defcallback %glfw-window-maximize-callback :void ((window :pointer) (maximized :int))
  (declare (ignore window))
  (if (/= maximized 0)
      (%flag-set (core-data-window-flags *core*) +flag-window-maximized+)     ; The window was maximized
      (%flag-clear (core-data-window-flags *core*) +flag-window-maximized+))) ; The window was restored

;;; GLFW3: Window focus callback, runs when window get/lose focus
(cffi:defcallback %glfw-window-focus-callback :void ((window :pointer) (focused :int))
  (declare (ignore window))
  (if (/= focused 0)
      (%flag-clear (core-data-window-flags *core*) +flag-window-unfocused+) ; The window was focused
      (%flag-set (core-data-window-flags *core*) +flag-window-unfocused+)))  ; The window lost focus

;;; GLFW3: Window drop callback, runs when files are dropped into window
(cffi:defcallback %glfw-window-drop-callback :void ((window :pointer) (count :int) (paths :pointer))
  (declare (ignore window))
  (when (> count 0)
    ;; In case previous dropped filepaths have not been freed, free them
    ;; WARNING: Paths are freed by GLFW when the callback returns, keeping an internal copy
    (setf (core-data-window-drop-file-count *core*) count
          (core-data-window-drop-filepaths *core*)
          (loop for i from 0 below count
                collect (cffi:mem-aref paths :string i)))))

;;; GLFW3: Keyboard callback, runs on key pressed
(cffi:defcallback %glfw-key-callback :void ((window :pointer) (key :int) (scancode :int) (action :int) (mods :int))
  (declare (ignore window scancode))
  (unless (< key 0)                     ; Security check, macOS fn key generates -1
    ;; WARNING: GLFW could return GLFW_REPEAT, it needs to be considered as 1
    ;; to work properly with our implementation (IsKeyDown/IsKeyUp checks)
    (cond ((= action +glfw-release+) (setf (aref (core-data-input-keyboard-current-key-state *core*) key) 0))
          ((= action +glfw-press+) (setf (aref (core-data-input-keyboard-current-key-state *core*) key) 1))
          ((= action +glfw-repeat+) (setf (aref (core-data-input-keyboard-key-repeat-in-frame *core*) key) 1)))
    ;; WARNING: Check if CAPS/NUM key modifiers are enabled and force down state for those keys
    (when (or (and (= key +key-caps-lock+) (%flag-is-set mods +glfw-mod-caps-lock+))
              (and (= key +key-num-lock+) (%flag-is-set mods +glfw-mod-num-lock+)))
      (setf (aref (core-data-input-keyboard-current-key-state *core*) key) 1))
    ;; Check if there is space available in the key queue
    (when (and (< (core-data-input-keyboard-key-pressed-queue-count *core*) +max-key-pressed-queue+)
               (= action +glfw-press+))
      ;; Add character to the queue
      (setf (aref (core-data-input-keyboard-key-pressed-queue *core*) (core-data-input-keyboard-key-pressed-queue-count *core*)) key)
      (incf (core-data-input-keyboard-key-pressed-queue-count *core*)))
    ;; Check the exit key to set close window
    (when (and (= key (core-data-input-keyboard-exit-key *core*)) (= action +glfw-press+))
      (%glfw:set-window-should-close (%handle) t))))

;;; GLFW3: Char callback, runs on key pressed to get unicode codepoint value
(cffi:defcallback %glfw-char-callback :void ((window :pointer) (codepoint :unsigned-int))
  (declare (ignore window))
  ;; NOTE: Registers any key down considering OS keyboard layout but
  ;; does not detect action events, those should be managed by user...
  ;; Check if there is space available in the queue
  (when (< (core-data-input-keyboard-char-pressed-queue-count *core*) +max-char-pressed-queue+)
    ;; Add character to the queue
    (setf (aref (core-data-input-keyboard-char-pressed-queue *core*) (core-data-input-keyboard-char-pressed-queue-count *core*)) codepoint)
    (incf (core-data-input-keyboard-char-pressed-queue-count *core*))))

;;; GLFW3: Mouse button callback, runs on mouse button pressed
(cffi:defcallback %glfw-mouse-button-callback :void ((window :pointer) (button :int) (action :int) (mods :int))
  (declare (ignore window mods))
  ;; WARNING: GLFW could only return GLFW_PRESS (1) or GLFW_RELEASE (0) for now,
  ;; but future releases may add more actions (i.e. GLFW_REPEAT)
  (setf (aref (core-data-input-mouse-current-button-state *core*) button) action
        (aref (core-data-input-touch-current-touch-state *core*) button) action)
  ;; SUPPORT_GESTURES_SYSTEM && SUPPORT_MOUSE_GESTURES
  ;; Process mouse events as touches to be able to use mouse-gestures
  (let ((gesture-event (make-gesture-event)))
    ;; Register touch actions
    (cond ((and (= (aref (core-data-input-mouse-current-button-state *core*) button) 1)
                (= (aref (core-data-input-mouse-previous-button-state *core*) button) 0))
           (setf (gesture-event-touch-action gesture-event) +touch-action-down+))
          ((and (= (aref (core-data-input-mouse-current-button-state *core*) button) 0)
                (= (aref (core-data-input-mouse-previous-button-state *core*) button) 1))
           (setf (gesture-event-touch-action gesture-event) +touch-action-up+)))
    ;; NOTE: TOUCH_ACTION_MOVE event is registered in MouseCursorPosCallback()
    ;; Assign a pointer ID
    (setf (aref (gesture-event-point-id gesture-event) 0) 0)
    ;; Register touch points count
    (setf (gesture-event-point-count gesture-event) 1)
    ;; Register touch points position, only one point registered
    ;; Normalize gestureEvent.position[0] for CORE.Window.screen.width and CORE.Window.screen.height
    (let ((position (get-mouse-position)))
      (setf (aref (gesture-event-position gesture-event) 0)
            (vec2 (/ (vx position) (float (get-screen-width) 1.0))
                  (/ (vy position) (float (get-screen-height) 1.0)))))
    ;; Gesture data is sent to gestures-system for processing
    (process-gesture-event gesture-event)))

;;; GLFW3: Cursor position callback, runs on mouse movement
(cffi:defcallback %glfw-mouse-cursor-pos-callback :void ((window :pointer) (x :double) (y :double))
  (declare (ignore window))
  (setf (core-data-input-mouse-current-position *core*) (vec2 (float x 1.0) (float y 1.0))
        (aref (core-data-input-touch-position *core*) 0) (vec2 (float x 1.0) (float y 1.0)))
  ;; SUPPORT_GESTURES_SYSTEM && SUPPORT_MOUSE_GESTURES
  ;; Process mouse events as touches to be able to use mouse-gestures
  (let ((gesture-event (make-gesture-event))
        (position (aref (core-data-input-touch-position *core*) 0)))
    (setf (gesture-event-touch-action gesture-event) +touch-action-move+)
    ;; Assign a pointer ID
    (setf (aref (gesture-event-point-id gesture-event) 0) 0)
    ;; Register touch points count
    (setf (gesture-event-point-count gesture-event) 1)
    ;; Register touch points position, only one point registered
    ;; Normalize gestureEvent.position[0] for CORE.Window.screen.width and CORE.Window.screen.height
    (setf (aref (gesture-event-position gesture-event) 0)
          (vec2 (/ (vx position) (float (get-screen-width) 1.0))
                (/ (vy position) (float (get-screen-height) 1.0))))
    ;; Gesture data is sent to gestures-system for processing
    (process-gesture-event gesture-event)))

;;; GLFW3: Mouse wheel scroll callback, runs on mouse wheel changes
(cffi:defcallback %glfw-mouse-scroll-callback :void ((window :pointer) (xoffset :double) (yoffset :double))
  (declare (ignore window))
  (setf (core-data-input-mouse-current-wheel-move *core*) (vec2 (float xoffset 1.0) (float yoffset 1.0))))

;;; GLFW3: Cursor ennter callback, when cursor enters the window
(cffi:defcallback %glfw-cursor-enter-callback :void ((window :pointer) (entered :int))
  (declare (ignore window))
  ;; NOTE: Mouse position updated by MouseCursorPosCallback()
  (setf (core-data-input-mouse-cursor-on-screen *core*) (/= entered 0)))

;;; GLFW3: Joystick connected/disconnected callback
(cffi:defcallback %glfw-joystick-callback :void ((jid :int) (event :int))
  (when (< jid +max-gamepads+)
    (cond ((= event +glfw-connected+)
           (setf (aref (core-data-input-gamepad-name *core*) jid) (or (%glfw:get-joystick-name jid) "")))
          ((= event +glfw-disconnected+)
           (setf (aref (core-data-input-gamepad-name *core*) jid) "")))))
