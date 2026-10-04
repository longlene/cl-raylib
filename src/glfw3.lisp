;;;===================================================================================
;;; GLFW 3 - Subset of raylib/src/external/glfw/include/GLFW/glfw3.h used by rcore_desktop_glfw.c
;;;
;;; raylib compiles its own copy of GLFW (rglfw.c); cl-raylib loads GLFW as a shared library:
;;;   1. the system GLFW (libglfw.so.3, libglfw.3.dylib, glfw3.dll) when it is version 3.4 or newer
;;;   2. otherwise the GLFW 3.5.1 build shipped in lib/ (built from raylib's external/glfw sources)
;;;
;;; NOTE: Enums are plain :int values (the GLFW_* constants live in glfw.lisp), boolean results
;;; and arguments use :bool
;;;===================================================================================

(defpackage #:cl-raylib.glfw3
  (:use #:cl)
  (:export
   #:libglfw #:load-glfw #:glfw-library-path
   ;; Structures
   #:image #:gamepad-state #:video-mode
   #:video-mode-width #:video-mode-height #:video-mode-refresh-rate
   ;; Initialization, version and error
   #:init #:terminate #:init-hint #:get-version #:get-error #:get-platform #:set-error-callback
   ;; Monitor
   #:get-monitors #:get-primary-monitor #:get-monitor-pos #:get-monitor-workarea #:get-monitor-physical-size
   #:get-monitor-name #:get-video-mode
   ;; Window
   #:default-window-hints #:window-hint #:create-window #:destroy-window #:window-should-close
   #:set-window-should-close #:set-window-title #:set-window-icon #:get-window-pos #:set-window-pos
   #:get-window-size #:set-window-size #:set-window-size-limits #:get-framebuffer-size
   #:get-window-content-scale #:set-window-opacity #:iconify-window #:restore-window #:maximize-window
   #:show-window #:hide-window #:focus-window #:get-window-monitor #:set-window-monitor
   #:get-window-attrib #:set-window-attrib
   #:set-window-pos-callback #:set-window-size-callback #:set-window-focus-callback
   #:set-window-iconify-callback #:set-window-maximize-callback #:set-framebuffer-size-callback
   #:set-window-content-scale-callback
   #:poll-events #:wait-events
   ;; Input
   #:set-input-mode #:raw-mouse-motion-supported #:get-key-name #:get-key-scancode #:set-cursor-pos
   #:set-cursor #:set-key-callback #:set-char-callback #:set-mouse-button-callback
   #:set-cursor-pos-callback #:set-cursor-enter-callback #:set-scroll-callback #:set-drop-callback
   #:joystick-present #:get-joystick-name #:set-joystick-callback #:update-gamepad-mappings
   #:get-gamepad-state #:set-clipboard-string #:get-clipboard-string #:get-time
   ;; Context
   #:make-context-current #:swap-buffers #:swap-interval #:get-proc-address
   ;; Native access
   #:get-win32-window #:get-cocoa-window #:get-x11-window #:get-wayland-window))

(in-package #:cl-raylib.glfw3)

;;----------------------------------------------------------------------------------
;; Library loading
;;----------------------------------------------------------------------------------

(cffi:define-foreign-library libglfw
  (:darwin "libglfw.3.dylib")
  (:windows "glfw3.dll")
  (:unix "libglfw.so.3"))

(defun glfw-library-path ()
  "Path of the GLFW library shipped with cl-raylib for this platform, or NIL"
  (let ((file #+(and linux x86-64) "lib/linux-x86-64/libglfw.so.3"
              #+darwin "lib/macos/libglfw.3.dylib"
              #+(and windows x86-64) "lib/windows-x86-64/glfw3.dll"
              #-(or (and linux x86-64) darwin (and windows x86-64)) nil))
    (when file
      (let ((path (asdf:system-relative-pathname "cl-raylib" file)))
        (and (probe-file path) path)))))

(defun %library-version ()
  "glfwGetVersion() of the loaded library as (major minor revision)"
  (cffi:with-foreign-objects ((major :int) (minor :int) (rev :int))
    (cffi:foreign-funcall "glfwGetVersion" :pointer major :pointer minor :pointer rev :void)
    (list (cffi:mem-ref major :int) (cffi:mem-ref minor :int) (cffi:mem-ref rev :int))))

(defun load-glfw ()
  "Load the system GLFW when it is 3.4 or newer, the bundled one otherwise"
  (unless (cffi:foreign-library-loaded-p 'libglfw)
    (let ((system (handler-case (cffi:load-foreign-library 'libglfw)
                    (cffi:load-foreign-library-error () nil))))
      (when system
        (destructuring-bind (major minor rev) (%library-version)
          (declare (ignore rev))
          (unless (or (> major 3) (and (= major 3) (>= minor 4)))
            ;; GLFW 3.4 API is required (glfwGetPlatform(), GLFW_PLATFORM init hint, ...)
            (cffi:close-foreign-library 'libglfw)
            (setf system nil))))
      (unless system
        (let ((path (glfw-library-path)))
          (unless path
            (error "cl-raylib: GLFW 3.4 or newer is required, install it with the system package manager"))
          ;; Point the library definition at the bundled file and load it under the same name
          (eval `(cffi:define-foreign-library libglfw (t ,(namestring path))))
          (cffi:load-foreign-library 'libglfw)))))
  t)

;; NOTE: Loaded before the function definitions below, so they link against it
(load-glfw)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------

;; GLFWimage
(cffi:defcstruct image
  (width :int)
  (height :int)
  (pixels :pointer))

;; GLFWgamepadstate
(cffi:defcstruct gamepad-state
  (buttons :uchar :count 15)
  (axes :float :count 6))

;; GLFWvidmode
(cffi:defcstruct (video-mode :conc-name video-mode-)
  (width :int)
  (height :int)
  (red-bits :int)
  (green-bits :int)
  (blue-bits :int)
  (refresh-rate :int))

;;----------------------------------------------------------------------------------
;; Functions
;;----------------------------------------------------------------------------------

(defmacro defglfw (lisp-name c-name return &rest args)
  `(progn
     (declaim (inline ,lisp-name))
     (cffi:defcfun (,c-name ,lisp-name) ,return ,@args)))

;; Initialization, version and error
(defglfw init "glfwInit" :bool)
(defglfw terminate "glfwTerminate" :void)
(defglfw init-hint "glfwInitHint" :void (hint :int) (value :int))
(defglfw get-version "glfwGetVersion" :void (major :pointer) (minor :pointer) (rev :pointer))
(defglfw get-error "glfwGetError" :int (description :pointer))
(defglfw get-platform "glfwGetPlatform" :int)
(defglfw set-error-callback "glfwSetErrorCallback" :pointer (callback :pointer))

;; Monitor
(defglfw get-monitors "glfwGetMonitors" :pointer (count :pointer))
(defglfw get-primary-monitor "glfwGetPrimaryMonitor" :pointer)
(defglfw get-monitor-pos "glfwGetMonitorPos" :void (monitor :pointer) (xpos :pointer) (ypos :pointer))
(defglfw get-monitor-workarea "glfwGetMonitorWorkarea" :void
  (monitor :pointer) (xpos :pointer) (ypos :pointer) (width :pointer) (height :pointer))
(defglfw get-monitor-physical-size "glfwGetMonitorPhysicalSize" :void (monitor :pointer) (width-mm :pointer) (height-mm :pointer))
(defglfw get-monitor-name "glfwGetMonitorName" :string (monitor :pointer))
(defglfw get-video-mode "glfwGetVideoMode" :pointer (monitor :pointer))

;; Window
(defglfw default-window-hints "glfwDefaultWindowHints" :void)
(defglfw window-hint "glfwWindowHint" :void (hint :int) (value :int))
(defglfw create-window "glfwCreateWindow" :pointer
  (width :int) (height :int) (title :string) (monitor :pointer) (share :pointer))
(defglfw destroy-window "glfwDestroyWindow" :void (window :pointer))
(defglfw window-should-close "glfwWindowShouldClose" :bool (window :pointer))
(defglfw set-window-should-close "glfwSetWindowShouldClose" :void (window :pointer) (value :bool))
(defglfw set-window-title "glfwSetWindowTitle" :void (window :pointer) (title :string))
(defglfw set-window-icon "glfwSetWindowIcon" :void (window :pointer) (count :int) (images :pointer))
(defglfw get-window-pos "glfwGetWindowPos" :void (window :pointer) (xpos :pointer) (ypos :pointer))
(defglfw set-window-pos "glfwSetWindowPos" :void (window :pointer) (xpos :int) (ypos :int))
(defglfw get-window-size "glfwGetWindowSize" :void (window :pointer) (width :pointer) (height :pointer))
(defglfw set-window-size "glfwSetWindowSize" :void (window :pointer) (width :int) (height :int))
(defglfw set-window-size-limits "glfwSetWindowSizeLimits" :void
  (window :pointer) (min-width :int) (min-height :int) (max-width :int) (max-height :int))
(defglfw get-framebuffer-size "glfwGetFramebufferSize" :void (window :pointer) (width :pointer) (height :pointer))
(defglfw get-window-content-scale "glfwGetWindowContentScale" :void (window :pointer) (xscale :pointer) (yscale :pointer))
(defglfw set-window-opacity "glfwSetWindowOpacity" :void (window :pointer) (opacity :float))
(defglfw iconify-window "glfwIconifyWindow" :void (window :pointer))
(defglfw restore-window "glfwRestoreWindow" :void (window :pointer))
(defglfw maximize-window "glfwMaximizeWindow" :void (window :pointer))
(defglfw show-window "glfwShowWindow" :void (window :pointer))
(defglfw hide-window "glfwHideWindow" :void (window :pointer))
(defglfw focus-window "glfwFocusWindow" :void (window :pointer))
(defglfw get-window-monitor "glfwGetWindowMonitor" :pointer (window :pointer))
(defglfw set-window-monitor "glfwSetWindowMonitor" :void
  (window :pointer) (monitor :pointer) (xpos :int) (ypos :int) (width :int) (height :int) (refresh-rate :int))
(defglfw get-window-attrib "glfwGetWindowAttrib" :int (window :pointer) (attrib :int))
(defglfw set-window-attrib "glfwSetWindowAttrib" :void (window :pointer) (attrib :int) (value :int))
(defglfw set-window-pos-callback "glfwSetWindowPosCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-window-size-callback "glfwSetWindowSizeCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-window-focus-callback "glfwSetWindowFocusCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-window-iconify-callback "glfwSetWindowIconifyCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-window-maximize-callback "glfwSetWindowMaximizeCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-framebuffer-size-callback "glfwSetFramebufferSizeCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-window-content-scale-callback "glfwSetWindowContentScaleCallback" :pointer (window :pointer) (callback :pointer))
(defglfw poll-events "glfwPollEvents" :void)
(defglfw wait-events "glfwWaitEvents" :void)

;; Input
(defglfw set-input-mode "glfwSetInputMode" :void (window :pointer) (mode :int) (value :int))
(defglfw raw-mouse-motion-supported "glfwRawMouseMotionSupported" :bool)
(defglfw get-key-name "glfwGetKeyName" :string (key :int) (scancode :int))
(defglfw get-key-scancode "glfwGetKeyScancode" :int (key :int))
(defglfw set-cursor-pos "glfwSetCursorPos" :void (window :pointer) (xpos :double) (ypos :double))
(defglfw set-cursor "glfwSetCursor" :void (window :pointer) (cursor :pointer))
(defglfw set-key-callback "glfwSetKeyCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-char-callback "glfwSetCharCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-mouse-button-callback "glfwSetMouseButtonCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-cursor-pos-callback "glfwSetCursorPosCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-cursor-enter-callback "glfwSetCursorEnterCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-scroll-callback "glfwSetScrollCallback" :pointer (window :pointer) (callback :pointer))
(defglfw set-drop-callback "glfwSetDropCallback" :pointer (window :pointer) (callback :pointer))
(defglfw joystick-present "glfwJoystickPresent" :bool (jid :int))
(defglfw get-joystick-name "glfwGetJoystickName" :string (jid :int))
(defglfw set-joystick-callback "glfwSetJoystickCallback" :pointer (callback :pointer))
(defglfw update-gamepad-mappings "glfwUpdateGamepadMappings" :bool (string :string))
(defglfw get-gamepad-state "glfwGetGamepadState" :bool (jid :int) (state :pointer))
(defglfw set-clipboard-string "glfwSetClipboardString" :void (window :pointer) (string :string))
(defglfw get-clipboard-string "glfwGetClipboardString" :string (window :pointer))
(defglfw get-time "glfwGetTime" :double)

;; Context
(defglfw make-context-current "glfwMakeContextCurrent" :void (window :pointer))
(defglfw swap-buffers "glfwSwapBuffers" :void (window :pointer))
(defglfw swap-interval "glfwSwapInterval" :void (interval :int))
(defglfw get-proc-address "glfwGetProcAddress" :pointer (procname :string))

;; Native access (only the function of the running platform is present in a GLFW build)
(defglfw get-win32-window "glfwGetWin32Window" :pointer (window :pointer))
(defglfw get-cocoa-window "glfwGetCocoaWindow" :pointer (window :pointer))
(defglfw get-x11-window "glfwGetX11Window" :unsigned-long (window :pointer))
(defglfw get-wayland-window "glfwGetWaylandWindow" :pointer (window :pointer))
