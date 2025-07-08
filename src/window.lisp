(in-package #:cl-raylib)

;;; Enhanced Window Management System
;;; This module provides comprehensive window and graphics context management

;;; Window state management  
(defvar *window-flags* 0 "Current window configuration flags")
(defvar *window-resized* nil "Window resized flag")
;; Core window state now managed by *core* structure
(defvar *fullscreen-toggle-cooldown* 0 "Cooldown timer for fullscreen toggle")

;;; Window Configuration Functions (matches raylib SetConfigFlags)

(defun set-config-flags (flags)
  "Set window configuration flags before window initialization (matches raylib SetConfigFlags)"
  (declare (type fixnum flags))
  ;; Check if window is already initialized (matches raylib warning)
  (when (core-data-window-ready *core*)
    (trace-log-warning "WINDOW: SetConfigFlags called after window initialization, Use \"set-window-state\" to set flags instead"))
  
  ;; Selected flags are set but not evaluated at this point,
  ;; flag evaluation happens at init-window() or set-window-state()
  ;; (matches raylib: CORE.Window.flags |= flags)
  (setf (core-data-window-flags *core*) 
        (logior (core-data-window-flags *core*) flags))
  
  (trace-log-debug "WINDOW: Configuration flags set: 0x~X" flags))

;;; Window properties
(defvar *window-min-width* 0 "Minimum window width")
(defvar *window-min-height* 0 "Minimum window height") 
(defvar *window-max-width* 0 "Maximum window width")
(defvar *window-max-height* 0 "Maximum window height")

;;; Monitor management
(defstruct monitor
  "Monitor information structure"
  (id 0 :type fixnum)
  (name "" :type string)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (physical-width 0 :type fixnum)
  (physical-height 0 :type fixnum)
  (refresh-rate 0 :type fixnum)
  (position nil :type (or null list))) ; Vector2 (x y)

;;; Enhanced window management functions

(defun setup-window-callbacks ()
  "Setup GLFW window callbacks"
  ;; Note: High-level API uses methods instead of direct callback setting
  ;; We'll override the callback methods or use the register-callbacks approach
  ;; For now, we'll use the window's register-callbacks method
  (glfw:register-callbacks (platform-data-handle *platform*)))

(defun setup-opengl-context (width height)
  "Setup OpenGL context and gl:viewport"
  (let ((render-width (get-render-width))
        (render-height (get-render-height)))
    (gl:viewport 0 0 render-width render-height))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 width height 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  
  ;; Enable VSync if requested
  (when (window-flag-set-p +flag-vsync-hint+)
    (setf (glfw:swap-interval (platform-data-handle *platform*)) 1)))

;;; Note: window-should-close is now in core.lisp

(defun is-window-hidden ()
  "Check if window is currently hidden"
  (and (platform-data-handle *platform*) 
       (not (glfw:visible-p (platform-data-handle *platform*)))))

(defun is-window-minimized ()
  "Check if window is currently minimized"
  (and (platform-data-handle *platform*)
       (glfw:iconified-p (platform-data-handle *platform*))))

(defun is-window-maximized ()
  "Check if window is currently maximized"
  ;; Note: :maximized attribute not available in this GLFW version
  ;; (and *window*
  ;;      (= (%glfw:get-window-attribute *window* :maximized) 1))
  nil)

(defun is-window-focused ()
  "Check if window is currently focused"
  (and (platform-data-handle *platform*)
       (glfw:attribute :focused (platform-data-handle *platform*))))

(defun is-window-resized ()
  "Check if window has been resized last frame"
  (prog1 *window-resized*
    (setf *window-resized* nil)))

;;; Window state modification functions

;;; Note: set-window-state and clear-window-state are now in core.lisp

;;; Note: toggle-fullscreen is now in core.lisp

(defun update-fullscreen-cooldown ()
  "Update fullscreen toggle cooldown timer"
  (when (> *fullscreen-toggle-cooldown* 0)
    (decf *fullscreen-toggle-cooldown*)))

;;; Note: maximize-window, minimize-window, restore-window are now in core.lisp

(defun hide-window ()
  "Hide window"
  (when (platform-data-handle *platform*)
    (glfw:hide (platform-data-handle *platform*))))

(defun show-window ()
  "Show window"
  (when (platform-data-handle *platform*)
    (glfw:show (platform-data-handle *platform*))))

;;; Cursor management

;;; Note: disable-cursor, enable-cursor, hide-cursor are now in core.lisp

;;; Window properties

;;; Note: set-window-title, set-window-position, get-window-position, set-window-size are now in core.lisp)

(defun set-window-min-size (width height)
  "Set window minimum dimensions"
  (setf *window-min-width* width)
  (setf *window-min-height* height)
  (when (platform-data-handle *platform*)
    (setf (glfw:size-limits (platform-data-handle *platform*)) 
          (list width height (or *window-max-width* -1) (or *window-max-height* -1)))))

(defun set-window-max-size (width height)
  "Set window maximum dimensions"
  (setf *window-max-width* width)
  (setf *window-max-height* height)
  (when (platform-data-handle *platform*)
    (setf (glfw:size-limits (platform-data-handle *platform*)) 
          (list (or *window-min-width* -1) (or *window-min-height* -1) width height))))

;;; Note: set-window-opacity and get-window-opacity are now in glfw.lisp

;;; Note: get-monitor-count is now in glfw.lisp

;;; Note: get-current-monitor is now in glfw.lisp

(defun get-monitor-info (monitor-id)
  "Get monitor information"
  (let ((monitors (glfw:list-monitors)))
    (when (< monitor-id (length monitors))
      (let ((monitor (nth monitor-id monitors)))
        (let* ((pos (glfw:location monitor))
               (x (first pos))
               (y (second pos)))
          (let* ((phys-size (glfw:physical-size monitor))
                 (phys-w (first phys-size))
                 (phys-h (second phys-size)))
            (let ((mode (glfw:video-mode monitor)))
              (make-monitor
               :id monitor-id
               :name (glfw:name monitor)
               :width (getf mode :width)
               :height (getf mode :height)
               :physical-width phys-w
               :physical-height phys-h
               :refresh-rate (getf mode :refresh-rate)
               :position (vec2 x y)))))))))

;;; Utility functions

(defun window-flag-set-p (flag)
  "Check if a specific window flag is enabled"
  (/= 0 (logand *window-flags* flag)))

(defun apply-window-flags ()
  "Apply current window flags to the window"
  ;; This would be called after flag changes to update window state
  ;; Implementation depends on whether window is already created
  ;; Note: Fullscreen is now handled directly in toggle-fullscreen
  (when (platform-data-handle *platform*)
    ;; Currently no other dynamic flags need handling
    ;; Other flags like resizable, decorated are set at window creation
    nil))

;;; Clipboard functions

;;; Note: set-clipboard-text and get-clipboard-text are now in glfw.lisp
