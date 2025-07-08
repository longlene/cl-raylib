(in-package #:cl-raylib)

;;; Enhanced Input Management System
;;; This module provides comprehensive input handling for keyboard, mouse, and gamepad

;;; GLFW action constants
(defconstant +glfw-release+ 0)
(defconstant +glfw-press+ 1)
(defconstant +glfw-repeat+ 2)

;;; Character input
(defvar *char-queue* nil "Queue for character input")
(defvar *char-queue-count* 0 "Number of characters in queue")

;;; Input callbacks
(defvar *key-pressed-callback* nil "Key pressed callback function")
(defvar *char-pressed-callback* nil "Character pressed callback function")
(defvar *mouse-button-callback* nil "Mouse button callback function")
(defvar *mouse-wheel-callback* nil "Mouse wheel callback function")

;;; GLFW to raylib key mapping
(defun glfw-to-raylib-key (glfw-key)
  "Convert GLFW key code to raylib key code"
  (case glfw-key
    ;; GLFW escape is 256, raylib escape is also 256
    (256 +key-escape+)   ; ESC
    (32 +key-space+)     ; SPACE  
    (257 +key-enter+)    ; ENTER
    (258 +key-tab+)      ; TAB
    (259 +key-backspace+) ; BACKSPACE
    (260 +key-insert+)   ; INSERT
    (261 +key-delete+)   ; DELETE
    (262 +key-right+)    ; RIGHT
    (263 +key-left+)     ; LEFT  
    (264 +key-down+)     ; DOWN
    (265 +key-up+)       ; UP
    ;; Letters (GLFW A=65, raylib A=65)
    ((65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90)
     glfw-key)  ; A-Z are the same
    ;; Numbers (GLFW 0=48, raylib 0=48)  
    ((48 49 50 51 52 53 54 55 56 57) glfw-key)  ; 0-9 are the same
    ;; For unknown keys, return the GLFW key as-is
    (t glfw-key)))

;;; Helper function to access core data (defined later)
(defvar *get-core-data-fn* nil "Function to get core data")

;;; Temporary window should close flag (until core loads)
;;(defvar (core-data-window-should-close *core*) nil "Window close request flag")

;;; CFFI callbacks for GLFW input
(cffi:defcallback key-callback :void
    ((window :pointer) (key :int) (scancode :int) (action :int) (mods :int))
  (declare (ignore window scancode mods))
  ;; Handle exit key for window close
  (let ((raylib-key (glfw-to-raylib-key key)))
    ;; Check exit key to set close window (like raylib)
    (when (and (= raylib-key +key-escape+) (= action +glfw-press+))
      ;; Set both the compatibility flag and GLFW window should close
      (setf (core-data-window-should-close *core*) t)
      (when (platform-data-handle *platform*)
        (setf (glfw:should-close-p (platform-data-handle *platform*)) t))
      ;; Also set the core data flag
      (when (boundp '*core*)
        (setf (core-data-window-should-close *core*) t)))
    ;; Update key state arrays using core data structure
    (when (< raylib-key +max-keyboard-keys+)
      (setf (aref (core-data-input-keyboard-current-state *core*) raylib-key)
            (or (= action +glfw-press+) (= action +glfw-repeat+)))))
  (when (and *key-pressed-callback* (= action +glfw-press+))
    (funcall *key-pressed-callback* key)))

(cffi:defcallback scroll-callback :void
    ((window :pointer) (x-offset :double) (y-offset :double))
  (declare (ignore window))
  (setf (core-data-input-mouse-current-wheel-move *core*) (vec2 (float x-offset) (float y-offset)))
  (when *mouse-wheel-callback*
    (funcall *mouse-wheel-callback* x-offset y-offset)))

(cffi:defcallback mouse-button-callback :void
    ((window :pointer) (button :int) (action :int) (mods :int))
  (declare (ignore window mods))
  (when (< button +max-mouse-buttons+)
    (setf (aref (core-data-input-mouse-current-button-state *core*) button) (= action +glfw-press+)))
  (when *mouse-button-callback*
    (funcall *mouse-button-callback* button action)))

(cffi:defcallback cursor-position-callback :void
    ((window :pointer) (x :double) (y :double))
  (declare (ignore window))
  (setf (core-data-input-mouse-current-position *core*) (vec2 (float x) (float y))))

;;; Setup input callbacks
(defun setup-input-callbacks ()
  "Setup GLFW input callbacks using CFFI callbacks"
  (let ((window (platform-data-handle *platform*)))
    (when window
      ;; Note: High-level API handles callbacks differently
      ;; We'll need to use the register-callbacks method or override callback methods
      ;; For now, we'll use the CFFI callbacks directly with the window pointer
      (let ((ptr (glfw:pointer window)))
        (%glfw:set-key-callback ptr (cffi:callback key-callback))
        (%glfw:set-mouse-button-callback ptr (cffi:callback mouse-button-callback))
        (%glfw:set-cursor-pos-callback ptr (cffi:callback cursor-position-callback))
        (%glfw:set-scroll-callback ptr (cffi:callback scroll-callback))))))

;;; Update input state (call once per frame) - matches raylib PollInputEvents
(defun update-input ()
  "Update input state - call this once per frame (matches raylib PollInputEvents)"
  ;; Reset keys/chars pressed registered (like raylib)
  (setf (core-data-input-keyboard-key-pressed-queue-count *core*) 0)
  (setf (core-data-input-keyboard-char-pressed-queue-count *core*) 0)
  
  ;; Clear character queue
  (setf *char-queue* nil)
  (setf *char-queue-count* 0)
  
  ;; Register previous keys states (like raylib)
  (loop for i from 0 below +max-keyboard-keys+ do
    (setf (aref (core-data-input-keyboard-previous-state *core*) i) 
          (aref (core-data-input-keyboard-current-state *core*) i))
    (setf (aref (core-data-input-keyboard-key-repeat-in-frame *core*) i) nil))
  
  ;; Register previous mouse button states
  (loop for i from 0 below +max-mouse-buttons+ do
    (setf (aref (core-data-input-mouse-previous-button-state *core*) i) 
          (aref (core-data-input-mouse-current-button-state *core*) i)))
  
  ;; Update mouse position from GLFW (save previous first)
  (let ((window (platform-data-handle *platform*)))
    (when window
      (setf (core-data-input-mouse-previous-position *core*) 
            (core-data-input-mouse-current-position *core*))
      (let* ((pos (glfw:cursor-location window))
             (x (first pos))
             (y (second pos)))
        (setf (core-data-input-mouse-current-position *core*) (vec2 x y)))))
  
  ;; Reset wheel movement after one frame
  (setf (core-data-input-mouse-previous-wheel-move *core*) 
        (core-data-input-mouse-current-wheel-move *core*))
  (setf (core-data-input-mouse-current-wheel-move *core*) (vec2 0 0))
  
  ;; Update gamepad state
  (update-gamepad-state)
  
  ;; Poll GLFW events (critical - this triggers callbacks that update current state)
  (glfw:poll-events)
  
  ;; Update window should close status from GLFW (like raylib)
  (when (platform-data-handle *platform*)
    (setf (core-data-window-should-close *core*) 
          (glfw:should-close-p (platform-data-handle *platform*)))))

;;; Keyboard input functions

(defun is-key-pressed (key)
  "Check if a key was pressed this frame"
  (let ((actual-key (keyword-to-key key)))
    (and (< actual-key +max-keyboard-keys+)
         (aref (core-data-input-keyboard-current-state *core*) actual-key)
         (not (aref (core-data-input-keyboard-previous-state *core*) actual-key)))))

(defun is-key-down (key)
  "Check if a key is being held down"
  (let ((actual-key (keyword-to-key key)))
    (and (< actual-key +max-keyboard-keys+)
         (aref (core-data-input-keyboard-current-state *core*) actual-key))))

(defun is-key-released (key)
  "Check if a key was released this frame"
  (let ((actual-key (keyword-to-key key)))
    (and (< actual-key +max-keyboard-keys+)
         (not (aref (core-data-input-keyboard-current-state *core*) actual-key))
         (aref (core-data-input-keyboard-previous-state *core*) actual-key))))

(defun is-key-up (key)
  "Check if a key is not being pressed"
  (let ((actual-key (keyword-to-key key)))
    (and (< actual-key +max-keyboard-keys+)
         (not (aref (core-data-input-keyboard-current-state *core*) actual-key)))))

(defun get-key-pressed ()
  "Get key pressed (keycode), call it multiple times for keys queued"
  ;; Simple implementation - return the first key found pressed
  (loop for i from 0 below 512 do
    (when (is-key-pressed i)
      (return i)))
  0)

(defun get-char-pressed ()
  "Get char pressed (unicode), call it multiple times for chars queued"
  (if *char-queue*
      (let ((char (pop *char-queue*)))
        (decf *char-queue-count*)
        char)
      0))

;;; Mouse input functions

(defun is-mouse-button-pressed (button)
  "Check if a mouse button was pressed this frame"
  (let ((actual-button (keyword-to-mouse-button button)))
    (and (< actual-button +max-mouse-buttons+)
         (aref (core-data-input-mouse-current-button-state *core*) actual-button)
         (not (aref (core-data-input-mouse-previous-button-state *core*) actual-button)))))

(defun is-mouse-button-down (button)
  "Check if a mouse button is being held down"
  (let ((actual-button (keyword-to-mouse-button button)))
    (and (< actual-button +max-mouse-buttons+)
         (aref (core-data-input-mouse-current-button-state *core*) actual-button))))

(defun is-mouse-button-released (button)
  "Check if a mouse button was released this frame"
  (let ((actual-button (keyword-to-mouse-button button)))
    (and (< actual-button +max-mouse-buttons+)
         (not (aref (core-data-input-mouse-current-button-state *core*) actual-button))
         (aref (core-data-input-mouse-previous-button-state *core*) actual-button))))

(defun is-mouse-button-up (button)
  "Check if a mouse button is not being pressed"
  (let ((actual-button (keyword-to-mouse-button button)))
    (and (< actual-button +max-mouse-buttons+)
         (not (aref (core-data-input-mouse-current-button-state *core*) actual-button)))))

(defun get-mouse-position ()
  "Get mouse position as Vector2"
  (vec2 (get-mouse-x) (get-mouse-y)))

(defun get-mouse-x ()
  "Get mouse position X"
  (truncate
   (* (+ (vx (core-data-input-mouse-current-position *core*)) (vx (core-data-input-mouse-offset *core*)))
      (vx (core-data-input-mouse-scale *core*)))))

(defun get-mouse-y ()
  "Get mouse position Y"
  (truncate
   (* (+ (vy (core-data-input-mouse-current-position *core*)) (vy (core-data-input-mouse-offset *core*)))
      (vy (core-data-input-mouse-scale *core*)))))

(defun set-mouse-position (x y)
  "Set mouse position"
  (when (platform-data-handle *platform*)
    (setf (glfw:cursor-location (platform-data-handle *platform*)) (list x y)))
  (setf (core-data-input-mouse-current-position *core*) (vec2 x y)))

(defun get-mouse-delta ()
  "Get mouse position delta between frames"
  (v- (core-data-input-mouse-current-position *core*) (core-data-input-mouse-previous-position *core*)))

(defun set-mouse-cursor (cursor)
  "Set mouse cursor type"
  (setf (core-data-input-mouse-cursor *core*) cursor)
  ;; Implementation would set GLFW cursor here
  )

;;; Cursor visibility functions
(defun disable-cursor ()
  "Disable cursor and lock it to the center of the window"
  (when (platform-data-handle *platform*)
    (setf (glfw:input-mode :cursor (platform-data-handle *platform*)) :cursor-disabled)))

(defun enable-cursor ()
  "Enable cursor and unlock it"
  (when (platform-data-handle *platform*)
    (setf (glfw:input-mode :cursor (platform-data-handle *platform*)) :cursor-normal)
    (setf (core-data-input-mouse-cursor-hidden *core*) nil)))

(defun hide-cursor ()
  "Hide cursor but keep it functional"
  (when (platform-data-handle *platform*)
    (setf (glfw:input-mode :cursor (platform-data-handle *platform*)) :cursor-hidden)
    (setf (core-data-input-mouse-cursor-hidden *core*) t)))

(defun show-cursor ()
  "Show cursor (alias for enable-cursor)"
  (enable-cursor))

(defun is-cursor-hidden ()
  "Check if cursor is currently hidden"
  (core-data-input-mouse-cursor-hidden *core*))

(defun get-mouse-wheel-move ()
  "Get mouse wheel movement for X or Y axis"
  (vy (core-data-input-mouse-current-wheel-move *core*))) ; Return Y component for compatibility

(defun get-mouse-wheel-move-v ()
  "Get mouse wheel movement for both X and Y axis"
  (core-data-input-mouse-current-wheel-move *core*))

;;; Gamepad input functions

(defun update-gamepad-state ()
  "Update gamepad states"
  (loop for gamepad from 0 below +max-gamepads+ do
    (setf (aref (core-data-input-gamepad-ready *core*) gamepad) 
          (%glfw:joystick-present gamepad))
    
    (when (aref (core-data-input-gamepad-ready *core*) gamepad)
      ;; Update axes
      (cffi:with-foreign-object (count :int)
        (let ((axes-ptr (%glfw:get-joystick-axes gamepad count)))
          (if (cffi:null-pointer-p axes-ptr)
              (setf (aref (core-data-input-gamepad-axis-count *core*) gamepad) 0)
              (let ((axes-count (cffi:mem-ref count :int)))
                (setf (aref (core-data-input-gamepad-axis-count *core*) gamepad) axes-count)
                (loop for i from 0 below (min +max-gamepad-axes+ axes-count) do
                  (setf (aref (core-data-input-gamepad-axis-state *core*) gamepad i)
                        (cffi:mem-aref axes-ptr :float i)))))))
      
      ;; Update buttons
      (cffi:with-foreign-object (count :int)
        (let ((buttons-ptr (%glfw:get-joystick-buttons gamepad count)))
        ;; Copy current to previous
        (loop for i from 0 below +max-gamepad-buttons+ do
          (setf (aref (core-data-input-gamepad-previous-button-state *core*) gamepad i)
                (aref (core-data-input-gamepad-current-button-state *core*) gamepad i)))
        ;; Update current
          (unless (cffi:null-pointer-p buttons-ptr)
            (let ((buttons-count (cffi:mem-ref count :int)))
              (loop for i from 0 below (min +max-gamepad-buttons+ buttons-count) do
                (setf (aref (core-data-input-gamepad-current-button-state *core*) gamepad i)
                      (= (cffi:mem-aref buttons-ptr :unsigned-char i) +glfw-press+))))))))))

(defun is-gamepad-available (gamepad)
  "Check if a gamepad is available"
  (and (< gamepad +max-gamepads+) (aref (core-data-input-gamepad-ready *core*) gamepad)))

(defun get-gamepad-name (gamepad)
  "Get gamepad internal name id"
  (if (and (< gamepad +max-gamepads+) (aref (core-data-input-gamepad-ready *core*) gamepad))
      (%glfw:get-joystick-name gamepad)
      ""))

(defun is-gamepad-button-pressed (gamepad button)
  "Check if a gamepad button was pressed this frame"
  (and (< gamepad +max-gamepads+) (< button +max-gamepad-buttons+)
       (aref (core-data-input-gamepad-ready *core*) gamepad)
       (aref (core-data-input-gamepad-current-button-state *core*) gamepad button)
       (not (aref (core-data-input-gamepad-previous-button-state *core*) gamepad button))))

(defun is-gamepad-button-down (gamepad button)
  "Check if a gamepad button is being held down"
  (and (< gamepad +max-gamepads+) (< button +max-gamepad-buttons+)
       (aref (core-data-input-gamepad-ready *core*) gamepad)
       (aref (core-data-input-gamepad-current-button-state *core*) gamepad button)))

(defun is-gamepad-button-released (gamepad button)
  "Check if a gamepad button was released this frame"
  (and (< gamepad +max-gamepads+) (< button +max-gamepad-buttons+)
       (aref (core-data-input-gamepad-ready *core*) gamepad)
       (not (aref (core-data-input-gamepad-current-button-state *core*) gamepad button))
       (aref (core-data-input-gamepad-previous-button-state *core*) gamepad button)))

(defun is-gamepad-button-up (gamepad button)
  "Check if a gamepad button is not being pressed"
  (and (< gamepad +max-gamepads+) (< button +max-gamepad-buttons+)
       (aref (core-data-input-gamepad-ready *core*) gamepad)
       (not (aref (core-data-input-gamepad-current-button-state *core*) gamepad button))))

(defun get-gamepad-axis-count (gamepad)
  "Get gamepad axis count"
  (if (< gamepad +max-gamepads+)
      (aref (core-data-input-gamepad-axis-count *core*) gamepad)
      0))

(defun get-gamepad-axis-movement (gamepad axis)
  "Get axis movement value for a gamepad axis"
  (if (and (< gamepad +max-gamepads+) (< axis +max-gamepad-axes+) (aref (core-data-input-gamepad-ready *core*) gamepad))
      (aref (core-data-input-gamepad-axis-state *core*) gamepad axis)
      0.0))

(defun get-gamepad-button-count (gamepad)
  "Get gamepad button count"
  (if (< gamepad +max-gamepads+)
      +max-gamepad-buttons+
      0))

;;; Touch input (placeholder - mobile platforms)
(defun get-touch-point-count ()
  "Get number of touch points"
  0)

(defun get-touch-position (index)
  "Get touch point position for a given index"
  (declare (ignore index))
  (vec2 0 0))

(defun get-touch-point-id (index)
  "Get touch point identifier for given index"
  (declare (ignore index))
  0)

;;; Gesture input (placeholder)
(defun set-gestures-enabled (flags)
  "Enable a set of gestures using flags"
  (declare (ignore flags)))

(defun is-gesture-detected (gesture)
  "Check if a gesture have been detected"
  (declare (ignore gesture))
  nil)

(defun get-gesture-detected ()
  "Get latest detected gesture"
  0)

(defun get-gesture-hold-duration ()
  "Get gesture hold time in milliseconds"
  0.0)

(defun get-gesture-drag-vector ()
  "Get gesture drag vector"
  (vec2 0 0))

(defun get-gesture-drag-angle ()
  "Get gesture drag angle"
  0.0)

(defun get-gesture-pinch-vector ()
  "Get gesture pinch delta"
  (vec2 0 0))

(defun get-gesture-pinch-angle ()
  "Get gesture pinch angle"
  0.0)

;;; Input mode functions
(defvar *exit-key* +key-escape+ "Key that triggers window close")

(defun set-exit-key (key)
  "Set a custom key to exit program (default is ESC)"
  (setf *exit-key* key))

;;; Keyword to constant mapping for compatibility
(defun keyword-to-key (key-keyword)
  "Convert keyword key names to key constants for cl-raylib.cffi compatibility"
  (case key-keyword
    ;; Special keys (both formats supported)
    ((:key-null :null) +key-null+)
    ((:key-space :space) +key-space+)
    ((:key-escape :escape) +key-escape+)
    ((:key-enter :enter) +key-enter+)
    ((:key-tab :tab) +key-tab+)
    ((:key-backspace :backspace) +key-backspace+)
    ((:key-insert :insert) +key-insert+)
    ((:key-delete :delete) +key-delete+)
    
    ;; Arrow keys (both formats supported)
    ((:key-right :right) +key-right+)
    ((:key-left :left) +key-left+)
    ((:key-up :up) +key-up+)
    ((:key-down :down) +key-down+)
    (:key-page-up +key-page-up+)
    (:key-page-down +key-page-down+)
    (:key-home +key-home+)
    (:key-end +key-end+)
    
    ;; Lock keys
    (:key-caps-lock +key-caps-lock+)
    (:key-scroll-lock +key-scroll-lock+)
    (:key-num-lock +key-num-lock+)
    (:key-print-screen +key-print-screen+)
    (:key-pause +key-pause+)
    
    ;; Function keys
    (:key-f1 +key-f1+) (:key-f2 +key-f2+) (:key-f3 +key-f3+) (:key-f4 +key-f4+)
    (:key-f5 +key-f5+) (:key-f6 +key-f6+) (:key-f7 +key-f7+) (:key-f8 +key-f8+)
    (:key-f9 +key-f9+) (:key-f10 +key-f10+) (:key-f11 +key-f11+) (:key-f12 +key-f12+)
    
    ;; Numbers
    (:key-zero +key-zero+) (:key-one +key-one+) (:key-two +key-two+) (:key-three +key-three+)
    (:key-four +key-four+) (:key-five +key-five+) (:key-six +key-six+) (:key-seven +key-seven+)
    (:key-eight +key-eight+) (:key-nine +key-nine+)
    
    ;; Letters A-Z (both formats supported)
    ((:key-a :a) +key-a+) ((:key-b :b) +key-b+) ((:key-c :c) +key-c+) ((:key-d :d) +key-d+) ((:key-e :e) +key-e+)
    ((:key-f :f) +key-f+) ((:key-g :g) +key-g+) ((:key-h :h) +key-h+) ((:key-i :i) +key-i+) ((:key-j :j) +key-j+)
    ((:key-k :k) +key-k+) ((:key-l :l) +key-l+) ((:key-m :m) +key-m+) ((:key-n :n) +key-n+) ((:key-o :o) +key-o+)
    ((:key-p :p) +key-p+) ((:key-q :q) +key-q+) ((:key-r :r) +key-r+) ((:key-s :s) +key-s+) ((:key-t :t) +key-t+)
    ((:key-u :u) +key-u+) ((:key-v :v) +key-v+) ((:key-w :w) +key-w+) ((:key-x :x) +key-x+) ((:key-y :y) +key-y+)
    ((:key-z :z) +key-z+)
    
    ;; Punctuation
    (:key-apostrophe +key-apostrophe+)
    (:key-comma +key-comma+)
    (:key-minus +key-minus+)
    (:key-period +key-period+)
    (:key-slash +key-slash+)
    (:key-semicolon +key-semicolon+)
    (:key-equal +key-equal+)
    
    ;; Modifier keys
    (:key-left-shift +key-left-shift+)
    (:key-left-control +key-left-control+)
    (:key-left-alt +key-left-alt+)
    (:key-left-super +key-left-super+)
    (:key-right-shift +key-right-shift+)
    (:key-right-control +key-right-control+)
    (:key-right-alt +key-right-alt+)
    (:key-right-super +key-right-super+)
    
    ;; Return original if already a number or unknown keyword
    (t (if (numberp key-keyword) key-keyword key-keyword))))

(defun keyword-to-mouse-button (button-keyword)
  "Convert keyword mouse button names to button constants"
  (case button-keyword
    (:mouse-button-left +mouse-button-left+)
    (:mouse-button-right +mouse-button-right+)
    (:mouse-button-middle +mouse-button-middle+)
    (:mouse-button-side +mouse-button-side+)
    (:mouse-button-extra +mouse-button-extra+)
    (:mouse-button-forward +mouse-button-forward+)
    (:mouse-button-back +mouse-button-back+)
    (t (if (numberp button-keyword) button-keyword button-keyword))))

;;; File Drop Functions

(defun is-file-dropped ()
  "Check if a file has been dropped into window (matches raylib IsFileDropped)"
  (> (core-data-window-drop-file-count *core*) 0))

(defun load-dropped-files ()
  "Load dropped filepaths (matches raylib LoadDroppedFiles)"
  (make-file-path-list
   :capacity (core-data-window-drop-file-count *core*)
   :count (core-data-window-drop-file-count *core*)
   :paths (copy-list (core-data-window-drop-filepaths *core*))))

(defun unload-dropped-files (files)
  "Unload dropped filepaths (matches raylib UnloadDroppedFiles)"
  (declare (ignore files)) ; We don't need to free individual strings in Lisp
  ;; Clear the internal state
  (setf (core-data-window-drop-filepaths *core*) nil)
  (setf (core-data-window-drop-file-count *core*) 0))

(defun file-path-list-path (file-path-list index)
  "Get a specific path from a FilePathList by index"
  (when (and (< index (file-path-list-count file-path-list))
             (>= index 0))
    (nth index (file-path-list-paths file-path-list))))
