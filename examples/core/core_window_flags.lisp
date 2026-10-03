(require :cl-raylib)

(in-package #:cl-raylib)

;;;; core_window_flags.lisp
;;;; Example demonstrating window configuration flags in cl-raylib
;;;; Based on raylib's core_window_flags.c example

(defun core-window-flags ()
  "Demonstrate window configuration flags"
  
  ;; Initialization
  (let* ((screen-width 800)
         (screen-height 450)
         (ball-position (vec2 (/ screen-width 2) (/ screen-height 2)))
         (ball-speed (vec2 5.0 4.0))
         (ball-radius 20.0)
         (frames-counter 0))
    
    ;; Initialize window with basic flags
    (init-window screen-width screen-height "raylib [core] example - window flags")
    
    ;; Set initial window state - make window resizable
    (set-window-state +flag-window-resizable+)
    
    (set-target-fps 60)
    
    ;; Main game loop
    (loop until (window-should-close) do
      
      ;; Update
      (incf frames-counter)
      
      ;; Handle keyboard input for window flags
      (cond
        ;; Toggle window resizable
        ((is-key-pressed :r)
         (if (is-window-state +flag-window-resizable+) ; Check if resizable flag is set
             (clear-window-state +flag-window-resizable+)
             (set-window-state +flag-window-resizable+)))
        
        ;; Toggle window decoration
        ((is-key-pressed :d) 
         (if (is-window-state +flag-window-undecorated+) ; Check if undecorated flag is set
             (clear-window-state +flag-window-undecorated+)
             (set-window-state +flag-window-undecorated+)))
        
        ;; Toggle window topmost
        ((is-key-pressed :t)
         (if (is-window-state +flag-window-topmost+) ; Check if topmost flag is set
             (clear-window-state +flag-window-topmost+)
             (set-window-state +flag-window-topmost+)))
        
        ;; Toggle window transparency  
        ((is-key-pressed :a)
         (if (is-window-state +flag-window-transparent+) ; Check if transparent flag is set
             (clear-window-state +flag-window-transparent+)
             (set-window-state +flag-window-transparent+)))
        
        ;; Toggle fullscreen
        ((is-key-pressed :f)
         (format t "DEBUG: F key pressed, calling toggle-fullscreen~%")
         (toggle-fullscreen))
        
        ;; Hide/Show window
        ((is-key-pressed :h)
         (if (is-window-hidden)
             (clear-window-state +flag-window-hidden+)
             (set-window-state +flag-window-hidden+)))
        
        ;; Minimize window
        ((is-key-pressed :m)
         (minimize-window))
        
        ;; Restore window
        ((is-key-pressed :space)
         (restore-window)))
      
      ;; Check if window was resized and update variables
      (when (is-window-resized)
        (setf screen-width (get-screen-width))
        (setf screen-height (get-screen-height)))
      
      ;; Update ball position
      (setf ball-position (v+ ball-position ball-speed))
      
      ;; Check walls collision for bouncing ball
      (when (or (>= (+ (vx ball-position) ball-radius) screen-width)
                (<= (- (vx ball-position) ball-radius) 0))
        (setf ball-speed (vec2 (* -1 (vx ball-speed)) (vy ball-speed))))
      
      (when (or (>= (+ (vy ball-position) ball-radius) screen-height)
                (<= (- (vy ball-position) ball-radius) 0))
        (setf ball-speed (vec2 (vx ball-speed) (* -1 (vy ball-speed)))))
      
      ;; Draw
      (with-drawing
        (clear-background +raywhite+)
        
        ;; Draw bouncing ball
        (draw-circle-v ball-position ball-radius +maroon+)
        
        ;; Draw status information
        (draw-text "Press keys to change window properties:" 10 10 18 +darkgray+)
        (draw-text "- R: Toggle window resizable" 10 40 12 +gray+)
        (draw-text "- D: Toggle window decoration" 10 60 12 +gray+)
        (draw-text "- T: Toggle window topmost" 10 80 12 +gray+)
        (draw-text "- A: Toggle window transparency" 10 100 12 +gray+)
        (draw-text "- F: Toggle fullscreen" 10 120 12 +gray+)
        (draw-text "- H: Hide/Show window" 10 140 12 +gray+)
        (draw-text "- M: Minimize window" 10 160 12 +gray+)
        (draw-text "- SPACE: Restore window" 10 180 12 +gray+)
        
        ;; Display current window status
        (let ((y-pos 220))
          (draw-text "Current window flags:" 10 y-pos 14 +black+)
          (incf y-pos 25)
          
          (when (is-window-state +flag-window-resizable+)
            (draw-text "FLAG_WINDOW_RESIZABLE" 10 y-pos 10 +lime+)
            (incf y-pos 15))
          
          (when (is-window-state +flag-window-undecorated+)
            (draw-text "FLAG_WINDOW_UNDECORATED" 10 y-pos 10 +lime+)
            (incf y-pos 15))
          
          (when (is-window-state +flag-window-topmost+)
            (draw-text "FLAG_WINDOW_TOPMOST" 10 y-pos 10 +lime+)
            (incf y-pos 15))
          
          (when (is-window-state +flag-window-transparent+)
            (draw-text "FLAG_WINDOW_TRANSPARENT" 10 y-pos 10 +lime+)
            (incf y-pos 15))
          
          (when (is-window-state +flag-fullscreen-mode+)
            (draw-text "FLAG_FULLSCREEN_MODE" 10 y-pos 10 +lime+)
            (incf y-pos 15))
          
          ;; Show window state
          (when (is-window-minimized)
            (draw-text "WINDOW MINIMIZED" 10 y-pos 10 +red+)
            (incf y-pos 15))
          
          (when (is-window-maximized)
            (draw-text "WINDOW MAXIMIZED" 10 y-pos 10 +green+)
            (incf y-pos 15))
          
          (when (is-window-hidden)
            (draw-text "WINDOW HIDDEN" 10 y-pos 10 +yellow+)
            (incf y-pos 15))
          
          (when (is-window-focused)
            (draw-text "WINDOW FOCUSED" 10 y-pos 10 +blue+)
            (incf y-pos 15)))
        
        ;; Draw window size information
        (draw-text (format nil "Window size: ~dx~d" screen-width screen-height) 
                   10 (- screen-height 40) 12 +darkgreen+)
        
        ;; Draw FPS
        (draw-fps 10 (- screen-height 20))))
    
    ;; De-Initialization
    (close-window)))

;; Run the example
(defun run-core-window-flags ()
  "Run the core window flags example"
  (core-window-flags))

(run-core-window-flags)
