;;;; raylib [core] example - window flags
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 3.5, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2020-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_window_flags.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-window-flags
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-window-flags)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;---------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    ;; Possible window flags
    #|
    FLAG_VSYNC_HINT
    FLAG_FULLSCREEN_MODE    -> not working properly -> wrong scaling!
    FLAG_WINDOW_RESIZABLE
    FLAG_WINDOW_UNDECORATED
    FLAG_WINDOW_TRANSPARENT
    FLAG_WINDOW_HIDDEN
    FLAG_WINDOW_MINIMIZED   -> Not supported on window creation
    FLAG_WINDOW_MAXIMIZED   -> Not supported on window creation
    FLAG_WINDOW_UNFOCUSED
    FLAG_WINDOW_TOPMOST
    FLAG_WINDOW_HIGHDPI     -> errors after minimize-resize, fb size is recalculated
    FLAG_WINDOW_ALWAYS_RUN
    FLAG_MSAA_4X_HINT
    |#

    ;; Set configuration flags for window creation
    ;;(set-config-flags (logior +flag-vsync-hint+ +flag-msaa-4x-hint+ +flag-window-highdpi+)) ; +flag-window-transparent+
    (init-window screen-width screen-height "raylib [core] example - window flags")

    (let ((ball-position (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))
          (ball-speed (vec2 5.0 4.0))
          (ball-radius 20.0)
          (frames-counter 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;----------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;-----------------------------------------------------
               (when (is-key-pressed +key-f+) (toggle-fullscreen)) ; modifies window size when scaling!

               (when (is-key-pressed +key-r+)
                 (if (is-window-state +flag-window-resizable+)
                     (clear-window-state +flag-window-resizable+)
                     (set-window-state +flag-window-resizable+)))

               (when (is-key-pressed +key-d+)
                 (if (is-window-state +flag-window-undecorated+)
                     (clear-window-state +flag-window-undecorated+)
                     (set-window-state +flag-window-undecorated+)))

               (when (is-key-pressed +key-h+)
                 (unless (is-window-state +flag-window-hidden+) (set-window-state +flag-window-hidden+))
                 (setf frames-counter 0))

               (when (is-window-state +flag-window-hidden+)
                 (incf frames-counter)
                 (when (>= frames-counter 240) (clear-window-state +flag-window-hidden+))) ; Show window after 3 seconds

               (when (is-key-pressed +key-n+)
                 (unless (is-window-state +flag-window-minimized+) (minimize-window))
                 (setf frames-counter 0))

               (when (is-window-state +flag-window-minimized+)
                 (incf frames-counter)
                 (when (>= frames-counter 240)
                   (restore-window)     ; Restore window after 3 seconds
                   (setf frames-counter 0)))

               (when (is-key-pressed +key-m+)
                 ;; NOTE: Requires FLAG_WINDOW_RESIZABLE enabled!
                 (if (is-window-state +flag-window-maximized+)
                     (restore-window)
                     (maximize-window)))

               (when (is-key-pressed +key-u+)
                 (if (is-window-state +flag-window-unfocused+)
                     (clear-window-state +flag-window-unfocused+)
                     (set-window-state +flag-window-unfocused+)))

               (when (is-key-pressed +key-t+)
                 (if (is-window-state +flag-window-topmost+)
                     (clear-window-state +flag-window-topmost+)
                     (set-window-state +flag-window-topmost+)))

               (when (is-key-pressed +key-a+)
                 (if (is-window-state +flag-window-always-run+)
                     (clear-window-state +flag-window-always-run+)
                     (set-window-state +flag-window-always-run+)))

               (when (is-key-pressed +key-v+)
                 (if (is-window-state +flag-vsync-hint+)
                     (clear-window-state +flag-vsync-hint+)
                     (set-window-state +flag-vsync-hint+)))

               (when (is-key-pressed +key-b+) (toggle-borderless-windowed))

               ;; Bouncing ball logic
               (incf (vx ball-position) (vx ball-speed))
               (incf (vy ball-position) (vy ball-speed))
               (when (or (>= (vx ball-position) (- (get-screen-width) ball-radius)) (<= (vx ball-position) ball-radius))
                 (setf (vx ball-speed) (* (vx ball-speed) -1.0)))
               (when (or (>= (vy ball-position) (- (get-screen-height) ball-radius)) (<= (vy ball-position) ball-radius))
                 (setf (vy ball-speed) (* (vy ball-speed) -1.0)))
               ;;-----------------------------------------------------

               ;; Draw
               ;;-----------------------------------------------------
               (begin-drawing)

               (if (is-window-state +flag-window-transparent+)
                   (clear-background +blank+)
                   (clear-background +raywhite+))

               (draw-circle-v ball-position ball-radius +maroon+)
               (draw-rectangle-lines-ex (make-rectangle :x 0.0 :y 0.0
                                                        :width (float (get-screen-width))
                                                        :height (float (get-screen-height)))
                                        4.0 +raywhite+)

               (draw-circle-v (get-mouse-position) 10.0 +darkblue+)

               (draw-fps 10 10)

               (draw-text (text-format "Screen Size: [%i, %i]" (get-screen-width) (get-screen-height)) 10 40 10 +green+)

               ;; Draw window state info
               (draw-text "Following flags can be set after window creation:" 10 60 10 +gray+)
               (if (is-window-state +flag-fullscreen-mode+)
                   (draw-text "[F] FLAG_FULLSCREEN_MODE: on" 10 80 10 +lime+)
                   (draw-text "[F] FLAG_FULLSCREEN_MODE: off" 10 80 10 +maroon+))
               (if (is-window-state +flag-window-resizable+)
                   (draw-text "[R] FLAG_WINDOW_RESIZABLE: on" 10 100 10 +lime+)
                   (draw-text "[R] FLAG_WINDOW_RESIZABLE: off" 10 100 10 +maroon+))
               (if (is-window-state +flag-window-undecorated+)
                   (draw-text "[D] FLAG_WINDOW_UNDECORATED: on" 10 120 10 +lime+)
                   (draw-text "[D] FLAG_WINDOW_UNDECORATED: off" 10 120 10 +maroon+))
               (if (is-window-state +flag-window-hidden+)
                   (draw-text "[H] FLAG_WINDOW_HIDDEN: on" 10 140 10 +lime+)
                   (draw-text "[H] FLAG_WINDOW_HIDDEN: off (hides for 3 seconds)" 10 140 10 +maroon+))
               (if (is-window-state +flag-window-minimized+)
                   (draw-text "[N] FLAG_WINDOW_MINIMIZED: on" 10 160 10 +lime+)
                   (draw-text "[N] FLAG_WINDOW_MINIMIZED: off (restores after 3 seconds)" 10 160 10 +maroon+))
               (if (is-window-state +flag-window-maximized+)
                   (draw-text "[M] FLAG_WINDOW_MAXIMIZED: on" 10 180 10 +lime+)
                   (draw-text "[M] FLAG_WINDOW_MAXIMIZED: off" 10 180 10 +maroon+))
               (if (is-window-state +flag-window-unfocused+)
                   (draw-text "[G] FLAG_WINDOW_UNFOCUSED: on" 10 200 10 +lime+)
                   (draw-text "[U] FLAG_WINDOW_UNFOCUSED: off" 10 200 10 +maroon+))
               (if (is-window-state +flag-window-topmost+)
                   (draw-text "[T] FLAG_WINDOW_TOPMOST: on" 10 220 10 +lime+)
                   (draw-text "[T] FLAG_WINDOW_TOPMOST: off" 10 220 10 +maroon+))
               (if (is-window-state +flag-window-always-run+)
                   (draw-text "[A] FLAG_WINDOW_ALWAYS_RUN: on" 10 240 10 +lime+)
                   (draw-text "[A] FLAG_WINDOW_ALWAYS_RUN: off" 10 240 10 +maroon+))
               (if (is-window-state +flag-vsync-hint+)
                   (draw-text "[V] FLAG_VSYNC_HINT: on" 10 260 10 +lime+)
                   (draw-text "[V] FLAG_VSYNC_HINT: off" 10 260 10 +maroon+))
               (if (is-window-state +flag-borderless-windowed-mode+)
                   (draw-text "[B] FLAG_BORDERLESS_WINDOWED_MODE: on" 10 280 10 +lime+)
                   (draw-text "[B] FLAG_BORDERLESS_WINDOWED_MODE: off" 10 280 10 +maroon+))

               (draw-text "Following flags can only be set before window creation:" 10 320 10 +gray+)
               (if (is-window-state +flag-window-highdpi+)
                   (draw-text "FLAG_WINDOW_HIGHDPI: on" 10 340 10 +lime+)
                   (draw-text "FLAG_WINDOW_HIGHDPI: off" 10 340 10 +maroon+))
               (if (is-window-state +flag-window-transparent+)
                   (draw-text "FLAG_WINDOW_TRANSPARENT: on" 10 360 10 +lime+)
                   (draw-text "FLAG_WINDOW_TRANSPARENT: off" 10 360 10 +maroon+))
               (if (is-window-state +flag-msaa-4x-hint+)
                   (draw-text "FLAG_MSAA_4X_HINT: on" 10 380 10 +lime+)
                   (draw-text "FLAG_MSAA_4X_HINT: off" 10 380 10 +maroon+))

               (end-drawing))
      ;;-----------------------------------------------------

      ;; De-Initialization
      ;;---------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
