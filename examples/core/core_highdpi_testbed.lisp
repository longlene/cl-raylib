;;;; raylib [core] example - highdpi testbed
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Ramon Santamaria (@raysan5) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/core/core_highdpi_testbed.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-highdpi-testbed
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-highdpi-testbed)

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags (logior +flag-window-resizable+ +flag-window-highdpi+))
    (init-window screen-width screen-height "raylib [core] example - highdpi testbed")

    (let ((scale-dpi (get-window-scale-dpi))
          (mouse-pos (get-mouse-position))
          (current-monitor (get-current-monitor))
          (window-pos (get-window-position))
          (grid-spacing 40))            ; Grid spacing in pixels

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (setf mouse-pos (get-mouse-position)
                     current-monitor (get-current-monitor)
                     scale-dpi (get-window-scale-dpi)
                     window-pos (get-window-position))

               (when (is-key-pressed +key-space+) (toggle-borderless-windowed))
               (when (is-key-pressed +key-f+) (toggle-fullscreen))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               ;; Draw grid
               (dotimes (h (1+ (floor (get-screen-height) grid-spacing)))
                 (draw-text (text-format "%02i" (* h grid-spacing)) 4 (- (* h grid-spacing) 4) 10 +gray+)
                 (draw-line 24 (* h grid-spacing) (get-screen-width) (* h grid-spacing) +lightgray+))
               (dotimes (v (1+ (floor (get-screen-width) grid-spacing)))
                 (draw-text (text-format "%02i" (* v grid-spacing)) (- (* v grid-spacing) 10) 4 10 +gray+)
                 (draw-line (* v grid-spacing) 20 (* v grid-spacing) (get-screen-height) +lightgray+))

               ;; Draw UI info
               (draw-text (text-format "CURRENT MONITOR: %i/%i (%ix%i)" (1+ current-monitor) (get-monitor-count)
                                       (get-monitor-width current-monitor) (get-monitor-height current-monitor))
                          50 50 20 +darkgray+)
               (draw-text (text-format "WINDOW POSITION: %ix%i" (truncate (vx window-pos)) (truncate (vy window-pos))) 50 90 20 +darkgray+)
               (draw-text (text-format "SCREEN SIZE: %ix%i" (get-screen-width) (get-screen-height)) 50 130 20 +darkgray+)
               (draw-text (text-format "RENDER SIZE: %ix%i" (get-render-width) (get-render-height)) 50 170 20 +darkgray+)
               (draw-text (text-format "SCALE FACTOR: %.2fx%.2f" (vx scale-dpi) (vy scale-dpi)) 50 210 20 +gray+)

               ;; Draw reference rectangles, top-left and bottom-right corners
               (draw-rectangle 0 0 30 60 +red+)
               (draw-rectangle (- (get-screen-width) 30) (- (get-screen-height) 60) 30 60 +blue+)

               ;; Draw mouse position
               (draw-circle-v (get-mouse-position) 20.0 +maroon+)
               (draw-rectangle-rec (make-rectangle :x (- (vx mouse-pos) 25) :y (vy mouse-pos) :width 50.0 :height 2.0) +black+)
               (draw-rectangle-rec (make-rectangle :x (vx mouse-pos) :y (- (vy mouse-pos) 25) :width 2.0 :height 50.0) +black+)
               (draw-text (text-format "[%i,%i]" (get-mouse-x) (get-mouse-y)) (- (truncate (vx mouse-pos)) 44)
                          (if (> (vy mouse-pos) (- (get-screen-height) 60)) (- (truncate (vy mouse-pos)) 46) (+ (truncate (vy mouse-pos)) 30))
                          20 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
