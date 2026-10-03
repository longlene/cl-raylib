;;;; raylib [core] example - highdpi demo
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 5.5
;;;;
;;;; Example contributed by Jonathan Marler (@marler8997) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Jonathan Marler (@marler8997)
;;;; Common Lisp port of raylib/examples/core/core_highdpi_demo.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-highdpi-demo
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-highdpi-demo)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun draw-text-center (text x y font-size color)
  (let* ((size (measure-text-ex (get-font-default) text (float font-size) 3.0))
         (pos (vec2 (- x (/ (vx size) 2)) (- y (/ (vy size) 2)))))
    (draw-text-ex (get-font-default) text pos (float font-size) 3.0 color)))

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags (logior +flag-window-highdpi+ +flag-window-resizable+))
    (init-window screen-width screen-height "raylib [core] example - highdpi demo")
    (set-window-min-size 450 450)

    (let* ((logical-grid-desc-y 120)
           (logical-grid-label-y (+ logical-grid-desc-y 30))
           (logical-grid-top (+ logical-grid-label-y 30))
           (logical-grid-bottom (+ logical-grid-top 80))
           (pixel-grid-top (- logical-grid-bottom 20))
           (pixel-grid-bottom (+ pixel-grid-top 80))
           (pixel-grid-label-y (+ pixel-grid-bottom 30))
           (pixel-grid-desc-y (+ pixel-grid-label-y 30))
           (cell-size 50)
           (cell-size-px (float cell-size)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((monitor-count (get-monitor-count)))
                 (when (and (> monitor-count 1) (is-key-pressed +key-n+))
                   (set-window-monitor (mod (1+ (get-current-monitor)) monitor-count)))

                 (let ((current-monitor (get-current-monitor))
                       (dpi-scale (get-window-scale-dpi)))
                   (setf cell-size-px (/ (float cell-size) (vx dpi-scale)))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)
                   (clear-background +raywhite+)

                   (let ((window-center (floor (get-screen-width) 2)))
                     (draw-text-center (text-format "Dpi Scale: %f" (vx dpi-scale)) window-center 30 40 +darkgray+)
                     (draw-text-center (text-format "Monitor: %d/%d ([N] next monitor)" (1+ current-monitor) monitor-count) window-center 70 20 +lightgray+)
                     (draw-text-center (text-format "Window is %d \"logical points\" wide" (get-screen-width)) window-center logical-grid-desc-y 20 +orange+)

                     (let ((odd t))
                       (loop for i from cell-size below (get-screen-width) by cell-size
                             do (when odd (draw-rectangle i logical-grid-top cell-size (- logical-grid-bottom logical-grid-top) +orange+))

                                (draw-text-center (text-format "%d" i) i logical-grid-label-y 10 +lightgray+)
                                (draw-line i (+ logical-grid-label-y 10) i logical-grid-bottom +gray+)
                                (setf odd (not odd))))

                     (let* ((odd t)
                            (min-text-space 30)
                            (last-text-x (- min-text-space)))
                       (loop for i from cell-size below (get-render-width) by cell-size
                             do (let ((x (truncate (/ (float i) (vx dpi-scale)))))
                                  (when odd (draw-rectangle x pixel-grid-top (truncate cell-size-px) (- pixel-grid-bottom pixel-grid-top) (list 0 121 241 100)))

                                  (draw-line x pixel-grid-top (truncate (/ (float i) (vx dpi-scale))) (- pixel-grid-label-y 10) +gray+)

                                  (when (>= (- x last-text-x) min-text-space)
                                    (draw-text-center (text-format "%d" i) x pixel-grid-label-y 10 +lightgray+)
                                    (setf last-text-x x))
                                  (setf odd (not odd)))))

                     (draw-text-center (text-format "Window is %d \"physical pixels\" wide" (get-render-width)) window-center pixel-grid-desc-y 20 +blue+)

                     (let* ((text "Can you see this?")
                            (size (measure-text-ex (get-font-default) text 20.0 3.0))
                            (pos (vec2 (- (get-screen-width) (vx size) 5) (- (get-screen-height) (vy size) 5))))
                       (draw-text-ex (get-font-default) text pos 20.0 3.0 +lightgray+)))

                   (end-drawing))))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
