;;;; core_high_dpi.lisp - HighDPI example (simplified)
;;;; Translated from raylib/examples/core/core_high_dpi.c

(require :cl-raylib)

(defpackage :core-high-dpi
  (:use :cl :cl-raylib))

(in-package :core-high-dpi)

(defun draw-text-center (text x y font-size color)
  "Draw text centered at position"
  (let* ((text-width (measure-text text font-size))
         (pos-x (- x (/ text-width 2)))
         (pos-y (- y (/ font-size 2))))
    (draw-text text pos-x pos-y font-size color)))

(defun main ()
  "Main function - HighDPI example"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (set-config-flags (logior +flag-window-highdpi+ +flag-window-resizable+))
    (init-window screen-width screen-height "raylib [core] example - highdpi")
    (set-window-min-size 450 450)

    (set-target-fps 60) ; Set game to run at 60 frames-per-second

    ;; Main game loop
    (loop until (window-should-close) do
      ;; Update
      (let ((monitor-count (get-monitor-count))
            (current-monitor (get-current-monitor)))
        
        ;; Switch monitor with N key
        (when (and (> monitor-count 1) (is-key-pressed +key-n+))
          (set-window-monitor (mod (1+ current-monitor) monitor-count)))

        ;; Draw
        (begin-drawing)
          (clear-background +raywhite+)

          ;; Calculate DPI scale (simplified)
          (let* ((screen-w (get-screen-width))
                 (render-w (get-render-width))
                 (dpi-scale (if (> screen-w 0) (/ (float render-w) (float screen-w)) 1.0))
                 (window-center (/ screen-w 2)))

            ;; Display DPI information
            (draw-text-center (text-format "DPI Scale: %.2f" dpi-scale) window-center 30 40 +darkgray+)
            (draw-text-center (text-format "Monitor: %d/%d ([N] next monitor)" (1+ current-monitor) monitor-count) 
                             window-center 70 16 +lightgray+)

            ;; Draw logical grid demonstration
            (let ((logical-grid-desc-y 120)
                  (logical-grid-label-y 150)
                  (logical-grid-top 180)
                  (logical-grid-bottom 260)
                  (cell-size 50))

              (draw-text-center (text-format "Window is %d \"logical points\" wide" screen-w) 
                               window-center logical-grid-desc-y 20 +orange+)

              ;; Draw logical grid
              (loop for i from cell-size by cell-size
                    for odd = t then (not odd)
                    while (< i screen-w) do
                (when odd
                  (draw-rectangle i logical-grid-top cell-size (- logical-grid-bottom logical-grid-top) +orange+))
                (draw-text-center (text-format "%d" i) i logical-grid-label-y 12 +lightgray+)
                (draw-line i (+ logical-grid-label-y 10) i logical-grid-bottom +gray+))

              ;; Draw physical pixel information
              (let ((pixel-grid-top (- logical-grid-bottom 20))
                    (pixel-grid-bottom (+ pixel-grid-top 80))
                    (pixel-grid-label-y (+ pixel-grid-bottom 30))
                    (pixel-grid-desc-y (+ pixel-grid-label-y 30)))

                (draw-text-center (text-format "Window is %d \"physical pixels\" wide" render-w) 
                                 window-center pixel-grid-desc-y 20 +blue+))

              ;; Draw corner text
              (let ((corner-text "Can you see this?")
                    (text-width (measure-text corner-text 16)))
                (draw-text corner-text 
                          (- screen-w text-width 5) 
                          (- (get-screen-height) 20) 
                          16 +lightgray+))))

        (end-drawing)))

    ;; De-Initialization
    (close-window))) ; Close window and OpenGL context

;; Run the example
(main)
