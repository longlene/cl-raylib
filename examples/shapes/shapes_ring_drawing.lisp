;;;; raylib [shapes] example - ring drawing
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 2.5
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2018-2025 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_ring_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-ring-drawing
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-ring-drawing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - ring drawing")

    (let ((center (vec2 (/ (- (get-screen-width) 300) 2.0) (/ (get-screen-height) 2.0)))

          (inner-radius 80.0)
          (outer-radius 190.0)

          (start-angle 0.0)
          (end-angle 360.0)
          (segments 0.0)

          (draw-ring t)
          (draw-ring-lines nil)
          (draw-circle-lines nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; NOTE: All variables update happens inside GUI control functions
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (draw-line 500 0 500 (get-screen-height) (fade +lightgray+ 0.6))
               (draw-rectangle 500 0 (- (get-screen-width) 500) (get-screen-height) (fade +lightgray+ 0.3))

               (when draw-ring (draw-ring center inner-radius outer-radius start-angle end-angle (truncate segments) (fade +maroon+ 0.3)))
               (when draw-ring-lines (draw-ring-lines center inner-radius outer-radius start-angle end-angle (truncate segments) (fade +black+ 0.4)))
               (when draw-circle-lines (draw-circle-sector-lines center outer-radius start-angle end-angle (truncate segments) (fade +black+ 0.4)))

               ;; Draw GUI controls
               ;;------------------------------------------------------------------------------
               (setf start-angle (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 40.0 :width 120.0 :height 20.0) "StartAngle" (text-format "%.2f" start-angle) start-angle -450 450)))
               (setf end-angle (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 70.0 :width 120.0 :height 20.0) "EndAngle" (text-format "%.2f" end-angle) end-angle -450 450)))

               (setf inner-radius (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 140.0 :width 120.0 :height 20.0) "InnerRadius" (text-format "%.2f" inner-radius) inner-radius 0 100)))
               (setf outer-radius (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 170.0 :width 120.0 :height 20.0) "OuterRadius" (text-format "%.2f" outer-radius) outer-radius 0 200)))

               (setf segments (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 240.0 :width 120.0 :height 20.0) "Segments" (text-format "%.2f" segments) segments 0 100)))

               (setf draw-ring (nth-value 1 (gui-check-box (make-rectangle :x 600.0 :y 320.0 :width 20.0 :height 20.0) "Draw Ring" draw-ring)))
               (setf draw-ring-lines (nth-value 1 (gui-check-box (make-rectangle :x 600.0 :y 350.0 :width 20.0 :height 20.0) "Draw RingLines" draw-ring-lines)))
               (setf draw-circle-lines (nth-value 1 (gui-check-box (make-rectangle :x 600.0 :y 380.0 :width 20.0 :height 20.0) "Draw CircleLines" draw-circle-lines)))
               ;;------------------------------------------------------------------------------

               (let ((min-segments (truncate (fceiling (/ (- end-angle start-angle) 90)))))
                 (draw-text (text-format "MODE: %s" (if (>= segments min-segments) "MANUAL" "AUTO")) 600 270 10 (if (>= segments min-segments) +maroon+ +darkgray+)))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
