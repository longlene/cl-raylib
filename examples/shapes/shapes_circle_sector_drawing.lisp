;;;; raylib [shapes] example - circle sector drawing
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
;;;; Common Lisp port of raylib/examples/shapes/shapes_circle_sector_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-circle-sector-drawing
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-circle-sector-drawing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - circle sector drawing")

    (let ((center (vec2 (/ (- (get-screen-width) 300) 2.0) (/ (get-screen-height) 2.0)))
          (outer-radius 180.0)
          (start-angle 0.0)
          (end-angle 180.0)
          (segments 10.0)
          (min-segments 4.0))

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

               (draw-circle-sector center outer-radius start-angle end-angle (truncate segments) (fade +maroon+ 0.3))
               (draw-circle-sector-lines center outer-radius start-angle end-angle (truncate segments) (fade +maroon+ 0.6))

               ;; Draw GUI controls
               ;;------------------------------------------------------------------------------
               (setf start-angle (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 40.0 :width 120.0 :height 20.0) "StartAngle" (text-format "%.2f" start-angle) start-angle 0 720)))
               (setf end-angle (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 70.0 :width 120.0 :height 20.0) "EndAngle" (text-format "%.2f" end-angle) end-angle 0 720)))

               (setf outer-radius (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 140.0 :width 120.0 :height 20.0) "Radius" (text-format "%.2f" outer-radius) outer-radius 0 200)))
               (setf segments (nth-value 1 (gui-slider-bar (make-rectangle :x 600.0 :y 170.0 :width 120.0 :height 20.0) "Segments" (text-format "%.2f" segments) segments 0 100)))
               ;;------------------------------------------------------------------------------

               (setf min-segments (ftruncate (fceiling (/ (- end-angle start-angle) 90))))
               (draw-text (text-format "MODE: %s" (if (>= segments min-segments) "MANUAL" "AUTO")) 600 200 10 (if (>= segments min-segments) +maroon+ +darkgray+))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
