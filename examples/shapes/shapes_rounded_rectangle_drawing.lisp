;;;; raylib [shapes] example - rounded rectangle drawing
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
;;;; Common Lisp port of raylib/examples/shapes/shapes_rounded_rectangle_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-rounded-rectangle-drawing
  (:use #:cl #:raylib #:raygui))        ; raygui: Required for GUI controls
(in-package #:raylib-examples/shapes-rounded-rectangle-drawing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - rounded rectangle drawing")

    (let ((roundness 0.2)
          (width 200.0)
          (height 100.0)
          (segments 0.0)
          (line-thick 1.0)

          (draw-rect nil)
          (draw-rounded-rect t)
          (draw-rounded-lines nil))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((rec (make-rectangle :x (/ (- (float (get-screen-width)) width 250) 2) :y (/ (- (get-screen-height) height) 2.0)
                                          :width width :height height)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (draw-line 560 0 560 (get-screen-height) (fade +lightgray+ 0.6))
                 (draw-rectangle 560 0 (- (get-screen-width) 500) (get-screen-height) (fade +lightgray+ 0.3))

                 (when draw-rect (draw-rectangle-rec rec (fade +gold+ 0.6)))
                 (when draw-rounded-rect (draw-rectangle-rounded rec roundness (truncate segments) (fade +maroon+ 0.2)))
                 (when draw-rounded-lines (draw-rectangle-rounded-lines-ex rec roundness (truncate segments) line-thick (fade +maroon+ 0.4)))

                 ;; Draw GUI controls
                 ;;------------------------------------------------------------------------------
                 (setf width (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 40.0 :width 105.0 :height 20.0) "Width" (text-format "%.2f" width) width 0 (- (float (get-screen-width)) 300))))
                 (setf height (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 70.0 :width 105.0 :height 20.0) "Height" (text-format "%.2f" height) height 0 (- (float (get-screen-height)) 50))))
                 (setf roundness (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 140.0 :width 105.0 :height 20.0) "Roundness" (text-format "%.2f" roundness) roundness 0.0 1.0)))
                 (setf line-thick (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 170.0 :width 105.0 :height 20.0) "Thickness" (text-format "%.2f" line-thick) line-thick 0 20)))
                 (setf segments (nth-value 1 (gui-slider-bar (make-rectangle :x 640.0 :y 240.0 :width 105.0 :height 20.0) "Segments" (text-format "%.2f" segments) segments 0 60)))

                 (setf draw-rounded-rect (nth-value 1 (gui-check-box (make-rectangle :x 640.0 :y 320.0 :width 20.0 :height 20.0) "DrawRoundedRect" draw-rounded-rect)))
                 (setf draw-rounded-lines (nth-value 1 (gui-check-box (make-rectangle :x 640.0 :y 350.0 :width 20.0 :height 20.0) "DrawRoundedLines" draw-rounded-lines)))
                 (setf draw-rect (nth-value 1 (gui-check-box (make-rectangle :x 640.0 :y 380.0 :width 20.0 :height 20.0) "DrawRect" draw-rect)))
                 ;;------------------------------------------------------------------------------

                 (draw-text (text-format "MODE: %s" (if (>= segments 4) "MANUAL" "AUTO")) 640 280 10 (if (>= segments 4) +maroon+ +darkgray+))

                 (draw-fps 10 10)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
