;;;; raylib [shapes] example - lines drawing
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Robin (@RobinsAviary)
;;;; Common Lisp port of raylib/examples/shapes/shapes_lines_drawing.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-lines-drawing
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-lines-drawing)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - lines drawing")

    (let (;; Hint text that shows before you click the screen
          (start-text t)
          ;; The mouse's position on the previous frame
          (mouse-position-previous (get-mouse-position))
          ;; The canvas to draw lines on
          (canvas (load-render-texture screen-width screen-height))
          ;; The line's thickness
          (line-thickness 8.0)
          ;; The lines hue (in HSV, from 0-360)
          (line-hue 0.0))

      ;; Clear the canvas to the background color
      (begin-texture-mode canvas)
      (clear-background +raywhite+)
      (end-texture-mode)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Disable the hint text once the user clicks
               (when (and (is-mouse-button-pressed +mouse-button-left+) start-text) (setf start-text nil))

               ;; Clear the canvas when the user middle-clicks
               (when (is-mouse-button-pressed +mouse-button-middle+)
                 (begin-texture-mode canvas)
                 (clear-background +raywhite+)
                 (end-texture-mode))

               ;; Store whether the left and right buttons are down
               (let ((left-button-down (is-mouse-button-down +mouse-button-left+))
                     (right-button-down (is-mouse-button-down +mouse-button-right+)))

                 (when (or left-button-down right-button-down)
                   ;; The color for the line
                   (let ((draw-color +white+))
                     (cond (left-button-down
                            ;; Increase the hue value by the distance our cursor has moved since the last frame (divided by 3)
                            (incf line-hue (/ (vector2-distance mouse-position-previous (get-mouse-position)) 3.0))

                            ;; While the hue is >=360, subtract it to bring it down into the range 0-360
                            ;; This is more visually accurate than resetting to zero
                            (loop while (>= line-hue 360.0) do (decf line-hue 360.0))

                            ;; Create the final color
                            (setf draw-color (color-from-hsv line-hue 1.0 1.0)))
                           (right-button-down (setf draw-color +raywhite+))) ; Use the background color as an "eraser"

                     ;; Draw the line onto the canvas
                     (begin-texture-mode canvas)

                     ;; Circles act as "caps", smoothing corners
                     (draw-circle-v mouse-position-previous (/ line-thickness 2.0) draw-color)
                     (draw-circle-v (get-mouse-position) (/ line-thickness 2.0) draw-color)
                     (draw-line-ex mouse-position-previous (get-mouse-position) line-thickness draw-color)

                     (end-texture-mode)))

                 ;; Update line thickness based on mousewheel
                 (incf line-thickness (get-mouse-wheel-move))
                 (setf line-thickness (clamp line-thickness 1.0 500.0))

                 ;; Update mouse's previous position
                 (setf mouse-position-previous (get-mouse-position))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 ;; Draw the render texture to the screen, flipped vertically to make it appear top-side up
                 (let ((texture (render-texture-texture canvas)))
                   (draw-texture-rec texture (make-rectangle :x 0.0 :y 0.0
                                                             :width (float (texture-width texture))
                                                             :height (float (- (texture-height texture))))
                                     (vector2-zero) +white+))

                 ;; Draw the preview circle
                 (unless left-button-down (draw-circle-lines-v (get-mouse-position) (/ line-thickness 2.0) (list 127 127 127 127)))

                 ;; Draw the hint text
                 (when start-text (draw-text "try clicking and dragging!" 275 215 20 +lightgray+))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-render-texture canvas)    ; Unload the canvas render texture

      (close-window))))                 ; Close window and OpenGL context

(main)
