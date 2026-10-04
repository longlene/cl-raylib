;;;; raylib [shapes] example - rlgl color wheel
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Robin (@RobinsAviary) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Robin (@RobinsAviary)
;;;; Common Lisp port of raylib/examples/shapes/shapes_rlgl_color_wheel.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-rlgl-color-wheel
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shapes-rlgl-color-wheel)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let* ((screen-width 800)
         (screen-height 450)

         ;; The minimum/maximum points the circle can have
         (points-min 3)
         (points-max 256)

         ;; The current number of points and the radius of the circle
         (triangle-count 64)
         (point-scale 150.0)

         ;; Slider value, literally maps to value in HSV
         (value 1.0)

         ;; The center of the screen
         (center (vec2 (/ (float screen-width) 2.0) (/ (float screen-height) 2.0)))
         ;; The location of the color wheel
         (circle-position (vcopy center))

         ;; The currently selected color
         (color (list 255 255 255 255))

         ;; Indicates if the slider is being clicked
         (slider-clicked nil)

         ;; Indicates if the current color going to be updated, as well as the handle position
         (setting-color nil)

         ;; How the color wheel will be rendered
         (render-type +rl-triangles+))

    ;; Enable anti-aliasing
    (set-config-flags +flag-msaa-4x-hint+)

    (init-window screen-width screen-height "raylib [shapes] example - rlgl color wheel")

    (set-target-fps 60)
    ;;--------------------------------------------------------------------------------------

    ;; Main game loop
    (loop until (window-should-close)   ; Detect window close button or ESC key
          do ;; Update
             ;;----------------------------------------------------------------------------------
             (incf triangle-count (truncate (get-mouse-wheel-move)))
             (setf triangle-count (truncate (clamp (float triangle-count) (float points-min) (float points-max))))

             (let* ((slider-rectangle (make-rectangle :x 42.0 :y (+ 16.0 64.0 45.0) :width 64.0 :height 16.0))
                    (mouse-position (get-mouse-position))

                    ;; Checks if the user is hovering over the value slider
                    (slider-hover (and (>= (vx mouse-position) (rectangle-x slider-rectangle)) (>= (vy mouse-position) (rectangle-y slider-rectangle))
                                       (< (vx mouse-position) (+ (rectangle-x slider-rectangle) (rectangle-width slider-rectangle)))
                                       (< (vy mouse-position) (+ (rectangle-y slider-rectangle) (rectangle-height slider-rectangle))))))

               ;; Copy color as hex
               (when (and (is-key-down +key-left-control+) (is-key-down +key-c+))
                 (when (is-key-pressed +key-c+)
                   (set-clipboard-text (text-format "#%02X%02X%02X" (first color) (second color) (third color)))))

               ;; Scale up the color wheel, adjusting the handle visually
               (when (is-key-down +key-up+)
                 (setf point-scale (* point-scale 1.025))

                 (if (> point-scale (/ (float screen-height) 2.0))
                     (setf point-scale (/ (float screen-height) 2.0))
                     (setf circle-position (vector2-add (vector2-multiply (vector2-subtract circle-position center) (vec2 1.025 1.025)) center))))

               ;; Scale down the wheel, adjusting the handle visually
               (when (is-key-down +key-down+)
                 (setf point-scale (* point-scale 0.975))

                 (if (< point-scale 32.0)
                     (setf point-scale 32.0)
                     (setf circle-position (vector2-add (vector2-multiply (vector2-subtract circle-position center) (vec2 0.975 0.975)) center)))

                 (let ((distance (/ (vector2-distance center circle-position) point-scale))
                       (angle (/ (+ (/ (vector2-angle (vec2 0.0 (- point-scale)) (vector2-subtract center circle-position)) +pi+) 1.0) 2.0)))

                   (when (> distance 1.0)
                     (setf circle-position (vector2-add (vec2 (* (sin (* angle (* +pi+ 2.0))) point-scale) (* (- (cos (* angle (* +pi+ 2.0)))) point-scale)) center)))))

               ;; Checks if the user clicked on the color wheel
               (when (and (is-mouse-button-pressed +mouse-button-left+) (<= (vector2-distance (get-mouse-position) center) (+ point-scale 10.0)))
                 (setf setting-color t))

               ;; Update flag when mouse button is released
               (when (is-mouse-button-released +mouse-button-left+) (setf setting-color nil))

               ;; Check if the user clicked/released the slider for the color's value
               (when (and slider-hover (is-mouse-button-pressed +mouse-button-left+)) (setf slider-clicked t))

               (when (and slider-clicked (is-mouse-button-released +mouse-button-left+)) (setf slider-clicked nil))

               ;; Update render mode accordingly
               (when (is-key-pressed +key-space+) (setf render-type +rl-lines+))
               (when (is-key-released +key-space+) (setf render-type +rl-triangles+))

               ;; If the slider or the wheel was clicked, update the current color
               (when (or setting-color slider-clicked)
                 (when setting-color (setf circle-position (get-mouse-position)))

                 (let ((distance (/ (vector2-distance center circle-position) point-scale))
                       (angle (/ (+ (/ (vector2-angle (vec2 0.0 (- point-scale)) (vector2-subtract center circle-position)) +pi+) 1.0) 2.0)))
                   (when (and setting-color (> distance 1.0))
                     (setf circle-position (vector2-add (vec2 (* (sin (* angle (* +pi+ 2.0))) point-scale) (* (- (cos (* angle (* +pi+ 2.0)))) point-scale)) center)))

                   (let ((angle360 (* angle 360.0))
                         (value-actual (clamp distance 0.0 1.0)))
                     (setf color (color-lerp (list (truncate (* value 255.0)) (truncate (* value 255.0)) (truncate (* value 255.0)) 255)
                                             (color-from-hsv angle360 (clamp distance 0.0 1.0) 1.0) value-actual)))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Begin rendering color wheel
               (rl-begin render-type)
               (dotimes (i triangle-count)
                 (let* ((angle-offset (/ (* +pi+ 2.0) (float triangle-count)))
                        (angle (* angle-offset (float i)))
                        (angle-offset-calculated (* (+ (float i) 1) angle-offset))

                        (scale (vec2 point-scale point-scale))

                        (offset (vector2-multiply (vec2 (sin angle) (- (cos angle))) scale))
                        (offset2 (vector2-multiply (vec2 (sin angle-offset-calculated) (- (cos angle-offset-calculated))) scale))

                        (position (vector2-add center offset))
                        (position2 (vector2-add center offset2))

                        (angle-non-radian (* (/ angle (* 2.0 +pi+)) 360.0))
                        (angle-non-radian-offset (* (/ angle-offset (* 2.0 +pi+)) 360.0))

                        (current-color (color-from-hsv angle-non-radian 1.0 1.0))
                        (offset-color (color-from-hsv (+ angle-non-radian angle-non-radian-offset) 1.0 1.0)))

                   ;; Input vertices differently depending on mode
                   (cond ((= render-type +rl-triangles+)
                          ;; RL_TRIANGLES expects three vertices per triangle
                          (apply #'rl-color4ub current-color)
                          (rl-vertex2f (vx position) (vy position))
                          (rl-color4f value value value 1.0)
                          (rl-vertex2f (vx center) (vy center))
                          (apply #'rl-color4ub offset-color)
                          (rl-vertex2f (vx position2) (vy position2)))
                         ((= render-type +rl-lines+)
                          ;; RL_LINES expects two vertices per line
                          (apply #'rl-color4ub current-color)
                          (rl-vertex2f (vx position) (vy position))
                          (apply #'rl-color4ub +white+)
                          (rl-vertex2f (vx center) (vy center))

                          (rl-vertex2f (vx center) (vy center))
                          (apply #'rl-color4ub offset-color)
                          (rl-vertex2f (vx position2) (vy position2))

                          (rl-vertex2f (vx position2) (vy position2))
                          (apply #'rl-color4ub current-color)
                          (rl-vertex2f (vx position) (vy position))))))
               (rl-end)

               ;; Make the handle slightly more visible overtop darker colors
               (let ((handle-color +black+))
                 (when (and (<= (/ (vector2-distance center circle-position) point-scale) 0.5) (<= value 0.5))
                   (setf handle-color +darkgray+))

                 ;; Draw the color handle
                 (draw-circle-lines-v circle-position 4.0 handle-color))

               ;; Draw the color in a preview, with a darkened outline.
               (draw-rectangle-v (vec2 8.0 8.0) (vec2 64.0 64.0) color)
               (draw-rectangle-lines-ex (make-rectangle :x 8.0 :y 8.0 :width 64.0 :height 64.0) 2.0 (color-lerp color +black+ 0.5))

               ;; Draw current color as hex and decimal
               (draw-text (text-format (format nil "#%02X%02X%02X~%(%d, %d, %d)") (first color) (second color) (third color) (first color) (second color) (third color)) 8 (+ 8 64 8) 20 +darkgray+)

               ;; Update the visuals for the copying text
               (let ((copy-color +darkgray+)
                     (offset 0))
                 (when (and (is-key-down +key-left-control+) (is-key-down +key-c+))
                   (setf copy-color +darkgreen+
                         offset 4))

                 ;; Draw the copying text
                 (draw-text "press ctrl+c to copy!" 8 (- 425 offset) 20 copy-color))

               ;; Display the number of rendered triangles
               (draw-text (text-format "triangle count: %d" triangle-count) 8 395 20 +darkgray+)

               ;; Slider to change color's value
               (setf value (nth-value 1 (gui-slider-bar slider-rectangle "value: " "" value 0.0 1.0)))

               ;; Draw FPS next to outlined color preview
               (draw-fps (+ 64 16) 8)

               (end-drawing)))
    ;;----------------------------------------------------------------------------------

    ;; De-Initialization
    ;;--------------------------------------------------------------------------------------
    (close-window)))                    ; Close window and OpenGL context

(main)
