;;;; raylib [shapes] example - pie chart
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Gideon Serfontein (@GideonSerf) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Gideon Serfontein (@GideonSerf)
;;;; Common Lisp port of raylib/examples/shapes/shapes_pie_chart.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-pie-chart
  (:use #:cl #:raylib #:raygui))
(in-package #:raylib-examples/shapes-pie-chart)

(defconstant +max-pie-slices+ 10)       ; Max pie slices

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - pie chart")

    (let* ((slice-count 7)
           (donut-inner-radius 25.0)
           (values (make-array +max-pie-slices+ :element-type 'single-float :initial-element 0.0)) ; Initial slice values
           (labels (make-array +max-pie-slices+))
           (editing-label (make-array +max-pie-slices+ :initial-element nil))

           (show-values t)
           (show-percentages nil)
           (show-donut nil)
           (hovered-slice -1)
           (scroll-panel-bounds (make-rectangle))
           (scroll-content-offset (vec2 0.0 0.0))
           (view (make-rectangle))

           ;; UI layout parameters
           (panel-width 270)
           (panel-margin 5)

           ;; UI Panel top-left anchor
           (panel-pos (vec2 (float (- screen-width panel-margin panel-width))
                            (float panel-margin)))

           ;; UI Panel rectangle
           (panel-rect (make-rectangle :x (vx panel-pos) :y (vy panel-pos)
                                       :width (float panel-width)
                                       :height (- (float screen-height) (* 2.0 panel-margin))))

           ;; Pie chart geometry
           (canvas (make-rectangle :x 0.0 :y 0.0 :width (vx panel-pos) :height (float screen-height)))
           (center (vec2 (/ (rectangle-width canvas) 2.0) (/ (rectangle-height canvas) 2.0)))
           (radius 205.0)

           ;; Total value for percentage calculations
           (total-value 0.0))

      (replace values '(300.0 100.0 450.0 350.0 600.0 380.0 750.0))
      (dotimes (i +max-pie-slices+)
        (setf (aref labels i) (text-format "Slice %02i" (1+ i))))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Calculate total value for percentage calculations
               (setf total-value 0.0)
               (dotimes (i slice-count) (incf total-value (aref values i)))

               ;; Check for mouse hover over slices
               (setf hovered-slice -1)  ; Reset hovered slice
               (let ((mouse-pos (get-mouse-position)))
                 (when (check-collision-point-rec mouse-pos canvas) ; Only check if mouse is inside the canvas
                   (let* ((dx (- (vx mouse-pos) (vx center)))
                          (dy (- (vy mouse-pos) (vy center)))
                          (distance (sqrt (+ (* dx dx) (* dy dy)))))

                     (when (<= distance radius) ; Inside the pie radius
                       (let ((angle (* (atan dy dx) +rad2deg+))
                             (current-angle 0.0))
                         (when (< angle 0) (incf angle 360))

                         (dotimes (i slice-count)
                           (let ((sweep (if (> total-value 0) (* (/ (aref values i) total-value) 360.0) 0.0)))

                             (when (and (>= angle current-angle) (< angle (+ current-angle sweep)))
                               (setf hovered-slice i)
                               (return))

                             (incf current-angle sweep))))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)
               (clear-background +raywhite+)

               ;; Draw the pie chart on the canvas
               (let ((start-angle 0.0))
                 (dotimes (i slice-count)
                   (let* ((sweep-angle (if (> total-value 0) (* (/ (aref values i) total-value) 360.0) 0.0))
                          (mid-angle (+ start-angle (/ sweep-angle 2.0))) ; Middle angle for label positioning

                          (color (color-from-hsv (* (/ (float i) slice-count) 360.0) 0.75 0.9))
                          (current-radius radius))

                     ;; Make the hovered slice pop out by adding 5 pixels to its radius
                     (when (= i hovered-slice) (incf current-radius 20.0))

                     ;; Draw the pie slice using raylib's DrawCircleSector function
                     (draw-circle-sector center current-radius start-angle (+ start-angle sweep-angle) 120 color)

                     ;; Draw the label for the current slice
                     (when (> (aref values i) 0)
                       (let* ((label-text (cond ((and show-values show-percentages)
                                                 (text-format "%.1f (%.0f%%)" (aref values i) (* (/ (aref values i) total-value) 100.0)))
                                                (show-values (text-format "%.1f" (aref values i)))
                                                (show-percentages (text-format "%.0f%%" (* (/ (aref values i) total-value) 100.0)))
                                                (t "")))
                              (text-size (measure-text-ex (get-font-default) label-text 20 1))
                              (label-radius (* radius 0.7))
                              (label-pos (vec2 (- (+ (vx center) (* (cos (* mid-angle +deg2rad+)) label-radius)) (/ (vx text-size) 2.0))
                                               (- (+ (vy center) (* (sin (* mid-angle +deg2rad+)) label-radius)) (/ (vy text-size) 2.0)))))
                         (draw-text label-text (truncate (vx label-pos)) (truncate (vy label-pos)) 20 +white+)))

                     ;; Draw inner circle to create donut effect
                     ;; TODO: This is a hacky solution, better use DrawRing()
                     (when show-donut (draw-circle-v center donut-inner-radius +raywhite+))

                     (incf start-angle sweep-angle))))

               ;; UI control panel
               (draw-rectangle-rec panel-rect (fade +lightgray+ 0.5))
               (draw-rectangle-lines-ex panel-rect 1.0 +gray+)

               (setf slice-count (nth-value 1 (gui-spinner (make-rectangle :x (+ (vx panel-pos) 95) :y (+ (vy panel-pos) 12) :width 125.0 :height 25.0) "Slices " slice-count 1 +max-pie-slices+ nil)))
               (setf show-values (nth-value 1 (gui-check-box (make-rectangle :x (+ (vx panel-pos) 20) :y (+ (vy panel-pos) 12 40) :width 20.0 :height 20.0) "Show Values" show-values)))
               (setf show-percentages (nth-value 1 (gui-check-box (make-rectangle :x (+ (vx panel-pos) 20) :y (+ (vy panel-pos) 12 70) :width 20.0 :height 20.0) "Show Percentages" show-percentages)))
               (setf show-donut (nth-value 1 (gui-check-box (make-rectangle :x (+ (vx panel-pos) 20) :y (+ (vy panel-pos) 12 100) :width 20.0 :height 20.0) "Make Donut" show-donut)))

               (when show-donut (gui-disable))
               (setf donut-inner-radius (nth-value 1 (gui-slider-bar (make-rectangle :x (+ (vx panel-pos) 80) :y (+ (vy panel-pos) 12 130) :width (- (rectangle-width panel-rect) 100) :height 30.0)
                                                                     "Inner Radius" nil donut-inner-radius 5.0 (- radius 10.0))))
               (gui-enable)

               (gui-line (make-rectangle :x (+ (vx panel-pos) 10) :y (+ (vy panel-pos) 12 170) :width (- (rectangle-width panel-rect) 20) :height 1.0) nil)

               ;; Scrollable area for slice editors
               (setf scroll-panel-bounds (make-rectangle :x (+ (vx panel-pos) panel-margin)
                                                         :y (+ (vy panel-pos) 12 190)
                                                         :width (- (rectangle-width panel-rect) (* panel-margin 2))
                                                         :height (- (+ (- (+ (rectangle-y panel-rect) (rectangle-height panel-rect)) (vy panel-pos)) 12 190) panel-margin)))
               (let ((content-height (* slice-count 35)))

                 (multiple-value-bind (result scroll new-view)
                     (gui-scroll-panel scroll-panel-bounds nil
                                       (make-rectangle :x 0.0 :y 0.0 :width (- (rectangle-width panel-rect) 25) :height (float content-height))
                                       scroll-content-offset view)
                   (declare (ignore result))
                   (setf scroll-content-offset scroll
                         view new-view)))

               (let ((content-x (+ (rectangle-x view) (vx scroll-content-offset))) ; Left of content
                     (content-y (+ (rectangle-y view) (vy scroll-content-offset)))) ; Top of content

                 (begin-scissor-mode (truncate (rectangle-x view)) (truncate (rectangle-y view)) (truncate (rectangle-width view)) (truncate (rectangle-height view)))

                 (dotimes (i slice-count)
                   (let ((row-y (truncate (+ content-y 5 (* i 35))))
                         ;; Color indicator
                         (color (color-from-hsv (* (/ (float i) slice-count) 360.0) 0.75 0.9)))
                     (draw-rectangle (truncate (+ content-x 15)) (+ row-y 5) 20 20 color)

                     ;; Label textbox
                     (multiple-value-bind (result text)
                         (gui-text-box (make-rectangle :x (+ content-x 45) :y (float row-y) :width 75.0 :height 30.0) (aref labels i) 32 (aref editing-label i))
                       (setf (aref labels i) text)
                       (when (/= result 0) (setf (aref editing-label i) (not (aref editing-label i)))))

                     (setf (aref values i) (nth-value 1 (gui-slider-bar (make-rectangle :x (+ content-x 130) :y (float row-y) :width 110.0 :height 30.0) nil nil (aref values i) 0.0 1000.0)))))

                 (end-scissor-mode))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
