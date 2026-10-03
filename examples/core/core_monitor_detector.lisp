;;;; raylib [core] example - monitor detector
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Maicon Santana (@maiconpintoabreu) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2025 Maicon Santana (@maiconpintoabreu)
;;;; Common Lisp port of raylib/examples/core/core_monitor_detector.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-monitor-detector
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-monitor-detector)

(defconstant +max-monitors+ 10)

;; Monitor info
(defstruct monitor-info
  (position (vec2 0.0 0.0))
  (name "")
  (width 0)
  (height 0)
  (physical-width 0)
  (physical-height 0)
  (refresh-rate 0))

(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - monitor detector")

    (let ((monitors (make-array +max-monitors+ :initial-element (make-monitor-info)))
          (current-monitor-index (get-current-monitor))
          (monitor-count 0))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let (;; Variables to find the max x and Y to calculate the scale
                     (max-width 1)
                     (max-height 1)
                     ;; Monitor offset is to fix when monitor position x is negative
                     (monitor-offset-x 0))

                 ;; Rebuild monitors array every frame
                 (setf monitor-count (get-monitor-count))
                 (dotimes (i monitor-count)
                   (setf (aref monitors i) (make-monitor-info :position (get-monitor-position i)
                                                              :name (get-monitor-name i)
                                                              :width (get-monitor-width i)
                                                              :height (get-monitor-height i)
                                                              :physical-width (get-monitor-physical-width i)
                                                              :physical-height (get-monitor-physical-height i)
                                                              :refresh-rate (get-monitor-refresh-rate i)))
                   (let ((m (aref monitors i)))
                     (when (< (vx (monitor-info-position m)) monitor-offset-x)
                       (setf monitor-offset-x (- (truncate (vx (monitor-info-position m))))))

                     (let ((width (+ (truncate (vx (monitor-info-position m))) (monitor-info-width m)))
                           (height (+ (truncate (vy (monitor-info-position m))) (monitor-info-height m))))
                       (when (< max-width width) (setf max-width width))
                       (when (< max-height height) (setf max-height height)))))

                 (if (and (is-key-pressed +key-enter+) (> monitor-count 1))
                     (progn
                       (incf current-monitor-index 1)

                       ;; Set index to 0 if the last one
                       (when (= current-monitor-index monitor-count) (setf current-monitor-index 0))

                       (set-window-monitor current-monitor-index)) ; Move window to currentMonitorIndex
                     (setf current-monitor-index (get-current-monitor))) ; Get currentMonitorIndex if manually moved

                 (let ((monitor-scale 0.6))
                   (if (> max-height (+ max-width monitor-offset-x))
                       (setf monitor-scale (* monitor-scale (/ (float screen-height) (float max-height))))
                       (setf monitor-scale (* monitor-scale (/ (float screen-width) (float (+ max-width monitor-offset-x))))))
                   ;;----------------------------------------------------------------------------------

                   ;; Draw
                   ;;----------------------------------------------------------------------------------
                   (begin-drawing)

                   (clear-background +raywhite+)

                   (draw-text "Press [Enter] to move window to next monitor available" 20 20 20 +darkgray+)

                   (draw-rectangle-lines 20 60 (- screen-width 40) (- screen-height 100) +darkgray+)

                   ;; Draw Monitor Rectangles with information inside
                   (dotimes (i monitor-count)
                     (let* ((m (aref monitors i))
                            ;; Calculate retangle position and size using monitorScale
                            (rec (make-rectangle :x (+ (* (+ (vx (monitor-info-position m)) monitor-offset-x) monitor-scale) 140)
                                                 :y (+ (* (vy (monitor-info-position m)) monitor-scale) 80)
                                                 :width (* (monitor-info-width m) monitor-scale)
                                                 :height (* (monitor-info-height m) monitor-scale))))

                       ;; Draw monitor name and information inside the rectangle
                       (draw-text (text-format "[%i] %s" i (monitor-info-name m))
                                  (+ (truncate (rectangle-x rec)) 10) (+ (truncate (rectangle-y rec)) (truncate (* 100 monitor-scale)))
                                  (truncate (* 120 monitor-scale)) +blue+)
                       (draw-text (text-format (format nil "Resolution: [%ipx x %ipx]~%RefreshRate: [%ihz]~%Physical Size: [%imm x %imm]~%Position: %3.0f x %3.0f")
                                               (monitor-info-width m)
                                               (monitor-info-height m)
                                               (monitor-info-refresh-rate m)
                                               (monitor-info-physical-width m)
                                               (monitor-info-physical-height m)
                                               (vx (monitor-info-position m))
                                               (vy (monitor-info-position m)))
                                  (+ (truncate (rectangle-x rec)) 10) (+ (truncate (rectangle-y rec)) (truncate (* 200 monitor-scale)))
                                  (truncate (* 120 monitor-scale)) +darkgray+)

                       ;; Highlight current monitor
                       (if (= i current-monitor-index)
                           (progn
                             (draw-rectangle-lines-ex rec 5.0 +red+)
                             (let ((window-position (vec2 (+ (* (+ (vx (get-window-position)) monitor-offset-x) monitor-scale) 140)
                                                          (+ (* (vy (get-window-position)) monitor-scale) 80))))
                               ;; Draw window position based on monitors
                               (draw-rectangle-v window-position (vec2 (* screen-width monitor-scale) (* screen-height monitor-scale))
                                                 (fade +green+ 0.5))))
                           (draw-rectangle-lines-ex rec 5.0 +gray+))))

                   (end-drawing))))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
