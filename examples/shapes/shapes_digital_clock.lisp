;;;; raylib [shapes] example - digital clock
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Hamza RAHAL (@hmz-rhl) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Hamza RAHAL (@hmz-rhl) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_digital_clock.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-digital-clock
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-digital-clock)

(defconstant +clock-analog+ 0)
(defconstant +clock-digital+ 1)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Clock hand type
(defstruct clock-hand
  (value 0)                             ; Time value

  ;; Visual elements
  (angle 0.0)                           ; Hand angle
  (length 0)                            ; Hand length
  (thickness 0)                         ; Hand thickness
  (color +blank+))                      ; Hand color

;; Clock hands
(defstruct clock
  (second (make-clock-hand))            ; Clock hand for seconds
  (minute (make-clock-hand))            ; Clock hand for minutes
  (hour (make-clock-hand)))             ; Clock hand for hours

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Update clock time
(defun update-clock (clock)
  (multiple-value-bind (tm-sec tm-min tm-hour) (get-decoded-time)
    (let ((second (clock-second clock))
          (minute (clock-minute clock))
          (hour (clock-hour clock)))

      ;; Updating time data
      (setf (clock-hand-value second) tm-sec
            (clock-hand-value minute) tm-min
            (clock-hand-value hour) tm-hour)

      (setf (clock-hand-angle hour) (/ (* (mod tm-hour 12) 180.0) 6.0))
      (incf (clock-hand-angle hour) (/ (* (mod tm-min 60) 30) 60.0))
      (decf (clock-hand-angle hour) 90)

      (setf (clock-hand-angle minute) (* (mod tm-min 60) 6.0))
      (incf (clock-hand-angle minute) (/ (* (mod tm-sec 60) 6) 60.0))
      (decf (clock-hand-angle minute) 90)

      (setf (clock-hand-angle second) (* (mod tm-sec 60) 6.0))
      (decf (clock-hand-angle second) 90))))

;; Draw analog clock
;; Parameter: position, refers to center position
(defun draw-clock-analog (clock position)
  (let ((second (clock-second clock))
        (minute (clock-minute clock))
        (hour (clock-hour clock)))
    ;; Draw clock base
    (draw-circle-v position (+ (clock-hand-length second) 40.0) +lightgray+)
    (draw-circle-v position 12.0 +gray+)

    ;; Draw clock minutes/seconds lines
    (dotimes (i 60)
      (let ((rad (* (- (* 6.0 i) 90.0) +deg2rad+))
            (inner (+ (clock-hand-length second) (if (/= (mod i 5) 0) 10 6)))
            (outer (+ (clock-hand-length second) 20)))
        (draw-line-ex (vec2 (+ (vx position) (* inner (cos rad)))
                            (+ (vy position) (* inner (sin rad))))
                      (vec2 (+ (vx position) (* outer (cos rad)))
                            (+ (vy position) (* outer (sin rad))))
                      (if (/= (mod i 5) 0) 1.0 3.0) +darkgray+))

      ;; Draw seconds numbers
      ;;(draw-text (text-format "%02i" i) (- (+ (vx center-position) (* (+ (clock-hand-length second) 50) (cos (* (- (* 6.0 i) 90.0) +deg2rad+)))) (/ 10 2))
      ;;           (- (+ (vy center-position) (* (+ (clock-hand-length second) 50) (sin (* (- (* 6.0 i) 90.0) +deg2rad+)))) (/ 10 2)) 10 +gray+)
      )

    ;; Draw hand seconds
    (draw-rectangle-pro (make-rectangle :x (vx position) :y (vy position)
                                        :width (float (clock-hand-length second)) :height (float (clock-hand-thickness second)))
                        (vec2 0.0 (/ (clock-hand-thickness second) 2.0)) (clock-hand-angle second) (clock-hand-color second))

    ;; Draw hand minutes
    (draw-rectangle-pro (make-rectangle :x (vx position) :y (vy position)
                                        :width (float (clock-hand-length minute)) :height (float (clock-hand-thickness minute)))
                        (vec2 0.0 (/ (clock-hand-thickness minute) 2.0)) (clock-hand-angle minute) (clock-hand-color minute))

    ;; Draw hand hours
    (draw-rectangle-pro (make-rectangle :x (vx position) :y (vy position)
                                        :width (float (clock-hand-length hour)) :height (float (clock-hand-thickness hour)))
                        (vec2 0.0 (/ (clock-hand-thickness hour) 2.0)) (clock-hand-angle hour) (clock-hand-color hour))))

;; Draw one 7-segment display segment, horizontal or vertical
(defun draw-display-segment (center length thick vertical color)
  (let ((cx (vx center)) (cy (vy center)))
    (if (not vertical)
        ;; Horizontal segment points
        #|
             3___________________________5
            /                             \
           /1             x               6\
           \                               /
            \2___________________________4/
        |#
        (let ((segment-points-h
                (vector (vec2 (- cx (/ length 2.0) (/ thick 2.0)) cy)  ; Point 1
                        (vec2 (- cx (/ length 2.0)) (+ cy (/ thick 2.0))) ; Point 2
                        (vec2 (- cx (/ length 2.0)) (- cy (/ thick 2.0))) ; Point 3
                        (vec2 (+ cx (/ length 2.0)) (+ cy (/ thick 2.0))) ; Point 4
                        (vec2 (+ cx (/ length 2.0)) (- cy (/ thick 2.0))) ; Point 5
                        (vec2 (+ cx (/ length 2.0) (/ thick 2.0)) cy))))  ; Point 6

          (draw-triangle-strip segment-points-h 6 color))
        ;; Vertical segment points
        (let ((segment-points-v
                (vector (vec2 cx (- cy (/ length 2.0) (/ thick 2.0)))  ; Point 1
                        (vec2 (- cx (/ thick 2.0)) (- cy (/ length 2.0))) ; Point 2
                        (vec2 (+ cx (/ thick 2.0)) (- cy (/ length 2.0))) ; Point 3
                        (vec2 (- cx (/ thick 2.0)) (+ cy (/ length 2.0))) ; Point 4
                        (vec2 (+ cx (/ thick 2.0)) (+ cy (/ length 2.0))) ; Point 5
                        (vec2 cx (+ cy (/ (float length) 2) (/ thick 2.0)))))) ; Point 6

          (draw-triangle-strip segment-points-v 6 color)))))

;; Draw seven segments display
;; Parameter: position, refers to top-left corner of display
;; Parameter: segments, defines in binary the segments to be activated
(defun draw-7s-display (position segments color-on color-off)
  (let* ((segment-len 60)
         (segment-thick 20)
         (offset-y-adjust (* segment-thick 0.3)) ; HACK: Adjust gap space between segment limits
         (x (vx position)) (y (vy position)))
    (flet ((pick (bit) (if (logtest segments bit) color-on color-off)))
      ;; Segment A
      (draw-display-segment (vec2 (+ x segment-thick (/ segment-len 2.0)) (+ y segment-thick))
                            segment-len segment-thick nil (pick #b00000001))
      ;; Segment B
      (draw-display-segment (vec2 (+ x segment-thick segment-len (/ segment-thick 2.0)) (- (+ y (* 2 segment-thick) (/ segment-len 2.0)) offset-y-adjust))
                            segment-len segment-thick t (pick #b00000010))
      ;; Segment C
      (draw-display-segment (vec2 (+ x segment-thick segment-len (/ segment-thick 2.0)) (- (+ y (* 4 segment-thick) segment-len (/ segment-len 2.0)) (* 3 offset-y-adjust)))
                            segment-len segment-thick t (pick #b00000100))
      ;; Segment D
      (draw-display-segment (vec2 (+ x segment-thick (/ segment-len 2.0)) (- (+ y (* 5 segment-thick) (* 2 segment-len)) (* 4 offset-y-adjust)))
                            segment-len segment-thick nil (pick #b00001000))
      ;; Segment E
      (draw-display-segment (vec2 (+ x (/ segment-thick 2.0)) (- (+ y (* 4 segment-thick) segment-len (/ segment-len 2.0)) (* 3 offset-y-adjust)))
                            segment-len segment-thick t (pick #b00010000))
      ;; Segment F
      (draw-display-segment (vec2 (+ x (/ segment-thick 2.0)) (- (+ y (* 2 segment-thick) (/ segment-len 2.0)) offset-y-adjust))
                            segment-len segment-thick t (pick #b00100000))
      ;; Segment G
      (draw-display-segment (vec2 (+ x segment-thick (/ segment-len 2.0)) (- (+ y (* 3 segment-thick) segment-len) (* 2 offset-y-adjust)))
                            segment-len segment-thick nil (pick #b01000000)))))

;; Draw 7-segment display with value
(defun draw-display-value (position value color-on color-off)
  (case value
    (0 (draw-7s-display position #b00111111 color-on color-off))
    (1 (draw-7s-display position #b00000110 color-on color-off))
    (2 (draw-7s-display position #b01011011 color-on color-off))
    (3 (draw-7s-display position #b01001111 color-on color-off))
    (4 (draw-7s-display position #b01100110 color-on color-off))
    (5 (draw-7s-display position #b01101101 color-on color-off))
    (6 (draw-7s-display position #b01111101 color-on color-off))
    (7 (draw-7s-display position #b00000111 color-on color-off))
    (8 (draw-7s-display position #b01111111 color-on color-off))
    (9 (draw-7s-display position #b01101111 color-on color-off))))

;; Draw digital clock
;; PARAM: position, refers to top-left corner
(defun draw-clock-digital (clock position)
  (let ((x (vx position)) (y (vy position))
        (hour (clock-hand-value (clock-hour clock)))
        (minute (clock-hand-value (clock-minute clock)))
        (second (clock-hand-value (clock-second clock))))
    ;; Draw clock using custom 7-segments display (made of shapes)
    (draw-display-value (vec2 x y) (truncate hour 10) +red+ (fade +lightgray+ 0.3))
    (draw-display-value (vec2 (+ x 120) y) (rem hour 10) +red+ (fade +lightgray+ 0.3))

    (draw-circle (+ (truncate x) 240) (+ (truncate y) 70) 12.0 (if (oddp second) +red+ (fade +lightgray+ 0.3)))
    (draw-circle (+ (truncate x) 240) (+ (truncate y) 150) 12.0 (if (oddp second) +red+ (fade +lightgray+ 0.3)))

    (draw-display-value (vec2 (+ x 260) y) (truncate minute 10) +red+ (fade +lightgray+ 0.3))
    (draw-display-value (vec2 (+ x 380) y) (rem minute 10) +red+ (fade +lightgray+ 0.3))

    (draw-circle (+ (truncate x) 500) (+ (truncate y) 70) 12.0 (if (oddp second) +red+ (fade +lightgray+ 0.3)))
    (draw-circle (+ (truncate x) 500) (+ (truncate y) 150) 12.0 (if (oddp second) +red+ (fade +lightgray+ 0.3)))

    (draw-display-value (vec2 (+ x 520) y) (truncate second 10) +red+ (fade +lightgray+ 0.3))
    (draw-display-value (vec2 (+ x 640) y) (rem second 10) +red+ (fade +lightgray+ 0.3))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - digital clock")

    (let ((clock-mode +clock-digital+)
          ;; Initialize clock
          ;; NOTE: Includes visual info for anlaog clock
          (clock (make-clock
                  :second (make-clock-hand :angle 45.0 :length 140 :thickness 3 :color +maroon+)
                  :minute (make-clock-hand :angle 10.0 :length 130 :thickness 7 :color +darkgray+)
                  :hour (make-clock-hand :angle 0.0 :length 100 :thickness 7 :color +black+))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (when (is-key-pressed +key-space+)
                 ;; Toggle clock mode
                 (cond ((= clock-mode +clock-digital+) (setf clock-mode +clock-analog+))
                       ((= clock-mode +clock-analog+) (setf clock-mode +clock-digital+))))

               (update-clock clock)     ; Update clock required data: value and angle
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Draw clock in selected mode
               (cond ((= clock-mode +clock-analog+) (draw-clock-analog clock (vec2 400.0 240.0)))
                     ((= clock-mode +clock-digital+)
                      (draw-clock-digital clock (vec2 30.0 60.0))

                      ;; Draw clock using default raylib font
                      (let ((clock-time (text-format "%02i:%02i:%02i" (clock-hand-value (clock-hour clock))
                                                     (clock-hand-value (clock-minute clock)) (clock-hand-value (clock-second clock)))))
                        (draw-text clock-time (- (truncate (get-screen-width) 2) (truncate (measure-text clock-time 150) 2)) 300 150 +black+))))

               (draw-text (text-format "Press [SPACE] to switch clock mode: %s"
                                       (if (= clock-mode +clock-digital+) "DIGITAL CLOCK" "ANALOGUE CLOCK")) 10 10 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
