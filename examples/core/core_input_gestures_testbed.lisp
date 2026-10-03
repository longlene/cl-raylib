;;;; raylib [core] example - input gestures testbed
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 5.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by ubkp (@ubkp) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Copyright (c) 2023-2025 ubkp (@ubkp)
;;;; Common Lisp port of raylib/examples/core/core_input_gestures_testbed.c

(require :cl-raylib)

(defpackage #:raylib-examples/core-input-gestures-testbed
  (:use #:cl #:raylib))
(in-package #:raylib-examples/core-input-gestures-testbed)

(defconstant +gesture-log-size+ 20)
(defconstant +max-touch-count+ 32)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Get text string for gesture value
(defun get-gesture-name (gesture)
  (case gesture
    (0 "None")
    (1 "Tap")
    (2 "Double Tap")
    (4 "Hold")
    (8 "Drag")
    (16 "Swipe Right")
    (32 "Swipe Left")
    (64 "Swipe Up")
    (128 "Swipe Down")
    (256 "Pinch In")
    (512 "Pinch Out")
    (t "Unknown")))

;; Get color for gesture value
(defun get-gesture-color (gesture)
  (case gesture
    (0 +black+)
    (1 +blue+)
    (2 +skyblue+)
    (4 +black+)
    (8 +lime+)
    ((16 32 64 128) +red+)
    (256 +violet+)
    (512 +orange+)
    (t +black+)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [core] example - input gestures testbed")

    (let* ((message-position (vec2 160.0 7.0))
           ;; Last gesture variables definitions
           (last-gesture 0)
           (last-gesture-position (vec2 165.0 130.0))
           ;; Gesture log variables definitions
           ;; NOTE: The gesture log uses an array (as an inverted circular queue) to store the performed gestures
           (gesture-log (make-array +gesture-log-size+ :initial-element ""))
           ;; NOTE: The index for the inverted circular queue (moving from last to first direction, then looping around)
           (gesture-log-index +gesture-log-size+)
           (previous-gesture 0)
           ;; Log mode values:
           ;; - 0 shows repeated events
           ;; - 1 hides repeated events
           ;; - 2 shows repeated events but hide hold events
           ;; - 3 hides repeated events and hide hold events
           (log-mode 1)
           (gesture-color (list 0 0 0 255))
           (log-button1 (make-rectangle :x 53.0 :y 7.0 :width 48.0 :height 26.0))
           (log-button2 (make-rectangle :x 108.0 :y 7.0 :width 36.0 :height 26.0))
           (gesture-log-position (vec2 10.0 10.0))
           ;; Protractor variables definitions
           (angle-length 90.0)
           (current-angle-degrees 0.0)
           (final-vector (vec2 0.0 0.0))
           (protractor-position (vec2 266.0 315.0)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Handle common gestures data
               (let* ((current-gesture (get-gesture-detected))
                      (current-drag-degrees (get-gesture-drag-angle))
                      (current-pitch-degrees (get-gesture-pinch-angle))
                      (touch-count (get-touch-point-count))
                      (fill-log nil)            ; Gate variable to be used to allow or not the gesture log to be filled
                      (touch-position (make-array +max-touch-count+ :initial-element (vec2 0.0 0.0)))
                      (mouse-position (vec2 0.0 0.0)))

                 ;; Handle last gesture
                 (when (and (/= current-gesture 0) (/= current-gesture 4) (/= current-gesture previous-gesture))
                   (setf last-gesture current-gesture)) ; Filter the meaningful gestures (1, 2, 8 to 512) for the display

                 ;; Handle gesture log
                 (when (is-mouse-button-released +mouse-button-left+)
                   (cond ((check-collision-point-rec (get-mouse-position) log-button1)
                          (setf log-mode (case log-mode (3 2) (2 3) (1 0) (t 1))))
                         ((check-collision-point-rec (get-mouse-position) log-button2)
                          (setf log-mode (case log-mode (3 1) (2 0) (1 3) (t 2))))))

                 (when (/= current-gesture 0)
                   (case log-mode
                     (3 ; 3 hides repeated events and hide hold events
                      (when (or (and (/= current-gesture 4) (/= current-gesture previous-gesture)) (< current-gesture 3))
                        (setf fill-log t)))
                     (2 ; 2 shows repeated events but hide hold events
                      (when (/= current-gesture 4) (setf fill-log t)))
                     (1 ; 1 hides repeated events
                      (when (/= current-gesture previous-gesture) (setf fill-log t)))
                     (t ; 0 shows repeated events
                      (setf fill-log t))))

                 (when fill-log ; If one of the conditions from logMode was met, fill the gesture log
                   (setf previous-gesture current-gesture
                         gesture-color (get-gesture-color current-gesture))
                   (when (<= gesture-log-index 0) (setf gesture-log-index +gesture-log-size+))
                   (decf gesture-log-index)

                   ;; Copy the gesture respective name to the gesture log array
                   (setf (aref gesture-log gesture-log-index) (get-gesture-name current-gesture)))

                 ;; Handle protractor
                 (cond ((> current-gesture 255) (setf current-angle-degrees current-pitch-degrees)) ; Pinch In and Pinch Out
                       ((> current-gesture 15) (setf current-angle-degrees current-drag-degrees))   ; Swipe Right, Swipe Left, Swipe Up and Swipe Down
                       ((> current-gesture 0) (setf current-angle-degrees 0.0)))                    ; Tap, Doubletap, Hold and Grab

                 (let ((current-angle-radians (/ (* (+ current-angle-degrees 90.0) +pi+) 180))) ; Convert the current angle to Radians
                   ;; Calculate the final vector for display
                   (setf final-vector (vec2 (+ (* angle-length (sin current-angle-radians)) (vx protractor-position))
                                            (+ (* angle-length (cos current-angle-radians)) (vy protractor-position)))))

                 ;; Handle touch and mouse pointer points
                 (when (/= current-gesture +gesture-none+)
                   (if (/= touch-count 0)
                       (dotimes (i touch-count) (setf (aref touch-position i) (get-touch-position i))) ; Fill the touch positions
                       (setf mouse-position (get-mouse-position))))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw common elements
                 (let ((mx (truncate (vx message-position))) (my (truncate (vy message-position))))
                   (draw-text "*" (+ mx 5) (+ my 5) 10 +black+)
                   (draw-text (format nil "Example optimized for Web/HTML5~%on Smartphones with Touch Screen.") (+ mx 15) (+ my 5) 10 +black+)
                   (draw-text "*" (+ mx 5) (+ my 35) 10 +black+)
                   (draw-text (format nil "While running on Desktop Web Browsers,~%inspect and turn on Touch Emulation.") (+ mx 15) (+ my 35) 10 +black+))

                 ;; Draw last gesture
                 (let* ((lx (vx last-gesture-position)) (ly (vy last-gesture-position))
                        (ilx (truncate lx)) (ily (truncate ly)))
                   (draw-text "Last gesture" (+ ilx 33) (- ily 47) 20 +black+)
                   (draw-text "Swipe         Tap       Pinch  Touch" (+ ilx 17) (- ily 18) 10 +black+)
                   (draw-rectangle (+ ilx 20) ily 20 20 (if (= last-gesture +gesture-swipe-up+) +red+ +lightgray+))
                   (draw-rectangle ilx (+ ily 20) 20 20 (if (= last-gesture +gesture-swipe-left+) +red+ +lightgray+))
                   (draw-rectangle (+ ilx 40) (+ ily 20) 20 20 (if (= last-gesture +gesture-swipe-right+) +red+ +lightgray+))
                   (draw-rectangle (+ ilx 20) (+ ily 40) 20 20 (if (= last-gesture +gesture-swipe-down+) +red+ +lightgray+))
                   (draw-circle (+ ilx 80) (+ ily 16) 10.0 (if (= last-gesture +gesture-tap+) +blue+ +lightgray+))
                   (draw-ring (vec2 (+ lx 103) (+ ly 16)) 6.0 11.0 0.0 360.0 0 (if (= last-gesture +gesture-drag+) +lime+ +lightgray+))
                   (draw-circle (+ ilx 80) (+ ily 43) 10.0 (if (= last-gesture +gesture-doubletap+) +skyblue+ +lightgray+))
                   (draw-circle (+ ilx 103) (+ ily 43) 10.0 (if (= last-gesture +gesture-doubletap+) +skyblue+ +lightgray+))
                   (draw-triangle (vec2 (+ lx 122) (+ ly 16)) (vec2 (+ lx 137) (+ ly 26)) (vec2 (+ lx 137) (+ ly 6))
                                  (if (= last-gesture +gesture-pinch-out+) +orange+ +lightgray+))
                   (draw-triangle (vec2 (+ lx 147) (+ ly 6)) (vec2 (+ lx 147) (+ ly 26)) (vec2 (+ lx 162) (+ ly 16))
                                  (if (= last-gesture +gesture-pinch-out+) +orange+ +lightgray+))
                   (draw-triangle (vec2 (+ lx 125) (+ ly 33)) (vec2 (+ lx 125) (+ ly 53)) (vec2 (+ lx 140) (+ ly 43))
                                  (if (= last-gesture +gesture-pinch-in+) +violet+ +lightgray+))
                   (draw-triangle (vec2 (+ lx 144) (+ ly 43)) (vec2 (+ lx 159) (+ ly 53)) (vec2 (+ lx 159) (+ ly 33))
                                  (if (= last-gesture +gesture-pinch-in+) +violet+ +lightgray+))
                   (dotimes (i 4)
                     (draw-circle (+ ilx 180) (+ ily 7 (* i 15)) 5.0 (if (<= touch-count i) +lightgray+ gesture-color))))

                 ;; Draw gesture log
                 (draw-text "Log" (truncate (vx gesture-log-position)) (truncate (vy gesture-log-position)) 20 +black+)

                 ;; Loop in both directions to print the gesture log array in the inverted order (and looping around if the index started somewhere in the middle)
                 (loop for i from 0 below +gesture-log-size+
                       for ii = gesture-log-index then (mod (1+ ii) +gesture-log-size+)
                       ;; NOTE: The first index is GESTURE_LOG_SIZE until a gesture is logged (C reads past the array)
                       do (draw-text (if (< ii +gesture-log-size+) (aref gesture-log ii) "") (truncate (vx gesture-log-position))
                                     (- (+ (truncate (vy gesture-log-position)) 410) (* i 20)) 20
                                     (if (= i 0) gesture-color +lightgray+)))

                 (multiple-value-bind (log-button1-color log-button2-color)
                     (case log-mode
                       (3 (values +maroon+ +maroon+))
                       (2 (values +gray+ +maroon+))
                       (1 (values +maroon+ +gray+))
                       (t (values +gray+ +gray+)))
                   (draw-rectangle-rec log-button1 log-button1-color)
                   (draw-text "Hide" (+ (truncate (rectangle-x log-button1)) 7) (+ (truncate (rectangle-y log-button1)) 3) 10 +white+)
                   (draw-text "Repeat" (+ (truncate (rectangle-x log-button1)) 7) (+ (truncate (rectangle-y log-button1)) 13) 10 +white+)
                   (draw-rectangle-rec log-button2 log-button2-color)
                   (draw-text "Hide" (+ (truncate (rectangle-x log-button1)) 62) (+ (truncate (rectangle-y log-button1)) 3) 10 +white+)
                   (draw-text "Hold" (+ (truncate (rectangle-x log-button1)) 62) (+ (truncate (rectangle-y log-button1)) 13) 10 +white+))

                 ;; Draw protractor
                 (let* ((px (vx protractor-position)) (py (vy protractor-position))
                        (ipx (truncate px)) (ipy (truncate py)))
                   (draw-text "Angle" (+ ipx 55) (+ ipy 76) 10 +black+)
                   (let* ((angle-string (text-format "%f" current-angle-degrees))
                          (angle-string-dot (text-find-index angle-string "."))
                          (angle-string-trim (text-subtext angle-string 0 (+ angle-string-dot 3))))
                     (draw-text angle-string-trim (+ ipx 55) (+ ipy 92) 20 gesture-color))
                   (draw-circle-v protractor-position 80.0 +white+)
                   (draw-line-ex (vec2 (- px 90) py) (vec2 (+ px 90) py) 3.0 +lightgray+)
                   (draw-line-ex (vec2 px (- py 90)) (vec2 px (+ py 90)) 3.0 +lightgray+)
                   (draw-line-ex (vec2 (- px 80) (- py 45)) (vec2 (+ px 80) (+ py 45)) 3.0 +green+)
                   (draw-line-ex (vec2 (- px 80) (+ py 45)) (vec2 (+ px 80) (- py 45)) 3.0 +green+)
                   (draw-text "0" (+ ipx 96) (- ipy 9) 20 +black+)
                   (draw-text "30" (+ ipx 74) (- ipy 68) 20 +black+)
                   (draw-text "90" (- ipx 11) (- ipy 110) 20 +black+)
                   (draw-text "150" (- ipx 100) (- ipy 68) 20 +black+)
                   (draw-text "180" (- ipx 124) (- ipy 9) 20 +black+)
                   (draw-text "210" (- ipx 100) (+ ipy 50) 20 +black+)
                   (draw-text "270" (- ipx 18) (+ ipy 92) 20 +black+)
                   (draw-text "330" (+ ipx 72) (+ ipy 50) 20 +black+)
                   (when (/= current-angle-degrees 0.0) (draw-line-ex protractor-position final-vector 3.0 gesture-color)))

                 ;; Draw touch and mouse pointer points
                 (when (/= current-gesture +gesture-none+)
                   (if (/= touch-count 0)
                       (progn
                         (dotimes (i touch-count)
                           (draw-circle-v (aref touch-position i) 50.0 (fade gesture-color 0.5))
                           (draw-circle-v (aref touch-position i) 5.0 gesture-color))

                         (when (= touch-count 2)
                           (draw-line-ex (aref touch-position 0) (aref touch-position 1) (if (= current-gesture 512) 8.0 12.0) gesture-color)))
                       (progn
                         (draw-circle-v mouse-position 35.0 (fade gesture-color 0.5))
                         (draw-circle-v mouse-position 5.0 gesture-color))))

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
