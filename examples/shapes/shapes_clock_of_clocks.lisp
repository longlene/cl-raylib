;;;; raylib [shapes] example - clock of clocks
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by JP Mortiboys (@themushroompirates) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 JP Mortiboys (@themushroompirates)
;;;; Common Lisp port of raylib/examples/shapes/shapes_clock_of_clocks.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-clock-of-clocks
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-clock-of-clocks)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - clock of clocks")

    (let* ((bg-color (color-lerp +darkblue+ +black+ 0.75))
           (hands-color (color-lerp +yellow+ +raywhite+ .25))

           (clock-face-size 24.0)
           (clock-face-spacing 8.0)
           (section-spacing 16.0)

           (tl (vec2 0.0 90.0))         ; Top-left corner
           (tr (vec2 90.0 180.0))       ; Top-right corner
           (br (vec2 180.0 270.0))      ; Bottom-right corner
           (bl (vec2 0.0 270.0))        ; Bottom-left corner
           (hh (vec2 0.0 180.0))        ; Horizontal line
           (vv (vec2 90.0 270.0))       ; Vertical line
           (zz (vec2 135.0 135.0))      ; Not relevant

           (digit-angles
             (vector
              #|0|# (vector tl hh hh tr #||# vv tl tr vv #||# vv vv vv vv #||# vv vv vv vv #||# vv bl br vv #||# bl hh hh br)
              #|1|# (vector tl hh tr zz #||# bl tr vv zz #||# zz vv vv zz #||# zz vv vv zz #||# tl br bl tr #||# bl hh hh br)
              #|2|# (vector tl hh hh tr #||# bl hh tr vv #||# tl hh br vv #||# vv tl hh br #||# vv bl hh tr #||# bl hh hh br)
              #|3|# (vector tl hh hh tr #||# bl hh tr vv #||# tl hh br vv #||# bl hh tr vv #||# tl hh br vv #||# bl hh hh br)
              #|4|# (vector tl tr tl tr #||# vv vv vv vv #||# vv bl br vv #||# bl hh tr vv #||# zz zz vv vv #||# zz zz bl br)
              #|5|# (vector tl hh hh tr #||# vv tl hh br #||# vv bl hh tr #||# bl hh tr vv #||# tl hh br vv #||# bl hh hh br)
              #|6|# (vector tl hh hh tr #||# vv tl hh br #||# vv bl hh tr #||# vv tl tr vv #||# vv bl br vv #||# bl hh hh br)
              #|7|# (vector tl hh hh tr #||# bl hh tr vv #||# zz zz vv vv #||# zz zz vv vv #||# zz zz vv vv #||# zz zz bl br)
              #|8|# (vector tl hh hh tr #||# vv tl tr vv #||# vv bl br vv #||# vv tl tr vv #||# vv bl br vv #||# bl hh hh br)
              #|9|# (vector tl hh hh tr #||# vv tl tr vv #||# vv bl br vv #||# bl hh tr vv #||# tl hh br vv #||# bl hh hh br)))

           ;; Time for the hands to move to the new position (in seconds); this must be <1s
           (hands-move-duration 0.5)

           (prev-seconds -1)
           (current-angles (make-array '(6 24)))
           (src-angles (make-array '(6 24)))
           (dst-angles (make-array '(6 24)))

           (hands-move-timer 0.0)
           (hour-mode 24))

      (dotimes (i (array-total-size current-angles))
        (setf (row-major-aref current-angles i) (vec2 0.0 0.0)
              (row-major-aref src-angles i) (vec2 0.0 0.0)
              (row-major-aref dst-angles i) (vec2 0.0 0.0)))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Get the current time
               (multiple-value-bind (tm-sec tm-min tm-hour) (get-decoded-time)

                 (when (/= tm-sec prev-seconds)
                   ;; The time has changed, so we need to move the hands to the new positions
                   (setf prev-seconds tm-sec)

                   ;; Format the current time so we can access the individual digits
                   (let ((clock-digits (text-format "%02d%02d%02d" (mod tm-hour hour-mode) tm-min tm-sec)))

                     ;; Fetch where we want all the hands to be
                     (dotimes (digit 6)
                       (dotimes (cell 24)
                         (setf (aref src-angles digit cell) (vcopy (aref current-angles digit cell)))
                         (setf (aref dst-angles digit cell) (vcopy (aref (aref digit-angles (digit-char-p (char clock-digits digit))) cell)))

                         ;; Quick exception for 12h mode
                         (when (and (= digit 0) (= hour-mode 12) (char= (char clock-digits 0) #\0)) (setf (aref dst-angles digit cell) (vcopy zz)))

                         (let ((src (aref src-angles digit cell))
                               (dst (aref dst-angles digit cell)))
                           (when (> (vx src) (vx dst)) (decf (vx src) 360.0))
                           (when (> (vy src) (vy dst)) (decf (vy src) 360.0))))))

                   ;; Reset the timer
                   (setf hands-move-timer (- (get-frame-time))))

                 ;; Now let's animate all the hands if we need to
                 (when (< hands-move-timer hands-move-duration)
                   ;; Increase the timer but don't go above the maximum
                   (setf hands-move-timer (clamp (+ hands-move-timer (get-frame-time)) 0.0 hands-move-duration))

                   ;; Calculate the%completion of the animation
                   (let ((tt (/ hands-move-timer hands-move-duration)))

                     ;; A little cheeky smoothstep
                     (setf tt (* tt tt (- 3.0 (* 2.0 tt))))

                     (dotimes (digit 6)
                       (dotimes (cell 24)
                         (let ((current (aref current-angles digit cell))
                               (src (aref src-angles digit cell))
                               (dst (aref dst-angles digit cell)))
                           (setf (vx current) (lerp (vx src) (vx dst) tt)
                                 (vy current) (lerp (vy src) (vy dst) tt)))))))

                 ;; Handle input
                 (when (is-key-pressed +key-space+) (setf hour-mode (- 36 hour-mode)))) ; Toggle between 12 and 24 hour mode with space
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background bg-color)

               (draw-text (text-format "%d-h mode, space to change" hour-mode) 10 30 20 +raywhite+)

               (let ((x-offset 4.0))

                 (dotimes (digit 6)
                   (dotimes (row 6)
                     (dotimes (col 4)
                       (let ((centre (vec2 (+ x-offset (* col (+ clock-face-size clock-face-spacing)) (* clock-face-size 0.5))
                                           (+ 100 (* row (+ clock-face-size clock-face-spacing)) (* clock-face-size 0.5)))))

                         (draw-ring centre (- (* clock-face-size 0.5) 2.0) (* clock-face-size 0.5) 0.0 360.0 24 +darkgray+)

                         ;; Big hand
                         (draw-rectangle-pro (make-rectangle :x (vx centre) :y (vy centre) :width (+ (* clock-face-size 0.5) 4.0) :height 4.0)
                                             (vec2 2.0 2.0)
                                             (vx (aref current-angles digit (+ (* row 4) col)))
                                             hands-color)

                         ;; Little hand
                         (draw-rectangle-pro (make-rectangle :x (vx centre) :y (vy centre) :width (+ (* clock-face-size 0.5) 2.0) :height 4.0)
                                             (vec2 2.0 2.0)
                                             (vy (aref current-angles digit (+ (* row 4) col)))
                                             hands-color))))

                   (incf x-offset (* (+ clock-face-size clock-face-spacing) 4))
                   (when (= (mod digit 2) 1)
                     (draw-ring (vec2 (+ x-offset 4.0) 160.0) 6.0 8.0 0.0 360.0 24 hands-color)
                     (draw-ring (vec2 (+ x-offset 4.0) 225.0) 6.0 8.0 0.0 360.0 24 hands-color)
                     (incf x-offset section-spacing))))

               (draw-fps 10 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
