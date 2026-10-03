;;;; raylib [shapes] example - mouse trail
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Balamurugan R (@Bala050814) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Balamurugan R (@Bala050814)
;;;; Common Lisp port of raylib/examples/shapes/shapes_mouse_trail.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-mouse-trail
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-mouse-trail)

;; Define the maximum number of positions to store in the trail
(defconstant +max-trail-length+ 30)

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - mouse trail")

    ;; Array to store the history of mouse positions (our fixed-size queue)
    (let ((trail-positions (make-array +max-trail-length+)))
      (dotimes (i +max-trail-length+) (setf (aref trail-positions i) (vec2 0.0 0.0)))

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((mouse-position (get-mouse-position)))

                 ;; Shift all existing positions backward by one slot in the array
                 ;; The last element (the oldest position) is dropped
                 (loop for i from (1- +max-trail-length+) above 0
                       do (setf (aref trail-positions i) (aref trail-positions (1- i))))

                 ;; Store the new, current mouse position at the start of the array (Index 0)
                 (setf (aref trail-positions 0) mouse-position)
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +black+)

                 ;; Draw the trail by looping through the history array
                 (dotimes (i +max-trail-length+)
                   (let ((pos (aref trail-positions i)))
                     ;; Ensure we skip drawing if the array hasn't been fully filled on startup
                     (when (or (/= (vx pos) 0.0) (/= (vy pos) 0.0))
                       ;; Calculate relative trail strength (ratio is near 1.0 for new, near 0.0 for old)
                       (let* ((ratio (/ (float (- +max-trail-length+ i)) +max-trail-length+))
                              ;; Fade effect: oldest positions are more transparent
                              ;; Fade (color, alpha) - alpha is 0.5 to 1.0 based on ratio
                              (trail-color (fade +skyblue+ (+ (* ratio 0.5) 0.5)))
                              ;; Size effect: oldest positions are smaller
                              (trail-radius (* 15.0 ratio)))

                         (draw-circle-v pos trail-radius trail-color)))))

                 ;; Draw a distinct white circle for the current mouse position (Index 0)
                 (draw-circle-v mouse-position 15.0 +white+)

                 (draw-text "Move the mouse to see the trail effect!" 10 (- screen-height 30) 20 +lightgray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
