;;;; raylib [shapes] example - easings rectangles
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; NOTE: This example requires 'easings.h' library, provided on raylib/src. Just copy
;;;; the library to same directory as example or make sure it's available on include path
;;;;
;;;; Example originally created with raylib 2.0, last time updated with raylib 2.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2014-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/shapes_easings_rectangles.c

(require :cl-raylib)
(load (merge-pathnames "reasings.lisp" *load-truename*)) ; Required for easing functions

(defpackage #:raylib-examples/shapes-easings-rectangles
  (:use #:cl #:raylib #:reasings))
(in-package #:raylib-examples/shapes-easings-rectangles)

(defconstant +recs-width+ 50)
(defconstant +recs-height+ 50)

(defconstant +max-recs-x+ (truncate 800 +recs-width+))
(defconstant +max-recs-y+ (truncate 450 +recs-height+))

(defconstant +play-time-in-frames+ 240) ; At 60 fps = 4 seconds

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - easings rectangles")

    (let ((recs (make-array (* +max-recs-x+ +max-recs-y+)))
          (rotation 0.0)
          (frames-counter 0)
          (state 0))                    ; Rectangles animation state: 0-Playing, 1-Finished

      (dotimes (y +max-recs-y+)
        (dotimes (x +max-recs-x+)
          (setf (aref recs (+ (* y +max-recs-x+) x))
                (make-rectangle :x (+ (/ +recs-width+ 2.0) (* +recs-width+ x))
                                :y (+ (/ +recs-height+ 2.0) (* +recs-height+ y))
                                :width (float +recs-width+)
                                :height (float +recs-height+)))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (cond ((= state 0)
                      (incf frames-counter)

                      (loop for rec across recs
                            do (setf (rectangle-height rec) (ease-circ-out (float frames-counter) +recs-height+ (- +recs-height+) +play-time-in-frames+))
                               (setf (rectangle-width rec) (ease-circ-out (float frames-counter) +recs-width+ (- +recs-width+) +play-time-in-frames+))

                               (when (< (rectangle-height rec) 0) (setf (rectangle-height rec) 0.0))
                               (when (< (rectangle-width rec) 0) (setf (rectangle-width rec) 0.0))

                               (when (and (= (rectangle-height rec) 0) (= (rectangle-width rec) 0)) (setf state 1)) ; Finish playing

                               (setf rotation (ease-linear-in (float frames-counter) 0.0 360.0 +play-time-in-frames+))))
                     ((and (= state 1) (is-key-pressed +key-space+))
                      ;; When animation has finished, press space to restart
                      (setf frames-counter 0)

                      (loop for rec across recs
                            do (setf (rectangle-height rec) (float +recs-height+)
                                     (rectangle-width rec) (float +recs-width+)))

                      (setf state 0)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (cond ((= state 0)
                      (loop for rec across recs
                            do (draw-rectangle-pro rec (vec2 (/ (rectangle-width rec) 2) (/ (rectangle-height rec) 2)) rotation +red+)))
                     ((= state 1) (draw-text "PRESS [SPACE] TO PLAY AGAIN!" 240 200 20 +gray+)))

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
