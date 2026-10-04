;;;; raylib [shaders] example - spotlight rendering
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 3.7
;;;;
;;;; Example contributed by Chris Camacho (@chriscamacho) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Chris Camacho (@chriscamacho) and Ramon Santamaria (@raysan5)
;;;;
;;;; *******************************************************************************************
;;;;
;;;; The shader makes alpha holes in the forground to give the appearance of a top
;;;; down look at a spotlight casting a pool of light...
;;;;
;;;; The right hand side of the screen there is just enough light to see whats
;;;; going on without the spot light, great for a stealth type game where you
;;;; have to avoid the spotlights
;;;;
;;;; The left hand side of the screen is in pitch dark except for where the spotlights are
;;;;
;;;; Although this example doesn't scale like the letterbox example, you could integrate
;;;; the two techniques, but by scaling the actual colour of the render texture rather
;;;; than using alpha as a mask
;;;; Common Lisp port of raylib/examples/shaders/shaders_spotlight_rendering.c

(require :cl-raylib)

(defpackage #:raylib-examples/shaders-spotlight-rendering
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shaders-spotlight-rendering)

(defconstant +glsl-version+ 330)        ; PLATFORM_DESKTOP

(defconstant +max-spots+ 3)             ; NOTE: It must be the same as define in shader
(defconstant +max-stars+ 400)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Spot data
(defstruct spot
  (position (vec2 0.0 0.0))
  (speed (vec2 0.0 0.0))
  (inner 0.0)
  (radius 0.0)

  ;; Shader locations
  (position-loc 0)
  (inner-loc 0)
  (radius-loc 0))

;; Stars in the star field have a position and velocity
(defstruct star
  (position (vec2 0.0 0.0))
  (speed (vec2 0.0 0.0)))

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
(defun reset-star (star)
  (setf (star-position star) (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))

  (setf (vx (star-speed star)) (/ (float (get-random-value -1000 1000)) 100.0)
        (vy (star-speed star)) (/ (float (get-random-value -1000 1000)) 100.0))

  ;; NOTE: Like C, the condition is !(fabs(x) + (fabs(y) > 1)), so it only loops while x == 0 and |y| <= 1
  (loop while (not (/= 0 (+ (abs (float (vx (star-speed star)) 1d0)) (if (> (abs (vy (star-speed star))) 1) 1 0))))
        do (setf (vx (star-speed star)) (/ (float (get-random-value -1000 1000)) 100.0)
                 (vy (star-speed star)) (/ (float (get-random-value -1000 1000)) 100.0)))

  (setf (star-position star) (vector2-add (star-position star) (vector2-multiply (star-speed star) (vec2 8.0 8.0)))))

(defun update-star (star)
  (setf (star-position star) (vector2-add (star-position star) (star-speed star)))

  (when (or (< (vx (star-position star)) 0) (> (vx (star-position star)) (get-screen-width))
            (< (vy (star-position star)) 0) (> (vy (star-position star)) (get-screen-height)))
    (reset-star star)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shaders] example - spotlight rendering")

    (hide-cursor)

    (let ((tex-ray (load-texture "resources/raysan.png"))
          (stars (make-array +max-stars+))
          (frame-counter 0)
          (shdr-spot nil)
          (spots (make-array +max-spots+))
          (w-loc 0))

      (dotimes (n +max-stars+)
        (setf (aref stars n) (make-star))
        (reset-star (aref stars n)))

      ;; Progress all the stars on, so they don't all start in the centre
      (loop for m from 0 below (/ screen-width 2.0)
            do (dotimes (n +max-stars+) (update-star (aref stars n))))

      ;; Use default vert shader
      (setf shdr-spot (load-shader nil (text-format "resources/shaders/glsl%i/spotlight.fs" +glsl-version+)))

      ;; Get the locations of spots in the shader
      (dotimes (i +max-spots+)
        (let ((spot (make-spot)))
          (setf (spot-position-loc spot) (get-shader-location shdr-spot (format nil "spots[~d].pos" i))
                (spot-inner-loc spot) (get-shader-location shdr-spot (format nil "spots[~d].inner" i))
                (spot-radius-loc spot) (get-shader-location shdr-spot (format nil "spots[~d].radius" i)))
          (setf (aref spots i) spot)))

      ;; Tell the shader how wide the screen is so we can have
      ;; a pitch black half and a dimly lit half
      (setf w-loc (get-shader-location shdr-spot "screenWidth"))
      (set-shader-value shdr-spot w-loc (float (get-screen-width)) +shader-uniform-float+)

      ;; Randomize the locations and velocities of the spotlights
      ;; and initialize the shader locations
      (dotimes (i +max-spots+)
        (let ((spot (aref spots i)))
          (setf (vx (spot-position spot)) (float (get-random-value 64 (- screen-width 64)))
                (vy (spot-position spot)) (float (get-random-value 64 (- screen-height 64))))
          (setf (spot-speed spot) (vec2 0.0 0.0))

          (loop while (< (+ (abs (vx (spot-speed spot))) (abs (vy (spot-speed spot)))) 2)
                do (setf (vx (spot-speed spot)) (/ (get-random-value -400 40) 25.0)
                         (vy (spot-speed spot)) (/ (get-random-value -400 40) 25.0)))

          (setf (spot-inner spot) (* 28.0 (1+ i))
                (spot-radius spot) (* 48.0 (1+ i)))

          (set-shader-value shdr-spot (spot-position-loc spot) (spot-position spot) +shader-uniform-vec2+)
          (set-shader-value shdr-spot (spot-inner-loc spot) (spot-inner spot) +shader-uniform-float+)
          (set-shader-value shdr-spot (spot-radius-loc spot) (spot-radius spot) +shader-uniform-float+)))

      (set-target-fps 60)               ; Set  to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (incf frame-counter)

               ;; Move the stars, resetting them if the go offscreen
               (dotimes (n +max-stars+) (update-star (aref stars n)))

               ;; Update the spots, send them to the shader
               (dotimes (i +max-spots+)
                 (let* ((spot (aref spots i))
                        (position (spot-position spot))
                        (speed (spot-speed spot)))
                   (if (= i 0)
                       (let ((mp (get-mouse-position)))
                         (setf (vx position) (vx mp)
                               (vy position) (- screen-height (vy mp))))
                       (progn
                         (incf (vx position) (vx speed))
                         (incf (vy position) (vy speed))

                         (when (< (vx position) 64) (setf (vx speed) (- (vx speed))))
                         (when (> (vx position) (- screen-width 64)) (setf (vx speed) (- (vx speed))))
                         (when (< (vy position) 64) (setf (vy speed) (- (vy speed))))
                         (when (> (vy position) (- screen-height 64)) (setf (vy speed) (- (vy speed))))))

                   (set-shader-value shdr-spot (spot-position-loc spot) position +shader-uniform-vec2+)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +darkblue+)

               ;; Draw stars and bobs
               (dotimes (n +max-stars+)
                 ;; Single pixel is just too small these days!
                 (draw-rectangle (truncate (vx (star-position (aref stars n)))) (truncate (vy (star-position (aref stars n)))) 2 2 +white+))

               ;; NOTE: Like C, cos()/sin() and the sums are computed in double precision
               (dotimes (i 16)
                 (draw-texture tex-ray
                               (truncate (- (+ (float (/ screen-width 2.0) 1d0)
                                               (* (cos (float (/ (float (+ frame-counter (* i 8))) 51.45) 1d0)) (float (/ screen-width 2.2) 1d0)))
                                            32))
                               (truncate (+ (float (/ screen-height 2.0) 1d0)
                                            (* (sin (float (/ (float (+ frame-counter (* i 8))) 17.87) 1d0)) (float (/ screen-height 4.2) 1d0))))
                               +white+))

               ;; Draw spot lights
               (begin-shader-mode shdr-spot)
               ;; Instead of a blank rectangle you could render here
               ;; a render texture of the full screen used to do screen
               ;; scaling (slight adjustment to shader would be required
               ;; to actually pay attention to the colour!)
               (draw-rectangle 0 0 screen-width screen-height +white+)
               (end-shader-mode)

               (draw-fps 10 10)

               (draw-text "Move the mouse!" 10 30 20 +green+)
               (draw-text "Pitch Black" (truncate (* screen-width 0.2)) (floor screen-height 2) 20 +green+)
               (draw-text "Dark" (truncate (* screen-width 0.66)) (floor screen-height 2) 20 +green+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-texture tex-ray)
      (unload-shader shdr-spot)

      (close-window))))                 ; Close window and OpenGL context

(main)
