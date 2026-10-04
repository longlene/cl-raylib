;;;; raylib [audio] example - sound positioning
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;;
;;;; Example contributed by Le Juez Victor (@Bigfoot71) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Le Juez Victor (@Bigfoot71)
;;;; Common Lisp port of raylib/examples/audio/audio_sound_positioning.c

(require :cl-raylib)

(defpackage #:raylib-examples/audio-sound-positioning
  (:use #:cl #:raylib))
(in-package #:raylib-examples/audio-sound-positioning)

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; Set sound 3d position
(defun set-sound-position (listener sound position max-dist)
  ;; Calculate direction vector and distance between listener and sound source
  (let* ((direction (vector3-subtract position (camera3d-position listener)))
         (distance (vector3-length direction))

         ;; Apply logarithmic distance attenuation and clamp between 0-1
         (attenuation (clamp (/ 1.0 (+ 1.0 (/ distance max-dist))) 0.0 1.0))

         ;; Calculate normalized vectors for spatial positioning
         (normalized-direction (vector3-normalize direction))
         (forward (vector3-normalize (vector3-subtract (camera3d-target listener) (camera3d-position listener))))
         (right (vector3-normalize (vector3-cross-product forward (camera3d-up listener))))

         ;; Reduce volume for sounds behind the listener
         (dot-product (vector3-dot-product forward normalized-direction)))

    (when (< dot-product 0.0) (setf attenuation (* attenuation (+ 1.0 (* dot-product 0.5)))))

    ;; Set stereo panning based on sound position relative to listener
    (let ((pan (vector3-dot-product normalized-direction right)))

      ;; Apply final sound properties
      (set-sound-volume sound attenuation)
      (set-sound-pan sound pan))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [audio] example - sound positioning")

    (init-audio-device)

    (let ((sound (load-sound "resources/coin.wav"))
          (camera (make-camera3d :position (vec3 0.0 5.0 5.0)
                                 :target (vec3 0.0 0.0 0.0)
                                 :up (vec3 0.0 1.0 0.0)
                                 :fovy 60
                                 :projection +camera-perspective+)))

      (disable-cursor)

      (set-target-fps 60)
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close)
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (update-camera camera +camera-free+)

               (let* ((th (float (get-time) 1.0))
                      (sphere-pos (vec3 (* 5.0 (cos th)) 0.0 (* 5.0 (sin th)))))

                 (set-sound-position camera sound sphere-pos 1.0)

                 (unless (is-sound-playing sound) (play-sound sound))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 (begin-mode-3d camera)
                 (draw-grid 10 2)
                 (draw-sphere sphere-pos 0.5 +red+)
                 (end-mode-3d)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-sound sound)
      (close-audio-device)              ; Close audio device

      (close-window))))                 ; Close window and OpenGL context

(main)
