;;;; raylib [textures] example - particles blending
;;;;
;;;; Example complexity rating: [★☆☆☆] 1/4
;;;;
;;;; Example originally created with raylib 1.7, last time updated with raylib 3.5
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2017-2025 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/textures/textures_particles_blending.c

(require :cl-raylib)

(defpackage #:raylib-examples/textures-particles-blending
  (:use #:cl #:raylib))
(in-package #:raylib-examples/textures-particles-blending)

(defconstant +max-particles+ 200)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Particle structure
(defstruct particle
  (position (vec2 0.0 0.0))
  (color +blank+)
  (alpha 0.0)
  (size 0.0)
  (rotation 0.0)
  (active nil))                         ; NOTE: Use it to activate/deactive particle

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [textures] example - particles blending")

    ;; Particles pool, reuse them!
    (let ((mouse-tail (make-array +max-particles+)))

      ;; Initialize particles
      (dotimes (i +max-particles+)
        (setf (aref mouse-tail i)
              (make-particle :position (vec2 0.0 0.0)
                             :color (let* ((r (get-random-value 0 255))
                                           (g (get-random-value 0 255))
                                           (b (get-random-value 0 255)))
                                      (list r g b 255))
                             :alpha 1.0
                             :size (/ (float (get-random-value 1 30)) 20.0)
                             :rotation (float (get-random-value 0 360))
                             :active nil)))

      (let ((gravity 3.0)
            (smoke (load-texture "resources/spark_flame.png"))
            (blending +blend-alpha+))

        (set-target-fps 60)
        ;;--------------------------------------------------------------------------------------

        ;; Main game loop
        (loop until (window-should-close) ; Detect window close button or ESC key
              do ;; Update
                 ;;----------------------------------------------------------------------------------
                 ;; Activate one particle every frame and Update active particles
                 ;; NOTE: Particles initial position should be mouse position when activated
                 ;; NOTE: Particles fall down with gravity and rotation... and disappear after 2 seconds (alpha = 0)
                 ;; NOTE: When a particle disappears, active = false and it can be reused
                 (loop for particle across mouse-tail
                       do (unless (particle-active particle)
                            (setf (particle-active particle) t
                                  (particle-alpha particle) 1.0
                                  (particle-position particle) (get-mouse-position))
                            (return)))

                 (loop for particle across mouse-tail
                       do (when (particle-active particle)
                            (incf (vy (particle-position particle)) (/ gravity 2))
                            (decf (particle-alpha particle) 0.005)

                            (when (<= (particle-alpha particle) 0.0) (setf (particle-active particle) nil))

                            (incf (particle-rotation particle) 2.0)))

                 (when (is-key-pressed +key-space+)
                   (if (= blending +blend-alpha+)
                       (setf blending +blend-additive+)
                       (setf blending +blend-alpha+)))
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +darkgray+)

                 (begin-blend-mode blending)

                 ;; Draw active particles
                 (loop for particle across mouse-tail
                       do (when (particle-active particle)
                            (let ((position (particle-position particle))
                                  (size (particle-size particle)))
                              (draw-texture-pro smoke (make-rectangle :x 0.0 :y 0.0 :width (float (texture-width smoke)) :height (float (texture-height smoke)))
                                                (make-rectangle :x (vx position) :y (vy position)
                                                                :width (* (texture-width smoke) size) :height (* (texture-height smoke) size))
                                                (vec2 (/ (* (texture-width smoke) size) 2.0) (/ (* (texture-height smoke) size) 2.0))
                                                (particle-rotation particle)
                                                (fade (particle-color particle) (particle-alpha particle))))))

                 (end-blend-mode)

                 (draw-text "PRESS SPACE to CHANGE BLENDING MODE" 180 20 20 +black+)

                 (if (= blending +blend-alpha+)
                     (draw-text "ALPHA BLENDING" 290 (- screen-height 40) 20 +black+)
                     (draw-text "ADDITIVE BLENDING" 280 (- screen-height 40) 20 +raywhite+))

                 (end-drawing))
        ;;----------------------------------------------------------------------------------

        ;; De-Initialization
        ;;--------------------------------------------------------------------------------------
        (unload-texture smoke)

        (close-window)))))              ; Close window and OpenGL context

(main)
