;;;; textures_particles_blending.lisp - Particles blending
;;;; Translated from raylib/examples/textures/textures_particles_blending.c

(require :cl-raylib)

(defpackage :textures-particles-blending
  (:use :cl :cl-raylib))

(in-package :textures-particles-blending)

(defconstant +max-particles+ 200)

;; Particle structure with basic data
(defstruct particle
  (position (vec2 0.0 0.0) :type vec2)
  (color +white+ :type color)
  (alpha 1.0 :type single-float)
  (size 1.0 :type single-float)
  (rotation 0.0 :type single-float)
  (active nil :type boolean)) ; Use it to activate/deactivate particle

(defun main ()
  "Main function - particles blending"
  (let ((screen-width 800)
        (screen-height 450))

    ;; Initialization
    (init-window screen-width screen-height "raylib [textures] example - particles blending")

    ;; Particles pool, reuse them!
    (let ((mouse-tail (make-array +max-particles+)))

      ;; Initialize particles
      (loop for i from 0 below +max-particles+ do
        (setf (aref mouse-tail i)
              (make-particle :position (vec2 0.0 0.0)
                            :color (make-color (get-random-value 0 255)
                                             (get-random-value 0 255)
                                             (get-random-value 0 255)
                                             255)
                            :alpha 1.0
                            :size (/ (get-random-value 1 30) 20.0)
                            :rotation (float (get-random-value 0 360))
                            :active nil)))

      (let ((gravity 3.0)
            (smoke (load-texture "examples/textures/resources/spark_flame.png"))
            (blending +blend-alpha+))

        (set-target-fps 60)

        ;; Main game loop
        (loop until (window-should-close) do
          ;; Update
          
          ;; Activate one particle every frame and Update active particles
          ;; NOTE: Particles initial position should be mouse position when activated
          ;; NOTE: Particles fall down with gravity and rotation... and disappear after 2 seconds (alpha = 0)
          ;; NOTE: When a particle disappears, active = false and it can be reused.
          (loop for i from 0 below +max-particles+ do
            (unless (particle-active (aref mouse-tail i))
              (setf (particle-active (aref mouse-tail i)) t)
              (setf (particle-alpha (aref mouse-tail i)) 1.0)
              (setf (particle-position (aref mouse-tail i)) (get-mouse-position))
              (return)))

          (loop for i from 0 below +max-particles+ do
            (when (particle-active (aref mouse-tail i))
              (incf (vy (particle-position (aref mouse-tail i))) (/ gravity 2))
              (decf (particle-alpha (aref mouse-tail i)) 0.005)

              (when (<= (particle-alpha (aref mouse-tail i)) 0.0)
                (setf (particle-active (aref mouse-tail i)) nil))

              (incf (particle-rotation (aref mouse-tail i)) 2.0)))

          (when (is-key-pressed +key-space+)
            (if (= blending +blend-alpha+)
                (setf blending +blend-additive+)
                (setf blending +blend-alpha+)))

          ;; Draw
          (begin-drawing)
            (clear-background +darkgray+)

            (begin-blend-mode blending)

              ;; Draw active particles
              (loop for i from 0 below +max-particles+ do
                (when (particle-active (aref mouse-tail i))
                  (let ((p (aref mouse-tail i)))
                    (draw-texture-pro smoke 
                                     (make-rectangle :x 0.0 :y 0.0 
                                                    :width (float (texture-width smoke))
                                                    :height (float (texture-height smoke)))
                                     (make-rectangle :x (vx (particle-position p))
                                                    :y (vy (particle-position p))
                                                    :width (* (texture-width smoke) (particle-size p))
                                                    :height (* (texture-height smoke) (particle-size p)))
                                     (vec2 (* (texture-width smoke) (particle-size p) 0.5)
                                          (* (texture-height smoke) (particle-size p) 0.5))
                                     (particle-rotation p)
                                     (fade (particle-color p) (particle-alpha p))))))

            (end-blend-mode)

            (draw-text "PRESS SPACE to CHANGE BLENDING MODE" 180 20 20 +black+)

            (if (= blending +blend-alpha+)
                (draw-text "ALPHA BLENDING" 290 (- screen-height 40) 20 +black+)
                (draw-text "ADDITIVE BLENDING" 280 (- screen-height 40) 20 +raywhite+))

          (end-drawing))

        ;; De-Initialization
        (unload-texture smoke))))

    ;; Close window
    (close-window)))

;; Run the example
(main)