;;;; raylib [shapes] example - simple particles
;;;;
;;;; Example complexity rating: [★★☆☆] 2/4
;;;;
;;;; Example originally created with raylib 5.6, last time updated with raylib 5.6
;;;;
;;;; Example contributed by Jordi Santonja (@JordSant)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 Jordi Santonja (@JordSant)
;;;; Common Lisp port of raylib/examples/shapes/shapes_simple_particles.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-simple-particles
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-simple-particles)

(defconstant +max-particles+ 3000)      ; Max number of particles

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; ParticleType
(defconstant +water+ 0)
(defconstant +smoke+ 1)
(defconstant +fire+ 2)

(defparameter *particle-type-names* (vector "WATER" "SMOKE" "FIRE"))

(defstruct particle
  (type +water+)                        ; Particle type (WATER, SMOKE, FIRE)
  (position (vec2 0.0 0.0))             ; Particle position on screen
  (velocity (vec2 0.0 0.0))             ; Particle current speed and direction
  (radius 0.0)                          ; Particle radius
  (color (list 0 0 0 0))                ; Particle color
  (life-time 0.0)                       ; Particle life time
  (alive nil))                          ; Particle alive: inside screen and life time

(defstruct circular-buffer
  (head 0)                              ; Index for the next write
  (tail 0)                              ; Index for the next read
  (buffer nil))                         ; Particle buffer array

;;------------------------------------------------------------------------------------
;; Module Functions Definition
;;------------------------------------------------------------------------------------
;; C rand() from libc, so the particles match the C example
(defun rand () (cffi:foreign-funcall "rand" :int))

(defun add-to-circular-buffer (circular-buffer)
  (let ((particle nil))
    ;; Check if buffer full
    (when (/= (mod (1+ (circular-buffer-head circular-buffer)) +max-particles+) (circular-buffer-tail circular-buffer))
      ;; Add new particle to the head position and advance head
      (setf particle (aref (circular-buffer-buffer circular-buffer) (circular-buffer-head circular-buffer)))
      (setf (circular-buffer-head circular-buffer) (mod (1+ (circular-buffer-head circular-buffer)) +max-particles+)))
    particle))

(defun emit-particle (circular-buffer emitter-position type)
  (let ((new-particle (add-to-circular-buffer circular-buffer)))
    ;; If buffer is full, newParticle is NULL
    (when new-particle
      ;; Fill particle properties
      (setf (particle-position new-particle) (vcopy emitter-position)
            (particle-alive new-particle) t
            (particle-life-time new-particle) 0.0
            (particle-type new-particle) type)
      (let ((speed (/ (float (mod (rand) 10)) 5.0)))
        (case type
          (#.+water+
           (setf (particle-radius new-particle) 5.0
                 (particle-color new-particle) (copy-list +blue+)))
          (#.+smoke+
           (setf (particle-radius new-particle) 7.0
                 (particle-color new-particle) (copy-list +gray+)))
          (#.+fire+
           (setf (particle-radius new-particle) 10.0
                 (particle-color new-particle) (copy-list +yellow+))
           (setf speed (/ speed 10.0))))

        (let ((direction (float (mod (rand) 360))))
          (setf (particle-velocity new-particle) (vec2 (* speed (cos (* direction +deg2rad+)))
                                                       (* speed (sin (* direction +deg2rad+))))))))))

(defun update-particles (circular-buffer screen-width screen-height)
  (loop with buffer = (circular-buffer-buffer circular-buffer)
        for i = (circular-buffer-tail circular-buffer) then (mod (1+ i) +max-particles+)
        until (= i (circular-buffer-head circular-buffer))
        do (let* ((particle (aref buffer i))
                  (position (particle-position particle))
                  (velocity (particle-velocity particle))
                  (color (particle-color particle)))
             ;; Update particle life and positions
             (incf (particle-life-time particle) (/ 1.0 60.0)) ; 60 FPS -> 1/60 seconds per frame

             (case (particle-type particle)
               (#.+water+
                (incf (vx position) (vx velocity))
                (incf (vy velocity) 0.2) ; Gravity
                (incf (vy position) (vy velocity)))
               (#.+smoke+
                (incf (vx position) (vx velocity))
                (decf (vy velocity) 0.05) ; Upwards
                (incf (vy position) (vy velocity))
                (incf (particle-radius particle) 0.5) ; Increment radius: smoke expands
                (setf (fourth color) (mod (- (fourth color) 4) 256)) ; Decrement alpha: smoke fades

                ;; If alpha transparent, particle dies
                (when (< (fourth color) 4) (setf (particle-alive particle) nil)))
               (#.+fire+
                ;; Add a little horizontal oscillation to fire particles
                (incf (vx position) (+ (vx velocity) (cos (* (particle-life-time particle) 215.0))))
                (decf (vy velocity) 0.05) ; Upwards
                (incf (vy position) (vy velocity))
                (decf (particle-radius particle) 0.15) ; Decrement radius: fire shrinks
                (setf (second color) (mod (- (second color) 3) 256)) ; Decrement green: fire turns reddish starting from yellow

                ;; If radius too small, particle dies
                (when (<= (particle-radius particle) 0.02) (setf (particle-alive particle) nil))))

             ;; Disable particle when out of screen
             (let ((radius (particle-radius particle)))
               (when (or (< (vx position) (- radius)) (> (vx position) (+ screen-width radius))
                         (< (vy position) (- radius)) (> (vy position) (+ screen-height radius)))
                 (setf (particle-alive particle) nil))))))

(defun update-circular-buffer (circular-buffer)
  ;; Update circular buffer: advance tail over dead particles
  (loop while (and (/= (circular-buffer-tail circular-buffer) (circular-buffer-head circular-buffer))
                   (not (particle-alive (aref (circular-buffer-buffer circular-buffer) (circular-buffer-tail circular-buffer)))))
        do (setf (circular-buffer-tail circular-buffer) (mod (1+ (circular-buffer-tail circular-buffer)) +max-particles+))))

(defun draw-particles (circular-buffer)
  (loop with buffer = (circular-buffer-buffer circular-buffer)
        for i = (circular-buffer-tail circular-buffer) then (mod (1+ i) +max-particles+)
        until (= i (circular-buffer-head circular-buffer))
        do (let ((particle (aref buffer i)))
             (when (particle-alive particle)
               (draw-circle-v (particle-position particle) (particle-radius particle) (particle-color particle))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [shapes] example - simple particles")

    (let* (;; Definition of particles
           (particles (let ((a (make-array +max-particles+))) ; Particle array
                        (dotimes (i +max-particles+ a) (setf (aref a i) (make-particle)))))
           (circular-buffer (make-circular-buffer :head 0 :tail 0 :buffer particles))

           ;; Particle emitter parameters
           (emission-rate -2)           ; Negative: on average every -X frames. Positive: particles per frame
           (current-type +water+)
           (emitter-position (vec2 (/ screen-width 2.0) (/ screen-height 2.0))))

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Emit new particles: when emissionRate is 1, emit every frame
               (if (< emission-rate 0)
                   (when (= (rem (rand) (- emission-rate)) 0) (emit-particle circular-buffer emitter-position current-type))
                   (loop for i from 0 to emission-rate do (emit-particle circular-buffer emitter-position current-type)))

               ;; Update the parameters of each particle
               (update-particles circular-buffer screen-width screen-height)

               ;; Remove dead particles from the circular buffer
               (update-circular-buffer circular-buffer)

               ;; Change Particle Emission Rate (UP/DOWN arrows)
               (when (is-key-pressed +key-up+) (incf emission-rate))
               (when (is-key-pressed +key-down+) (decf emission-rate))

               ;; Change Particle Type (LEFT/RIGHT arrows)
               (when (is-key-pressed +key-right+) (if (= current-type +fire+) (setf current-type +water+) (incf current-type)))
               (when (is-key-pressed +key-left+) (if (= current-type +water+) (setf current-type +fire+) (decf current-type)))

               (when (is-mouse-button-down +mouse-left-button+) (setf emitter-position (get-mouse-position)))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               ;; Call the function with a loop to draw all particles
               (draw-particles circular-buffer)

               ;; Draw UI and Instructions
               (draw-rectangle 5 5 315 75 (fade +skyblue+ 0.5))
               (draw-rectangle-lines 5 5 315 75 +blue+)

               (draw-text "CONTROLS:" 15 15 10 +black+)
               (draw-text "UP/DOWN: Change Particle Emission Rate" 15 35 10 +black+)
               (draw-text "LEFT/RIGHT: Change Particle Type (Water, Smoke, Fire)" 15 55 10 +black+)

               (if (< emission-rate 0)
                   (draw-text (text-format "Particles every %d frames | Type: %s" (- emission-rate) (aref *particle-type-names* current-type)) 15 95 10 +darkgray+)
                   (draw-text (text-format "%d Particles per frame | Type: %s" (+ emission-rate 1) (aref *particle-type-names* current-type)) 15 95 10 +darkgray+))

               (draw-fps (- screen-width 80) 10)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
