;;;; raylib [shapes] example - penrose tile
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 5.5, last time updated with raylib 6.0
;;;; Based on: https://processing.org/examples/penrosetile.html
;;;;
;;;; Example contributed by David Buzatto (@davidbuzatto) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 David Buzatto (@davidbuzatto)
;;;; Common Lisp port of raylib/examples/shapes/shapes_penrose_tile.c

(require :cl-raylib)

(defpackage #:raylib-examples/shapes-penrose-tile
  (:use #:cl #:raylib))
(in-package #:raylib-examples/shapes-penrose-tile)

(defconstant +str-max-size+ 10000)
(defconstant +turtle-stack-max-size+ 50)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct turtle-state
  (origin (vec2 0.0 0.0))
  (angle 0.0))

(defstruct penrose-l-system
  (steps 0)
  (production "")
  (rule-w "")
  (rule-x "")
  (rule-y "")
  (rule-z "")
  (draw-length 0.0)
  (theta 0.0))

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defparameter *turtle-stack* (make-array +turtle-stack-max-size+ :initial-element nil))
(defparameter *turtle-top* -1)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
;; Push turtle state for next step
(defun push-turtle-state (state)
  (if (< *turtle-top* (1- +turtle-stack-max-size+))
      (setf (aref *turtle-stack* (incf *turtle-top*))
            (make-turtle-state :origin (vcopy (turtle-state-origin state)) :angle (turtle-state-angle state)))
      (trace-log +log-warning+ "TURTLE STACK OVERFLOW!")))

;; Pop turtle state step
(defun pop-turtle-state ()
  (if (>= *turtle-top* 0)
      (prog1 (aref *turtle-stack* *turtle-top*) (decf *turtle-top*))
      (progn
        (trace-log +log-warning+ "TURTLE STACK UNDERFLOW!")
        (make-turtle-state :origin (vec2 0.0 0.0) :angle 0.0))))

;; Create a new penrose tile structure
(defun create-penrose-l-system (draw-length)
  ;; TODO: Review constant values assignment on recreation?
  (make-penrose-l-system
   :steps 0
   :rule-w "YF++ZF4-XF[-YF4-WF]++"
   :rule-x "+YF--ZF[3-WF--XF]+"
   :rule-y "-WF++XF[+++YF++ZF]-"
   :rule-z "--YF++++WF[+ZF++++XF]--XF"
   :draw-length draw-length
   :theta 36.0                          ; Degrees
   :production (copy-seq "[X]++[X]++[X]++[X]++[X]")))

;; Build next penrose step
(defun build-production-step (ls)
  ;; NOTE: The production is limited to STR_MAX_SIZE - 1 characters, like the C string buffer
  (let ((new-production (make-array +str-max-size+ :element-type 'character :fill-pointer 0)))
    (flet ((strncat (rule remaining-space)
             (loop for c across rule
                   repeat remaining-space
                   do (vector-push c new-production))))
      (loop for step across (penrose-l-system-production ls)
            for remaining-space = (- +str-max-size+ (length new-production) 1)
            do (case step
                 (#\W (strncat (penrose-l-system-rule-w ls) remaining-space))
                 (#\X (strncat (penrose-l-system-rule-x ls) remaining-space))
                 (#\Y (strncat (penrose-l-system-rule-y ls) remaining-space))
                 (#\Z (strncat (penrose-l-system-rule-z ls) remaining-space))
                 (t (when (and (char/= step #\F) (< (length new-production) (1- +str-max-size+)))
                      (vector-push step new-production))))))

    (setf (penrose-l-system-draw-length ls) (* (penrose-l-system-draw-length ls) 0.5))
    (setf (penrose-l-system-production ls) (coerce new-production 'simple-string))))

;; Draw penrose tile lines
(defun draw-penrose-l-system (ls)
  (let* ((screen-center (vec2 (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0)))
         (turtle (make-turtle-state :origin (vec2 0.0 0.0) :angle -90.0))
         (repeats 1)
         (production (penrose-l-system-production ls))
         (production-length (length production)))

    (incf (penrose-l-system-steps ls) 12)

    (when (> (penrose-l-system-steps ls) production-length) (setf (penrose-l-system-steps ls) production-length))

    (dotimes (i (penrose-l-system-steps ls))
      (let ((step (char production i)))
        (cond ((char= step #\F)
               (dotimes (j repeats)
                 (let* ((start-pos-world (vcopy (turtle-state-origin turtle)))
                        (rad-angle (* +deg2rad+ (turtle-state-angle turtle)))
                        (origin (turtle-state-origin turtle)))
                   (incf (vx origin) (* (penrose-l-system-draw-length ls) (cos rad-angle)))
                   (incf (vy origin) (* (penrose-l-system-draw-length ls) (sin rad-angle)))
                   (let ((start-pos-screen (vec2 (+ (vx start-pos-world) (vx screen-center)) (+ (vy start-pos-world) (vy screen-center))))
                         (end-pos-screen (vec2 (+ (vx origin) (vx screen-center)) (+ (vy origin) (vy screen-center)))))
                     (draw-line-ex start-pos-screen end-pos-screen 2.0 (fade +black+ 0.2)))))

               (setf repeats 1))
              ((char= step #\+)
               (dotimes (j repeats) (incf (turtle-state-angle turtle) (penrose-l-system-theta ls)))

               (setf repeats 1))
              ((char= step #\-)
               (dotimes (j repeats) (incf (turtle-state-angle turtle) (- (penrose-l-system-theta ls))))

               (setf repeats 1))
              ((char= step #\[) (push-turtle-state turtle))
              ((char= step #\]) (setf turtle (pop-turtle-state)))
              ((char<= #\0 step #\9) (setf repeats (- (char-code step) 48))))))

    (setf *turtle-top* -1)))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags +flag-msaa-4x-hint+)
    (init-window screen-width screen-height "raylib [shapes] example - penrose tile")

    (let* ((draw-length 460.0)
           (min-generations 0)
           (max-generations 4)
           (generations 0)

           ;; Initializee new penrose tile
           (ls (create-penrose-l-system (* draw-length (/ generations (float max-generations))))))

      (dotimes (i generations) (build-production-step ls))

      (set-target-fps 120)              ; Set our game to run at 120 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((rebuild nil))

                 (cond ((is-key-pressed +key-up+)
                        (when (< generations max-generations)
                          (incf generations)
                          (setf rebuild t)))
                       ((is-key-pressed +key-down+)
                        (when (> generations min-generations)
                          (decf generations)
                          (when (> generations 0) (setf rebuild t)))))

                 (when rebuild
                   (setf ls (create-penrose-l-system (* draw-length (/ generations (float max-generations)))))
                   (dotimes (i generations) (build-production-step ls))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (when (> generations 0) (draw-penrose-l-system ls))

               (draw-text "penrose l-system" 10 10 20 +darkgray+)
               (draw-text "press up or down to change generations" 10 30 20 +darkgray+)
               (draw-text (text-format "generations: %d" generations) 10 50 20 +darkgray+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
