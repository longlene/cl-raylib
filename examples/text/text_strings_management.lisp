;;;; raylib [text] example - strings management
;;;;
;;;; Example complexity rating: [★★★☆] 3/4
;;;;
;;;; Example originally created with raylib 6.0, last time updated with raylib 6.0
;;;;
;;;; Example contributed by David Buzatto (@davidbuzatto) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2025 David Buzatto (@davidbuzatto)
;;;; Common Lisp port of raylib/examples/text/text_strings_management.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-strings-management
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-strings-management)

(defconstant +max-text-length+ 100)
(defconstant +max-text-particles+ 100)
(defconstant +font-size+ 30)

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
(defstruct text-particle
  (text "")
  (rect (make-rectangle))               ; Boundary
  (vel (vec2 0.0 0.0))                  ; Velocity
  (ppos (vec2 0.0 0.0))                 ; Previous position
  (padding 0.0)
  (border-width 0.0)
  (friction 0.0)
  (elasticity 0.0)
  (color +blank+)
  (grabbed nil))

;; NOTE: TPS is the particles array and the C TextParticle pointers into it are slot indices;
;; the particle count is kept in *PARTICLE-COUNT* (C passes int *particleCount)
(defvar *particle-count* 0)

;;----------------------------------------------------------------------------------
;; Module Functions Definition
;;----------------------------------------------------------------------------------
(defun create-text-particle (text x y color)
  (let ((tp (make-text-particle
             :text ""
             :rect (make-rectangle :x x :y y :width 30.0 :height 30.0)
             :vel (let* ((vx (float (get-random-value -200 200)))
                         (vy (float (get-random-value -200 200))))
                    (vec2 vx vy))
             :ppos (vec2 0.0 0.0)
             :padding 5.0
             :border-width 5.0
             :friction 0.99
             :elasticity 0.9
             :color color
             :grabbed nil)))

    (setf (text-particle-text tp) (subseq text 0 (min (length text) (1- +max-text-length+))))
    (setf (rectangle-width (text-particle-rect tp)) (+ (measure-text (text-particle-text tp) +font-size+) (* (text-particle-padding tp) 2))
          (rectangle-height (text-particle-rect tp)) (+ +font-size+ (* (text-particle-padding tp) 2)))
    tp))

(defun random-color ()
  (let* ((r (get-random-value 0 255))
         (g (get-random-value 0 255))
         (b (get-random-value 0 255)))
    (list r g b 255)))

(defun prepare-first-text-particle (text tps)
  (setf (aref tps 0) (create-text-particle text (/ (get-screen-width) 2.0) (/ (get-screen-height) 2.0) +raywhite+))
  (setf *particle-count* 1))

(defun realocate-text-particles (tps particle-pos)
  (loop for i from (1+ particle-pos) below *particle-count*
        do (setf (aref tps (1- i)) (aref tps i)))
  (decf *particle-count*))

(defun slice-text-particle (tp particle-pos slice-length tps)
  (let ((length (length (text-particle-text tp))))

    (when (and (> length 1) (< (+ *particle-count* length) +max-text-particles+))
      (loop for i from 0 below length by slice-length
            do (let* ((text (if (= slice-length 1)
                                (string (char (text-particle-text tp) i))
                                (text-subtext (text-particle-text tp) i slice-length)))
                      (color (random-color)))
                 (setf (aref tps *particle-count*)
                       (create-text-particle text
                                             (+ (rectangle-x (text-particle-rect tp)) (/ (* i (rectangle-width (text-particle-rect tp))) length))
                                             (rectangle-y (text-particle-rect tp))
                                             color))
                 (incf *particle-count*)))
      (realocate-text-particles tps particle-pos))))

(defun slice-text-particle-by-char (tp char-to-slice tps)
  (multiple-value-bind (tokens token-count) (text-split (text-particle-text tp) char-to-slice)

    (when (> token-count 1)
      (let ((text-length (length (text-particle-text tp))))
        (dotimes (i text-length)
          (when (char= (char (text-particle-text tp) i) char-to-slice)
            (let ((color (random-color)))
              (setf (aref tps *particle-count*)
                    (create-text-particle (string char-to-slice)
                                          (rectangle-x (text-particle-rect tp))
                                          (rectangle-y (text-particle-rect tp))
                                          color))
              (incf *particle-count*)))))
      (loop for token in tokens
            for i from 0
            do (let ((token-length (length token))
                     (color (random-color)))
                 (setf (aref tps *particle-count*)
                       (create-text-particle token
                                             (+ (rectangle-x (text-particle-rect tp)) (/ (* i (rectangle-width (text-particle-rect tp))) token-length))
                                             (rectangle-y (text-particle-rect tp))
                                             color))
                 (incf *particle-count*)))
      (when (> token-count 0)
        (realocate-text-particles tps 0)))))

(defun shatter-text-particle (tp particle-pos tps)
  (slice-text-particle tp particle-pos 1 tps))

;; NOTE: GRABBED and TARGET are slot indices
(defun glue-text-particles (grabbed target tps)
  (let ((p1 -1)
        (p2 -1))

    (dotimes (i *particle-count*)
      (when (= i grabbed) (setf p1 i))
      (when (= i target) (setf p2 i)))

    (when (and (/= p1 -1) (/= p2 -1))
      (let ((tp (create-text-particle (text-format "%s%s" (text-particle-text (aref tps grabbed)) (text-particle-text (aref tps target)))
                                      (rectangle-x (text-particle-rect (aref tps grabbed)))
                                      (rectangle-y (text-particle-rect (aref tps grabbed)))
                                      +raywhite+)))
        (setf (text-particle-grabbed tp) t)
        (setf (aref tps *particle-count*) tp)
        (incf *particle-count*)
        (setf (text-particle-grabbed (aref tps grabbed)) nil)
        (if (< p1 p2)
            (progn
              (realocate-text-particles tps p2)
              (realocate-text-particles tps p1))
            (progn
              (realocate-text-particles tps p1)
              (realocate-text-particles tps p2)))))))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (init-window screen-width screen-height "raylib [text] example - strings management")

    (let ((text-particles (make-array +max-text-particles+ :initial-element nil))
          (grabbed-text-particle nil)   ; Slot index of the grabbed particle
          (press-offset (vec2 0.0 0.0)))

      (dotimes (i +max-text-particles+) (setf (aref text-particles i) (make-text-particle)))
      (setf *particle-count* 0)

      (prepare-first-text-particle "raylib => fun videogames programming!" text-particles)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main game loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               (let ((delta (get-frame-time))
                     (mouse-pos (get-mouse-position)))

                 ;; Checks if a text particle was grabbed
                 (when (is-mouse-button-pressed +mouse-button-left+)
                   (loop for i from (1- *particle-count*) downto 0
                         do (let ((tp (aref text-particles i)))
                              (setf (vx press-offset) (- (vx mouse-pos) (rectangle-x (text-particle-rect tp)))
                                    (vy press-offset) (- (vy mouse-pos) (rectangle-y (text-particle-rect tp))))
                              (when (check-collision-point-rec mouse-pos (text-particle-rect tp))
                                (setf (text-particle-grabbed tp) t
                                      grabbed-text-particle i)
                                (return)))))

                 ;; Releases any text particle the was grabbed
                 (when (is-mouse-button-released +mouse-button-left+)
                   (when grabbed-text-particle
                     (setf (text-particle-grabbed (aref text-particles grabbed-text-particle)) nil
                           grabbed-text-particle nil)))

                 ;; Slice os shatter a text particle
                 (when (is-mouse-button-pressed +mouse-button-right+)
                   (loop for i from (1- *particle-count*) downto 0
                         do (let ((tp (aref text-particles i)))
                              (when (check-collision-point-rec mouse-pos (text-particle-rect tp))
                                (if (is-key-down +key-left-shift+)
                                    (shatter-text-particle tp i text-particles)
                                    (slice-text-particle tp i (floor (length (text-particle-text tp)) 2) text-particles))
                                (return)))))

                 ;; Shake text particles
                 (when (is-mouse-button-pressed +mouse-button-middle+)
                   (dotimes (i *particle-count*)
                     (unless (text-particle-grabbed (aref text-particles i))
                       (setf (text-particle-vel (aref text-particles i))
                             (let* ((vx (float (get-random-value -2000 2000)))
                                    (vy (float (get-random-value -2000 2000))))
                               (vec2 vx vy))))))

                 ;; Reset using TextTo* functions
                 (when (is-key-pressed +key-one+) (prepare-first-text-particle "raylib => fun videogames programming!" text-particles))
                 (when (is-key-pressed +key-two+) (prepare-first-text-particle (text-to-upper "raylib => fun videogames programming!") text-particles))
                 (when (is-key-pressed +key-three+) (prepare-first-text-particle (text-to-lower "raylib => fun videogames programming!") text-particles))
                 (when (is-key-pressed +key-four+) (prepare-first-text-particle (text-to-pascal "raylib_fun_videogames_programming") text-particles))
                 (when (is-key-pressed +key-five+) (prepare-first-text-particle (text-to-snake "RaylibFunVideogamesProgramming") text-particles))
                 (when (is-key-pressed +key-six+) (prepare-first-text-particle (text-to-camel "raylib_fun_videogames_programming") text-particles))

                 ;; Slice by char pressed only when we have one text particle
                 (let* ((code (ldb (byte 8 0) (get-char-pressed))) ; C stores it in a char
                        (char-pressed (if (>= code 128) (- code 256) code)))
                   (when (and (>= char-pressed (char-code #\A)) (<= char-pressed (char-code #\z)) (= *particle-count* 1))
                     (slice-text-particle-by-char (aref text-particles 0) (code-char char-pressed) text-particles)))

                 ;; Updates each text particle state
                 (dotimes (i *particle-count*)
                   (let* ((tp (aref text-particles i))
                          (rect (text-particle-rect tp))
                          (vel (text-particle-vel tp)))

                     ;; The text particle is not grabbed
                     (if (not (text-particle-grabbed tp))
                         (progn
                           ;; text particle repositioning using the velocity
                           (incf (rectangle-x rect) (* (vx vel) delta))
                           (incf (rectangle-y rect) (* (vy vel) delta))

                           ;; Does the text particle hit the screen right boundary?
                           (cond ((>= (+ (rectangle-x rect) (rectangle-width rect)) screen-width)
                                  (setf (rectangle-x rect) (- screen-width (rectangle-width rect)) ; Text particle repositioning
                                        (vx vel) (* (- (vx vel)) (text-particle-elasticity tp)))) ; Elasticity makes the text particle lose 10% of its velocity on hit
                                 ;; Does the text particle hit the screen left boundary?
                                 ((<= (rectangle-x rect) 0)
                                  (setf (rectangle-x rect) 0.0
                                        (vx vel) (* (- (vx vel)) (text-particle-elasticity tp)))))

                           ;; The same for y axis
                           (cond ((>= (+ (rectangle-y rect) (rectangle-height rect)) screen-height)
                                  (setf (rectangle-y rect) (- screen-height (rectangle-height rect))
                                        (vy vel) (* (- (vy vel)) (text-particle-elasticity tp))))
                                 ((<= (rectangle-y rect) 0)
                                  (setf (rectangle-y rect) 0.0
                                        (vy vel) (* (- (vy vel)) (text-particle-elasticity tp)))))

                           ;; Friction makes the text particle lose 1% of its velocity each frame
                           (setf (vx vel) (* (vx vel) (text-particle-friction tp))
                                 (vy vel) (* (vy vel) (text-particle-friction tp))))
                         (let ((ppos (text-particle-ppos tp)))
                           ;; Text particle repositioning using the mouse position
                           (setf (rectangle-x rect) (- (vx mouse-pos) (vx press-offset))
                                 (rectangle-y rect) (- (vy mouse-pos) (vy press-offset)))

                           ;; While the text particle is grabbed, recalculates its velocity
                           (setf (vx vel) (/ (- (rectangle-x rect) (vx ppos)) delta)
                                 (vy vel) (/ (- (rectangle-y rect) (vy ppos)) delta)
                                 (vx ppos) (rectangle-x rect)
                                 (vy ppos) (rectangle-y rect))

                           ;; Glue text particles when dragging and pressing left ctrl
                           (when (is-key-down +key-left-control+)
                             (dotimes (j *particle-count*)
                               (when (and (/= j grabbed-text-particle) (text-particle-grabbed (aref text-particles grabbed-text-particle)))
                                 (when (check-collision-recs (text-particle-rect (aref text-particles grabbed-text-particle)) (text-particle-rect (aref text-particles j)))
                                   (glue-text-particles grabbed-text-particle j text-particles)
                                   (setf grabbed-text-particle (1- *particle-count*)))))))))))
               ;;----------------------------------------------------------------------------------

               ;; Draw
               ;;----------------------------------------------------------------------------------
               (begin-drawing)

               (clear-background +raywhite+)

               (dotimes (i *particle-count*)
                 (let* ((tp (aref text-particles i))
                        (rect (text-particle-rect tp))
                        (border (text-particle-border-width tp)))
                   (draw-rectangle-rec (make-rectangle :x (- (rectangle-x rect) border) :y (- (rectangle-y rect) border)
                                                       :width (+ (rectangle-width rect) (* border 2)) :height (+ (rectangle-height rect) (* border 2)))
                                       +black+)
                   (draw-rectangle-rec rect (text-particle-color tp))
                   (draw-text (text-particle-text tp) (truncate (+ (rectangle-x rect) (text-particle-padding tp))) (truncate (+ (rectangle-y rect) (text-particle-padding tp))) +font-size+ +black+)))

               (draw-text "grab a text particle by pressing with the mouse and throw it by releasing" 10 10 10 +darkgray+)
               (draw-text "slice a text particle by pressing it with the mouse right button" 10 30 10 +darkgray+)
               (draw-text "shatter a text particle keeping left shift pressed and pressing it with the mouse right button" 10 50 10 +darkgray+)
               (draw-text "glue text particles by grabbing than and keeping left control pressed" 10 70 10 +darkgray+)
               (draw-text "1 to 6 to reset" 10 90 10 +darkgray+)
               (draw-text "when you have only one text particle, you can slice it by pressing a char" 10 110 10 +darkgray+)
               (draw-text (text-format "TEXT PARTICLE COUNT: %d" *particle-count*) 10 (- (get-screen-height) 30) 20 +black+)

               (end-drawing))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (close-window))))                 ; Close window and OpenGL context

(main)
