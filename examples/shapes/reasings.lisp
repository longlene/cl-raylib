;;;; reasings - raylib easings library, based on Robert Penner library
;;;;
;;;; Useful easing functions for values animation
;;;;
;;;; How to use:
;;;; The four inputs tt,b,c,d are defined as follows:
;;;; tt = current time (in any unit measure, but same unit as duration)
;;;; b = starting value to interpolate
;;;; c = the total change in value of b that needs to occur
;;;; d = total time it should take to complete (duration)
;;;;
;;;; A port of Robert Penner's easing equations to C (http://robertpenner.com/easing/)
;;;;
;;;; Robert Penner License: open source under the BSD License.
;;;; Copyright (c) 2001 Robert Penner. All rights reserved.
;;;;
;;;; Copyright (c) 2015-2024 Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/shapes/reasings.h
;;;; (the C time argument t is named tt here, t is a CL constant)

(require :cl-raylib)

(defpackage #:reasings
  (:use #:cl)
  (:import-from #:raylib #:+pi+)
  (:export #:ease-linear-none #:ease-linear-in #:ease-linear-out #:ease-linear-in-out
           #:ease-sine-in #:ease-sine-out #:ease-sine-in-out
           #:ease-circ-in #:ease-circ-out #:ease-circ-in-out
           #:ease-cubic-in #:ease-cubic-out #:ease-cubic-in-out
           #:ease-quad-in #:ease-quad-out #:ease-quad-in-out
           #:ease-expo-in #:ease-expo-out #:ease-expo-in-out
           #:ease-back-in #:ease-back-out #:ease-back-in-out
           #:ease-bounce-out #:ease-bounce-in #:ease-bounce-in-out
           #:ease-elastic-in #:ease-elastic-out #:ease-elastic-in-out))
(in-package #:reasings)

(defmacro defease (name (&rest args) &body body)
  "Define an easing function on single-floats, like EASEDEF float Name(float t, float b, float c, float d)"
  `(defun ,name ,args
     (let ,(mapcar (lambda (a) `(,a (float ,a 1.0))) args)
       ,@body)))

;; Linear Easing functions
(defease ease-linear-none (tt b c d) (+ (/ (* c tt) d) b))   ; Ease: Linear
(defease ease-linear-in (tt b c d) (+ (/ (* c tt) d) b))     ; Ease: Linear In
(defease ease-linear-out (tt b c d) (+ (/ (* c tt) d) b))    ; Ease: Linear Out
(defease ease-linear-in-out (tt b c d) (+ (/ (* c tt) d) b)) ; Ease: Linear In Out

;; Sine Easing functions
(defease ease-sine-in (tt b c d) (+ (- (* c (cos (* (/ tt d) (/ +pi+ 2.0))))) c b))            ; Ease: Sine In
(defease ease-sine-out (tt b c d) (+ (* c (sin (* (/ tt d) (/ +pi+ 2.0)))) b))                 ; Ease: Sine Out
(defease ease-sine-in-out (tt b c d) (+ (* (/ (- c) 2.0) (- (cos (/ (* +pi+ tt) d)) 1.0)) b))  ; Ease: Sine In Out

;; Circular Easing functions
(defease ease-circ-in (tt b c d)        ; Ease: Circular In
  (setf tt (/ tt d))
  (+ (* (- c) (- (sqrt (- 1.0 (* tt tt))) 1.0)) b))
(defease ease-circ-out (tt b c d)       ; Ease: Circular Out
  (setf tt (- (/ tt d) 1.0))
  (+ (* c (sqrt (- 1.0 (* tt tt)))) b))
(defease ease-circ-in-out (tt b c d)    ; Ease: Circular In Out
  (if (< (setf tt (/ tt (/ d 2.0))) 1.0)
      (+ (* (/ (- c) 2.0) (- (sqrt (- 1.0 (* tt tt))) 1.0)) b)
      (progn
        (decf tt 2.0)
        (+ (* (/ c 2.0) (+ (sqrt (- 1.0 (* tt tt))) 1.0)) b))))

;; Cubic Easing functions
(defease ease-cubic-in (tt b c d)       ; Ease: Cubic In
  (setf tt (/ tt d))
  (+ (* c tt tt tt) b))
(defease ease-cubic-out (tt b c d)      ; Ease: Cubic Out
  (setf tt (- (/ tt d) 1.0))
  (+ (* c (+ (* tt tt tt) 1.0)) b))
(defease ease-cubic-in-out (tt b c d)   ; Ease: Cubic In Out
  (if (< (setf tt (/ tt (/ d 2.0))) 1.0)
      (+ (* (/ c 2.0) tt tt tt) b)
      (progn
        (decf tt 2.0)
        (+ (* (/ c 2.0) (+ (* tt tt tt) 2.0)) b))))

;; Quadratic Easing functions
(defease ease-quad-in (tt b c d)        ; Ease: Quadratic In
  (setf tt (/ tt d))
  (+ (* c tt tt) b))
(defease ease-quad-out (tt b c d)       ; Ease: Quadratic Out
  (setf tt (/ tt d))
  (+ (* (- c) tt (- tt 2.0)) b))
(defease ease-quad-in-out (tt b c d)    ; Ease: Quadratic In Out
  (if (< (setf tt (/ tt (/ d 2))) 1)
      (+ (* (/ c 2) (* tt tt)) b)
      (+ (* (/ (- c) 2.0) (- (* (- tt 1.0) (- tt 3.0)) 1.0)) b)))

;; Exponential Easing functions
(defease ease-expo-in (tt b c d)        ; Ease: Exponential In
  (if (= tt 0.0) b (+ (* c (expt 2.0 (* 10.0 (- (/ tt d) 1.0)))) b)))
(defease ease-expo-out (tt b c d)       ; Ease: Exponential Out
  (if (= tt d) (+ b c) (+ (* c (+ (- (expt 2.0 (/ (* -10.0 tt) d))) 1.0)) b)))
(defease ease-expo-in-out (tt b c d)    ; Ease: Exponential In Out
  (cond ((= tt 0.0) b)
        ((= tt d) (+ b c))
        ((< (setf tt (/ tt (/ d 2.0))) 1.0) (+ (* (/ c 2.0) (expt 2.0 (* 10.0 (- tt 1.0)))) b))
        (t (+ (* (/ c 2.0) (+ (- (expt 2.0 (* -10.0 (- tt 1.0)))) 2.0)) b))))

;; Back Easing functions
(defease ease-back-in (tt b c d)        ; Ease: Back In
  (let* ((s 1.70158)
         (post-fix (setf tt (/ tt d))))
    (+ (* c post-fix tt (- (* (+ s 1.0) tt) s)) b)))
(defease ease-back-out (tt b c d)       ; Ease: Back Out
  (let ((s 1.70158))
    (setf tt (- (/ tt d) 1.0))
    (+ (* c (+ (* tt tt (+ (* (+ s 1.0) tt) s)) 1.0)) b)))
(defease ease-back-in-out (tt b c d)    ; Ease: Back In Out
  (let ((s 1.70158))
    (if (< (setf tt (/ tt (/ d 2.0))) 1.0)
        (progn
          (setf s (* s 1.525))
          (+ (* (/ c 2.0) (* tt tt (- (* (+ s 1.0) tt) s))) b))
        (let ((post-fix (decf tt 2.0)))
          (setf s (* s 1.525))
          (+ (* (/ c 2.0) (+ (* post-fix tt (+ (* (+ s 1.0) tt) s)) 2.0)) b)))))

;; Bounce Easing functions
(defease ease-bounce-out (tt b c d)     ; Ease: Bounce Out
  (cond ((< (setf tt (/ tt d)) (/ 1.0 2.75))
         (+ (* c (* 7.5625 tt tt)) b))
        ((< tt (/ 2.0 2.75))
         (let ((post-fix (decf tt (/ 1.5 2.75))))
           (+ (* c (+ (* 7.5625 post-fix tt) 0.75)) b)))
        ((< tt (/ 2.5d0 2.75d0))        ; double comparison in C
         (let ((post-fix (decf tt (/ 2.25 2.75))))
           (+ (* c (+ (* 7.5625 post-fix tt) 0.9375)) b)))
        (t
         (let ((post-fix (decf tt (/ 2.625 2.75))))
           (+ (* c (+ (* 7.5625 post-fix tt) 0.984375)) b)))))
(defease ease-bounce-in (tt b c d)      ; Ease: Bounce In
  (+ (- c (ease-bounce-out (- d tt) 0.0 c d)) b))
(defease ease-bounce-in-out (tt b c d)  ; Ease: Bounce In Out
  (if (< tt (/ d 2.0))
      (+ (* (ease-bounce-in (* tt 2.0) 0.0 c d) 0.5) b)
      (+ (* (ease-bounce-out (- (* tt 2.0) d) 0.0 c d) 0.5) (* c 0.5) b)))

;; Elastic Easing functions
(defease ease-elastic-in (tt b c d)     ; Ease: Elastic In
  (cond ((= tt 0.0) b)
        ((= (setf tt (/ tt d)) 1.0) (+ b c))
        (t
         (let* ((p (* d 0.3))
                (a c)
                (s (/ p 4.0))
                (post-fix (* a (expt 2.0 (* 10.0 (decf tt 1.0))))))
           (+ (- (* post-fix (sin (/ (* (- (* tt d) s) (* 2.0 +pi+)) p)))) b)))))
(defease ease-elastic-out (tt b c d)    ; Ease: Elastic Out
  (cond ((= tt 0.0) b)
        ((= (setf tt (/ tt d)) 1.0) (+ b c))
        (t
         (let* ((p (* d 0.3))
                (a c)
                (s (/ p 4.0)))
           (+ (* a (expt 2.0 (* -10.0 tt)) (sin (/ (* (- (* tt d) s) (* 2.0 +pi+)) p))) c b)))))
(defease ease-elastic-in-out (tt b c d) ; Ease: Elastic In Out
  (cond ((= tt 0.0) b)
        ((= (setf tt (/ tt (/ d 2.0))) 2.0) (+ b c))
        (t
         (let* ((p (* d (* 0.3 1.5)))
                (a c)
                (s (/ p 4.0)))
           (if (< tt 1.0)
               (let ((post-fix (* a (expt 2.0 (* 10.0 (decf tt 1.0))))))
                 (+ (* -0.5 (* post-fix (sin (/ (* (- (* tt d) s) (* 2.0 +pi+)) p)))) b))
               (let ((post-fix (* a (expt 2.0 (* -10.0 (decf tt 1.0))))))
                 (+ (* post-fix (sin (/ (* (- (* tt d) s) (* 2.0 +pi+)) p)) 0.5) c b)))))))
