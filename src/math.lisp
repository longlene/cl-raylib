(in-package #:cl-raylib)

;;; Math constants
(defconstant +pi+ 3.14159265358979323846)
(defconstant +deg2rad+ (/ +pi+ 180.0))
(defconstant +rad2deg+ (/ 180.0 +pi+))
(defconstant +epsilon+ 0.000001)

;;; Utility functions

(defun degrees-to-radians (degrees)
  "Convert degrees to radians"
  (* degrees +deg2rad+))

(defun radians-to-degrees (radians)
  "Convert radians to degrees"
  (* radians +rad2deg+))

(defun clamp-angle (angle)
  "Clamp angle to [-PI, PI] range"
  (cond
    ((> angle +pi+) (- angle (* 2 +pi+)))
    ((< angle (- +pi+)) (+ angle (* 2 +pi+)))
    (t angle)))
