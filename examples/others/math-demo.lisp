;;;; Math functions demo for cl-raylib
;;;; This demonstrates the vector math functionality

(require :cl-raylib)

(defpackage :cl-raylib-math-demo
  (:use :cl :cl-raylib))

(in-package :cl-raylib-math-demo)

(defun math-demo ()
  "Demonstrate vector math operations"
  
  ;; Vector2 operations
  (format t "=== Vector2 Math Demo ===~%")
  
  (let ((v1 (vec2 3.0 4.0))
        (v2 (vec2 1.0 2.0)))
    
    (format t "v1: ~a~%" v1)
    (format t "v2: ~a~%" v2)
    (format t "v1 + v2: ~a~%" (vector2-add v1 v2))
    (format t "v1 - v2: ~a~%" (vector2-subtract v1 v2))
    (format t "v1 * 2.0: ~a~%" (vector2-scale v1 2.0))
    (format t "v1 length: ~a~%" (vector2-length v1))
    (format t "v1 normalized: ~a~%" (vector2-normalize v1))
    (format t "v1 dot v2: ~a~%" (vector2-dot-product v1 v2))
    (format t "distance v1 to v2: ~a~%" (vector2-distance v1 v2))
    )
  
  ;; Vector3 operations
  (format t "~%=== Vector3 Math Demo ===~%")
  
  (let ((v1 (vec3 1.0 0.0 0.0))
        (v2 (vec3 0.0 1.0 0.0)))
    
    (format t "v1: ~a~%" v1)
    (format t "v2: ~a~%" v2)
    (format t "v1 + v2: ~a~%" (vector3-add v1 v2))
    (format t "v1 cross v2: ~a~%" (vector3-cross-product v1 v2))
    (format t "v1 dot v2: ~a~%" (vector3-dot-product v1 v2))
    )
  
  ;; Color operations
  (format t "~%=== Color Demo ===~%")
  
  (let ((red-color +red+)
        (custom-color (make-color 128 64 192 255)))
    
    (format t "Red color: ~a~%" red-color)
    (format t "Custom color: ~a~%" custom-color)
    (format t "Red normalized: ~a~%" (color-normalize red-color))
    (format t "Red with 50%% alpha: ~a~%" (color-fade red-color 0.5))
    
    ;; HSV conversion
    (let ((hsv (color-to-hsv red-color)))
      (format t "Red in HSV: ~a~%" hsv)
      (format t "HSV back to RGB: ~a~%" (color-from-hsv (vx hsv) (vy hsv) (vz hsv))))
    ))

;; Run the demo
(math-demo)