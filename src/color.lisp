(in-package #:cl-raylib)

;;; Color type and constructors
;; Color is represented as a list (r g b a) with values 0-255
(defun make-color (r g b &optional (a 255))
  "Create a color with RGBA components (0-255)"
  (list (clamp (round r) 0 255)
        (clamp (round g) 0 255) 
        (clamp (round b) 0 255)
        (clamp (round a) 0 255)))

;; Color component accessors
(defun color-r (color) (first color))
(defun color-g (color) (second color))
(defun color-b (color) (third color))
(defun color-a (color) (fourth color))

;;; Predefined colors (following raylib constants)
(defparameter +black+     (list 0 0 0 255))
(defparameter +white+     (list 255 255 255 255))
(defparameter +red+       (list 230 41 55 255))
(defparameter +green+     (list 0 228 48 255))
(defparameter +blue+      (list 0 121 241 255))
(defparameter +yellow+    (list 253 249 0 255))
(defparameter +magenta+   (list 255 0 255 255))
(defparameter +cyan+      (list 0 255 255 255))
(defparameter +gray+      (list 130 130 130 255))
(defparameter +lightgray+ (list 200 200 200 255))
(defparameter +darkgray+  (list 80 80 80 255))
(defparameter +raywhite+  (list 245 245 245 255))
(defparameter +blank+     (list 0 0 0 0))

;; Additional raylib colors for compatibility
(defparameter +maroon+    (list 190 33 55 255))
(defparameter +orange+    (list 255 161 0 255))
(defparameter +darkgreen+ (list 0 117 44 255))
(defparameter +darkblue+  (list 0 82 172 255))
(defparameter +skyblue+   (list 135 206 235 255))
(defparameter +purple+    (list 200 122 255 255))
(defparameter +lime+      (list 0 158 47 255))
(defparameter +beige+     (list 211 176 131 255))
(defparameter +brown+     (list 127 106 79 255))
(defparameter +gold+      (list 255 203 0 255))
(defparameter +violet+    (list 135 60 190 255))
(defparameter +darkpurple+ (list 112 31 126 255))

;;; Color utility functions
(defun color-normalize (color)
  "Convert color from 0-255 range to 0.0-1.0 range for OpenGL"
  (list (/ (color-r color) 255.0)
        (/ (color-g color) 255.0)
        (/ (color-b color) 255.0)
        (/ (color-a color) 255.0)))

(defun set-gl-color (color)
  "Set OpenGL color from cl-raylib color"
  (let* ((actual-color (keyword-to-color color))
         (normalized (color-normalize actual-color)))
    (gl:color (first normalized) (second normalized) 
              (third normalized) (fourth normalized))))

(defun color-from-normalized (r g b &optional (a 1.0))
  "Create color from normalized 0.0-1.0 values"
  (make-color (* r 255) (* g 255) (* b 255) (* a 255)))

(defun color-alpha (color alpha)
  "Create color with new alpha value (0-255)"
  (list (color-r color)
        (color-g color)
        (color-b color)
        (clamp (round alpha) 0 255)))

(defun color-fade (color alpha)
  "Create color with fade effect (alpha 0.0-1.0)"
  (color-alpha color (* alpha 255)))

(defun fade (color alpha)
  "Alias for color-fade (matches raylib Fade function)"
  (color-fade color alpha))

;;; HSV conversion functions
(defun color-from-hsv (hue saturation value)
  "Create color from HSV values (hue: 0-360, saturation: 0.0-1.0, value: 0.0-1.0)"
  (let* ((h (mod hue 360.0))
         (s (clamp saturation 0.0 1.0))
         (v (clamp value 0.0 1.0))
         (c (* v s))
         (x (* c (- 1.0 (abs (- (mod (/ h 60.0) 2.0) 1.0)))))
         (m (- v c)))
    (multiple-value-bind (r g b)
        (cond
          ((< h 60)   (values c x 0))
          ((< h 120)  (values x c 0))
          ((< h 180)  (values 0 c x))
          ((< h 240)  (values 0 x c))
          ((< h 300)  (values x 0 c))
          (t          (values c 0 x)))
      (make-color (* (+ r m) 255)
                  (* (+ g m) 255)
                  (* (+ b m) 255)))))

(defun color-to-hsv (color)
  "Convert color to HSV values, returns (hue saturation value)"
  (let* ((r (/ (color-r color) 255.0))
         (g (/ (color-g color) 255.0))
         (b (/ (color-b color) 255.0))
         (max-val (max r g b))
         (min-val (min r g b))
         (delta (- max-val min-val)))
    (list
     ;; Hue
     (cond
       ((= delta 0) 0.0)
       ((= max-val r) (* 60.0 (mod (/ (- g b) delta) 6.0)))
       ((= max-val g) (* 60.0 (+ (/ (- b r) delta) 2.0)))
       (t             (* 60.0 (+ (/ (- r g) delta) 4.0))))
     ;; Saturation
     (if (= max-val 0) 0.0 (/ delta max-val))
     ;; Value
     max-val)))

;;; Color keyword mapping
(defun keyword-to-color (color-keyword)
  "Convert keyword color names to color values for compatibility"
  (case color-keyword
    (:black +black+)
    (:white +white+)
    (:red +red+)
    (:green +green+)
    (:blue +blue+)
    (:yellow +yellow+)
    (:magenta +magenta+)
    (:cyan +cyan+)
    (:gray +gray+)
    (:lightgray +lightgray+)
    (:darkgray +darkgray+)
    (:raywhite +raywhite+)
    (:blank +blank+)
    (:maroon +maroon+)
    (:orange +orange+)
    (:darkgreen +darkgreen+)
    (:darkblue +darkblue+)
    (:skyblue +skyblue+)
    (:purple +purple+)
    (:lime +lime+)
    (:beige +beige+)
    (:brown +brown+)
    (:gold +gold+)
    (:violet +violet+)
    (t (if (listp color-keyword)
           color-keyword  ; Already a color list
           +white+))))    ; Default fallback

;;; Color blending
(defun color-alpha-blend (dst src tint)
  "Alpha blend colors"
  (let* ((src-norm (color-normalize src))
         (dst-norm (color-normalize dst))
         (tint-norm (color-normalize tint))
         (alpha (* (fourth src-norm) (fourth tint-norm))))
    (if (> alpha +epsilon+)
        (color-from-normalized
         (/ (+ (* (first dst-norm) (fourth dst-norm) (- 1.0 alpha))
               (* (first src-norm) (first tint-norm) alpha))
            (+ (* (fourth dst-norm) (- 1.0 alpha)) alpha))
         (/ (+ (* (second dst-norm) (fourth dst-norm) (- 1.0 alpha))
               (* (second src-norm) (second tint-norm) alpha))
            (+ (* (fourth dst-norm) (- 1.0 alpha)) alpha))
         (/ (+ (* (third dst-norm) (fourth dst-norm) (- 1.0 alpha))
               (* (third src-norm) (third tint-norm) alpha))
            (+ (* (fourth dst-norm) (- 1.0 alpha)) alpha))
         (+ (* (fourth dst-norm) (- 1.0 alpha)) alpha))
        dst)))