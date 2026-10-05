(in-package #:cl-raylib)

;;;; Color type, constants and the raylib color functions
;;;; NOTE: raylib defines the Color* functions in rtextures.c (Module Functions
;;;; Definition - Color/pixel related functions); they live here because shapes,
;;;; text and textures all depend on them.

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
(defparameter +lightgray+  (list 200 200 200 255) "Light Gray")
(defparameter +gray+       (list 130 130 130 255) "Gray")
(defparameter +darkgray+   (list 80 80 80 255)    "Dark Gray")
(defparameter +yellow+     (list 253 249 0 255)   "Yellow")
(defparameter +gold+       (list 255 203 0 255)   "Gold")
(defparameter +orange+     (list 255 161 0 255)   "Orange")
(defparameter +pink+       (list 255 109 194 255) "Pink")
(defparameter +red+        (list 230 41 55 255)   "Red")
(defparameter +maroon+     (list 190 33 55 255)   "Maroon")
(defparameter +green+      (list 0 228 48 255)    "Green")
(defparameter +lime+       (list 0 158 47 255)    "Lime")
(defparameter +darkgreen+  (list 0 117 44 255)    "Dark Green")
(defparameter +skyblue+    (list 102 191 255 255) "Sky Blue")
(defparameter +blue+       (list 0 121 241 255)   "Blue")
(defparameter +darkblue+   (list 0 82 172 255)    "Dark Blue")
(defparameter +purple+     (list 200 122 255 255) "Purple")
(defparameter +violet+     (list 135 60 190 255)  "Violet")
(defparameter +darkpurple+ (list 112 31 126 255)  "Dark Purple")
(defparameter +beige+      (list 211 176 131 255) "Beige")
(defparameter +brown+      (list 127 106 79 255)  "Brown")
(defparameter +darkbrown+  (list 76 63 47 255)    "Dark Brown")
(defparameter +white+      (list 255 255 255 255) "White")
(defparameter +black+      (list 0 0 0 255)       "Black")
(defparameter +blank+      (list 0 0 0 0)         "Blank (Transparent)")
(defparameter +magenta+    (list 255 0 255 255)   "Magenta")
(defparameter +raywhite+   (list 245 245 245 255) "My own White (raylib logo)")
(defparameter +cyan+       (list 0 255 255 255)   "Cyan (not a raylib color, kept for compatibility)")

;;; Color keyword mapping
(defun keyword-to-color (color-keyword)
  "Convert keyword color names to color values for compatibility"
  (case color-keyword
    (:lightgray +lightgray+)
    (:gray +gray+)
    (:darkgray +darkgray+)
    (:yellow +yellow+)
    (:gold +gold+)
    (:orange +orange+)
    (:pink +pink+)
    (:red +red+)
    (:maroon +maroon+)
    (:green +green+)
    (:lime +lime+)
    (:darkgreen +darkgreen+)
    (:skyblue +skyblue+)
    (:blue +blue+)
    (:darkblue +darkblue+)
    (:purple +purple+)
    (:violet +violet+)
    (:darkpurple +darkpurple+)
    (:beige +beige+)
    (:brown +brown+)
    (:darkbrown +darkbrown+)
    (:white +white+)
    (:black +black+)
    (:blank +blank+)
    (:magenta +magenta+)
    (:raywhite +raywhite+)
    (:cyan +cyan+)
    (t (if (listp color-keyword)
           color-keyword                ; Already a color list
           +white+))))                  ; Default fallback


(declaim (inline %u8))
(defun %u8 (x)
  "C (unsigned char) cast of a number: truncate toward zero, keep the low 8 bits"
  (logand (truncate x) #xff))


;;;----------------------------------------------------------------------------------
;;; Color/pixel related functions (raylib rtextures.c)
;;;----------------------------------------------------------------------------------

(defun color-is-equal (col1 col2)
  "Check if two colors are equal"
  (destructuring-bind (r1 g1 b1 a1) (keyword-to-color col1)
    (destructuring-bind (r2 g2 b2 a2) (keyword-to-color col2)
      (and (= r1 r2) (= g1 g2) (= b1 b2) (= a1 a2)))))

(defun fade (color alpha)
  "Get color with alpha applied, alpha goes from 0.0f to 1.0f"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (declare (ignore a))
    (list r g b (%u8 (* 255.0 (clamp alpha 0.0 1.0))))))

(defun color-fade (color alpha)
  "Alias of fade (not part of raylib)"
  (fade color alpha))

(defun color-to-int (color)
  "Get hexadecimal value for a Color (0xRRGGBBAA), as a signed 32-bit int like C"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (let ((u (logior (ash r 24) (ash g 16) (ash b 8) a)))
      (if (>= u #x80000000) (- u #x100000000) u))))

(defun color-normalize (color)
  "Get color normalized as float [0..1]"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (vec4 (/ r 255.0) (/ g 255.0) (/ b 255.0) (/ a 255.0))))

(defun color-from-normalized (normalized)
  "Get color from normalized values [0..1] (a Vector4, or a (r g b a) list)"
  (if (consp normalized)
      (destructuring-bind (x y z &optional (w 1.0)) normalized
        (list (%u8 (* x 255.0)) (%u8 (* y 255.0)) (%u8 (* z 255.0)) (%u8 (* w 255.0))))
      (list (%u8 (* (vx normalized) 255.0)) (%u8 (* (vy normalized) 255.0))
            (%u8 (* (vz normalized) 255.0)) (%u8 (* (vw normalized) 255.0)))))

;; NOTE: Hue is returned as degrees [0..360]
(defun color-to-hsv (color)
  "Get HSV values for a Color"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (declare (ignore a))
    (let* ((r (/ r 255.0)) (g (/ g 255.0)) (b (/ b 255.0))
           (min (min r g b))
           (max (max r g b))
           (delta (- max min)))
      (cond ((< delta 0.00001)
             (vec3 0.0 0.0 max))
            ;; NOTE: If max is 0, then r = g = b = 0, s = 0, h is undefined (raylib returns NAN)
            ((not (> max 0.0))
             (vec3 0.0 0.0 max))
            (t
             (let ((h (cond ((>= r max) (/ (- g b) delta))           ; Between yellow & magenta
                            ((>= g max) (+ 2.0 (/ (- b r) delta)))   ; Between cyan & yellow
                            (t (+ 4.0 (/ (- r g) delta))))))         ; Between magenta & cyan
               (setf h (* h 60.0))                                    ; Convert to degrees
               (when (< h 0.0) (incf h 360.0))
               (vec3 h (/ delta max) max)))))))

;; Implementation reference: https://en.wikipedia.org/wiki/HSL_and_HSV#Alternative_HSV_conversion
;; NOTE: Color->HSV->Color conversion will not yield exactly the same color due to rounding errors
;; Hue is provided in degrees: [0..360]
;; Saturation/Value are provided normalized: [0.0f..1.0f]
(defun color-from-hsv (hue saturation value)
  "Get a Color from HSV values, hue [0..360], saturation/value [0..1]"
  (flet ((channel (n)
           (let* ((k (%fmodf (+ n (/ hue 60.0)) 6))
                  (k (min k (- 4.0 k) 1))
                  (k (max k 0)))
             (%u8 (* (- value (* value saturation k)) 255.0)))))
    (list (channel 5.0) (channel 3.0) (channel 1.0) 255)))

(defun color-tint (color tint)
  "Get color multiplied with another color"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (destructuring-bind (tr tg tb ta) (keyword-to-color tint)
      (list (%u8 (floor (* r tr) 255)) (%u8 (floor (* g tg) 255))
            (%u8 (floor (* b tb) 255)) (%u8 (floor (* a ta) 255))))))

(defun color-brightness (color factor)
  "Get color with brightness correction, brightness factor goes from -1.0f to 1.0f"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (let ((factor (clamp factor -1.0 1.0))
          (red (float r)) (green (float g)) (blue (float b)))
      (if (< factor 0.0)
          (let ((factor (+ 1.0 factor)))
            (setf red (* red factor) green (* green factor) blue (* blue factor)))
          (setf red (+ (* (- 255 red) factor) red)
                green (+ (* (- 255 green) factor) green)
                blue (+ (* (- 255 blue) factor) blue)))
      (list (%u8 red) (%u8 green) (%u8 blue) a))))

;; NOTE: Contrast values between -1.0f and 1.0f
(defun color-contrast (color contrast)
  "Get color with contrast correction"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (let* ((contrast (+ 1.0 (clamp contrast -1.0 1.0)))
           (contrast (* contrast contrast)))
      (flet ((adjust (c)
               (%u8 (clamp (* (+ (* (- (/ c 255.0) 0.5) contrast) 0.5) 255) 0 255))))
        (list (adjust r) (adjust g) (adjust b) a)))))

(defun color-alpha (color alpha)
  "Get color with alpha applied, alpha goes from 0.0f to 1.0f"
  (fade color alpha))

(defun color-alpha-blend (dst src tint)
  "Get src alpha-blended into dst color with tint"
  (destructuring-bind (dr dg db da) (keyword-to-color dst)
    (destructuring-bind (sr sg sb sa) (keyword-to-color src)
      (destructuring-bind (tr tg tb ta) (keyword-to-color tint)
        ;; Apply color tint to source color
        (let ((sr (%u8 (ash (* sr (1+ tr)) -8)))
              (sg (%u8 (ash (* sg (1+ tg)) -8)))
              (sb (%u8 (ash (* sb (1+ tb)) -8)))
              (sa (%u8 (ash (* sa (1+ ta)) -8))))
          (cond ((= sa 0) (list dr dg db da))
                ((= sa 255) (list sr sg sb sa))
                (t
                 ;; Shifting by 8 (dividing by 256), so need to take that excess into account
                 (let* ((alpha (1+ sa))
                        (ra (%u8 (ash (+ (* alpha 256) (* da (- 256 alpha))) -8))))
                   (if (> ra 0)
                       (flet ((mix (s d)
                                (%u8 (ash (floor (+ (* s alpha 256) (* d da (- 256 alpha))) ra) -8))))
                         (list (mix sr dr) (mix sg dg) (mix sb db) ra))
                       (list 255 255 255 ra))))))))))

(defun color-lerp (color1 color2 factor)
  "Get color lerp interpolation between two colors, factor [0.0f..1.0f]"
  (let ((factor (clamp factor 0.0 1.0)))
    (mapcar (lambda (c1 c2) (%u8 (+ (* (- 1.0 factor) c1) (* factor c2))))
            (keyword-to-color color1) (keyword-to-color color2))))

(defun get-color (hex-value)
  "Get a Color structure from hexadecimal value (0xRRGGBBAA)"
  (let ((hex (logand hex-value #xffffffff)))
    (list (ldb (byte 8 24) hex) (ldb (byte 8 16) hex) (ldb (byte 8 8) hex) (ldb (byte 8 0) hex))))
