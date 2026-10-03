(in-package #:cl-raylib)

;;;===================================================================================
;;; raymath - Math functions to work with Vector2, Vector3, Matrix and Quaternions
;;; Port of raylib/src/raymath.h
;;;
;;; Types:
;;;   Vector2/3/4 -> 3d-vectors vec2/vec3/vec4
;;;   Quaternion  -> vec4 (x y z w), same as raylib (typedef Vector4 Quaternion)
;;;   Matrix      -> 3d-matrices mat4 holding the matrix in standard math layout,
;;;                  same as cl-raylib.cffi: raylib field mN is
;;;                  (mcref4 mat (mod N 4) (floor N 4)), so C struct initializers
;;;                  (written row by row: m0 m4 m8 m12 ...) map directly to MAT.
;;;   float3/float16 -> (simple-array single-float (3)/(16))
;;;
;;; Functions with C out-parameters return multiple values instead:
;;;   Vector3OrthoNormalize, QuaternionToAxisAngle, MatrixDecompose
;;;===================================================================================

;;; Defines and Macros
(defconstant +pi+ 3.14159265358979323846)
(defconstant +epsilon+ 0.000001)
(defconstant +deg2rad+ (/ +pi+ 180.0))
(defconstant +rad2deg+ (/ 180.0 +pi+))

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

;; C sqrtf(): NaN for negative or NaN arguments
;; NOTE: CL SQRT returns a complex for negative arguments, and SBCL rejects a NaN result
(declaim (inline %sqrtf))
(defun %sqrtf (x)
  (let ((x (float x 1.0)))
    (if (or (sb-ext:float-nan-p x) (minusp x))
        (sb-kernel:make-single-float -4194304) ; -NaN, as x86-64 sqrtss
        (sqrt x))))

;; Matrix field access by raylib index: (%m mat 12) <=> mat.m12
(defmacro %m (mat index)
  `(mcref4 ,mat ,(mod index 4) ,(floor index 4)))

;; Bind PREFIX0..PREFIX15 to the raylib fields of MAT
(defmacro %with-matrix ((prefix mat) &body body)
  (let ((m (gensym "MAT"))
        (names (loop for i below 16 collect (alexandria:symbolicate prefix (princ-to-string i)))))
    `(let* ((,m ,mat)
            ,@(loop for name in names
                    for i from 0
                    collect `(,name (mcref4 ,m ,(mod i 4) ,(floor i 4)))))
       (declare (ignorable ,@names))
       ,@body)))

;; Build a Matrix from fields given in raylib index order m0, m1, ..., m15
(defun %matrix (m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)
  (mat m0 m4 m8 m12
       m1 m5 m9 m13
       m2 m6 m10 m14
       m3 m7 m11 m15))

(declaim (inline %f))
(defun %f (x) (float x 1.0))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Utils math
;;;----------------------------------------------------------------------------------

;; Clamp float value: alexandria:clamp (imported as CLAMP)

(defun lerp (start end amount)
  "Calculate linear interpolation between two floats"
  (+ start (* amount (- end start))))

(defun normalize (value start end)
  "Normalize input value within input range"
  (/ (- value start) (- end start)))

(defun remap (value input-start input-end output-start output-end)
  "Remap input value within input range to output range"
  (+ (* (/ (- value input-start) (- input-end input-start))
        (- output-end output-start))
     output-start))

(defun wrap (value min max)
  "Wrap input value from min to max"
  (- value (* (- max min) (ffloor (/ (- value min) (- max min))))))

(defun float-equals (x y)
  "Check whether two given floats are almost equal"
  (<= (abs (- x y)) (* +epsilon+ (max 1.0 (max (abs x) (abs y))))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Vector2 math
;;;----------------------------------------------------------------------------------

(defun vector2-zero ()
  "Vector with components value 0.0f"
  (vec2 0.0 0.0))

(defun vector2-one ()
  "Vector with components value 1.0f"
  (vec2 1.0 1.0))

(defun vector2-add (v1 v2)
  "Add two vectors (v1 + v2)"
  (vec2 (+ (vx v1) (vx v2)) (+ (vy v1) (vy v2))))

(defun vector2-add-value (v add)
  "Add vector and float value"
  (vec2 (+ (vx v) add) (+ (vy v) add)))

(defun vector2-subtract (v1 v2)
  "Subtract two vectors (v1 - v2)"
  (vec2 (- (vx v1) (vx v2)) (- (vy v1) (vy v2))))

(defun vector2-subtract-value (v sub)
  "Subtract vector by float value"
  (vec2 (- (vx v) sub) (- (vy v) sub)))

(defun vector2-length (v)
  "Calculate vector length"
  (%sqrtf (+ (* (vx v) (vx v)) (* (vy v) (vy v)))))

(defun vector2-length-sqr (v)
  "Calculate vector square length"
  (+ (* (vx v) (vx v)) (* (vy v) (vy v))))

(defun vector2-dot-product (v1 v2)
  "Calculate two vectors dot product"
  (+ (* (vx v1) (vx v2)) (* (vy v1) (vy v2))))

(defun vector2-cross-product (v1 v2)
  "Calculate two vectors cross product"
  (- (* (vx v1) (vy v2)) (* (vy v1) (vx v2))))

(defun vector2-distance (v1 v2)
  "Calculate distance between two vectors"
  (%sqrtf (vector2-distance-sqr v1 v2)))

(defun vector2-distance-sqr (v1 v2)
  "Calculate square distance between two vectors"
  (+ (* (- (vx v1) (vx v2)) (- (vx v1) (vx v2)))
     (* (- (vy v1) (vy v2)) (- (vy v1) (vy v2)))))

(defun vector2-angle (v1 v2)
  "Calculate the signed angle from v1 to v2, relative to the origin (0, 0)
NOTE: Coordinate system convention: positive X right, positive Y down
positive angles appear clockwise, and negative angles appear counterclockwise"
  (let ((dot (+ (* (vx v1) (vx v2)) (* (vy v1) (vy v2))))
        (det (- (* (vx v1) (vy v2)) (* (vy v1) (vx v2)))))
    (atan det dot)))

(defun vector2-line-angle (start end)
  "Calculate angle defined by a two vectors line
NOTE: Parameters need to be normalized
Current implementation should be aligned with glm::angle"
  ;; TODO(10/9/2023): Currently angles move clockwise, determine if this is wanted behavior
  (- (atan (- (vy end) (vy start)) (- (vx end) (vx start)))))

(defun vector2-scale (v scale)
  "Scale vector (multiply by value)"
  (vec2 (* (vx v) scale) (* (vy v) scale)))

(defun vector2-multiply (v1 v2)
  "Multiply vector by vector"
  (vec2 (* (vx v1) (vx v2)) (* (vy v1) (vy v2))))

(defun vector2-negate (v)
  "Negate vector"
  (vec2 (- (vx v)) (- (vy v))))

(defun vector2-divide (v1 v2)
  "Divide vector by vector"
  (vec2 (/ (vx v1) (vx v2)) (/ (vy v1) (vy v2))))

(defun vector2-normalize (v)
  "Normalize provided vector"
  (let ((length (%sqrtf (+ (* (vx v) (vx v)) (* (vy v) (vy v))))))
    (if (> length 0)
        (let ((ilength (/ 1.0 length)))
          (vec2 (* (vx v) ilength) (* (vy v) ilength)))
        (vec2 0.0 0.0))))

(defun vector2-transform (v mat)
  "Transforms a Vector2 by a given Matrix"
  (let ((x (vx v))
        (y (vy v))
        (z 0.0))
    (vec2 (+ (* (%m mat 0) x) (* (%m mat 4) y) (* (%m mat 8) z) (%m mat 12))
          (+ (* (%m mat 1) x) (* (%m mat 5) y) (* (%m mat 9) z) (%m mat 13)))))

(defun vector2-lerp (v1 v2 amount)
  "Calculate linear interpolation between two vectors"
  (vec2 (+ (vx v1) (* amount (- (vx v2) (vx v1))))
        (+ (vy v1) (* amount (- (vy v2) (vy v1))))))

(defun vector2-reflect (v normal)
  "Calculate reflected vector to normal"
  (let ((dot-product (+ (* (vx v) (vx normal)) (* (vy v) (vy normal)))))
    (vec2 (- (vx v) (* (* 2.0 (vx normal)) dot-product))
          (- (vy v) (* (* 2.0 (vy normal)) dot-product)))))

(defun vector2-min (v1 v2)
  "Get min value for each pair of components"
  (vec2 (min (vx v1) (vx v2)) (min (vy v1) (vy v2))))

(defun vector2-max (v1 v2)
  "Get max value for each pair of components"
  (vec2 (max (vx v1) (vx v2)) (max (vy v1) (vy v2))))

(defun vector2-rotate (v angle)
  "Rotate vector by angle"
  (let ((cosres (cos angle))
        (sinres (sin angle)))
    (vec2 (- (* (vx v) cosres) (* (vy v) sinres))
          (+ (* (vx v) sinres) (* (vy v) cosres)))))

(defun vector2-move-towards (v target max-distance)
  "Move Vector towards target"
  (let* ((dx (- (vx target) (vx v)))
         (dy (- (vy target) (vy v)))
         (value (+ (* dx dx) (* dy dy))))
    (if (or (= value 0)
            (and (>= max-distance 0) (<= value (* max-distance max-distance))))
        target
        (let ((dist (%sqrtf value)))
          (vec2 (+ (vx v) (* (/ dx dist) max-distance))
                (+ (vy v) (* (/ dy dist) max-distance)))))))

(defun vector2-invert (v)
  "Invert the given vector"
  (vec2 (/ 1.0 (vx v)) (/ 1.0 (vy v))))

(defun vector2-clamp (v min max)
  "Clamp the components of the vector between min and max values specified by the given vectors"
  (vec2 (min (vx max) (max (vx min) (vx v)))
        (min (vy max) (max (vy min) (vy v)))))

(defun vector2-clamp-value (v min max)
  "Clamp the magnitude of the vector between two min and max values"
  (let ((length (+ (* (vx v) (vx v)) (* (vy v) (vy v)))))
    (if (> length 0.0)
        (let ((length (%sqrtf length))
              (scale 1))                ; By default, 1 as the neutral element
          (cond ((< length min) (setf scale (/ min length)))
                ((> length max) (setf scale (/ max length))))
          (vec2 (* (vx v) scale) (* (vy v) scale)))
        (vec2 (vx v) (vy v)))))

(defun vector2-equals (p q)
  "Check whether two given vectors are almost equal"
  (and (float-equals (vx p) (vx q))
       (float-equals (vy p) (vy q))))

(defun vector2-refract (v n r)
  "Compute the direction of a refracted ray
v: normalized direction of the incoming ray
n: normalized normal vector of the interface of two optical media
r: ratio of the refractive index of the medium from where the ray comes
   to the refractive index of the medium on the other side of the surface"
  (let* ((dot (+ (* (vx v) (vx n)) (* (vy v) (vy n))))
         (d (- 1.0 (* r r (- 1.0 (* dot dot))))))
    (if (>= d 0.0)
        (let ((d (%sqrtf d)))
          (vec2 (- (* r (vx v)) (* (+ (* r dot) d) (vx n)))
                (- (* r (vy v)) (* (+ (* r dot) d) (vy n)))))
        (vec2 0.0 0.0))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Vector3 math
;;;----------------------------------------------------------------------------------

(defun vector3-zero ()
  "Vector with components value 0.0f"
  (vec3 0.0 0.0 0.0))

(defun vector3-one ()
  "Vector with components value 1.0f"
  (vec3 1.0 1.0 1.0))

(defun vector3-add (v1 v2)
  "Add two vectors"
  (vec3 (+ (vx v1) (vx v2)) (+ (vy v1) (vy v2)) (+ (vz v1) (vz v2))))

(defun vector3-add-value (v add)
  "Add vector and float value"
  (vec3 (+ (vx v) add) (+ (vy v) add) (+ (vz v) add)))

(defun vector3-subtract (v1 v2)
  "Subtract two vectors"
  (vec3 (- (vx v1) (vx v2)) (- (vy v1) (vy v2)) (- (vz v1) (vz v2))))

(defun vector3-subtract-value (v sub)
  "Subtract vector by float value"
  (vec3 (- (vx v) sub) (- (vy v) sub) (- (vz v) sub)))

(defun vector3-scale (v scalar)
  "Multiply vector by scalar"
  (vec3 (* (vx v) scalar) (* (vy v) scalar) (* (vz v) scalar)))

(defun vector3-multiply (v1 v2)
  "Multiply vector by vector"
  (vec3 (* (vx v1) (vx v2)) (* (vy v1) (vy v2)) (* (vz v1) (vz v2))))

(defun vector3-cross-product (v1 v2)
  "Calculate two vectors cross product"
  (vec3 (- (* (vy v1) (vz v2)) (* (vz v1) (vy v2)))
        (- (* (vz v1) (vx v2)) (* (vx v1) (vz v2)))
        (- (* (vx v1) (vy v2)) (* (vy v1) (vx v2)))))

(defun vector3-perpendicular (v)
  "Calculate one vector perpendicular vector"
  (let ((min (abs (vx v)))
        (cardinal-axis (vec3 1.0 0.0 0.0)))
    (when (< (abs (vy v)) min)
      (setf min (abs (vy v))
            cardinal-axis (vec3 0.0 1.0 0.0)))
    (when (< (abs (vz v)) min)
      (setf cardinal-axis (vec3 0.0 0.0 1.0)))
    ;; Cross product between vectors
    (vector3-cross-product v cardinal-axis)))

(defun vector3-length (v)
  "Calculate vector length"
  (%sqrtf (+ (* (vx v) (vx v)) (* (vy v) (vy v)) (* (vz v) (vz v)))))

(defun vector3-length-sqr (v)
  "Calculate vector square length"
  (+ (* (vx v) (vx v)) (* (vy v) (vy v)) (* (vz v) (vz v))))

(defun vector3-dot-product (v1 v2)
  "Calculate two vectors dot product"
  (+ (* (vx v1) (vx v2)) (* (vy v1) (vy v2)) (* (vz v1) (vz v2))))

(defun vector3-distance (v1 v2)
  "Calculate distance between two vectors"
  (%sqrtf (vector3-distance-sqr v1 v2)))

(defun vector3-distance-sqr (v1 v2)
  "Calculate square distance between two vectors"
  (let ((dx (- (vx v2) (vx v1)))
        (dy (- (vy v2) (vy v1)))
        (dz (- (vz v2) (vz v1))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defun vector3-angle (v1 v2)
  "Calculate angle between two vectors"
  (let* ((cross (vector3-cross-product v1 v2))
         (len (vector3-length cross))
         (dot (vector3-dot-product v1 v2)))
    (atan len dot)))

(defun vector3-negate (v)
  "Negate provided vector (invert direction)"
  (vec3 (- (vx v)) (- (vy v)) (- (vz v))))

(defun vector3-divide (v1 v2)
  "Divide vector by vector"
  (vec3 (/ (vx v1) (vx v2)) (/ (vy v1) (vy v2)) (/ (vz v1) (vz v2))))

(defun vector3-normalize (v)
  "Normalize provided vector"
  (let ((length (vector3-length v)))
    (if (/= length 0.0)
        (let ((ilength (/ 1.0 length)))
          (vec3 (* (vx v) ilength) (* (vy v) ilength) (* (vz v) ilength)))
        (vec3 (vx v) (vy v) (vz v)))))

(defun vector3-project (v1 v2)
  "Calculate the projection of the vector v1 on to v2"
  (let* ((v1dv2 (vector3-dot-product v1 v2))
         (v2dv2 (vector3-dot-product v2 v2))
         (mag (/ v1dv2 v2dv2)))
    (vec3 (* (vx v2) mag) (* (vy v2) mag) (* (vz v2) mag))))

(defun vector3-reject (v1 v2)
  "Calculate the rejection of the vector v1 on to v2"
  (let* ((v1dv2 (vector3-dot-product v1 v2))
         (v2dv2 (vector3-dot-product v2 v2))
         (mag (/ v1dv2 v2dv2)))
    (vec3 (- (vx v1) (* (vx v2) mag))
          (- (vy v1) (* (vy v2) mag))
          (- (vz v1) (* (vz v2) mag)))))

(defun vector3-ortho-normalize (v1 v2)
  "Orthonormalize provided vectors
Makes vectors normalized and orthogonal to each other
Gram-Schmidt function implementation
Returns the new v1 and v2 as multiple values"
  ;; Vector3Normalize(*v1)
  (let ((length (vector3-length v1)))
    (when (= length 0.0) (setf length 1.0))
    (let* ((ilength (/ 1.0 length))
           (v1 (vector3-scale v1 ilength))
           ;; Vector3CrossProduct(*v1, *v2)
           (vn1 (vector3-cross-product v1 v2)))
      ;; Vector3Normalize(vn1)
      (let ((length (vector3-length vn1)))
        (when (= length 0.0) (setf length 1.0))
        (setf vn1 (vector3-scale vn1 (/ 1.0 length))))
      ;; Vector3CrossProduct(vn1, *v1)
      (values v1 (vector3-cross-product vn1 v1)))))

(defun vector3-transform (v mat)
  "Transforms a Vector3 by a given Matrix"
  (let ((x (vx v))
        (y (vy v))
        (z (vz v)))
    (vec3 (+ (* (%m mat 0) x) (* (%m mat 4) y) (* (%m mat 8) z) (%m mat 12))
          (+ (* (%m mat 1) x) (* (%m mat 5) y) (* (%m mat 9) z) (%m mat 13))
          (+ (* (%m mat 2) x) (* (%m mat 6) y) (* (%m mat 10) z) (%m mat 14)))))

(defun vector3-rotate-by-quaternion (v q)
  "Transform a vector by quaternion rotation"
  (let ((x (vx q)) (y (vy q)) (z (vz q)) (w (vw q)))
    (vec3 (+ (* (vx v) (- (+ (* x x) (* w w)) (* y y) (* z z)))
             (* (vy v) (- (* 2 x y) (* 2 w z)))
             (* (vz v) (+ (* 2 x z) (* 2 w y))))
          (+ (* (vx v) (+ (* 2 w z) (* 2 x y)))
             (* (vy v) (- (+ (- (* w w) (* x x)) (* y y)) (* z z)))
             (* (vz v) (+ (* -2 w x) (* 2 y z))))
          (+ (* (vx v) (+ (* -2 w y) (* 2 x z)))
             (* (vy v) (+ (* 2 w x) (* 2 y z)))
             (* (vz v) (+ (- (* w w) (* x x) (* y y)) (* z z)))))))

(defun vector3-rotate-by-axis-angle (v axis angle)
  "Rotates a vector around an axis
Using Euler-Rodrigues Formula
Ref.: https://en.wikipedia.org/w/index.php?title=Euler%E2%80%93Rodrigues_formula"
  ;; Vector3Normalize(axis)
  (let ((length (vector3-length axis)))
    (when (= length 0.0) (setf length 1.0))
    (let* ((axis (vector3-scale axis (/ 1.0 length)))
           (angle (/ angle 2.0))
           (a (sin angle))
           (b (* (vx axis) a))
           (c (* (vy axis) a))
           (d (* (vz axis) a))
           (a (cos angle))
           (w (vec3 b c d))
           ;; Vector3CrossProduct(w, v)
           (wv (vector3-cross-product w v))
           ;; Vector3CrossProduct(w, wv)
           (wwv (vector3-cross-product w wv)))
      ;; Vector3Scale(wv, 2*a)
      (setf wv (vector3-scale wv (* a 2)))
      ;; Vector3Scale(wwv, 2)
      (setf wwv (vector3-scale wwv 2))
      (vec3 (+ (vx v) (vx wv) (vx wwv))
            (+ (vy v) (vy wv) (vy wwv))
            (+ (vz v) (vz wv) (vz wwv))))))

(defun vector3-move-towards (v target max-distance)
  "Move Vector towards target"
  (let* ((dx (- (vx target) (vx v)))
         (dy (- (vy target) (vy v)))
         (dz (- (vz target) (vz v)))
         (value (+ (* dx dx) (* dy dy) (* dz dz))))
    (if (or (= value 0)
            (and (>= max-distance 0) (<= value (* max-distance max-distance))))
        target
        (let ((dist (%sqrtf value)))
          (vec3 (+ (vx v) (* (/ dx dist) max-distance))
                (+ (vy v) (* (/ dy dist) max-distance))
                (+ (vz v) (* (/ dz dist) max-distance)))))))

(defun vector3-lerp (v1 v2 amount)
  "Calculate linear interpolation between two vectors"
  (vec3 (+ (vx v1) (* amount (- (vx v2) (vx v1))))
        (+ (vy v1) (* amount (- (vy v2) (vy v1))))
        (+ (vz v1) (* amount (- (vz v2) (vz v1))))))

(defun vector3-cubic-hermite (v1 tangent1 v2 tangent2 amount)
  "Calculate cubic hermite interpolation between two vectors and their tangents
as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic"
  (let* ((amount-pow2 (* amount amount))
         (amount-pow3 (* amount amount amount))
         (h00 (+ (- (* 2 amount-pow3) (* 3 amount-pow2)) 1))
         (h10 (+ (- amount-pow3 (* 2 amount-pow2)) amount))
         (h01 (+ (* -2 amount-pow3) (* 3 amount-pow2)))
         (h11 (- amount-pow3 amount-pow2)))
    (flet ((component (a ta b tb)
             (+ (* h00 a) (* h10 ta) (* h01 b) (* h11 tb))))
      (vec3 (component (vx v1) (vx tangent1) (vx v2) (vx tangent2))
            (component (vy v1) (vy tangent1) (vy v2) (vy tangent2))
            (component (vz v1) (vz tangent1) (vz v2) (vz tangent2))))))

(defun vector3-reflect (v normal)
  "Calculate reflected vector to normal"
  ;; I is the original vector
  ;; N is the normal of the incident plane
  ;; R = I - (2*N*(DotProduct[I, N]))
  (let ((dot-product (vector3-dot-product v normal)))
    (vec3 (- (vx v) (* (* 2.0 (vx normal)) dot-product))
          (- (vy v) (* (* 2.0 (vy normal)) dot-product))
          (- (vz v) (* (* 2.0 (vz normal)) dot-product)))))

(defun vector3-min (v1 v2)
  "Get min value for each pair of components"
  (vec3 (min (vx v1) (vx v2)) (min (vy v1) (vy v2)) (min (vz v1) (vz v2))))

(defun vector3-max (v1 v2)
  "Get max value for each pair of components"
  (vec3 (max (vx v1) (vx v2)) (max (vy v1) (vy v2)) (max (vz v1) (vz v2))))

(defun vector3-barycenter (p a b c)
  "Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)
NOTE: Assumes P is on the plane of the triangle"
  (let* ((v0 (vector3-subtract b a))
         (v1 (vector3-subtract c a))
         (v2 (vector3-subtract p a))
         (d00 (vector3-dot-product v0 v0))
         (d01 (vector3-dot-product v0 v1))
         (d11 (vector3-dot-product v1 v1))
         (d20 (vector3-dot-product v2 v0))
         (d21 (vector3-dot-product v2 v1))
         (denom (- (* d00 d11) (* d01 d01)))
         (y (/ (- (* d11 d20) (* d01 d21)) denom))
         (z (/ (- (* d00 d21) (* d01 d20)) denom)))
    (vec3 (- 1.0 (+ z y)) y z)))

(defun vector3-unproject (source projection view)
  "Projects a Vector3 from screen space into object space
NOTE: We are avoiding calling other raymath functions despite available"
  ;; Calculate unprojected matrix (multiply view matrix by projection matrix) and invert it
  (let* ((mat-view-proj-inv (matrix-invert (matrix-multiply view projection)))
         ;; Create quaternion from source point
         (quat (vec4 (vx source) (vy source) (vz source) 1.0))
         ;; Multiply quat point by unprojecte matrix
         (qtransformed (quaternion-transform quat mat-view-proj-inv)))
    ;; Normalized world points in vectors
    (vec3 (/ (vx qtransformed) (vw qtransformed))
          (/ (vy qtransformed) (vw qtransformed))
          (/ (vz qtransformed) (vw qtransformed)))))

(defun vector3-to-float-v (v)
  "Get Vector3 as float array"
  (make-array 3 :element-type 'single-float
                :initial-contents (list (%f (vx v)) (%f (vy v)) (%f (vz v)))))

(defun vector3-invert (v)
  "Invert the given vector"
  (vec3 (/ 1.0 (vx v)) (/ 1.0 (vy v)) (/ 1.0 (vz v))))

(defun vector3-clamp (v min max)
  "Clamp the components of the vector between
min and max values specified by the given vectors"
  (vec3 (min (vx max) (max (vx min) (vx v)))
        (min (vy max) (max (vy min) (vy v)))
        (min (vz max) (max (vz min) (vz v)))))

(defun vector3-clamp-value (v min max)
  "Clamp the magnitude of the vector between two values"
  (let ((length (vector3-length-sqr v)))
    (if (> length 0.0)
        (let ((length (%sqrtf length))
              (scale 1))                ; By default, 1 as the neutral element
          (cond ((< length min) (setf scale (/ min length)))
                ((> length max) (setf scale (/ max length))))
          (vec3 (* (vx v) scale) (* (vy v) scale) (* (vz v) scale)))
        (vec3 (vx v) (vy v) (vz v)))))

(defun vector3-equals (p q)
  "Check whether two given vectors are almost equal"
  (and (float-equals (vx p) (vx q))
       (float-equals (vy p) (vy q))
       (float-equals (vz p) (vz q))))

(defun vector3-refract (v n r)
  "Compute the direction of a refracted ray
v: normalized direction of the incoming ray
n: normalized normal vector of the interface of two optical media
r: ratio of the refractive index of the medium from where the ray comes
   to the refractive index of the medium on the other side of the surface"
  (let* ((dot (vector3-dot-product v n))
         (d (- 1.0 (* r r (- 1.0 (* dot dot))))))
    (if (>= d 0.0)
        (let ((d (%sqrtf d)))
          (vec3 (- (* r (vx v)) (* (+ (* r dot) d) (vx n)))
                (- (* r (vy v)) (* (+ (* r dot) d) (vy n)))
                (- (* r (vz v)) (* (+ (* r dot) d) (vz n)))))
        (vec3 0.0 0.0 0.0))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Vector4 math
;;;----------------------------------------------------------------------------------

(defun vector4-zero ()
  (vec4 0.0 0.0 0.0 0.0))

(defun vector4-one ()
  (vec4 1.0 1.0 1.0 1.0))

(defun vector4-add (v1 v2)
  (vec4 (+ (vx v1) (vx v2)) (+ (vy v1) (vy v2)) (+ (vz v1) (vz v2)) (+ (vw v1) (vw v2))))

(defun vector4-add-value (v add)
  (vec4 (+ (vx v) add) (+ (vy v) add) (+ (vz v) add) (+ (vw v) add)))

(defun vector4-subtract (v1 v2)
  (vec4 (- (vx v1) (vx v2)) (- (vy v1) (vy v2)) (- (vz v1) (vz v2)) (- (vw v1) (vw v2))))

(defun vector4-subtract-value (v add)
  (vec4 (- (vx v) add) (- (vy v) add) (- (vz v) add) (- (vw v) add)))

(defun vector4-length (v)
  (%sqrtf (vector4-length-sqr v)))

(defun vector4-length-sqr (v)
  (+ (* (vx v) (vx v)) (* (vy v) (vy v)) (* (vz v) (vz v)) (* (vw v) (vw v))))

(defun vector4-dot-product (v1 v2)
  (+ (* (vx v1) (vx v2)) (* (vy v1) (vy v2)) (* (vz v1) (vz v2)) (* (vw v1) (vw v2))))

(defun vector4-distance (v1 v2)
  "Calculate distance between two vectors"
  (%sqrtf (vector4-distance-sqr v1 v2)))

(defun vector4-distance-sqr (v1 v2)
  "Calculate square distance between two vectors"
  (+ (* (- (vx v1) (vx v2)) (- (vx v1) (vx v2)))
     (* (- (vy v1) (vy v2)) (- (vy v1) (vy v2)))
     (* (- (vz v1) (vz v2)) (- (vz v1) (vz v2)))
     (* (- (vw v1) (vw v2)) (- (vw v1) (vw v2)))))

(defun vector4-scale (v scale)
  (vec4 (* (vx v) scale) (* (vy v) scale) (* (vz v) scale) (* (vw v) scale)))

(defun vector4-multiply (v1 v2)
  "Multiply vector by vector"
  (vec4 (* (vx v1) (vx v2)) (* (vy v1) (vy v2)) (* (vz v1) (vz v2)) (* (vw v1) (vw v2))))

(defun vector4-negate (v)
  "Negate vector"
  (vec4 (- (vx v)) (- (vy v)) (- (vz v)) (- (vw v))))

(defun vector4-divide (v1 v2)
  "Divide vector by vector"
  (vec4 (/ (vx v1) (vx v2)) (/ (vy v1) (vy v2)) (/ (vz v1) (vz v2)) (/ (vw v1) (vw v2))))

(defun vector4-normalize (v)
  "Normalize provided vector"
  (let ((length (vector4-length v)))
    (if (> length 0)
        (vector4-scale v (/ 1.0 length))
        (vec4 0.0 0.0 0.0 0.0))))

(defun vector4-min (v1 v2)
  "Get min value for each pair of components"
  (vec4 (min (vx v1) (vx v2)) (min (vy v1) (vy v2)) (min (vz v1) (vz v2)) (min (vw v1) (vw v2))))

(defun vector4-max (v1 v2)
  "Get max value for each pair of components"
  (vec4 (max (vx v1) (vx v2)) (max (vy v1) (vy v2)) (max (vz v1) (vz v2)) (max (vw v1) (vw v2))))

(defun vector4-lerp (v1 v2 amount)
  "Calculate linear interpolation between two vectors"
  (vec4 (+ (vx v1) (* amount (- (vx v2) (vx v1))))
        (+ (vy v1) (* amount (- (vy v2) (vy v1))))
        (+ (vz v1) (* amount (- (vz v2) (vz v1))))
        (+ (vw v1) (* amount (- (vw v2) (vw v1))))))

(defun vector4-move-towards (v target max-distance)
  "Move Vector towards target"
  (let* ((dx (- (vx target) (vx v)))
         (dy (- (vy target) (vy v)))
         (dz (- (vz target) (vz v)))
         (dw (- (vw target) (vw v)))
         (value (+ (* dx dx) (* dy dy) (* dz dz) (* dw dw))))
    (if (or (= value 0)
            (and (>= max-distance 0) (<= value (* max-distance max-distance))))
        target
        (let ((dist (%sqrtf value)))
          (vec4 (+ (vx v) (* (/ dx dist) max-distance))
                (+ (vy v) (* (/ dy dist) max-distance))
                (+ (vz v) (* (/ dz dist) max-distance))
                (+ (vw v) (* (/ dw dist) max-distance)))))))

(defun vector4-invert (v)
  "Invert the given vector"
  (vec4 (/ 1.0 (vx v)) (/ 1.0 (vy v)) (/ 1.0 (vz v)) (/ 1.0 (vw v))))

(defun vector4-equals (p q)
  "Check whether two given vectors are almost equal"
  (and (float-equals (vx p) (vx q))
       (float-equals (vy p) (vy q))
       (float-equals (vz p) (vz q))
       (float-equals (vw p) (vw q))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Matrix math
;;;----------------------------------------------------------------------------------

(defun matrix-determinant (mat)
  "Compute matrix determinant"
  (%with-matrix (m mat)
    (- (+ (- (* m0 (+ (- (* m5 (- (* m10 m15) (* m11 m14))) (* m9 (- (* m6 m15) (* m7 m14))))
                      (* m13 (- (* m6 m11) (* m7 m10)))))
             (* m4 (+ (- (* m1 (- (* m10 m15) (* m11 m14))) (* m9 (- (* m2 m15) (* m3 m14))))
                      (* m13 (- (* m2 m11) (* m3 m10))))))
          (* m8 (+ (- (* m1 (- (* m6 m15) (* m7 m14))) (* m5 (- (* m2 m15) (* m3 m14))))
                   (* m13 (- (* m2 m7) (* m3 m6))))))
       (* m12 (+ (- (* m1 (- (* m6 m11) (* m7 m10))) (* m5 (- (* m2 m11) (* m3 m10))))
                 (* m9 (- (* m2 m7) (* m3 m6))))))))

(defun matrix-trace (mat)
  "Get the trace of the matrix (sum of the values along the diagonal)"
  (+ (%m mat 0) (%m mat 5) (%m mat 10) (%m mat 15)))

(defun matrix-transpose (mat)
  "Transposes provided matrix"
  (%with-matrix (m mat)
    (%matrix m0 m4 m8 m12
             m1 m5 m9 m13
             m2 m6 m10 m14
             m3 m7 m11 m15)))

(defun matrix-invert (mat)
  "Invert provided matrix"
  (%with-matrix (m mat)
    (let* ((a00 m0) (a01 m1) (a02 m2) (a03 m3)
           (a10 m4) (a11 m5) (a12 m6) (a13 m7)
           (a20 m8) (a21 m9) (a22 m10) (a23 m11)
           (a30 m12) (a31 m13) (a32 m14) (a33 m15)
           (b00 (- (* a00 a11) (* a01 a10)))
           (b01 (- (* a00 a12) (* a02 a10)))
           (b02 (- (* a00 a13) (* a03 a10)))
           (b03 (- (* a01 a12) (* a02 a11)))
           (b04 (- (* a01 a13) (* a03 a11)))
           (b05 (- (* a02 a13) (* a03 a12)))
           (b06 (- (* a20 a31) (* a21 a30)))
           (b07 (- (* a20 a32) (* a22 a30)))
           (b08 (- (* a20 a33) (* a23 a30)))
           (b09 (- (* a21 a32) (* a22 a31)))
           (b10 (- (* a21 a33) (* a23 a31)))
           (b11 (- (* a22 a33) (* a23 a32)))
           ;; Calculate the invert determinant (inlined to avoid double-caching)
           (inv-det (/ 1.0 (+ (- (* b00 b11) (* b01 b10)) (* b02 b09) (- (* b03 b08) (* b04 b07)) (* b05 b06)))))
      (%matrix (* (+ (- (* a11 b11) (* a12 b10)) (* a13 b09)) inv-det)
               (* (- (+ (- (* a01 b11)) (* a02 b10)) (* a03 b09)) inv-det)
               (* (+ (- (* a31 b05) (* a32 b04)) (* a33 b03)) inv-det)
               (* (- (+ (- (* a21 b05)) (* a22 b04)) (* a23 b03)) inv-det)
               (* (- (+ (- (* a10 b11)) (* a12 b08)) (* a13 b07)) inv-det)
               (* (+ (- (* a00 b11) (* a02 b08)) (* a03 b07)) inv-det)
               (* (- (+ (- (* a30 b05)) (* a32 b02)) (* a33 b01)) inv-det)
               (* (+ (- (* a20 b05) (* a22 b02)) (* a23 b01)) inv-det)
               (* (+ (- (* a10 b10) (* a11 b08)) (* a13 b06)) inv-det)
               (* (- (+ (- (* a00 b10)) (* a01 b08)) (* a03 b06)) inv-det)
               (* (+ (- (* a30 b04) (* a31 b02)) (* a33 b00)) inv-det)
               (* (- (+ (- (* a20 b04)) (* a21 b02)) (* a23 b00)) inv-det)
               (* (- (+ (- (* a10 b09)) (* a11 b07)) (* a12 b06)) inv-det)
               (* (+ (- (* a00 b09) (* a01 b07)) (* a02 b06)) inv-det)
               (* (- (+ (- (* a30 b03)) (* a31 b01)) (* a32 b00)) inv-det)
               (* (+ (- (* a20 b03) (* a21 b01)) (* a22 b00)) inv-det)))))

(defun matrix-identity ()
  "Get identity matrix"
  (mat 1.0 0.0 0.0 0.0
       0.0 1.0 0.0 0.0
       0.0 0.0 1.0 0.0
       0.0 0.0 0.0 1.0))

(defun matrix-add (left right)
  "Add two matrices"
  (m+ left right))

(defun matrix-subtract (left right)
  "Subtract two matrices (left - right)"
  (m- left right))

(defun matrix-multiply (left right)
  "Get two matrix multiplication
NOTE: When multiplying matrices... the order matters!
raylib MatrixMultiply(left, right) applies LEFT first, then RIGHT,
which in standard math notation is RIGHT x LEFT"
  (%with-matrix (l left)
    (%with-matrix (r right)
      (%matrix (+ (* l0 r0) (* l1 r4) (* l2 r8) (* l3 r12))
               (+ (* l0 r1) (* l1 r5) (* l2 r9) (* l3 r13))
               (+ (* l0 r2) (* l1 r6) (* l2 r10) (* l3 r14))
               (+ (* l0 r3) (* l1 r7) (* l2 r11) (* l3 r15))
               (+ (* l4 r0) (* l5 r4) (* l6 r8) (* l7 r12))
               (+ (* l4 r1) (* l5 r5) (* l6 r9) (* l7 r13))
               (+ (* l4 r2) (* l5 r6) (* l6 r10) (* l7 r14))
               (+ (* l4 r3) (* l5 r7) (* l6 r11) (* l7 r15))
               (+ (* l8 r0) (* l9 r4) (* l10 r8) (* l11 r12))
               (+ (* l8 r1) (* l9 r5) (* l10 r9) (* l11 r13))
               (+ (* l8 r2) (* l9 r6) (* l10 r10) (* l11 r14))
               (+ (* l8 r3) (* l9 r7) (* l10 r11) (* l11 r15))
               (+ (* l12 r0) (* l13 r4) (* l14 r8) (* l15 r12))
               (+ (* l12 r1) (* l13 r5) (* l14 r9) (* l15 r13))
               (+ (* l12 r2) (* l13 r6) (* l14 r10) (* l15 r14))
               (+ (* l12 r3) (* l13 r7) (* l14 r11) (* l15 r15))))))

(defun matrix-multiply-value (left value)
  "Multiply matrix components by value"
  (m* left value))

(defun matrix-translate (x y z)
  "Get translation matrix"
  (mat 1.0 0.0 0.0 x
       0.0 1.0 0.0 y
       0.0 0.0 1.0 z
       0.0 0.0 0.0 1.0))

(defun matrix-rotate (axis angle)
  "Create rotation matrix from axis and angle
NOTE: Angle should be provided in radians"
  (let* ((x (vx axis)) (y (vy axis)) (z (vz axis))
         (length-squared (+ (* x x) (* y y) (* z z))))
    (when (and (/= length-squared 1.0) (/= length-squared 0.0))
      (let ((ilength (/ 1.0 (%sqrtf length-squared))))
        (setf x (* x ilength)
              y (* y ilength)
              z (* z ilength))))
    (let* ((sinres (sin angle))
           (cosres (cos angle))
           (tt (- 1.0 cosres)))
      (%matrix (+ (* x x tt) cosres)
               (+ (* y x tt) (* z sinres))
               (- (* z x tt) (* y sinres))
               0.0
               (- (* x y tt) (* z sinres))
               (+ (* y y tt) cosres)
               (+ (* z y tt) (* x sinres))
               0.0
               (+ (* x z tt) (* y sinres))
               (- (* y z tt) (* x sinres))
               (+ (* z z tt) cosres)
               0.0
               0.0 0.0 0.0 1.0))))

(defun matrix-rotate-x (angle)
  "Get x-rotation matrix
NOTE: Angle must be provided in radians"
  (let ((cosres (cos angle))
        (sinres (sin angle)))
    (%matrix 1.0 0.0 0.0 0.0
             0.0 cosres sinres 0.0
             0.0 (- sinres) cosres 0.0
             0.0 0.0 0.0 1.0)))

(defun matrix-rotate-y (angle)
  "Get y-rotation matrix
NOTE: Angle must be provided in radians"
  (let ((cosres (cos angle))
        (sinres (sin angle)))
    (%matrix cosres 0.0 (- sinres) 0.0
             0.0 1.0 0.0 0.0
             sinres 0.0 cosres 0.0
             0.0 0.0 0.0 1.0)))

(defun matrix-rotate-z (angle)
  "Get z-rotation matrix
NOTE: Angle must be provided in radians"
  (let ((cosres (cos angle))
        (sinres (sin angle)))
    (%matrix cosres sinres 0.0 0.0
             (- sinres) cosres 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0)))

(defun matrix-rotate-xyz (angle)
  "Get xyz-rotation matrix
NOTE: Angle must be provided in radians"
  (let ((cosz (cos (- (vz angle))))
        (sinz (sin (- (vz angle))))
        (cosy (cos (- (vy angle))))
        (siny (sin (- (vy angle))))
        (cosx (cos (- (vx angle))))
        (sinx (sin (- (vx angle)))))
    (%matrix (* cosz cosy)
             (- (* cosz siny sinx) (* sinz cosx))
             (+ (* cosz siny cosx) (* sinz sinx))
             0.0
             (* sinz cosy)
             (+ (* sinz siny sinx) (* cosz cosx))
             (- (* sinz siny cosx) (* cosz sinx))
             0.0
             (- siny)
             (* cosy sinx)
             (* cosy cosx)
             0.0
             0.0 0.0 0.0 1.0)))

(defun matrix-rotate-zyx (angle)
  "Get zyx-rotation matrix
NOTE: Angle must be provided in radians"
  (let ((cz (cos (vz angle)))
        (sz (sin (vz angle)))
        (cy (cos (vy angle)))
        (sy (sin (vy angle)))
        (cx (cos (vx angle)))
        (sx (sin (vx angle))))
    (mat (* cz cy) (- (* cz sy sx) (* cx sz)) (+ (* sz sx) (* cz cx sy)) 0.0
         (* cy sz) (+ (* cz cx) (* sz sy sx)) (- (* cx sz sy) (* cz sx)) 0.0
         (- sy) (* cy sx) (* cy cx) 0.0
         0.0 0.0 0.0 1.0)))

(defun matrix-scale (x y z)
  "Get scaling matrix"
  (mat x 0.0 0.0 0.0
       0.0 y 0.0 0.0
       0.0 0.0 z 0.0
       0.0 0.0 0.0 1.0))

(defun matrix-frustum (left right bottom top near-plane far-plane)
  "Get perspective projection matrix"
  (let ((rl (%f (- right left)))
        (tb (%f (- top bottom)))
        (fn (%f (- far-plane near-plane))))
    (%matrix (/ (* (%f near-plane) 2.0) rl) 0.0 0.0 0.0
             0.0 (/ (* (%f near-plane) 2.0) tb) 0.0 0.0
             (/ (+ (%f right) (%f left)) rl)
             (/ (+ (%f top) (%f bottom)) tb)
             (/ (- (+ (%f far-plane) (%f near-plane))) fn)
             -1.0
             0.0 0.0 (/ (- (* (%f far-plane) (%f near-plane) 2.0)) fn) 0.0)))

(defun matrix-perspective (fov-y aspect near-plane far-plane)
  "Get perspective projection matrix
NOTE: Fovy angle must be provided in radians"
  (let* ((top (* (float near-plane 1d0) (tan (* (float fov-y 1d0) 0.5d0))))
         (bottom (- top))
         (right (* top (float aspect 1d0)))
         (left (- right)))
    ;; MatrixFrustum(-right, right, -top, top, near, far);
    (matrix-frustum left right bottom top near-plane far-plane)))

(defun matrix-ortho (left right bottom top near-plane far-plane)
  "Get orthographic projection matrix"
  (let ((rl (%f (- right left)))
        (tb (%f (- top bottom)))
        (fn (%f (- far-plane near-plane))))
    (%matrix (/ 2.0 rl) 0.0 0.0 0.0
             0.0 (/ 2.0 tb) 0.0 0.0
             0.0 0.0 (/ -2.0 fn) 0.0
             (/ (- (+ (%f left) (%f right))) rl)
             (/ (- (+ (%f top) (%f bottom))) tb)
             (/ (- (+ (%f far-plane) (%f near-plane))) fn)
             1.0)))

(defun matrix-look-at (eye target up)
  "Get camera look-at matrix (view matrix)"
  (let* ((vz (vector3-subtract eye target))
         (length (vector3-length vz)))
    ;; Vector3Normalize(vz)
    (when (= length 0.0) (setf length 1.0))
    (setf vz (vector3-scale vz (/ 1.0 length)))
    ;; Vector3CrossProduct(up, vz) normalized
    (let* ((vx (vector3-cross-product up vz))
           (length (vector3-length vx)))
      (when (= length 0.0) (setf length 1.0))
      (setf vx (vector3-scale vx (/ 1.0 length)))
      ;; Vector3CrossProduct(vz, vx)
      (let ((vy (vector3-cross-product vz vx)))
        (%matrix (vx vx) (vx vy) (vx vz) 0.0
                 (vy vx) (vy vy) (vy vz) 0.0
                 (vz vx) (vz vy) (vz vz) 0.0
                 (- (vector3-dot-product vx eye))
                 (- (vector3-dot-product vy eye))
                 (- (vector3-dot-product vz eye))
                 1.0)))))

(defun matrix-to-float-v (mat)
  "Get float array of matrix data (column-major, as OpenGL expects)"
  (let ((result (make-array 16 :element-type 'single-float)))
    (dotimes (i 16 result)
      (setf (aref result i) (%f (mcref4 mat (mod i 4) (floor i 4)))))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Quaternion math
;;;----------------------------------------------------------------------------------

(defun quaternion-add (q1 q2)
  "Add two quaternions"
  (vector4-add q1 q2))

(defun quaternion-add-value (q add)
  "Add quaternion and float value"
  (vector4-add-value q add))

(defun quaternion-subtract (q1 q2)
  "Subtract two quaternions"
  (vector4-subtract q1 q2))

(defun quaternion-subtract-value (q sub)
  "Subtract quaternion and float value"
  (vector4-subtract-value q sub))

(defun quaternion-identity ()
  "Get identity quaternion"
  (vec4 0.0 0.0 0.0 1.0))

(defun quaternion-length (q)
  "Computes the length of a quaternion"
  (vector4-length q))

(defun quaternion-normalize (q)
  "Normalize provided quaternion"
  (let ((length (vector4-length q)))
    (when (= length 0.0) (setf length 1.0))
    (vector4-scale q (/ 1.0 length))))

(defun quaternion-invert (q)
  "Invert provided quaternion"
  (let ((length-sq (vector4-length-sqr q)))
    (if (/= length-sq 0.0)
        (let ((inv-length (/ 1.0 length-sq)))
          (vec4 (* (vx q) (- inv-length))
                (* (vy q) (- inv-length))
                (* (vz q) (- inv-length))
                (* (vw q) inv-length)))
        (vec4 (vx q) (vy q) (vz q) (vw q)))))

(defun quaternion-multiply (q1 q2)
  "Calculate two quaternion multiplication"
  (let ((qax (vx q1)) (qay (vy q1)) (qaz (vz q1)) (qaw (vw q1))
        (qbx (vx q2)) (qby (vy q2)) (qbz (vz q2)) (qbw (vw q2)))
    (vec4 (- (+ (* qax qbw) (* qaw qbx) (* qay qbz)) (* qaz qby))
          (- (+ (* qay qbw) (* qaw qby) (* qaz qbx)) (* qax qbz))
          (- (+ (* qaz qbw) (* qaw qbz) (* qax qby)) (* qay qbx))
          (- (* qaw qbw) (* qax qbx) (* qay qby) (* qaz qbz)))))

(defun quaternion-scale (q mul)
  "Scale quaternion by float value"
  (vector4-scale q mul))

(defun quaternion-divide (q1 q2)
  "Divide two quaternions"
  (vector4-divide q1 q2))

(defun quaternion-lerp (q1 q2 amount)
  "Calculate linear interpolation between two quaternions"
  (vector4-lerp q1 q2 amount))

(defun quaternion-nlerp (q1 q2 amount)
  "Calculate slerp-optimized interpolation between two quaternions"
  (quaternion-normalize (vector4-lerp q1 q2 amount)))

(defun quaternion-slerp (q1 q2 amount)
  "Calculates spherical linear interpolation between two quaternions"
  (let ((cos-half-theta (vector4-dot-product q1 q2)))
    (when (< cos-half-theta 0)
      (setf q2 (vector4-negate q2)
            cos-half-theta (- cos-half-theta)))
    (cond
      ((>= (abs cos-half-theta) 1.0) q1)
      ((> cos-half-theta 0.95) (quaternion-nlerp q1 q2 amount))
      (t
       (let ((half-theta (acos cos-half-theta))
             (sin-half-theta (%sqrtf (- 1.0 (* cos-half-theta cos-half-theta)))))
         (if (< (abs sin-half-theta) +epsilon+)
             (vec4 (+ (* (vx q1) 0.5) (* (vx q2) 0.5))
                   (+ (* (vy q1) 0.5) (* (vy q2) 0.5))
                   (+ (* (vz q1) 0.5) (* (vz q2) 0.5))
                   (+ (* (vw q1) 0.5) (* (vw q2) 0.5)))
             (let ((ratio-a (/ (sin (* (- 1 amount) half-theta)) sin-half-theta))
                   (ratio-b (/ (sin (* amount half-theta)) sin-half-theta)))
               (vec4 (+ (* (vx q1) ratio-a) (* (vx q2) ratio-b))
                     (+ (* (vy q1) ratio-a) (* (vy q2) ratio-b))
                     (+ (* (vz q1) ratio-a) (* (vz q2) ratio-b))
                     (+ (* (vw q1) ratio-a) (* (vw q2) ratio-b))))))))))

(defun quaternion-cubic-hermite-spline (q1 out-tangent1 q2 in-tangent2 tt)
  "Calculate quaternion cubic spline interpolation using Cubic Hermite Spline algorithm
as described in the GLTF 2.0 specification: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#interpolation-cubic"
  (let* ((t2 (* tt tt))
         (t3 (* t2 tt))
         (h00 (+ (- (* 2 t3) (* 3 t2)) 1))
         (h10 (+ (- t3 (* 2 t2)) tt))
         (h01 (+ (* -2 t3) (* 3 t2)))
         (h11 (- t3 t2))
         (p0 (quaternion-scale q1 h00))
         (m0 (quaternion-scale out-tangent1 h10))
         (p1 (quaternion-scale q2 h01))
         (m1 (quaternion-scale in-tangent2 h11))
         (result (quaternion-add p0 m0)))
    (setf result (quaternion-add result p1))
    (setf result (quaternion-add result m1))
    (quaternion-normalize result)))

(defun quaternion-from-vector3-to-vector3 (from to)
  "Calculate quaternion based on the rotation from one vector to another"
  (let* ((cos2-theta (vector3-dot-product from to))
         (cross (vector3-cross-product from to)))
    ;; QuaternionNormalize(q);
    ;; NOTE: Normalize to essentially nlerp the original and identity to 0.5
    (quaternion-normalize
     (vec4 (vx cross) (vy cross) (vz cross)
           (+ (%sqrtf (+ (* (vx cross) (vx cross)) (* (vy cross) (vy cross))
                       (* (vz cross) (vz cross)) (* cos2-theta cos2-theta)))
              cos2-theta)))))

(defun quaternion-from-matrix (mat)
  "Get a quaternion for a given rotation matrix"
  (%with-matrix (m mat)
    (let* ((four-w-squared-minus1 (+ m0 m5 m10))
           (four-x-squared-minus1 (- m0 m5 m10))
           (four-y-squared-minus1 (- m5 m0 m10))
           (four-z-squared-minus1 (- m10 m0 m5))
           (biggest-index 0)
           (four-biggest-squared-minus1 four-w-squared-minus1))
      (when (> four-x-squared-minus1 four-biggest-squared-minus1)
        (setf four-biggest-squared-minus1 four-x-squared-minus1
              biggest-index 1))
      (when (> four-y-squared-minus1 four-biggest-squared-minus1)
        (setf four-biggest-squared-minus1 four-y-squared-minus1
              biggest-index 2))
      (when (> four-z-squared-minus1 four-biggest-squared-minus1)
        (setf four-biggest-squared-minus1 four-z-squared-minus1
              biggest-index 3))
      (let* ((biggest-val (* (%sqrtf (+ four-biggest-squared-minus1 1.0)) 0.5))
             (mult (/ 0.25 biggest-val)))
        (ecase biggest-index
          (0 (vec4 (* (- m6 m9) mult) (* (- m8 m2) mult) (* (- m1 m4) mult) biggest-val))
          (1 (vec4 biggest-val (* (+ m1 m4) mult) (* (+ m8 m2) mult) (* (- m6 m9) mult)))
          (2 (vec4 (* (+ m1 m4) mult) biggest-val (* (+ m6 m9) mult) (* (- m8 m2) mult)))
          (3 (vec4 (* (+ m8 m2) mult) (* (+ m6 m9) mult) biggest-val (* (- m1 m4) mult))))))))

(defun quaternion-to-matrix (q)
  "Get a matrix for a given quaternion"
  (let* ((a2 (* (vx q) (vx q)))
         (b2 (* (vy q) (vy q)))
         (c2 (* (vz q) (vz q)))
         (ac (* (vx q) (vz q)))
         (ab (* (vx q) (vy q)))
         (bc (* (vy q) (vz q)))
         (ad (* (vw q) (vx q)))
         (bd (* (vw q) (vy q)))
         (cd (* (vw q) (vz q))))
    (%matrix (- 1 (* 2 (+ b2 c2))) (* 2 (+ ab cd)) (* 2 (- ac bd)) 0.0
             (* 2 (- ab cd)) (- 1 (* 2 (+ a2 c2))) (* 2 (+ bc ad)) 0.0
             (* 2 (+ ac bd)) (* 2 (- bc ad)) (- 1 (* 2 (+ a2 b2))) 0.0
             0.0 0.0 0.0 1.0)))

(defun quaternion-from-axis-angle (axis angle)
  "Get rotation quaternion for an angle and axis
NOTE: Angle must be provided in radians"
  (let ((length (vector3-length axis)))
    (if (/= length 0.0)
        (let* ((angle (* angle 0.5))
               ;; Vector3Normalize(axis)
               (axis (vector3-scale axis (/ 1.0 length)))
               (sinres (sin angle))
               (cosres (cos angle)))
          ;; QuaternionNormalize(q);
          (quaternion-normalize
           (vec4 (* (vx axis) sinres) (* (vy axis) sinres) (* (vz axis) sinres) cosres)))
        (vec4 0.0 0.0 0.0 1.0))))

(defun quaternion-to-axis-angle (q)
  "Get the rotation angle and axis for a given quaternion
Returns (values axis angle)"
  (when (> (abs (vw q)) 1.0)
    ;; QuaternionNormalize(q);
    (setf q (quaternion-normalize q)))
  (let ((res-axis (vec3 0.0 0.0 0.0))
        (res-angle (* 2.0 (acos (vw q))))
        (den (%sqrtf (- 1.0 (* (vw q) (vw q))))))
    (if (> den +epsilon+)
        (setf res-axis (vec3 (/ (vx q) den) (/ (vy q) den) (/ (vz q) den)))
        ;; This occurs when the angle is zero
        ;; Not a problem: just set an arbitrary normalized axis
        (setf (vx res-axis) 1.0))
    (values res-axis res-angle)))

(defun quaternion-from-euler (pitch yaw roll)
  "Get the quaternion equivalent to Euler angles
NOTE: Rotation order is ZYX"
  (let ((x0 (cos (* pitch 0.5)))
        (x1 (sin (* pitch 0.5)))
        (y0 (cos (* yaw 0.5)))
        (y1 (sin (* yaw 0.5)))
        (z0 (cos (* roll 0.5)))
        (z1 (sin (* roll 0.5))))
    (vec4 (- (* x1 y0 z0) (* x0 y1 z1))
          (+ (* x0 y1 z0) (* x1 y0 z1))
          (- (* x0 y0 z1) (* x1 y1 z0))
          (+ (* x0 y0 z0) (* x1 y1 z1)))))

(defun quaternion-to-euler (q)
  "Get the Euler angles equivalent to quaternion (roll, pitch, yaw)
NOTE: Angles are returned in a Vector3 struct in radians"
  (let* ((x (vx q)) (y (vy q)) (z (vz q)) (w (vw q))
         ;; Roll (x-axis rotation)
         (x0 (* 2.0 (+ (* w x) (* y z))))
         (x1 (- 1.0 (* 2.0 (+ (* x x) (* y y)))))
         ;; Pitch (y-axis rotation)
         (y0 (* 2.0 (- (* w y) (* z x))))
         ;; Yaw (z-axis rotation)
         (z0 (* 2.0 (+ (* w z) (* x y))))
         (z1 (- 1.0 (* 2.0 (+ (* y y) (* z z))))))
    (setf y0 (if (> y0 1.0) 1.0 y0))
    (setf y0 (if (< y0 -1.0) -1.0 y0))
    (vec3 (atan x0 x1) (asin y0) (atan z0 z1))))

(defun quaternion-transform (q mat)
  "Transform a quaternion given a transformation matrix"
  (let ((x (vx q)) (y (vy q)) (z (vz q)) (w (vw q)))
    (vec4 (+ (* (%m mat 0) x) (* (%m mat 4) y) (* (%m mat 8) z) (* (%m mat 12) w))
          (+ (* (%m mat 1) x) (* (%m mat 5) y) (* (%m mat 9) z) (* (%m mat 13) w))
          (+ (* (%m mat 2) x) (* (%m mat 6) y) (* (%m mat 10) z) (* (%m mat 14) w))
          (+ (* (%m mat 3) x) (* (%m mat 7) y) (* (%m mat 11) z) (* (%m mat 15) w)))))

(defun quaternion-equals (p q)
  "Check whether two given quaternions are almost equal"
  (or (and (float-equals (vx p) (vx q))
           (float-equals (vy p) (vy q))
           (float-equals (vz p) (vz q))
           (float-equals (vw p) (vw q)))
      (flet ((neg-equals (a b)
               (<= (abs (+ a b)) (* +epsilon+ (max 1.0 (max (abs a) (abs b)))))))
        (and (neg-equals (vx p) (vx q))
             (neg-equals (vy p) (vy q))
             (neg-equals (vz p) (vz q))
             (neg-equals (vw p) (vw q))))))

(defun matrix-compose (translation rotation scale)
  "Compose a transformation matrix from rotational, translational and scaling components
TODO: This function is not following raymath conventions defined in header: NOT self-contained"
  ;; Initialize vectors
  (let ((right (vec3 1.0 0.0 0.0))
        (up (vec3 0.0 1.0 0.0))
        (forward (vec3 0.0 0.0 1.0)))
    ;; Scale vectors
    (setf right (vector3-scale right (vx scale))
          up (vector3-scale up (vy scale))
          forward (vector3-scale forward (vz scale)))
    ;; Rotate vectors
    (setf right (vector3-rotate-by-quaternion right rotation)
          up (vector3-rotate-by-quaternion up rotation)
          forward (vector3-rotate-by-quaternion forward rotation))
    ;; Set result matrix output
    (mat (vx right) (vx up) (vx forward) (vx translation)
         (vy right) (vy up) (vy forward) (vy translation)
         (vz right) (vz up) (vz forward) (vz translation)
         0.0 0.0 0.0 1.0)))

(defun matrix-decompose (mat)
  "Decompose a transformation matrix into its rotational, translational and scaling components and remove shear
TODO: This function is not following raymath conventions defined in header: NOT self-contained
Returns (values translation rotation scale)"
  (%with-matrix (m mat)
    (let* ((eps 1e-9)
           ;; Extract Translation
           (translation (vec3 m12 m13 m14))
           ;; Matrix Columns - Rotation will be extracted into here
           (c0 (vec3 m0 m4 m8))
           (c1 (vec3 m1 m5 m9))
           (c2 (vec3 m2 m6 m10))
           ;; Shear Parameters XY, XZ, and YZ (extract and ignored)
           (shear (make-array 3 :initial-element 0.0))
           ;; Normalized Scale Parameters
           (scl (vec3 0.0 0.0 0.0))
           ;; Max-Normalizing helps numerical stability
           (stabilizer eps))
      (dolist (col (list c0 c1 c2))
        (setf stabilizer (max stabilizer (abs (vx col))))
        (setf stabilizer (max stabilizer (abs (vy col))))
        (setf stabilizer (max stabilizer (abs (vz col)))))
      (setf c0 (vector3-scale c0 (/ 1.0 stabilizer))
            c1 (vector3-scale c1 (/ 1.0 stabilizer))
            c2 (vector3-scale c2 (/ 1.0 stabilizer)))
      ;; X Scale
      (setf (vx scl) (vector3-length c0))
      (when (> (vx scl) eps) (setf c0 (vector3-scale c0 (/ 1.0 (vx scl)))))
      ;; Compute XY shear and make col2 orthogonal
      (setf (aref shear 0) (vector3-dot-product c0 c1))
      (setf c1 (vector3-subtract c1 (vector3-scale c0 (aref shear 0))))
      ;; Y Scale
      (setf (vy scl) (vector3-length c1))
      (when (> (vy scl) eps)
        (setf c1 (vector3-scale c1 (/ 1.0 (vy scl))))
        (setf (aref shear 0) (/ (aref shear 0) (vy scl)))) ; Correct XY shear
      ;; Compute XZ and YZ shears and make col3 orthogonal
      (setf (aref shear 1) (vector3-dot-product c0 c2))
      (setf c2 (vector3-subtract c2 (vector3-scale c0 (aref shear 1))))
      (setf (aref shear 2) (vector3-dot-product c1 c2))
      (setf c2 (vector3-subtract c2 (vector3-scale c1 (aref shear 2))))
      ;; Z Scale
      (setf (vz scl) (vector3-length c2))
      (when (> (vz scl) eps)
        (setf c2 (vector3-scale c2 (/ 1.0 (vz scl))))
        (setf (aref shear 1) (/ (aref shear 1) (vz scl))) ; Correct XZ shear
        (setf (aref shear 2) (/ (aref shear 2) (vz scl)))) ; Correct YZ shear
      ;; matColumns are now orthonormal in O(3). Now ensure its in SO(3) by enforcing det = 1
      (when (< (vector3-dot-product c0 (vector3-cross-product c1 c2)) 0)
        (setf scl (vector3-negate scl)
              c0 (vector3-negate c0)
              c1 (vector3-negate c1)
              c2 (vector3-negate c2)))
      ;; Set Scale
      (let ((scale (vector3-scale scl stabilizer))
            ;; Extract Rotation
            (rotation-matrix (mat (vx c0) (vy c0) (vz c0) 0.0
                                  (vx c1) (vy c1) (vz c1) 0.0
                                  (vx c2) (vy c2) (vz c2) 0.0
                                  0.0 0.0 0.0 1.0)))
        (values translation (quaternion-from-matrix rotation-matrix) scale)))))
