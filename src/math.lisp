(in-package #:cl-raylib)

;;; Math constants (matching raylib raymath.h)
(defconstant +pi+ 3.14159265358979323846)
(defconstant +deg2rad+ (/ +pi+ 180.0))
(defconstant +rad2deg+ (/ 180.0 +pi+))
(defconstant +epsilon+ 0.000001)

;;; Utility functions (matching raylib raymath.h)

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

;;;===================================================================================
;;; Core math utility functions (raylib raymath.h)
;;;===================================================================================

(defun float-equals (x y)
  "Check whether two given floats are almost equal"
  (<= (abs (- x y)) (* +epsilon+ (max 1.0 (max (abs x) (abs y))))))

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
  (- value (* (- max min) (floor (/ (- value min) (- max min))))))

;;;===================================================================================
;;; Vector2 math functions (raylib raymath.h Vector2 functions)
;;; Note: Most basic vector operations are handled by 3d-vectors library
;;; These are raylib-specific convenience functions and additional operations
;;;===================================================================================

(defun vector2-zero ()
  "Vector with components value 0.0"
  (vec2 0.0 0.0))

(defun vector2-one ()
  "Vector with components value 1.0"  
  (vec2 1.0 1.0))

;; Raylib-specific functions that wrap 3d-vectors functions
(defun vector2-add (v1 v2)
  "Add two vectors (v1 + v2)"
  (v+ v1 v2))

(defun vector2-subtract (v1 v2)
  "Subtract two vectors (v1 - v2)"
  (v- v1 v2))

(defun vector2-scale (v scale)
  "Scale vector (multiply by value)"
  (v* v scale))

(defun vector2-multiply (v1 v2)
  "Multiply vector by vector"
  (vec2 (* (vx2 v1) (vx2 v2)) (* (vy2 v1) (vy2 v2))))

(defun vector2-negate (v)
  "Negate vector"
  (v- v))

(defun vector2-divide (v1 v2)
  "Divide vector by vector"
  (vec2 (/ (vx2 v1) (vx2 v2)) (/ (vy2 v1) (vy2 v2))))

(defun vector2-normalize (v)
  "Normalize provided vector"
  (vunit v))

(defun vector2-length (v)
  "Calculate vector length"
  (vlength v))

(defun vector2-length-sqr (v)
  "Calculate vector square length"
  (vsqrlength v))

(defun vector2-dot-product (v1 v2)
  "Calculate two vectors dot product"
  (v. v1 v2))

(defun vector2-distance (v1 v2)
  "Calculate distance between two vectors"
  (vdistance v1 v2))

(defun vector2-distance-sqr (v1 v2)
  "Calculate square distance between two vectors"
  (vsqrdistance v1 v2))

(defun vector2-lerp (v1 v2 amount)
  "Calculate linear interpolation between two vectors"
  (vlerp v1 v2 amount))

(defun vector2-min (v1 v2)
  "Get min value for each pair of components"
  (vmin v1 v2))

(defun vector2-max (v1 v2)
  "Get max value for each pair of components"
  (vmax v1 v2))

(defun vector2-clamp (v min max)
  "Clamp the components of the vector between min and max values"
  (vclamp v min max))

;; Raylib-specific functions not directly available in 3d-vectors
(defun vector2-add-value (v add)
  "Add vector and float value"
  (vec2 (+ (vx2 v) add) (+ (vy2 v) add)))

(defun vector2-subtract-value (v sub)
  "Subtract vector by float value"
  (vec2 (- (vx2 v) sub) (- (vy2 v) sub)))

(defun vector2-cross-product (v1 v2)
  "Calculate two vectors cross product"
  (- (* (vx2 v1) (vy2 v2)) (* (vy2 v1) (vx2 v2))))

(defun vector2-angle (v1 v2)
  "Calculate angle between two vectors (from v1 to v2)"
  (let ((dot (v. v1 v2))
        (det (vector2-cross-product v1 v2)))
    (atan det dot)))

(defun vector2-line-angle (start end)
  "Calculate angle defined by a two vectors line"
  (- (atan (- (vy2 end) (vy2 start)) (- (vx2 end) (vx2 start)))))

(defun vector2-reflect (v normal)
  "Calculate reflected vector to normal"
  (let ((dot-product (* 2.0 (v. v normal))))
    (v- v (v* normal dot-product))))

(defun vector2-rotate (v angle)
  "Rotate vector by angle"
  (let ((cos-res (cos angle))
        (sin-res (sin angle))
        (x (vx2 v))
        (y (vy2 v)))
    (vec2 (- (* x cos-res) (* y sin-res))
          (+ (* x sin-res) (* y cos-res)))))

(defun vector2-move-towards (v target max-distance)
  "Move Vector towards target, up to maximum distance"
  (let ((diff (v- target v)))
    (let ((distance (vlength diff)))
      (if (or (<= distance max-distance) (= distance 0))
          target
          (v+ v (v* (vunit diff) max-distance))))))

(defun vector2-invert (v)
  "Invert the given vector"
  (vec2 (/ 1.0 (vx2 v)) (/ 1.0 (vy2 v))))

(defun vector2-clamp-value (v min max)
  "Clamp the magnitude of the vector between two values"
  (let ((length (vlength v)))
    (if (> length 0)
        (let ((clamped-length (clamp length min max)))
          (v* (vunit v) clamped-length))
        v)))

(defun vector2-equals (p q)
  "Check whether two given vectors are almost equal"
  (and (float-equals (vx2 p) (vx2 q))
       (float-equals (vy2 p) (vy2 q))))

(defun vector2-transform (v mat)
  "Transforms a Vector2 by a given Matrix"
  (let ((x (vx2 v))
        (y (vy2 v)))
    (vec2 (+ (* x (mcref4 mat 0 0)) (* y (mcref4 mat 1 0)) (mcref4 mat 3 0))
          (+ (* x (mcref4 mat 0 1)) (* y (mcref4 mat 1 1)) (mcref4 mat 3 1)))))

;;;===================================================================================
;;; Vector3 math functions (raylib raymath.h Vector3 functions)
;;; Note: Most basic vector operations are handled by 3d-vectors library
;;; These are raylib-specific convenience functions and additional operations
;;;===================================================================================

(defun vector3-zero ()
  "Vector with components value 0.0"
  (vec3 0.0 0.0 0.0))

(defun vector3-one ()
  "Vector with components value 1.0"
  (vec3 1.0 1.0 1.0))

;; Raylib-specific functions that wrap 3d-vectors functions
(defun vector3-add (v1 v2)
  "Add two vectors (v1 + v2)"
  (v+ v1 v2))

(defun vector3-subtract (v1 v2)
  "Subtract two vectors (v1 - v2)"
  (v- v1 v2))

(defun vector3-scale (v scalar)
  "Multiply vector by scalar"
  (v* v scalar))

(defun vector3-cross-product (v1 v2)
  "Calculate two vectors cross product"
  (vc v1 v2))

(defun vector3-length (v)
  "Calculate vector length"
  (vlength v))

(defun vector3-length-sqr (v)
  "Calculate vector square length"
  (vsqrlength v))

(defun vector3-dot-product (v1 v2)
  "Calculate two vectors dot product"
  (v. v1 v2))

(defun vector3-distance (v1 v2)
  "Calculate distance between two vectors"
  (vdistance v1 v2))

(defun vector3-distance-sqr (v1 v2)
  "Calculate square distance between two vectors"
  (vsqrdistance v1 v2))

(defun vector3-angle (v1 v2)
  "Calculate angle between two vectors"
  (vangle v1 v2))

(defun vector3-negate (v)
  "Negate provided vector (invert direction)"
  (v- v))

(defun vector3-normalize (v)
  "Normalize provided vector"
  (vunit v))

(defun vector3-lerp (v1 v2 amount)
  "Calculate linear interpolation between two vectors"
  (vlerp v1 v2 amount))

(defun vector3-min (v1 v2)
  "Get min value for each pair of components"
  (vmin v1 v2))

(defun vector3-max (v1 v2)
  "Get max value for each pair of components"
  (vmax v1 v2))

(defun vector3-clamp (v min max)
  "Clamp the components of the vector between min and max values"
  (vclamp v min max))

;; Raylib-specific functions not directly available in 3d-vectors
(defun vector3-add-value (v add)
  "Add vector and float value"
  (vec3 (+ (vx3 v) add) (+ (vy3 v) add) (+ (vz3 v) add)))

(defun vector3-subtract-value (v sub)
  "Subtract vector by float value"
  (vec3 (- (vx3 v) sub) (- (vy3 v) sub) (- (vz3 v) sub)))

(defun vector3-multiply (v1 v2)
  "Multiply vector by vector"
  (vec3 (* (vx3 v1) (vx3 v2)) (* (vy3 v1) (vy3 v2)) (* (vz3 v1) (vz3 v2))))

(defun vector3-divide (v1 v2)
  "Divide vector by vector"
  (vec3 (/ (vx3 v1) (vx3 v2)) (/ (vy3 v1) (vy3 v2)) (/ (vz3 v1) (vz3 v2))))

(defun vector3-perpendicular (v)
  "Calculate one vector perpendicular vector"
  (let ((min (min (abs (vx3 v)) (abs (vy3 v)) (abs (vz3 v)))))
    (cond
      ((= min (abs (vx3 v))) (vec3 0.0 (- (vz3 v)) (vy3 v)))
      ((= min (abs (vy3 v))) (vec3 (- (vz3 v)) 0.0 (vx3 v)))
      (t (vec3 (- (vy3 v)) (vx3 v) 0.0)))))

(defun vector3-project (v1 v2)
  "Calculate the projection of the vector v1 on to v2"
  (let ((v1-dot-v2 (v. v1 v2))
        (v2-dot-v2 (v. v2 v2)))
    (v* v2 (/ v1-dot-v2 v2-dot-v2))))

(defun vector3-reject (v1 v2)
  "Calculate the rejection of the vector v1 on to v2"
  (v- v1 (vector3-project v1 v2)))

(defun vector3-ortho-normalize (v1 v2)
  "Orthonormalize provided vectors"
  (let ((nv1 (vunit v1)))
    (let ((nv2 (vunit (v- v2 (v* nv1 (v. v2 nv1))))))
      (list nv1 nv2))))

(defun vector3-transform (v mat)
  "Transforms a Vector3 by a given Matrix"
  (let ((x (vx3 v))
        (y (vy3 v))
        (z (vz3 v)))
    (vec3 (+ (* x (mcref4 mat 0 0)) (* y (mcref4 mat 1 0)) (* z (mcref4 mat 2 0)) (mcref4 mat 3 0))
          (+ (* x (mcref4 mat 0 1)) (* y (mcref4 mat 1 1)) (* z (mcref4 mat 2 1)) (mcref4 mat 3 1))
          (+ (* x (mcref4 mat 0 2)) (* y (mcref4 mat 1 2)) (* z (mcref4 mat 2 2)) (mcref4 mat 3 2)))))

(defun vector3-rotate-by-quaternion (v q)
  "Transform a vector by quaternion rotation"
  (let ((qx (vx4 q))
        (qy (vy4 q))
        (qz (vz4 q))
        (qw (vw4 q))
        (vx (vx3 v))
        (vy (vy3 v))
        (vz (vz3 v)))
    (vec3 (+ vx (* 2.0 (- (* qy (- (* qw vz) (* qz vy))) (* qz (+ (* qw vy) (* qy vz))))))
          (+ vy (* 2.0 (- (* qz (- (* qw vx) (* qx vz))) (* qx (+ (* qw vz) (* qz vx))))))
          (+ vz (* 2.0 (- (* qx (- (* qw vy) (* qy vx))) (* qy (+ (* qw vx) (* qx vy)))))))))

(defun vector3-rotate-by-axis-angle (v axis angle)
  "Rotate a vector around an axis"
  (let ((cos-a (cos angle))
        (sin-a (sin angle))
        (dot (v. v axis))
        (cross (vc axis v)))
    (v+ (v* v cos-a)
        (v+ (v* cross sin-a)
            (v* axis (* dot (- 1.0 cos-a)))))))

(defun vector3-reflect (v normal)
  "Calculate reflected vector to normal"
  (let ((dot-product (* 2.0 (v. v normal))))
    (v- v (v* normal dot-product))))

(defun vector3-barycenter (p a b c)
  "Compute barycenter coordinates (u, v, w) for point p with respect to triangle (a, b, c)"
  (let ((v0 (v- c a))
        (v1 (v- b a))
        (v2 (v- p a)))
    (let ((dot00 (v. v0 v0))
          (dot01 (v. v0 v1))
          (dot02 (v. v0 v2))
          (dot11 (v. v1 v1))
          (dot12 (v. v1 v2)))
      (let ((inv-denom (/ 1.0 (- (* dot00 dot11) (* dot01 dot01)))))
        (let ((u (* (- (* dot11 dot02) (* dot01 dot12)) inv-denom))
              (v (* (- (* dot00 dot12) (* dot01 dot02)) inv-denom)))
          (vec3 (- 1.0 u v) v u))))))

(defun vector3-unproject (source projection view)
  "Projects a Vector3 from screen space into object space"
  (let ((mat-view-proj (minv (m* projection view))))
    (let ((quat (vec4 (- (* (vx3 source) 2.0) 1.0)
                      (- (* (vy3 source) 2.0) 1.0)
                      (- (* (vz3 source) 2.0) 1.0)
                      1.0)))
      (let ((quat-transformed (m* mat-view-proj quat)))
        (vec3 (/ (vx4 quat-transformed) (vw4 quat-transformed))
              (/ (vy4 quat-transformed) (vw4 quat-transformed))
              (/ (vz4 quat-transformed) (vw4 quat-transformed)))))))

(defun vector3-invert (v)
  "Invert the given vector"
  (vec3 (/ 1.0 (vx3 v)) (/ 1.0 (vy3 v)) (/ 1.0 (vz3 v))))

(defun vector3-clamp-value (v min max)
  "Clamp the magnitude of the vector between two values"
  (let ((length (vlength v)))
    (if (> length 0)
        (let ((clamped-length (clamp length min max)))
          (v* (vunit v) clamped-length))
        v)))

(defun vector3-equals (p q)
  "Check whether two given vectors are almost equal"
  (and (float-equals (vx3 p) (vx3 q))
       (float-equals (vy3 p) (vy3 q))
       (float-equals (vz3 p) (vz3 q))))

;;;===================================================================================
;;; Matrix math functions (raylib raymath.h Matrix functions)
;;; Note: Most basic matrix operations are handled by 3d-matrices library
;;; These are raylib-specific convenience functions
;;;===================================================================================

(defun matrix-determinant (mat)
  "Compute matrix determinant"
  (mdet mat))

(defun matrix-trace (mat)
  "Get the trace of the matrix (sum of the values along the diagonal)"
  (mtrace mat))

(defun matrix-transpose (mat)
  "Transposes provided matrix"
  (mtranspose mat))

(defun matrix-invert (mat)
  "Invert provided matrix"
  (minv mat))

(defun matrix-identity ()
  "Get identity matrix"
  (meye 4))

(defun matrix-add (left right)
  "Add two matrices"
  (m+ left right))

(defun matrix-subtract (left right)
  "Subtract two matrices (left - right)"
  (m- left right))

(defun matrix-multiply (left right)
  "Get two matrix multiplication"
  (m* left right))

(defun matrix-translate (x y z)
  "Get translation matrix"
  (mtranslation (vec3 x y z)))

(defun matrix-rotate (axis angle)
  "Create rotation matrix from axis and angle"
  (mrotation (vunit axis) angle))

(defun matrix-rotate-x (angle)
  "Get x-rotation matrix"
  (mrotation (vec3 1.0 0.0 0.0) angle))

(defun matrix-rotate-y (angle)
  "Get y-rotation matrix"
  (mrotation (vec3 0.0 1.0 0.0) angle))

(defun matrix-rotate-z (angle)
  "Get z-rotation matrix"
  (mrotation (vec3 0.0 0.0 1.0) angle))

(defun matrix-rotate-xyz (angles)
  "Get xyz-rotation matrix"
  (let ((x-rot (mrotation (vec3 1.0 0.0 0.0) (vx3 angles)))
        (y-rot (mrotation (vec3 0.0 1.0 0.0) (vy3 angles)))
        (z-rot (mrotation (vec3 0.0 0.0 1.0) (vz3 angles))))
    (m* z-rot (m* y-rot x-rot))))

(defun matrix-rotate-zyx (angles)
  "Get zyx-rotation matrix"
  (let ((x-rot (mrotation (vec3 1.0 0.0 0.0) (vx3 angles)))
        (y-rot (mrotation (vec3 0.0 1.0 0.0) (vy3 angles)))
        (z-rot (mrotation (vec3 0.0 0.0 1.0) (vz3 angles))))
    (m* x-rot (m* y-rot z-rot))))

(defun matrix-scale (x y z)
  "Get scaling matrix"
  (mscaling (vec3 x y z)))

(defun matrix-frustum (left right bottom top near far)
  "Get perspective projection matrix"
  (mfrustum left right bottom top near far))

(defun matrix-perspective (fovy aspect near far)
  "Get perspective projection matrix"
  (mperspective fovy aspect near far))

(defun matrix-ortho (left right bottom top near far)
  "Get orthographic projection matrix"
  (mortho left right bottom top near far))

(defun matrix-look-at (eye target up)
  "Get camera look-at matrix (view matrix)"
  (mlookat eye target up))

(defun matrix-to-float-v (mat)
  "Get float array of matrix data"
  (marr4 mat))

;;;===================================================================================
;;; Quaternion math functions (raylib raymath.h Quaternion functions)
;;; Note: Most basic quaternion operations are handled by 3d-quaternions library
;;; These are raylib-specific convenience functions and additional operations
;;;===================================================================================

(defun quaternion-identity ()
  "Get identity quaternion"
  (vec4 0.0 0.0 0.0 1.0))

(defun quaternion-length (q)
  "Computes the length of a quaternion"
  (qlength q))

(defun quaternion-normalize (q)
  "Normalize provided quaternion"
  (qunit q))

(defun quaternion-invert (q)
  "Invert provided quaternion"
  (qinv q))

(defun quaternion-multiply (q1 q2)
  "Calculate two quaternion multiplication"
  (q* q1 q2))

(defun quaternion-divide (q1 q2)
  "Divide two quaternions"
  (q/ q1 q2))

(defun quaternion-lerp (q1 q2 amount)
  "Calculate linear interpolation between two quaternions"
  (qnlerp q1 q2 amount))

(defun quaternion-nlerp (q1 q2 amount)
  "Calculate normalized linear interpolation between two quaternions"
  (qnlerp q1 q2 amount))

(defun quaternion-slerp (q1 q2 amount)
  "Calculates spherical linear interpolation between two quaternions"
  (qslerp q1 q2 amount))

(defun quaternion-from-matrix (mat)
  "Get a quaternion for a given rotation matrix"
  (qfrom-mat mat))

(defun quaternion-to-matrix (q)
  "Get a matrix for a given quaternion"
  (qmat4 q))

(defun quaternion-from-axis-angle (axis angle)
  "Get rotation quaternion for an angle and axis"
  (qfrom-angle axis angle))

(defun quaternion-to-axis-angle (q)
  "Get the rotation angle and axis for a given quaternion"
  (list (qaxis q) (qangle q)))

(defun quaternion-equals (p q)
  "Check whether two given quaternions are almost equal"
  (q= p q))

;; Raylib-specific quaternion functions not directly available in 3d-quaternions
(defun quaternion-scale (q mul)
  "Scale quaternion by float value"
  (vec4 (* (vx4 q) mul) (* (vy4 q) mul) (* (vz4 q) mul) (* (vw4 q) mul)))

(defun quaternion-from-vector3-to-vector3 (from to)
  "Calculate quaternion based on the rotation from one vector to another"
  (qfrom-angle (vunit (vc from to)) (vangle from to)))

(defun quaternion-from-euler (pitch yaw roll)
  "Get the quaternion equivalent to Euler angles"
  (let ((x (* pitch 0.5))
        (y (* yaw 0.5))
        (z (* roll 0.5)))
    (let ((c1 (cos x))
          (c2 (cos y))
          (c3 (cos z))
          (s1 (sin x))
          (s2 (sin y))
          (s3 (sin z)))
      (vec4 (+ (* s1 c2 c3) (* c1 s2 s3))
            (- (* c1 s2 c3) (* s1 c2 s3))
            (+ (* c1 c2 s3) (* s1 s2 c3))
            (- (* c1 c2 c3) (* s1 s2 s3))))))

(defun quaternion-to-euler (q)
  "Get the Euler angles equivalent to quaternion (roll, pitch, yaw)"
  (let ((x (vx4 q))
        (y (vy4 q))
        (z (vz4 q))
        (w (vw4 q)))
    (let ((x0 (* 2.0 (+ (* w x) (* y z))))
          (x1 (- 1.0 (* 2.0 (+ (* x x) (* y y)))))
          (y0 (* 2.0 (+ (* w y) (* z x))))
          (z0 (* 2.0 (+ (* w z) (* x y))))
          (z1 (- 1.0 (* 2.0 (+ (* y y) (* z z))))))
      (vec3 (atan x0 x1)
            (asin (clamp y0 -1.0 1.0))
            (atan z0 z1)))))

(defun quaternion-transform (q mat)
  "Transform a quaternion given a transformation matrix"
  (let ((x (vx4 q))
        (y (vy4 q))
        (z (vz4 q))
        (w (vw4 q)))
    (vec4 (+ (* x (mcref4 mat 0 0)) (* y (mcref4 mat 1 0)) (* z (mcref4 mat 2 0)) (* w (mcref4 mat 3 0)))
          (+ (* x (mcref4 mat 0 1)) (* y (mcref4 mat 1 1)) (* z (mcref4 mat 2 1)) (* w (mcref4 mat 3 1)))
          (+ (* x (mcref4 mat 0 2)) (* y (mcref4 mat 1 2)) (* z (mcref4 mat 2 2)) (* w (mcref4 mat 3 2)))
          (+ (* x (mcref4 mat 0 3)) (* y (mcref4 mat 1 3)) (* z (mcref4 mat 2 3)) (* w (mcref4 mat 3 3))))))
