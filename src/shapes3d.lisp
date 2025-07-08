(in-package #:cl-raylib)

;;; 3D Shapes Drawing Functions
;;; This module provides comprehensive 3D primitive drawing capabilities

;;; 3D Cube drawing functions

(defun draw-cube (position width height length color)
  "Draw a cube with center at position"
  (draw-cube-v position (vec3 width height length) color))

(defun draw-cube-v (position size color)
  "Draw a cube (Vector version) - simplified implementation"
  (let* ((x (vx position))
         (y (vy position))
         (z (vz position))
         (w (/ (vx size) 2.0))
         (h (/ (vy size) 2.0))
         (d (/ (vz size) 2.0)))
    
    ;; Set color for cube
    (set-gl-color color)
    
    ;; Draw cube using quads (simpler than triangles)
    (gl:with-primitive :quads
      ;; Front face
      (gl:normal 0.0 0.0 1.0)
      (gl:vertex (- x w) (- y h) (+ z d))
      (gl:vertex (+ x w) (- y h) (+ z d))
      (gl:vertex (+ x w) (+ y h) (+ z d))
      (gl:vertex (- x w) (+ y h) (+ z d))
      
      ;; Back face
      (gl:normal 0.0 0.0 -1.0)
      (gl:vertex (- x w) (- y h) (- z d))
      (gl:vertex (- x w) (+ y h) (- z d))
      (gl:vertex (+ x w) (+ y h) (- z d))
      (gl:vertex (+ x w) (- y h) (- z d))
      
      ;; Top face
      (gl:normal 0.0 1.0 0.0)
      (gl:vertex (- x w) (+ y h) (- z d))
      (gl:vertex (- x w) (+ y h) (+ z d))
      (gl:vertex (+ x w) (+ y h) (+ z d))
      (gl:vertex (+ x w) (+ y h) (- z d))
      
      ;; Bottom face
      (gl:normal 0.0 -1.0 0.0)
      (gl:vertex (- x w) (- y h) (- z d))
      (gl:vertex (+ x w) (- y h) (- z d))
      (gl:vertex (+ x w) (- y h) (+ z d))
      (gl:vertex (- x w) (- y h) (+ z d))
      
      ;; Right face
      (gl:normal 1.0 0.0 0.0)
      (gl:vertex (+ x w) (- y h) (- z d))
      (gl:vertex (+ x w) (+ y h) (- z d))
      (gl:vertex (+ x w) (+ y h) (+ z d))
      (gl:vertex (+ x w) (- y h) (+ z d))
      
      ;; Left face
      (gl:normal -1.0 0.0 0.0)
      (gl:vertex (- x w) (- y h) (- z d))
      (gl:vertex (- x w) (- y h) (+ z d))
      (gl:vertex (- x w) (+ y h) (+ z d))
      (gl:vertex (- x w) (+ y h) (- z d)))))

(defun draw-cube-face (center width height)
  "Draw a single cube face centered at position"
  (let* ((w (/ width 2.0))
         (h (/ height 2.0))
         (x (vx center))
         (y (vy center))
         (z (vz center)))
    ;; Two triangles to form a quad
    ;; Triangle 1
    (gl:vertex (- x w) (- y h) z)
    (gl:vertex (+ x w) (- y h) z)
    (gl:vertex (+ x w) (+ y h) z)
    
    ;; Triangle 2
    (gl:vertex (- x w) (- y h) z)
    (gl:vertex (+ x w) (+ y h) z)
    (gl:vertex (- x w) (+ y h) z)))

(defun draw-cube-wires (position width height length color)
  "Draw cube wireframe"
  (draw-cube-wires-v position (vec3 width height length) color))

(defun draw-cube-wires-v (position size color)
  "Draw cube wireframe (Vector version)"
  (set-gl-color color)
  (let* ((half-size (v* size 0.5))
         (x (vx position))
         (y (vy position))
         (z (vz position))
         (hx (vx half-size))
         (hy (vy half-size))
         (hz (vz half-size)))
    
    (gl:with-primitive :lines
      ;; Bottom face
      (gl:vertex (- x hx) (- y hy) (- z hz))
      (gl:vertex (+ x hx) (- y hy) (- z hz))
      
      (gl:vertex (+ x hx) (- y hy) (- z hz))
      (gl:vertex (+ x hx) (- y hy) (+ z hz))
      
      (gl:vertex (+ x hx) (- y hy) (+ z hz))
      (gl:vertex (- x hx) (- y hy) (+ z hz))
      
      (gl:vertex (- x hx) (- y hy) (+ z hz))
      (gl:vertex (- x hx) (- y hy) (- z hz))
      
      ;; Top face
      (gl:vertex (- x hx) (+ y hy) (- z hz))
      (gl:vertex (+ x hx) (+ y hy) (- z hz))
      
      (gl:vertex (+ x hx) (+ y hy) (- z hz))
      (gl:vertex (+ x hx) (+ y hy) (+ z hz))
      
      (gl:vertex (+ x hx) (+ y hy) (+ z hz))
      (gl:vertex (- x hx) (+ y hy) (+ z hz))
      
      (gl:vertex (- x hx) (+ y hy) (+ z hz))
      (gl:vertex (- x hx) (+ y hy) (- z hz))
      
      ;; Vertical edges
      (gl:vertex (- x hx) (- y hy) (- z hz))
      (gl:vertex (- x hx) (+ y hy) (- z hz))
      
      (gl:vertex (+ x hx) (- y hy) (- z hz))
      (gl:vertex (+ x hx) (+ y hy) (- z hz))
      
      (gl:vertex (+ x hx) (- y hy) (+ z hz))
      (gl:vertex (+ x hx) (+ y hy) (+ z hz))
      
      (gl:vertex (- x hx) (- y hy) (+ z hz))
      (gl:vertex (- x hx) (+ y hy) (+ z hz)))))

;;; 3D Sphere drawing functions

(defun draw-sphere (center-pos radius color)
  "Draw a sphere"
  (draw-sphere-ex center-pos radius 16 32 color))

(defun draw-sphere-ex (center-pos radius rings slices color)
  "Draw sphere with specified detail level"
  (set-gl-color color)
  (let ((center center-pos))
    (gl:with-primitive :triangles
      (loop for i from 0 below rings do
        (let* ((lat0 (* (/ i rings) +pi+))
               (lat1 (* (/ (1+ i) rings) +pi+))
               (sin-lat0 (sin lat0))
               (cos-lat0 (cos lat0))
               (sin-lat1 (sin lat1))
               (cos-lat1 (cos lat1)))
          (loop for j from 0 below slices do
            (let* ((lng0 (* (/ j slices) 2.0 +pi+))
                   (lng1 (* (/ (1+ j) slices) 2.0 +pi+))
                   (sin-lng0 (sin lng0))
                   (cos-lng0 (cos lng0))
                   (sin-lng1 (sin lng1))
                   (cos-lng1 (cos lng1)))
              
              ;; Calculate vertices
              (let ((x0 (* radius sin-lat0 cos-lng0))
                    (y0 (* radius cos-lat0))
                    (z0 (* radius sin-lat0 sin-lng0))
                    (x1 (* radius sin-lat0 cos-lng1))
                    (y1 (* radius cos-lat0))
                    (z1 (* radius sin-lat0 sin-lng1))
                    (x2 (* radius sin-lat1 cos-lng1))
                    (y2 (* radius cos-lat1))
                    (z2 (* radius sin-lat1 sin-lng1))
                    (x3 (* radius sin-lat1 cos-lng0))
                    (y3 (* radius cos-lat1))
                    (z3 (* radius sin-lat1 sin-lng0)))
                
                ;; Triangle 1
                (gl:normal (/ x0 radius) (/ y0 radius) (/ z0 radius))
                (gl:vertex (+ (vx center) x0) (+ (vy center) y0) (+ (vz center) z0))
                (gl:normal (/ x2 radius) (/ y2 radius) (/ z2 radius))
                (gl:vertex (+ (vx center) x2) (+ (vy center) y2) (+ (vz center) z2))
                (gl:normal (/ x1 radius) (/ y1 radius) (/ z1 radius))
                (gl:vertex (+ (vx center) x1) (+ (vy center) y1) (+ (vz center) z1))
                
                ;; Triangle 2
                (gl:normal (/ x0 radius) (/ y0 radius) (/ z0 radius))
                (gl:vertex (+ (vx center) x0) (+ (vy center) y0) (+ (vz center) z0))
                (gl:normal (/ x3 radius) (/ y3 radius) (/ z3 radius))
                (gl:vertex (+ (vx center) x3) (+ (vy center) y3) (+ (vz center) z3))
                (gl:normal (/ x2 radius) (/ y2 radius) (/ z2 radius))
                (gl:vertex (+ (vx center) x2) (+ (vy center) y2) (+ (vz center) z2)))))))))

(defun draw-sphere-wires (center-pos radius rings slices color)
  "Draw sphere wireframe"
  (set-gl-color color)
  (let ((center center-pos))
    (gl:with-primitive :lines
      ;; Latitude lines
      (loop for i from 0 to rings do
        (let* ((lat (* (/ i rings) +pi+))
               (sin-lat (sin lat))
               (cos-lat (cos lat))
               (y (* radius cos-lat))
               (ring-radius (* radius sin-lat)))
          (loop for j from 0 below slices do
            (let* ((lng0 (* (/ j slices) 2.0 +pi+))
                   (lng1 (* (/ (1+ j) slices) 2.0 +pi+))
                   (x0 (* ring-radius (cos lng0)))
                   (z0 (* ring-radius (sin lng0)))
                   (x1 (* ring-radius (cos lng1)))
                   (z1 (* ring-radius (sin lng1))))
              (gl:vertex (+ (vx center) x0) (+ (vy center) y) (+ (vz center) z0))
              (gl:vertex (+ (vx center) x1) (+ (vy center) y) (+ (vz center) z1))))))
      
      ;; Longitude lines
      (loop for j from 0 below slices do
        (let ((lng (* (/ j slices) 2.0 +pi+)))
          (loop for i from 0 below rings do
            (let* ((lat0 (* (/ i rings) +pi+))
                   (lat1 (* (/ (1+ i) rings) +pi+))
                   (x0 (* radius (sin lat0) (cos lng)))
                   (y0 (* radius (cos lat0)))
                   (z0 (* radius (sin lat0) (sin lng)))
                   (x1 (* radius (sin lat1) (cos lng)))
                   (y1 (* radius (cos lat1)))
                   (z1 (* radius (sin lat1) (sin lng))))
              (gl:vertex (+ (vx center) x0) (+ (vy center) y0) (+ (vz center) z0))
              (gl:vertex (+ (vx center) x1) (+ (vy center) y1) (+ (vz center) z1)))))))))

;;; 3D Cylinder drawing functions

(defun draw-cylinder (position radius-top radius-bottom height slices color)
  "Draw a cylinder"
  (set-gl-color color)
  (let ((half-height (/ height 2.0)))
    (gl:with-primitive :triangles
      ;; Side surface
      (loop for i from 0 below slices do
        (let* ((angle0 (* (/ i slices) 2.0 +pi+))
               (angle1 (* (/ (1+ i) slices) 2.0 +pi+))
               (cos0 (cos angle0))
               (sin0 (sin angle0))
               (cos1 (cos angle1))
               (sin1 (sin angle1)))
          
          ;; Top quad as two triangles
          (let ((x0-top (* radius-top cos0))
                (z0-top (* radius-top sin0))
                (x1-top (* radius-top cos1))
                (z1-top (* radius-top sin1))
                (x0-bottom (* radius-bottom cos0))
                (z0-bottom (* radius-bottom sin0))
                (x1-bottom (* radius-bottom cos1))
                (z1-bottom (* radius-bottom sin1)))
            
            ;; Triangle 1
            (gl:normal cos0 0.0 sin0)
            (gl:vertex (+ (first position) x0-top) (+ (second position) half-height) (+ (third position) z0-top))
            (gl:normal cos1 0.0 sin1)
            (gl:vertex (+ (first position) x1-bottom) (- (second position) half-height) (+ (third position) z1-bottom))
            (gl:normal cos1 0.0 sin1)
            (gl:vertex (+ (first position) x1-top) (+ (second position) half-height) (+ (third position) z1-top))
            
            ;; Triangle 2
            (gl:normal cos0 0.0 sin0)
            (gl:vertex (+ (first position) x0-top) (+ (second position) half-height) (+ (third position) z0-top))
            (gl:normal cos0 0.0 sin0)
            (gl:vertex (+ (first position) x0-bottom) (- (second position) half-height) (+ (third position) z0-bottom))
            (gl:normal cos1 0.0 sin1)
            (gl:vertex (+ (first position) x1-bottom) (- (second position) half-height) (+ (third position) z1-bottom)))))
      
      ;; Top cap (if radius > 0)
      (when (> radius-top 0.0)
        (loop for i from 0 below slices do
          (let* ((angle0 (* (/ i slices) 2.0 +pi+))
                 (angle1 (* (/ (1+ i) slices) 2.0 +pi+))
                 (x0 (* radius-top (cos angle0)))
                 (z0 (* radius-top (sin angle0)))
                 (x1 (* radius-top (cos angle1)))
                 (z1 (* radius-top (sin angle1))))
            (gl:normal 0.0 1.0 0.0)
            (gl:vertex (first position) (+ (second position) half-height) (third position))
            (gl:vertex (+ (first position) x0) (+ (second position) half-height) (+ (third position) z0))
            (gl:vertex (+ (first position) x1) (+ (second position) half-height) (+ (third position) z1)))))
      
      ;; Bottom cap (if radius > 0)
      (when (> radius-bottom 0.0)
        (loop for i from 0 below slices do
          (let* ((angle0 (* (/ i slices) 2.0 +pi+))
                 (angle1 (* (/ (1+ i) slices) 2.0 +pi+))
                 (x0 (* radius-bottom (cos angle0)))
                 (z0 (* radius-bottom (sin angle0)))
                 (x1 (* radius-bottom (cos angle1)))
                 (z1 (* radius-bottom (sin angle1))))
            (gl:normal 0.0 -1.0 0.0)
            (gl:vertex (first position) (- (second position) half-height) (third position))
            (gl:vertex (+ (first position) x1) (- (second position) half-height) (+ (third position) z1))
            (gl:vertex (+ (first position) x0) (- (second position) half-height) (+ (third position) z0))))))))

;;; 3D Plane drawing functions

(defun draw-plane (center-pos size color)
  "Draw a plane (horizontal by default)"
  (set-gl-color color)
  (let* ((cx (if (vec3-p center-pos) (vx3 center-pos) (first center-pos)))
         (cy (if (vec3-p center-pos) (vy3 center-pos) (second center-pos)))
         (cz (if (vec3-p center-pos) (vz3 center-pos) (third center-pos)))
         (sx (if (vec2-p size) (vx2 size) (first size)))
         (sz (if (vec2-p size) (vy2 size) (second size)))
         (half-x (/ sx 2.0))
         (half-z (/ sz 2.0)))
    (gl:with-primitive :triangles
      (gl:normal 0.0 1.0 0.0)
      
      ;; Triangle 1
      (gl:vertex (- cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (+ cz half-z))
      
      ;; Triangle 2
      (gl:vertex (- cx half-x) cy (- cz half-z))
      (gl:vertex (+ cx half-x) cy (+ cz half-z))
      (gl:vertex (- cx half-x) cy (+ cz half-z)))))

;;; Note: draw-grid has been moved to models.lisp to match raylib's rmodels.c structure

;;; 3D Ray drawing functions

(defun draw-ray (ray length color)
  "Draw a 3D ray"
  (set-gl-color color)
  (let* ((start (ray-position ray))
         (direction (ray-direction ray))
         (end-point (v+ start (v* direction length))))
    (gl:with-primitive :lines
      (gl:vertex (vx start) (vy start) (vz start))
      (gl:vertex (vx end-point) (vy end-point) (vz end-point)))))

;;; 3D Line drawing functions

(defun draw-line-3d (start-pos end-pos color)
  "Draw a 3D line between two points"
  (set-gl-color color)
  (gl:with-primitive :lines
    (gl:vertex (vx start-pos) (vy start-pos) (vz start-pos))
    (gl:vertex (vx end-pos) (vy end-pos) (vz end-pos))))

(defun draw-point-3d (position color)
  "Draw a 3D point"
  (set-gl-color color)
  (gl:point-size 4.0)
  (gl:with-primitive :points
    (gl:vertex (vx position) (vy position) (vz position)))
  (gl:point-size 1.0))

;;; 3D Triangle drawing functions

(defun draw-triangle-3d (v1 v2 v3 color)
  "Draw a 3D triangle"
  (set-gl-color color)
  (gl:with-primitive :triangles
    ;; Calculate normal
    (let* ((edge1 (v- v2 v1))
           (edge2 (v- v3 v1))
           (normal (vunit (vc edge1 edge2))))
      (gl:normal (vx normal) (vy normal) (vz normal))
      (gl:vertex (vx v1) (vy v1) (vz v1))
      (gl:vertex (vx v2) (vy v2) (vz v2))
      (gl:vertex (vx v3) (vy v3) (vz v3)))))

;;; Utility functions for 3D transformations

(defun push-matrix ()
  "Push current matrix onto stack"
  (gl:push-matrix))

(defun pop-matrix ()
  "Pop matrix from stack"
  (gl:pop-matrix))

(defun translate-3d (x y z)
  "Apply 3D translation"
  (gl:translate x y z))

(defun rotate-3d (angle x y z)
  "Apply 3D rotation (angle in degrees)"
  (gl:rotate angle x y z))

(defun scale-3d (x y z)
  "Apply 3D scaling"
  (gl:scale x y z))

(defmacro with-matrix (&body body)
  "Execute body with pushed matrix (automatically restored)"
  `(unwind-protect
       (progn
         (push-matrix)
         ,@body)
     (pop-matrix))))