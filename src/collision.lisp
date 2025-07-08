(in-package #:cl-raylib)

;;; Collision Detection Functions
;;; Based on raylib collision detection functionality
;;; Note: Organized here for convenience, but following raylib's stateless design

;;; 2D Collision Detection (matches raylib rshapes.c functionality)

(defun check-collision-point-rec (point rec)
  "Check if point is inside rectangle (matches raylib CheckCollisionPointRec)"
  (and (>= (vx point) (rectangle-x rec))
       (<= (vx point) (+ (rectangle-x rec) (rectangle-width rec)))
       (>= (vy point) (rectangle-y rec))
       (<= (vy point) (+ (rectangle-y rec) (rectangle-height rec)))))

(defun check-collision-recs (rec1 rec2)
  "Check collision between two rectangles (matches raylib CheckCollisionRecs)"
  (and (< (rectangle-x rec1) (+ (rectangle-x rec2) (rectangle-width rec2)))
       (> (+ (rectangle-x rec1) (rectangle-width rec1)) (rectangle-x rec2))
       (< (rectangle-y rec1) (+ (rectangle-y rec2) (rectangle-height rec2)))
       (> (+ (rectangle-y rec1) (rectangle-height rec1)) (rectangle-y rec2))))

(defun check-collision-point-circle (point center radius)
  "Check if point is inside circle (matches raylib CheckCollisionPointCircle)"
  (<= (vlength (v- point center)) radius))

(defun check-collision-circles (center1-or-circle1 radius1-or-circle2 &optional center2 radius2)
  "Check collision between two circles (matches raylib CheckCollisionCircles)
   Can be called with either:
   - (check-collision-circles circle1 circle2) - circle structures
   - (check-collision-circles center1 radius1 center2 radius2) - positions and radii"
  (if (and center2 radius2)
      ;; Four arguments: center1 radius1 center2 radius2
      (<= (vlength (v- center1-or-circle1 radius1-or-circle2)) (+ center2 radius2))
      ;; Two arguments: circle1 circle2 (structures)
      (let ((center1 (circle-center center1-or-circle1))
            (radius1 (circle-radius center1-or-circle1))
            (center2 (circle-center radius1-or-circle2))
            (radius2 (circle-radius radius1-or-circle2)))
        (<= (vlength (v- center1 center2)) (+ radius1 radius2)))))

(defun get-collision-rec (rec1 rec2)
  "Get collision rectangle for two colliding rectangles (matches raylib GetCollisionRec)"
  (if (check-collision-recs rec1 rec2)
      (let ((x1 (max (rectangle-x rec1) (rectangle-x rec2)))
            (y1 (max (rectangle-y rec1) (rectangle-y rec2)))
            (x2 (min (+ (rectangle-x rec1) (rectangle-width rec1))
                     (+ (rectangle-x rec2) (rectangle-width rec2))))
            (y2 (min (+ (rectangle-y rec1) (rectangle-height rec1))
                     (+ (rectangle-y rec2) (rectangle-height rec2)))))
        (make-rectangle :x x1 :y y1 :width (- x2 x1) :height (- y2 y1)))
      (make-rectangle :x 0.0 :y 0.0 :width 0.0 :height 0.0)))

;;; 3D Collision Detection (matches raylib rmodels.c functionality)

(defun check-collision-point-triangle (point a b c)
  "Check if point is inside a triangle in 3D space"
  (let* ((v0 (v- c a))
         (v1 (v- b a))
         (v2 (v- point a))
         (dot00 (v. v0 v0))
         (dot01 (v. v0 v1))
         (dot02 (v. v0 v2))
         (dot11 (v. v1 v1))
         (dot12 (v. v1 v2))
         (inv-denom (/ 1.0 (- (* dot00 dot11) (* dot01 dot01))))
         (u (* (- (* dot11 dot02) (* dot01 dot12)) inv-denom))
         (v (* (- (* dot00 dot12) (* dot01 dot02)) inv-denom)))
    (and (>= u 0) (>= v 0) (<= (+ u v) 1))))

(defun check-collision-point-box (point box-min box-max)
  "Check if point is inside a 3D box (matches raylib CheckCollisionPointBoundingBox)"
  (and (>= (vx point) (vx box-min)) (<= (vx point) (vx box-max))
       (>= (vy point) (vy box-min)) (<= (vy point) (vy box-max))
       (>= (vz point) (vz box-min)) (<= (vz point) (vz box-max))))

;;; Ray Collision Detection

(defun get-ray-collision-sphere (ray center radius)
  "Get ray collision info with sphere (matches raylib GetRayCollisionSphere)"
  (let* ((ray-to-center (v- center (ray-position ray)))
         (ray-dir (ray-direction ray))
         (closest-point (v. ray-to-center ray-dir))
         (closest-on-ray (if (< closest-point 0.0)
                             (ray-position ray)
                             (v+ (ray-position ray) (v* ray-dir closest-point))))
         (distance-to-center (vlength (v- center closest-on-ray))))
    (if (<= distance-to-center radius)
        (let* ((distance-to-sphere (- closest-point (sqrt (- (* radius radius) 
                                                            (* distance-to-center distance-to-center)))))
               (hit-point (v+ (ray-position ray) (v* ray-dir distance-to-sphere))))
          (make-ray-collision :hit t :distance distance-to-sphere :point hit-point :normal (vunit (v- hit-point center))))
        (make-ray-collision :hit nil :distance 0.0 :point (vec3 0 0 0) :normal (vec3 0 0 0)))))

(defun get-ray-collision-box (ray box-or-min &optional box-max)
  "Get ray collision info with box (matches raylib GetRayCollisionBox)"
  (let* ((box-min (if box-max box-or-min (bounding-box-min box-or-min)))
         (box-max (or box-max (bounding-box-max box-or-min)))
         (ray-pos (ray-position ray))
         (ray-dir (ray-direction ray))
         (t-min-x (/ (- (vx box-min) (vx ray-pos)) (vx ray-dir)))
         (t-max-x (/ (- (vx box-max) (vx ray-pos)) (vx ray-dir)))
         (t-min-y (/ (- (vy box-min) (vy ray-pos)) (vy ray-dir)))
         (t-max-y (/ (- (vy box-max) (vy ray-pos)) (vy ray-dir)))
         (t-min-z (/ (- (vz box-min) (vz ray-pos)) (vz ray-dir)))
         (t-max-z (/ (- (vz box-max) (vz ray-pos)) (vz ray-dir))))
    
    (when (> t-min-x t-max-x) (rotatef t-min-x t-max-x))
    (when (> t-min-y t-max-y) (rotatef t-min-y t-max-y))
    (when (> t-min-z t-max-z) (rotatef t-min-z t-max-z))
    
    (let ((t-min (max t-min-x t-min-y t-min-z))
          (t-max (min t-max-x t-max-y t-max-z)))
      
      (if (and (>= t-max 0) (<= t-min t-max))
          (let* ((t-hit (if (>= t-min 0) t-min t-max))
                 (hit-point (v+ ray-pos (v* ray-dir t-hit)))
                 (normal (cond
                          ((= t-hit t-min-x) (vec3 (if (< (vx ray-dir) 0) 1 -1) 0 0))
                          ((= t-hit t-max-x) (vec3 (if (> (vx ray-dir) 0) 1 -1) 0 0))
                          ((= t-hit t-min-y) (vec3 0 (if (< (vy ray-dir) 0) 1 -1) 0))
                          ((= t-hit t-max-y) (vec3 0 (if (> (vy ray-dir) 0) 1 -1) 0))
                          ((= t-hit t-min-z) (vec3 0 0 (if (< (vz ray-dir) 0) 1 -1)))
                          (t (vec3 0 0 (if (> (vz ray-dir) 0) 1 -1))))))
            (make-ray-collision :hit t :distance t-hit :point hit-point :normal normal))
          (make-ray-collision :hit nil :distance 0.0 :point (vec3 0 0 0) :normal (vec3 0 0 0))))))

;;; Geometry helper functions for collision detection demos

;;; 2D geometry helpers

(defun make-circle-at (x y radius)
  "Create a circle at specified position with radius"
  (make-circle :center (vec2 (float x) (float y)) :radius (float radius)))

(defun make-aabb-from-center-size (center-x center-y width height)
  "Create an AABB from center point and size"
  (let ((center-x-f (float center-x))
        (center-y-f (float center-y))
        (half-width (/ (float width) 2.0))
        (half-height (/ (float height) 2.0)))
    (make-aabb :min (vec2 (- center-x-f half-width) (- center-y-f half-height))
               :max (vec2 (+ center-x-f half-width) (+ center-y-f half-height)))))

(defun get-aabb-center (aabb)
  "Get the center point of an AABB"
  (let ((min-pt (aabb-min aabb))
        (max-pt (aabb-max aabb)))
    (vec2 (/ (+ (vx min-pt) (vx max-pt)) 2.0)
          (/ (+ (vy min-pt) (vy max-pt)) 2.0))))

(defun get-aabb-width (aabb)
  "Get the width of an AABB"
  (let ((min-pt (aabb-min aabb))
        (max-pt (aabb-max aabb)))
    (- (vx max-pt) (vx min-pt))))

(defun get-aabb-height (aabb)
  "Get the height of an AABB"
  (let ((min-pt (aabb-min aabb))
        (max-pt (aabb-max aabb)))
    (- (vy max-pt) (vy min-pt))))

;;; 3D geometry helpers

(defun make-sphere-at (x y z radius)
  "Create a sphere at specified position with radius"
  (make-sphere :center (vec3 (float x) (float y) (float z)) :radius (float radius)))

(defun make-aabb3d-from-center-size (center-x center-y center-z width height depth)
  "Create a 3D AABB from center point and size"
  (let ((center-x-f (float center-x))
        (center-y-f (float center-y))
        (center-z-f (float center-z))
        (half-width (/ (float width) 2.0))
        (half-height (/ (float height) 2.0))
        (half-depth (/ (float depth) 2.0)))
    (make-aabb3d :min (vec3 (- center-x-f half-width) (- center-y-f half-height) (- center-z-f half-depth))
                 :max (vec3 (+ center-x-f half-width) (+ center-y-f half-height) (+ center-z-f half-depth)))))

;;; Enhanced collision detection functions

(defun check-collision-circle-rectangle (circle rect)
  "Check collision between circle and rectangle/AABB"
  (let* ((circle-center (circle-center circle))
         (circle-radius (circle-radius circle))
         ;; Handle different rectangle formats (list, rectangle struct, or AABB struct)
         (rect-x (cond 
                   ((listp rect) (first rect))
                   ((aabb-p rect) (vx (aabb-min rect)))
                   (t (rectangle-x rect))))
         (rect-y (cond 
                   ((listp rect) (second rect))
                   ((aabb-p rect) (vy (aabb-min rect)))
                   (t (rectangle-y rect))))
         (rect-w (cond 
                   ((listp rect) (third rect))
                   ((aabb-p rect) (- (vx (aabb-max rect)) (vx (aabb-min rect))))
                   (t (rectangle-width rect))))
         (rect-h (cond 
                   ((listp rect) (fourth rect))
                   ((aabb-p rect) (- (vy (aabb-max rect)) (vy (aabb-min rect))))
                   (t (rectangle-height rect))))
         ;; Find the closest point on rectangle to circle center
         (closest-x (clamp (vx circle-center) rect-x (+ rect-x rect-w)))
         (closest-y (clamp (vy circle-center) rect-y (+ rect-y rect-h)))
         ;; Calculate distance from circle center to closest point
         (distance-sq (+ (* (- (vx circle-center) closest-x) (- (vx circle-center) closest-x))
                        (* (- (vy circle-center) closest-y) (- (vy circle-center) closest-y)))))
    (<= distance-sq (* circle-radius circle-radius))))

(defun check-collision-line-rectangle (line rect)
  "Check collision between line segment and rectangle"
  (declare (ignore line rect))
  ;; Simplified implementation - always return false for now
  ;; TODO: Implement proper line-rectangle intersection
  nil)

(defun check-collision-sphere-aabb3d (sphere aabb3d)
  "Check collision between sphere and 3D AABB"
  (let* ((sphere-center (sphere-center sphere))
         (sphere-radius (sphere-radius sphere))
         (aabb-min (aabb3d-min aabb3d))
         (aabb-max (aabb3d-max aabb3d))
         ;; Find closest point on AABB to sphere center
         (closest-x (clamp (vx sphere-center) (vx aabb-min) (vx aabb-max)))
         (closest-y (clamp (vy sphere-center) (vy aabb-min) (vy aabb-max)))
         (closest-z (clamp (vz sphere-center) (vz aabb-min) (vz aabb-max)))
         ;; Calculate squared distance
         (dx (- (vx sphere-center) closest-x))
         (dy (- (vy sphere-center) closest-y))
         (dz (- (vz sphere-center) closest-z))
         (distance-sq (+ (* dx dx) (* dy dy) (* dz dz))))
    (<= distance-sq (* sphere-radius sphere-radius))))

(defun check-collision-rectangle-rectangle (rect1 rect2)
  "Check collision between two rectangles (alias for check-collision-recs)"
  (check-collision-recs rect1 rect2))

;;; Legacy compatibility functions (for existing demos)
;;; Note: These are no-ops since raylib collision system is stateless

(defun init-collision-system ()
  "Initialize collision detection system (no-op, matches raylib stateless design)"
  ;; No initialization needed - all collision functions are stateless
  (trace-log-info "COLLISION: Collision functions ready (stateless design)"))

(defun cleanup-collision-system ()
  "Cleanup collision detection system (no-op, matches raylib stateless design)"
  ;; No cleanup needed - all collision functions are stateless
  (trace-log-info "COLLISION: Collision cleanup (no-op)"))
