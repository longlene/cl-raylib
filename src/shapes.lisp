(in-package #:cl-raylib)

;;; Helper functions for OpenGL drawing
;;; Note: set-gl-color is now in color.lisp

;;; Basic 2D drawing functions

;;; Pixel drawing
(defun draw-pixel (pos-x pos-y color)
  "Draw a pixel"
  (set-gl-color color)
  (gl:with-primitive :points
    (gl:vertex pos-x pos-y)))

(defun draw-pixel-v (position color)
  "Draw a pixel (Vector version)"
  (let ((x (if (listp position) (first position) (vx position)))
        (y (if (listp position) (second position) (vy position))))
    (draw-pixel x y color)))

;;; Line drawing
(defun draw-line (start-pos-x start-pos-y end-pos-x end-pos-y color)
  "Draw a line"
  (set-gl-color color)
  (gl:with-primitive :lines
    (gl:vertex start-pos-x start-pos-y)
    (gl:vertex end-pos-x end-pos-y)))

(defun draw-line-v (start-pos end-pos color)
  "Draw a line (Vector version)"
  (let ((start-x (if (listp start-pos) (first start-pos) (vx start-pos)))
        (start-y (if (listp start-pos) (second start-pos) (vy start-pos)))
        (end-x (if (listp end-pos) (first end-pos) (vx end-pos)))
        (end-y (if (listp end-pos) (second end-pos) (vy end-pos))))
    (draw-line start-x start-y end-x end-y color)))

(defun draw-line-ex (start-pos end-pos thick color)
  "Draw a line with thickness"
  ;; For thick lines, we'll use line width (limited but simple)
  (gl:line-width thick)
  (draw-line-v start-pos end-pos color)
  (gl:line-width 1.0)) ; Reset to default

(defun draw-line-strip (points color)
  "Draw connected lines"
  (set-gl-color color)
  (when (>= (length points) 2)
    (gl:with-primitive :line-strip
      (loop for point in points do
        (gl:vertex (first point) (second point))))))

(defun draw-line-bezier (start-pos end-pos thick color)
  "Draw line segment with a bezier curve"
  ;; Simplified version - just draw a straight line for now
  ;; TODO: Implement proper bezier curve calculation
  (declare (ignore thick))
  (draw-line-v start-pos end-pos color))

;;; Circle drawing
(defun draw-circle (center-x center-y radius color)
  "Draw a color-filled circle"
  (set-gl-color color)
  (let ((segments 36)) ; Number of segments for circle approximation
    (gl:with-primitive :triangle-fan
      (gl:vertex center-x center-y) ; Center point
      (loop for i from 0 to segments do
        (let ((angle (* 2.0 +pi+ (/ i segments))))
          (gl:vertex (+ center-x (* radius (cos angle)))
                     (+ center-y (* radius (sin angle)))))))))

(defun draw-circle-v (center radius color)
  "Draw a color-filled circle (Vector version)"
  (let ((center-list (if (listp center) 
                         center
                         (list (vx center) (vy center)))))
    (draw-circle (first center-list) (second center-list) radius color)))

(defun draw-circle-lines (center-x center-y radius color)
  "Draw circle outline"
  (set-gl-color color)
  (let ((segments 36))
    (gl:with-primitive :line-loop
      (loop for i from 0 below segments do
        (let ((angle (* 2.0 +pi+ (/ i segments))))
          (gl:vertex (+ center-x (* radius (cos angle)))
                     (+ center-y (* radius (sin angle)))))))))

(defun draw-circle-lines-v (center radius color)
  "Draw circle outline (Vector version)"
  (let ((center-list (if (listp center) 
                         center
                         (list (vx center) (vy center)))))
    (draw-circle-lines (first center-list) (second center-list) radius color)))

(defun draw-circle-sector (center radius start-angle end-angle segments color)
  "Draw a piece of a circle"
  (set-gl-color color)
  (when (> segments 0)
    (let ((angle-step (/ (- end-angle start-angle) segments)))
      (gl:with-primitive :triangle-fan
        (gl:vertex (first center) (second center)) ; Center point
        (loop for i from 0 to segments do
          (let ((angle (+ start-angle (* i angle-step))))
            (gl:vertex (+ (first center) (* radius (cos angle)))
                       (+ (second center) (* radius (sin angle))))))))))

(defun draw-circle-sector-lines (center radius start-angle end-angle segments color)
  "Draw circle sector outline"
  (set-gl-color color)
  (when (> segments 0)
    (let ((angle-step (/ (- end-angle start-angle) segments)))
      (gl:with-primitive :line-strip
        (loop for i from 0 to segments do
          (let ((angle (+ start-angle (* i angle-step))))
            (gl:vertex (+ (first center) (* radius (cos angle)))
                       (+ (second center) (* radius (sin angle))))))))))

(defun draw-circle-gradient (center-x center-y radius color1 color2)
  "Draw a gradient-filled circle"
  ;; Simplified version - just use color1 for now
  ;; TODO: Implement proper gradient rendering
  (declare (ignore color2))
  (draw-circle center-x center-y radius color1))

;;; Rectangle drawing
(defun draw-rectangle (pos-x pos-y width height color)
  "Draw a color-filled rectangle"
  (set-gl-color color)
  (gl:with-primitive :quads
    (gl:vertex pos-x pos-y)
    (gl:vertex (+ pos-x width) pos-y)
    (gl:vertex (+ pos-x width) (+ pos-y height))
    (gl:vertex pos-x (+ pos-y height))))

(defun draw-rectangle-v (position size color)
  "Draw a color-filled rectangle (Vector version)"
  (draw-rectangle (first position) (second position)
                  (first size) (second size) color))

(defun draw-rectangle-rec (rec color)
  "Draw a color-filled rectangle (Rectangle version)"
  ;; Handle both list format (x y width height) and rectangle structure
  (if (listp rec)
      (draw-rectangle (first rec) (second rec) 
                      (third rec) (fourth rec) color)
      (draw-rectangle (rectangle-x rec) (rectangle-y rec) 
                      (rectangle-width rec) (rectangle-height rec) color)))

(defun draw-rectangle-lines (pos-x pos-y width height color)
  "Draw rectangle outline"
  (set-gl-color color)
  (gl:with-primitive :line-loop
    (gl:vertex pos-x pos-y)
    (gl:vertex (+ pos-x width) pos-y)
    (gl:vertex (+ pos-x width) (+ pos-y height))
    (gl:vertex pos-x (+ pos-y height))))

(defun draw-rectangle-pro (rec origin rotation color)
  "Draw a color-filled rectangle with pro parameters"
  ;; Pro version with origin and rotation
  (let ((x (if (listp rec) (first rec) (rectangle-x rec)))
        (y (if (listp rec) (second rec) (rectangle-y rec)))
        (width (if (listp rec) (third rec) (rectangle-width rec)))
        (height (if (listp rec) (fourth rec) (rectangle-height rec)))
        (ox (first origin))
        (oy (second origin)))
    (set-gl-color color)
    (gl:with-pushed-matrix
      (gl:translate (+ x ox) (+ y oy) 0.0)
      (gl:rotate rotation 0.0 0.0 1.0)
      (gl:translate (- ox) (- oy) 0.0)
      (gl:with-primitive :quads
        (gl:vertex 0.0 0.0)
        (gl:vertex width 0.0)
        (gl:vertex width height)
        (gl:vertex 0.0 height)))))

(defun draw-rectangle-gradient-v (pos-x pos-y width height color1 color2)
  "Draw a vertical gradient-filled rectangle"
  ;; Simplified version - just use color1 for now
  ;; TODO: Implement proper gradient rendering
  (declare (ignore color2))
  (draw-rectangle pos-x pos-y width height color1))

(defun draw-rectangle-gradient-h (pos-x pos-y width height color1 color2)
  "Draw a horizontal gradient-filled rectangle"
  ;; Simplified version - just use color1 for now
  ;; TODO: Implement proper gradient rendering
  (declare (ignore color2))
  (draw-rectangle pos-x pos-y width height color1))

(defun draw-rectangle-gradient-ex (rec color1 color2 color3 color4)
  "Draw a gradient-filled rectangle with pro parameters"
  ;; Simplified version - just use color1 for now
  ;; TODO: Implement proper 4-color gradient
  (declare (ignore color2 color3 color4))
  (draw-rectangle-rec rec color1))

(defun draw-rectangle-lines-ex (rec line-thick color)
  "Draw rectangle outline with extended parameters"
  (set-gl-color color)
  (gl:line-width line-thick)
  (let ((x (if (listp rec) (first rec) (rectangle-x rec)))
        (y (if (listp rec) (second rec) (rectangle-y rec)))
        (width (if (listp rec) (third rec) (rectangle-width rec)))
        (height (if (listp rec) (fourth rec) (rectangle-height rec))))
    (gl:with-primitive :line-loop
      (gl:vertex x y)
      (gl:vertex (+ x width) y)
      (gl:vertex (+ x width) (+ y height))
      (gl:vertex x (+ y height))))
  (gl:line-width 1.0))

(defun draw-rectangle-rounded (rec roundness segments color)
  "Draw rectangle with rounded corners"
  ;; Simplified version - just draw regular rectangle for now
  ;; TODO: Implement proper rounded corners with arc segments
  (declare (ignore roundness segments))
  (draw-rectangle-rec rec color))

(defun draw-rectangle-rounded-lines (rec roundness segments line-thick color)
  "Draw rectangle with rounded corners outline"
  ;; Simplified version - just draw regular rectangle outline for now
  ;; TODO: Implement proper rounded corners with arc segments
  (declare (ignore roundness segments))
  (draw-rectangle-lines-ex rec line-thick color))

(defun draw-rectangle-rounded-lines-ex (rec roundness segments line-thick color)
  "Draw rectangle with rounded corners outline extended"
  ;; Simplified version - just draw regular rectangle outline for now
  ;; TODO: Implement proper rounded corners with arc segments
  (declare (ignore roundness segments))
  (draw-rectangle-lines-ex rec line-thick color))

;;; Triangle drawing (following raylib rshapes.c implementation exactly)
(defun draw-triangle (v1 v2 v3 color)
  "Draw a color-filled triangle with three vertices (raylib DrawTriangle) - expects Vector2 structs"
  (set-gl-color color)
  (gl:with-primitive :triangles
    ;; Access Vector2 components directly (v1.x, v1.y in raylib C code)
    (gl:vertex (if (vec2-p v1) (vx2 v1) (first v1)) 
               (if (vec2-p v1) (vy2 v1) (second v1)))
    (gl:vertex (if (vec2-p v2) (vx2 v2) (first v2)) 
               (if (vec2-p v2) (vy2 v2) (second v2)))
    (gl:vertex (if (vec2-p v3) (vx2 v3) (first v3)) 
               (if (vec2-p v3) (vy2 v3) (second v3)))))

(defun draw-triangle-lines (v1 v2 v3 color)
  "Draw triangle outline (raylib DrawTriangleLines) - expects Vector2 structs"
  (set-gl-color color)
  ;; raylib uses RL_LINES (separate line segments), not line-loop
  (gl:with-primitive :lines
    ;; Line v1 -> v2
    (gl:vertex (if (vec2-p v1) (vx2 v1) (first v1)) 
               (if (vec2-p v1) (vy2 v1) (second v1)))
    (gl:vertex (if (vec2-p v2) (vx2 v2) (first v2)) 
               (if (vec2-p v2) (vy2 v2) (second v2)))
    ;; Line v2 -> v3  
    (gl:vertex (if (vec2-p v2) (vx2 v2) (first v2)) 
               (if (vec2-p v2) (vy2 v2) (second v2)))
    (gl:vertex (if (vec2-p v3) (vx2 v3) (first v3)) 
               (if (vec2-p v3) (vy2 v3) (second v3)))
    ;; Line v3 -> v1
    (gl:vertex (if (vec2-p v3) (vx2 v3) (first v3)) 
               (if (vec2-p v3) (vy2 v3) (second v3)))
    (gl:vertex (if (vec2-p v1) (vx2 v1) (first v1)) 
               (if (vec2-p v1) (vy2 v1) (second v1)))))

(defun draw-triangle-fan (points color)
  "Draw a triangle fan"
  (set-gl-color color)
  (when (>= (length points) 3)
    (gl:with-primitive :triangle-fan
      (loop for point in points do
        (gl:vertex (first point) (second point))))))

(defun draw-triangle-strip (points color)
  "Draw a triangle strip"
  (set-gl-color color)
  (when (>= (length points) 3)
    (gl:with-primitive :triangle-strip
      (loop for point in points do
        (gl:vertex (first point) (second point))))))

;;; Ellipse drawing
(defun draw-ellipse (center-x center-y radius-h radius-v color)
  "Draw ellipse"
  (set-gl-color color)
  (let ((segments 36))
    (gl:with-primitive :triangle-fan
      (gl:vertex center-x center-y) ; Center point
      (loop for i from 0 to segments do
        (let ((angle (* 2.0 +pi+ (/ i segments))))
          (gl:vertex (+ center-x (* radius-h (cos angle)))
                     (+ center-y (* radius-v (sin angle)))))))))

(defun draw-ellipse-v (center radius-h radius-v color)
  "Draw ellipse (Vector version)"
  (let ((center-list (if (listp center) 
                         center
                         (list (vx center) (vy center)))))
    (draw-ellipse (first center-list) (second center-list) radius-h radius-v color)))

(defun draw-ellipse-lines (center-x center-y radius-h radius-v color)
  "Draw ellipse outline"
  (set-gl-color color)
  (let ((segments 36))
    (gl:with-primitive :line-loop
      (loop for i from 0 below segments do
        (let ((angle (* 2.0 +pi+ (/ i segments))))
          (gl:vertex (+ center-x (* radius-h (cos angle)))
                     (+ center-y (* radius-v (sin angle)))))))))

(defun draw-ellipse-lines-v (center radius-h radius-v color)
  "Draw ellipse outline (Vector version)"
  (let ((center-list (if (listp center) 
                         center
                         (list (vx center) (vy center)))))
    (draw-ellipse-lines (first center-list) (second center-list) radius-h radius-v color)))

;;; Ring drawing
(defun draw-ring (center inner-radius outer-radius start-angle end-angle segments color)
  "Draw ring"
  (set-gl-color color)
  (when (> segments 0)
    (let ((angle-step (/ (- end-angle start-angle) segments)))
      (gl:with-primitive :triangle-strip
        (loop for i from 0 to segments do
          (let ((angle (+ start-angle (* i angle-step))))
            (gl:vertex (+ (first center) (* inner-radius (cos angle)))
                       (+ (second center) (* inner-radius (sin angle))))
            (gl:vertex (+ (first center) (* outer-radius (cos angle)))
                       (+ (second center) (* outer-radius (sin angle))))))))))

(defun draw-ring-lines (center inner-radius outer-radius start-angle end-angle segments color)
  "Draw ring outline"
  (set-gl-color color)
  (when (> segments 0)
    (let ((angle-step (/ (- end-angle start-angle) segments)))
      ;; Draw inner arc
      (gl:with-primitive :line-strip
        (loop for i from 0 to segments do
          (let ((angle (+ start-angle (* i angle-step))))
            (gl:vertex (+ (first center) (* inner-radius (cos angle)))
                       (+ (second center) (* inner-radius (sin angle)))))))
      ;; Draw outer arc
      (gl:with-primitive :line-strip
        (loop for i from 0 to segments do
          (let ((angle (+ start-angle (* i angle-step))))
            (gl:vertex (+ (first center) (* outer-radius (cos angle)))
                       (+ (second center) (* outer-radius (sin angle))))))))))

;;; Polygon drawing (following raylib rshapes.c implementation exactly)
(defun draw-poly (center sides radius rotation color)
  "Draw a regular polygon (raylib DrawPoly) - expects Vector2 center"
  (when (< sides 3) (setf sides 3)) ; Minimum 3 sides like raylib
  (set-gl-color color)
  ;; Convert rotation from degrees to radians (raylib uses DEG2RAD)
  (let* ((central-angle (* rotation (/ +pi+ 180.0))) ; DEG2RAD conversion
         (angle-step (/ (* 2.0 +pi+) sides))
         (center-x (if (vec2-p center) (vx2 center) (first center)))
         (center-y (if (vec2-p center) (vy2 center) (second center))))
    (gl:with-primitive :triangle-fan
      (gl:vertex center-x center-y) ; Center vertex
      (loop for i from 0 to sides do
        (let ((angle (+ central-angle (* i angle-step))))
          (gl:vertex (+ center-x (* radius (cos angle)))
                     (+ center-y (* radius (sin angle)))))))))

(defun draw-poly-lines (center sides radius rotation color)
  "Draw regular polygon outline (raylib DrawPolyLines) - expects Vector2 center"
  (when (< sides 3) (setf sides 3)) ; Minimum 3 sides like raylib
  (set-gl-color color)
  ;; Convert rotation from degrees to radians (raylib uses DEG2RAD)
  (let* ((central-angle (* rotation (/ +pi+ 180.0))) ; DEG2RAD conversion
         (angle-step (/ (* 2.0 +pi+) sides))
         (center-x (if (vec2-p center) (vx2 center) (first center)))
         (center-y (if (vec2-p center) (vy2 center) (second center))))
    (gl:with-primitive :line-loop
      (loop for i from 0 below sides do
        (let ((angle (+ central-angle (* i angle-step))))
          (gl:vertex (+ center-x (* radius (cos angle)))
                     (+ center-y (* radius (sin angle)))))))))

;;; Utility functions
(defun rectangle (x y width height)
  "Create a rectangle structure (x y width height)"
  (list x y width height))

(defun check-collision-point-rec (point rec)
  "Check if point is inside rectangle"
  (let ((x (if (listp rec) (first rec) (rectangle-x rec)))
        (y (if (listp rec) (second rec) (rectangle-y rec)))
        (width (if (listp rec) (third rec) (rectangle-width rec)))
        (height (if (listp rec) (fourth rec) (rectangle-height rec))))
    (and (>= (first point) x)
         (<= (first point) (+ x width))
         (>= (second point) y)
         (<= (second point) (+ y height)))))

(defun draw-poly-lines-ex (center sides radius rotation line-thick color)
  "Draw a polygon outline of n sides with extended parameters"
  ;; For now, just draw regular polygon lines - ignore thickness
  (declare (ignore line-thick))
  (draw-poly-lines center sides radius rotation color))
