(in-package #:cl-raylib)

;;;; Port of raylib rshapes.c - Basic functions to draw 2d shapes and check collisions
;;;;
;;;; NOTE: Where rshapes.c provides both a SUPPORT_QUADS_DRAW_MODE (RL_QUADS) and a
;;;; RL_TRIANGLES implementation, the RL_TRIANGLES one is ported; functions that only
;;;; have an RL_QUADS implementation use the shapes texture like raylib does.
;;;; SUPPORT_SPLINE_MITERS is disabled (raylib default).
;;;;
;;;; Vector2 arguments accept 3d-vectors vec2 or (x y) lists, Rectangle arguments
;;;; accept rectangle structs or (x y width height) lists, colors accept color
;;;; lists or color keywords.

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(defconstant +smooth-circle-error-rate+ 0.5 "Circle error rate")
(defconstant +spline-segment-divisions+ 24 "Spline segment divisions")

;;;----------------------------------------------------------------------------------
;;; Global Variables Definition
;;;----------------------------------------------------------------------------------
(defvar *tex-shapes* nil "Texture used on shapes drawing (NIL: rlgl default white pixel)")
(defvar *tex-shapes-rec* (make-rectangle :x 0.0 :y 0.0 :width 1.0 :height 1.0)
  "Texture source rectangle used on shapes drawing")

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions
;;;----------------------------------------------------------------------------------
(declaim (inline %vertex %polar))

(defun %color (color)
  "rlColor4ub() from a color list or keyword"
  (destructuring-bind (r g b a) (keyword-to-color color)
    (rl-color4ub r g b a)))

(defun %vertex (x y)
  (rl-vertex2f (float x 1.0) (float y 1.0)))

(defun %polar (cx cy angle radius-h &optional (radius-v radius-h))
  "rlVertex2f() at ANGLE degrees on the circle/ellipse centered at (CX, CY)"
  (%vertex (+ cx (* (cos (* +deg2rad+ angle)) radius-h))
           (+ cy (* (sin (* +deg2rad+ angle)) radius-v))))

(defun %points (points)
  "Points sequence as a simple-vector"
  (coerce points 'simple-vector))

(defun %half-thick-size (thick dx dy)
  "0.5*thick/sqrtf(dx*dx + dy*dy), 0 for a degenerate segment (C would produce NaN)"
  (let ((len (sqrt (+ (* dx dx) (* dy dy)))))
    (if (zerop len) 0.0 (/ (* 0.5 thick) len))))

(defun %shapes-texcoords ()
  "Return u0 v0 u1 v1 of the shapes texture rectangle"
  (let ((tex (get-shapes-texture)))
    (multiple-value-bind (x y w h) (%rec *tex-shapes-rec*)
      (let ((tw (float (texture-width tex) 1.0))
            (th (float (texture-height tex) 1.0)))
        (values (/ x tw) (/ y th) (/ (+ x w) tw) (/ (+ y h) th))))))

(defun %segments-for-arc (arc radius segments min-segments)
  "Calculate the number of segments needed to draw a smooth ARC (degrees) of RADIUS"
  (if (>= segments min-segments)
      segments
      ;; Calculate the maximum angle between segments based on the error rate (usually 0.5f)
      (let ((th (acos (- (* 2 (expt (- 1 (/ +smooth-circle-error-rate+ radius)) 2)) 1))))
        (if (or (complexp th) (<= th 0.0))
            min-segments
            (let ((n (ceiling (/ (* arc (/ (* 2 +pi+) th)) 360.0))))
              (if (<= n 0) min-segments n))))))

(defun %matrix-transform-scale ()
  "Return m0 and m5 of the current transform matrix (rlGetMatrixTransform)"
  (handler-case
      (let ((m (gl:get-float :modelview-matrix)))
        (values (if (zerop (aref m 0)) 1.0 (aref m 0))
                (if (zerop (aref m 5)) 1.0 (aref m 5))))
    (error () (values 1.0 1.0))))

(defun %ease-cubic-in-out (time b c d)
  "Cubic easing in-out (used by draw-line-bezier only)"
  (let ((time (/ time (* 0.5 d))))
    (if (< time 1)
        (+ (* 0.5 c time time time) b)
        (let ((time (- time 2)))
          (+ (* 0.5 c (+ (* time time time) 2.0)) b)))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

;; Set texture and rectangle to be used on shapes drawing
;; NOTE: It can be useful when using basic shapes and one single font,
;; defining a font char white rectangle would allow drawing everything in a single draw call
(defun set-shapes-texture (texture rec)
  "Set texture and rectangle to be used on shapes drawing"
  (multiple-value-bind (x y w h) (%rec rec)
    ;; Reset texture to default pixel if required
    (if (or (null texture) (zerop (texture-id texture)) (zerop w) (zerop h))
        (setf *tex-shapes* nil
              *tex-shapes-rec* (make-rectangle :x 0.0 :y 0.0 :width 1.0 :height 1.0))
        (setf *tex-shapes* texture
              *tex-shapes-rec* (make-rectangle :x x :y y :width w :height h)))))

(defun get-shapes-texture ()
  "Get texture that is used for shapes drawing"
  (or *tex-shapes*
      (make-texture :id (rl-get-texture-id-default) :width 1 :height 1 :mipmaps 1
                    :format +pixelformat-uncompressed-r8g8b8a8+)))

(defun get-shapes-texture-rectangle ()
  "Get texture source rectangle that is used for shapes drawing"
  (copy-rectangle *tex-shapes-rec*))

(defun draw-pixel (pos-x pos-y color)
  "Draw a pixel"
  (draw-pixel-v (vec2 (float pos-x 1.0) (float pos-y 1.0)) color))

(defun draw-pixel-v (position color)
  "Draw a pixel (Vector version)"
  (let ((x (%x position)) (y (%y position)))
    (rl-begin +rl-triangles+)
    (%color color)
    (%vertex x y)
    (%vertex x (+ y 1))
    (%vertex (+ x 1) y)

    (%vertex (+ x 1) y)
    (%vertex x (+ y 1))
    (%vertex (+ x 1) (+ y 1))
    (rl-end)))

(defun draw-line (start-pos-x start-pos-y end-pos-x end-pos-y color)
  "Draw a line (using gl lines)"
  (rl-begin +rl-lines+)
  (%color color)
  (%vertex start-pos-x start-pos-y)
  (%vertex end-pos-x end-pos-y)
  (rl-end))

(defun draw-line-ex (start-pos end-pos thick color)
  "Draw a line defining thickness"
  (let* ((dx (- (%x end-pos) (%x start-pos)))
         (dy (- (%y end-pos) (%y start-pos)))
         (len (sqrt (+ (* dx dx) (* dy dy)))))
    (when (and (> len 0) (> thick 0))
      (let* ((scale (/ thick (* 2 len)))
             (rx (* (- scale) dy))
             (ry (* scale dx)))
        (draw-triangle-strip
         (vector (vec2 (- (%x start-pos) rx) (- (%y start-pos) ry))
                 (vec2 (+ (%x start-pos) rx) (+ (%y start-pos) ry))
                 (vec2 (- (%x end-pos) rx) (- (%y end-pos) ry))
                 (vec2 (+ (%x end-pos) rx) (+ (%y end-pos) ry)))
         4 color)))))

(defun draw-line-v (start-pos end-pos color)
  "Draw a line (using gl lines)"
  (rl-begin +rl-lines+)
  (%color color)
  (%vertex (%x start-pos) (%y start-pos))
  (%vertex (%x end-pos) (%y end-pos))
  (rl-end))

(defun draw-line-strip (points point-count color)
  "Draw lines sequence (using gl lines)"
  (when (>= point-count 2)              ; Security check
    (let ((points (%points points)))
      (rl-begin +rl-lines+)
      (%color color)
      (loop for i from 0 below (1- point-count)
            for p = (svref points i)
            for q = (svref points (1+ i))
            do (%vertex (%x p) (%y p))
               (%vertex (%x q) (%y q)))
      (rl-end))))

(defun draw-line-bezier (start-pos end-pos thick color)
  "Draw line using cubic-bezier spline, in-out interpolation, no control points"
  (let* ((n +spline-segment-divisions+)
         (points (make-array (+ (* 2 n) 2)))
         (sx (%x start-pos)) (sy (%y start-pos))
         (ex (%x end-pos)) (ey (%y end-pos))
         (prev-x sx) (prev-y sy))
    (loop for i from 1 to n
          do (let* (;; Cubic easing in-out
                    ;; NOTE: Easing is calculated only for y position value
                    (cur-y (%ease-cubic-in-out (float i) sy (- ey sy) (float n)))
                    (cur-x (+ prev-x (/ (- ex sx) (float n))))
                    (dy (- cur-y prev-y))
                    (dx (- cur-x prev-x))
                    (size (%half-thick-size thick dx dy)))
               (when (= i 1)
                 (setf (aref points 0) (vec2 (+ prev-x (* dy size)) (- prev-y (* dx size)))
                       (aref points 1) (vec2 (- prev-x (* dy size)) (+ prev-y (* dx size)))))
               (setf (aref points (1+ (* 2 i))) (vec2 (- cur-x (* dy size)) (+ cur-y (* dx size)))
                     (aref points (* 2 i)) (vec2 (+ cur-x (* dy size)) (- cur-y (* dx size))))
               (setf prev-x cur-x prev-y cur-y)))
    (draw-triangle-strip points (+ (* 2 n) 2) color)))

(defun draw-line-dashed (start-pos end-pos dash-size space-size color)
  "Draw a dashed line"
  (let* ((sx (%x start-pos)) (sy (%y start-pos))
         ;; Calculate the vector and length of the line
         (dx (- (%x end-pos) sx))
         (dy (- (%y end-pos) sy))
         (line-length (sqrt (+ (* dx dx) (* dy dy)))))
    ;; If the line is too short for dashing or dash size is invalid, draw a solid thick line
    (if (or (< line-length (+ dash-size space-size)) (<= dash-size 0))
        (draw-line-v start-pos end-pos color)
        ;; Calculate the normalized direction vector of the line
        (let* ((inv-line-length (/ 1.0 line-length))
               (dir-x (* dx inv-line-length))
               (dir-y (* dy inv-line-length))
               (cur-x sx) (cur-y sy)
               (distance-traveled 0.0))
          (rl-begin +rl-lines+)
          (%color color)
          (loop while (< distance-traveled line-length)
                do (let ((dash-end-dist (min (+ distance-traveled dash-size) line-length)))
                     ;; Draw the dash segment
                     (%vertex cur-x cur-y)
                     (%vertex (+ sx (* dash-end-dist dir-x)) (+ sy (* dash-end-dist dir-y)))
                     ;; Update the distance traveled and move the current position for the next dash
                     (setf distance-traveled (+ dash-end-dist space-size)
                           cur-x (+ sx (* distance-traveled dir-x))
                           cur-y (+ sy (* distance-traveled dir-y)))))
          (rl-end)))))

;; NOTE: Vertex must be provided in counter-clockwise order
(defun draw-triangle (v1 v2 v3 color)
  "Draw a color-filled triangle (vertex in counter-clockwise order!)"
  (draw-triangle-gradient v1 v2 v3 color color color))

(defun draw-triangle-gradient (v1 v2 v3 c1 c2 c3)
  "Draw triangle with interpolated colors (vertex in counter-clockwise order!)"
  (rl-begin +rl-triangles+)
  (%color c1)
  (%vertex (%x v1) (%y v1))
  (%color c2)
  (%vertex (%x v2) (%y v2))
  (%color c3)
  (%vertex (%x v3) (%y v3))
  (rl-end))

(defun draw-triangle-lines (v1 v2 v3 color)
  "Draw triangle outline (vertex in counter-clockwise order!)"
  (rl-begin +rl-lines+)
  (%color color)
  (%vertex (%x v1) (%y v1))
  (%vertex (%x v2) (%y v2))

  (%vertex (%x v2) (%y v2))
  (%vertex (%x v3) (%y v3))

  (%vertex (%x v3) (%y v3))
  (%vertex (%x v1) (%y v1))
  (rl-end))

(defun draw-triangle-lines-ex (v1 v2 v3 thick color)
  "Draw triangle outline with line thickness (vertex in counter-clockwise order!)"
  ;; The exterior points are v1-3, the interior points are v4-6, and the exterior edges are e1-3
  (let* ((x1 (%x v1)) (y1 (%y v1))
         (x2 (%x v2)) (y2 (%y v2))
         (x3 (%x v3)) (y3 (%y v3))
         (e1-length (sqrt (+ (expt (- x2 x3) 2) (expt (- y2 y3) 2))))
         (e2-length (sqrt (+ (expt (- x3 x1) 2) (expt (- y3 y1) 2))))
         (e3-length (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
         (perimeter (+ e1-length e2-length e3-length))
         (semiperimeter (/ perimeter 2.0)))
    (when (zerop perimeter) (return-from draw-triangle-lines-ex nil))
    (let* (;; The incenter of a triangle is equidistant from each edge, which is useful for drawing a nice looking outline
           (incenter-x (/ (+ (* e1-length x1) (* e2-length x2) (* e3-length x3)) perimeter))
           (incenter-y (/ (+ (* e1-length y1) (* e2-length y2) (* e3-length y3)) perimeter))
           ;; The inradius of a triangle is the radius of the biggest circle that can fit inside of said triangle
           (inradius (sqrt (max 0.0 (/ (* (- semiperimeter e1-length) (- semiperimeter e2-length)
                                          (- semiperimeter e3-length))
                                       semiperimeter))))
           ;; The triangle (v1, v2, v3) will be scaled by this to get (v4, v5, v6)
           (scale (if (zerop inradius) 0.0 (- 1.0 (/ thick inradius)))))
      (if (<= scale 0.0)
          ;; Just a filled-in triangle
          (draw-triangle v1 v2 v3 color)
          ;; In order for the scaling to be correct, the incenter has to be at the origin (0, 0) when scaling
          (let ((x4 (+ incenter-x (* (- x1 incenter-x) scale))) (y4 (+ incenter-y (* (- y1 incenter-y) scale)))
                (x5 (+ incenter-x (* (- x2 incenter-x) scale))) (y5 (+ incenter-y (* (- y2 incenter-y) scale)))
                (x6 (+ incenter-x (* (- x3 incenter-x) scale))) (y6 (+ incenter-y (* (- y3 incenter-y) scale))))
            ;; Swap the vertices so the winding order is correct
            (when (< thick 0.0)
              (rotatef x1 x4) (rotatef y1 y4)
              (rotatef x2 x5) (rotatef y2 y5)
              (rotatef x3 x6) (rotatef y3 y6))
            (rl-begin +rl-triangles+)
            (%color color)
            ;; Edge 3
            (%vertex x1 y1) (%vertex x2 y2) (%vertex x4 y4)
            (%vertex x2 y2) (%vertex x5 y5) (%vertex x4 y4)
            ;; Edge 1
            (%vertex x2 y2) (%vertex x3 y3) (%vertex x5 y5)
            (%vertex x3 y3) (%vertex x6 y6) (%vertex x5 y5)
            ;; Edge 2
            (%vertex x3 y3) (%vertex x1 y1) (%vertex x4 y4)
            (%vertex x3 y3) (%vertex x4 y4) (%vertex x6 y6)
            (rl-end))))))

;; NOTE: First vertex provided is the center, shared by all triangles
;; By default, following vertex should be provided in counter-clockwise order
(defun draw-triangle-fan (points point-count color)
  "Draw a triangle fan defined by points (first vertex is the center)"
  (when (>= point-count 3)
    (let ((points (%points points)))
      (rl-set-texture (texture-id (get-shapes-texture)))
      (multiple-value-bind (u0 v0 u1 v1) (%shapes-texcoords)
        (rl-begin +rl-quads+)
        (%color color)
        (loop with p0 = (svref points 0)
              for i from 1 below (1- point-count)
              for pa = (svref points i)
              for pb = (svref points (1+ i))
              do (rl-tex-coord2f u0 v0)
                 (%vertex (%x p0) (%y p0))
                 (rl-tex-coord2f u0 v1)
                 (%vertex (%x pa) (%y pa))
                 (rl-tex-coord2f u1 v1)
                 (%vertex (%x pb) (%y pb))
                 (rl-tex-coord2f u1 v0)
                 (%vertex (%x pb) (%y pb)))
        (rl-end))
      (rl-set-texture 0))))

;; NOTE: Every new vertex connects with previous two
(defun draw-triangle-strip (points point-count color)
  "Draw a triangle strip defined by points"
  (when (>= point-count 3)
    (let ((points (%points points)))
      (rl-begin +rl-triangles+)
      (%color color)
      (loop for i from 2 below point-count
            for a = (svref points i)
            for b = (svref points (- i 1))
            for c = (svref points (- i 2))
            do (if (evenp i)
                   (progn (%vertex (%x a) (%y a)) (%vertex (%x c) (%y c)) (%vertex (%x b) (%y b)))
                   (progn (%vertex (%x a) (%y a)) (%vertex (%x b) (%y b)) (%vertex (%x c) (%y c)))))
      (rl-end))))

(defun draw-rectangle (pos-x pos-y width height color)
  "Draw a color-filled rectangle"
  (draw-rectangle-v (vec2 (float pos-x 1.0) (float pos-y 1.0))
                    (vec2 (float width 1.0) (float height 1.0))
                    color))

;; NOTE: On OpenGL 3.3 and ES2 using QUADS to avoid drawing order issues
(defun draw-rectangle-v (position size color)
  "Draw a color-filled rectangle (Vector version)"
  (draw-rectangle-pro (make-rectangle :x (%x position) :y (%y position) :width (%x size) :height (%y size))
                      (vec2 0.0 0.0) 0.0 color))

(defun draw-rectangle-rec (rec color)
  "Draw a color-filled rectangle"
  (draw-rectangle-pro rec (vec2 0.0 0.0) 0.0 color))

(defun draw-rectangle-pro (rec origin rotation color)
  "Draw a color-filled rectangle with pro parameters"
  (multiple-value-bind (rx ry rw rh) (%rec rec)
    (let ((ox (%x origin)) (oy (%y origin))
          tl-x tl-y tr-x tr-y bl-x bl-y br-x br-y)
      ;; Only calculate rotation if needed
      (if (zerop rotation)
          (let ((x (- rx ox)) (y (- ry oy)))
            (setf tl-x x tl-y y
                  tr-x (+ x rw) tr-y y
                  bl-x x bl-y (+ y rh)
                  br-x (+ x rw) br-y (+ y rh)))
          (let ((sin-r (sin (* rotation +deg2rad+)))
                (cos-r (cos (* rotation +deg2rad+)))
                (dx (- ox)) (dy (- oy)))
            (setf tl-x (+ rx (* dx cos-r) (- (* dy sin-r)))
                  tl-y (+ ry (* dx sin-r) (* dy cos-r))
                  tr-x (+ rx (* (+ dx rw) cos-r) (- (* dy sin-r)))
                  tr-y (+ ry (* (+ dx rw) sin-r) (* dy cos-r))
                  bl-x (+ rx (* dx cos-r) (- (* (+ dy rh) sin-r)))
                  bl-y (+ ry (* dx sin-r) (* (+ dy rh) cos-r))
                  br-x (+ rx (* (+ dx rw) cos-r) (- (* (+ dy rh) sin-r)))
                  br-y (+ ry (* (+ dx rw) sin-r) (* (+ dy rh) cos-r)))))
      (rl-begin +rl-triangles+)
      (%color color)
      (%vertex tl-x tl-y)
      (%vertex bl-x bl-y)
      (%vertex tr-x tr-y)

      (%vertex tr-x tr-y)
      (%vertex bl-x bl-y)
      (%vertex br-x br-y)
      (rl-end))))

(defun draw-rectangle-gradient-v (pos-x pos-y width height top bottom)
  "Draw a vertical-gradient-filled rectangle"
  (draw-rectangle-gradient-ex (make-rectangle :x (float pos-x 1.0) :y (float pos-y 1.0)
                                              :width (float width 1.0) :height (float height 1.0))
                              top bottom bottom top))

(defun draw-rectangle-gradient-h (pos-x pos-y width height left right)
  "Draw a horizontal-gradient-filled rectangle"
  (draw-rectangle-gradient-ex (make-rectangle :x (float pos-x 1.0) :y (float pos-y 1.0)
                                              :width (float width 1.0) :height (float height 1.0))
                              left left right right))

(defun draw-rectangle-gradient-ex (rec col1 col2 col3 col4)
  "Draw a gradient-filled rectangle with custom vertex colors, counter-clockwise color order"
  (multiple-value-bind (x y w h) (%rec rec)
    (rl-set-texture (texture-id (get-shapes-texture)))
    (multiple-value-bind (u0 v0 u1 v1) (%shapes-texcoords)
      (rl-begin +rl-quads+)
      (rl-normal3f 0.0 0.0 1.0)
      (%color col1)
      (rl-tex-coord2f u0 v0)
      (%vertex x y)
      (%color col2)
      (rl-tex-coord2f u0 v1)
      (%vertex x (+ y h))
      (%color col3)
      (rl-tex-coord2f u1 v1)
      (%vertex (+ x w) (+ y h))
      (%color col4)
      (rl-tex-coord2f u1 v0)
      (%vertex (+ x w) y)
      (rl-end))
    (rl-set-texture 0)))

;; WARNING: All Draw*Lines() functions use RL_LINES for drawing,
;; it implies flushing the current batch and changing draw mode to RL_LINES
;; but it solves another issue: https://github.com/raysan5/raylib/issues/3884
(defun draw-rectangle-lines (pos-x pos-y width height color)
  "Draw rectangle outline"
  (multiple-value-bind (m0 m5) (%matrix-transform-scale)
    (let ((x-offset (/ 0.5 m0))
          (y-offset (/ 0.5 m5)))
      (rl-begin +rl-lines+)
      (%color color)
      (%vertex (+ pos-x x-offset) (+ pos-y y-offset))
      (%vertex (- (+ pos-x width) x-offset) (+ pos-y y-offset))

      (%vertex (- (+ pos-x width) x-offset) (+ pos-y y-offset))
      (%vertex (- (+ pos-x width) x-offset) (- (+ pos-y height) y-offset))

      (%vertex (- (+ pos-x width) x-offset) (- (+ pos-y height) y-offset))
      (%vertex (+ pos-x x-offset) (- (+ pos-y height) y-offset))

      (%vertex (+ pos-x x-offset) (- (+ pos-y height) y-offset))
      (%vertex (+ pos-x x-offset) (+ pos-y y-offset))
      (rl-end))))

(defun draw-rectangle-lines-ex (rec line-thick color)
  "Draw rectangle outline with extended parameters"
  (multiple-value-bind (x y w h) (%rec rec)
    (let ((thick (float line-thick 1.0)))
      (when (or (> thick (/ w 2)) (> thick (/ h 2)))
        (cond ((>= w h) (setf thick (/ h 2)))
              ((<= w h) (setf thick (/ w 2)))))
      (if (> thick 0.0)
          ;; When rec = { x, y, 8.0f, 6.0f } and thick = 2, the following
          ;; four rectangles are drawn ([T]op, [B]ottom, [L]eft, [R]ight):
          ;;   TTTTTTTT
          ;;   TTTTTTTT
          ;;   LL    RR
          ;;   LL    RR
          ;;   BBBBBBBB
          ;;   BBBBBBBB
          (progn
            (draw-rectangle-rec (make-rectangle :x x :y y :width w :height thick) color)
            (draw-rectangle-rec (make-rectangle :x x :y (+ (- y thick) h) :width w :height thick) color)
            (draw-rectangle-rec (make-rectangle :x x :y (+ y thick) :width thick :height (- h (* thick 2.0))) color)
            (draw-rectangle-rec (make-rectangle :x (+ (- x thick) w) :y (+ y thick) :width thick
                                                :height (- h (* thick 2.0)))
                                color))
          ;; When thick is negative, the outline is drawn outside the rectangle
          (let ((thick (- thick)))
            (draw-rectangle-rec (make-rectangle :x (- x thick) :y (- y thick) :width (+ w (* thick 2.0)) :height thick) color)
            (draw-rectangle-rec (make-rectangle :x (- x thick) :y (+ y h) :width (+ w (* thick 2.0)) :height thick) color)
            (draw-rectangle-rec (make-rectangle :x (- x thick) :y y :width thick :height h) color)
            (draw-rectangle-rec (make-rectangle :x (+ x w) :y y :width thick :height h) color))))))

(defun %rounded-corner-radius (w h roundness)
  (if (> w h) (/ (* h roundness) 2) (/ (* w roundness) 2)))

(defun %rounded-corner-segments (radius segments)
  "Calculate number of segments to use for the corners"
  (if (>= segments 1)
      segments
      ;; Calculate the maximum angle between segments based on the error rate (usually 0.5f)
      (let ((th (acos (- (* 2 (expt (- 1 (/ +smooth-circle-error-rate+ radius)) 2)) 1))))
        (if (or (complexp th) (<= th 0.0))
            4
            (let ((n (ceiling (/ (/ (* 2 +pi+) th) 4.0))))
              (if (<= n 0) 4 n))))))

(defun draw-rectangle-rounded (rec roundness segments color)
  "Draw rectangle with rounded edges"
  ;; Not a rounded rectangle
  (when (<= roundness 0.0)
    (return-from draw-rectangle-rounded (draw-rectangle-rec rec color)))
  (multiple-value-bind (x y w h) (%rec rec)
    (let* ((roundness (min roundness 1.0))
           ;; Calculate corner radius
           (radius (%rounded-corner-radius w h roundness)))
      (when (<= radius 0.0) (return-from draw-rectangle-rounded nil))
      (let* ((segments (%rounded-corner-segments radius segments))
             (step-length (/ 90.0 segments))
             ;;      P0____________________P1
             ;;      /|                    |\
             ;;     /1|          2         |3\
             ;; P7 /__|____________________|__\ P2
             ;;   |   |P8                P9|   |
             ;;   | 8 |          9         | 4 |
             ;;   | __|____________________|__ |
             ;; P6 \  |P11              P10|  / P3
             ;;     \7|          6         |5/
             ;;      \|____________________|/
             ;;      P5                    P4
             ;; The x-coordinates used for the rounded rect
             (x0 (+ x radius)) (x1 (- (+ x w) radius)) (x2 (+ x w)) (x3 x)
             ;; The y-coordinates used for the rounded rect
             (y0 y) (y1 (+ y radius)) (y2 (- (+ y h) radius)) (y3 (+ y h))
             (points (vector (cons x0 y0) (cons x1 y0) (cons x2 y1) (cons x2 y2)
                             (cons x1 y3) (cons x0 y3) (cons x3 y2) (cons x3 y1)
                             (cons x0 y1) (cons x1 y1) (cons x1 y2) (cons x0 y2)))
             (centers (vector (aref points 8) (aref points 9) (aref points 10) (aref points 11)))
             (angles #(180.0 270.0 0.0 90.0)))
        (flet ((pt (i) (let ((p (aref points i))) (%vertex (car p) (cdr p)))))
          (rl-begin +rl-triangles+)
          ;; Draw all of the 4 corners: [1] Upper Left Corner, [3] Upper Right Corner, [5] Lower Right Corner, [7] Lower Left Corner
          (dotimes (k 4)
            (let ((angle (aref angles k))
                  (cx (car (aref centers k)))
                  (cy (cdr (aref centers k))))
              (dotimes (i segments)
                (%color color)
                (%vertex cx cy)
                (%polar cx cy (+ angle step-length) radius)
                (%polar cx cy angle radius)
                (incf angle step-length))))
          ;; [2] Upper Rectangle
          (%color color)
          (pt 0) (pt 8) (pt 9) (pt 1) (pt 0) (pt 9)
          ;; [4] Right Rectangle
          (%color color)
          (pt 9) (pt 10) (pt 3) (pt 2) (pt 9) (pt 3)
          ;; [6] Bottom Rectangle
          (%color color)
          (pt 11) (pt 5) (pt 4) (pt 10) (pt 11) (pt 4)
          ;; [8] Left Rectangle
          (%color color)
          (pt 7) (pt 6) (pt 11) (pt 8) (pt 7) (pt 11)
          ;; [9] Middle Rectangle
          (%color color)
          (pt 8) (pt 11) (pt 10) (pt 9) (pt 8) (pt 10)
          (rl-end))))))

(defun draw-rectangle-rounded-lines (rec roundness segments color)
  "Draw rectangle lines with rounded edges"
  (multiple-value-bind (x y w h) (%rec rec)
    ;; Not a rounded rectangle
    (when (<= roundness 0.0)
      (return-from draw-rectangle-rounded-lines
        (draw-rectangle-lines (truncate x) (truncate y) (truncate w) (truncate h) color)))
    (let* ((roundness (min roundness 1.0))
           ;; Calculate corner radius
           (radius (%rounded-corner-radius w h roundness)))
      (when (<= radius 0.0) (return-from draw-rectangle-rounded-lines nil))
      (let* ((segments (%rounded-corner-segments radius segments))
             (step-length (/ 90.0 segments))
             ;; The x-coordinates used for the outline
             (x0 (+ x radius 0.5)) (x1 (- (+ x w) radius 0.5)) (x2 (- (+ x w) 0.5)) (x3 (+ x 0.5))
             ;; The y-coordinates used for the outline
             (y0 (+ y 0.5)) (y1 (+ y radius 0.5)) (y2 (- (+ y h) radius 0.5)) (y3 (- (+ y h) 0.5))
             (points (vector (cons x0 y0) (cons x1 y0) (cons x2 y1) (cons x2 y2)
                             (cons x1 y3) (cons x0 y3) (cons x3 y2) (cons x3 y1)))
             (centers (vector (cons x0 y1) (cons x1 y1) (cons x1 y2) (cons x0 y2)))
             (angles #(180.0 270.0 0.0 90.0)))
        (rl-begin +rl-lines+)
        ;; Draw all the 4 corners first: Upper Left Corner, Upper Right Corner, Lower Right Corner, Lower Left Corner
        (dotimes (k 4)
          (let ((angle (aref angles k))
                (cx (car (aref centers k)))
                (cy (cdr (aref centers k))))
            (dotimes (i segments)
              (%color color)
              (%polar cx cy angle radius)
              (%polar cx cy (+ angle step-length) radius)
              (incf angle step-length))))
        ;; And now the remaining 4 lines
        (loop for i from 0 below 8 by 2
              do (%color color)
                 (%vertex (car (aref points i)) (cdr (aref points i)))
                 (%vertex (car (aref points (1+ i))) (cdr (aref points (1+ i)))))
        (rl-end)))))

(defun draw-rectangle-rounded-lines-ex (rec roundness segments line-thick color)
  "Draw rectangle with rounded edges outline with line thickness"
  ;; Not a rounded rectangle
  (when (<= roundness 0.0)
    (return-from draw-rectangle-rounded-lines-ex (draw-rectangle-lines-ex rec line-thick color)))
  (multiple-value-bind (x y w h) (%rec rec)
    (let* ((roundness (min roundness 1.0))
           (thick (float line-thick 1.0))
           (rounded-outline-thick 0.0)
           (outer-radius 0.0)
           (inner-radius 0.0))
      (if (>= thick 0.0)
          (let ((radius (%rounded-corner-radius w h roundness)))
            (when (<= radius 0.0) (return-from draw-rectangle-rounded-lines-ex nil))
            (setf outer-radius radius
                  inner-radius (- outer-radius thick))
            ;; The maximum thickness the outline can have and still be rounded on the interior edge is equal to the corner radius
            ;; Put another way, when `innerRadius <= 0`, the interior of the outline is just a normal rectangle with no rounding
            (if (<= inner-radius 0.0)
                (progn
                  (setf inner-radius 0.0
                        rounded-outline-thick outer-radius)
                  ;; Draw the not-rounded portion of the outline
                  (draw-rectangle-lines-ex (make-rectangle :x (+ x outer-radius) :y (+ y outer-radius)
                                                           :width (- w (* outer-radius 2.0))
                                                           :height (- h (* outer-radius 2.0)))
                                           (- thick outer-radius) color))
                (setf rounded-outline-thick thick))
            (setf segments (%rounded-corner-segments outer-radius segments)))
          (let ((radius (%rounded-corner-radius w h roundness)))
            ;; Only possible if the rectangle has 0 width or height
            (when (<= radius 0.0) (return-from draw-rectangle-rounded-lines-ex nil))
            ;; Expand the rectangle
            (incf x thick)
            (incf y thick)
            (decf w (* thick 2.0))
            (decf h (* thick 2.0))
            (setf inner-radius radius
                  outer-radius (- inner-radius thick)
                  rounded-outline-thick (- thick))
            (setf segments (%rounded-corner-segments inner-radius segments))))
      (let* ((step-length (/ 90.0 segments))
             ;; The x-coordinates used for the outline
             (x0 (+ x outer-radius)) (x1 (- (+ x w) outer-radius)) (x2 (+ x w)) (x3 x)
             (x4 (- (+ x w) rounded-outline-thick)) (x5 (+ x rounded-outline-thick))
             ;; The y-coordinates used for the outline
             (y0 y) (y1 (+ y outer-radius)) (y2 (- (+ y h) outer-radius)) (y3 (+ y h))
             (y4 (+ y rounded-outline-thick)) (y5 (- (+ y h) rounded-outline-thick))
             (points (vector (cons x0 y0) (cons x1 y0) (cons x2 y1) (cons x2 y2)
                             (cons x1 y3) (cons x0 y3) (cons x3 y2) (cons x3 y1)
                             (cons x0 y4) (cons x1 y4) (cons x4 y1) (cons x4 y2)
                             (cons x1 y5) (cons x0 y5) (cons x5 y2) (cons x5 y1)))
             (centers (vector (cons x0 y1) (cons x1 y1) (cons x1 y2) (cons x0 y2)))
             (angles #(180.0 270.0 0.0 90.0)))
        (flet ((pt (i) (let ((p (aref points i))) (%vertex (car p) (cdr p)))))
          (rl-begin +rl-triangles+)
          ;; Draw all of the 4 corners first: Upper Left Corner, Upper Right Corner, Lower Right Corner, Lower Left Corner
          (dotimes (k 4)
            (let ((angle (aref angles k))
                  (cx (car (aref centers k)))
                  (cy (cdr (aref centers k))))
              (dotimes (i segments)
                (%color color)
                (%polar cx cy angle inner-radius)
                (%polar cx cy (+ angle step-length) inner-radius)
                (%polar cx cy angle outer-radius)

                (%polar cx cy (+ angle step-length) inner-radius)
                (%polar cx cy (+ angle step-length) outer-radius)
                (%polar cx cy angle outer-radius)
                (incf angle step-length))))
          ;; Upper rectangle
          (%color color)
          (pt 0) (pt 8) (pt 9) (pt 1) (pt 0) (pt 9)
          ;; Right rectangle
          (%color color)
          (pt 10) (pt 11) (pt 3) (pt 2) (pt 10) (pt 3)
          ;; Lower rectangle
          (%color color)
          (pt 13) (pt 5) (pt 4) (pt 12) (pt 13) (pt 4)
          ;; Left rectangle
          (%color color)
          (pt 7) (pt 6) (pt 14) (pt 15) (pt 7) (pt 14)
          (rl-end))))))

(defun draw-poly (center sides radius rotation color)
  "Draw a regular polygon (Vector version)"
  (let* ((sides (max sides 3))
         (cx (%x center)) (cy (%y center))
         (central-angle (* rotation +deg2rad+))
         (angle-step (* (/ 360.0 sides) +deg2rad+)))
    (rl-begin +rl-triangles+)
    (dotimes (i sides)
      (%color color)
      (%vertex cx cy)
      (%vertex (+ cx (* (cos (+ central-angle angle-step)) radius))
               (+ cy (* (sin (+ central-angle angle-step)) radius)))
      (%vertex (+ cx (* (cos central-angle) radius))
               (+ cy (* (sin central-angle) radius)))
      (incf central-angle angle-step))
    (rl-end)))

(defun draw-poly-lines (center sides radius rotation color)
  "Draw a polygon outline of n sides"
  (let* ((sides (max sides 3))
         (cx (%x center)) (cy (%y center))
         (central-angle (* rotation +deg2rad+))
         (angle-step (* (/ 360.0 sides) +deg2rad+)))
    (rl-begin +rl-lines+)
    (dotimes (i sides)
      (%color color)
      (%vertex (+ cx (* (cos central-angle) radius))
               (+ cy (* (sin central-angle) radius)))
      (%vertex (+ cx (* (cos (+ central-angle angle-step)) radius))
               (+ cy (* (sin (+ central-angle angle-step)) radius)))
      (incf central-angle angle-step))
    (rl-end)))

(defun draw-poly-lines-ex (center sides radius rotation line-thick color)
  "Draw a polygon outline of n sides with extended parameters"
  (let* ((sides (max sides 3))
         (cx (%x center)) (cy (%y center))
         (central-angle (* rotation +deg2rad+))
         (exterior-angle (* (/ 360.0 sides) +deg2rad+))
         (apothem (* radius (cos (/ (* +deg2rad+ 180.0) sides))))
         (thick (float line-thick 1.0))
         outer-radius inner-radius)
    (if (>= thick 0.0)
        (setf outer-radius radius
              inner-radius (max 0.0 (- radius (* thick (/ radius apothem)))))
        (setf thick (- thick)
              outer-radius (+ radius (* thick (/ radius apothem)))
              inner-radius radius))
    (flet ((pv (angle r) (%vertex (+ cx (* (cos angle) r)) (+ cy (* (sin angle) r)))))
      (rl-begin +rl-triangles+)
      (dotimes (i sides)
        (%color color)
        (let ((next-angle (+ central-angle exterior-angle)))
          (pv next-angle outer-radius)
          (pv central-angle outer-radius)
          (pv central-angle inner-radius)

          (pv central-angle inner-radius)
          (pv next-angle inner-radius)
          (pv next-angle outer-radius)
          (setf central-angle next-angle)))
      (rl-end))))

(defun draw-circle (center-x center-y radius color)
  "Draw a color-filled circle"
  (draw-circle-v (vec2 (float center-x 1.0) (float center-y 1.0)) radius color))

;; NOTE: On OpenGL 3.3 and ES2 using QUADS to avoid drawing order issues
(defun draw-circle-v (center radius color)
  "Draw a color-filled circle (Vector version)"
  (draw-circle-sector center radius 0 360 36 color))

(defun draw-circle-gradient (center radius inner outer)
  "Draw a gradient-filled circle"
  (let ((cx (%x center)) (cy (%y center)))
    (rl-begin +rl-triangles+)
    (loop for i from 0 below 360 by 10
          do (%color inner)
             (%vertex cx cy)
             (%color outer)
             (%polar cx cy (+ i 10) radius)
             (%color outer)
             (%polar cx cy i radius))
    (rl-end)))

(defun draw-circle-sector (center radius start-angle end-angle segments color)
  "Draw a piece of a circle"
  (when (or (= start-angle end-angle)
            (<= radius 0.0))            ; There's nothing to draw (also avoid div by zero)
    (return-from draw-circle-sector nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
  (when (>= (- end-angle start-angle) 360.0) (setf end-angle (+ start-angle 360.0)))
  (let* ((cx (%x center)) (cy (%y center))
         (arc (- end-angle start-angle))
         (segments (%segments-for-arc arc radius segments (ceiling arc 90)))
         (step-length (/ arc (float segments)))
         (angle (float start-angle)))
    (rl-begin +rl-triangles+)
    (dotimes (i segments)
      (%color color)
      (%vertex cx cy)
      (%polar cx cy (+ angle step-length) radius)
      (%polar cx cy angle radius)
      (incf angle step-length))
    (rl-end)))

(defun draw-circle-sector-lines (center radius start-angle end-angle segments color)
  "Draw a piece of a circle outlines"
  (when (or (= start-angle end-angle)
            (<= radius 0.0))            ; There's nothing to draw (also avoid div by zero)
    (return-from draw-circle-sector-lines nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  (let ((show-cap-lines t))
    ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
    (when (>= (- end-angle start-angle) 360.0)
      (setf show-cap-lines nil
            end-angle (+ start-angle 360.0)))
    (let* ((cx (%x center)) (cy (%y center))
           (arc (- end-angle start-angle))
           (segments (%segments-for-arc arc radius segments (ceiling arc 90)))
           (step-length (/ arc (float segments)))
           (angle (float start-angle)))
      (rl-begin +rl-lines+)
      (when show-cap-lines
        (%color color)
        (%vertex cx cy)
        (%polar cx cy angle radius))
      (dotimes (i segments)
        (%color color)
        (%polar cx cy angle radius)
        (%polar cx cy (+ angle step-length) radius)
        (incf angle step-length))
      (when show-cap-lines
        (%color color)
        (%vertex cx cy)
        (%polar cx cy angle radius))
      (rl-end))))

(defun draw-circle-sector-lines-ex (center radius start-angle end-angle segments thick color)
  "Draw a piece of a circle outlines with thickness"
  (when (or (= start-angle end-angle)
            (<= radius 0.0))            ; There's nothing to draw (also avoid div by zero)
    (return-from draw-circle-sector-lines-ex nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  (let ((show-cap-lines t)
        (start-angle (float start-angle 1.0))
        (end-angle (float end-angle 1.0))
        (thick (float thick 1.0)))
    ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
    (when (>= (- end-angle start-angle) 360.0)
      (setf show-cap-lines (>= thick 0.0)
            end-angle (+ start-angle 360.0)))
    (let* ((cx (%x center)) (cy (%y center))
           (arc (- end-angle start-angle))
           (segments (%segments-for-arc arc radius segments (ceiling arc 90)))
           (step-length (/ arc (float segments)))
           (angle start-angle)
           ;; We are not drawing a circle, we are drawing an n-sided polygon
           ;; So, we need to adjust the outline thickness of the "circle" for it to look correct with fewer segments
           (apothem (* radius (cos (/ (* +deg2rad+ (/ arc 2.0)) (float segments)))))
           (radius-thick (* thick (/ radius apothem)))
           (outer-radius (float radius 1.0))
           (inner-radius (- radius radius-thick))
           ;; Cap 1 vertices
           (c0x 0.0) (c0y 0.0) (c1x 0.0) (c1y 0.0) (c2x 0.0) (c2y 0.0)
           ;; Cap 2 vertices
           (c3x 0.0) (c3y 0.0) (c4x 0.0) (c4y 0.0) (c5x 0.0) (c5y 0.0)
           ;; The number of angle steps that come before C1 (from `startAngle`, counter clockwise)
           (steps-before-c1 0)
           (s1-outside-of-circle nil)
           ;; The number of angle steps that come before C0 (from `startAngle`, counter clockwise)
           ;; Only used if S1 is outside of the circle
           (steps-before-c0 0))
      (flet ((full-sector ()
               (return-from draw-circle-sector-lines-ex
                 (draw-circle-sector center radius start-angle end-angle segments color)))
             (ccos (a) (cos (* +deg2rad+ a)))
             (csin (a) (sin (* +deg2rad+ a))))
        (if (>= thick 0.0)
            (when (>= thick inner-radius) (full-sector))
            (rotatef outer-radius inner-radius))
        (when show-cap-lines
          (if (>= thick 0.0)
              (progn
                (setf c2x (+ cx (* (ccos start-angle) inner-radius)) c2y (+ cy (* (csin start-angle) inner-radius))
                      c5x (+ cx (* (ccos end-angle) inner-radius)) c5y (+ cy (* (csin end-angle) inner-radius)))
                ;; For C1 and C4, we need to find the point that lies on the circle (n-sided polygon, actually)
                ;; We want C1 and C4 to be `thick` pixels perpendicularly from the `startAngle` and `endAngle` edges
                ;; and to be on the `innerRadius` edge
                (let ((c1-angle (* +rad2deg+ (asin (/ thick inner-radius)))))
                  ;; There are more segments before C1 than there are segments being drawn,
                  ;; so the whole circle sector must be covered
                  (when (>= (/ c1-angle step-length) segments) (full-sector))
                  ;; Do this after the previous check just in case `stepLength` is really small and
                  ;; dividing by it produces a very large number
                  (setf steps-before-c1 (truncate (/ c1-angle step-length)))
                  ;; The angles of the vertices on the circle outline before and after C1
                  (let* ((vertex-angle-before-c1 (* step-length steps-before-c1))
                         (vertex-angle-after-c1 (* step-length (1+ steps-before-c1)))
                         (p1y (* (csin vertex-angle-before-c1) inner-radius))
                         (p2y (* (csin vertex-angle-after-c1) inner-radius))
                         ;; Find the `t` of C1 between p1 and p2 ('t' as in `Lerp(start, end, t)`)
                         ;; This is used to lerp between the actual vertices (outside of our modified frame of reference)
                         ;; before and after C1
                         (tt (/ (- p1y thick) (- p1y p2y)))
                         (vb1x (+ cx (* (ccos (+ start-angle vertex-angle-before-c1)) inner-radius)))
                         (vb1y (+ cy (* (csin (+ start-angle vertex-angle-before-c1)) inner-radius)))
                         (va1x (+ cx (* (ccos (+ start-angle vertex-angle-after-c1)) inner-radius)))
                         (va1y (+ cy (* (csin (+ start-angle vertex-angle-after-c1)) inner-radius)))
                         (vb2x (+ cx (* (ccos (- end-angle vertex-angle-before-c1)) inner-radius)))
                         (vb2y (+ cy (* (csin (- end-angle vertex-angle-before-c1)) inner-radius)))
                         (va2x (+ cx (* (ccos (- end-angle vertex-angle-after-c1)) inner-radius)))
                         (va2y (+ cy (* (csin (- end-angle vertex-angle-after-c1)) inner-radius))))
                    (setf c1x (+ vb1x (* (- va1x vb1x) tt))
                          c1y (+ vb1y (* (- va1y vb1y) tt))
                          c4x (+ vb2x (* (- va2x vb2x) tt))
                          c4y (+ vb2y (* (- va2y vb2y) tt)))
                    ;; `innerAngleBetweenCapEnds` is the angle of the diagonal line between the cap ends
                    ;; `S1Length` is the length of that line
                    (let* ((inner-angle-between-cap-ends (/ (- (- end-angle 90.0) (+ start-angle 90.0)) 2.0))
                           (s1-length (/ thick (ccos inner-angle-between-cap-ends))))
                      ;; As `startAngle` and `endAngle` draw more of a circle, S1 goes further out from the center
                      ;; It can go so far that it is outside of the circle, by a lot
                      ;; If S1 is outside of the circle, we need to find the two points (C0 and C3) where
                      ;; the line segments C0->C1 and C0->C3 intersect the circle outline,
                      ;; using the same method we used to find C1 and C4
                      (if (and (< inner-angle-between-cap-ends 90.0) (<= s1-length inner-radius))
                          ;; S1 is inside of the circle
                          (let ((between-start-and-end-angle (/ (+ end-angle start-angle) 2.0)))
                            (setf c0x (+ cx (* (ccos between-start-and-end-angle) s1-length))
                                  c0y (+ cy (* (csin between-start-and-end-angle) s1-length))
                                  c3x c0x c3y c0y)
                            (let* ((p1x (- c1x cx)) (p1y (- c1y cy))
                                   (p2x (- c0x cx)) (p2y (- c0y cy))
                                   ;; Copied from "raymath.h" Vector2Angle()
                                   (dot (+ (* p1x p2x) (* p1y p2y)))
                                   (det (- (* p1x p2y) (* p1y p2x)))
                                   (c1-to-s1-angle (atan det dot)))
                              ;; If C1 and C4 are on the wrong side of S1, the whole circle sector is covered
                              (when (< c1-to-s1-angle 0.0) (full-sector))))
                          ;; S1 is outside of the circle
                          (progn
                            (when (<= (- end-angle start-angle) 180.0) (full-sector))
                            (setf s1-outside-of-circle t
                                  steps-before-c0 (truncate (/ (+ 180.0 (* +rad2deg+ (asin (/ thick (- inner-radius)))))
                                                               step-length)))
                            ;; Reuse the code for finding C1 and C4 to find C0 and C3
                            (let* ((vertex-angle-before-c0 (* step-length steps-before-c0))
                                   (vertex-angle-after-c0 (* step-length (1+ steps-before-c0)))
                                   (p1y (* (csin vertex-angle-before-c0) inner-radius))
                                   (p2y (* (csin vertex-angle-after-c0) inner-radius))
                                   (tt (/ (- p1y thick) (- p1y p2y)))
                                   (vb1x (+ cx (* (ccos (+ start-angle vertex-angle-before-c0)) inner-radius)))
                                   (vb1y (+ cy (* (csin (+ start-angle vertex-angle-before-c0)) inner-radius)))
                                   (va1x (+ cx (* (ccos (+ start-angle vertex-angle-after-c0)) inner-radius)))
                                   (va1y (+ cy (* (csin (+ start-angle vertex-angle-after-c0)) inner-radius)))
                                   (vb2x (+ cx (* (ccos (- end-angle vertex-angle-before-c0)) inner-radius)))
                                   (vb2y (+ cy (* (csin (- end-angle vertex-angle-before-c0)) inner-radius)))
                                   (va2x (+ cx (* (ccos (- end-angle vertex-angle-after-c0)) inner-radius)))
                                   (va2y (+ cy (* (csin (- end-angle vertex-angle-after-c0)) inner-radius))))
                              (setf c0x (+ vb1x (* (- va1x vb1x) tt))
                                    c0y (+ vb1y (* (- va1y vb1y) tt))
                                    c3x (+ vb2x (* (- va2x vb2x) tt))
                                    c3y (+ vb2y (* (- va2y vb2y) tt))))))))))
              ;; Negative thickness
              (let* ((outer-angle-between-cap-ends (/ (- (+ end-angle 90.0) (- start-angle 90.0)) 2.0))
                     (s1-length (/ thick (ccos outer-angle-between-cap-ends)))
                     (between-start-and-end-angle (+ 180.0 (/ (+ end-angle start-angle) 2.0))))
                (setf c0x (+ cx (* (ccos between-start-and-end-angle) s1-length))
                      c0y (+ cy (* (csin between-start-and-end-angle) s1-length))
                      c3x c0x c3y c0y
                      c2x (+ cx (* (ccos start-angle) outer-radius)) c2y (+ cy (* (csin start-angle) outer-radius))
                      c5x (+ cx (* (ccos end-angle) outer-radius)) c5y (+ cy (* (csin end-angle) outer-radius)))
                ;; Change the frame of reference so that `center` is the origin and `startAngle` is 0 degrees
                (flet ((rot (x y angle)
                         ;; Copied from "raymath.h" Vector2Rotate()
                         (values (- (* (cos angle) x) (* (sin angle) y))
                                 (+ (* (sin angle) x) (* (cos angle) y)))))
                  (let ((rot-a (- (* +deg2rad+ start-angle))))
                    (multiple-value-bind (c0tx c0ty) (rot (- c0x cx) (- c0y cy) rot-a)
                      (multiple-value-bind (cv1x cv1y) (rot (- c2x cx) (- c2y cy) rot-a)
                        (multiple-value-bind (cv2x cv2y) (rot (* (ccos (+ start-angle step-length)) outer-radius)
                                                              (* (csin (+ start-angle step-length)) outer-radius)
                                                              rot-a)
                          ;; Figure out the line that `circleVertex1` and `circleVertex2` are on
                          (let* ((rise (- cv1y cv2y))
                                 (run (- cv1x cv2x))
                                 ;; Get where that line intersects the horizontal line that `c0Translated` is on
                                 (c1-rise (- c0ty cv1y))
                                 (c1-run (* (/ c1-rise rise) run))
                                 (c1-distance-from-c0 (- (+ cv1x c1-run) c0tx)))
                            (setf c1x (+ c0x (* (ccos start-angle) c1-distance-from-c0))
                                  c1y (+ c0y (* (csin start-angle) c1-distance-from-c0))
                                  c4x (+ c0x (* (ccos end-angle) c1-distance-from-c0))
                                  c4y (+ c0y (* (csin end-angle) c1-distance-from-c0)))
                            (when (< c1-distance-from-c0 0.0)
                              ;; The caps are intersecting each other
                              (multiple-value-bind (cv3x cv3y) (rot (- c5x cx) (- c5y cy) rot-a)
                                (multiple-value-bind (cv4x cv4y) (rot (* (ccos (- end-angle step-length)) outer-radius)
                                                                      (* (csin (- end-angle step-length)) outer-radius)
                                                                      rot-a)
                                  ;; `startAngle` is 0 degrees within this frame of reference,
                                  ;; so C1 just goes horizontally out from C0
                                  (let ((c1tx (+ c0tx c1-distance-from-c0))
                                        (c1ty c0ty))
                                    ;; Make `circleVertex2` the origin
                                    (decf cv1x cv2x) (decf cv1y cv2y)
                                    (decf cv3x cv2x) (decf cv3y cv2y)
                                    (decf cv4x cv2x) (decf cv4y cv2y)
                                    (decf c1tx cv2x) (decf c1ty cv2y)
                                    ;; Make the line between `circleVertex1` and `circleVertex2` a horizontal line
                                    (let ((theta (- (atan cv1y cv1x))))
                                      (multiple-value-setq (cv1x cv1y) (rot cv1x cv1y theta))
                                      (multiple-value-setq (cv3x cv3y) (rot cv3x cv3y theta))
                                      (multiple-value-setq (cv4x cv4y) (rot cv4x cv4y theta))
                                      (multiple-value-setq (c1tx c1ty) (rot c1tx c1ty theta)))
                                    ;; Find where the line that `circleVertex3` and `circleVertex4` are on would intersect the
                                    ;; line segment defined by `circleVertex1` and `c1Translated`
                                    (let* ((rise (- cv3y cv4y))
                                           (run (- cv3x cv4x))
                                           (target-rise (- cv3y))
                                           (target-x (+ cv3x (* (/ target-rise rise) run)))
                                           (tt (/ (- c1tx target-x) (- c1tx cv1x))))
                                      (setf c1x (+ c1x (* (- c2x c1x) tt))
                                            c1y (+ c1y (* (- c2y c1y) tt))
                                            c4x c1x c4y c1y
                                            c0x c1x c0y c1y
                                            c3x c1x c3y c1y))))))))))))
                ;; Swap vertices to correct the winding order
                (rotatef c0x c2x) (rotatef c0y c2y)
                (rotatef c3x c5x) (rotatef c3y c5y))))
        (rl-begin +rl-triangles+)
        (%color color)
        ;; Draw the circle outline
        (dotimes (i segments)
          (%polar cx cy angle outer-radius)
          (%polar cx cy angle inner-radius)
          (%polar cx cy (+ angle step-length) inner-radius)

          (%polar cx cy angle outer-radius)
          (%polar cx cy (+ angle step-length) inner-radius)
          (%polar cx cy (+ angle step-length) outer-radius)
          (incf angle step-length))
        ;; Draw the caps
        (when show-cap-lines
          ;; Cap 1
          (%vertex cx cy) (%vertex c0x c0y) (%vertex c1x c1y)
          (%vertex cx cy) (%vertex c1x c1y) (%vertex c2x c2y)
          ;; Cap 2
          (%vertex cx cy) (%vertex c5x c5y) (%vertex c4x c4y)
          (%vertex cx cy) (%vertex c4x c4y) (%vertex c3x c3y)
          ;; Some extra work may be needed when `thick` is positive
          (when (>= thick 0.0)
            ;; Fill in the gaps between cap 1 and the circle outline and cap 2 and the circle outline
            (when (> steps-before-c1 0)
              (setf angle 0.0)
              (dotimes (i steps-before-c1)
                ;; Cap 1
                (%vertex c1x c1y)
                (%polar cx cy (+ start-angle angle step-length) inner-radius)
                (%polar cx cy (+ start-angle angle) inner-radius)
                ;; Cap 2
                (%vertex c4x c4y)
                (%polar cx cy (- end-angle angle) inner-radius)
                (%polar cx cy (- end-angle (+ angle step-length)) inner-radius)
                (incf angle step-length)))
            ;; Fill in the gap between C0, C3 and the circle outline
            (when s1-outside-of-circle
              (let ((vertices-between-c0-and-c3 (1- (- segments (* steps-before-c0 2)))))
                (if (zerop vertices-between-c0-and-c3)
                    ;; No gap to fill
                    (progn (%vertex cx cy) (%vertex c3x c3y) (%vertex c0x c0y))
                    ;; There's a gap to fill
                    (progn
                      ;; Triangle touching C0
                      (%vertex cx cy)
                      (%polar cx cy (+ start-angle (* step-length (1+ steps-before-c0))) inner-radius)
                      (%vertex c0x c0y)
                      ;; Triangle touching C3
                      (%vertex cx cy)
                      (%vertex c3x c3y)
                      (%polar cx cy (- end-angle (* step-length (1+ steps-before-c0))) inner-radius)
                      ;; Triangles between the previous two
                      (decf vertices-between-c0-and-c3)
                      (setf angle (+ start-angle (* step-length (1+ steps-before-c0))))
                      (dotimes (i vertices-between-c0-and-c3)
                        (%vertex cx cy)
                        (%polar cx cy (+ angle step-length) inner-radius)
                        (%polar cx cy angle inner-radius)
                        (incf angle step-length))))))))
        (rl-end)))))

(defun draw-circle-lines (center-x center-y radius color)
  "Draw circle outline"
  (draw-circle-lines-v (vec2 (float center-x 1.0) (float center-y 1.0)) radius color))

(defun draw-circle-lines-v (center radius color)
  "Draw circle outline (Vector version)"
  (let ((cx (%x center)) (cy (%y center)))
    (rl-begin +rl-lines+)
    (%color color)
    ;; NOTE: Circle outline is drawn as 36 line segments (one vertex every 10 degrees)
    (loop for i from 0 below 360 by 10
          do (%polar cx cy i radius)
             (%polar cx cy (+ i 10) radius))
    (rl-end)))

(defun draw-circle-lines-ex (center radius thick color)
  "Draw circle outline with line thickness"
  (draw-ring center (- radius thick) radius 0.0 360.0 36 color))

(defun draw-ellipse (center-x center-y radius-h radius-v color)
  "Draw ellipse"
  (draw-ellipse-v (vec2 (float center-x 1.0) (float center-y 1.0)) radius-h radius-v color))

(defun draw-ellipse-v (center radius-h radius-v color)
  "Draw ellipse (Vector version)"
  (let ((cx (%x center)) (cy (%y center)))
    (rl-begin +rl-triangles+)
    (loop for i from 0 below 360 by 10
          do (%color color)
             (%vertex cx cy)
             (%polar cx cy (+ i 10) radius-h radius-v)
             (%polar cx cy i radius-h radius-v))
    (rl-end)))

(defun draw-ellipse-lines (center-x center-y radius-h radius-v color)
  "Draw ellipse outline"
  (draw-ellipse-lines-v (vec2 (float center-x 1.0) (float center-y 1.0)) radius-h radius-v color))

(defun draw-ellipse-lines-v (center radius-h radius-v color)
  "Draw ellipse outline (Vector version)"
  (let ((cx (%x center)) (cy (%y center)))
    (rl-begin +rl-lines+)
    (loop for i from 0 below 360 by 10
          do (%color color)
             (%polar cx cy (+ i 10) radius-h radius-v)
             (%polar cx cy i radius-h radius-v))
    (rl-end)))

(defun draw-ellipse-lines-ex (center radius-h radius-v thick color)
  "Draw ellipse outline with line thickness"
  (let ((outer-h radius-h) (inner-h (- radius-h thick))
        (outer-v radius-v) (inner-v (- radius-v thick))
        (cx (%x center)) (cy (%y center)))
    (if (>= thick 0.0)
        ;; Just a filled-in ellipse
        (when (or (<= inner-h 0.0) (<= inner-v 0.0))
          (return-from draw-ellipse-lines-ex (draw-ellipse-v center radius-h radius-v color)))
        ;; The outline is growing outside of the ellipse, so swap the inner and outer radius
        (progn (rotatef outer-h inner-h)
               (rotatef outer-v inner-v)))
    (rl-begin +rl-triangles+)
    (%color color)
    (loop for i from 0 below 360 by 10
          do (%polar cx cy i inner-h inner-v)
             (%polar cx cy (+ i 10) inner-h inner-v)
             (%polar cx cy (+ i 10) outer-h outer-v)

             (%polar cx cy i inner-h inner-v)
             (%polar cx cy (+ i 10) outer-h outer-v)
             (%polar cx cy i outer-h outer-v))
    (rl-end)))

(defun draw-ring (center inner-radius outer-radius start-angle end-angle segments color)
  "Draw ring"
  (when (= start-angle end-angle) (return-from draw-ring nil))
  ;; Function expects (outerRadius > innerRadius)
  (when (< outer-radius inner-radius) (rotatef outer-radius inner-radius))
  ;; There's nothing to draw (also avoid div by zero)
  (when (<= outer-radius 0.0) (return-from draw-ring nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
  (when (>= (- end-angle start-angle) 360.0) (setf end-angle (+ start-angle 360.0)))
  (let* ((arc (- end-angle start-angle))
         (segments (%segments-for-arc arc outer-radius segments (ceiling arc 90))))
    ;; Not a ring
    (when (<= inner-radius 0.0)
      (return-from draw-ring (draw-circle-sector center outer-radius start-angle end-angle segments color)))
    (let ((cx (%x center)) (cy (%y center))
          (step-length (/ arc (float segments)))
          (angle (float start-angle)))
      (rl-begin +rl-triangles+)
      (dotimes (i segments)
        (%color color)
        (%polar cx cy angle inner-radius)
        (%polar cx cy (+ angle step-length) inner-radius)
        (%polar cx cy angle outer-radius)

        (%polar cx cy (+ angle step-length) inner-radius)
        (%polar cx cy (+ angle step-length) outer-radius)
        (%polar cx cy angle outer-radius)
        (incf angle step-length))
      (rl-end))))

(defun draw-ring-lines (center inner-radius outer-radius start-angle end-angle segments color)
  "Draw ring outline"
  (when (= start-angle end-angle) (return-from draw-ring-lines nil))
  ;; Function expects (outerRadius > innerRadius)
  (when (< outer-radius inner-radius) (rotatef outer-radius inner-radius))
  ;; There's nothing to draw (also avoid div by zero)
  (when (<= outer-radius 0.0) (return-from draw-ring-lines nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  (let ((show-cap-lines t))
    ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
    (when (>= (- end-angle start-angle) 360.0)
      (setf show-cap-lines nil
            end-angle (+ start-angle 360.0)))
    (let* ((arc (- end-angle start-angle))
           (segments (%segments-for-arc arc outer-radius segments (ceiling arc 90))))
      (when (<= inner-radius 0.0)
        (return-from draw-ring-lines
          (draw-circle-sector-lines center outer-radius start-angle end-angle segments color)))
      (let ((cx (%x center)) (cy (%y center))
            (step-length (/ arc (float segments)))
            (angle (float start-angle)))
        (rl-begin +rl-lines+)
        (when show-cap-lines
          (%color color)
          (%polar cx cy angle outer-radius)
          (%polar cx cy angle inner-radius))
        (dotimes (i segments)
          (%color color)
          (%polar cx cy angle outer-radius)
          (%polar cx cy (+ angle step-length) outer-radius)

          (%polar cx cy angle inner-radius)
          (%polar cx cy (+ angle step-length) inner-radius)
          (incf angle step-length))
        (when show-cap-lines
          (%color color)
          (%polar cx cy angle outer-radius)
          (%polar cx cy angle inner-radius))
        (rl-end)))))

(defun draw-ring-lines-ex (center inner-radius outer-radius start-angle end-angle segments thick color)
  "Draw ring outline with line thickness"
  (when (= start-angle end-angle) (return-from draw-ring-lines-ex nil))
  ;; Function expects (outerRadius > innerRadius)
  (when (< outer-radius inner-radius) (rotatef outer-radius inner-radius))
  ;; There's nothing to draw (also avoid div by zero)
  (when (<= outer-radius 0.0) (return-from draw-ring-lines-ex nil))
  ;; Function expects (endAngle > startAngle)
  (when (< end-angle start-angle) (rotatef start-angle end-angle))
  (let ((show-cap-lines t)
        (start-angle (float start-angle 1.0))
        (end-angle (float end-angle 1.0))
        (inner-radius (float inner-radius 1.0))
        (outer-radius (float outer-radius 1.0))
        (thick (float thick 1.0)))
    ;; Drawing a whole circle, things get weird without limiting the circle to 360 degrees
    (when (>= (- end-angle start-angle) 360.0)
      (setf show-cap-lines (>= thick 0.0)
            end-angle (+ start-angle 360.0)))
    (let* ((cx (%x center)) (cy (%y center))
           (arc (- end-angle start-angle))
           (segments (%segments-for-arc arc outer-radius segments (ceiling arc 90)))
           (step-length (/ arc (float segments)))
           ;; We are not drawing a circle, we are drawing an n-sided polygon
           ;; So, we need to adjust the outline thickness of the "circle" for it to look correct with fewer segments
           (apothem (* outer-radius (cos (/ (* +deg2rad+ (/ arc 2.0)) (float segments)))))
           (radius-thick (* thick (/ outer-radius apothem)))
           ;; Since 2 rings are being drawn, there are 4 radii
           ;; "Inner" means closer to the center, "outer" means farther from the center
           ;; Sorted from farthest to closest: outerOuter, innerOuter, outerInner, innerInner
           (inner-outer-radius 0.0)
           (outer-outer-radius 0.0)
           (inner-inner-radius 0.0)
           (outer-inner-radius 0.0)
           ;; For positive `thick` values
           (steps-before-inner 0)
           (steps-before-outer 0)
           (t-inner 0.0)
           (t-outer 0.0)
           (inner-angles-cross-each-other nil)
           ;; For negative `thick` values
           (cap1-second-inner-x 0.0) (cap1-second-inner-y 0.0)
           (cap1-second-outer-x 0.0) (cap1-second-outer-y 0.0)
           (cap2-second-inner-x 0.0) (cap2-second-inner-y 0.0)
           (cap2-second-outer-x 0.0) (cap2-second-outer-y 0.0)
           (caps-intersect nil)
           (cap-intersection-x 0.0) (cap-intersection-y 0.0))
      (flet ((ccos (a) (cos (* +deg2rad+ a)))
             (csin (a) (sin (* +deg2rad+ a))))
        (if (>= thick 0.0)
            (progn
              (setf inner-radius (max 0.0 inner-radius))
              ;; Just a filled-in ring
              (when (> radius-thick (/ (- outer-radius inner-radius) 2.0))
                (return-from draw-ring-lines-ex
                  (draw-ring center inner-radius outer-radius start-angle end-angle segments color)))
              (setf inner-inner-radius inner-radius
                    outer-inner-radius (+ inner-inner-radius radius-thick)
                    outer-outer-radius outer-radius
                    inner-outer-radius (- outer-outer-radius radius-thick)))
            (progn
              ;; Just a circle sector outline
              (when (<= inner-radius 0.0)
                (return-from draw-ring-lines-ex
                  (draw-circle-sector-lines-ex center outer-radius start-angle end-angle segments thick color)))
              (setf outer-inner-radius inner-radius
                    inner-inner-radius (max 0.0 (+ outer-inner-radius radius-thick))
                    inner-outer-radius outer-radius
                    outer-outer-radius (- inner-outer-radius radius-thick))))
        (when show-cap-lines
          (if (>= thick 0.0)
              ;; Get the angle of the arc that has `thick` length along the inner and outer radii
              (let ((cap1-inner-angle-end (* +rad2deg+ (/ thick outer-inner-radius)))
                    (cap1-outer-angle-end (* +rad2deg+ (/ thick inner-outer-radius))))
                ;; Just a filled-in ring
                (when (< (- end-angle start-angle) (* cap1-outer-angle-end 2.0))
                  (return-from draw-ring-lines-ex
                    (draw-ring center inner-radius outer-radius start-angle end-angle segments color)))
                (when (< (- end-angle start-angle) (* cap1-inner-angle-end 2.0))
                  (setf inner-angles-cross-each-other t))
                (setf steps-before-inner (truncate (/ cap1-inner-angle-end step-length))
                      steps-before-outer (truncate (/ cap1-outer-angle-end step-length)))
                ;; We need to find where `cap1InnerAngleEnd` intersects the edge defined
                ;; by `beforeInnerVertex` and `afterInnerVertex`
                ;; We can make this easy by making `center` the origin (0, 0) and
                ;; making `cap1InnerAngleEnd` 0 degrees (a horizontal line)
                ;; With that, we know these lines intersect when 'y' equals 0,
                ;; so we just need to solve for 't' (as in `Lerp(start, end, t)`)
                (let ((before-inner-y (* (csin (- (* steps-before-inner step-length) cap1-inner-angle-end)) outer-inner-radius))
                      (after-inner-y (* (csin (- (* (1+ steps-before-inner) step-length) cap1-inner-angle-end)) outer-inner-radius))
                      ;; The same as above, but for the outer edge
                      (before-outer-y (* (csin (- (* steps-before-outer step-length) cap1-outer-angle-end)) inner-outer-radius))
                      (after-outer-y (* (csin (- (* (1+ steps-before-outer) step-length) cap1-outer-angle-end)) inner-outer-radius)))
                  (setf t-inner (/ before-inner-y (- before-inner-y after-inner-y))
                        t-outer (/ before-outer-y (- before-outer-y after-outer-y)))))
              ;; "Cap 1" is the outline on `startAngle` and "Cap 2" is the outline on `endAngle`
              ;; We're using a frame of reference where `center` is (0, 0) and `startAngle` is 0 degrees
              (let* ((cap1-o0x outer-outer-radius) (cap1-o0y 0.0)
                     (cap1-o1x (* (ccos step-length) outer-outer-radius))
                     (cap1-o1y (* (csin step-length) outer-outer-radius))
                     ;; Assuming a linear interpolation such as `value = Lerp(start, end, t)`
                     ;; We can find O2 by getting its 't' between O1.y and O0.y (which is always greater than 1)
                     (t-outer (/ (- cap1-o1y thick) cap1-o1y))
                     (cap1-o2x (+ cap1-o1x (* (- cap1-o0x cap1-o1x) t-outer)))
                     (cap1-o2y thick)
                     (cap-long-edge-length (- outer-outer-radius inner-inner-radius))
                     (cap1-i2x (- cap1-o2x cap-long-edge-length))
                     (cap1-i2y thick)
                     (delta (- end-angle start-angle))
                     (cap2-o0x (* (ccos delta) outer-outer-radius))
                     (cap2-o0y (* (csin delta) outer-outer-radius))
                     (cap2-o1x (* (ccos (- delta step-length)) outer-outer-radius))
                     (cap2-o1y (* (csin (- delta step-length)) outer-outer-radius))
                     (cap2-o2x (+ cap2-o1x (* (- cap2-o0x cap2-o1x) t-outer)))
                     (cap2-o2y (+ cap2-o1y (* (- cap2-o0y cap2-o1y) t-outer)))
                     (cap2-i0x (* (ccos delta) inner-inner-radius))
                     (cap2-i0y (* (csin delta) inner-inner-radius))
                     (cap2-i2x (- cap2-o2x (* (ccos delta) cap-long-edge-length)))
                     (cap2-i2y (- cap2-o2y (* (csin delta) cap-long-edge-length)))
                     ;; The 't' of the intersection between I2 and O2 (`Lerp(I2, O2, t)`)
                     (t-cap-long-edge-cross -1.0))
                ;; Avoid division by zero
                (when (/= (- cap2-i2y cap2-o2y) 0.0)
                  ;; Find where the long edge of cap 2 intersects the long edge of cap 1
                  (setf t-cap-long-edge-cross (/ (- cap2-i2y thick) (- cap2-i2y cap2-o2y)))
                  (when (and (>= t-cap-long-edge-cross 0.0) (<= t-cap-long-edge-cross 1.0))
                    (setf caps-intersect t)))
                ;; Rotate the frame of reference so that cap 1's I0->I2 edge is a vertical line
                ;; Copied from "raymath.h" Vector2Rotate(), though we only use the x axis
                (let* ((rotate-by (/ (- (* +deg2rad+ step-length)) 2.0))
                       (cosres (cos rotate-by))
                       (sinres (sin rotate-by))
                       (r-cap1-i2x (- (* cap1-i2x cosres) (* cap1-i2y sinres)))
                       (r-cap1-o2x (- (* cap1-o2x cosres) (* cap1-o2y sinres)))
                       (r-cap2-i0x (- (* cap2-i0x cosres) (* cap2-i0y sinres)))
                       (r-cap2-i2x (- (* cap2-i2x cosres) (* cap2-i2y sinres)))
                       (r-cap2-o0x (- (* cap2-o0x cosres) (* cap2-o0y sinres)))
                       (r-cap2-o2x (- (* cap2-o2x cosres) (* cap2-o2y sinres)))
                       ;; The 't' of the intersection between I0 and I2 (`Lerp(I0, I2, t)`)
                       (t-cross-inner -1.0)
                       ;; The 't' of the intersection between O0 and O2 (`Lerp(O0, O2, t)`)
                       (t-cross-outer -1.0))
                  ;; Avoid division by zero
                  (when (/= (- r-cap2-i0x r-cap2-i2x) 0.0)
                    (setf t-cross-inner (/ (- r-cap2-i0x r-cap1-i2x) (- r-cap2-i0x r-cap2-i2x))))
                  ;; Make sure `tCrossInner` is 0 when it should be (mitigate floating-point rounding woes)
                  (when (<= inner-inner-radius 0.0) (setf t-cross-inner 0.0))
                  ;; Avoid division by zero
                  (when (/= (- r-cap2-o0x r-cap2-o2x) 0.0)
                    (setf t-cross-outer (/ (- r-cap2-o0x r-cap1-o2x) (- r-cap2-o0x r-cap2-o2x))))
                  ;; With our additional information, calculate the vertices we need
                  ;; outside of our modified frame of reference
                  (setf cap1-o0x (+ cx (* (ccos start-angle) outer-outer-radius))
                        cap1-o0y (+ cy (* (csin start-angle) outer-outer-radius))
                        cap1-o1x (+ cx (* (ccos (+ start-angle step-length)) outer-outer-radius))
                        cap1-o1y (+ cy (* (csin (+ start-angle step-length)) outer-outer-radius))
                        cap1-o2x (+ cap1-o1x (* (- cap1-o0x cap1-o1x) t-outer))
                        cap1-o2y (+ cap1-o1y (* (- cap1-o0y cap1-o1y) t-outer))
                        cap2-o0x (+ cx (* (ccos end-angle) outer-outer-radius))
                        cap2-o0y (+ cy (* (csin end-angle) outer-outer-radius))
                        cap2-o1x (+ cx (* (ccos (- end-angle step-length)) outer-outer-radius))
                        cap2-o1y (+ cy (* (csin (- end-angle step-length)) outer-outer-radius))
                        cap2-o2x (+ cap2-o1x (* (- cap2-o0x cap2-o1x) t-outer))
                        cap2-o2y (+ cap2-o1y (* (- cap2-o0y cap2-o1y) t-outer))
                        cap1-i2x (- cap1-o2x (* (ccos start-angle) cap-long-edge-length))
                        cap1-i2y (- cap1-o2y (* (csin start-angle) cap-long-edge-length))
                        cap2-i0x (+ cx (* (ccos end-angle) inner-inner-radius))
                        cap2-i0y (+ cy (* (csin end-angle) inner-inner-radius))
                        cap2-i2x (- cap2-o2x (* (ccos end-angle) cap-long-edge-length))
                        cap2-i2y (- cap2-o2y (* (csin end-angle) cap-long-edge-length)))
                  (cond (caps-intersect
                         (setf cap-intersection-x (+ cap2-i2x (* (- cap2-o2x cap2-i2x) t-cap-long-edge-cross))
                               cap-intersection-y (+ cap2-i2y (* (- cap2-o2y cap2-i2y) t-cap-long-edge-cross))
                               cap2-i2x (+ cap2-i0x (* (- cap2-i2x cap2-i0x) t-cross-inner))
                               cap2-i2y (+ cap2-i0y (* (- cap2-i2y cap2-i0y) t-cross-inner))
                               cap1-i2x cap2-i2x
                               cap1-i2y cap2-i2y))
                        ((and (>= t-cross-outer 0.0) (<= t-cross-outer 1.0))
                         (setf cap2-o2x (+ cap2-o0x (* (- cap2-o2x cap2-o0x) t-cross-outer))
                               cap2-o2y (+ cap2-o0y (* (- cap2-o2y cap2-o0y) t-cross-outer))
                               cap1-o2x cap2-o2x
                               cap1-o2y cap2-o2y
                               cap2-i2x (+ cap2-i0x (* (- cap2-i2x cap2-i0x) t-cross-inner))
                               cap2-i2y (+ cap2-i0y (* (- cap2-i2y cap2-i0y) t-cross-inner))
                               cap1-i2x cap2-i2x
                               cap1-i2y cap2-i2y)))
                  (setf cap1-second-inner-x cap1-i2x cap1-second-inner-y cap1-i2y
                        cap1-second-outer-x cap1-o2x cap1-second-outer-y cap1-o2y
                        cap2-second-inner-x cap2-i2x cap2-second-inner-y cap2-i2y
                        cap2-second-outer-x cap2-o2x cap2-second-outer-y cap2-o2y)))))
        (let ((angle start-angle))
          (rl-begin +rl-triangles+)
          (%color color)
          (dotimes (i segments)
            ;; `innerRadius` outline
            (%polar cx cy angle outer-inner-radius)
            (%polar cx cy angle inner-inner-radius)
            (%polar cx cy (+ angle step-length) inner-inner-radius)

            (%polar cx cy angle outer-inner-radius)
            (%polar cx cy (+ angle step-length) inner-inner-radius)
            (%polar cx cy (+ angle step-length) outer-inner-radius)
            ;; `outerRadius` outline
            (%polar cx cy angle outer-outer-radius)
            (%polar cx cy angle inner-outer-radius)
            (%polar cx cy (+ angle step-length) inner-outer-radius)

            (%polar cx cy angle outer-outer-radius)
            (%polar cx cy (+ angle step-length) inner-outer-radius)
            (%polar cx cy (+ angle step-length) outer-outer-radius)
            (incf angle step-length))
          (when show-cap-lines
            (if (>= thick 0.0)
                (progn
                  (setf angle 0.0)
                  (dotimes (i steps-before-outer)
                    ;; Cap 1
                    (%polar cx cy (+ start-angle angle) outer-inner-radius)
                    (%polar cx cy (+ start-angle angle step-length) outer-inner-radius)
                    (%polar cx cy (+ start-angle angle step-length) inner-outer-radius)

                    (%polar cx cy (+ start-angle angle) outer-inner-radius)
                    (%polar cx cy (+ start-angle angle step-length) inner-outer-radius)
                    (%polar cx cy (+ start-angle angle) inner-outer-radius)
                    ;; Cap 2
                    (%polar cx cy (- end-angle angle) outer-inner-radius)
                    (%polar cx cy (- end-angle angle) inner-outer-radius)
                    (%polar cx cy (- end-angle angle step-length) inner-outer-radius)

                    (%polar cx cy (- end-angle angle) outer-inner-radius)
                    (%polar cx cy (- end-angle angle step-length) inner-outer-radius)
                    (%polar cx cy (- end-angle angle step-length) outer-inner-radius)
                    (incf angle step-length))
                  ;; We've already moved `stepsBeforeOuter` steps from each end
                  (let* ((total-steps-left (- segments (* steps-before-outer 2)))
                         (inner-steps-left (- steps-before-inner steps-before-outer))
                         ;; Cap 1
                         (c1-obe-x (+ cx (* (ccos (+ start-angle angle)) inner-outer-radius)))
                         (c1-obe-y (+ cy (* (csin (+ start-angle angle)) inner-outer-radius)))
                         (c1-oae-x (+ cx (* (ccos (+ start-angle angle step-length)) inner-outer-radius)))
                         (c1-oae-y (+ cy (* (csin (+ start-angle angle step-length)) inner-outer-radius)))
                         (c1-ibe-x (+ cx (* (ccos (+ start-angle angle (* inner-steps-left step-length))) outer-inner-radius)))
                         (c1-ibe-y (+ cy (* (csin (+ start-angle angle (* inner-steps-left step-length))) outer-inner-radius)))
                         (c1-iae-x (+ cx (* (ccos (+ start-angle angle (* (1+ inner-steps-left) step-length))) outer-inner-radius)))
                         (c1-iae-y (+ cy (* (csin (+ start-angle angle (* (1+ inner-steps-left) step-length))) outer-inner-radius)))
                         (c1-ie-x (+ c1-ibe-x (* (- c1-iae-x c1-ibe-x) t-inner)))
                         (c1-ie-y (+ c1-ibe-y (* (- c1-iae-y c1-ibe-y) t-inner)))
                         (c1-oe-x (+ c1-obe-x (* (- c1-oae-x c1-obe-x) t-outer)))
                         (c1-oe-y (+ c1-obe-y (* (- c1-oae-y c1-obe-y) t-outer)))
                         ;; Cap 2
                         (c2-obe-x (+ cx (* (ccos (- end-angle angle)) inner-outer-radius)))
                         (c2-obe-y (+ cy (* (csin (- end-angle angle)) inner-outer-radius)))
                         (c2-oae-x (+ cx (* (ccos (- end-angle angle step-length)) inner-outer-radius)))
                         (c2-oae-y (+ cy (* (csin (- end-angle angle step-length)) inner-outer-radius)))
                         (c2-ibe-x (+ cx (* (ccos (- end-angle angle (* inner-steps-left step-length))) outer-inner-radius)))
                         (c2-ibe-y (+ cy (* (csin (- end-angle angle (* inner-steps-left step-length))) outer-inner-radius)))
                         (c2-iae-x (+ cx (* (ccos (- end-angle angle (* (1+ inner-steps-left) step-length))) outer-inner-radius)))
                         (c2-iae-y (+ cy (* (csin (- end-angle angle (* (1+ inner-steps-left) step-length))) outer-inner-radius)))
                         (c2-ie-x (+ c2-ibe-x (* (- c2-iae-x c2-ibe-x) t-inner)))
                         (c2-ie-y (+ c2-ibe-y (* (- c2-iae-y c2-ibe-y) t-inner)))
                         (c2-oe-x (+ c2-obe-x (* (- c2-oae-x c2-obe-x) t-outer)))
                         (c2-oe-y (+ c2-obe-y (* (- c2-oae-y c2-obe-y) t-outer)))
                         (steps-count (if inner-angles-cross-each-other (floor total-steps-left 2) inner-steps-left)))
                    (dotimes (i steps-count)
                      ;; Cap 1
                      (%vertex c1-obe-x c1-obe-y)
                      (%polar cx cy (+ start-angle angle) outer-inner-radius)
                      (%polar cx cy (+ start-angle angle step-length) outer-inner-radius)
                      ;; Cap 2
                      (%vertex c2-obe-x c2-obe-y)
                      (%polar cx cy (- end-angle angle step-length) outer-inner-radius)
                      (%polar cx cy (- end-angle angle) outer-inner-radius)
                      (incf angle step-length))
                    ;; When the inner angles coming from `startAngle` and `endAngle` cross each other,
                    ;; the `*innerVertexEnd` vertices go past each other and cause the geometry to intersect itself
                    (if inner-angles-cross-each-other
                        ;; We need to find where the line defined by `cap1InnerVertexEnd` and `cap1OuterVertexEnd` intersects
                        ;; the line defined by `cap2InnerVertexEnd` and `cap2OuterVertexEnd`
                        ;; That point is then used instead to prevent the outline from intersecting itself
                        (let* (;; Make `cap1InnerVertexEnd` the origin and the angle to `cap1OuterVertexEnd` 0 degrees
                               (t1ox (- c1-oe-x c1-ie-x)) (t1oy (- c1-oe-y c1-ie-y))
                               (t2ix (- c2-ie-x c1-ie-x)) (t2iy (- c2-ie-y c1-ie-y))
                               (t2ox (- c2-oe-x c1-ie-x)) (t2oy (- c2-oe-y c1-ie-y))
                               (rotate-by (- (atan t1oy t1ox)))
                               ;; We only need the y coordinates, so only rotate the y coordinates
                               (start (+ (* (sin rotate-by) t2ix) (* (cos rotate-by) t2iy)))
                               (end (+ (* (sin rotate-by) t2ox) (* (cos rotate-by) t2oy)))
                               (t-cross (/ start (- start end)))
                               (ix (+ c2-ie-x (* (- c2-oe-x c2-ie-x) t-cross)))
                               (iy (+ c2-ie-y (* (- c2-oe-y c2-ie-y) t-cross))))
                          (if (evenp segments)
                              ;; There are an even number of segments (which means there's an odd number of vertices),
                              ;; so there's 1 vertex exactly in the middle
                              (let ((mx (+ cx (* (ccos (+ start-angle angle)) outer-inner-radius)))
                                    (my (+ cy (* (csin (+ start-angle angle)) outer-inner-radius))))
                                ;; Cap 1
                                (%vertex ix iy) (%vertex c1-oe-x c1-oe-y) (%vertex c1-obe-x c1-obe-y)
                                (%vertex ix iy) (%vertex c1-obe-x c1-obe-y) (%vertex mx my)
                                ;; Cap 2
                                (%vertex ix iy) (%vertex mx my) (%vertex c2-obe-x c2-obe-y)
                                (%vertex ix iy) (%vertex c2-obe-x c2-obe-y) (%vertex c2-oe-x c2-oe-y))
                              ;; There are an odd number of segments (which means there's an even number of vertices),
                              ;; so there are 2 vertices in the middle
                              (let ((m1x (+ cx (* (ccos (+ start-angle angle)) outer-inner-radius)))
                                    (m1y (+ cy (* (csin (+ start-angle angle)) outer-inner-radius)))
                                    (m2x (+ cx (* (ccos (- end-angle angle)) outer-inner-radius)))
                                    (m2y (+ cy (* (csin (- end-angle angle)) outer-inner-radius))))
                                ;; Cap 1
                                (%vertex ix iy) (%vertex c1-oe-x c1-oe-y) (%vertex c1-obe-x c1-obe-y)
                                (%vertex ix iy) (%vertex c1-obe-x c1-obe-y) (%vertex m1x m1y)
                                ;; Cap 2
                                (%vertex ix iy) (%vertex m2x m2y) (%vertex c2-obe-x c2-obe-y)
                                (%vertex ix iy) (%vertex c2-obe-x c2-obe-y) (%vertex c2-oe-x c2-oe-y)
                                ;; Triangle between the caps
                                (%vertex ix iy) (%vertex ix iy) (%vertex m1x m1y)
                                (%vertex ix iy) (%vertex m1x m1y) (%vertex m2x m2y))))
                        (progn
                          ;; Cap 1
                          (%vertex c1-obe-x c1-obe-y) (%vertex c1-ibe-x c1-ibe-y) (%vertex c1-ie-x c1-ie-y)
                          (%vertex c1-obe-x c1-obe-y) (%vertex c1-ie-x c1-ie-y) (%vertex c1-oe-x c1-oe-y)
                          ;; Cap 2
                          (%vertex c2-obe-x c2-obe-y) (%vertex c2-oe-x c2-oe-y) (%vertex c2-ie-x c2-ie-y)
                          (%vertex c2-obe-x c2-obe-y) (%vertex c2-ie-x c2-ie-y) (%vertex c2-ibe-x c2-ibe-y)))))
                (let (;; Cap 1
                      (c1-fi-x (+ cx (* (ccos start-angle) inner-inner-radius)))
                      (c1-fi-y (+ cy (* (csin start-angle) inner-inner-radius)))
                      (c1-fo-x (+ cx (* (ccos start-angle) outer-outer-radius)))
                      (c1-fo-y (+ cy (* (csin start-angle) outer-outer-radius)))
                      ;; Cap 2
                      (c2-fi-x (+ cx (* (ccos end-angle) inner-inner-radius)))
                      (c2-fi-y (+ cy (* (csin end-angle) inner-inner-radius)))
                      (c2-fo-x (+ cx (* (ccos end-angle) outer-outer-radius)))
                      (c2-fo-y (+ cy (* (csin end-angle) outer-outer-radius))))
                  ;; Cap 1
                  (%vertex c1-fi-x c1-fi-y) (%vertex c1-fo-x c1-fo-y) (%vertex cap1-second-outer-x cap1-second-outer-y)
                  (%vertex c1-fi-x c1-fi-y) (%vertex cap1-second-outer-x cap1-second-outer-y) (%vertex cap1-second-inner-x cap1-second-inner-y)
                  ;; Cap 2
                  (%vertex c2-fi-x c2-fi-y) (%vertex cap2-second-inner-x cap2-second-inner-y) (%vertex cap2-second-outer-x cap2-second-outer-y)
                  (%vertex c2-fi-x c2-fi-y) (%vertex cap2-second-outer-x cap2-second-outer-y) (%vertex c2-fo-x c2-fo-y)
                  (when caps-intersect
                    ;; Cap 1
                    (%vertex cap1-second-inner-x cap1-second-inner-y)
                    (%vertex cap1-second-outer-x cap1-second-outer-y)
                    (%vertex cap-intersection-x cap-intersection-y)
                    ;; Cap 2
                    (%vertex cap2-second-inner-x cap2-second-inner-y)
                    (%vertex cap-intersection-x cap-intersection-y)
                    (%vertex cap2-second-outer-x cap2-second-outer-y)))))
          (rl-end))))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Splines functions
;;;----------------------------------------------------------------------------------

(defun %spline-strip-push (vertices index cx cy dx dy size)
  "Store the pair of strip vertices around (CX, CY) at 2*INDEX and 2*INDEX+1"
  (setf (aref vertices (* 2 index)) (vec2 (+ cx (* dy size)) (- cy (* dx size)))
        (aref vertices (1+ (* 2 index))) (vec2 (- cx (* dy size)) (+ cy (* dx size)))))

(defun draw-spline-linear (points point-count thick color)
  "Draw spline: Linear, minimum 2 points"
  (when (< point-count 2) (return-from draw-spline-linear nil))
  (let ((points (%points points))
        (scale 0.0))
    (dotimes (i (1- point-count))
      (let* ((p (svref points i)) (q (svref points (1+ i)))
             (dx (- (%x q) (%x p)))
             (dy (- (%y q) (%y p)))
             (len (sqrt (+ (* dx dx) (* dy dy)))))
        (when (> len 0) (setf scale (/ thick (* 2 len))))
        (let ((rx (* (- scale) dy)) (ry (* scale dx)))
          (draw-triangle-strip
           (vector (vec2 (- (%x p) rx) (- (%y p) ry))
                   (vec2 (+ (%x p) rx) (+ (%y p) ry))
                   (vec2 (- (%x q) rx) (- (%y q) ry))
                   (vec2 (+ (%x q) rx) (+ (%y q) ry)))
           4 color))))))

(defun %basis-coefficients (p1 p2 p3 p4)
  "Return the B-Spline polynomial coefficient arrays A (x) and B (y)"
  (flet ((coeffs (a b c d)
           (vector (/ (+ (- a) (* 3.0 b) (* -3.0 c) d) 6.0)
                   (/ (+ (* 3.0 a) (* -6.0 b) (* 3.0 c)) 6.0)
                   (/ (+ (* -3.0 a) (* 3.0 c)) 6.0)
                   (/ (+ a (* 4.0 b) c) 6.0))))
    (values (coeffs (%x p1) (%x p2) (%x p3) (%x p4))
            (coeffs (%y p1) (%y p2) (%y p3) (%y p4)))))

(defun %basis-point (a b tt)
  (values (+ (aref a 3) (* tt (+ (aref a 2) (* tt (+ (aref a 1) (* tt (aref a 0)))))))
          (+ (aref b 3) (* tt (+ (aref b 2) (* tt (+ (aref b 1) (* tt (aref b 0)))))))))

(defun %catmull-rom-point (p1 p2 p3 p4 tt)
  (let ((q0 (+ (* -1.0 tt tt tt) (* 2.0 tt tt) (* -1.0 tt)))
        (q1 (+ (* 3.0 tt tt tt) (* -5.0 tt tt) 2.0))
        (q2 (+ (* -3.0 tt tt tt) (* 4.0 tt tt) tt))
        (q3 (- (* tt tt tt) (* tt tt))))
    (values (* 0.5 (+ (* (%x p1) q0) (* (%x p2) q1) (* (%x p3) q2) (* (%x p4) q3)))
            (* 0.5 (+ (* (%y p1) q0) (* (%y p2) q1) (* (%y p3) q2) (* (%y p4) q3))))))

(defun draw-spline-basis (points point-count thick color)
  "Draw spline: B-Spline, minimum 4 points"
  (when (< point-count 4) (return-from draw-spline-basis nil))
  (let* ((points (%points points))
         (n +spline-segment-divisions+)
         (vertices (make-array (+ (* 2 n) 2)))
         (dy 0.0) (dx 0.0) (size 0.0)
         (cur-x 0.0) (cur-y 0.0))
    (dotimes (i (- point-count 3))
      (multiple-value-bind (a b) (%basis-coefficients (svref points i) (svref points (+ i 1))
                                                      (svref points (+ i 2)) (svref points (+ i 3)))
        (setf cur-x (aref a 3) cur-y (aref b 3))
        (when (= i 0) (draw-circle-v (vec2 cur-x cur-y) (/ thick 2.0) color)) ; Draw init line circle-cap
        (when (> i 0) (%spline-strip-push vertices 0 cur-x cur-y dx dy size))
        (loop for j from 1 to n
              do (multiple-value-bind (next-x next-y) (%basis-point a b (/ (float j) (float n)))
                   (setf dy (- next-y cur-y)
                         dx (- next-x cur-x)
                         size (%half-thick-size thick dx dy))
                   (when (and (= i 0) (= j 1)) (%spline-strip-push vertices 0 cur-x cur-y dx dy size))
                   (%spline-strip-push vertices j next-x next-y dx dy size)
                   (setf cur-x next-x cur-y next-y)))
        (draw-triangle-strip vertices (+ (* 2 n) 2) color)))
    ;; Cap circle drawing at the end of every segment
    (draw-circle-v (vec2 cur-x cur-y) (/ thick 2.0) color)))

(defun draw-spline-catmull-rom (points point-count thick color)
  "Draw spline: Catmull-Rom, minimum 4 points"
  (when (< point-count 4) (return-from draw-spline-catmull-rom nil))
  (let* ((points (%points points))
         (n +spline-segment-divisions+)
         (vertices (make-array (+ (* 2 n) 2)))
         (dy 0.0) (dx 0.0) (size 0.0)
         (cur-x (%x (svref points 1))) (cur-y (%y (svref points 1))))
    (draw-circle-v (vec2 cur-x cur-y) (/ thick 2.0) color) ; Draw init line circle-cap
    (dotimes (i (- point-count 3))
      (let ((p1 (svref points i)) (p2 (svref points (+ i 1)))
            (p3 (svref points (+ i 2))) (p4 (svref points (+ i 3))))
        (when (> i 0) (%spline-strip-push vertices 0 cur-x cur-y dx dy size))
        (loop for j from 1 to n
              do (multiple-value-bind (next-x next-y)
                     (%catmull-rom-point p1 p2 p3 p4 (/ (float j) (float n)))
                   (setf dy (- next-y cur-y)
                         dx (- next-x cur-x)
                         size (%half-thick-size thick dx dy))
                   (when (and (= i 0) (= j 1)) (%spline-strip-push vertices 0 cur-x cur-y dx dy size))
                   (%spline-strip-push vertices j next-x next-y dx dy size)
                   (setf cur-x next-x cur-y next-y)))
        (draw-triangle-strip vertices (+ (* 2 n) 2) color)))
    ;; Cap circle drawing at the end of every segment
    (draw-circle-v (vec2 cur-x cur-y) (/ thick 2.0) color)))

(defun draw-spline-bezier-quadratic (points point-count thick color)
  "Draw spline: Quadratic Bezier, minimum 3 points (1 control point): [p1, c2, p3, c4...]"
  (when (>= point-count 3)
    (let ((points (%points points)))
      (loop for i from 0 below (- point-count 2) by 2
            do (draw-spline-segment-bezier-quadratic (svref points i) (svref points (+ i 1))
                                                     (svref points (+ i 2)) thick color)))))

(defun draw-spline-bezier-cubic (points point-count thick color)
  "Draw spline: Cubic Bezier, minimum 4 points (2 control points): [p1, c2, c3, p4, c5, c6...]"
  (when (>= point-count 4)
    (let ((points (%points points)))
      (loop for i from 0 below (- point-count 3) by 3
            do (draw-spline-segment-bezier-cubic (svref points i) (svref points (+ i 1))
                                                 (svref points (+ i 2)) (svref points (+ i 3))
                                                 thick color)))))

(defun draw-spline-segment-linear (p1 p2 thick color)
  "Draw spline segment: Linear, 2 points"
  ;; NOTE: For the linear spline no subdivisions are used, only a single quad
  (let* ((dx (- (%x p2) (%x p1)))
         (dy (- (%y p2) (%y p1)))
         (len (sqrt (+ (* dx dx) (* dy dy)))))
    (when (and (> len 0) (> thick 0))
      (let* ((scale (/ thick (* 2 len)))
             (rx (* (- scale) dy)) (ry (* scale dx)))
        (draw-triangle-strip
         (vector (vec2 (- (%x p1) rx) (- (%y p1) ry))
                 (vec2 (+ (%x p1) rx) (+ (%y p1) ry))
                 (vec2 (- (%x p2) rx) (- (%y p2) ry))
                 (vec2 (+ (%x p2) rx) (+ (%y p2) ry)))
         4 color)))))

(defun %draw-spline-segment (point-fn start-x start-y thick color)
  "Shared body of the spline segment drawers: POINT-FN maps t to x, y
   NOTE: raylib also evaluates t = 0 for B-Spline/Catmull-Rom segments; the vertices
   produced there are overwritten by the t = step iteration, so callers just pass the
   t = 0 point as START-X/START-Y"
  (let* ((n +spline-segment-divisions+)
         (step (/ 1.0 n))
         (points (make-array (+ (* 2 n) 2)))
         (prev-x start-x) (prev-y start-y))
    (loop for i from 1 to n
          do (multiple-value-bind (cur-x cur-y) (funcall point-fn (* step i))
               (let* ((dy (- cur-y prev-y))
                      (dx (- cur-x prev-x))
                      (size (%half-thick-size thick dx dy)))
                 (when (= i 1) (%spline-strip-push points 0 prev-x prev-y dx dy size))
                 (%spline-strip-push points i cur-x cur-y dx dy size)
                 (setf prev-x cur-x prev-y cur-y))))
    (draw-triangle-strip points (+ (* 2 n) 2) color)))

(defun draw-spline-segment-basis (p1 p2 p3 p4 thick color)
  "Draw spline segment: B-Spline, 4 points"
  (multiple-value-bind (a b) (%basis-coefficients p1 p2 p3 p4)
    (%draw-spline-segment (lambda (tt) (%basis-point a b tt))
                          (aref a 3) (aref b 3) thick color)))

(defun draw-spline-segment-catmull-rom (p1 p2 p3 p4 thick color)
  "Draw spline segment: Catmull-Rom, 4 points"
  ;; NOTE: The curve starts at p2 (the t = 0 point)
  (%draw-spline-segment (lambda (tt) (%catmull-rom-point p1 p2 p3 p4 tt))
                        (%x p2) (%y p2) thick color))

(defun draw-spline-segment-bezier-quadratic (p1 c2 p3 thick color)
  "Draw spline segment: Quadratic Bezier, 2 points, 1 control point"
  (%draw-spline-segment (lambda (tt)
                          (let ((a (expt (- 1.0 tt) 2))
                                (b (* 2.0 (- 1.0 tt) tt))
                                (c (expt tt 2)))
                            ;; NOTE: The easing functions aren't suitable here because they don't take a control point
                            (values (+ (* a (%x p1)) (* b (%x c2)) (* c (%x p3)))
                                    (+ (* a (%y p1)) (* b (%y c2)) (* c (%y p3))))))
                        (%x p1) (%y p1) thick color))

(defun draw-spline-segment-bezier-cubic (p1 c2 c3 p4 thick color)
  "Draw spline segment: Cubic Bezier, 2 points, 2 control points"
  (%draw-spline-segment (lambda (tt)
                          (let ((a (expt (- 1.0 tt) 3))
                                (b (* 3.0 (expt (- 1.0 tt) 2) tt))
                                (c (* 3.0 (- 1.0 tt) (expt tt 2)))
                                (d (expt tt 3)))
                            (values (+ (* a (%x p1)) (* b (%x c2)) (* c (%x c3)) (* d (%x p4)))
                                    (+ (* a (%y p1)) (* b (%y c2)) (* c (%y c3)) (* d (%y p4))))))
                        (%x p1) (%y p1) thick color))

(defun get-spline-point-linear (start-pos end-pos tt)
  "Get (evaluate) spline point: Linear"
  (vec2 (+ (* (%x start-pos) (- 1.0 tt)) (* (%x end-pos) tt))
        (+ (* (%y start-pos) (- 1.0 tt)) (* (%y end-pos) tt))))

(defun get-spline-point-basis (p1 p2 p3 p4 tt)
  "Get (evaluate) spline point: B-Spline"
  (multiple-value-bind (a b) (%basis-coefficients p1 p2 p3 p4)
    (multiple-value-bind (x y) (%basis-point a b tt)
      (vec2 x y))))

(defun get-spline-point-catmull-rom (p1 p2 p3 p4 tt)
  "Get (evaluate) spline point: Catmull-Rom"
  (multiple-value-bind (x y) (%catmull-rom-point p1 p2 p3 p4 tt)
    (vec2 x y)))

(defun get-spline-point-bezier-quadratic (start-pos control-pos end-pos tt)
  "Get (evaluate) spline point: Quadratic Bezier"
  (let ((a (expt (- 1.0 tt) 2))
        (b (* 2.0 (- 1.0 tt) tt))
        (c (expt tt 2)))
    (vec2 (+ (* a (%x start-pos)) (* b (%x control-pos)) (* c (%x end-pos)))
          (+ (* a (%y start-pos)) (* b (%y control-pos)) (* c (%y end-pos))))))

(defun get-spline-point-bezier-cubic (start-pos start-control-pos end-control-pos end-pos tt)
  "Get (evaluate) spline point: Cubic Bezier"
  (let ((a (expt (- 1.0 tt) 3))
        (b (* 3.0 (expt (- 1.0 tt) 2) tt))
        (c (* 3.0 (- 1.0 tt) (expt tt 2)))
        (d (expt tt 3)))
    (vec2 (+ (* a (%x start-pos)) (* b (%x start-control-pos)) (* c (%x end-control-pos)) (* d (%x end-pos)))
          (+ (* a (%y start-pos)) (* b (%y start-control-pos)) (* c (%y end-control-pos)) (* d (%y end-pos))))))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Collision Detection functions
;;;----------------------------------------------------------------------------------

(defun check-collision-point-rec (point rec)
  "Check if point is inside rectangle"
  (multiple-value-bind (x y w h) (%rec rec)
    (and (>= (%x point) x) (< (%x point) (+ x w))
         (>= (%y point) y) (< (%y point) (+ y h)))))

(defun check-collision-point-circle (point center radius)
  "Check if point is inside circle"
  (let ((distance-squared (+ (expt (- (%x point) (%x center)) 2)
                             (expt (- (%y point) (%y center)) 2))))
    (<= distance-squared (* radius radius))))

(defun check-collision-point-triangle (point p1 p2 p3)
  "Check if point is inside a triangle defined by three points (p1, p2, p3)"
  (let* ((px (%x point)) (py (%y point))
         (x1 (%x p1)) (y1 (%y p1))
         (x2 (%x p2)) (y2 (%y p2))
         (x3 (%x p3)) (y3 (%y p3))
         (denom (+ (* (- y2 y3) (- x1 x3)) (* (- x3 x2) (- y1 y3)))))
    (unless (zerop denom)
      (let* ((alpha (/ (+ (* (- y2 y3) (- px x3)) (* (- x3 x2) (- py y3))) denom))
             (beta (/ (+ (* (- y3 y1) (- px x3)) (* (- x1 x3) (- py y3))) denom))
             (gamma (- 1.0 alpha beta)))
        (and (> alpha 0) (> beta 0) (> gamma 0))))))

;; NOTE: Based on http://jeffreythompson.org/collision-detection/poly-point.php
(defun check-collision-point-poly (point points point-count)
  "Check if point is within a polygon described by array of vertices"
  (let ((collision nil))
    (when (> point-count 2)
      (let ((points (%points points))
            (px (%x point)) (py (%y point)))
        (loop for i from 0 below point-count
              for j = (1- point-count) then (1- i)
              for pa = (svref points i)
              for pb = (svref points j)
              do (when (and (not (eq (> (%y pa) py) (> (%y pb) py)))
                            (< px (+ (/ (* (- (%x pb) (%x pa)) (- py (%y pa)))
                                        (- (%y pb) (%y pa)))
                                     (%x pa))))
                   (setf collision (not collision))))))
    collision))

(defun check-collision-recs (rec1 rec2)
  "Check collision between two rectangles"
  (multiple-value-bind (x1 y1 w1 h1) (%rec rec1)
    (multiple-value-bind (x2 y2 w2 h2) (%rec rec2)
      (and (< x1 (+ x2 w2)) (> (+ x1 w1) x2)
           (< y1 (+ y2 h2)) (> (+ y1 h1) y2)))))

(defun check-collision-circles (center1 radius1 &optional center2 radius2)
  "Check collision between two circles
   Also accepts two circle structs: (check-collision-circles circle1 circle2)"
  (unless center2
    (psetf center1 (circle-center center1) radius1 (circle-radius center1)
           center2 (circle-center radius1) radius2 (circle-radius radius1)))
  (let* ((dx (- (%x center2) (%x center1)))     ; X distance between centers
         (dy (- (%y center2) (%y center1)))     ; Y distance between centers
         (distance-squared (+ (* dx dx) (* dy dy)))
         (radius-sum (+ radius1 radius2)))
    (<= distance-squared (* radius-sum radius-sum))))

;; NOTE: Reviewed version to take into account corner limit case
(defun check-collision-circle-rec (center radius rec)
  "Check collision between circle and rectangle"
  (multiple-value-bind (x y w h) (%rec rec)
    (let* ((rec-center-x (+ x (/ w 2.0)))
           (rec-center-y (+ y (/ h 2.0)))
           (dx (abs (- (%x center) rec-center-x)))
           (dy (abs (- (%y center) rec-center-y))))
      (when (and (<= dx (+ (/ w 2.0) radius)) (<= dy (+ (/ h 2.0) radius)))
        (cond ((<= dx (/ w 2.0)) t)
              ((<= dy (/ h 2.0)) t)
              (t (let ((corner-distance-sq (+ (expt (- dx (/ w 2.0)) 2)
                                              (expt (- dy (/ h 2.0)) 2))))
                   (<= corner-distance-sq (* radius radius)))))))))

;; REF: https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line_segment
(defun check-collision-lines (start-pos1 end-pos1 start-pos2 end-pos2 &optional collision-point)
  "Check the collision between two lines defined by two points each.
   Returns the collision flag and, as second value, the collision point; when a vec2
   COLLISION-POINT is given it is also updated in place (C out parameter)"
  (let* ((rx (- (%x end-pos1) (%x start-pos1)))
         (ry (- (%y end-pos1) (%y start-pos1)))
         (sx (- (%x end-pos2) (%x start-pos2)))
         (sy (- (%y end-pos2) (%y start-pos2)))
         (div (- (* rx sy) (* ry sx))))
    (if (>= (abs div) single-float-epsilon)
        (let* ((s12x (- (%x start-pos2) (%x start-pos1)))
               (s12y (- (%y start-pos2) (%y start-pos1)))
               (tt (/ (- (* s12x sy) (* s12y sx)) div))
               (u (/ (- (* s12x ry) (* s12y rx)) div)))
          (if (and (<= 0.0 tt 1.0) (<= 0.0 u 1.0))
              (let ((px (+ (%x start-pos1) (* tt rx)))
                    (py (+ (%y start-pos1) (* tt ry))))
                (when (typep collision-point 'vec2)
                  (setf (vx collision-point) px
                        (vy collision-point) py))
                (values t (vec2 px py)))
              (values nil nil)))
        (values nil nil))))

(defun check-collision-point-line (point p1 p2 threshold)
  "Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]"
  (let* ((dxc (- (%x point) (%x p1)))
         (dyc (- (%y point) (%y p1)))
         (dxl (- (%x p2) (%x p1)))
         (dyl (- (%y p2) (%y p1)))
         (cross (- (* dxc dyl) (* dyc dxl))))
    (when (< (abs cross) (* threshold (max (abs dxl) (abs dyl))))
      (if (>= (abs dxl) (abs dyl))
          (if (> dxl 0)
              (<= (%x p1) (%x point) (%x p2))
              (<= (%x p2) (%x point) (%x p1)))
          (if (> dyl 0)
              (<= (%y p1) (%y point) (%y p2))
              (<= (%y p2) (%y point) (%y p1)))))))

(defun check-collision-circle-line (center radius p1 p2)
  "Check if circle collides with a line created between two points [p1] and [p2]"
  (let ((dx (- (%x p1) (%x p2)))
        (dy (- (%y p1) (%y p2))))
    (if (<= (+ (abs dx) (abs dy)) single-float-epsilon)
        (check-collision-circles p1 0 center radius)
        (let* ((length-sq (+ (* dx dx) (* dy dy)))
               (dot-product (clamp (/ (+ (* (- (%x center) (%x p1)) (- (%x p2) (%x p1)))
                                         (* (- (%y center) (%y p1)) (- (%y p2) (%y p1))))
                                      length-sq)
                                   0.0 1.0))
               (dx2 (- (- (%x p1) (* dot-product dx)) (%x center)))
               (dy2 (- (- (%y p1) (* dot-product dy)) (%y center)))
               (distance-sq (+ (* dx2 dx2) (* dy2 dy2))))
          (<= distance-sq (* radius radius))))))

(defun get-collision-rec (rec1 rec2)
  "Get collision rectangle for two rectangles collision"
  (multiple-value-bind (x1 y1 w1 h1) (%rec rec1)
    (multiple-value-bind (x2 y2 w2 h2) (%rec rec2)
      (let ((left (max x1 x2))
            (right (min (+ x1 w1) (+ x2 w2)))
            (top (max y1 y2))
            (bottom (min (+ y1 h1) (+ y2 h2))))
        (if (and (< left right) (< top bottom))
            (make-rectangle :x left :y top :width (- right left) :height (- bottom top))
            (make-rectangle))))))

;;;----------------------------------------------------------------------------------
;;; Additional helpers (not part of raylib)
;;;----------------------------------------------------------------------------------

(defun rectangle (x y width height)
  "Create a rectangle as a list (x y width height)"
  (list x y width height))
