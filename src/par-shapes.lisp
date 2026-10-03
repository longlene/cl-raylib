(in-package #:cl-raylib)

;;;===================================================================================
;;; par_shapes - Simple C library for creation of triangle meshes
;;; Port of the raylib/src/external/par_shapes.h subset used by rmodels.c
;;; (parametric shapes, platonic cube, disk, merge, transformations, normals, weld)
;;;
;;; NOTE: C float/double promotions are reproduced (PAR_PI is a double constant,
;;; sin()/cos()/sqrt() are double functions) and sinf()/cosf() come from libm
;;;===================================================================================

(defconstant +par-pi+ 3.14159265359d0)

(defstruct (par-shapes-mesh (:conc-name par-))
  (points (make-array 0 :element-type 'single-float) :type (simple-array single-float (*))) ; Flat list of 3-tuples (X Y Z X Y Z...)
  (npoints 0 :type fixnum)              ; Number of points
  (triangles (make-array 0 :element-type '(unsigned-byte 16)) :type (simple-array (unsigned-byte 16) (*))) ; Flat list of 3-tuples (I J K I J K...)
  (ntriangles 0 :type fixnum)           ; Number of triangles
  (normals nil :type (or null (simple-array single-float (*)))) ; Optional list of 3-tuples (X Y Z X Y Z...)
  (tcoords nil :type (or null (simple-array single-float (*))))) ; Optional list of 2-tuples (U V U V U V...)

(defvar *par-shapes-epsilon-welded-normals* 0.001)
(defvar *par-shapes-epsilon-degenerate-sphere* 0.0001)

(declaim (inline %sinf %cosf %f32 %d))
(defun %sinf (x) (cffi:foreign-funcall "sinf" :float (float x 1.0) :float))
(defun %cosf (x) (cffi:foreign-funcall "cosf" :float (float x 1.0) :float))
(defun %f32 (x) (float x 1.0))
(defun %d (x) (float x 1d0))

(defun %par-floats (n)
  (make-array n :element-type 'single-float :initial-element 0.0))

(defun %par-indices (n)
  (make-array n :element-type '(unsigned-byte 16) :initial-element 0))

;; Vector helpers operating on (array offset) 3-tuples
(defmacro %par-v3 (array offset)
  `(values (aref ,array ,offset) (aref ,array (+ ,offset 1)) (aref ,array (+ ,offset 2))))

(defun %par-normalize3 (v &optional (o 0))
  (let ((lsqr (%f32 (sqrt (%d (+ (* (aref v o) (aref v o))
                                 (* (aref v (+ o 1)) (aref v (+ o 1)))
                                 (* (aref v (+ o 2)) (aref v (+ o 2)))))))))
    (when (> lsqr 0)
      (let ((a (/ 1.0 lsqr)))
        (setf (aref v o) (* (aref v o) a)
              (aref v (+ o 1)) (* (aref v (+ o 1)) a)
              (aref v (+ o 2)) (* (aref v (+ o 2)) a))))
    v))

(defun %par-cross3 (ax ay az bx by bz)
  (values (- (* ay bz) (* az by))
          (- (* az bx) (* ax bz))
          (- (* ax by) (* ay bx))))

;; Parametric surfaces: (u v userdata) -> x y z
(defun %par-sphere (u v userdata)
  (declare (ignore userdata))
  (let ((phi (%f32 (* (%d u) +par-pi+)))
        (theta (%f32 (* (%d (* v 2)) +par-pi+))))
    (values (* (%cosf theta) (%sinf phi))
            (* (%sinf theta) (%sinf phi))
            (%cosf phi))))

(defun %par-hemisphere (u v userdata)
  (declare (ignore userdata))
  (let ((phi (%f32 (* (%d u) +par-pi+)))
        (theta (%f32 (* (%d v) +par-pi+))))
    (values (* (%cosf theta) (%sinf phi))
            (* (%sinf theta) (%sinf phi))
            (%cosf phi))))

(defun %par-plane (u v userdata)
  (declare (ignore userdata))
  (values u v 0.0))

(defun %par-cylinder (u v userdata)
  (declare (ignore userdata))
  (let ((theta (%f32 (* (%d (* v 2)) +par-pi+))))
    (values (%sinf theta) (%cosf theta) u)))

(defun %par-cone (u v userdata)
  (declare (ignore userdata))
  (let ((r (- 1.0 u))
        (theta (%f32 (* (%d (* v 2)) +par-pi+))))
    (values (* r (%sinf theta)) (* r (%cosf theta)) u)))

(defun %par-torus (u v userdata)
  (let* ((major 1.0)
         (minor userdata)
         (theta (%f32 (* (%d (* u 2)) +par-pi+)))
         (phi (%f32 (* (%d (* v 2)) +par-pi+)))
         (beta (+ major (* minor (%cosf phi)))))
    (values (* (%cosf theta) beta)
            (* (%sinf theta) beta)
            (* (%sinf phi) minor))))

(defun %par-trefoil (uu vv userdata)
  (let* ((minor userdata)
         (a 0.5) (b 0.3) (c 0.5)
         (d (* minor 0.1))
         (u (%f32 (* (%d (* (- 1 uu) 4)) +par-pi+)))
         (v (%f32 (* (%d (* vv 2)) +par-pi+)))
         (u15 (%d (* 1.5 u)))
         (r (%f32 (+ a (* b (cos u15)))))
         (x (%f32 (* r (cos (%d u)))))
         (y (%f32 (* r (sin (%d u)))))
         (z (%f32 (* c (sin u15))))
         (q (make-array 3 :element-type 'single-float)))
    (setf (aref q 0) (%f32 (- (* (* -1.5 b) (sin u15) (cos (%d u)))
                              (* (+ a (* b (cos u15))) (sin (%d u)))))
          (aref q 1) (%f32 (+ (* (* -1.5 b) (sin u15) (sin (%d u)))
                              (* (+ a (* b (cos u15))) (cos (%d u)))))
          (aref q 2) (%f32 (* (* 1.5 c) (cos u15))))
    (%par-normalize3 q)
    (let ((qvn (make-array 3 :element-type 'single-float
                             :initial-contents (list (aref q 1) (- (aref q 0)) 0.0))))
      (%par-normalize3 qvn)
      (multiple-value-bind (w0 w1 w2) (%par-cross3 (aref q 0) (aref q 1) (aref q 2)
                                                   (aref qvn 0) (aref qvn 1) (aref qvn 2))
        (values (%f32 (+ x (* d (+ (* (aref qvn 0) (cos (%d v))) (* w0 (sin (%d v)))))))
                (%f32 (+ y (* d (+ (* (aref qvn 1) (cos (%d v))) (* w1 (sin (%d v)))))))
                (%f32 (+ z (* (* d w2) (sin (%d v))))))))))

;;;----------------------------------------------------------------------------------
;;; Mesh generation
;;;----------------------------------------------------------------------------------

(defun par-shapes-create-parametric (fn slices stacks userdata)
  (let* ((mesh (make-par-shapes-mesh))
         (npoints (* (+ slices 1) (+ stacks 1)))
         (points (%par-floats (* 3 npoints)))
         (tcoords (%par-floats (* 2 npoints)))
         (ntriangles (* 2 slices stacks))
         (triangles (%par-indices (* 3 ntriangles))))
    ;; Generate verts.
    (let ((p 0))
      (dotimes (stack (+ stacks 1))
        (let ((u (/ (%f32 stack) stacks)))
          (dotimes (slice (+ slices 1))
            (let ((v (/ (%f32 slice) slices)))
              (multiple-value-bind (x y z) (funcall fn u v userdata)
                (setf (aref points p) (%f32 x)
                      (aref points (+ p 1)) (%f32 y)
                      (aref points (+ p 2)) (%f32 z))
                (incf p 3)))))))
    ;; Generate texture coordinates.
    (let ((p 0))
      (dotimes (stack (+ stacks 1))
        (let ((u (/ (%f32 stack) stacks)))
          (dotimes (slice (+ slices 1))
            (setf (aref tcoords p) u
                  (aref tcoords (+ p 1)) (/ (%f32 slice) slices))
            (incf p 2)))))
    ;; Generate faces.
    (let ((v 0) (f 0))
      (dotimes (stack stacks)
        (dotimes (slice slices)
          (let ((next (+ slice 1)))
            (flet ((put (i) (setf (aref triangles f) (logand i #xffff)) (incf f)))
              (put (+ v slice slices 1))
              (put (+ v next))
              (put (+ v slice))
              (put (+ v slice slices 1))
              (put (+ v next slices 1))
              (put (+ v next)))))
        (incf v (+ slices 1))))
    (setf (par-npoints mesh) npoints
          (par-points mesh) points
          (par-tcoords mesh) tcoords
          (par-ntriangles mesh) ntriangles
          (par-triangles mesh) triangles)
    (%par-shapes-compute-welded-normals mesh)
    mesh))

(defun par-shapes-create-cylinder (slices stacks)
  (when (and (>= slices 3) (>= stacks 1))
    (par-shapes-create-parametric #'%par-cylinder slices stacks nil)))

(defun par-shapes-create-cone (slices stacks)
  (when (and (>= slices 3) (>= stacks 1))
    (par-shapes-create-parametric #'%par-cone slices stacks nil)))

(defun par-shapes-create-parametric-sphere (slices stacks)
  (when (and (>= slices 3) (>= stacks 3))
    (let ((m (par-shapes-create-parametric #'%par-sphere slices stacks nil)))
      (par-shapes-remove-degenerate m *par-shapes-epsilon-degenerate-sphere*)
      m)))

(defun par-shapes-create-hemisphere (slices stacks)
  (when (and (>= slices 3) (>= stacks 3))
    (let ((m (par-shapes-create-parametric #'%par-hemisphere slices stacks nil)))
      (par-shapes-remove-degenerate m *par-shapes-epsilon-degenerate-sphere*)
      m)))

(defun par-shapes-create-torus (slices stacks radius)
  (when (and (>= slices 3) (>= stacks 3))
    (assert (<= radius 1.0) () "Use smaller radius to avoid self-intersection.")
    (assert (>= radius 0.1) () "Use larger radius to avoid self-intersection.")
    (par-shapes-create-parametric #'%par-torus slices stacks (%f32 radius))))

(defun par-shapes-create-trefoil-knot (slices stacks radius)
  (when (and (>= slices 3) (>= stacks 3))
    (assert (<= radius 3.0) () "Use smaller radius to avoid self-intersection.")
    (assert (>= radius 0.5) () "Use larger radius to avoid self-intersection.")
    (par-shapes-create-parametric #'%par-trefoil slices stacks (%f32 radius))))

(defun par-shapes-create-plane (slices stacks)
  (when (and (>= slices 1) (>= stacks 1))
    (par-shapes-create-parametric #'%par-plane slices stacks nil)))

(defun par-shapes-create-cube ()
  (let* ((verts '(0 0 0  0 1 0  1 1 0  1 0 0  0 0 1  0 1 1  1 1 1  1 0 1))
         (quads '((7 6 5 4)            ; front
                  (0 1 2 3)            ; back
                  (6 7 3 2)            ; right
                  (5 6 2 1)            ; top
                  (4 5 1 0)            ; left
                  (7 4 0 3)))          ; bottom
         (mesh (make-par-shapes-mesh :npoints 8
                                     :points (make-array 24 :element-type 'single-float
                                                            :initial-contents (mapcar #'%f32 verts))
                                     :ntriangles 12))
         (tris (%par-indices 36))
         (i 0))
    (loop for (q0 q1 q2 q3) in quads
          do (dolist (v (list q0 q1 q2 q2 q3 q0))
               (setf (aref tris i) v)
               (incf i)))
    (setf (par-triangles mesh) tris)
    mesh))

(defun par-shapes-create-disk (radius slices center normal)
  (let* ((radius (%f32 radius))
         (npoints (+ slices 1))
         (mesh (make-par-shapes-mesh :npoints npoints :points (%par-floats (* 3 npoints))))
         (points (par-points mesh)))
    (dotimes (i slices)
      (let ((theta (%f32 (/ (* (* i +par-pi+) 2) slices))))
        (setf (aref points (* 3 (+ i 1))) (%f32 (* radius (cos (%d theta))))
              (aref points (+ (* 3 (+ i 1)) 1)) (%f32 (* radius (sin (%d theta))))
              (aref points (+ (* 3 (+ i 1)) 2)) 0.0)))
    (let ((nnormal (make-array 3 :element-type 'single-float
                                 :initial-contents (mapcar #'%f32 normal))))
      (%par-normalize3 nnormal)
      (let ((norms (%par-floats (* 3 npoints))))
        (dotimes (i npoints)
          (replace norms nnormal :start1 (* 3 i)))
        (setf (par-normals mesh) norms))
      (let ((triangles (%par-indices (* 3 slices))))
        (dotimes (i slices)
          (setf (aref triangles (* 3 i)) 0
                (aref triangles (+ (* 3 i) 1)) (+ 1 i)
                (aref triangles (+ (* 3 i) 2)) (+ 1 (mod (+ i 1) slices))))
        (setf (par-ntriangles mesh) slices
              (par-triangles mesh) triangles))
      (multiple-value-bind (ax ay az) (%par-cross3 (aref nnormal 0) (aref nnormal 1) (aref nnormal 2) 0.0 0.0 -1.0)
        (let ((axis (make-array 3 :element-type 'single-float :initial-contents (list ax ay az))))
          (%par-normalize3 axis)
          (par-shapes-rotate mesh (%f32 (acos (%d (aref nnormal 2)))) axis))))
    (par-shapes-translate mesh (%f32 (elt center 0)) (%f32 (elt center 1)) (%f32 (elt center 2)))
    mesh))

(defun par-shapes-create-empty ()
  (make-par-shapes-mesh))

(defun par-shapes-free-mesh (mesh)
  (declare (ignore mesh))
  nil)

(defun par-shapes-set-epsilon-welded-normals (epsilon)
  (setf *par-shapes-epsilon-welded-normals* (%f32 epsilon)))

(defun par-shapes-set-epsilon-degenerate-sphere (epsilon)
  (setf *par-shapes-epsilon-degenerate-sphere* (%f32 epsilon)))

;;;----------------------------------------------------------------------------------
;;; Transformations
;;;----------------------------------------------------------------------------------

(defun par-shapes-merge (dst src)
  (let* ((offset (par-npoints dst))
         (npoints (+ (par-npoints dst) (par-npoints src)))
         (points (%par-floats (* 3 npoints))))
    (replace points (par-points dst) :end2 (* 3 (par-npoints dst)))
    (replace points (par-points src) :start1 (* 3 offset) :end2 (* 3 (par-npoints src)))
    (setf (par-points dst) points
          (par-npoints dst) npoints)
    (when (or (par-normals src) (par-normals dst))
      (let ((normals (%par-floats (* 3 npoints))))
        (when (par-normals dst) (replace normals (par-normals dst) :end2 (* 3 offset)))
        (when (par-normals src) (replace normals (par-normals src) :start1 (* 3 offset) :end2 (* 3 (par-npoints src))))
        (setf (par-normals dst) normals)))
    (when (or (par-tcoords src) (par-tcoords dst))
      (let ((tcoords (%par-floats (* 2 npoints))))
        (when (par-tcoords dst) (replace tcoords (par-tcoords dst) :end2 (* 2 offset)))
        (when (par-tcoords src) (replace tcoords (par-tcoords src) :start1 (* 2 offset) :end2 (* 2 (par-npoints src))))
        (setf (par-tcoords dst) tcoords)))
    (let* ((ntriangles (+ (par-ntriangles dst) (par-ntriangles src)))
           (triangles (%par-indices (* 3 ntriangles))))
      (replace triangles (par-triangles dst) :end2 (* 3 (par-ntriangles dst)))
      (dotimes (i (* 3 (par-ntriangles src)))
        (setf (aref triangles (+ (* 3 (par-ntriangles dst)) i))
              (logand (+ offset (aref (par-triangles src) i)) #xffff)))
      (setf (par-triangles dst) triangles
            (par-ntriangles dst) ntriangles))))

(defun par-shapes-merge-and-free (dst src)
  (par-shapes-merge dst src)
  (par-shapes-free-mesh src))

(defun par-shapes-translate (m x y z)
  (let ((points (par-points m)) (x (%f32 x)) (y (%f32 y)) (z (%f32 z)))
    (dotimes (i (par-npoints m))
      (incf (aref points (* 3 i)) x)
      (incf (aref points (+ (* 3 i) 1)) y)
      (incf (aref points (+ (* 3 i) 2)) z))))

(defun par-shapes-rotate (mesh radians axis)
  (let* ((s (%sinf radians))
         (c (%cosf radians))
         (x (%f32 (elt axis 0)))
         (y (%f32 (elt axis 1)))
         (z (%f32 (elt axis 2)))
         (xy (* x y))
         (yz (* y z))
         (zx (* z x))
         (one-minus-c (- 1.0 c))
         (col0 (list (+ (* (* x x) one-minus-c) c) (+ (* xy one-minus-c) (* z s)) (- (* zx one-minus-c) (* y s))))
         (col1 (list (- (* xy one-minus-c) (* z s)) (+ (* (* y y) one-minus-c) c) (+ (* yz one-minus-c) (* x s))))
         (col2 (list (+ (* zx one-minus-c) (* y s)) (- (* yz one-minus-c) (* x s)) (+ (* (* z z) one-minus-c) c))))
    (flet ((transform (array)
             (dotimes (i (par-npoints mesh))
               (let* ((o (* 3 i))
                      (p0 (aref array o)) (p1 (aref array (+ o 1))) (p2 (aref array (+ o 2))))
                 (setf (aref array o) (+ (* (first col0) p0) (* (first col1) p1) (* (first col2) p2))
                       (aref array (+ o 1)) (+ (* (second col0) p0) (* (second col1) p1) (* (second col2) p2))
                       (aref array (+ o 2)) (+ (* (third col0) p0) (* (third col1) p1) (* (third col2) p2)))))))
      (transform (par-points mesh))
      (when (par-normals mesh) (transform (par-normals mesh))))))

(defun par-shapes-scale (m x y z)
  (let ((points (par-points m)) (x (%f32 x)) (y (%f32 y)) (z (%f32 z)))
    (dotimes (i (par-npoints m))
      (setf (aref points (* 3 i)) (* (aref points (* 3 i)) x)
            (aref points (+ (* 3 i) 1)) (* (aref points (+ (* 3 i) 1)) y)
            (aref points (+ (* 3 i) 2)) (* (aref points (+ (* 3 i) 2)) z)))
    (let ((n (par-normals m)))
      (when (and n (not (and (= x y) (= y z))))
        (let ((x-zero (= x 0)) (y-zero (= y 0)) (z-zero (= z 0)))
          (if (and (not x-zero) (not y-zero) (not z-zero))
              (setf x (/ 1.0 x) y (/ 1.0 y) z (/ 1.0 z))
              (setf x (if (and x-zero (not y-zero) (not z-zero)) 1.0 0.0)
                    y (if (and y-zero (not x-zero) (not z-zero)) 1.0 0.0)
                    z (if (and z-zero (not x-zero) (not y-zero)) 1.0 0.0))))
        (dotimes (i (par-npoints m))
          (let ((o (* 3 i)))
            (setf (aref n o) (* (aref n o) x)
                  (aref n (+ o 1)) (* (aref n (+ o 1)) y)
                  (aref n (+ o 2)) (* (aref n (+ o 2)) z))
            (%par-normalize3 n o)))))))

(defun par-shapes-compute-aabb (m)
  "Returns the aabb as a 6 floats vector (minx miny minz maxx maxy maxz)"
  (let ((points (par-points m))
        (aabb (%par-floats 6)))
    (setf (aref aabb 0) (aref points 0) (aref aabb 3) (aref points 0)
          (aref aabb 1) (aref points 1) (aref aabb 4) (aref points 1)
          (aref aabb 2) (aref points 2) (aref aabb 5) (aref points 2))
    (loop for i from 1 below (par-npoints m)
          for o = (* 3 i)
          do (dotimes (c 3)
               (let ((p (aref points (+ o c))))
                 ;; PAR_MIN(a, b) (a > b ? b : a), PAR_MAX(a, b) (a > b ? a : b)
                 (setf (aref aabb c) (if (> p (aref aabb c)) (aref aabb c) p)
                       (aref aabb (+ c 3)) (if (> p (aref aabb (+ c 3))) p (aref aabb (+ c 3)))))))
    aabb))

(defun par-shapes-clone (mesh)
  (make-par-shapes-mesh :npoints (par-npoints mesh)
                        :points (copy-seq (par-points mesh))
                        :ntriangles (par-ntriangles mesh)
                        :triangles (copy-seq (par-triangles mesh))
                        :normals (when (par-normals mesh) (copy-seq (par-normals mesh)))
                        :tcoords (when (par-tcoords mesh) (copy-seq (par-tcoords mesh)))))

(defun par-shapes-compute-normals (m)
  (let ((normals (%par-floats (* 3 (par-npoints m))))
        (points (par-points m))
        (tris (par-triangles m)))
    (setf (par-normals m) normals)
    (dotimes (f (par-ntriangles m))
      (let ((ia (* 3 (aref tris (* 3 f))))
            (ib (* 3 (aref tris (+ (* 3 f) 1))))
            (ic (* 3 (aref tris (+ (* 3 f) 2)))))
        (flet ((accumulate (target p0 p1 p2)
                 ;; next = p1 - p0, prev = p2 - p0, cp = cross(next, prev)
                 (multiple-value-bind (cx cy cz)
                     (%par-cross3 (- (aref points p1) (aref points p0))
                                  (- (aref points (+ p1 1)) (aref points (+ p0 1)))
                                  (- (aref points (+ p1 2)) (aref points (+ p0 2)))
                                  (- (aref points p2) (aref points p0))
                                  (- (aref points (+ p2 1)) (aref points (+ p0 1)))
                                  (- (aref points (+ p2 2)) (aref points (+ p0 2))))
                   (incf (aref normals target) cx)
                   (incf (aref normals (+ target 1)) cy)
                   (incf (aref normals (+ target 2)) cz))))
          (accumulate ia ia ib ic)
          (accumulate ib ib ic ia)
          (accumulate ic ic ia ib))))
    (dotimes (p (par-npoints m))
      (%par-normalize3 normals (* 3 p)))))

(defun %par-shapes-grid-index (points d gridsize)
  (let ((o (* d 3)))
    (+ (truncate (aref points o))
       (* gridsize (truncate (aref points (+ o 1))))
       (* gridsize gridsize (truncate (aref points (+ o 2)))))))

(defun %par-shapes-sort-points (mesh gridsize)
  "Spatially sort the points, returns the sortmap (inverse reorder mapping)
NOTE: glibc qsort() is a stable merge sort, reproduced with STABLE-SORT"
  (let* ((npoints (par-npoints mesh))
         (points (par-points mesh))
         (sortmap (stable-sort (let ((v (%par-indices npoints))) (dotimes (i npoints v) (setf (aref v i) i)))
                               #'< :key (lambda (d) (%par-shapes-grid-index points d gridsize))))
         (newpts (%par-floats (* 3 npoints)))
         (invmap (%par-indices npoints)))
    ;; Apply the reorder mapping to the XYZ coordinate data.
    (dotimes (i npoints)
      (setf (aref invmap (aref sortmap i)) i)
      (replace newpts points :start1 (* 3 i) :start2 (* 3 (aref sortmap i)) :end2 (+ 3 (* 3 (aref sortmap i)))))
    (setf (par-points mesh) newpts)
    ;; Apply the inverse reorder mapping to the triangle indices.
    (let ((newinds (%par-indices (* 3 (par-ntriangles mesh)))))
      (dotimes (i (* 3 (par-ntriangles mesh)))
        (setf (aref newinds i) (aref invmap (aref (par-triangles mesh) i))))
      (setf (par-triangles mesh) newinds))
    invmap))

(defun %par-shapes-weld-points (mesh gridsize epsilon weldmap)
  (let* ((npoints-in (par-npoints mesh))
         (points (par-points mesh))
         ;; Each bin contains a "pointer" (really an index) to its first point.
         ;; We add 1 because 0 is reserved to mean that the bin is empty.
         ;; Since the points are spatially sorted, there's no need to store
         ;; a point count in each bin.
         (bins (%par-indices (* gridsize gridsize gridsize)))
         (prev-binindex -1)
         (nremoved 0))
    (dotimes (p npoints-in)
      (let ((this-binindex (%par-shapes-grid-index points p gridsize)))
        (when (/= this-binindex prev-binindex)
          (setf (aref bins this-binindex) (logand (+ 1 p) #xffff)))
        (setf prev-binindex this-binindex)))
    ;; Examine all bins that intersect the epsilon-sized cube centered at each
    ;; point, and check for colocated points within those bins.
    (dotimes (p npoints-in)
      ;; Skip if this point has already been welded.
      (when (= (aref weldmap p) p)
        ;; Build a list of bins that intersect the epsilon-sized cube.
        (let ((nearby nil) (nbins 0)
              (pt (* p 3)))
          (block build
            (let ((minp (loop for c below 3 collect (truncate (- (aref points (+ pt c)) epsilon))))
                  (maxp (loop for c below 3 collect (truncate (+ (aref points (+ pt c)) epsilon)))))
              (loop for i from (first minp) to (first maxp)
                    do (loop for j from (second minp) to (second maxp)
                             do (loop for k from (third minp) to (third maxp)
                                      do (let* ((binindex (+ i (* gridsize j) (* gridsize gridsize k)))
                                                (binvalue (aref bins binindex)))
                                           (when (> binvalue 0)
                                             (when (= nbins 8)
                                               (format t "Epsilon value is too large.~%")
                                               (return))
                                             (push binindex nearby)
                                             (incf nbins))))))))
          (setf nearby (nreverse nearby))
          ;; Check for colocated points in each nearby bin.
          (dolist (binindex nearby)
            (let ((nindex (1- (aref bins binindex))))
              (loop
                ;; If this isn't "self" and it's colocated, then weld it!
                (when (and (/= nindex p) (= (aref weldmap nindex) nindex))
                  (let* ((that (* nindex 3))
                         (dx (- (aref points that) (aref points pt)))
                         (dy (- (aref points (+ that 1)) (aref points (+ pt 1))))
                         (dz (- (aref points (+ that 2)) (aref points (+ pt 2))))
                         (dist2 (+ (* dx dx) (* dy dy) (* dz dz))))
                    (when (< dist2 epsilon)
                      (setf (aref weldmap nindex) p)
                      (incf nremoved))))
                ;; Advance to the next point if possible.
                (when (>= (incf nindex) npoints-in) (return))
                ;; If the next point is outside the bin, then we're done.
                (when (/= (%par-shapes-grid-index points nindex gridsize) binindex) (return))))))))
    ;; Apply the weldmap to the vertices.
    (let* ((npoints (- npoints-in nremoved))
           (newpts (%par-floats (* 3 npoints)))
           (condensed-map (%par-indices npoints-in))
           (ci 0))
      (dotimes (p npoints-in)
        (if (= (aref weldmap p) p)
            (progn
              (replace newpts points :start1 (* 3 ci) :start2 (* 3 p) :end2 (+ 3 (* 3 p)))
              (setf (aref condensed-map p) ci)
              (incf ci))
            (setf (aref condensed-map p) (aref condensed-map (aref weldmap p)))))
      (replace weldmap condensed-map)
      (setf (par-points mesh) newpts
            (par-npoints mesh) npoints))
    ;; Apply the weldmap to the triangle indices and skip the degenerates.
    (let ((tris (par-triangles mesh))
          (ntriangles 0))
      (dotimes (i (par-ntriangles mesh))
        (let ((a (aref weldmap (aref tris (* 3 i))))
              (b (aref weldmap (aref tris (+ (* 3 i) 1))))
              (c (aref weldmap (aref tris (+ (* 3 i) 2)))))
          (when (and (/= a b) (/= a c) (/= b c))
            (setf (aref tris (* 3 ntriangles)) a
                  (aref tris (+ (* 3 ntriangles) 1)) b
                  (aref tris (+ (* 3 ntriangles) 2)) c)
            (incf ntriangles))))
      (setf (par-ntriangles mesh) ntriangles))))

(defun par-shapes-weld (mesh epsilon &optional weldmap)
  "Merge colocated verts, build a new index buffer, and return the optimized mesh
WELDMAP (npoints indices) gets filled with the mapping from old vertex indices to new indices"
  (let* ((clone (par-shapes-clone mesh))
         (gridsize 20)
         (maxcell (%f32 (- gridsize 1)))
         (aabb (par-shapes-compute-aabb clone))
         (scale (list (if (= (aref aabb 3) (aref aabb 0)) 1.0 (/ maxcell (- (aref aabb 3) (aref aabb 0))))
                      (if (= (aref aabb 4) (aref aabb 1)) 1.0 (/ maxcell (- (aref aabb 4) (aref aabb 1))))
                      (if (= (aref aabb 5) (aref aabb 2)) 1.0 (/ maxcell (- (aref aabb 5) (aref aabb 2)))))))
    (par-shapes-translate clone (- (aref aabb 0)) (- (aref aabb 1)) (- (aref aabb 2)))
    (apply #'par-shapes-scale clone scale)
    (let* ((sortmap (%par-shapes-sort-points clone gridsize))
           (owner (null weldmap))
           (weldmap (or weldmap (%par-indices (par-npoints mesh)))))
      (dotimes (i (par-npoints mesh)) (setf (aref weldmap i) i))
      (%par-shapes-weld-points clone gridsize epsilon weldmap)
      (unless owner
        (let ((newmap (%par-indices (par-npoints mesh))))
          (dotimes (i (par-npoints mesh))
            (setf (aref newmap i) (aref weldmap (aref sortmap i))))
          (replace weldmap newmap))))
    (par-shapes-scale clone (%f32 (/ 1d0 (first scale))) (%f32 (/ 1d0 (second scale))) (%f32 (/ 1d0 (third scale))))
    (par-shapes-translate clone (aref aabb 0) (aref aabb 1) (aref aabb 2))
    clone))

(defun %par-shapes-compute-welded-normals (m)
  (let* ((epsilon *par-shapes-epsilon-welded-normals*)
         (weldmap (%par-indices (par-npoints m)))
         (welded (progn
                   ;; NOTE: C allocates (uninitialized) normals before welding, they are not used
                   (setf (par-normals m) (%par-floats (* 3 (par-npoints m))))
                   (par-shapes-weld m epsilon weldmap)))
         (normals (par-normals m)))
    (par-shapes-compute-normals welded)
    (dotimes (i (par-npoints m))
      (replace normals (par-normals welded) :start1 (* 3 i)
                                             :start2 (* 3 (aref weldmap i)) :end2 (+ 3 (* 3 (aref weldmap i)))))))

(defun par-shapes-remove-degenerate (mesh mintriarea)
  (let* ((ntriangles 0)
         (triangles (%par-indices (* 3 (par-ntriangles mesh))))
         (src (par-triangles mesh))
         (points (par-points mesh))
         (mincplen2 (* (* mintriarea 2) (* mintriarea 2))))
    (dotimes (f (par-ntriangles mesh))
      (let ((pa (* 3 (aref src (* 3 f))))
            (pb (* 3 (aref src (+ (* 3 f) 1))))
            (pc (* 3 (aref src (+ (* 3 f) 2)))))
        (multiple-value-bind (cx cy cz)
            (%par-cross3 (- (aref points pb) (aref points pa))
                         (- (aref points (+ pb 1)) (aref points (+ pa 1)))
                         (- (aref points (+ pb 2)) (aref points (+ pa 2)))
                         (- (aref points pc) (aref points pa))
                         (- (aref points (+ pc 1)) (aref points (+ pa 1)))
                         (- (aref points (+ pc 2)) (aref points (+ pa 2))))
          ;; par_shapes__dot3(cp, cp): b[0]*a[0] + b[1]*a[1] + b[2]*a[2]
          (let ((cplen2 (+ (* cx cx) (* cy cy) (* cz cz))))
            (when (>= cplen2 mincplen2)
              (replace triangles src :start1 (* 3 ntriangles) :start2 (* 3 f) :end2 (+ 3 (* 3 f)))
              (incf ntriangles))))))
    (setf (par-ntriangles mesh) ntriangles
          (par-triangles mesh) triangles)))
