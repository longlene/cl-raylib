(in-package #:cl-raylib)

;;;===================================================================================
;;; vox_loader - MagicaVoxel .vox file format loader
;;; Port of raylib/src/external/vox_loader.h (used by rmodels.c)
;;;===================================================================================

(defconstant +vox-success+ 0)
(defconstant +vox-error-file-not-found+ -1)
(defconstant +vox-error-invalid-format+ -2)
(defconstant +vox-error-file-version-not-match+ -3)

(defconstant +vox-chunksize+ 16 "chunk size (CHUNKSIZE*CHUNKSIZE*CHUNKSIZE) in voxels")
(defconstant +vox-chunksize-opshift+ 4 "1<<4=16 -> Warning depend of CHUNKSIZE")
(defconstant +vox-chunk-flattenoffset-opshift+ 8 "Warning depend of CHUNKSIZE")

;; Array for voxels
;; Array is divised into chunks of CHUNKSIZE*CHUNKSIZE*CHUNKSIZE voxels size
(defstruct (vox-array-3d (:conc-name voxa-))
  ;; Array size in voxels
  (size-x 0) (size-y 0) (size-z 0)
  ;; Chunks size into array (array is divised into chunks)
  (chunks-size-x 0) (chunks-size-y 0) (chunks-size-z 0)
  ;; Chunks array (vector of NIL or octet vectors)
  (array-chunks nil)
  (chunk-flatten-offset 0)
  (chunks-allocated 0)
  (chunks-total 0)
  ;; Arrays for mesh build
  (vertices (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)) ; x y z ...
  (normals (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
  (indices (make-array 0 :element-type '(unsigned-byte 16) :adjustable t :fill-pointer 0))
  (colors (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) ; r g b a ...
  ;; Palette for voxels (256 RGBA colors)
  (palette (make-array (* 256 4) :element-type '(unsigned-byte 8) :initial-element 0)))

;; used right handed system and CCW face
;; indexes for voxelcoords, per face orientation
(alexandria:define-constant +vox-fv+
  #(#(0 2 6 4)                          ; -X
    #(5 7 3 1)                          ; +X
    #(0 4 5 1)                          ; -y
    #(6 2 3 7)                          ; +y
    #(1 3 2 0)                          ; -Z
    #(4 6 7 5))                         ; +Z
  :test #'equalp)

(alexandria:define-constant +vox-solid-vertex+
  #((0 0 0) (1 0 0) (0 1 0) (1 1 0) (0 0 1) (1 0 1) (0 1 1) (1 1 1))
  :test #'equalp)

(alexandria:define-constant +vox-faces-per-side-normal+
  #((-1.0 0.0 0.0) (1.0 0.0 0.0) (0.0 -1.0 0.0) (0.0 1.0 0.0) (0.0 0.0 -1.0) (0.0 0.0 1.0))
  :test #'equalp)

;; Allocated VoxArray3D size
(defun %vox-alloc-array (voxarray sx sy sz)
  (let* ((sx (+ sx (mod (- +vox-chunksize+ (rem sx +vox-chunksize+)) +vox-chunksize+)))
         (sy (+ sy (mod (- +vox-chunksize+ (rem sy +vox-chunksize+)) +vox-chunksize+)))
         (sz (+ sz (mod (- +vox-chunksize+ (rem sz +vox-chunksize+)) +vox-chunksize+)))
         (chx (ash sx (- +vox-chunksize-opshift+))) ; Chunks Count in X
         (chy (ash sy (- +vox-chunksize-opshift+))) ; Chunks Count in Y
         (chz (ash sz (- +vox-chunksize-opshift+)))) ; Chunks Count in Z
    (setf (voxa-size-x voxarray) sx
          (voxa-size-y voxarray) sy
          (voxa-size-z voxarray) sz
          (voxa-chunks-size-x voxarray) chx
          (voxa-chunks-size-y voxarray) chy
          (voxa-chunks-size-z voxarray) chz
          (voxa-chunk-flatten-offset voxarray) (* chy chz) ; m_arrayChunks[(x * (sy*sz)) + (z * sy) + y]
          ;; Alloc chunks array
          (voxa-array-chunks voxarray) (make-array (* chx chy chz) :initial-element nil)
          (voxa-chunks-total voxarray) (* chx chy chz)
          (voxa-chunks-allocated voxarray) 0)))

(defun %vox-chunk-offset (voxarray x y z)
  (+ (* (ash x (- +vox-chunksize-opshift+)) (voxa-chunk-flatten-offset voxarray))
     (* (ash z (- +vox-chunksize-opshift+)) (voxa-chunks-size-y voxarray))
     (ash y (- +vox-chunksize-opshift+))))

(defun %vox-in-chunk-offset (x y z)
  (let ((chx (- x (ash (ash x -4) 4)))
        (chy (- y (ash (ash y -4) 4)))
        (chz (- z (ash (ash z -4) 4))))
    (+ (ash chx +vox-chunk-flattenoffset-opshift+) (ash chz +vox-chunksize-opshift+) chy)))

;; Set voxel ID from its position into VoxArray3D
(defun %vox-set-voxel (voxarray x y z id)
  ;; A .vox file can place voxels outside the volume its own SIZE chunk declares,
  ;; and the offsets below are derived directly from those coordinates. Same range
  ;; checks Vox_GetVoxel() already performs.
  (when (or (< x 0) (< y 0) (< z 0)) (return-from %vox-set-voxel))
  (when (or (>= x (voxa-size-x voxarray)) (>= y (voxa-size-y voxarray)) (>= z (voxa-size-z voxarray)))
    (return-from %vox-set-voxel))
  (let ((offset (%vox-chunk-offset voxarray x y z)))
    (when (or (< offset 0) (>= offset (voxa-chunks-total voxarray))) (return-from %vox-set-voxel))
    (let ((chunk (aref (voxa-array-chunks voxarray) offset)))
      (unless chunk
        (setf chunk (make-array (* +vox-chunksize+ +vox-chunksize+ +vox-chunksize+) :element-type '(unsigned-byte 8) :initial-element 0)
              (aref (voxa-array-chunks voxarray) offset) chunk)
        (incf (voxa-chunks-allocated voxarray)))
      (let ((offset (%vox-in-chunk-offset x y z)))
        (when (and (>= offset 0) (< offset (length chunk)))
          (setf (aref chunk offset) id))))))

;; Get voxel ID from its position into VoxArray3D
(defun %vox-get-voxel (voxarray x y z)
  (when (or (< x 0) (< y 0) (< z 0)) (return-from %vox-get-voxel 0))
  (when (or (>= x (voxa-size-x voxarray)) (>= y (voxa-size-y voxarray)) (>= z (voxa-size-z voxarray)))
    (return-from %vox-get-voxel 0))
  (let ((chunk (aref (voxa-array-chunks voxarray) (%vox-chunk-offset voxarray x y z))))
    (if chunk
        (aref chunk (%vox-in-chunk-offset x y z))
        0)))

;; Calc visibles faces from a voxel position
(defun %vox-calc-faces-visible (voxarray cx cy cz)
  (let ((mask 0))
    (when (= (%vox-get-voxel voxarray (- cx 1) cy cz) 0) (setf mask (logior mask (ash 1 0)))) ; -x
    (when (= (%vox-get-voxel voxarray (+ cx 1) cy cz) 0) (setf mask (logior mask (ash 1 1)))) ; +x
    (when (= (%vox-get-voxel voxarray cx (- cy 1) cz) 0) (setf mask (logior mask (ash 1 2)))) ; -y
    (when (= (%vox-get-voxel voxarray cx (+ cy 1) cz) 0) (setf mask (logior mask (ash 1 3)))) ; +y
    (when (= (%vox-get-voxel voxarray cx cy (- cz 1)) 0) (setf mask (logior mask (ash 1 4)))) ; -z
    (when (= (%vox-get-voxel voxarray cx cy (+ cz 1)) 0) (setf mask (logior mask (ash 1 5)))) ; +z
    mask))

;; Get a vertex position from a voxel's corner
(defun %vox-get-vertex-position (wcx wcy wcz num-vertex)
  (let ((scale 0.25)
        (vtx (aref +vox-solid-vertex+ num-vertex)))
    (list (* (+ (float (first vtx) 1.0) wcx) scale)
          (* (+ (float (second vtx) 1.0) wcy) scale)
          (* (+ (float (third vtx) 1.0) wcz) scale))))

;; Build a voxel vertices/colors/indices
(defun %vox-build-voxel (voxarray x y z mat-id)
  (let ((mask (%vox-calc-faces-visible voxarray x y z)))
    (when (= mask 0) (return-from %vox-build-voxel))
    (let ((vert-computed (make-array 8 :initial-element '(0.0 0.0 0.0))))
      ;; For each Cube's faces
      (dotimes (i 6)
        (when (logtest mask (ash 1 i))  ; If face is visible
          (loop for num-vertex across (aref +vox-fv+ i)
                do (setf (aref vert-computed num-vertex) (%vox-get-vertex-position x y z num-vertex)))))
      ;; Add face
      (dotimes (i 6)
        (when (logtest mask (ash 1 i))
          (let ((idx (truncate (length (voxa-vertices voxarray)) 3)))
            (loop for v across (aref +vox-fv+ i)
                  do (dolist (c (aref vert-computed v)) (vector-push-extend c (voxa-vertices voxarray)))
                     (dolist (c (aref +vox-faces-per-side-normal+ i)) (vector-push-extend c (voxa-normals voxarray)))
                     (dotimes (k 4) (vector-push-extend (aref (voxa-palette voxarray) (+ (* mat-id 4) k)) (voxa-colors voxarray))))
            ;; v0 - v1 - v2, v0 - v2 - v3
            (dolist (k '(0 2 1 0 3 2))
              (vector-push-extend (logand (+ idx k) #xffff) (voxa-indices voxarray)))))))))

;; MagicaVoxel *.vox file format Loader
;; Returns a status code (VOX_SUCCESS on success)
(defun vox-load-from-memory (vox-data vox-data-size voxarray)
  (flet ((u32 (p) (logior (aref vox-data p) (ash (aref vox-data (+ p 1)) 8)
                          (ash (aref vox-data (+ p 2)) 16) (ash (aref vox-data (+ p 3)) 24))))
    ;; 4 bytes: magic number ('V' 'O' 'X' 'space')
    ;; 4 bytes: version number (current version is 150)
    (unless (and (>= vox-data-size 8)
                 (equalp (subseq vox-data 0 4) (map 'vector #'char-code "VOX ")))
      (return-from vox-load-from-memory +vox-error-invalid-format+)) ; "Not an MagicaVoxel File format"
    (let ((version (u32 4))
          (p 8)
          (end vox-data-size))
      (unless (or (= version 150) (= version 200))
        (return-from vox-load-from-memory +vox-error-file-version-not-match+)) ; "MagicaVoxel version doesn't match"

      ;; header
      ;; 4 bytes: chunk id
      ;; 4 bytes: size of chunk contents (n)
      ;; 4 bytes: total size of children chunks(m)
      (loop while (< p end)
            do ;; A chunk header is 12 bytes: id + content size + children size
               (when (< (- end p) 12) (return))
               (let ((chunk-name (map 'string #'code-char (subseq vox-data p (+ p 4))))
                     (chunk-size (u32 (+ p 4))))
                 (incf p 12)
                 (cond
                   ((string= chunk-name "SIZE")
                    (when (< (- end p) 12) (return))
                    ;; (4 bytes x 3 : x, y, z )
                    (let ((size-x (u32 p)) (size-y (u32 (+ p 4))) (size-z (u32 (+ p 8))))
                      (incf p 12)
                      ;; Alloc vox array
                      (%vox-alloc-array voxarray (%i32 size-x) (%i32 size-z) (%i32 size-y)))) ; Reverse Y<>Z for left to right handed system
                   ((string= chunk-name "XYZI")
                    ;; (numVoxels : 4 bytes )
                    ;; (each voxel: 1 byte x 4 : x, y, z, colorIndex ) x numVoxels
                    (when (< (- end p) 4) (return))
                    (let ((num-voxels (u32 p)))
                      (incf p 4)
                      (when (> num-voxels (truncate (- end p) 4))
                        (setf num-voxels (truncate (- end p) 4)))
                      (dotimes (i num-voxels)
                        (let ((vx (aref vox-data p)) (vy (aref vox-data (+ p 1)))
                              (vz (aref vox-data (+ p 2))) (vi (aref vox-data (+ p 3))))
                          (incf p 4)
                          (%vox-set-voxel voxarray vx vz (- (voxa-size-z voxarray) vy 1) vi))))) ; Reverse Y<>Z for left to right handed system
                   ((string= chunk-name "RGBA")
                    (when (< (- end p) (* (- 256 1) 4)) (return))
                    ;; (each pixel: 1 byte x 4 : r, g, b, a ) x 256
                    (dotimes (i (- 256 1))
                      (dotimes (k 4)
                        (setf (aref (voxa-palette voxarray) (+ (* (+ i 1) 4) k)) (aref vox-data p))
                        (incf p))))
                   (t
                    (when (> chunk-size (- end p)) (return))
                    (incf p chunk-size)))))

      ;;////////////////////////////////////////////////////////
      ;; Building Mesh
      ;;   TODO compute globals indices array
      ;; Create vertices and indices buffers
      (when (voxa-array-chunks voxarray)
        (loop for x from 0 to (voxa-size-x voxarray)
              do (loop for z from 0 to (voxa-size-z voxarray)
                       do (loop for y from 0 to (voxa-size-y voxarray)
                                do (let ((mat-id (%vox-get-voxel voxarray x y z)))
                                     (when (/= mat-id 0)
                                       (%vox-build-voxel voxarray x y z mat-id)))))))
      +vox-success+)))
