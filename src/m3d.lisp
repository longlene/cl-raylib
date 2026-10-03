(in-package #:cl-raylib)

;;;===================================================================================
;;; m3d - Model 3D (M3D) importer
;;; Port of raylib/src/external/m3d.h as compiled by rmodels.c: binary format importer
;;; (no M3D_ASCII, no exporter), m3d_load(), m3d_pose() and m3d_free()
;;;
;;; NOTE: Shapes (SHPE), labels (LBLS), procedural surfaces (PROC) and the bone matrices
;;; and weight lists are not kept, raylib never reads them (they only set non-fatal error
;;; codes). Textures are decoded with the PNG decoder in textures.lisp instead of the
;;; stb_image copy embedded in m3d.h (16 bit PNG textures are returned as 8 bit).
;;; Reads past the end of the file data return 0 (C reads out of bounds). When normals are
;;; generated for a model mixing faces with and without normals, faces with normals add
;;; nothing to the vertex normals (C adds uninitialized memory).
;;;===================================================================================

(defconstant +m3d-undef+ #xffffffff)
(defconstant +m3d-numbone+ 4 "Maximum bones per vertex supported by the importer")

;;; Error codes
(defconstant +m3d-success+ 0)
(defconstant +m3d-err-alloc+ -1)
(defconstant +m3d-err-badfile+ -2)
(defconstant +m3d-err-unimpl+ -65)
(defconstant +m3d-err-unkprop+ -66)
(defconstant +m3d-err-unkmesh+ -67)
(defconstant +m3d-err-unkimg+ -68)
(defconstant +m3d-err-unkframe+ -69)
(defconstant +m3d-err-unkcmd+ -70)
(defconstant +m3d-err-unkvox+ -71)
(defconstant +m3d-err-trunc+ -72)
(defconstant +m3d-err-cmap+ -73)
(defconstant +m3d-err-tmap+ -74)
(defconstant +m3d-err-vrts+ -75)
(defconstant +m3d-err-bone+ -76)
(defconstant +m3d-err-mtrl+ -77)
(defconstant +m3d-err-shpe+ -78)
(defconstant +m3d-err-voxt+ -79)

(defun m3d-err-isfatal (x) (and (< x 0) (> x -65)))

;;; Material property types
(defconstant +m3dp-kd+ 0)
(defconstant +m3dp-ka+ 1)
(defconstant +m3dp-ks+ 2)
(defconstant +m3dp-ns+ 3)
(defconstant +m3dp-ke+ 4)
(defconstant +m3dp-tf+ 5)
(defconstant +m3dp-km+ 6)
(defconstant +m3dp-d+ 7)
(defconstant +m3dp-il+ 8)
(defconstant +m3dp-pr+ 64)
(defconstant +m3dp-pm+ 65)
(defconstant +m3dp-ps+ 66)
(defconstant +m3dp-ni+ 67)
(defconstant +m3dp-nt+ 68)
(defconstant +m3dp-map-kd+ 128)
(defconstant +m3dp-map-ka+ 129)
(defconstant +m3dp-map-ks+ 130)
(defconstant +m3dp-map-ns+ 131)
(defconstant +m3dp-map-ke+ 132)
(defconstant +m3dp-map-tf+ 133)
(defconstant +m3dp-map-km+ 134)          ; bump map
(defconstant +m3dp-map-d+ 135)
(defconstant +m3dp-map-n+ 136)           ; normal map
(defconstant +m3dp-map-pr+ 192)
(defconstant +m3dp-map-pm+ 193)
(defconstant +m3dp-map-ps+ 194)
(defconstant +m3dp-map-ni+ 195)
(defconstant +m3dp-map-nt+ 196)

;; Material property definitions: (id . format), format one of :color :uint8 :uint16 :uint32 :float :map
(alexandria:define-constant +m3d-propertytypes+
  '((0 . :color) (1 . :color) (2 . :color) (3 . :float) (4 . :color) (5 . :color) (6 . :float) (7 . :float) (8 . :uint8)
    (64 . :float) (65 . :float) (66 . :float) (67 . :float) (68 . :float)
    ;; aliases
    (134 . :map) (136 . :map) (193 . :map))
  :test #'equal)

;;;----------------------------------------------------------------------------------
;;; In-memory model structure
;;;----------------------------------------------------------------------------------

(defstruct (m3d-vertex (:conc-name m3dv-))
  (x 0.0 :type single-float) (y 0.0 :type single-float) (z 0.0 :type single-float) (w 0.0 :type single-float)
  (color 0)                             ; default vertex color
  (skinid 0))                           ; skin index

(defstruct (m3d-face (:conc-name m3df-))
  (materialid +m3d-undef+)
  (vertex (make-array 3 :initial-element +m3d-undef+))
  (normal (make-array 3 :initial-element +m3d-undef+))
  (texcoord (make-array 3 :initial-element +m3d-undef+)))

(defstruct (m3d-bone (:conc-name m3db-))
  (parent +m3d-undef+) (name nil) (pos 0) (ori 0))

(defstruct (m3d-skin (:conc-name m3ds-))
  (boneid (make-array +m3d-numbone+ :initial-element +m3d-undef+))
  (weight (make-array +m3d-numbone+ :element-type 'single-float :initial-element 0.0)))

(defstruct (m3d-texture (:conc-name m3dtx-))
  (name nil) (image nil))               ; decoded image (NIL if not decoded)

(defstruct (m3d-property (:conc-name m3dp-))
  (type 0) (value 0))                   ; color/num (integer), fnum (float) or textureid

(defstruct (m3d-material (:conc-name m3dm-))
  (name nil) (props (make-array 0 :adjustable t :fill-pointer 0)))

(defstruct (m3d-frame (:conc-name m3dfr-))
  (msec 0) (transforms #()))            ; vector of (boneid pos ori)

(defstruct (m3d-action (:conc-name m3da-))
  (name nil) (numframe 0) (durationmsec 0) (frames #()))

(defstruct (m3d-voxel-type (:conc-name m3dvt-))
  (materialid +m3d-undef+) (color 0) (skinid +m3d-undef+))

(defstruct (m3d-voxel (:conc-name m3dvx-))
  (x 0) (y 0) (z 0) (w 0) (h 0) (d 0) (data nil))

(defstruct m3d
  (errcode +m3d-success+)
  (raw nil) (raw-base 0)                ; octets holding the HEAD chunk and its offset
  (scale 1.0)
  (vc-s 0) (vi-s 0) (si-s 0) (ci-s 0) (ti-s 0) (bi-s 0) (nb-s 0) (sk-s 0) (fc-s 0) (hi-s 0) (fi-s 0) (vd-s 0) (vp-s 0)
  (cmap nil)                            ; offset of the color map in raw
  (numcmap 0)
  (tmap #())                            ; vector of (u . v)
  (vertex (make-array 0 :adjustable t :fill-pointer 0))
  (numvertex 0)
  (bone #()) (numbone 0)
  (skin #()) (numskin 0)
  (material (make-array 0 :adjustable t :fill-pointer 0))
  (texture (make-array 0 :adjustable t :fill-pointer 0))
  (face (make-array 0 :adjustable t :fill-pointer 0))
  (voxtype #())
  (voxel (make-array 0 :adjustable t :fill-pointer 0))
  (action (make-array 0 :adjustable t :fill-pointer 0))
  (inlined nil))                        ; list of (name start length)

(defun m3d-numface (model) (fill-pointer (m3d-face model)))
(defun m3d-nummaterial (model) (fill-pointer (m3d-material model)))
(defun m3d-numaction (model) (fill-pointer (m3d-action model)))

;;;----------------------------------------------------------------------------------
;;; Data readers (little endian, 0 past the end of the data)
;;;----------------------------------------------------------------------------------

(declaim (inline %m3d-u8))
(defun %m3d-u8 (data offset)
  (if (< -1 offset (length data)) (aref data offset) 0))
(defun %m3d-u16 (data offset)
  (logior (%m3d-u8 data offset) (ash (%m3d-u8 data (+ offset 1)) 8)))
(defun %m3d-u32 (data offset)
  (logior (%m3d-u16 data offset) (ash (%m3d-u16 data (+ offset 2)) 16)))
(defun %m3d-s8 (data offset)
  (let ((v (%m3d-u8 data offset))) (if (>= v #x80) (- v #x100) v)))
(defun %m3d-s16 (data offset)
  (let ((v (%m3d-u16 data offset))) (if (>= v #x8000) (- v #x10000) v)))
(defun %m3d-s32 (data offset)
  (%i32 (%m3d-u32 data offset)))
(defun %m3d-f32 (data offset)
  (sb-kernel:make-single-float (%m3d-s32 data offset)))
(defun %m3d-f64 (data offset)
  "(float) of a double"
  (float-features:with-float-traps-masked t
    (coerce (sb-kernel:make-double-float (%m3d-s32 data (+ offset 4)) (%m3d-u32 data offset)) 'single-float)))

(defun %m3d-magic-p (data offset magic)
  "M3D_CHUNKMAGIC()"
  (loop for c across magic
        for k from offset
        always (= (%m3d-u8 data k) (char-code c))))

(defun %m3d-getidx (data offset type current)
  "_m3d_getidx(): returns (values index new-offset), CURRENT is kept for unknown sizes"
  (case type
    (1 (let ((v (%m3d-u8 data offset)))
         (values (if (> v 253) (logand (- v 256) #xffffffff) v) (+ offset 1))))
    (2 (let ((v (%m3d-u16 data offset)))
         (values (if (> v 65533) (logand (- v 65536) #xffffffff) v) (+ offset 2))))
    (4 (values (%m3d-u32 data offset) (+ offset 4)))
    (t (values current offset))))

(defun %m3d-c-string (data offset)
  "NUL terminated string at OFFSET (bytes kept as latin-1 characters, like C strcmp)"
  (let ((end (or (position 0 data :start (min offset (length data))) (length data))))
    (map 'string #'code-char (subseq data (min offset (length data)) end))))

(defun %m3d-octets-string (string)
  "Latin-1 kept bytes of STRING as an UTF-8 decoded string"
  (babel:octets-to-string (map '(vector (unsigned-byte 8)) #'char-code string) :encoding :utf-8 :errorp nil))

(defun %m3d-rsq (x)
  "_m3d_rsq(): fast inverse square root (as written in m3d.h, the Newton step uses the approximation as x)"
  (declare (type single-float x))
  (float-features:with-float-traps-masked t
    (let* ((x2 (* x 0.5))
           (i (logand (- #x5f3759df (ash (logand (sb-kernel:single-float-bits x) #xffffffff) -1)) #xffffffff))
           (y (sb-kernel:make-single-float (%i32 i))))
      (* y (- 1.5 (* (* x2 y) y))))))

(defun %m3d-zlib-decode (data offset len)
  "stbi_zlib_decode_malloc_guesssize_headerflag() with zlib header parsing, NIL on error"
  (let* ((end (min (length data) (+ offset len)))
         (cmf (%m3d-u8 data offset))
         (flg (%m3d-u8 data (+ offset 1))))
    (when (or (/= (mod (+ (* cmf 256) flg) 31) 0) (logtest flg 32) (/= (logand cmf 15) 8) (> (+ offset 2) end))
      (return-from %m3d-zlib-decode nil))
    (handler-case (chipz:decompress nil 'chipz:deflate (subseq data (+ offset 2) end))
      (error () nil))))

;;;----------------------------------------------------------------------------------
;;; Texture loading
;;;----------------------------------------------------------------------------------

(defun %m3d-gettx (model readfilecb fn)
  "_m3d_gettx(): load and decode a texture, returns the texture index or +m3d-undef+"
  ;; failsafe
  (when (or (null fn) (zerop (length fn))) (return-from %m3d-gettx +m3d-undef+))
  ;; do we have loaded this texture already?
  (let ((textures (m3d-texture model)))
    (dotimes (i (fill-pointer textures))
      (when (string= fn (m3dtx-name (aref textures i))) (return-from %m3d-gettx i))))
  (let ((buff nil))
    ;; see if it's inlined in the model
    (let ((inlined (find fn (m3d-inlined model) :key #'first :test #'string=)))
      (when inlined
        (destructuring-bind (name start length) inlined
          (declare (ignore name))
          (setf buff (subseq (m3d-raw model) (min start (length (m3d-raw model)))
                             (min (+ start length) (length (m3d-raw model))))))))
    ;; try to load from external source
    (when (and (null buff) readfilecb)
      (let ((i (length fn)))
        (when (or (< i 5) (char/= (char fn (- i 4)) #\.))
          (setf buff (funcall readfilecb (concatenate 'string fn ".png"))))
        (unless buff
          (setf buff (funcall readfilecb fn))
          (unless buff (return-from %m3d-gettx +m3d-undef+)))))
    ;; add to textures array
    (let ((texture (make-m3d-texture :name fn)))
      (vector-push-extend texture (m3d-texture model))
      (when buff
        (if (and (>= (length buff) 4) (= (aref buff 0) #x89) (= (aref buff 1) 80) (= (aref buff 2) 78) (= (aref buff 3) 71))
            ;; return pixel buffer of the decoded texture
            (setf (m3dtx-image texture) (handler-case (%load-png buff) (error () nil)))
            ;; Unimplemented interpreter
            nil))
      (unless (and (m3dtx-image texture) (image-data (m3dtx-image texture)))
        (setf (m3dtx-image texture) nil
              (m3d-errcode model) +m3d-err-unkimg+))
      (1- (fill-pointer (m3d-texture model))))))

;;;----------------------------------------------------------------------------------
;;; Model loading
;;;----------------------------------------------------------------------------------

(defun m3d-load (data readfilecb)
  "Decode a Model 3D file into in-memory format, NIL on failure
READFILECB is called with a file name and returns its octets (or NIL)"
  (unless (and data (%m3d-magic-p data 0 "3DMO")) (return-from m3d-load nil))
  (let* ((model (make-m3d))
         (len (logand (- (%m3d-u32 data 4) 8) #xffffffff))
         (pos 8)
         (buf data)
         (neednorm nil))
    ;; Binary variant
    (when (%m3d-magic-p data pos "PRVW")
      ;; optional preview chunk
      (let ((plen (%m3d-u32 data (+ pos 4))))
        (incf pos plen)
        (setf len (logand (- len plen) #xffffffff))))
    (if (not (%m3d-magic-p data pos "HEAD"))
        (let ((out (%m3d-zlib-decode data pos len)))
          (when (or (null out) (zerop (length out)) (not (%m3d-magic-p out 0 "HEAD")))
            (return-from m3d-load nil))
          (setf buf out pos 0 len (length out)))
        nil)
    (setf (m3d-raw model) buf
          (m3d-raw-base model) pos)

    (let* ((raw pos)
           (end (+ raw len))
           (chunk (+ raw (%m3d-u32 buf (+ raw 4))))
           (types (%m3d-u32 buf (+ raw 12))))
      (flet ((size (shift) (ash 1 (logand (ash types (- shift)) 3))))
        ;; parse header
        (setf (m3d-scale model) (%m3d-f32 buf (+ raw 8)))
        (when (<= (m3d-scale model) 0.0) (setf (m3d-scale model) 1.0))
        (setf (m3d-vc-s model) (size 0)        ; vertex coordinate size
              (m3d-vi-s model) (size 2)        ; vertex index size
              (m3d-si-s model) (size 4)        ; string offset size
              (m3d-ci-s model) (size 6)        ; color index size
              (m3d-ti-s model) (size 8)        ; tmap index size
              (m3d-bi-s model) (size 10)       ; bone index size
              (m3d-nb-s model) (size 12)       ; number of bones per vertex
              (m3d-sk-s model) (size 14)       ; skin index size
              (m3d-fc-s model) (size 16)       ; frame counter size
              (m3d-hi-s model) (size 18)       ; shape index size
              (m3d-fi-s model) (size 20)       ; face index size
              (m3d-vd-s model) (size 22)       ; voxel dimension size
              (m3d-vp-s model) (size 24)))     ; voxel pixel size
      ;; optional indices
      (when (= (m3d-ci-s model) 8) (setf (m3d-ci-s model) 0))
      (when (= (m3d-ti-s model) 8) (setf (m3d-ti-s model) 0))
      (when (= (m3d-bi-s model) 8) (setf (m3d-bi-s model) 0))
      (when (= (m3d-sk-s model) 8) (setf (m3d-sk-s model) 0))
      (when (= (m3d-fc-s model) 8) (setf (m3d-fc-s model) 0))
      (when (= (m3d-hi-s model) 8) (setf (m3d-hi-s model) 0))
      (when (= (m3d-fi-s model) 8) (setf (m3d-fi-s model) 0))

      ;; variable limit checks
      (when (> (m3d-vc-s model) 4)
        ;; Double precision coordinates not supported, truncating to float...
        (setf (m3d-errcode model) +m3d-err-trunc+))
      (when (and (< 2 (m3d-vp-s model)) (/= (m3d-vp-s model) 8))
        ;; 32 bit indices not supported, unable to load model
        (return-from m3d-load nil))
      (when (or (> (m3d-vi-s model) 4) (> (m3d-si-s model) 4) (= (m3d-vp-s model) 4))
        ;; Invalid index size, unable to load model
        (return-from m3d-load nil))
      (unless (%m3d-magic-p buf (- end 4) "OMD3")
        ;; Missing end chunk
        (return-from m3d-load nil))
      (when (> (m3d-nb-s model) +m3d-numbone+)
        ;; Model has more bones per vertex than what importer was configured to support
        (setf (m3d-errcode model) +m3d-err-trunc+))

      (let ((si-s (m3d-si-s model)) (vi-s (m3d-vi-s model)) (vc-s (m3d-vc-s model)) (ci-s (m3d-ci-s model))
            (bi-s (m3d-bi-s model)) (sk-s (m3d-sk-s model)))
        (macrolet ((getidx (place type)
                     `(multiple-value-setq (,place data) (%m3d-getidx buf data ,type ,place)))
                   (getstr (place)
                     ;; M3D_GETSTR(): string offset from the start of the string table (raw + 16)
                     (let ((offs (gensym)))
                       `(let ((,offs 0))
                          (getidx ,offs si-s)
                          (setf ,place (if (/= ,offs 0) (%m3d-c-string buf (+ raw 16 ,offs)) nil))))))
          (flet ((read-color (data)
                   ;; Returns (values color new-data)
                   (case ci-s
                     (1 (values (if (m3d-cmap model) (%m3d-u32 buf (+ (m3d-cmap model) (* 4 (%m3d-u8 buf data)))) 0) (+ data 1)))
                     (2 (values (if (m3d-cmap model) (%m3d-u32 buf (+ (m3d-cmap model) (* 4 (%m3d-u16 buf data)))) 0) (+ data 2)))
                     (4 (values (%m3d-u32 buf data) (+ data 4)))
                     (t (values 0 data)))))

            ;; look for inlined assets in advance, material and procedural chunks may need them
            (let ((buff chunk))
              (loop while (and (< buff end) (not (%m3d-magic-p buf buff "OMD3")))
                    do (let* ((data buff)
                              (clen (%m3d-u32 buf (+ data 4))))
                         (incf buff clen)
                         (when (or (< clen 8) (>= buff end))
                           ;; Invalid chunk size
                           (return))
                         (let ((alen (logand (- clen 8 si-s) #xffffffff)))
                           ;; inlined assets
                           (when (and (%m3d-magic-p buf data "ASET") (> alen 0))
                             (incf data 8)
                             (let ((name nil))
                               (getstr name)
                               (setf (m3d-inlined model) (append (m3d-inlined model) (list (list name data alen))))))))))

            ;; parse chunks
            (loop while (and (< chunk end) (not (%m3d-magic-p buf chunk "OMD3")))
                  do (let* ((data chunk)
                            (clen (%m3d-u32 buf (+ chunk 4))))
                       (incf chunk clen)
                       (when (or (< clen 8) (>= chunk end))
                         ;; Invalid chunk size
                         (return))
                       (let ((len (- clen 8)))
                         (cond
                           ;; color map
                           ((%m3d-magic-p buf data "CMAP")
                            (cond ((m3d-cmap model) (setf (m3d-errcode model) +m3d-err-cmap+))
                                  ((zerop ci-s) (setf (m3d-errcode model) +m3d-err-cmap+))
                                  (t (setf (m3d-numcmap model) (floor len 4)
                                           (m3d-cmap model) (+ data 8)))))
                           ;; texture map
                           ((%m3d-magic-p buf data "TMAP")
                            (cond ((plusp (length (m3d-tmap model))) (setf (m3d-errcode model) +m3d-err-tmap+))
                                  ((zerop (m3d-ti-s model)) (setf (m3d-errcode model) +m3d-err-tmap+))
                                  (t
                                   (let* ((reclen (+ vc-s vc-s))
                                          (tmap (make-array (floor len reclen))))
                                     (setf (m3d-tmap model) tmap)
                                     (loop for i from 0
                                           for d from (+ data 8) by reclen
                                           while (and (< d chunk) (< i (length tmap)))
                                           do (setf (svref tmap i)
                                                    (case vc-s
                                                      (1 (cons (/ (float (%m3d-u8 buf d) 1.0) 255.0) (/ (float (%m3d-u8 buf (+ d 1)) 1.0) 255.0)))
                                                      (2 (cons (/ (float (%m3d-u16 buf d) 1.0) 65535.0) (/ (float (%m3d-u16 buf (+ d 2)) 1.0) 65535.0)))
                                                      (4 (cons (%m3d-f32 buf d) (%m3d-f32 buf (+ d 4))))
                                                      (t (cons (%m3d-f64 buf d) (%m3d-f64 buf (+ d 8)))))))))))
                           ;; vertex list
                           ((%m3d-magic-p buf data "VRTS")
                            (if (plusp (m3d-numvertex model))
                                (setf (m3d-errcode model) +m3d-err-vrts+)
                                (progn
                                  (when (and (/= ci-s 0) (< ci-s 4) (null (m3d-cmap model))) (setf (m3d-errcode model) +m3d-err-cmap+))
                                  (let* ((reclen (+ ci-s sk-s (* 4 vc-s)))
                                         (numvertex (floor len reclen))
                                         (vertices (make-array numvertex :adjustable t :fill-pointer numvertex)))
                                    (dotimes (i numvertex) (setf (aref vertices i) (make-m3d-vertex)))
                                    (setf (m3d-vertex model) vertices
                                          (m3d-numvertex model) numvertex)
                                    (incf data 8)
                                    (loop for i from 0
                                          while (and (< data chunk) (< i numvertex))
                                          do (let ((v (aref vertices i)))
                                               (flet ((coord (k)
                                                        (case vc-s
                                                          (1 (/ (float (%m3d-s8 buf (+ data k)) 1.0) 127.0))
                                                          (2 (/ (float (%m3d-s16 buf (+ data (* 2 k))) 1.0) 32767.0))
                                                          (4 (%m3d-f32 buf (+ data (* 4 k))))
                                                          (t (%m3d-f64 buf (+ data (* 8 k)))))))
                                                 (setf (m3dv-x v) (coord 0) (m3dv-y v) (coord 1) (m3dv-z v) (coord 2) (m3dv-w v) (coord 3)))
                                               (incf data (* 4 vc-s))
                                               (setf (values (m3dv-color v) data) (read-color data))
                                               (setf (m3dv-skinid v) +m3d-undef+)
                                               (let ((skinid +m3d-undef+))
                                                 (getidx skinid sk-s)
                                                 (setf (m3dv-skinid v) skinid))))))))
                           ;; skeleton: bone hierarchy and skin
                           ((%m3d-magic-p buf data "BONE")
                            (cond
                              ((plusp (length (m3d-bone model))) (setf (m3d-errcode model) +m3d-err-bone+))
                              ((zerop bi-s) (setf (m3d-errcode model) +m3d-err-bone+))
                              ((zerop (m3d-numvertex model))
                               ;; No vertex chunk before bones
                               (setf (m3d-errcode model) +m3d-err-vrts+)
                               (return))
                              (t
                               (incf data 8)
                               (let ((numbone 0) (numskin 0))
                                 (getidx numbone bi-s)
                                 (getidx numskin sk-s)
                                 (let ((bones (make-array numbone)) (i 0))
                                   ;; read bone hierarchy
                                   (loop while (and (< data chunk) (< i numbone))
                                         do (let ((bone (make-m3d-bone)) (parent 0) (name nil) (bpos 0) (ori 0))
                                              (getidx parent bi-s)
                                              (getstr name)
                                              (getidx bpos vi-s)
                                              (getidx ori vi-s)
                                              (setf (m3db-parent bone) parent (m3db-name bone) name (m3db-pos bone) bpos (m3db-ori bone) ori
                                                    (svref bones i) bone)
                                              (incf i)))
                                   (when (/= i numbone)
                                     ;; Truncated bone chunk
                                     (setf numbone i numskin 0 (m3d-errcode model) +m3d-err-bone+))
                                   (setf (m3d-bone model) (subseq bones 0 numbone)
                                         (m3d-numbone model) numbone))
                                 ;; read skin definitions
                                 (when (plusp numskin)
                                   (let ((skins (make-array numskin)) (i 0) (nb-s (m3d-nb-s model)))
                                     (loop while (and (< data chunk) (< i numskin))
                                           do (let ((skin (make-m3d-skin))
                                                    (weights (make-array 8 :initial-element 0))
                                                    (w 0.0))
                                                (if (= nb-s 1)
                                                    (setf (aref weights 0) 255)
                                                    (progn
                                                      (dotimes (k nb-s) (setf (aref weights k) (%m3d-u8 buf (+ data k))))
                                                      (incf data nb-s)))
                                                (dotimes (j nb-s)
                                                  (when (/= (aref weights j) 0)
                                                    (if (>= j +m3d-numbone+)
                                                        (incf data bi-s)
                                                        (let ((boneid (aref (m3ds-boneid skin) j)))
                                                          (setf (aref (m3ds-weight skin) j) (/ (float (aref weights j) 1.0) 255.0))
                                                          (setf w (+ w (aref (m3ds-weight skin) j)))
                                                          (getidx boneid bi-s)
                                                          (setf (aref (m3ds-boneid skin) j) boneid)))))
                                                ;; this can occur if model has more bones than what the importer is configured to handle
                                                (when (and (/= w 1.0) (/= w 0.0))
                                                  (dotimes (j +m3d-numbone+)
                                                    (setf (aref (m3ds-weight skin) j) (/ (aref (m3ds-weight skin) j) w))))
                                                (setf (svref skins i) skin)
                                                (incf i)))
                                     (when (/= i numskin)
                                       ;; Truncated skin in bone chunk
                                       (setf numskin i (m3d-errcode model) +m3d-err-bone+))
                                     (setf (m3d-skin model) (subseq skins 0 numskin))))
                                 (setf (m3d-numskin model) numskin)))))
                           ;; material
                           ((%m3d-magic-p buf data "MTRL")
                            (incf data 8)
                            (let ((name nil))
                              (getstr name)
                              (when (and (< ci-s 4) (zerop (m3d-numcmap model))) (setf (m3d-errcode model) +m3d-err-cmap+))
                              (when (find name (m3d-material model) :key #'m3dm-name :test #'equal)
                                ;; Multiple definitions for material
                                (setf (m3d-errcode model) +m3d-err-mtrl+
                                      name nil))
                              (when name
                                (let ((m (make-m3d-material :name name)))
                                  (vector-push-extend m (m3d-material model))
                                  (loop while (< data chunk)
                                        do (let* ((type (%m3d-u8 buf data))
                                                  (prop (make-m3d-property :type type :value 0))
                                                  (format (if (>= type 128) :map (cdr (assoc type +m3d-propertytypes+)))))
                                             (incf data)
                                             (vector-push-extend prop (m3dm-props m))
                                             (case format
                                               (:color (setf (values (m3dp-value prop) data) (read-color data)))
                                               (:uint8 (setf (m3dp-value prop) (%m3d-u8 buf data)) (incf data 1))
                                               (:uint16 (setf (m3dp-value prop) (%m3d-u16 buf data)) (incf data 2))
                                               (:uint32 (setf (m3dp-value prop) (%m3d-u32 buf data)) (incf data 4))
                                               (:float (setf (m3dp-value prop) (%m3d-f32 buf data)) (incf data 4))
                                               (:map
                                                (let ((tname nil))
                                                  (getstr tname)
                                                  (setf (m3dp-value prop) (%m3d-gettx model readfilecb tname))
                                                  ;; this error code only returned if readfilecb was specified
                                                  (when (= (m3dp-value prop) +m3d-undef+)
                                                    ;; Texture not found
                                                    (vector-pop (m3dm-props m)))))
                                               (t
                                                ;; Unknown material property
                                                (setf (m3d-errcode model) +m3d-err-unkprop+
                                                      data chunk)))))))))
                           ;; procedural surface (interpreter not implemented)
                           ((%m3d-magic-p buf data "PROC")
                            (setf (m3d-errcode model) +m3d-err-unimpl+))
                           ;; mesh
                           ((%m3d-magic-p buf data "MESH")
                            (when (zerop (m3d-numvertex model))
                              ;; No vertex chunk before mesh
                              (setf (m3d-errcode model) +m3d-err-vrts+))
                            (incf data 8)
                            (let ((mi +m3d-undef+))
                              (loop while (< data chunk)
                                    do (let* ((k (%m3d-u8 buf data))
                                              (n (ash k -4)))
                                         (incf data)
                                         (setf k (logand k 15))
                                         (cond
                                           ((zerop n)
                                            (let ((name nil))
                                              (getstr name)
                                              (when (zerop k)
                                                ;; use material
                                                (setf mi +m3d-undef+)
                                                (when name
                                                  (setf mi (or (position name (m3d-material model) :key #'m3dm-name :test #'equal)
                                                               +m3d-undef+))
                                                  (when (= mi +m3d-undef+) (setf (m3d-errcode model) +m3d-err-mtrl+))))))
                                           ((/= n 3)
                                            ;; Only triangle mesh supported for now
                                            (setf (m3d-errcode model) +m3d-err-unkmesh+)
                                            (return-from m3d-load model))
                                           (t
                                            ;; set all index to -1 by default
                                            (let ((face (make-m3d-face :materialid mi))
                                                  (j 0))
                                              (vector-push-extend face (m3d-face model))
                                              (loop while (and (< data chunk) (< j n))
                                                    do (let ((index 0))
                                                         ;; vertex
                                                         (setf index (aref (m3df-vertex face) j))
                                                         (getidx index vi-s)
                                                         (setf (aref (m3df-vertex face) j) index)
                                                         ;; texcoord
                                                         (when (logtest k 1)
                                                           (setf index (aref (m3df-texcoord face) j))
                                                           (getidx index (m3d-ti-s model))
                                                           (setf (aref (m3df-texcoord face) j) index))
                                                         ;; normal
                                                         (when (logtest k 2)
                                                           (setf index (aref (m3df-normal face) j))
                                                           (getidx index vi-s)
                                                           (setf (aref (m3df-normal face) j) index))
                                                         (when (= (aref (m3df-normal face) j) +m3d-undef+) (setf neednorm t))
                                                         ;; maximum
                                                         (when (logtest k 4) (incf data vi-s))
                                                         (incf j)))
                                              (when (/= j n)
                                                ;; Invalid mesh
                                                (setf (fill-pointer (m3d-face model)) 0
                                                      (m3d-errcode model) +m3d-err-unkmesh+)
                                                (return-from m3d-load model)))))))))
                           ;; voxel types
                           ((%m3d-magic-p buf data "VOXT")
                            (if (plusp (length (m3d-voxtype model)))
                                (setf (m3d-errcode model) +m3d-err-voxt+)
                                (progn
                                  (when (and (/= ci-s 0) (< ci-s 4) (null (m3d-cmap model))) (setf (m3d-errcode model) +m3d-err-cmap+))
                                  (let* ((reclen (+ ci-s si-s 3 sk-s))
                                         (k (floor len reclen))
                                         (voxtypes (make-array k))
                                         (i 0))
                                    (incf data 8)
                                    (loop while (and (< data chunk) (< i k))
                                          do (let ((vt (make-m3d-voxel-type)) (name nil) (skinid +m3d-undef+))
                                               (setf (values (m3dvt-color vt) data) (read-color data))
                                               (getstr name)
                                               ;; NOTE: the voxel type material lookup is commented out in m3d.h
                                               (incf data 2) ; rotation, voxshape
                                               (let ((numitem (%m3d-u8 buf data)))
                                                 (incf data)
                                                 (getidx skinid sk-s)
                                                 (setf (m3dvt-skinid vt) skinid)
                                                 (dotimes (j numitem)
                                                   (let ((item-name nil))
                                                     (incf data 2) ; count
                                                     (getstr item-name))))
                                               (setf (svref voxtypes i) vt)
                                               (incf i)))
                                    (setf (m3d-voxtype model) (subseq voxtypes 0 i))))))
                           ;; voxel data
                           ((%m3d-magic-p buf data "VOXD")
                            (incf data 8)
                            (let ((name nil))
                              (getstr name)
                              (if (or (> (m3d-vd-s model) 4) (> (m3d-vp-s model) 2))
                                  ;; No voxel index size
                                  (setf (m3d-errcode model) +m3d-err-unkvox+)
                                  (let ((voxel (make-m3d-voxel)))
                                    (when (zerop (length (m3d-voxtype model)))
                                      ;; No voxel type chunk before voxel data
                                      (setf (m3d-errcode model) +m3d-err-voxt+))
                                    (vector-push-extend voxel (m3d-voxel model))
                                    (case (m3d-vd-s model)
                                      (1 (setf (m3dvx-x voxel) (%m3d-s8 buf data) (m3dvx-y voxel) (%m3d-s8 buf (+ data 1))
                                               (m3dvx-z voxel) (%m3d-s8 buf (+ data 2)) (m3dvx-w voxel) (%m3d-u8 buf (+ data 3))
                                               (m3dvx-h voxel) (%m3d-u8 buf (+ data 4)) (m3dvx-d voxel) (%m3d-u8 buf (+ data 5)))
                                       (incf data 6))
                                      (2 (setf (m3dvx-x voxel) (%m3d-s16 buf data) (m3dvx-y voxel) (%m3d-s16 buf (+ data 2))
                                               (m3dvx-z voxel) (%m3d-s16 buf (+ data 4)) (m3dvx-w voxel) (%m3d-u16 buf (+ data 6))
                                               (m3dvx-h voxel) (%m3d-u16 buf (+ data 8)) (m3dvx-d voxel) (%m3d-u16 buf (+ data 10)))
                                       (incf data 12))
                                      (4 (setf (m3dvx-x voxel) (%m3d-s32 buf data) (m3dvx-y voxel) (%m3d-s32 buf (+ data 4))
                                               (m3dvx-z voxel) (%m3d-s32 buf (+ data 8)) (m3dvx-w voxel) (%m3d-u32 buf (+ data 12))
                                               (m3dvx-h voxel) (%m3d-u32 buf (+ data 16)) (m3dvx-d voxel) (%m3d-u32 buf (+ data 20)))
                                       (incf data 24)))
                                    (incf data 2) ; uncertain, groupid
                                    (let* ((k (logand (* (m3dvx-w voxel) (m3dvx-h voxel) (m3dvx-d voxel)) #xffffffff))
                                           (vdata (make-array k :element-type '(unsigned-byte 16) :initial-element #xffff))
                                           (j 0)
                                           (mi 0))
                                      (setf (m3dvx-data voxel) vdata)
                                      (loop while (and (< data chunk) (< j k))
                                            do (let ((l (1+ (logand (%m3d-u8 buf data) #x7f)))
                                                     (rle (logtest (%m3d-u8 buf data) #x80)))
                                                 (incf data)
                                                 (if rle
                                                     (progn
                                                       (getidx mi (m3d-vp-s model))
                                                       (loop while (and (> l 0) (< j k))
                                                             do (setf (aref vdata j) (logand mi #xffff)) (incf j) (decf l)))
                                                     (loop while (and (> l 0) (< j k))
                                                           do (getidx mi (m3d-vp-s model))
                                                              (setf (aref vdata j) (logand mi #xffff)) (incf j) (decf l))))))))))
                           ;; action
                           ((%m3d-magic-p buf data "ACTN")
                            (incf data 8)
                            (let ((action (make-m3d-action)) (name nil))
                              (getstr name)
                              (setf (m3da-name action) name
                                    (m3da-numframe action) (%m3d-u16 buf data))
                              (incf data 2)
                              (when (>= (m3da-numframe action) 1)
                                (vector-push-extend action (m3d-action model))
                                (setf (m3da-durationmsec action) (%m3d-u32 buf data))
                                (incf data 4)
                                (let ((frames (make-array (m3da-numframe action))))
                                  (dotimes (i (length frames)) (setf (svref frames i) (make-m3d-frame)))
                                  (setf (m3da-frames action) frames)
                                  (loop for i from 0
                                        while (and (< data chunk) (< i (length frames)))
                                        do (let ((frame (svref frames i)) (numtransform 0))
                                             (setf (m3dfr-msec frame) (%m3d-u32 buf data))
                                             (incf data 4)
                                             (getidx numtransform (m3d-fc-s model))
                                             (let ((transforms (make-array numtransform)))
                                               (dotimes (j numtransform)
                                                 (let ((boneid 0) (tpos 0) (ori 0))
                                                   (getidx boneid bi-s)
                                                   (getidx tpos vi-s)
                                                   (getidx ori vi-s)
                                                   (setf (svref transforms j) (list boneid tpos ori))))
                                               (setf (m3dfr-transforms frame) transforms))))))))
                           ;; NOTE: SHPE (shapes), LBLS (labels) and unknown chunks are not used by raylib
                           (t nil))))))))

      ;; calculate normals, normalize skin weights, create bone/vertex cross-references and calculate transform matrices
      (%m3d-post-process model neednorm))
    model))

(defun %m3d-add-vertex (model)
  (let ((v (make-m3d-vertex)))
    (vector-push-extend v (m3d-vertex model))
    (incf (m3d-numvertex model))
    v))

(defun %m3d-voxels-to-mesh (model)
  "Converting voxels into vertices and mesh"
  (let* ((vertices (m3d-vertex model))
         (voxtypes (m3d-voxtype model))
         (numvoxtype (length voxtypes))
         (enorm (m3d-numvertex model)))
    ;; add normals
    (dotimes (l 6) (setf (m3dv-skinid (%m3d-add-vertex model)) +m3d-undef+))
    (setf (m3dv-y (aref vertices (+ enorm 0))) -1.0
          (m3dv-z (aref vertices (+ enorm 1))) -1.0
          (m3dv-x (aref vertices (+ enorm 2))) -1.0
          (m3dv-y (aref vertices (+ enorm 3))) 1.0
          (m3dv-z (aref vertices (+ enorm 4))) 1.0
          (m3dv-x (aref vertices (+ enorm 5))) 1.0)
    ;; this is a fast, not so memory efficient version, only basic face culling used
    (let ((min-x 2147483647) (min-y 2147483647) (min-z 2147483647)
          (max-x -2147483647) (max-y -2147483647) (max-z -2147483647))
      (loop for voxel across (m3d-voxel model)
            do (setf max-x (max max-x (%i32 (+ (m3dvx-x voxel) (m3dvx-w voxel))))
                     min-x (min min-x (m3dvx-x voxel))
                     max-y (max max-y (%i32 (+ (m3dvx-y voxel) (m3dvx-h voxel))))
                     min-y (min min-y (m3dvx-y voxel))
                     max-z (max max-z (%i32 (+ (m3dvx-z voxel) (m3dvx-d voxel))))
                     min-z (min min-z (m3dvx-z voxel))))
      (let* ((i (logand (if (> (- min-x) max-x) (- min-x) max-x) #xffffffff))
             (j (logand (if (> (- min-y) max-y) (- min-y) max-y) #xffffffff))
             (k (logand (if (> (- min-z) max-z) (- min-z) max-z) #xffffffff)))
        (when (> j i) (setf i j))
        (when (> k i) (setf i k))
        (when (<= i 1) (setf i 1))
        (let ((w (/ 1.0 (float i 1.0))))
          (when (>= i 254) (setf (m3d-vc-s model) 2))
          (when (>= i 65534) (setf (m3d-vc-s model) 4))
          (loop for voxel across (m3d-voxel model)
                do (let ((sx (m3dvx-w voxel)) (sz (m3dvx-d voxel)) (sy (m3dvx-h voxel))
                         (vdata (m3dvx-data voxel))
                         (j 0))
                     (flet ((empty-p (index) (>= (aref vdata index) numvoxtype)))
                       (dotimes (y sy)
                         (dotimes (z sz)
                           (dotimes (x sx)
                             (when (< (aref vdata j) numvoxtype)
                               (let ((k 0) (n 0) (am 0))
                                 (when (or (zerop y) (empty-p (- j (* sx sz)))) (incf n) (setf am (logior am 1)) (setf k (logior k 1 2 4 8)))
                                 (when (or (zerop z) (empty-p (- j sx))) (incf n) (setf am (logior am 2)) (setf k (logior k 1 2 16 32)))
                                 (when (or (zerop x) (empty-p (- j 1))) (incf n) (setf am (logior am 4)) (setf k (logior k 1 4 16 64)))
                                 (when (or (= y (1- sy)) (empty-p (+ j (* sx sz)))) (incf n) (setf am (logior am 8)) (setf k (logior k 16 32 64 128)))
                                 (when (or (= z (1- sz)) (empty-p (+ j sx))) (incf n) (setf am (logior am 16)) (setf k (logior k 4 8 64 128)))
                                 (when (or (= x (1- sx)) (empty-p (+ j 1))) (incf n) (setf am (logior am 32)) (setf k (logior k 2 8 32 128)))
                                 (when (/= k 0)
                                   (let ((edge (make-array 8 :initial-element +m3d-undef+))
                                         (vt (aref voxtypes (aref vdata j))))
                                     (dotimes (l 8)
                                       (when (logbitp l k)
                                         (setf (aref edge l) (m3d-numvertex model))
                                         (let ((v (%m3d-add-vertex model))
                                               (dx (if (logbitp 0 l) 1 0)) ; corners: bit0 x+1, bit1 z+1, bit2 y+1
                                               (dz (if (logbitp 1 l) 1 0))
                                               (dy (if (logbitp 2 l) 1 0)))
                                           (setf (m3dv-skinid v) (m3dvt-skinid vt)
                                                 (m3dv-color v) (m3dvt-color vt)
                                                 (m3dv-x v) (* (float (%i32 (+ (m3dvx-x voxel) x dx)) 1.0) w)
                                                 (m3dv-y v) (* (float (%i32 (+ (m3dvx-y voxel) y dy)) 1.0) w)
                                                 (m3dv-z v) (* (float (%i32 (+ (m3dvx-z voxel) z dz)) 1.0) w)))))
                                     (flet ((quad (normal a0 a1 a2 b0 b1 b2)
                                              (dolist (tri (list (list a0 a1 a2) (list b0 b1 b2)))
                                                (let ((face (make-m3d-face :materialid (m3dvt-materialid vt))))
                                                  (loop for e in tri for c from 0
                                                        do (setf (aref (m3df-vertex face) c) (aref edge e)
                                                                 (aref (m3df-normal face) c) (+ enorm normal)))
                                                  (vector-push-extend face (m3d-face model))))))
                                       (when (logtest am 1) (quad 0 0 1 2 2 1 3))      ; bottom
                                       (when (logtest am 2) (quad 1 0 4 1 1 4 5))      ; north
                                       (when (logtest am 4) (quad 2 0 2 4 2 6 4))      ; west
                                       (when (logtest am 8) (quad 3 4 6 5 5 6 7))      ; top
                                       (when (logtest am 16) (quad 4 2 7 6 7 2 3))     ; south
                                       (when (logtest am 32) (quad 5 1 5 7 1 7 3)))))))   ; east
                             (incf j))))))))))))

(defun %m3d-post-process (model neednorm)
  (when (plusp (length (m3d-voxel model)))
    (%m3d-voxels-to-mesh model))
  (let ((faces (m3d-face model))
        (vertices (m3d-vertex model)))
    (when (and (plusp (fill-pointer faces)) neednorm)
      ;; if they are missing, calculate triangle normals into a temporary buffer
      (let* ((numface (fill-pointer faces))
             (n (m3d-numvertex model))
             (norm (make-array numface :initial-element nil)))
        (float-features:with-float-traps-masked t
          (dotimes (i numface)
            (let ((face (aref faces i)))
              (when (= (aref (m3df-normal face) 0) +m3d-undef+)
                (let* ((v0 (aref vertices (aref (m3df-vertex face) 0)))
                       (v1 (aref vertices (aref (m3df-vertex face) 1)))
                       (v2 (aref vertices (aref (m3df-vertex face) 2)))
                       (vax (- (m3dv-x v1) (m3dv-x v0))) (vay (- (m3dv-y v1) (m3dv-y v0))) (vaz (- (m3dv-z v1) (m3dv-z v0)))
                       (vbx (- (m3dv-x v2) (m3dv-x v0))) (vby (- (m3dv-y v2) (m3dv-y v0))) (vbz (- (m3dv-z v2) (m3dv-z v0)))
                       (nx (- (* vay vbz) (* vaz vby)))
                       (ny (- (* vaz vbx) (* vax vbz)))
                       (nz (- (* vax vby) (* vay vbx)))
                       (w (%m3d-rsq (+ (+ (* nx nx) (* ny ny)) (* nz nz)))))
                  (setf (aref norm i) (list (* nx w) (* ny w) (* nz w)))
                  (dotimes (c 3)
                    (setf (aref (m3df-normal face) c) (logand (+ (aref (m3df-vertex face) c) n) #xffffffff)))))))
          ;; this is the fast way, we don't care if a normal is repeated in model->vertex
          (dotimes (i n) (%m3d-add-vertex model))
          (dotimes (i numface)
            (when (aref norm i)
              (destructuring-bind (nx ny nz) (aref norm i)
                (dotimes (j 3)
                  (let ((v0 (aref vertices (+ (aref (m3df-vertex (aref faces i)) j) n))))
                    (setf (m3dv-x v0) (+ (m3dv-x v0) nx)
                          (m3dv-y v0) (+ (m3dv-y v0) ny)
                          (m3dv-z v0) (+ (m3dv-z v0) nz)))))))
          ;; for each vertex, take the average of the temporary normals and use that
          (dotimes (i n)
            (let* ((v0 (aref vertices (+ n i)))
                   (w (%m3d-rsq (+ (+ (* (m3dv-x v0) (m3dv-x v0)) (* (m3dv-y v0) (m3dv-y v0))) (* (m3dv-z v0) (m3dv-z v0))))))
              (setf (m3dv-x v0) (* (m3dv-x v0) w)
                    (m3dv-y v0) (* (m3dv-y v0) w)
                    (m3dv-z v0) (* (m3dv-z v0) w)
                    (m3dv-skinid v0) +m3d-undef+))))))

    (when (and (plusp (m3d-numbone model)) (plusp (m3d-numskin model)) (plusp (m3d-numvertex model)))
      ;; Generating weight cross-reference: skin weights are normalized for every vertex using them
      ;; NOTE: Bone weight lists and transformation matrices are not used by raylib
      (float-features:with-float-traps-masked t
        (dotimes (i (m3d-numvertex model))
          (let ((skinid (m3dv-skinid (aref vertices i))))
            (when (< skinid (m3d-numskin model))
              (let* ((sk (svref (m3d-skin model) skinid))
                     (boneid (m3ds-boneid sk))
                     (weight (m3ds-weight sk))
                     (count (loop for j below +m3d-numbone+
                                  while (and (/= (aref boneid j) +m3d-undef+) (> (aref weight j) 0.0))
                                  count t))
                     (w 0.0))
                (dotimes (j count) (setf w (+ w (aref weight j))))
                (dotimes (j count) (setf (aref weight j) (/ (aref weight j) w)))))))))))

;;;----------------------------------------------------------------------------------
;;; Animation
;;;----------------------------------------------------------------------------------

(defun m3d-pose (model actionid msec)
  "Returns interpolated animation-pose: vector of bones (copies) with pos/ori vertex indices
NOTE: Interpolated vertices are stored past numvertex in the model vertex array"
  (when (or (zerop (m3d-numbone model)) (zerop (length (m3d-bone model))))
    (setf (m3d-errcode model) +m3d-err-unkframe+)
    (return-from m3d-pose nil))
  (let ((ret (map 'vector #'copy-m3d-bone (m3d-bone model)))
        (numbone (m3d-numbone model)))
    (when (>= actionid (m3d-numaction model))
      (setf (m3d-errcode model) +m3d-err-unkframe+)
      (return-from m3d-pose ret))
    (let* ((action (aref (m3d-action model) actionid))
           (frames (m3da-frames action))
           (numframe (m3da-numframe action))
           (msec (mod msec (m3da-durationmsec action)))
           (j 0) (l 0))
      (setf (m3d-errcode model) +m3d-success+)
      (flet ((apply-frame (fr bones)
               (loop for (boneid pos ori) across (m3dfr-transforms fr)
                     do (setf (m3db-pos (aref bones boneid)) pos
                              (m3db-ori (aref bones boneid)) ori))))
        (loop while (and (< j numframe) (<= (m3dfr-msec (svref frames j)) msec))
              do (let ((fr (svref frames j)))
                   (setf l (m3dfr-msec fr))
                   (apply-frame fr ret))
                 (incf j))
        (when (/= l msec)
          ;; Room for the interpolated vertices
          (let ((vertices (m3d-vertex model))
                (numvertex (m3d-numvertex model)))
            (loop while (< (fill-pointer vertices) (+ numvertex (* 2 numbone)))
                  do (vector-push-extend (make-m3d-vertex) vertices))
            (let ((tmp (map 'vector #'copy-m3d-bone ret))
                  (fr (svref frames (mod j numframe)))
                  (tt 0.0))
              (setf tt (if (>= l (m3dfr-msec fr))
                           1.0
                           (/ (float (logand (- msec l) #xffffffff) 1.0) (float (logand (- (m3dfr-msec fr) l) #xffffffff) 1.0))))
              (apply-frame fr tmp)
              (float-features:with-float-traps-masked t
                (let ((j numvertex))
                  (dotimes (i numbone)
                    (let ((r (aref ret i)) (tm (aref tmp i)))
                      ;; interpolation of position
                      (when (/= (m3db-pos r) (m3db-pos tm))
                        (let ((p (aref vertices (m3db-pos r)))
                              (f (aref vertices (m3db-pos tm)))
                              (v (aref vertices j)))
                          (setf (m3dv-x v) (+ (m3dv-x p) (* tt (- (m3dv-x f) (m3dv-x p))))
                                (m3dv-y v) (+ (m3dv-y p) (* tt (- (m3dv-y f) (m3dv-y p))))
                                (m3dv-z v) (+ (m3dv-z p) (* tt (- (m3dv-z f) (m3dv-z p)))))
                          (setf (m3db-pos r) j)
                          (incf j)))
                      ;; interpolation of orientation
                      (when (/= (m3db-ori r) (m3db-ori tm))
                        (let* ((p (aref vertices (m3db-ori r)))
                               (f (aref vertices (m3db-ori tm)))
                               (v (aref vertices j))
                               (d (+ (+ (+ (* (m3dv-w p) (m3dv-w f)) (* (m3dv-x p) (m3dv-x f))) (* (m3dv-y p) (m3dv-y f))) (* (m3dv-z p) (m3dv-z f))))
                               (s 1.0))
                          (if (< d 0) (setf d (- d) s -1.0) (setf s 1.0))
                          ;; approximated NLERP, original approximation by Arseny Kapoulkine, heavily optimized by me
                          ;; NOTE: t is updated in place, it carries over to the next bones like in m3d.h
                          (let ((c (- tt 0.5)))
                            (setf tt (+ tt (* (* (* tt c) (- tt 1.0))
                                              (+ (* (* (+ 1.0904 (* d (+ -3.2452 (* d (- 3.55645 (* d 1.43519)))))) c) c)
                                                 (+ 0.848013 (* d (+ -1.06021 (* d 0.215638)))))))))
                          (setf (m3dv-x v) (+ (m3dv-x p) (* tt (- (* s (m3dv-x f)) (m3dv-x p))))
                                (m3dv-y v) (+ (m3dv-y p) (* tt (- (* s (m3dv-y f)) (m3dv-y p))))
                                (m3dv-z v) (+ (m3dv-z p) (* tt (- (* s (m3dv-z f)) (m3dv-z p))))
                                (m3dv-w v) (+ (m3dv-w p) (* tt (- (* s (m3dv-w f)) (m3dv-w p)))))
                          (setf d (%m3d-rsq (+ (+ (+ (* (m3dv-w v) (m3dv-w v)) (* (m3dv-x v) (m3dv-x v))) (* (m3dv-y v) (m3dv-y v)))
                                               (* (m3dv-z v) (m3dv-z v)))))
                          (setf (m3dv-x v) (* (m3dv-x v) d)
                                (m3dv-y v) (* (m3dv-y v) d)
                                (m3dv-z v) (* (m3dv-z v) d)
                                (m3dv-w v) (* (m3dv-w v) d))
                          (setf (m3db-ori r) j)
                          (incf j))))))))))))
    ;; NOTE: Bone transformation matrices (mat4) are not computed, raylib only uses pos/ori
    ret))

(defun m3d-free (model)
  "Free the in-memory model (memory is managed by the garbage collector)"
  (declare (ignore model))
  nil)
