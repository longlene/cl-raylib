(in-package #:cl-raylib)

;;;===================================================================================
;;; tinyobj_loader_c - Wavefront .obj/.mtl loader
;;; Port of raylib/src/external/tinyobj_loader_c.h (used by rmodels.c)
;;;
;;; NOTE: Text is processed as bytes (octet vectors) like C, names are decoded as UTF-8
;;; NOTE: Inputs with undefined behavior in C (lines longer than 4095 bytes, faces with
;;; more than 16 indices) are ignored
;;;===================================================================================

(defconstant +tinyobj-flag-triangulate+ 1)
(defconstant +tinyobj-invalid-index+ -2147483648 "(int)0x80000000")
(defconstant +tinyobj-max-faces-per-f-line+ 16)

(defstruct (tinyobj-material (:conc-name tobjm-))
  (name nil)
  (ambient (make-array 3 :element-type 'single-float :initial-element 0.0))
  (diffuse (make-array 3 :element-type 'single-float :initial-element 0.0))
  (specular (make-array 3 :element-type 'single-float :initial-element 0.0))
  (transmittance (make-array 3 :element-type 'single-float :initial-element 0.0))
  (emission (make-array 3 :element-type 'single-float :initial-element 0.0))
  (shininess 1.0)
  (ior 1.0)                             ; index of refraction
  (dissolve 1.0)                        ; 1 == opaque; 0 == fully transparent
  (illum 0)                             ; illumination model
  (ambient-texname nil)                 ; map_Ka
  (diffuse-texname nil)                 ; map_Kd
  (specular-texname nil)                ; map_Ks
  (specular-highlight-texname nil)      ; map_Ns
  (bump-texname nil)                    ; map_bump, bump
  (displacement-texname nil)            ; disp
  (alpha-texname nil))                  ; map_d

(defstruct (tinyobj-shape (:conc-name tobjs-))
  (name nil)                            ; group name or object name
  (face-offset 0)
  (length 0))

(defstruct (tinyobj-attrib (:conc-name tobja-))
  (num-vertices 0)
  (num-normals 0)
  (num-texcoords 0)
  (num-faces 0)
  (num-face-num-verts 0)
  (vertices nil)                        ; single-float vector
  (normals nil)
  (texcoords nil)
  (faces nil)                           ; vector of (v-idx vt-idx vn-idx)
  (face-num-verts nil)
  (material-ids nil))

;;;----------------------------------------------------------------------------------
;;; Byte string helpers (BUF is an octet vector, positions are indices, 0 past the end)
;;;----------------------------------------------------------------------------------
(declaim (inline %tob))
(defun %tob (buf i)
  (if (< i (length buf)) (aref buf i) 0))

(defmacro %tob-is-space (x) `(let ((c ,x)) (or (= c 32) (= c 9))))
(defmacro %tob-is-digit (x) `(let ((c ,x)) (<= 48 c 57)))
(defmacro %tob-is-new-line (x) `(let ((c ,x)) (or (= c 13) (= c 10) (= c 0))))

(defun %tob-string (buf start end)
  "Bytes [START, END) as a UTF-8 string, stopping at the first NUL"
  (let* ((end (min end (length buf)))
         (nul (position 0 buf :start start :end end)))
    (babel:octets-to-string buf :start start :end (or nul end) :encoding :utf-8 :errorp nil)))

(defun %tob-skip-space (buf p)
  (loop while (%tob-is-space (%tob buf p)) do (incf p))
  p)

(defun %tob-skip-space-and-cr (buf p)
  (loop while (let ((c (%tob buf p))) (or (= c 32) (= c 9) (= c 13))) do (incf p))
  p)

(defun %tob-until-space (buf p)
  (let ((q p))
    (loop while (let ((c (%tob buf q))) (and (/= c 0) (/= c 32) (/= c 9) (/= c 13))) do (incf q))
    (- q p)))

(defun %tob-length-until-newline (buf p n)
  ;; Assume token[n-1] = '\0'
  (let ((len 0))
    (loop while (< len (- n 1))
          do (let ((c (%tob buf (+ p len))))
               (when (= c 10) (return))
               (when (and (= c 13) (< len (- n 2)) (/= (%tob buf (+ p len 1)) 10)) (return)))
             (incf len))
    len))

(defun %tob-length-until-line-feed (buf p n)
  (let ((len 0))
    (loop while (< len n)
          do (let ((c (%tob buf (+ p len))))
               (when (or (= c 10) (= c 13)) (return)))
             (incf len))
    len))

(defun %tob-atoi (buf p)
  (let ((value 0) (sign 1))
    (when (or (= (%tob buf p) 43) (= (%tob buf p) 45))
      (when (= (%tob buf p) 45) (setf sign -1))
      (incf p))
    (loop while (%tob-is-digit (%tob buf p))
          do (setf value (%i32 (+ (* value 10) (- (%tob buf p) 48))))
             (incf p))
    (%i32 (* value sign))))

;; Make index zero-base, and also support relative index.
(defun %tob-fix-index (idx n)
  (cond ((> idx 0) (- idx 1))
        ((= idx 0) 0)
        (t (%i32 (+ n idx)))))          ; negative value = relative

;; Parse raw triples: i, i/j/k, i//k, i/j
;; Returns (values (v-idx vt-idx vn-idx) new-position)
(defun %tob-parse-raw-triple (buf p)
  (let ((v-idx +tinyobj-invalid-index+)
        (vn-idx +tinyobj-invalid-index+)
        (vt-idx +tinyobj-invalid-index+))
    (flet ((skip-index ()
             (loop while (let ((c (%tob buf p))) (and (/= c 0) (/= c 47) (/= c 32) (/= c 9) (/= c 13)))
                   do (incf p))))
      (setf v-idx (%tob-atoi buf p))
      (skip-index)
      (when (/= (%tob buf p) 47)
        (return-from %tob-parse-raw-triple (values (list v-idx vt-idx vn-idx) p)))
      (incf p)
      ;; i//k
      (when (= (%tob buf p) 47)
        (incf p)
        (setf vn-idx (%tob-atoi buf p))
        (skip-index)
        (return-from %tob-parse-raw-triple (values (list v-idx vt-idx vn-idx) p)))
      ;; i/j/k or i/j
      (setf vt-idx (%tob-atoi buf p))
      (skip-index)
      (when (/= (%tob buf p) 47)
        (return-from %tob-parse-raw-triple (values (list v-idx vt-idx vn-idx) p)))
      ;; i/j/k
      (incf p)                          ; skip '/'
      (setf vn-idx (%tob-atoi buf p))
      (skip-index)
      (values (list v-idx vt-idx vn-idx) p))))

(defun %tob-parse-int (buf p)
  (setf p (%tob-skip-space buf p))
  (let ((i (%tob-atoi buf p)))
    (values i (+ p (%tob-until-space buf p)))))

;; Tries to parse a floating point number located at s (see tinyobj_loader_c.h)
;; Returns the double result or NIL on failure
(defun %tob-try-parse-double (buf s s-end)
  (let ((mantissa 0d0)
        (exponent 0)
        (sign 43) (exp-sign 43)
        (curr s)
        (read 0)
        (end-not-reached nil))
    (block parse
      (when (>= s s-end) (return-from %tob-try-parse-double nil)) ; fail
      ;; Find out what sign we've got.
      (cond ((or (= (%tob buf curr) 43) (= (%tob buf curr) 45))
             (setf sign (%tob buf curr))
             (incf curr))
            ((%tob-is-digit (%tob buf curr)))  ; Pass through.
            (t (return-from %tob-try-parse-double nil)))
      ;; Read the integer part.
      (setf end-not-reached (/= curr s-end))
      (loop while (and end-not-reached (%tob-is-digit (%tob buf curr)))
            do (setf mantissa (* mantissa 10))
               (setf mantissa (+ mantissa (- (%tob buf curr) 48)))
               (incf curr)
               (incf read)
               (setf end-not-reached (/= curr s-end)))
      ;; We must make sure we actually got something.
      (when (= read 0) (return-from %tob-try-parse-double nil))
      ;; We allow numbers of form "#", "###" etc.
      (unless end-not-reached (return-from parse))
      ;; Read the decimal part.
      (cond ((= (%tob buf curr) 46)
             (incf curr)
             (setf read 1)
             (setf end-not-reached (/= curr s-end))
             (loop while (and end-not-reached (%tob-is-digit (%tob buf curr)))
                   do (let ((frac-value 1d0))
                        ;; pow(10.0, -read)
                        (dotimes (f read) (setf frac-value (* frac-value 0.1d0)))
                        (setf mantissa (+ mantissa (* (- (%tob buf curr) 48) frac-value))))
                      (incf read)
                      (incf curr)
                      (setf end-not-reached (/= curr s-end))))
            ((or (= (%tob buf curr) 101) (= (%tob buf curr) 69)))
            (t (return-from parse)))
      (unless end-not-reached (return-from parse))
      ;; Read the exponent part.
      (when (or (= (%tob buf curr) 101) (= (%tob buf curr) 69))
        (incf curr)
        ;; Figure out if a sign is present and if it is.
        (setf end-not-reached (/= curr s-end))
        (cond ((and end-not-reached (or (= (%tob buf curr) 43) (= (%tob buf curr) 45)))
               (setf exp-sign (%tob buf curr))
               (incf curr))
              ((%tob-is-digit (%tob buf curr)))  ; Pass through.
              (t (return-from %tob-try-parse-double nil))) ; Empty E is not allowed.
        (setf read 0)
        (setf end-not-reached (/= curr s-end))
        (loop while (and end-not-reached (%tob-is-digit (%tob buf curr)))
              do (setf exponent (%i32 (+ (* exponent 10) (- (%tob buf curr) 48))))
                 (incf curr)
                 (incf read)
                 (setf end-not-reached (/= curr s-end)))
        (when (= read 0) (return-from %tob-try-parse-double nil))))
    ;; assemble
    (float-features:with-float-traps-masked t
      (let ((a 1d0)                     ; = pow(5.0, exponent);
            (b 1d0))                    ; = 2.0^exponent
        (dotimes (i (max exponent 0)) (setf a (* a 5d0)))
        (dotimes (i (max exponent 0)) (setf b (* b 2d0)))
        (when (= exp-sign 45)
          (setf a (/ 1d0 a)
                b (/ 1d0 b)))
        (* (if (= sign 43) 1 -1) (* (* mantissa a) b))))))

(defun %tob-parse-float (buf p)
  "Returns (values float new-position)"
  (setf p (%tob-skip-space buf p))
  (let* ((end (+ p (%tob-until-space buf p)))
         (val (or (%tob-try-parse-double buf p end) 0d0)))
    (values (float-features:with-float-traps-masked t (coerce val 'single-float)) end)))

(defun %tob-parse-float3 (buf p)
  (multiple-value-bind (x p) (%tob-parse-float buf p)
    (multiple-value-bind (y p) (%tob-parse-float buf p)
      (multiple-value-bind (z p) (%tob-parse-float buf p)
        (values x y z p)))))

;;;----------------------------------------------------------------------------------
;;; Material table (string to int hashtable, keyed by the djb2 hash like C)
;;;----------------------------------------------------------------------------------
(defun %tob-hash-djb2 (name)
  (let ((hash 5381))
    (loop for c across (babel:string-to-octets name :encoding :utf-8)
          do (setf hash (logand (+ (ash hash 5) hash c) #xffffffffffffffff)))
    hash))

(defun %tob-hash-table-set (name value table)
  (setf (gethash (%tob-hash-djb2 name) table) value))

(defun %tob-hash-table-get (name table)
  (gethash (%tob-hash-djb2 name) table))

;;;----------------------------------------------------------------------------------
;;; MTL parsing
;;;----------------------------------------------------------------------------------

;; Returns (values materials-vector result) where result is :success or :error-file-operation
(defun %tinyobj-parse-and-index-mtl-file (filename material-table)
  (let ((data (handler-case (with-open-file (s filename :element-type '(unsigned-byte 8))
                              (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
                                (read-sequence v s)
                                v))
                (error () nil))))
    (unless data
      (format *error-output* "TINYOBJ: Error reading file '~a': ~a~%" filename
              (if (probe-file filename) "Permission denied (13)" "No such file or directory (2)"))
      (return-from %tinyobj-parse-and-index-mtl-file (values (vector) :error-file-operation)))
    (let ((materials (make-array 0 :adjustable t :fill-pointer 0))
          (material (make-tinyobj-material))  ; Create a default material
          (has-previous-material nil)
          (pos 0)
          (n (or (position 0 data) (length data)))) ; fgets() stops at NUL like C strings
      (loop while (< pos n)
            do (let* ((eol (position 10 data :start pos :end n))
                      (line-end (if eol (1+ eol) n)) ; line includes '\n' (fgets)
                      (buf (subseq data pos line-end))
                      (token 0))
                 (setf pos line-end)
                 (flet ((starts (str &optional (at token))
                          (and (<= (+ at (length str)) (length buf))
                               (every (lambda (ch i) (= (char-code ch) (aref buf (+ at i))))
                                      str (loop for i below (length str) collect i))))
                        (rest-of-line (start)
                          ;; my_strdup(token, line_end - token): up to '\n' or '\r'
                          (let ((len (%tob-length-until-line-feed buf start (- (length buf) start))))
                            (%tob-string buf start (+ start len))))
                        (float3 (target)
                          (multiple-value-bind (r g b) (%tob-parse-float3 buf (+ token 2))
                            (setf (aref target 0) r (aref target 1) g (aref target 2) b))))
                   ;; Skip leading space.
                   (loop while (and (< token (length buf)) (%tob-is-space (aref buf token))) do (incf token))
                   (let ((c0 (%tob buf token)) (c1 (%tob buf (+ token 1))) (c2 (%tob buf (+ token 2))))
                     (cond
                       ((= c0 0))       ; empty line
                       ((= c0 35))      ; comment line
                       ;; new mtl
                       ((and (starts "newmtl") (%tob-is-space (%tob buf (+ token 6))))
                        ;; flush previous material.
                        (if has-previous-material
                            (vector-push-extend material materials)
                            (setf has-previous-material t))
                        ;; initial temporary material
                        (setf material (make-tinyobj-material))
                        ;; set new mtl name: sscanf(token, "%s", namebuf)
                        (let* ((start (loop for i from (+ token 7) below (length buf)
                                            unless (member (aref buf i) '(32 9 10 11 12 13)) return i
                                            finally (return (length buf))))
                               (end (or (position-if (lambda (b) (member b '(32 9 10 11 12 13 0))) buf :start start)
                                        (length buf))))
                          (setf (tobjm-name material) (%tob-string buf start end)))
                        ;; Add material to material table
                        (when material-table
                          (%tob-hash-table-set (tobjm-name material) (length materials) material-table)))
                       ;; ambient
                       ((and (= c0 75) (= c1 97) (%tob-is-space c2)) (float3 (tobjm-ambient material)))
                       ;; diffuse
                       ((and (= c0 75) (= c1 100) (%tob-is-space c2)) (float3 (tobjm-diffuse material)))
                       ;; specular
                       ((and (= c0 75) (= c1 115) (%tob-is-space c2)) (float3 (tobjm-specular material)))
                       ;; transmittance
                       ((and (= c0 75) (= c1 116) (%tob-is-space c2)) (float3 (tobjm-transmittance material)))
                       ;; ior(index of refraction)
                       ((and (= c0 78) (= c1 105) (%tob-is-space c2))
                        (setf (tobjm-ior material) (%tob-parse-float buf (+ token 2))))
                       ;; emission
                       ((and (= c0 75) (= c1 101) (%tob-is-space c2)) (float3 (tobjm-emission material)))
                       ;; shininess
                       ((and (= c0 78) (= c1 115) (%tob-is-space c2))
                        (setf (tobjm-shininess material) (%tob-parse-float buf (+ token 2))))
                       ;; illum model
                       ((and (starts "illum") (%tob-is-space (%tob buf (+ token 5))))
                        (setf (tobjm-illum material) (%tob-parse-int buf (+ token 6))))
                       ;; dissolve
                       ((and (= c0 100) (%tob-is-space c1))
                        (setf (tobjm-dissolve material) (%tob-parse-float buf (+ token 1))))
                       ((and (= c0 84) (= c1 114) (%tob-is-space c2))
                        ;; Invert value of Tr(assume Tr is in range [0, 1])
                        (setf (tobjm-dissolve material) (- 1.0 (%tob-parse-float buf (+ token 2)))))
                       ;; ambient texture
                       ((and (starts "map_Ka") (%tob-is-space (%tob buf (+ token 6))))
                        (setf (tobjm-ambient-texname material) (rest-of-line (+ token 7))))
                       ;; diffuse texture
                       ((and (starts "map_Kd") (%tob-is-space (%tob buf (+ token 6))))
                        (setf (tobjm-diffuse-texname material) (rest-of-line (+ token 7))))
                       ;; specular texture
                       ((and (starts "map_Ks") (%tob-is-space (%tob buf (+ token 6))))
                        (setf (tobjm-specular-texname material) (rest-of-line (+ token 7))))
                       ;; specular highlight texture
                       ((and (starts "map_Ns") (%tob-is-space (%tob buf (+ token 6))))
                        (setf (tobjm-specular-highlight-texname material) (rest-of-line (+ token 7))))
                       ;; bump texture
                       ((and (starts "map_bump") (%tob-is-space (%tob buf (+ token 8))))
                        (setf (tobjm-bump-texname material) (rest-of-line (+ token 9))))
                       ;; alpha texture
                       ((and (starts "map_d") (%tob-is-space (%tob buf (+ token 5))))
                        (setf (tobjm-alpha-texname material) (rest-of-line (+ token 6))))
                       ;; bump texture
                       ((and (starts "bump") (%tob-is-space (%tob buf (+ token 4))))
                        (setf (tobjm-bump-texname material) (rest-of-line (+ token 5))))
                       ;; displacement texture
                       ((and (starts "disp") (%tob-is-space (%tob buf (+ token 4))))
                        (setf (tobjm-displacement-texname material) (rest-of-line (+ token 5)))))))))
      (when (tobjm-name material)
        ;; Flush last material element
        (vector-push-extend material materials))
      (values (coerce materials 'simple-vector) :success))))

(defun tinyobj-parse-mtl-file (filename)
  "Returns (values materials-vector result)"
  (%tinyobj-parse-and-index-mtl-file filename nil))

;;;----------------------------------------------------------------------------------
;;; OBJ parsing
;;;----------------------------------------------------------------------------------

(defstruct (%tob-command (:conc-name tobc-))
  (type :empty)
  vx vy vz nx ny nz tx ty
  (f nil)                               ; list of (v vt vn)
  (f-num-verts nil)
  (name-start 0) (name-len 0))          ; group/object/material/mtllib name in BUF

(defun %tob-parse-line (buf start p-len triangulate)
  "Parse line [START, START+P-LEN) of BUF, returns a command or NIL"
  (when (> p-len 4095) (return-from %tob-parse-line nil))
  ;; NOTE: C works on a NUL terminated copy of the line, positions past the line read 0
  (let* ((line (let ((v (make-array (+ p-len 1) :element-type '(unsigned-byte 8) :initial-element 0)))
                 (replace v buf :start2 start :end2 (+ start p-len))
                 v))
         (token (%tob-skip-space line 0))
         (c0 (%tob line token)) (c1 (%tob line (+ token 1))) (c2 (%tob line (+ token 2))))
    (flet ((starts (str)
             (loop for ch across str for i from token
                   always (= (char-code ch) (%tob line i))))
           (name-command (type at len-extra)
             (make-%tob-command :type type :name-start (+ start at)
                                :name-len (+ (%tob-length-until-newline line at (+ (- p-len at) len-extra))
                                             (if (eq type :usemtl) 0 1)))))
      (cond
        ((= c0 0) nil)                  ; empty line
        ((= c0 35) nil)                 ; comment line
        ;; vertex
        ((and (= c0 118) (%tob-is-space c1))
         (multiple-value-bind (x y z) (%tob-parse-float3 line (+ token 2))
           (make-%tob-command :type :v :vx x :vy y :vz z)))
        ;; normal
        ((and (= c0 118) (= c1 110) (%tob-is-space c2))
         (multiple-value-bind (x y z) (%tob-parse-float3 line (+ token 3))
           (make-%tob-command :type :vn :nx x :ny y :nz z)))
        ;; texcoord
        ((and (= c0 118) (= c1 116) (%tob-is-space c2))
         (multiple-value-bind (x p) (%tob-parse-float line (+ token 3))
           (make-%tob-command :type :vt :tx x :ty (%tob-parse-float line p))))
        ;; face
        ((and (= c0 102) (%tob-is-space c1))
         (let ((f nil) (num-f 0)
               (p (%tob-skip-space line (+ token 2))))
           (loop until (%tob-is-new-line (%tob line p))
                 do (multiple-value-bind (vi np) (%tob-parse-raw-triple line p)
                      (setf p (%tob-skip-space-and-cr line np))
                      (push vi f)
                      (incf num-f)
                      ;; NOTE: C overflows its f[16] buffer here (undefined behavior)
                      (when (> num-f +tinyobj-max-faces-per-f-line+) (return-from %tob-parse-line nil))))
           (setf f (nreverse f))
           (if triangulate
               (progn
                 (when (> (* 3 num-f) +tinyobj-max-faces-per-f-line+) (return-from %tob-parse-line nil))
                 (let ((i0 (first f)) (i1 nil) (i2 (second f)) (faces nil) (n 0))
                   (loop for k from 2 below num-f
                         do (setf i1 i2
                                  i2 (nth k f))
                            (push i0 faces) (push i1 faces) (push i2 faces)
                            (incf n))
                   (make-%tob-command :type :f :f (nreverse faces) :f-num-verts (make-list n :initial-element 3))))
               (make-%tob-command :type :f :f f :f-num-verts (list num-f)))))
        ;; use mtl
        ((and (starts "usemtl") (%tob-is-space (%tob line (+ token 6))))
         (let ((at (%tob-skip-space line (+ token 7))))
           (name-command :usemtl at 1)))
        ;; load mtl
        ((and (starts "mtllib") (%tob-is-space (%tob line (+ token 6))))
         ;; By specification, `mtllib` should be appear only once in .obj
         (let ((at (%tob-skip-space line (+ token 7))))
           (name-command :mtllib at 0)))
        ;; group name
        ((and (= c0 103) (%tob-is-space c1))
         ;; @todo { multiple group name. }
         (name-command :g (+ token 2) 0))
        ;; object name
        ((and (= c0 111) (%tob-is-space c1))
         ;; @todo { multiple object name? }
         (name-command :o (+ token 2) 0))
        (t nil)))))

(defun %tob-is-line-ending (buf i end-i)
  (let ((c (%tob buf i)))
    (or (= c 0)
        (= c 10)                        ; this includes \r\n
        (and (= c 13) (< (+ i 1) end-i) (/= (%tob buf (+ i 1)) 10))))) ; detect only \r case

;; Parse wavefront .obj (BUF is the file content octet vector, LEN its length)
;; Returns (values attrib shapes materials result)
(defun tinyobj-parse-obj (buf len flags)
  (when (< len 1) (return-from tinyobj-parse-obj (values nil nil nil :error-invalid-parameter)))
  (let ((attrib (make-tinyobj-attrib))
        (line-infos nil)
        (num-lines 0)
        (commands nil)
        (num-v 0) (num-vn 0) (num-vt 0) (num-f 0) (num-faces 0)
        (mtllib-line-index -1)
        (materials (vector))
        (material-table (make-hash-table)))
    ;; 1. Find '\n' and create line data.
    (let ((end-idx len) (prev-pos 0) (last-line-ending 0))
      ;; Count # of lines.
      (dotimes (i end-idx)
        (when (%tob-is-line-ending buf i end-idx)
          (incf num-lines)
          (setf last-line-ending i)))
      ;; The last char from the input may not be a line
      ;; ending character so add an extra line if there
      ;; are more characters after the last line ending
      ;; that was found.
      (when (> (- end-idx last-line-ending) 0) (incf num-lines))
      (when (= num-lines 0) (return-from tinyobj-parse-obj (values nil nil nil :error-empty)))
      ;; Fill line infos.
      (dotimes (i end-idx)
        (when (%tob-is-line-ending buf i end-idx)
          (let ((line-len (- i prev-pos)))
            ;; ---- QUICK BUG FIX : https://github.com/raysan5/raylib/issues/3473
            (when (and (> i 0) (= (%tob buf (- i 1)) 13)) (decf line-len))
            (push (cons prev-pos (logand line-len #xffffffff)) line-infos))
          (setf prev-pos (+ i 1))))
      (when (> (- end-idx last-line-ending) 0)
        (push (cons prev-pos (- end-idx 1 last-line-ending)) line-infos))
      (setf line-infos (coerce (nreverse line-infos) 'simple-vector)))

    ;; 2. parse each line
    (setf commands (make-array num-lines :initial-element nil))
    (dotimes (i num-lines)
      (let ((command (%tob-parse-line buf (car (svref line-infos i)) (cdr (svref line-infos i))
                                      (logtest flags +tinyobj-flag-triangulate+))))
        (setf (svref commands i) command)
        (when command
          (case (tobc-type command)
            (:v (incf num-v))
            (:vn (incf num-vn))
            (:vt (incf num-vt))
            (:f (incf num-f (length (tobc-f command)))
                (incf num-faces (length (tobc-f-num-verts command)))))
          (when (eq (tobc-type command) :mtllib)
            (setf mtllib-line-index i)))))

    ;; Load material(if exits)
    (when (and (>= mtllib-line-index 0) (> (tobc-name-len (svref commands mtllib-line-index)) 0))
      (let* ((command (svref commands mtllib-line-index))
             (filename (%tob-string buf (tobc-name-start command) (+ (tobc-name-start command) (tobc-name-len command)))))
        (multiple-value-bind (mats ret) (%tinyobj-parse-and-index-mtl-file filename material-table)
          (setf materials mats)
          (unless (eq ret :success)
            ;; warning.
            (format *error-output* "TINYOBJ: Failed to parse material file '~a': ~d~%" filename
                    (case ret (:error-empty -1) (:error-invalid-parameter -2) (:error-file-operation -3) (t 0)))))))

    ;; Construct attributes
    (let ((v-count 0) (n-count 0) (t-count 0) (f-count 0) (face-count 0)
          (material-id -1)              ; -1 = default unknown material.
          (vertices (make-array (* num-v 3) :element-type 'single-float :initial-element 0.0))
          (normals (make-array (* num-vn 3) :element-type 'single-float :initial-element 0.0))
          (texcoords (make-array (* num-vt 2) :element-type 'single-float :initial-element 0.0))
          (faces (make-array num-f))
          (face-num-verts (make-array num-faces :initial-element 0))
          (material-ids (make-array num-faces :initial-element 0)))
      (setf (tobja-vertices attrib) vertices (tobja-num-vertices attrib) num-v
            (tobja-normals attrib) normals (tobja-num-normals attrib) num-vn
            (tobja-texcoords attrib) texcoords (tobja-num-texcoords attrib) num-vt
            (tobja-faces attrib) faces (tobja-face-num-verts attrib) face-num-verts
            (tobja-num-faces attrib) num-faces (tobja-num-face-num-verts attrib) num-f
            (tobja-material-ids attrib) material-ids)
      (loop for command across commands
            when command
              do (case (tobc-type command)
                   (:usemtl
                    (when (> (tobc-name-len command) 0)
                      (let ((name (%tob-string buf (tobc-name-start command) (+ (tobc-name-start command) (tobc-name-len command)))))
                        (setf material-id (or (%tob-hash-table-get name material-table) -1)))))
                   (:v (setf (aref vertices (* 3 v-count)) (tobc-vx command)
                             (aref vertices (+ (* 3 v-count) 1)) (tobc-vy command)
                             (aref vertices (+ (* 3 v-count) 2)) (tobc-vz command))
                    (incf v-count))
                   (:vn (setf (aref normals (* 3 n-count)) (tobc-nx command)
                              (aref normals (+ (* 3 n-count) 1)) (tobc-ny command)
                              (aref normals (+ (* 3 n-count) 2)) (tobc-nz command))
                    (incf n-count))
                   (:vt (setf (aref texcoords (* 2 t-count)) (tobc-tx command)
                              (aref texcoords (+ (* 2 t-count) 1)) (tobc-ty command))
                    (incf t-count))
                   (:f (loop for (v vt vn) in (tobc-f command)
                             for k from 0
                             do (setf (svref faces (+ f-count k))
                                      (list (%tob-fix-index v v-count) (%tob-fix-index vt t-count) (%tob-fix-index vn n-count))))
                    (loop for nv in (tobc-f-num-verts command)
                          for k from 0
                          do (setf (aref material-ids (+ face-count k)) material-id
                                   (aref face-num-verts (+ face-count k)) nv))
                    (incf f-count (length (tobc-f command)))
                    (incf face-count (length (tobc-f-num-verts command)))))))

    ;; 5. Construct shape information.
    (let ((face-count 0)
          (shape-idx 0)
          (shape-name nil)
          (prev-shape-name nil)
          (prev-shape-face-offset 0)
          (prev-face-offset 0)
          (shapes nil))
      (flet ((name-of (command) (when command
                                  (%tob-string buf (tobc-name-start command)
                                               (+ (tobc-name-start command) (tobc-name-len command))))))
        (loop for command across commands
              do (when (and command (member (tobc-type command) '(:o :g)))
                   (setf shape-name command)
                   (if (= face-count 0)
                       ;; 'o' or 'g' appears before any 'f'
                       (setf prev-shape-name shape-name
                             prev-shape-face-offset face-count
                             prev-face-offset face-count)
                       (progn
                         (if (= shape-idx 0)
                             ;; 'o' or 'g' after some 'v' lines.
                             (progn
                               (push (make-tinyobj-shape :name (name-of prev-shape-name) ; may be NULL
                                                         :face-offset 0 ; prev_shape.face_offset
                                                         :length (- face-count prev-face-offset))
                                     shapes)
                               (incf shape-idx)
                               (setf prev-face-offset face-count))
                             (when (> (- face-count prev-face-offset) 0)
                               (push (make-tinyobj-shape :name (name-of prev-shape-name)
                                                         :face-offset prev-face-offset
                                                         :length (- face-count prev-face-offset))
                                     shapes)
                               (incf shape-idx)
                               (setf prev-face-offset face-count)))
                         ;; Record shape info for succeeding 'o' or 'g' command.
                         (setf prev-shape-name shape-name
                               prev-shape-face-offset face-count))))
                 (when (and command (eq (tobc-type command) :f))
                   (incf face-count)))
        (when (> (- face-count prev-face-offset) 0)
          (let ((length (- face-count prev-shape-face-offset)))
            (when (> length 0)
              (push (make-tinyobj-shape :name (name-of prev-shape-name)
                                        :face-offset prev-face-offset
                                        :length (- face-count prev-face-offset))
                    shapes))))
        ;; else: Guess no 'v' line occurrence after 'o' or 'g', so discards current shape information.
        (values attrib (coerce (nreverse shapes) 'simple-vector) materials :success)))))
