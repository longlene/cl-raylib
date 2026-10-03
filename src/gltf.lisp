(in-package #:cl-raylib)

;;;===================================================================================
;;; cgltf - glTF 2.0 loader (subset used by rmodels.c)
;;; Port of raylib/src/external/cgltf.h: cgltf_parse(), cgltf_load_buffers(),
;;; cgltf_load_buffer_base64(), cgltf_decode_uri(), cgltf_node_transform_local/world(),
;;; cgltf_accessor_read_float() and the embedded jsmn tokenizer (JSMN_STRICT, JSMN_PARENT_LINKS)
;;;
;;; NOTE: Objects raylib never reads (cameras, lights, material variants, material
;;; extensions, EXT_mesh_gpu_instancing, meshopt compression, texture transforms) are
;;; only checked to be objects with string keys and then skipped, so malformed contents
;;; inside them are not rejected like cgltf does
;;; NOTE: Buffer data reads out of the loaded data return 0 (C reads out of bounds or
;;; dereferences NULL when a buffer failed to load)
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Types and Structures Definition
;;;----------------------------------------------------------------------------------

;; cgltf_result: :success, :data-too-short, :unknown-format, :invalid-json, :invalid-gltf,
;; :invalid-options, :file-not-found, :io-error, :out-of-memory, :legacy-gltf

;; cgltf_file_type: :invalid, :gltf, :glb
;; cgltf_component_type: :invalid, :r-8, :r-8u, :r-16, :r-16u, :r-32u, :r-32f
;; cgltf_type: :invalid, :scalar, :vec2, :vec3, :vec4, :mat2, :mat3, :mat4
;; cgltf_primitive_type: :invalid, :points, :lines, :line-loop, :line-strip, :triangles, :triangle-strip, :triangle-fan
;; cgltf_attribute_type: :invalid, :position, :normal, :tangent, :texcoord, :color, :joints, :weights, :custom
;; cgltf_interpolation_type: :linear, :step, :cubic-spline, :max-enum
;; cgltf_animation_path_type: :invalid, :translation, :rotation, :scale, :weights

;; NOTE: Pointer fields hold CGLTF_PTRINDEX() values (index + 1, 0 for NULL) while parsing,
;; cgltf-fixup-pointers replaces them with the referenced objects (or NIL)

(defun %cgltf-floats (n &optional (value 0.0))
  (make-array n :element-type 'single-float :initial-element value))

(defstruct cgltf-buffer
  (name nil) (size 0) (uri nil)
  (data nil))                           ; Octet vector (NIL until cgltf-load-buffers)

(defstruct cgltf-buffer-view
  (name nil) (buffer 0) (offset 0) (size 0) (stride 0) (type :invalid))

(defstruct cgltf-accessor
  (name nil)
  (component-type :invalid)
  (normalized nil)
  (type :invalid)
  (offset 0)
  (count 0)
  (stride 0)
  (buffer-view 0)
  (has-min nil) (min (%cgltf-floats 16))
  (has-max nil) (max (%cgltf-floats 16))
  (is-sparse nil)
  ;; cgltf_accessor_sparse
  (sparse-count 0)
  (sparse-indices-buffer-view 0)
  (sparse-indices-byte-offset 0)
  (sparse-indices-component-type :invalid)
  (sparse-values-buffer-view 0)
  (sparse-values-byte-offset 0))

(defstruct cgltf-attribute
  (name nil) (type :invalid) (index 0) (data 0))

(defstruct cgltf-image
  (name nil) (uri nil) (buffer-view 0) (mime-type nil))

(defstruct cgltf-sampler
  (name nil) (mag-filter 0) (min-filter 0) (wrap-s 10497) (wrap-t 10497))

(defstruct cgltf-texture
  (name nil) (image 0) (sampler 0)
  (has-basisu nil) (basisu-image 0)
  (has-webp nil) (webp-image 0))

(defstruct cgltf-texture-view
  (texture 0) (texcoord 0) (scale 1.0) (has-transform nil))

(defstruct cgltf-material
  (name nil)
  (has-pbr-metallic-roughness nil)
  ;; cgltf_pbr_metallic_roughness
  (base-color-texture (make-cgltf-texture-view))
  (metallic-roughness-texture (make-cgltf-texture-view))
  (base-color-factor (%cgltf-floats 4 1.0))
  (metallic-factor 1.0)
  (roughness-factor 1.0)
  (normal-texture (make-cgltf-texture-view))
  (occlusion-texture (make-cgltf-texture-view))
  (emissive-texture (make-cgltf-texture-view))
  (emissive-factor (%cgltf-floats 3))
  (alpha-mode :opaque)
  (alpha-cutoff 0.5)
  (double-sided nil)
  (unlit nil))

(defstruct cgltf-morph-target
  (attributes nil))

(defstruct cgltf-primitive
  (type :triangles)
  (indices 0)
  (material 0)
  (attributes nil)                      ; Vector of cgltf-attribute
  (targets nil)                         ; Vector of cgltf-morph-target
  (has-draco-mesh-compression nil)
  (draco-buffer-view 0)
  (draco-attributes nil))

(defstruct cgltf-mesh
  (name nil) (primitives nil) (weights nil) (target-names nil))

(defstruct cgltf-skin
  (name nil) (joints nil) (skeleton 0) (inverse-bind-matrices 0))

(defstruct cgltf-node
  (name nil)
  (parent nil)
  (children nil)
  (skin 0) (mesh 0) (camera 0) (light 0)
  (weights nil)
  (has-translation nil) (has-rotation nil) (has-scale nil) (has-matrix nil)
  (translation (%cgltf-floats 3))
  (rotation (let ((r (%cgltf-floats 4))) (setf (aref r 3) 1.0) r))
  (scale (%cgltf-floats 3 1.0))
  (matrix (let ((m (%cgltf-floats 16))) (dolist (k '(0 5 10 15) m) (setf (aref m k) 1.0)))))

(defstruct cgltf-scene
  (name nil) (nodes nil))

(defstruct cgltf-animation-sampler
  (input 0) (output 0) (interpolation :linear))

(defstruct cgltf-animation-channel
  (sampler 0) (target-node 0) (target-path :invalid))

(defstruct cgltf-animation
  (name nil) (samplers nil) (channels nil))

(defstruct cgltf-data
  (file-type :invalid)
  (asset-version nil)
  ;; Arrays (simple-vector), NIL while not parsed
  (meshes nil) (materials nil) (accessors nil) (buffer-views nil) (buffers nil)
  (images nil) (textures nil) (samplers nil) (skins nil) (cameras-count 0) (lights-count 0)
  (nodes nil) (scenes nil) (scene 0) (animations nil)
  (extensions-used nil) (extensions-required nil)
  (bin nil))                            ; GLB binary chunk (octet vector)

;;;----------------------------------------------------------------------------------
;;; C library helpers: atoi(), atoll(), atof()
;;;----------------------------------------------------------------------------------

(defun %c-isspace (code)
  (member code '(32 9 10 11 12 13)))

(defun %c-strtoll (bytes start end)
  "C strtoll(str, NULL, 10) over BYTES[START, END), clamped to the long long range"
  (let ((i start) (sign 1) (value 0) (end (or end (length bytes))))
    (flet ((ch () (if (< i end) (aref bytes i) 0)))
      (loop while (%c-isspace (ch)) do (incf i))
      (case (ch)
        (45 (setf sign -1) (incf i))    ; '-'
        (43 (incf i)))                  ; '+'
      (loop while (<= 48 (ch) 57)
            do (setf value (+ (* value 10) (- (ch) 48)))
               (incf i))
      (max (- (expt 2 63)) (min (1- (expt 2 63)) (* sign value))))))

(defun %c-atoi (bytes start end)
  "C atoi(): (int)strtol(str, NULL, 10)"
  (%i32 (logand (%c-strtoll bytes start end) #xffffffff)))

(defun %c-strtod (bytes start end)
  "C strtod() over BYTES[START, END), result as a correctly rounded double-float"
  (let ((i start) (negative nil) (end (or end (length bytes))))
    (labels ((ch (&optional (k 0)) (if (< (+ i k) end) (aref bytes (+ i k)) 0))
             (lower (&optional (k 0)) (let ((c (ch k))) (if (<= 65 c 90) (+ c 32) c)))
             (match (string)
               (loop for c across string for k from 0 always (= (lower k) (char-code c))))
             (digit (c base)
               (cond ((<= 48 c 57) (- c 48))
                     ((and (= base 16) (<= 97 (logior c 32) 102)) (+ 10 (- (logior c 32) 97)))))
             (signed (value) (if negative (- value) value))
             (to-double (rational)
               ;; Correctly rounded rational -> double, with overflow to infinity
               (cond ((zerop rational) (if negative -0d0 0d0))
                     ((> rational most-positive-double-float)
                      (if negative sb-ext:double-float-negative-infinity sb-ext:double-float-positive-infinity))
                     (t (signed (coerce rational 'double-float))))))
      (loop while (%c-isspace (ch)) do (incf i))
      (case (ch)
        (45 (setf negative t) (incf i))
        (43 (incf i)))
      (cond ((match "inf")
             (signed sb-ext:double-float-positive-infinity))
            ((match "nan")
             ;; Quiet NaN, sign bit set for "-nan"
             (if negative (sb-kernel:make-double-float -524288 0) (sb-kernel:make-double-float 2146959360 0)))
            (t
             (let* ((base (if (and (= (ch) 48) (= (lower 1) 120) (or (digit (ch 2) 16) (and (= (ch 2) 46) (digit (ch 3) 16)))) 16 10))
                    (mantissa 0) (scale 0) (any-digit nil))
               (when (= base 16) (incf i 2))
               (loop for d = (digit (ch) base) while d
                     do (setf mantissa (+ (* mantissa base) d) any-digit t) (incf i))
               (when (= (ch) 46)        ; '.'
                 (incf i)
                 (loop for d = (digit (ch) base) while d
                       do (setf mantissa (+ (* mantissa base) d) any-digit t) (decf scale) (incf i)))
               (if (not any-digit)
                   0d0
                   (let ((exponent 0))
                     ;; Exponent part only consumed when digits follow
                     (when (and (= (lower) (if (= base 16) 112 101))
                                (or (<= 48 (ch 1) 57)
                                    (and (member (ch 1) '(43 45)) (<= 48 (ch 2) 57))))
                       (incf i)
                       (let ((exp-sign 1))
                         (case (ch) (45 (setf exp-sign -1) (incf i)) (43 (incf i)))
                         (loop while (<= 48 (ch) 57)
                               do (setf exponent (min 100000 (+ (* exponent 10) (- (ch) 48)))) (incf i))
                         (setf exponent (* exp-sign exponent))))
                     (if (= base 16)
                         (to-double (* mantissa (expt 2 (+ (* 4 scale) exponent))))
                         (let ((e10 (+ scale exponent))
                               (digits (if (zerop mantissa) 0 (length (princ-to-string mantissa)))))
                           (cond ((zerop mantissa) (if negative -0d0 0d0))
                                 ((> (+ e10 digits) 400) (to-double (expt 10 400)))
                                 ((< (+ e10 digits) -400) (if negative -0d0 0d0))
                                 (t (to-double (* mantissa (expt 10 e10)))))))))))))))

(defun %c-atof-single (bytes start end)
  "(float)atof(str)"
  (float-features:with-float-traps-masked t
    (coerce (%c-strtod bytes start end) 'single-float)))

;;;----------------------------------------------------------------------------------
;;; jsmn tokenizer (JSMN_STRICT and JSMN_PARENT_LINKS defined, as in cgltf)
;;;----------------------------------------------------------------------------------

(defstruct (%jsmn-token (:conc-name %jtok-))
  (type :undefined) (start -1) (end -1) (size 0) (parent -1))

(defun %jsmn-parse (js start len)
  "Tokenize JSON in JS[START, START+LEN), returns a vector of tokens (positions relative to START) or NIL on error"
  (let ((tokens (make-array 256 :adjustable t :fill-pointer 0))
        (pos 0) (toksuper -1))
    (labels ((c (p) (if (< p len) (aref js (+ start p)) 0))
             (tok (k) (aref tokens k))
             (alloc-token ()
               (let ((token (make-%jsmn-token)))
                 (vector-push-extend token tokens)
                 token))
             (parse-primitive ()
               (let ((pstart pos))
                 (loop while (and (< pos len) (/= (c pos) 0))
                       do (let ((ch (c pos)))
                            (when (member ch '(9 13 10 32 44 93 125)) ; \t \r \n space , ] }
                              (let ((token (alloc-token)))
                                (setf (%jtok-type token) :primitive (%jtok-start token) pstart (%jtok-end token) pos
                                      (%jtok-parent token) toksuper))
                              (decf pos)
                              (return-from parse-primitive t))
                            (when (or (< ch 32) (>= ch 127))
                              (return-from parse-primitive nil)))
                          (incf pos))
                 ;; In strict mode primitive must be followed by a comma/object/array
                 nil))
             (parse-string ()
               (let ((sstart pos))
                 (incf pos)
                 (loop while (and (< pos len) (/= (c pos) 0))
                       do (let ((ch (c pos)))
                            ;; Quote: end of string
                            (when (= ch 34)
                              (let ((token (alloc-token)))
                                (setf (%jtok-type token) :string (%jtok-start token) (1+ sstart) (%jtok-end token) pos
                                      (%jtok-parent token) toksuper))
                              (return-from parse-string t))
                            ;; Backslash: Quoted symbol expected
                            (when (and (= ch 92) (< (1+ pos) len))
                              (incf pos)
                              (case (c pos)
                                ((34 47 92 98 102 114 110 116)) ; " / \ b f r n t
                                (117    ; 'u'
                                 (incf pos)
                                 (loop for k below 4
                                       while (and (< pos len) (/= (c pos) 0))
                                       do (let ((h (c pos)))
                                            (unless (or (<= 48 h 57) (<= 65 h 70) (<= 97 h 102))
                                              (return-from parse-string nil))
                                            (incf pos)))
                                 (decf pos))
                                (t (return-from parse-string nil)))))
                          (incf pos))
                 nil)))
      (loop while (and (< pos len) (/= (c pos) 0))
            do (let ((ch (c pos)))
                 (case ch
                   ((123 91)            ; { [
                    (let ((token (alloc-token)))
                      (when (/= toksuper -1)
                        (incf (%jtok-size (tok toksuper)))
                        (setf (%jtok-parent token) toksuper))
                      (setf (%jtok-type token) (if (= ch 123) :object :array)
                            (%jtok-start token) pos
                            toksuper (1- (fill-pointer tokens)))))
                   ((125 93)            ; } ]
                    (let ((type (if (= ch 125) :object :array)))
                      (when (< (fill-pointer tokens) 1) (return-from %jsmn-parse nil))
                      (let ((token (tok (1- (fill-pointer tokens)))))
                        (loop
                          (when (and (/= (%jtok-start token) -1) (= (%jtok-end token) -1))
                            (unless (eq (%jtok-type token) type) (return-from %jsmn-parse nil))
                            (setf (%jtok-end token) (1+ pos)
                                  toksuper (%jtok-parent token))
                            (return))
                          (when (= (%jtok-parent token) -1)
                            (when (or (not (eq (%jtok-type token) type)) (= toksuper -1))
                              (return-from %jsmn-parse nil))
                            (return))
                          (setf token (tok (%jtok-parent token)))))))
                   (34                  ; "
                    (unless (parse-string) (return-from %jsmn-parse nil))
                    (when (/= toksuper -1) (incf (%jtok-size (tok toksuper)))))
                   ((9 13 10 32))
                   (58                  ; :
                    (setf toksuper (1- (fill-pointer tokens))))
                   (44                  ; ,
                    (when (and (/= toksuper -1)
                               (not (member (%jtok-type (tok toksuper)) '(:array :object))))
                      (setf toksuper (%jtok-parent (tok toksuper)))))
                   ;; In strict mode primitives are: numbers and booleans
                   ((45 48 49 50 51 52 53 54 55 56 57 116 102 110) ; - 0-9 t f n
                    ;; And they must not be keys of the object
                    (when (/= toksuper -1)
                      (let ((super (tok toksuper)))
                        (when (or (eq (%jtok-type super) :object)
                                  (and (eq (%jtok-type super) :string) (/= (%jtok-size super) 0)))
                          (return-from %jsmn-parse nil))))
                    (unless (parse-primitive) (return-from %jsmn-parse nil))
                    (when (/= toksuper -1) (incf (%jtok-size (tok toksuper)))))
                   ;; Unexpected char in strict mode
                   (t (return-from %jsmn-parse nil))))
               (incf pos))
      ;; Unmatched opened object or array
      (loop for token across tokens
            when (and (/= (%jtok-start token) -1) (= (%jtok-end token) -1))
              do (return-from %jsmn-parse nil))
      (if (zerop (fill-pointer tokens)) nil (coerce tokens 'simple-vector)))))

;;;----------------------------------------------------------------------------------
;;; JSON parsing helpers
;;;----------------------------------------------------------------------------------

(defvar *cgltf-json* nil "JSON chunk octets")
(defvar *cgltf-json-start* 0 "Offset of the JSON chunk in *cgltf-json*")
(defvar *cgltf-tokens* nil "jsmn tokens")

(defconstant +cgltf-error-json+ -1)
(defconstant +cgltf-error-nomem+ -2)
(defconstant +cgltf-error-legacy+ -3)

(defun %cgltf-error (&optional (code +cgltf-error-json+))
  (throw '%cgltf-error code))

(defun %tok (i)
  "Token I, an UNDEFINED token past the end of the stream"
  (if (< -1 i (length *cgltf-tokens*))
      (svref *cgltf-tokens* i)
      (load-time-value (make-%jsmn-token))))

(defun %tok-type (i) (%jtok-type (%tok i)))
(defun %tok-size (i) (%jtok-size (%tok i)))

(defun %check-toktype (i type)
  (unless (eq (%tok-type i) type) (%cgltf-error)))

(defun %check-key (i)
  ;; checking size for 0 verifies that a value follows the key
  (unless (and (eq (%tok-type i) :string) (/= (%tok-size i) 0)) (%cgltf-error)))

(defun %json-strcmp-p (i string)
  "cgltf_json_strcmp() == 0"
  (let ((tok (%tok i)))
    (and (eq (%jtok-type tok) :string)
         (= (length string) (- (%jtok-end tok) (%jtok-start tok)))
         (loop for c across string
               for k from (+ *cgltf-json-start* (%jtok-start tok))
               always (= (aref *cgltf-json* k) (char-code c))))))

(defun %tok-bounds (i)
  "Token text bounds as cgltf copies it into char tmp[128]"
  (let* ((tok (%tok i))
         (start (+ *cgltf-json-start* (%jtok-start tok)))
         (size (min (- (%jtok-end tok) (%jtok-start tok)) 127)))
    (values start (+ start size))))

(defun %json-to-int (i)
  (if (eq (%tok-type i) :primitive)
      (multiple-value-bind (start end) (%tok-bounds i) (%c-atoi *cgltf-json* start end))
      +cgltf-error-json+))

(defun %json-to-size (i)
  (if (eq (%tok-type i) :primitive)
      (multiple-value-bind (start end) (%tok-bounds i) (max 0 (%c-strtoll *cgltf-json* start end)))
      0))

(defun %json-to-float (i)
  (if (eq (%tok-type i) :primitive)
      (multiple-value-bind (start end) (%tok-bounds i) (%c-atof-single *cgltf-json* start end))
      -1.0))

(defun %json-to-bool (i)
  (let ((tok (%tok i)))
    (and (= (- (%jtok-end tok) (%jtok-start tok)) 4)
         (%json-strcmp-p-raw tok "true"))))

(defun %json-strcmp-p-raw (tok string)
  (loop for c across string
        for k from (+ *cgltf-json-start* (%jtok-start tok))
        always (and (< -1 k (length *cgltf-json*)) (= (aref *cgltf-json* k) (char-code c)))))

(defun %json-ptrindex (index)
  "CGLTF_PTRINDEX(type, idx): (cgltf_size)idx + 1"
  (logand (1+ index) #xffffffffffffffff))

(defun %skip-json (i)
  (let ((end (1+ i)))
    (loop while (< i end)
          do (case (%tok-type i)
               (:object (incf end (* (%tok-size i) 2)))
               (:array (incf end (%tok-size i)))
               ((:primitive :string))
               (t (%cgltf-error)))
             (incf i))
    i))

(defun %parse-json-float-array (i out-array size)
  (%check-toktype i :array)
  (unless (= (%tok-size i) size) (%cgltf-error))
  (incf i)
  (dotimes (j size)
    (%check-toktype i :primitive)
    (setf (aref out-array j) (%json-to-float i))
    (incf i))
  i)

(defun %parse-json-string (i current)
  "Returns the new token index and the token string (raw, escapes are kept like cgltf)"
  (%check-toktype i :string)
  (when current (%cgltf-error))
  (let ((tok (%tok i)))
    (values (1+ i)
            (babel:octets-to-string *cgltf-json* :start (+ *cgltf-json-start* (%jtok-start tok))
                                                 :end (+ *cgltf-json-start* (%jtok-end tok))
                                                 :encoding :utf-8 :errorp nil))))

(defun %parse-json-array (i current)
  "Returns the new token index and the array size"
  (unless (eq (%tok-type i) :array)
    (%cgltf-error (if (eq (%tok-type i) :object) +cgltf-error-legacy+ +cgltf-error-json+)))
  (when current (%cgltf-error))
  (values (1+ i) (%tok-size i)))

(defun %parse-json-string-array (i current)
  (%check-toktype i :array)
  (multiple-value-bind (ni size) (%parse-json-array i current)
    (setf i ni)
    (let ((strings (make-array size)))
      (dotimes (j size)
        (setf (values i (svref strings j)) (%parse-json-string i nil)))
      (values i strings))))

(defun %parse-json-extras (i seen)
  (when seen (%cgltf-error))
  (%skip-json i))

(defun %parse-json-unprocessed-extension (i)
  (%check-toktype i :string)
  (%check-toktype (1+ i) :object)
  (%skip-json (1+ i)))

(defun %parse-json-unprocessed-extensions (i seen)
  (incf i)
  (%check-toktype i :object)
  (when seen (%cgltf-error))
  (let ((size (%tok-size i)))
    (incf i)
    (dotimes (j size)
      (%check-key i)
      (setf i (%parse-json-unprocessed-extension i)))
    i))

(defun %parse-json-skipped-object (i)
  "Object not used by raylib: checked to be an object with keys, contents skipped"
  (%check-toktype i :object)
  (let ((size (%tok-size i)))
    (incf i)
    (dotimes (j size)
      (%check-key i)
      (setf i (%skip-json (1+ i))))
    i))

;; Parse the JSON object at token I (a variable): each clause is (KEY FORM), FORM is evaluated
;; with I at the key token and returns the next token index; unknown keys are skipped
;; :EXTRAS and :EXTENSIONS add the generic "extras"/"extensions" (unprocessed) clauses
(defmacro %json-object ((i &key extras extensions) &body clauses)
  (let ((size (gensym "SIZE")) (seen-extras (gensym "EXTRAS")) (seen-extensions (gensym "EXTENSIONS")))
    `(progn
       (%check-toktype ,i :object)
       (let ((,size (%tok-size ,i)) (,seen-extras nil) (,seen-extensions nil))
         (declare (ignorable ,seen-extras ,seen-extensions))
         (incf ,i)
         (dotimes (j ,size)
           (declare (ignorable j))
           (%check-key ,i)
           (setf ,i (cond ,@(loop for (key form) in clauses
                                  collect `((%json-strcmp-p ,i ,key) ,form))
                          ,@(when extras
                              `(((%json-strcmp-p ,i "extras")
                                 (%parse-json-extras (1+ ,i) (shiftf ,seen-extras t)))))
                          ,@(when extensions
                              `(((%json-strcmp-p ,i "extensions")
                                 (%parse-json-unprocessed-extensions ,i (shiftf ,seen-extensions t)))))
                          (t (%skip-json (1+ ,i))))))
         ,i))))

;; Value parsing for "key": value pairs, I at the key token, returns the next token index
(defmacro %json-value (i place reader)
  `(progn (setf ,place (,reader (1+ ,i))) (+ ,i 2)))

(defmacro %json-ptr-value (i place &optional check-primitive)
  `(progn
     ,@(when check-primitive `((%check-toktype (1+ ,i) :primitive)))
     (setf ,place (%json-ptrindex (%json-to-int (1+ ,i))))
     (+ ,i 2)))

(defmacro %json-string-value (i place)
  (let ((ni (gensym)) (s (gensym)))
    `(multiple-value-bind (,ni ,s) (%parse-json-string (1+ ,i) ,place)
       (setf ,place ,s)
       ,ni)))

(defmacro %json-ptr-array-value (i place)
  "Array of CGLTF_PTRINDEX() values (joints, children, scene nodes)"
  (let ((ni (gensym)) (size (gensym)) (v (gensym)))
    `(multiple-value-bind (,ni ,size) (%parse-json-array (1+ ,i) ,place)
       (let ((,v (make-array ,size)))
         (setf ,place ,v)
         (dotimes (k ,size)
           (setf (svref ,v k) (%json-ptrindex (%json-to-int ,ni)))
           (incf ,ni))
         ,ni))))

(defmacro %json-objects-value (i place parser)
  "Array of objects parsed by PARSER (i -> (values next-i object))"
  (let ((ni (gensym)) (size (gensym)) (v (gensym)))
    `(multiple-value-bind (,ni ,size) (%parse-json-array ,i ,place)
       (let ((,v (make-array ,size)))
         (setf ,place ,v)
         (dotimes (k ,size)
           (setf (values ,ni (svref ,v k)) (,parser ,ni)))
         ,ni))))

;;;----------------------------------------------------------------------------------
;;; glTF objects parsing
;;;----------------------------------------------------------------------------------

(defun %json-to-component-type (i)
  (case (%json-to-int i)
    (5120 :r-8) (5121 :r-8u) (5122 :r-16) (5123 :r-16u) (5125 :r-32u) (5126 :r-32f)
    (t :invalid)))

(defun %json-to-primitive-type (i)
  (case (%json-to-int i)
    (0 :points) (1 :lines) (2 :line-loop) (3 :line-strip) (4 :triangles) (5 :triangle-strip) (6 :triangle-fan)
    (t :invalid)))

(defun %parse-attribute-type (name)
  "cgltf_parse_attribute_type(): returns (values type index)"
  (if (and (> (length name) 0) (char= (char name 0) #\_))
      (values :custom 0)
      (let* ((us (position #\_ name))
             (prefix (subseq name 0 (or us (length name))))
             (type (cond ((string= prefix "POSITION") :position)
                         ((string= prefix "NORMAL") :normal)
                         ((string= prefix "TANGENT") :tangent)
                         ((string= prefix "TEXCOORD") :texcoord)
                         ((string= prefix "COLOR") :color)
                         ((string= prefix "JOINTS") :joints)
                         ((string= prefix "WEIGHTS") :weights)
                         (t :invalid)))
             (index 0))
        (when (and us (not (eq type :invalid)))
          (let ((bytes (babel:string-to-octets name :start (1+ us) :encoding :utf-8)))
            (setf index (%c-atoi bytes 0 (length bytes))))
          (when (< index 0) (setf type :invalid index 0)))
        (values type index))))

(defun %parse-json-attribute-list (i current)
  (%check-toktype i :object)
  (when current (%cgltf-error))
  (let* ((count (%tok-size i))
         (attributes (make-array count)))
    (incf i)
    (dotimes (j count)
      (%check-key i)
      (let ((attribute (make-cgltf-attribute)))
        (setf (svref attributes j) attribute)
        (multiple-value-bind (ni name) (%parse-json-string i nil)
          (setf i ni (cgltf-attribute-name attribute) name))
        (multiple-value-bind (type index) (%parse-attribute-type (cgltf-attribute-name attribute))
          (setf (cgltf-attribute-type attribute) type (cgltf-attribute-index attribute) index))
        (setf (cgltf-attribute-data attribute) (%json-ptrindex (%json-to-int i)))
        (incf i)))
    (values i attributes)))

(defmacro %json-attribute-list-value (i place)
  (let ((ni (gensym)) (v (gensym)))
    `(multiple-value-bind (,ni ,v) (%parse-json-attribute-list (1+ ,i) ,place)
       (setf ,place ,v)
       ,ni)))

(defun %parse-json-primitive (i)
  (let ((prim (make-cgltf-primitive)) (seen-extras nil) (seen-extensions nil))
    (setf i (%json-object (i)
              ("mode" (%json-value i (cgltf-primitive-type prim) %json-to-primitive-type))
              ("indices" (%json-ptr-value i (cgltf-primitive-indices prim)))
              ("material" (%json-ptr-value i (cgltf-primitive-material prim)))
              ("attributes" (%json-attribute-list-value i (cgltf-primitive-attributes prim)))
              ("targets"
               (multiple-value-bind (ni size) (%parse-json-array (1+ i) (cgltf-primitive-targets prim))
                 (let ((targets (make-array size)))
                   (setf (cgltf-primitive-targets prim) targets)
                   (dotimes (k size)
                     (let ((target (make-cgltf-morph-target)))
                       (setf (svref targets k) target)
                       (multiple-value-bind (nni attributes) (%parse-json-attribute-list ni nil)
                         (setf ni nni (cgltf-morph-target-attributes target) attributes))))
                   ni)))
              ("extras" (%parse-json-extras (1+ i) (shiftf seen-extras t)))
              ("extensions"
               (progn
                 (incf i)
                 (%check-toktype i :object)
                 (when (shiftf seen-extensions t) (%cgltf-error))
                 (let ((size (%tok-size i)))
                   (incf i)
                   (dotimes (k size)
                     (%check-key i)
                     (setf i (cond ((%json-strcmp-p i "KHR_draco_mesh_compression")
                                    (setf (cgltf-primitive-has-draco-mesh-compression prim) t)
                                    (%parse-json-draco-mesh-compression (1+ i) prim))
                                   ((%json-strcmp-p i "KHR_materials_variants")
                                    (%parse-json-skipped-object (1+ i)))
                                   (t (%parse-json-unprocessed-extension i)))))
                   i)))))
    (values i prim)))

(defun %parse-json-draco-mesh-compression (i prim)
  (%json-object (i)
    ("attributes" (%json-attribute-list-value i (cgltf-primitive-draco-attributes prim)))
    ("bufferView" (%json-ptr-value i (cgltf-primitive-draco-buffer-view prim)))))

(defun %parse-json-mesh (i)
  (let ((mesh (make-cgltf-mesh)))
    (setf i (%json-object (i :extensions t)
              ("name" (%json-string-value i (cgltf-mesh-name mesh)))
              ("primitives" (%json-objects-value (1+ i) (cgltf-mesh-primitives mesh) %parse-json-primitive))
              ("weights"
               (multiple-value-bind (ni size) (%parse-json-array (1+ i) (cgltf-mesh-weights mesh))
                 (setf (cgltf-mesh-weights mesh) (%cgltf-floats size))
                 (%parse-json-float-array (1- ni) (cgltf-mesh-weights mesh) size)))
              ("extras"
               (progn
                 (incf i)
                 (if (eq (%tok-type i) :object)
                     (let ((size (%tok-size i)))
                       (incf i)
                       (dotimes (k size)
                         (%check-key i)
                         (setf i (if (and (%json-strcmp-p i "targetNames") (eq (%tok-type (1+ i)) :array))
                                     (multiple-value-bind (ni names)
                                         (%parse-json-string-array (1+ i) (cgltf-mesh-target-names mesh))
                                       (setf (cgltf-mesh-target-names mesh) names)
                                       ni)
                                     (%skip-json (1+ i)))))
                       i)
                     (%skip-json i))))))
    (values i mesh)))

(defun %parse-json-accessor-sparse (i accessor)
  (%json-object (i)
    ("count" (%json-value i (cgltf-accessor-sparse-count accessor) %json-to-size))
    ("indices"
     (progn
       (incf i)
       (%json-object (i)
         ("bufferView" (%json-ptr-value i (cgltf-accessor-sparse-indices-buffer-view accessor)))
         ("byteOffset" (%json-value i (cgltf-accessor-sparse-indices-byte-offset accessor) %json-to-size))
         ("componentType" (%json-value i (cgltf-accessor-sparse-indices-component-type accessor) %json-to-component-type)))))
    ("values"
     (progn
       (incf i)
       (%json-object (i)
         ("bufferView" (%json-ptr-value i (cgltf-accessor-sparse-values-buffer-view accessor)))
         ("byteOffset" (%json-value i (cgltf-accessor-sparse-values-byte-offset accessor) %json-to-size)))))))

(defun %parse-json-accessor (i)
  (let ((accessor (make-cgltf-accessor)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-accessor-name accessor)))
              ("bufferView" (%json-ptr-value i (cgltf-accessor-buffer-view accessor)))
              ("byteOffset" (%json-value i (cgltf-accessor-offset accessor) %json-to-size))
              ("componentType" (%json-value i (cgltf-accessor-component-type accessor) %json-to-component-type))
              ("normalized" (%json-value i (cgltf-accessor-normalized accessor) %json-to-bool))
              ("count" (%json-value i (cgltf-accessor-count accessor) %json-to-size))
              ("type"
               (progn
                 (incf i)
                 (loop for (name type) in '(("SCALAR" :scalar) ("VEC2" :vec2) ("VEC3" :vec3) ("VEC4" :vec4)
                                            ("MAT2" :mat2) ("MAT3" :mat3) ("MAT4" :mat4))
                       when (%json-strcmp-p i name)
                         do (setf (cgltf-accessor-type accessor) type) (return))
                 (1+ i)))
              ;; NOTE: we can't parse the precise number of elements since type may not have been computed yet
              ("min"
               (progn
                 (incf i)
                 (setf (cgltf-accessor-has-min accessor) t)
                 (%parse-json-float-array i (cgltf-accessor-min accessor) (min (%tok-size i) 16))))
              ("max"
               (progn
                 (incf i)
                 (setf (cgltf-accessor-has-max accessor) t)
                 (%parse-json-float-array i (cgltf-accessor-max accessor) (min (%tok-size i) 16))))
              ("sparse"
               (progn
                 (setf (cgltf-accessor-is-sparse accessor) t)
                 (%parse-json-accessor-sparse (1+ i) accessor)))))
    (values i accessor)))

(defun %parse-json-texture-view (i view)
  (%check-toktype i :object)
  (setf (cgltf-texture-view-scale view) 1.0)
  (%json-object (i)
    ("index" (%json-ptr-value i (cgltf-texture-view-texture view)))
    ("texCoord" (%json-value i (cgltf-texture-view-texcoord view) %json-to-int))
    ("scale" (%json-value i (cgltf-texture-view-scale view) %json-to-float))
    ("strength" (%json-value i (cgltf-texture-view-scale view) %json-to-float))
    ("extensions"
     (progn
       (incf i)
       (%check-toktype i :object)
       (let ((size (%tok-size i)))
         (incf i)
         (dotimes (k size)
           (%check-key i)
           (setf i (if (%json-strcmp-p i "KHR_texture_transform")
                       (progn (setf (cgltf-texture-view-has-transform view) t)
                              (%parse-json-skipped-object (1+ i)))
                       (%skip-json (1+ i)))))
         i)))))

(defun %parse-json-pbr-metallic-roughness (i material)
  (%json-object (i)
    ("metallicFactor" (%json-value i (cgltf-material-metallic-factor material) %json-to-float))
    ("roughnessFactor" (%json-value i (cgltf-material-roughness-factor material) %json-to-float))
    ("baseColorFactor" (%parse-json-float-array (1+ i) (cgltf-material-base-color-factor material) 4))
    ("baseColorTexture" (%parse-json-texture-view (1+ i) (cgltf-material-base-color-texture material)))
    ("metallicRoughnessTexture" (%parse-json-texture-view (1+ i) (cgltf-material-metallic-roughness-texture material)))))

(defun %parse-json-material (i)
  (let ((material (make-cgltf-material)) (seen-extensions nil))
    (setf i (%json-object (i :extras t)
              ("name" (%json-string-value i (cgltf-material-name material)))
              ("pbrMetallicRoughness"
               (progn
                 (setf (cgltf-material-has-pbr-metallic-roughness material) t)
                 (%parse-json-pbr-metallic-roughness (1+ i) material)))
              ("emissiveFactor" (%parse-json-float-array (1+ i) (cgltf-material-emissive-factor material) 3))
              ("normalTexture" (%parse-json-texture-view (1+ i) (cgltf-material-normal-texture material)))
              ("occlusionTexture" (%parse-json-texture-view (1+ i) (cgltf-material-occlusion-texture material)))
              ("emissiveTexture" (%parse-json-texture-view (1+ i) (cgltf-material-emissive-texture material)))
              ("alphaMode"
               (progn
                 (incf i)
                 (cond ((%json-strcmp-p i "OPAQUE") (setf (cgltf-material-alpha-mode material) :opaque))
                       ((%json-strcmp-p i "MASK") (setf (cgltf-material-alpha-mode material) :mask))
                       ((%json-strcmp-p i "BLEND") (setf (cgltf-material-alpha-mode material) :blend)))
                 (1+ i)))
              ("alphaCutoff" (%json-value i (cgltf-material-alpha-cutoff material) %json-to-float))
              ("doubleSided" (%json-value i (cgltf-material-double-sided material) %json-to-bool))
              ("extensions"
               (progn
                 (incf i)
                 (%check-toktype i :object)
                 (when (shiftf seen-extensions t) (%cgltf-error))
                 (let ((size (%tok-size i)))
                   (incf i)
                   (dotimes (k size)
                     (%check-key i)
                     (setf i (cond ((%json-strcmp-p i "KHR_materials_unlit")
                                    (setf (cgltf-material-unlit material) t)
                                    (%skip-json (1+ i)))
                                   ((member-if (lambda (name) (%json-strcmp-p i name))
                                               '("KHR_materials_pbrSpecularGlossiness" "KHR_materials_clearcoat"
                                                 "KHR_materials_ior" "KHR_materials_specular" "KHR_materials_transmission"
                                                 "KHR_materials_volume" "KHR_materials_sheen" "KHR_materials_emissive_strength"
                                                 "KHR_materials_iridescence" "KHR_materials_diffuse_transmission"
                                                 "KHR_materials_anisotropy" "KHR_materials_dispersion"))
                                    (%parse-json-skipped-object (1+ i)))
                                   (t (%parse-json-unprocessed-extension i)))))
                   i)))))
    (values i material)))

(defun %parse-json-image (i)
  (let ((image (make-cgltf-image)))
    (setf i (%json-object (i :extras t :extensions t)
              ("uri" (%json-string-value i (cgltf-image-uri image)))
              ("bufferView" (%json-ptr-value i (cgltf-image-buffer-view image)))
              ("mimeType" (%json-string-value i (cgltf-image-mime-type image)))
              ("name" (%json-string-value i (cgltf-image-name image)))))
    (values i image)))

(defun %parse-json-sampler (i)
  (let ((sampler (make-cgltf-sampler)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-sampler-name sampler)))
              ("magFilter" (%json-value i (cgltf-sampler-mag-filter sampler) %json-to-int))
              ("minFilter" (%json-value i (cgltf-sampler-min-filter sampler) %json-to-int))
              ("wrapS" (%json-value i (cgltf-sampler-wrap-s sampler) %json-to-int))
              ("wrapT" (%json-value i (cgltf-sampler-wrap-t sampler) %json-to-int))))
    (values i sampler)))

(defun %parse-json-texture-source-extension (i set-source)
  "KHR_texture_basisu / EXT_texture_webp: { source }"
  (incf i)
  (%check-toktype i :object)
  (let ((size (%tok-size i)))
    (incf i)
    (dotimes (k size)
      (%check-key i)
      (setf i (if (%json-strcmp-p i "source")
                  (progn (funcall set-source (%json-ptrindex (%json-to-int (1+ i)))) (+ i 2))
                  (%skip-json (1+ i)))))
    i))

(defun %parse-json-texture (i)
  (let ((texture (make-cgltf-texture)) (seen-extensions nil))
    (setf i (%json-object (i :extras t)
              ("name" (%json-string-value i (cgltf-texture-name texture)))
              ("sampler" (%json-ptr-value i (cgltf-texture-sampler texture)))
              ("source" (%json-ptr-value i (cgltf-texture-image texture)))
              ("extensions"
               (progn
                 (incf i)
                 (%check-toktype i :object)
                 (when (shiftf seen-extensions t) (%cgltf-error))
                 (let ((size (%tok-size i)))
                   (incf i)
                   (dotimes (k size)
                     (%check-key i)
                     (setf i (cond ((%json-strcmp-p i "KHR_texture_basisu")
                                    (setf (cgltf-texture-has-basisu texture) t)
                                    (%parse-json-texture-source-extension
                                     i (lambda (v) (setf (cgltf-texture-basisu-image texture) v))))
                                   ((%json-strcmp-p i "EXT_texture_webp")
                                    (setf (cgltf-texture-has-webp texture) t)
                                    (%parse-json-texture-source-extension
                                     i (lambda (v) (setf (cgltf-texture-webp-image texture) v))))
                                   (t (%parse-json-unprocessed-extension i)))))
                   i)))))
    (values i texture)))

(defun %parse-json-buffer-view (i)
  (let ((view (make-cgltf-buffer-view)) (seen-extensions nil))
    (setf i (%json-object (i :extras t)
              ("name" (%json-string-value i (cgltf-buffer-view-name view)))
              ("buffer" (%json-ptr-value i (cgltf-buffer-view-buffer view)))
              ("byteOffset" (%json-value i (cgltf-buffer-view-offset view) %json-to-size))
              ("byteLength" (%json-value i (cgltf-buffer-view-size view) %json-to-size))
              ("byteStride" (%json-value i (cgltf-buffer-view-stride view) %json-to-size))
              ("target"
               (progn
                 (setf (cgltf-buffer-view-type view)
                       (case (%json-to-int (1+ i)) (34962 :vertices) (34963 :indices) (t :invalid)))
                 (+ i 2)))
              ("extensions"
               (progn
                 (incf i)
                 (%check-toktype i :object)
                 (when (shiftf seen-extensions t) (%cgltf-error))
                 (let ((size (%tok-size i)))
                   (incf i)
                   (dotimes (k size)
                     (%check-key i)
                     (setf i (if (or (%json-strcmp-p i "EXT_meshopt_compression")
                                     (%json-strcmp-p i "KHR_meshopt_compression"))
                                 (%parse-json-skipped-object (1+ i))
                                 (%parse-json-unprocessed-extension i))))
                   i)))))
    (values i view)))

(defun %parse-json-buffer (i)
  (let ((buffer (make-cgltf-buffer)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-buffer-name buffer)))
              ("byteLength" (%json-value i (cgltf-buffer-size buffer) %json-to-size))
              ("uri" (%json-string-value i (cgltf-buffer-uri buffer)))))
    (values i buffer)))

(defun %parse-json-skin (i)
  (let ((skin (make-cgltf-skin)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-skin-name skin)))
              ("joints" (%json-ptr-array-value i (cgltf-skin-joints skin)))
              ("skeleton" (%json-ptr-value i (cgltf-skin-skeleton skin) t))
              ("inverseBindMatrices" (%json-ptr-value i (cgltf-skin-inverse-bind-matrices skin) t))))
    (values i skin)))

(defun %parse-json-node (i)
  (let ((node (make-cgltf-node)) (seen-extensions nil))
    (setf i (%json-object (i :extras t)
              ("name" (%json-string-value i (cgltf-node-name node)))
              ("children" (%json-ptr-array-value i (cgltf-node-children node)))
              ("mesh" (%json-ptr-value i (cgltf-node-mesh node) t))
              ("skin" (%json-ptr-value i (cgltf-node-skin node) t))
              ("camera" (%json-ptr-value i (cgltf-node-camera node) t))
              ("translation"
               (progn (setf (cgltf-node-has-translation node) t)
                      (%parse-json-float-array (1+ i) (cgltf-node-translation node) 3)))
              ("rotation"
               (progn (setf (cgltf-node-has-rotation node) t)
                      (%parse-json-float-array (1+ i) (cgltf-node-rotation node) 4)))
              ("scale"
               (progn (setf (cgltf-node-has-scale node) t)
                      (%parse-json-float-array (1+ i) (cgltf-node-scale node) 3)))
              ("matrix"
               (progn (setf (cgltf-node-has-matrix node) t)
                      (%parse-json-float-array (1+ i) (cgltf-node-matrix node) 16)))
              ("weights"
               (multiple-value-bind (ni size) (%parse-json-array (1+ i) (cgltf-node-weights node))
                 (setf (cgltf-node-weights node) (%cgltf-floats size))
                 (%parse-json-float-array (1- ni) (cgltf-node-weights node) size)))
              ("extensions"
               (progn
                 (incf i)
                 (%check-toktype i :object)
                 (when (shiftf seen-extensions t) (%cgltf-error))
                 (let ((size (%tok-size i)))
                   (incf i)
                   (dotimes (k size)
                     (%check-key i)
                     (setf i (cond ((%json-strcmp-p i "KHR_lights_punctual")
                                    (incf i)
                                    (%json-object (i)
                                      ("light" (%json-ptr-value i (cgltf-node-light node) t))))
                                   ((%json-strcmp-p i "EXT_mesh_gpu_instancing")
                                    (%parse-json-skipped-object (1+ i)))
                                   (t (%parse-json-unprocessed-extension i)))))
                   i)))))
    (values i node)))

(defun %parse-json-scene (i)
  (let ((scene (make-cgltf-scene)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-scene-name scene)))
              ("nodes" (%json-ptr-array-value i (cgltf-scene-nodes scene)))))
    (values i scene)))

(defun %parse-json-animation-sampler (i)
  (let ((sampler (make-cgltf-animation-sampler)))
    (setf i (%json-object (i :extras t :extensions t)
              ("input" (%json-ptr-value i (cgltf-animation-sampler-input sampler)))
              ("output" (%json-ptr-value i (cgltf-animation-sampler-output sampler)))
              ("interpolation"
               (progn
                 (incf i)
                 (cond ((%json-strcmp-p i "LINEAR") (setf (cgltf-animation-sampler-interpolation sampler) :linear))
                       ((%json-strcmp-p i "STEP") (setf (cgltf-animation-sampler-interpolation sampler) :step))
                       ((%json-strcmp-p i "CUBICSPLINE") (setf (cgltf-animation-sampler-interpolation sampler) :cubic-spline)))
                 (1+ i)))))
    (values i sampler)))

(defun %parse-json-animation-channel (i)
  (let ((channel (make-cgltf-animation-channel)))
    (setf i (%json-object (i)
              ("sampler" (%json-ptr-value i (cgltf-animation-channel-sampler channel)))
              ("target"
               (progn
                 (incf i)
                 (%json-object (i :extras t :extensions t)
                   ("node" (%json-ptr-value i (cgltf-animation-channel-target-node channel)))
                   ("path"
                    (progn
                      (incf i)
                      (loop for (name path) in '(("translation" :translation) ("rotation" :rotation)
                                                 ("scale" :scale) ("weights" :weights))
                            when (%json-strcmp-p i name)
                              do (setf (cgltf-animation-channel-target-path channel) path) (return))
                      (1+ i))))))))
    (values i channel)))

(defun %parse-json-animation (i)
  (let ((animation (make-cgltf-animation)))
    (setf i (%json-object (i :extras t :extensions t)
              ("name" (%json-string-value i (cgltf-animation-name animation)))
              ("samplers" (%json-objects-value (1+ i) (cgltf-animation-samplers animation) %parse-json-animation-sampler))
              ("channels" (%json-objects-value (1+ i) (cgltf-animation-channels animation) %parse-json-animation-channel))))
    (values i animation)))

(defun %parse-json-asset (i data)
  (let ((copyright nil) (generator nil) (min-version nil))
    (setf i (%json-object (i :extras t :extensions t)
              ("copyright" (%json-string-value i copyright))
              ("generator" (%json-string-value i generator))
              ("version" (%json-string-value i (cgltf-data-asset-version data)))
              ("minVersion" (%json-string-value i min-version)))))
  (let ((version (cgltf-data-asset-version data)))
    (when version
      (let ((bytes (babel:string-to-octets version :encoding :utf-8)))
        (when (< (%c-strtod bytes 0 (length bytes)) 2) (%cgltf-error +cgltf-error-legacy+)))))
  i)

(defun %parse-json-skipped-objects (i)
  "Array of objects not used by raylib (cameras, lights, variants): returns (values i count)"
  (multiple-value-bind (ni size) (%parse-json-array i nil)
    (dotimes (k size)
      (setf ni (%parse-json-skipped-object ni)))
    (values ni size)))

(defun %parse-json-root (i data)
  (let ((seen-extensions nil))
    (%json-object (i :extras t)
      ("asset" (%parse-json-asset (1+ i) data))
      ("meshes" (%json-objects-value (1+ i) (cgltf-data-meshes data) %parse-json-mesh))
      ("accessors" (%json-objects-value (1+ i) (cgltf-data-accessors data) %parse-json-accessor))
      ("bufferViews" (%json-objects-value (1+ i) (cgltf-data-buffer-views data) %parse-json-buffer-view))
      ("buffers" (%json-objects-value (1+ i) (cgltf-data-buffers data) %parse-json-buffer))
      ("materials" (%json-objects-value (1+ i) (cgltf-data-materials data) %parse-json-material))
      ("images" (%json-objects-value (1+ i) (cgltf-data-images data) %parse-json-image))
      ("textures" (%json-objects-value (1+ i) (cgltf-data-textures data) %parse-json-texture))
      ("samplers" (%json-objects-value (1+ i) (cgltf-data-samplers data) %parse-json-sampler))
      ("skins" (%json-objects-value (1+ i) (cgltf-data-skins data) %parse-json-skin))
      ("cameras"
       (multiple-value-bind (ni count) (%parse-json-skipped-objects (1+ i))
         (setf (cgltf-data-cameras-count data) count)
         ni))
      ("nodes" (%json-objects-value (1+ i) (cgltf-data-nodes data) %parse-json-node))
      ("scenes" (%json-objects-value (1+ i) (cgltf-data-scenes data) %parse-json-scene))
      ("scene" (%json-ptr-value i (cgltf-data-scene data)))
      ("animations" (%json-objects-value (1+ i) (cgltf-data-animations data) %parse-json-animation))
      ("extensions"
       (progn
         (incf i)
         (%check-toktype i :object)
         (when (shiftf seen-extensions t) (%cgltf-error))
         (let ((size (%tok-size i)))
           (incf i)
           (dotimes (k size)
             (%check-key i)
             (setf i (cond ((%json-strcmp-p i "KHR_lights_punctual")
                            (incf i)
                            (%json-object (i)
                              ("lights"
                               (multiple-value-bind (ni count) (%parse-json-skipped-objects (1+ i))
                                 (setf (cgltf-data-lights-count data) count)
                                 ni))))
                           ((%json-strcmp-p i "KHR_materials_variants")
                            (incf i)
                            (%json-object (i)
                              ("variants" (nth-value 0 (%parse-json-skipped-objects (1+ i))))))
                           (t (%parse-json-unprocessed-extension i)))))
           i)))
      ("extensionsUsed"
       (multiple-value-bind (ni names) (%parse-json-string-array (1+ i) (cgltf-data-extensions-used data))
         (setf (cgltf-data-extensions-used data) names)
         ni))
      ("extensionsRequired"
       (multiple-value-bind (ni names) (%parse-json-string-array (1+ i) (cgltf-data-extensions-required data))
         (setf (cgltf-data-extensions-required data) names)
         ni)))))

;;;----------------------------------------------------------------------------------
;;; Pointers fixup
;;;----------------------------------------------------------------------------------

(defun %ptrfixup (value array &optional required)
  "CGLTF_PTRFIXUP / CGLTF_PTRFIXUP_REQ: index + 1 -> object"
  (cond ((eql value 0) (if required (%cgltf-error) nil))
        ((> value (length array)) (%cgltf-error))
        (t (aref array (1- value)))))

(defun %ptrfixup-count (value count)
  "CGLTF_PTRFIXUP for objects not kept (cameras, lights): only the index is checked"
  (cond ((eql value 0) nil)
        ((> value count) (%cgltf-error))
        (t (1- value))))

(defun cgltf-num-components (type)
  (case type (:vec2 2) (:vec3 3) (:vec4 4) (:mat2 4) (:mat3 9) (:mat4 16) (t 1)))

(defun cgltf-component-size (component-type)
  (case component-type ((:r-8 :r-8u) 1) ((:r-16 :r-16u) 2) ((:r-32u :r-32f) 4) (t 0)))

(defun cgltf-calc-size (type component-type)
  (let ((component-size (cgltf-component-size component-type)))
    (cond ((and (eq type :mat2) (= component-size 1)) (* 8 component-size))
          ((and (eq type :mat3) (or (= component-size 1) (= component-size 2))) (* 12 component-size))
          (t (* component-size (cgltf-num-components type))))))

(defun %cgltf-fixup-pointers (data)
  (let ((accessors (cgltf-data-accessors data))
        (materials (cgltf-data-materials data))
        (buffer-views (cgltf-data-buffer-views data))
        (textures (cgltf-data-textures data))
        (nodes (cgltf-data-nodes data)))
    (loop for mesh across (cgltf-data-meshes data)
          do (loop for prim across (or (cgltf-mesh-primitives mesh) #())
                   do (setf (cgltf-primitive-indices prim) (%ptrfixup (cgltf-primitive-indices prim) accessors)
                            (cgltf-primitive-material prim) (%ptrfixup (cgltf-primitive-material prim) materials))
                      (loop for attribute across (or (cgltf-primitive-attributes prim) #())
                            do (setf (cgltf-attribute-data attribute) (%ptrfixup (cgltf-attribute-data attribute) accessors t)))
                      (loop for target across (or (cgltf-primitive-targets prim) #())
                            do (loop for attribute across (or (cgltf-morph-target-attributes target) #())
                                     do (setf (cgltf-attribute-data attribute) (%ptrfixup (cgltf-attribute-data attribute) accessors t))))
                      (when (cgltf-primitive-has-draco-mesh-compression prim)
                        (setf (cgltf-primitive-draco-buffer-view prim) (%ptrfixup (cgltf-primitive-draco-buffer-view prim) buffer-views t))
                        (loop for attribute across (or (cgltf-primitive-draco-attributes prim) #())
                              do (setf (cgltf-attribute-data attribute) (%ptrfixup (cgltf-attribute-data attribute) accessors t))))))

    (loop for accessor across accessors
          do (setf (cgltf-accessor-buffer-view accessor) (%ptrfixup (cgltf-accessor-buffer-view accessor) buffer-views))
             (when (cgltf-accessor-is-sparse accessor)
               (setf (cgltf-accessor-sparse-indices-buffer-view accessor)
                     (%ptrfixup (cgltf-accessor-sparse-indices-buffer-view accessor) buffer-views t)
                     (cgltf-accessor-sparse-values-buffer-view accessor)
                     (%ptrfixup (cgltf-accessor-sparse-values-buffer-view accessor) buffer-views t)))
             (when (cgltf-accessor-buffer-view accessor)
               (setf (cgltf-accessor-stride accessor) (cgltf-buffer-view-stride (cgltf-accessor-buffer-view accessor))))
             (when (zerop (cgltf-accessor-stride accessor))
               (setf (cgltf-accessor-stride accessor)
                     (cgltf-calc-size (cgltf-accessor-type accessor) (cgltf-accessor-component-type accessor)))))

    (loop for texture across textures
          do (setf (cgltf-texture-image texture) (%ptrfixup (cgltf-texture-image texture) (cgltf-data-images data))
                   (cgltf-texture-basisu-image texture) (%ptrfixup (cgltf-texture-basisu-image texture) (cgltf-data-images data))
                   (cgltf-texture-webp-image texture) (%ptrfixup (cgltf-texture-webp-image texture) (cgltf-data-images data))
                   (cgltf-texture-sampler texture) (%ptrfixup (cgltf-texture-sampler texture) (cgltf-data-samplers data))))

    (loop for image across (cgltf-data-images data)
          do (setf (cgltf-image-buffer-view image) (%ptrfixup (cgltf-image-buffer-view image) buffer-views)))

    (loop for material across materials
          do (dolist (view (list (cgltf-material-normal-texture material)
                                 (cgltf-material-emissive-texture material)
                                 (cgltf-material-occlusion-texture material)
                                 (cgltf-material-base-color-texture material)
                                 (cgltf-material-metallic-roughness-texture material)))
               (setf (cgltf-texture-view-texture view) (%ptrfixup (cgltf-texture-view-texture view) textures))))

    (loop for view across buffer-views
          do (setf (cgltf-buffer-view-buffer view) (%ptrfixup (cgltf-buffer-view-buffer view) (cgltf-data-buffers data) t)))

    (loop for skin across (cgltf-data-skins data)
          do (let ((joints (or (cgltf-skin-joints skin) #())))
               (dotimes (j (length joints))
                 (setf (svref joints j) (%ptrfixup (svref joints j) nodes t))))
             (setf (cgltf-skin-skeleton skin) (%ptrfixup (cgltf-skin-skeleton skin) nodes)
                   (cgltf-skin-inverse-bind-matrices skin) (%ptrfixup (cgltf-skin-inverse-bind-matrices skin) accessors)))

    (loop for node across nodes
          do (let ((children (or (cgltf-node-children node) #())))
               (dotimes (j (length children))
                 (let ((child (%ptrfixup (svref children j) nodes t)))
                   (setf (svref children j) child)
                   (when (cgltf-node-parent child) (%cgltf-error))
                   (setf (cgltf-node-parent child) node))))
             (setf (cgltf-node-mesh node) (%ptrfixup (cgltf-node-mesh node) (cgltf-data-meshes data))
                   (cgltf-node-skin node) (%ptrfixup (cgltf-node-skin node) (cgltf-data-skins data))
                   (cgltf-node-camera node) (%ptrfixup-count (cgltf-node-camera node) (cgltf-data-cameras-count data))
                   (cgltf-node-light node) (%ptrfixup-count (cgltf-node-light node) (cgltf-data-lights-count data))))

    (loop for scene across (cgltf-data-scenes data)
          do (let ((scene-nodes (or (cgltf-scene-nodes scene) #())))
               (dotimes (j (length scene-nodes))
                 (let ((node (%ptrfixup (svref scene-nodes j) nodes t)))
                   (setf (svref scene-nodes j) node)
                   (when (cgltf-node-parent node) (%cgltf-error))))))

    (setf (cgltf-data-scene data) (%ptrfixup (cgltf-data-scene data) (cgltf-data-scenes data)))

    (loop for animation across (cgltf-data-animations data)
          do (loop for sampler across (or (cgltf-animation-samplers animation) #())
                   do (setf (cgltf-animation-sampler-input sampler) (%ptrfixup (cgltf-animation-sampler-input sampler) accessors t)
                            (cgltf-animation-sampler-output sampler) (%ptrfixup (cgltf-animation-sampler-output sampler) accessors t)))
             (loop for channel across (or (cgltf-animation-channels animation) #())
                   do (setf (cgltf-animation-channel-sampler channel)
                            (%ptrfixup (cgltf-animation-channel-sampler channel) (or (cgltf-animation-samplers animation) #()) t)
                            (cgltf-animation-channel-target-node channel)
                            (%ptrfixup (cgltf-animation-channel-target-node channel) nodes))))
    0))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

(defun %cgltf-parse-json (bytes start size)
  "cgltf_parse_json(): returns (values result data)"
  (let ((tokens (%jsmn-parse bytes start size)))
    (unless tokens (return-from %cgltf-parse-json (values :invalid-json nil)))
    (let* ((data (make-cgltf-data))
           (code (let ((*cgltf-json* bytes) (*cgltf-json-start* start) (*cgltf-tokens* tokens))
                   (catch '%cgltf-error
                     (%parse-json-root 0 data)))))
      ;; Arrays not defined in the file are empty
      (dolist (accessor '(cgltf-data-meshes cgltf-data-materials cgltf-data-accessors cgltf-data-buffer-views
                          cgltf-data-buffers cgltf-data-images cgltf-data-textures cgltf-data-samplers
                          cgltf-data-skins cgltf-data-nodes cgltf-data-scenes cgltf-data-animations))
        (unless (funcall accessor data) (funcall (fdefinition `(setf ,accessor)) #() data)))
      (cond ((minusp code)
             (values (case code (#.+cgltf-error-nomem+ :out-of-memory) (#.+cgltf-error-legacy+ :legacy-gltf) (t :invalid-gltf)) nil))
            ((minusp (catch '%cgltf-error (%cgltf-fixup-pointers data)))
             (values :invalid-gltf nil))
            (t (values :success data))))))

(defun %u32le-at (bytes offset)
  (logior (aref bytes offset) (ash (aref bytes (+ offset 1)) 8)
          (ash (aref bytes (+ offset 2)) 16) (ash (aref bytes (+ offset 3)) 24)))

(defconstant +glb-header-size+ 12)
(defconstant +glb-chunk-header-size+ 8)
(defconstant +glb-version+ 2)
(defconstant +glb-magic+ #x46546C67)
(defconstant +glb-magic-json-chunk+ #x4E4F534A)
(defconstant +glb-magic-bin-chunk+ #x004E4942)

(defun cgltf-parse (data size)
  "Parse glTF/GLB data, returns (values result cgltf-data)"
  (when (< size +glb-header-size+) (return-from cgltf-parse (values :data-too-short nil)))

  ;; Magic
  (when (/= (%u32le-at data 0) +glb-magic+)
    (multiple-value-bind (result out) (%cgltf-parse-json data 0 size)
      (when out (setf (cgltf-data-file-type out) :gltf))
      (return-from cgltf-parse (values result out))))

  ;; Version
  (let ((version (%u32le-at data 4)))
    (when (/= version +glb-version+)
      (return-from cgltf-parse (values (if (< version +glb-version+) :legacy-gltf :unknown-format) nil))))

  ;; Total length
  (when (> (%u32le-at data 8) size) (return-from cgltf-parse (values :data-too-short nil)))

  (when (> (+ +glb-header-size+ +glb-chunk-header-size+) size) (return-from cgltf-parse (values :data-too-short nil)))

  ;; JSON chunk: length
  (let ((json-length (%u32le-at data +glb-header-size+))
        (json-chunk (+ +glb-header-size+ +glb-chunk-header-size+))
        (bin nil))
    (when (> json-length (- size +glb-header-size+ +glb-chunk-header-size+))
      (return-from cgltf-parse (values :data-too-short nil)))

    ;; JSON chunk: magic
    (when (/= (%u32le-at data (+ +glb-header-size+ 4)) +glb-magic-json-chunk+)
      (return-from cgltf-parse (values :unknown-format nil)))

    (when (<= +glb-chunk-header-size+ (- size +glb-header-size+ +glb-chunk-header-size+ json-length))
      ;; We can read another chunk
      (let* ((bin-chunk (+ json-chunk json-length))
             (bin-length (%u32le-at data bin-chunk)))
        ;; Bin chunk: length
        (when (> bin-length (- size +glb-header-size+ +glb-chunk-header-size+ json-length +glb-chunk-header-size+))
          (return-from cgltf-parse (values :data-too-short nil)))
        ;; Bin chunk: magic
        (when (/= (%u32le-at data (+ bin-chunk 4)) +glb-magic-bin-chunk+)
          (return-from cgltf-parse (values :unknown-format nil)))
        (setf bin (subseq data (+ bin-chunk +glb-chunk-header-size+) (+ bin-chunk +glb-chunk-header-size+ bin-length)))))

    (multiple-value-bind (result out) (%cgltf-parse-json data json-chunk json-length)
      (when out
        (setf (cgltf-data-file-type out) :glb
              (cgltf-data-bin out) bin))
      (values result out))))

(defun %cgltf-unhex (code)
  (cond ((<= 48 code 57) (- code 48))
        ((<= 65 code 70) (+ (- code 65) 10))
        ((<= 97 code 102) (+ (- code 97) 10))
        (t -1)))

(defun cgltf-decode-uri (uri)
  "Decode %XX sequences of an uri (as octets, result decoded as UTF-8)"
  (let* ((bytes (babel:string-to-octets uri :encoding :utf-8))
         (out (make-array (length bytes) :element-type '(unsigned-byte 8) :fill-pointer 0))
         (i 0))
    (flet ((b (k) (if (< k (length bytes)) (aref bytes k) 0)))
      (loop while (< i (length bytes))
            do (let ((ch1 (if (= (b i) 37) (%cgltf-unhex (b (+ i 1))) -1))
                     (ch2 (if (= (b i) 37) (%cgltf-unhex (b (+ i 2))) -1)))
                 (if (and (>= ch1 0) (>= ch2 0))
                     (progn (vector-push (+ (* ch1 16) ch2) out) (incf i 3))
                     (progn (vector-push (b i) out) (incf i))))))
    ;; A decoded NUL ends the C string
    (let ((end (or (position 0 out) (length out))))
      (babel:octets-to-string out :end end :encoding :utf-8 :errorp nil))))

(defun %cgltf-combine-paths (base uri)
  (let* ((s0 (position #\/ base :from-end t))
         (s1 (position #\\ base :from-end t))
         (slash (if s0 (if (and s1 (> s1 s0)) s1 s0) s1)))
    (if slash
        (concatenate 'string (subseq base 0 (1+ slash)) uri)
        uri)))

(defun %cgltf-load-buffer-file (uri gltf-path)
  "Returns (values result data)"
  ;; after combining, the tail of the resulting path is a uri; decode_uri converts it into path
  (let* ((path (%cgltf-combine-paths gltf-path (cgltf-decode-uri uri)))
         (file-data (load-file-data path)))
    (if file-data (values :success file-data) (values :io-error nil))))

(defun cgltf-load-buffer-base64 (size base64 &optional (start 0))
  "Decode SIZE bytes from the BASE64 string starting at START, returns (values result data)"
  (let ((data (make-array size :element-type '(unsigned-byte 8)))
        (buffer 0) (buffer-bits 0) (pos start))
    (dotimes (i size)
      (loop while (< buffer-bits 8)
            do (let* ((ch (if (< pos (length base64)) (char-code (char base64 pos)) 0))
                      (index (cond ((<= 65 ch 90) (- ch 65))
                                   ((<= 97 ch 122) (+ (- ch 97) 26))
                                   ((<= 48 ch 57) (+ (- ch 48) 52))
                                   ((= ch 43) 62)
                                   ((= ch 47) 63)
                                   (t -1))))
                 (incf pos)
                 (when (< index 0) (return-from cgltf-load-buffer-base64 (values :io-error nil)))
                 (setf buffer (logand (logior (ash buffer 6) index) #xffffffff))
                 (incf buffer-bits 6)))
      (setf (aref data i) (logand (ash buffer (- (- buffer-bits 8))) #xff))
      (decf buffer-bits 8))
    (values :success data)))

(defun cgltf-load-buffers (data gltf-path)
  "Load buffers data (GLB binary chunk, base64 data uris or external files)"
  (let ((buffers (cgltf-data-buffers data)))
    (when (and (> (length buffers) 0)
               (null (cgltf-buffer-data (aref buffers 0)))
               (null (cgltf-buffer-uri (aref buffers 0)))
               (cgltf-data-bin data))
      (when (< (length (cgltf-data-bin data)) (cgltf-buffer-size (aref buffers 0)))
        (return-from cgltf-load-buffers :data-too-short))
      (setf (cgltf-buffer-data (aref buffers 0)) (cgltf-data-bin data)))

    (loop for buffer across buffers
          do (let ((uri (cgltf-buffer-uri buffer)))
               (unless (or (cgltf-buffer-data buffer) (null uri))
                 (cond ((and (>= (length uri) 5) (string= uri "data:" :end1 5))
                        (let ((comma (position #\, uri)))
                          (if (and comma (>= comma 7) (string= uri ";base64" :start1 (- comma 7) :end1 comma))
                              (multiple-value-bind (res bytes)
                                  (cgltf-load-buffer-base64 (cgltf-buffer-size buffer) uri (1+ comma))
                                (setf (cgltf-buffer-data buffer) bytes)
                                (unless (eq res :success) (return-from cgltf-load-buffers res)))
                              (return-from cgltf-load-buffers :unknown-format))))
                       ((and (null (search "://" uri)) gltf-path)
                        (multiple-value-bind (res bytes) (%cgltf-load-buffer-file uri gltf-path)
                          (setf (cgltf-buffer-data buffer) bytes)
                          (unless (eq res :success) (return-from cgltf-load-buffers res))))
                       (t (return-from cgltf-load-buffers :unknown-format))))))
    :success))

;;;----------------------------------------------------------------------------------
;;; Node transforms
;;;----------------------------------------------------------------------------------

(defun cgltf-node-transform-local (node)
  "Local transform of NODE as a column-major float[16]"
  (if (cgltf-node-has-matrix node)
      (copy-seq (cgltf-node-matrix node))
      (let* ((lm (%cgltf-floats 16))
             (tr (cgltf-node-translation node))
             (q (cgltf-node-rotation node))
             (s (cgltf-node-scale node))
             (qx (aref q 0)) (qy (aref q 1)) (qz (aref q 2)) (qw (aref q 3))
             (sx (aref s 0)) (sy (aref s 1)) (sz (aref s 2)))
        (declare (type single-float qx qy qz qw sx sy sz))
        (float-features:with-float-traps-masked t
          (setf (aref lm 0) (* (- (- 1.0 (* (* 2.0 qy) qy)) (* (* 2.0 qz) qz)) sx)
                (aref lm 1) (* (+ (* (* 2.0 qx) qy) (* (* 2.0 qz) qw)) sx)
                (aref lm 2) (* (- (* (* 2.0 qx) qz) (* (* 2.0 qy) qw)) sx)
                (aref lm 3) 0.0

                (aref lm 4) (* (- (* (* 2.0 qx) qy) (* (* 2.0 qz) qw)) sy)
                (aref lm 5) (* (- (- 1.0 (* (* 2.0 qx) qx)) (* (* 2.0 qz) qz)) sy)
                (aref lm 6) (* (+ (* (* 2.0 qy) qz) (* (* 2.0 qx) qw)) sy)
                (aref lm 7) 0.0

                (aref lm 8) (* (+ (* (* 2.0 qx) qz) (* (* 2.0 qy) qw)) sz)
                (aref lm 9) (* (- (* (* 2.0 qy) qz) (* (* 2.0 qx) qw)) sz)
                (aref lm 10) (* (- (- 1.0 (* (* 2.0 qx) qx)) (* (* 2.0 qy) qy)) sz)
                (aref lm 11) 0.0

                (aref lm 12) (aref tr 0)
                (aref lm 13) (aref tr 1)
                (aref lm 14) (aref tr 2)
                (aref lm 15) 1.0))
        lm)))

(defun cgltf-node-transform-world (node)
  "World transform of NODE as a column-major float[16]"
  (let ((lm (cgltf-node-transform-local node)))
    (float-features:with-float-traps-masked t
      (loop for parent = (cgltf-node-parent node) then (cgltf-node-parent parent)
            while parent
            do (let ((pm (cgltf-node-transform-local parent)))
                 (dotimes (i 4)
                   (let ((l0 (aref lm (+ (* i 4) 0)))
                         (l1 (aref lm (+ (* i 4) 1)))
                         (l2 (aref lm (+ (* i 4) 2))))
                     (setf (aref lm (+ (* i 4) 0)) (+ (+ (* l0 (aref pm 0)) (* l1 (aref pm 4))) (* l2 (aref pm 8)))
                           (aref lm (+ (* i 4) 1)) (+ (+ (* l0 (aref pm 1)) (* l1 (aref pm 5))) (* l2 (aref pm 9)))
                           (aref lm (+ (* i 4) 2)) (+ (+ (* l0 (aref pm 2)) (* l1 (aref pm 6))) (* l2 (aref pm 10))))))
                 (incf (aref lm 12) (aref pm 12))
                 (incf (aref lm 13) (aref pm 13))
                 (incf (aref lm 14) (aref pm 14)))))
    lm))

;;;----------------------------------------------------------------------------------
;;; Accessors data reading
;;;----------------------------------------------------------------------------------

;; Buffer bytes readers (0 out of the buffer data)
(declaim (inline %gltf-u8))
(defun %gltf-u8 (data offset)
  (if (and data (< -1 offset (length data))) (aref data offset) 0))
(defun %gltf-u16 (data offset)
  (logior (%gltf-u8 data offset) (ash (%gltf-u8 data (+ offset 1)) 8)))
(defun %gltf-u32 (data offset)
  (logior (%gltf-u16 data offset) (ash (%gltf-u16 data (+ offset 2)) 16)))
(defun %gltf-s8 (data offset)
  (let ((v (%gltf-u8 data offset))) (if (>= v #x80) (- v #x100) v)))
(defun %gltf-s16 (data offset)
  (let ((v (%gltf-u16 data offset))) (if (>= v #x8000) (- v #x10000) v)))
(defun %gltf-f32 (data offset)
  (sb-kernel:make-single-float (%i32 (%gltf-u32 data offset))))

(defun %cgltf-component-read-integer (data offset component-type)
  (case component-type
    (:r-16 (%gltf-s16 data offset))
    (:r-16u (%gltf-u16 data offset))
    (:r-32u (%gltf-u32 data offset))
    (:r-8 (%gltf-s8 data offset))
    (:r-8u (%gltf-u8 data offset))
    (t 0)))

(defun %cgltf-component-read-index (data offset component-type)
  (case component-type
    (:r-16u (%gltf-u16 data offset))
    (:r-32u (%gltf-u32 data offset))
    (:r-8u (%gltf-u8 data offset))
    (t 0)))

(defun %cgltf-component-read-float (data offset component-type normalized)
  (cond ((eq component-type :r-32f) (%gltf-f32 data offset))
        (normalized
         ;; NOTE: glTF spec doesn't currently define normalized conversions for 32-bit integers
         (case component-type
           (:r-16 (/ (float (%gltf-s16 data offset) 1.0) 32767.0))
           (:r-16u (/ (float (%gltf-u16 data offset) 1.0) 65535.0))
           (:r-8 (/ (float (%gltf-s8 data offset) 1.0) 127.0))
           (:r-8u (/ (float (%gltf-u8 data offset) 1.0) 255.0))
           (t 0.0)))
        (t (coerce (%cgltf-component-read-integer data offset component-type) 'single-float))))

(defun %cgltf-element-read-float (data element type component-type normalized out element-size)
  (let ((num-components (cgltf-num-components type)))
    (when (< element-size num-components) (return-from %cgltf-element-read-float nil))
    ;; There are three special cases for component extraction, see #data-alignment in the 2.0 spec
    (let* ((component-size (cgltf-component-size component-type))
           (offsets (cond ((and (eq type :mat2) (= component-size 1)) '(0 1 4 5))
                          ((and (eq type :mat3) (= component-size 1)) '(0 1 2 4 5 6 8 9 10))
                          ((and (eq type :mat3) (= component-size 2)) '(0 2 4 8 10 12 16 18 20))
                          (t (loop for i below num-components collect (* component-size i))))))
      (loop for offset in offsets
            for i from 0
            do (setf (aref out i) (%cgltf-component-read-float data (+ element offset) component-type normalized)))
      t)))

(defun %cgltf-buffer-view-data (view)
  "cgltf_buffer_view_data(): (values bytes offset), NIL if the buffer is not loaded"
  (let ((data (cgltf-buffer-data (cgltf-buffer-view-buffer view))))
    (if data (values data (cgltf-buffer-view-offset view)) nil)))

(defun %cgltf-find-sparse-index (accessor needle)
  "Returns (values bytes offset) of the sparse value for element NEEDLE, NIL if not found"
  (multiple-value-bind (index-data index-base) (%cgltf-buffer-view-data (cgltf-accessor-sparse-indices-buffer-view accessor))
    (multiple-value-bind (value-data value-base) (%cgltf-buffer-view-data (cgltf-accessor-sparse-values-buffer-view accessor))
      (when (or (null index-data) (null value-data)) (return-from %cgltf-find-sparse-index nil))
      (let* ((index-base (+ index-base (cgltf-accessor-sparse-indices-byte-offset accessor)))
             (value-base (+ value-base (cgltf-accessor-sparse-values-byte-offset accessor)))
             (component-type (cgltf-accessor-sparse-indices-component-type accessor))
             (index-stride (cgltf-component-size component-type))
             (offset 0)
             (length (cgltf-accessor-sparse-count accessor)))
        (loop while (> length 0)
              do (let ((rem (mod length 2)))
                   (setf length (floor length 2))
                   (let ((index (%cgltf-component-read-index index-data (+ index-base (* (+ offset length) index-stride)) component-type)))
                     (incf offset (if (< index needle) (+ length rem) 0)))))
        (when (= offset (cgltf-accessor-sparse-count accessor)) (return-from %cgltf-find-sparse-index nil))
        (let ((index (%cgltf-component-read-index index-data (+ index-base (* offset index-stride)) component-type)))
          (if (= index needle)
              (values value-data (+ value-base (* offset (cgltf-accessor-stride accessor))))
              nil))))))

(defun cgltf-accessor-read-float (accessor index out element-size)
  "Read element INDEX of ACCESSOR as floats into OUT, returns T on success"
  (when (cgltf-accessor-is-sparse accessor)
    (multiple-value-bind (data element) (%cgltf-find-sparse-index accessor index)
      (when data
        (return-from cgltf-accessor-read-float
          (%cgltf-element-read-float data element (cgltf-accessor-type accessor) (cgltf-accessor-component-type accessor)
                                     (cgltf-accessor-normalized accessor) out element-size)))))
  (unless (cgltf-accessor-buffer-view accessor)
    (fill out 0.0 :end element-size)
    (return-from cgltf-accessor-read-float t))
  (multiple-value-bind (data element) (%cgltf-buffer-view-data (cgltf-accessor-buffer-view accessor))
    (unless data (return-from cgltf-accessor-read-float nil))
    (%cgltf-element-read-float data (+ element (cgltf-accessor-offset accessor) (* (cgltf-accessor-stride accessor) index))
                               (cgltf-accessor-type accessor) (cgltf-accessor-component-type accessor)
                               (cgltf-accessor-normalized accessor) out element-size)))
