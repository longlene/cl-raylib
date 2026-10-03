(in-package #:cl-raylib)

;;;; Port of raylib rtextures.c - Basic functions to load and draw textures
;;;;
;;;; Image data (CPU) is a byte vector with exactly the raylib memory layout for the
;;;; image pixel format; the image manipulation and drawing functions follow
;;;; rtextures.c function by function. The Color* functions live in color.lisp.
;;;; Texture (GPU) functions use OpenGL directly.

;;; Texture filtering modes
(defconstant +texture-filter-point+ 0)           ; No filter, just pixel approximation  
(defconstant +texture-filter-bilinear+ 1)        ; Linear filtering
(defconstant +texture-filter-trilinear+ 2)       ; Trilinear filtering (linear with mipmaps)
(defconstant +texture-filter-anisotropic-4x+ 3)  ; Anisotropic filtering 4x
(defconstant +texture-filter-anisotropic-8x+ 4)  ; Anisotropic filtering 8x
(defconstant +texture-filter-anisotropic-16x+ 5) ; Anisotropic filtering 16x

;;; Texture wrap modes
(defconstant +texture-wrap-repeat+ 0)        ; Repeats texture in tiled mode
(defconstant +texture-wrap-clamp+ 1)         ; Clamps texture to edge pixel in tiled mode
(defconstant +texture-wrap-mirror-repeat+ 2) ; Mirrors and repeats the texture in tiled mode
(defconstant +texture-wrap-mirror-clamp+ 3)  ; Mirrors and clamps to border the texture in tiled mode

;;; Cubemap layout types
(defconstant +cubemap-layout-auto-detect+ 0)        ; Automatically detect layout type
(defconstant +cubemap-layout-line-vertical+ 1)      ; Layout is defined by a vertical line with faces
(defconstant +cubemap-layout-line-horizontal+ 2)    ; Layout is defined by a horizontal line with faces
(defconstant +cubemap-layout-cross-three-by-four+ 3) ; Layout is defined by a 3x4 cross with cubemap faces
(defconstant +cubemap-layout-cross-four-by-three+ 4) ; Layout is defined by a 4x3 cross with cubemap faces

;;; N-patch layout
(defconstant +npatch-nine-patch+ 0 "Npatch layout: 3x3 tiles")
(defconstant +npatch-three-patch-vertical+ 1 "Npatch layout: 1x3 tiles")
(defconstant +npatch-three-patch-horizontal+ 2 "Npatch layout: 3x1 tiles")

;;; Global texture management
(defvar *texture-id-counter* 1 "OpenGL texture ID counter")
(defvar *current-texture-id* 0 "Currently bound texture ID")
(defvar *texture-registry* (make-hash-table) "Registry of loaded textures")
(defvar *default-texture* nil "Default white 1x1 texture")

;;; NPatch structure for 9-patch drawing
;; NPatchInfo structure is defined in raylib.lisp

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(defconstant +pixelformat-uncompressed-r5g5b5a1-alpha-threshold+ 50
  "Threshold over 255 to set alpha as 0")
(defconstant +gaussian-blur-iterations+ 4
  "Number of box blur iterations to approximate gaussian blur")

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions
;;; NOTE: Image data is a byte vector laid out exactly like raylib image data,
;;; 16bit and 32bit (float) components are stored little-endian
;;;----------------------------------------------------------------------------------
(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun %make-octets (size)
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(declaim (inline %u16 (setf %u16) %f32 (setf %f32) %c-round %image-ready-p))

(defun %u16 (data offset)
  "unsigned short at byte OFFSET"
  (logior (aref data offset) (ash (aref data (1+ offset)) 8)))

(defun (setf %u16) (value data offset)
  (setf (aref data offset) (ldb (byte 8 0) value)
        (aref data (1+ offset)) (ldb (byte 8 8) value))
  value)

(defun %f32 (data offset)
  "float at byte OFFSET"
  (ieee-floats:decode-float32 (logior (aref data offset)
                                      (ash (aref data (+ offset 1)) 8)
                                      (ash (aref data (+ offset 2)) 16)
                                      (ash (aref data (+ offset 3)) 24))))

(defun (setf %f32) (value data offset)
  (let ((bits (ieee-floats:encode-float32 (float value 1.0))))
    (setf (aref data offset) (ldb (byte 8 0) bits)
          (aref data (+ offset 1)) (ldb (byte 8 8) bits)
          (aref data (+ offset 2)) (ldb (byte 8 16) bits)
          (aref data (+ offset 3)) (ldb (byte 8 24) bits)))
  value)

(defun %c-round (x)
  "C round(): halfway cases rounded away from zero"
  (if (minusp x) (- (floor (+ (- x) 0.5))) (floor (+ x 0.5))))

(defun %image-ready-p (image)
  "Security check used by most image functions: image->data, width and height"
  (and image (image-data image) (/= (image-width image) 0) (/= (image-height image) 0)))

(defun %col (color)
  "Color argument as a (r g b a) list"
  (keyword-to-color color))

;; Convert half-float (stored as unsigned short) to float
;; REF: https://stackoverflow.com/questions/1659440/32-bit-to-16-bit-floating-point-conversion/60047308#60047308
(defun %half-to-float (x)
  (let* ((e (ash (logand x #x7c00) -10))                                   ; Exponent
         (m (ash (logand x #x03ff) 13))                                    ; Mantissa
         (v (ash (ieee-floats:encode-float32 (float m 1.0)) -23)))        ; Evil log2 bit hack to count leading zeros in denormalized format
    (ieee-floats:decode-float32
     (logand #xffffffff
             (logior (ash (logand x #x8000) 16)                                       ; sign
                     (if (/= e 0) (logior (ash (+ e 112) 23) m) 0)                     ; normalized
                     (if (and (= e 0) (/= m 0))                                        ; denormalized
                         (logior (ash (- v 37) 23)
                                 (logand (ash m (- 150 v)) #x007fe000))
                         0))))))

;; Convert float to half-float (stored as unsigned short)
(defun %float-to-half (x)
  (let* ((b (logand (+ (ieee-floats:encode-float32 (float x 1.0)) #x00001000) #xffffffff)) ; Round-to-nearest-even
         (e (ash (logand b #x7f800000) -23))                               ; Exponent
         (m (logand b #x007fffff)))                                        ; Mantissa
    (logand #xffff
            (logior (ash (logand b #x80000000) -16)                                            ; sign
                    (if (> e 112) (logior (logand (ash (- e 112) 10) #x7c00) (ash m -13)) 0)   ; normalized
                    (if (and (< e 113) (> e 101))                                              ; denormalized
                        (ash (1+ (ash (+ #x007ff000 m) (- (- 125 e)))) -1)
                        0)
                    (if (> e 143) #x7fff 0)))))                                                ; saturate

(defun %bytes-per-pixel (format)
  (get-pixel-data-size 1 1 format))

(defun %compressed-format-p (format)
  (>= format +pixelformat-compressed-dxt1-rgb+))

(defun %gray (r g b)
  "Grayscale equivalent of 0..255 components (raylib luminance weights)"
  (%u8 (* (+ (* (/ r 255.0) 0.299) (* (/ g 255.0) 0.587) (* (/ b 255.0) 0.114)) 255.0)))

(defun %image-color (data i format)
  "Color of pixel I in DATA, converted like LoadImageColors()/GetImageColor()"
  (alexandria:switch (format)
    (+pixelformat-uncompressed-grayscale+
     (let ((v (aref data i))) (list v v v 255)))
    (+pixelformat-uncompressed-gray-alpha+
     (let ((v (aref data (* i 2)))) (list v v v (aref data (1+ (* i 2))))))
    (+pixelformat-uncompressed-r5g5b5a1+
     (let ((p (%u16 data (* i 2))))
       (list (* (ldb (byte 5 11) p) 8) (* (ldb (byte 5 6) p) 8) (* (ldb (byte 5 1) p) 8)
             (* (logand p 1) 255))))
    (+pixelformat-uncompressed-r5g6b5+
     (let ((p (%u16 data (* i 2))))
       (list (* (ldb (byte 5 11) p) 8) (* (ldb (byte 6 5) p) 4) (* (ldb (byte 5 0) p) 8) 255)))
    (+pixelformat-uncompressed-r4g4b4a4+
     (let ((p (%u16 data (* i 2))))
       (list (* (ldb (byte 4 12) p) 17) (* (ldb (byte 4 8) p) 17) (* (ldb (byte 4 4) p) 17)
             (* (ldb (byte 4 0) p) 17))))
    (+pixelformat-uncompressed-r8g8b8a8+
     (let ((k (* i 4)))
       (list (aref data k) (aref data (+ k 1)) (aref data (+ k 2)) (aref data (+ k 3)))))
    (+pixelformat-uncompressed-r8g8b8+
     (let ((k (* i 3)))
       (list (aref data k) (aref data (+ k 1)) (aref data (+ k 2)) 255)))
    (+pixelformat-uncompressed-r32+
     (list (%u8 (* (%f32 data (* i 4)) 255.0)) 0 0 255))
    (+pixelformat-uncompressed-r32g32b32+
     (let ((k (* i 12)))
       (list (%u8 (* (%f32 data k) 255.0)) (%u8 (* (%f32 data (+ k 4)) 255.0))
             (%u8 (* (%f32 data (+ k 8)) 255.0)) 255)))
    (+pixelformat-uncompressed-r32g32b32a32+
     (let ((k (* i 16)))
       (list (%u8 (* (%f32 data k) 255.0)) (%u8 (* (%f32 data (+ k 4)) 255.0))
             (%u8 (* (%f32 data (+ k 8)) 255.0)) (%u8 (* (%f32 data (+ k 12)) 255.0)))))
    (+pixelformat-uncompressed-r16+
     (list (%u8 (* (%half-to-float (%u16 data (* i 2))) 255.0)) 0 0 255))
    (+pixelformat-uncompressed-r16g16b16+
     (let ((k (* i 6)))
       (list (%u8 (* (%half-to-float (%u16 data k)) 255.0))
             (%u8 (* (%half-to-float (%u16 data (+ k 2))) 255.0))
             (%u8 (* (%half-to-float (%u16 data (+ k 4))) 255.0)) 255)))
    (+pixelformat-uncompressed-r16g16b16a16+
     (let ((k (* i 8)))
       (list (%u8 (* (%half-to-float (%u16 data k)) 255.0))
             (%u8 (* (%half-to-float (%u16 data (+ k 2))) 255.0))
             (%u8 (* (%half-to-float (%u16 data (+ k 4))) 255.0))
             (%u8 (* (%half-to-float (%u16 data (+ k 6))) 255.0)))))
    (t (list 0 0 0 0))))

(defun %load-image-colors (image)
  "LoadImageColors() as a raw R8G8B8A8 byte vector (the C Color array memory layout)"
  (let* ((count (* (image-width image) (image-height image)))
         (format (image-pixel-format image))
         (data (image-data image))
         (pixels (%make-octets (* count 4))))
    (cond ((%compressed-format-p format)
           (trace-log-warning "IMAGE: Pixel data retrieval not supported for compressed image formats"))
          ((= format +pixelformat-uncompressed-r8g8b8a8+)
           (replace pixels data :end2 (* count 4)))
          (t
           (when (member format (list +pixelformat-uncompressed-r32+ +pixelformat-uncompressed-r32g32b32+
                                      +pixelformat-uncompressed-r32g32b32a32+))
             (trace-log-warning "IMAGE: Pixel format converted from 32bit to 8bit per channel"))
           (when (member format (list +pixelformat-uncompressed-r16+ +pixelformat-uncompressed-r16g16b16+
                                      +pixelformat-uncompressed-r16g16b16a16+))
             (trace-log-warning "IMAGE: Pixel format converted from 16bit to 8bit per channel"))
           (dotimes (i count)
             (destructuring-bind (r g b a) (%image-color data i format)
               (let ((k (* i 4)))
                 (setf (aref pixels k) r (aref pixels (+ k 1)) g
                       (aref pixels (+ k 2)) b (aref pixels (+ k 3)) a))))))
    pixels))

(defun %set-image-rgba8 (image pixels)
  "Replace image data with R8G8B8A8 PIXELS, then convert back to the original format"
  (let ((format (image-pixel-format image)))
    (setf (image-data image) pixels
          (image-pixel-format image) +pixelformat-uncompressed-r8g8b8a8+)
    (image-format image format)))

;; Get pixel data from image as Vector4 array (float normalized)
(defun %load-image-data-normalized (image)
  "Normalized RGBA floats of every pixel, 4 consecutive single-floats per pixel"
  (let* ((count (* (image-width image) (image-height image)))
         (pixels (make-array (* count 4) :element-type 'single-float :initial-element 0.0))
         (data (image-data image))
         (format (image-pixel-format image)))
    (if (%compressed-format-p format)
        (trace-log-warning "IMAGE: Pixel data retrieval not supported for compressed image formats")
        (flet ((put (i x y z w)
                 (let ((o (* i 4)))
                   (setf (aref pixels o) (float x 1.0) (aref pixels (+ o 1)) (float y 1.0)
                         (aref pixels (+ o 2)) (float z 1.0) (aref pixels (+ o 3)) (float w 1.0)))))
          (dotimes (i count)
            (alexandria:switch (format)
              (+pixelformat-uncompressed-grayscale+
               (let ((v (/ (aref data i) 255.0))) (put i v v v 1.0)))
              (+pixelformat-uncompressed-gray-alpha+
               (let ((v (/ (aref data (* i 2)) 255.0)))
                 (put i v v v (/ (aref data (1+ (* i 2))) 255.0))))
              (+pixelformat-uncompressed-r5g5b5a1+
               (let ((p (%u16 data (* i 2))))
                 (put i (* (ldb (byte 5 11) p) (/ 1.0 31)) (* (ldb (byte 5 6) p) (/ 1.0 31))
                      (* (ldb (byte 5 1) p) (/ 1.0 31)) (if (zerop (logand p 1)) 0.0 1.0))))
              (+pixelformat-uncompressed-r5g6b5+
               (let ((p (%u16 data (* i 2))))
                 (put i (* (ldb (byte 5 11) p) (/ 1.0 31)) (* (ldb (byte 6 5) p) (/ 1.0 63))
                      (* (ldb (byte 5 0) p) (/ 1.0 31)) 1.0)))
              (+pixelformat-uncompressed-r4g4b4a4+
               (let ((p (%u16 data (* i 2))))
                 (put i (* (ldb (byte 4 12) p) (/ 1.0 15)) (* (ldb (byte 4 8) p) (/ 1.0 15))
                      (* (ldb (byte 4 4) p) (/ 1.0 15)) (* (ldb (byte 4 0) p) (/ 1.0 15)))))
              (+pixelformat-uncompressed-r8g8b8a8+
               (let ((k (* i 4)))
                 (put i (/ (aref data k) 255.0) (/ (aref data (+ k 1)) 255.0)
                      (/ (aref data (+ k 2)) 255.0) (/ (aref data (+ k 3)) 255.0))))
              (+pixelformat-uncompressed-r8g8b8+
               (let ((k (* i 3)))
                 (put i (/ (aref data k) 255.0) (/ (aref data (+ k 1)) 255.0)
                      (/ (aref data (+ k 2)) 255.0) 1.0)))
              (+pixelformat-uncompressed-r32+
               (put i (%f32 data (* i 4)) 0.0 0.0 1.0))
              (+pixelformat-uncompressed-r32g32b32+
               (let ((k (* i 12)))
                 (put i (%f32 data k) (%f32 data (+ k 4)) (%f32 data (+ k 8)) 1.0)))
              (+pixelformat-uncompressed-r32g32b32a32+
               (let ((k (* i 16)))
                 (put i (%f32 data k) (%f32 data (+ k 4)) (%f32 data (+ k 8)) (%f32 data (+ k 12)))))
              (+pixelformat-uncompressed-r16+
               (put i (%half-to-float (%u16 data (* i 2))) 0.0 0.0 1.0))
              (+pixelformat-uncompressed-r16g16b16+
               (let ((k (* i 6)))
                 (put i (%half-to-float (%u16 data k)) (%half-to-float (%u16 data (+ k 2)))
                      (%half-to-float (%u16 data (+ k 4))) 1.0)))
              (+pixelformat-uncompressed-r16g16b16a16+
               (let ((k (* i 8)))
                 (put i (%half-to-float (%u16 data k)) (%half-to-float (%u16 data (+ k 2)))
                      (%half-to-float (%u16 data (+ k 4))) (%half-to-float (%u16 data (+ k 6))))))))))
    pixels))

;;; Port of the stb_perlin.h functions used by GenImagePerlinNoise()

(alexandria:define-constant +perlin-randtab+
  (coerce '(23 125 161 52 103 117 70 37 247 101 203 169 124 126 44 123
    152 238 145 45 171 114 253 10 192 136 4 157 249 30 35 72
    175 63 77 90 181 16 96 111 133 104 75 162 93 56 66 240
    8 50 84 229 49 210 173 239 141 1 87 18 2 198 143 57
    225 160 58 217 168 206 245 204 199 6 73 60 20 230 211 233
    94 200 88 9 74 155 33 15 219 130 226 202 83 236 42 172
    165 218 55 222 46 107 98 154 109 67 196 178 127 158 13 243
    65 79 166 248 25 224 115 80 68 51 184 128 232 208 151 122
    26 212 105 43 179 213 235 148 146 89 14 195 28 78 112 76
    250 47 24 251 140 108 186 190 228 170 183 139 39 188 244 246
    132 48 119 144 180 138 134 193 82 182 120 121 86 220 209 3
    91 241 149 85 205 150 113 216 31 100 41 164 177 214 153 231
    38 71 185 174 97 201 29 95 7 92 54 254 191 118 34 221
    131 11 163 99 234 81 227 147 156 176 17 142 69 12 110 62
    27 255 0 194 59 116 242 252 19 21 187 53 207 129 64 135
    61 40 167 237 102 223 106 159 197 189 215 137 36 32 22 5)
          '(simple-array (unsigned-byte 8) (256)))
  :test #'equalp)

(alexandria:define-constant +perlin-randtab-grad-idx+
  (coerce '(7 9 5 0 11 1 6 9 3 9 11 1 8 10 4 7
    8 6 1 5 3 10 9 10 0 8 4 1 5 2 7 8
    7 11 9 10 1 0 4 7 5 0 11 6 1 4 2 8
    8 10 4 9 9 2 5 7 9 1 7 2 2 6 11 5
    5 4 6 9 0 1 1 0 7 6 9 8 4 10 3 1
    2 8 8 9 10 11 5 11 11 2 6 10 3 4 2 4
    9 10 3 2 6 3 6 10 5 3 4 10 11 2 9 11
    1 11 10 4 9 4 11 0 4 11 4 0 0 0 7 6
    10 4 1 3 11 5 3 4 2 9 1 3 0 1 8 0
    6 7 8 7 0 4 6 10 8 2 3 11 11 8 0 2
    4 8 3 0 0 10 6 1 2 2 4 5 6 0 1 3
    11 9 5 5 9 6 9 8 3 8 1 8 9 6 9 11
    10 7 5 6 5 9 1 3 7 0 2 10 11 2 6 1
    3 11 7 7 2 1 7 3 0 8 1 1 5 0 6 10
    11 11 0 2 7 0 10 8 3 5 7 1 11 1 0 7
    9 0 11 5 10 3 2 3 5 9 7 9 8 4 6 5)
          '(simple-array (unsigned-byte 8) (256)))
  :test #'equalp)

(defun %perlin-fastfloor (a)
  (let ((ai (truncate a)))
    (if (< a ai) (1- ai) ai)))

(defun %perlin-grad (grad-idx x y z)
  (let ((basis #(( 1  1  0) (-1  1  0) ( 1 -1  0) (-1 -1  0)
                 ( 1  0  1) (-1  0  1) ( 1  0 -1) (-1  0 -1)
                 ( 0  1  1) ( 0 -1  1) ( 0  1 -1) ( 0 -1 -1))))
    (destructuring-bind (gx gy gz) (svref basis grad-idx)
      (+ (* gx x) (* gy y) (* gz z)))))

(defun %perlin-noise3-internal (x y z x-wrap y-wrap z-wrap seed)
  (flet ((lerp (a b tt) (+ a (* (- b a) tt)))
         (ease (a) (* (+ (* (- (* a 6) 15) a) 10) a a a))
         (rand (i) (aref +perlin-randtab+ (logand i 255)))
         (grad-idx (i) (aref +perlin-randtab-grad-idx+ (logand i 255))))
    (let* ((x-mask (logand (1- x-wrap) 255))
           (y-mask (logand (1- y-wrap) 255))
           (z-mask (logand (1- z-wrap) 255))
           (px (%perlin-fastfloor x))
           (py (%perlin-fastfloor y))
           (pz (%perlin-fastfloor z))
           (x0 (logand px x-mask)) (x1 (logand (1+ px) x-mask))
           (y0 (logand py y-mask)) (y1 (logand (1+ py) y-mask))
           (z0 (logand pz z-mask)) (z1 (logand (1+ pz) z-mask))
           (x (- x px)) (u (ease x))
           (y (- y py)) (v (ease y))
           (z (- z pz)) (w (ease z))
           (r0 (rand (+ x0 seed)))
           (r1 (rand (+ x1 seed)))
           (r00 (rand (+ r0 y0)))
           (r01 (rand (+ r0 y1)))
           (r10 (rand (+ r1 y0)))
           (r11 (rand (+ r1 y1)))
           (n000 (%perlin-grad (grad-idx (+ r00 z0)) x y z))
           (n001 (%perlin-grad (grad-idx (+ r00 z1)) x y (- z 1)))
           (n010 (%perlin-grad (grad-idx (+ r01 z0)) x (- y 1) z))
           (n011 (%perlin-grad (grad-idx (+ r01 z1)) x (- y 1) (- z 1)))
           (n100 (%perlin-grad (grad-idx (+ r10 z0)) (- x 1) y z))
           (n101 (%perlin-grad (grad-idx (+ r10 z1)) (- x 1) y (- z 1)))
           (n110 (%perlin-grad (grad-idx (+ r11 z0)) (- x 1) (- y 1) z))
           (n111 (%perlin-grad (grad-idx (+ r11 z1)) (- x 1) (- y 1) (- z 1)))
           (n00 (lerp n000 n001 w))
           (n01 (lerp n010 n011 w))
           (n10 (lerp n100 n101 w))
           (n11 (lerp n110 n111 w))
           (n0 (lerp n00 n01 v))
           (n1 (lerp n10 n11 v)))
      (lerp n0 n1 u))))

(defun %perlin-fbm-noise3 (x y z lacunarity gain octaves)
  (let ((frequency 1.0) (amplitude 1.0) (sum 0.0))
    (dotimes (i octaves sum)
      (incf sum (* (%perlin-noise3-internal (* x frequency) (* y frequency) (* z frequency) 0 0 0 (logand i 255))
                   amplitude))
      (setf frequency (* frequency lacunarity)
            amplitude (* amplitude gain)))))

;;; Image resampling used by ImageResize()
;;; NOTE: raylib uses stb_image_resize2 stbir_resize_uint8_linear(); this implements
;;; the same default filters (Catmull-Rom upsampling, Mitchell downsampling, clamped
;;; edges, alpha weighting for RGBA) but output is not guaranteed bit-identical

(defun %stbir-kernel (upsample x)
  (let ((x (abs x)))
    (if upsample
        (cond ((< x 1.0) (- 1.0 (* x x (- 2.5 (* 1.5 x)))))
              ((< x 2.0) (- 2.0 (* x (+ 4.0 (* x (- (* 0.5 x) 2.5))))))
              (t 0.0))
        (cond ((< x 1.0) (/ (+ 16.0 (* x x (- (* 21.0 x) 36.0))) 18.0))
              ((< x 2.0) (/ (+ 32.0 (* x (+ -60.0 (* x (- 36.0 (* 7.0 x)))))) 18.0))
              (t 0.0)))))

(defun %resample-weights (src-size dst-size)
  "For each destination index: (first-source-index . weights vector)"
  (let* ((scale (/ (float dst-size) src-size))
         (upsample (>= scale 1.0))
         (support (if upsample 2.0 (/ 2.0 scale)))
         (result (make-array dst-size)))
    (dotimes (d dst-size result)
      (let* ((center (- (/ (+ d 0.5) scale) 0.5))
             (first (ceiling (- center support)))
             (last (floor (+ center support)))
             (weights (make-array (1+ (- last first)) :element-type 'single-float))
             (sum 0.0))
        (loop for s from first to last
              for k from 0
              do (let ((w (%stbir-kernel upsample (if upsample (- s center) (* (- s center) scale)))))
                   (setf (aref weights k) w)
                   (incf sum w)))
        (unless (zerop sum)
          (dotimes (k (length weights)) (setf (aref weights k) (/ (aref weights k) sum))))
        (setf (aref result d) (cons first weights))))))

(defun %resize-uint8 (src width height channels new-width new-height &optional alpha-weighted)
  "Separable resampling of an interleaved 8-bit image with CHANNELS components"
  (let* ((xw (%resample-weights width new-width))
         (yw (%resample-weights height new-height))
         (tmp (make-array (* new-width height channels) :element-type 'single-float :initial-element 0.0))
         (out (%make-octets (* new-width new-height channels)))
         (alpha (1- channels)))
    (flet ((sample (x y c)
             (let* ((x (max 0 (min (1- width) x)))
                    (y (max 0 (min (1- height) y)))
                    (o (* (+ (* y width) x) channels))
                    (v (float (aref src (+ o c)))))
               (if (and alpha-weighted (/= c alpha))
                   (* v (/ (aref src (+ o alpha)) 255.0))
                   v))))
      ;; Horizontal pass
      (dotimes (y height)
        (dotimes (x new-width)
          (destructuring-bind (first . weights) (aref xw x)
            (dotimes (c channels)
              (let ((acc 0.0))
                (dotimes (k (length weights))
                  (incf acc (* (aref weights k) (sample (+ first k) y c))))
                (setf (aref tmp (+ (* (+ (* y new-width) x) channels) c)) acc))))))
      ;; Vertical pass
      (dotimes (y new-height)
        (destructuring-bind (first . weights) (aref yw y)
          (dotimes (x new-width)
            (let ((values (make-array channels :element-type 'single-float :initial-element 0.0)))
              (dotimes (c channels)
                (dotimes (k (length weights))
                  (let ((sy (max 0 (min (1- height) (+ first k)))))
                    (incf (aref values c) (* (aref weights k) (aref tmp (+ (* (+ (* sy new-width) x) channels) c)))))))
              (let ((a (if alpha-weighted (aref values alpha) 255.0)))
                (dotimes (c channels)
                  (let ((v (if (and alpha-weighted (/= c alpha))
                               (if (> a 0.0) (/ (aref values c) (/ a 255.0)) 0.0)
                               (aref values c))))
                    (setf (aref out (+ (* (+ (* y new-width) x) channels) c))
                          (max 0 (min 255 (floor (+ v 0.5)))))))))))))
    out))

;;;------------------------------------------------------------------------------------
;;; Image file decoders/encoders
;;; NOTE: raylib uses stb_image/stb_image_write/qoi.h; here PNG/JPG/TGA/PNM go through
;;; imago, GIF through skippy, PNG writing through zpng, BMP and QOI are implemented here.
;;; Decoded images keep the component count stb_image would report (1..4 channels)
;;;------------------------------------------------------------------------------------

(defun %u32-le (data offset)
  (logior (aref data offset) (ash (aref data (+ offset 1)) 8)
          (ash (aref data (+ offset 2)) 16) (ash (aref data (+ offset 3)) 24)))

(defun %u32-be (data offset)
  (logior (ash (aref data offset) 24) (ash (aref data (+ offset 1)) 16)
          (ash (aref data (+ offset 2)) 8) (aref data (+ offset 3))))

(defun %s32-le (data offset)
  (let ((u (%u32-le data offset)))
    (if (>= u #x80000000) (- u #x100000000) u)))

(defun %channels->format (channels)
  (case channels
    (1 +pixelformat-uncompressed-grayscale+)
    (2 +pixelformat-uncompressed-gray-alpha+)
    (3 +pixelformat-uncompressed-r8g8b8+)
    (t +pixelformat-uncompressed-r8g8b8a8+)))

(defun %format->channels (format)
  "Channels used by the image exporters, 0 when data needs a Color array conversion"
  (alexandria:switch (format)
    (+pixelformat-uncompressed-grayscale+ 1)
    (+pixelformat-uncompressed-gray-alpha+ 2)
    (+pixelformat-uncompressed-r8g8b8+ 3)
    (+pixelformat-uncompressed-r8g8b8a8+ 4)
    (t 0)))

(defun %rgba->channels (rgba count channels)
  "Pack COUNT R8G8B8A8 pixels into CHANNELS components per pixel (gray uses the red component)"
  (if (= channels 4)
      rgba
      (let ((out (%make-octets (* count channels))))
        (dotimes (i count out)
          (let ((s (* i 4)) (d (* i channels)))
            (case channels
              (1 (setf (aref out d) (aref rgba s)))
              (2 (setf (aref out d) (aref rgba s) (aref out (1+ d)) (aref rgba (+ s 3))))
              (3 (setf (aref out d) (aref rgba s) (aref out (+ d 1)) (aref rgba (+ s 1))
                       (aref out (+ d 2)) (aref rgba (+ s 2))))))))))

(defun %imago->image (imago-image channels)
  "Convert an imago image to an image with CHANNELS components per pixel"
  (let* ((rgb (if (typep imago-image 'imago:rgb-image) imago-image (imago:convert-to-rgb imago-image)))
         (width (imago:image-width rgb))
         (height (imago:image-height rgb))
         (rgba (%make-octets (* width height 4))))
    (dotimes (y height)
      (dotimes (x width)
        (let ((pixel (imago:image-pixel rgb x y)))
          (%put-rgba rgba (+ (* y width) x) (imago:color-red pixel) (imago:color-green pixel)
                     (imago:color-blue pixel) (imago:color-alpha pixel)))))
    (make-image :data (%rgba->channels rgba (* width height) channels) :width width :height height
                :mipmaps 1 :format (%channels->format channels))))

(defun %load-png (data)
  "Decode a PNG file"
  (let ((offset 8)
        width height depth color-type interlace
        palette transparency
        (idat (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    ;; Read chunks
    (loop while (< (+ offset 8) (length data))
          do (let* ((len (%u32-be data offset))
                    (type (map 'string #'code-char (subseq data (+ offset 4) (+ offset 8))))
                    (start (+ offset 8)))
               (cond ((string= type "IHDR")
                      (setf width (%u32-be data start)
                            height (%u32-be data (+ start 4))
                            depth (aref data (+ start 8))
                            color-type (aref data (+ start 9))
                            interlace (aref data (+ start 12))))
                     ((string= type "PLTE") (setf palette (subseq data start (+ start len))))
                     ((string= type "tRNS") (setf transparency (subseq data start (+ start len))))
                     ((string= type "IDAT") (loop for k from start below (+ start len)
                                                  do (vector-push-extend (aref data k) idat)))
                     ((string= type "IEND") (loop-finish)))
               (incf offset (+ len 12))))
    (let* ((img-n (case color-type (0 1) (2 3) (3 1) (4 2) (6 4)))
           (raw (chipz:decompress nil 'chipz:zlib (coerce idat '(simple-array (unsigned-byte 8) (*)))))
           (samples (make-array (* width height img-n) :element-type '(unsigned-byte 16)))
           (filter-bpp (max 1 (floor (* img-n depth) 8)))
           (pos 0))
      ;; Unfilter one (sub)image and store its samples in the full image
      (flet ((decode-pass (pass-w pass-h x0 y0 dx dy)
               (when (and (> pass-w 0) (> pass-h 0))
                 (let* ((stride (ceiling (* pass-w img-n depth) 8))
                        (prev (%make-octets stride))
                        (cur (%make-octets stride)))
                   (dotimes (row pass-h)
                     (let ((filter (aref raw pos)))
                       (incf pos)
                       (dotimes (k stride)
                         (let* ((x (aref raw (+ pos k)))
                                (a (if (>= k filter-bpp) (aref cur (- k filter-bpp)) 0))
                                (b (aref prev k))
                                (c (if (>= k filter-bpp) (aref prev (- k filter-bpp)) 0)))
                           (setf (aref cur k)
                                 (logand #xff
                                         (+ x (case filter
                                                (0 0)
                                                (1 a)
                                                (2 b)
                                                (3 (floor (+ a b) 2))
                                                (4 (let* ((p (- (+ a b) c))
                                                          (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
                                                     (cond ((and (<= pa pb) (<= pa pc)) a)
                                                           ((<= pb pc) b)
                                                           (t c))))
                                                (t 0)))))))
                       (incf pos stride)
                       ;; Unpack samples
                       (dotimes (px pass-w)
                         (dotimes (ch img-n)
                           (let* ((sample-index (+ (* px img-n) ch))
                                  (value (case depth
                                           (8 (aref cur sample-index))
                                           (16 (logior (ash (aref cur (* sample-index 2)) 8)
                                                       (aref cur (1+ (* sample-index 2)))))
                                           (t (let ((bit (* sample-index depth)))
                                                (ldb (byte depth (- 8 depth (mod bit 8)))
                                                     (aref cur (floor bit 8))))))))
                             (setf (aref samples (+ (* (+ (* (+ y0 (* row dy)) width) (+ x0 (* px dx))) img-n) ch))
                                   value))))
                       (rotatef prev cur)))))))
        (if (= interlace 1)
            ;; Adam7 interlacing
            (loop for (x0 y0 dx dy) in '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))
                  do (decode-pass (ceiling (- width x0) dx) (ceiling (- height y0) dy) x0 y0 dx dy))
            (decode-pass width height 0 0 1 1)))
      ;; Convert samples to 8 bit output
      (let* ((key (when (and transparency (member color-type '(0 2)))
                    (loop for k from 0 below (* 2 img-n) by 2
                          collect (logand (logior (ash (aref transparency k) 8) (aref transparency (1+ k)))
                                          (1- (ash 1 depth))))))
             (out-n (cond ((= color-type 3) (if transparency 4 3))
                          (key (1+ img-n))
                          (t img-n)))
             (count (* width height))
             (out (%make-octets (* count out-n)))
             (scale (if (= color-type 3) 1 (case depth (1 255) (2 85) (4 17) (t 1)))))
        (dotimes (i count)
          (let ((o (* i out-n)))
            (if (= color-type 3)
                (let ((index (aref samples i)))
                  (dotimes (c 3)
                    (setf (aref out (+ o c)) (if (< (+ (* index 3) c) (length palette))
                                                 (aref palette (+ (* index 3) c)) 0)))
                  (when transparency
                    (setf (aref out (+ o 3)) (if (< index (length transparency)) (aref transparency index) 255))))
                (progn
                  (dotimes (c img-n)
                    (let ((v (aref samples (+ (* i img-n) c))))
                      (setf (aref out (+ o c)) (if (= depth 16) (ash v -8) (* v scale)))))
                  (when key
                    (setf (aref out (+ o img-n))
                          (if (loop for c from 0 below img-n
                                    always (= (aref samples (+ (* i img-n) c)) (nth c key)))
                              0 255)))))))
        (make-image :data out :width width :height height :mipmaps 1 :format (%channels->format out-n))))))

(defun %read-imago-from-memory (reader file-data)
  "Decode FILE-DATA with an imago stream reader
   NOTE: imago readers require a file stream (they use file-length), so data goes through a temporary file"
  (uiop:with-temporary-file (:stream out :pathname path :element-type '(unsigned-byte 8))
    (write-sequence file-data out)
    (finish-output out)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (funcall reader in))))

(defun %load-bmp (data)
  "Decode an uncompressed BMP file (1/4/8 bit paletted, 16/24/32 bit)"
  (let* ((pixel-offset (%u32-le data 10))
         (header-size (%u32-le data 14))
         (os2 (= header-size 12))
         (width (if os2 (logior (aref data 18) (ash (aref data 19) 8)) (%s32-le data 18)))
         (raw-height (if os2 (logior (aref data 20) (ash (aref data 21) 8)) (%s32-le data 22)))
         (bpp (if os2 (logior (aref data 24) (ash (aref data 25) 8)) (logior (aref data 28) (ash (aref data 29) 8))))
         (compression (if os2 0 (%u32-le data 30)))
         (flip (> raw-height 0))
         (height (abs raw-height))
         (mr 0) (mg 0) (mb 0) (ma 0))
    (unless (member compression '(0 3))
      (trace-log-warning "IMAGE: BMP compressed data not supported")
      (return-from %load-bmp nil))
    ;; Channel masks (stb_image defaults when not provided)
    (case bpp
      (16 (setf mr #x7c00 mg #x03e0 mb #x001f))
      (32 (setf mr #x00ff0000 mg #x0000ff00 mb #x000000ff ma #xff000000)))
    (when (= compression 3)
      (setf mr (%u32-le data 54) mg (%u32-le data 58) mb (%u32-le data 62)
            ma (if (>= header-size 56) (%u32-le data 66) 0)))
    (let* ((channels (if (and (> bpp 8) (/= ma 0)) 4 3))
           (palette-offset (+ 14 header-size))
           (palette-entry (if os2 3 4))
           (stride (* 4 (ceiling (* width bpp) 32)))
           (rgba (%make-octets (* width height 4)))
           (all-alpha-zero t))
      (flet ((channel (value mask)
               ;; Extract the masked bits and scale them to 8 bits
               (if (zerop mask) 0
                   (let* ((shift (loop for s from 0 below 32 until (logbitp s mask) finally (return s)))
                          (bits (integer-length (ash mask (- shift))))
                          (v (ldb (byte bits shift) value)))
                     (if (>= bits 8) (ash v (- 8 bits)) (floor (* v 255) (1- (ash 1 bits))))))))
        (dotimes (row height)
          (let ((src (+ pixel-offset (* (if flip (- height 1 row) row) stride))))
            (dotimes (x width)
              (let ((i (+ (* row width) x)))
                (if (<= bpp 8)
                    (let* ((bit-offset (* x bpp))
                           (byte (aref data (+ src (floor bit-offset 8))))
                           (index (ldb (byte bpp (- 8 bpp (mod bit-offset 8))) byte))
                           (p (+ palette-offset (* index palette-entry))))
                      (%put-rgba rgba i (aref data (+ p 2)) (aref data (+ p 1)) (aref data p) 255))
                    (let* ((bytes (floor bpp 8))
                           (o (+ src (* x bytes)))
                           (value (loop for k below bytes sum (ash (aref data (+ o k)) (* 8 k)))))
                      (if (= bpp 24)
                          (%put-rgba rgba i (aref data (+ o 2)) (aref data (+ o 1)) (aref data o) 255)
                          (let ((a (if (zerop ma) 255 (channel value ma))))
                            (unless (zerop a) (setf all-alpha-zero nil))
                            (%put-rgba rgba i (channel value mr) (channel value mg) (channel value mb) a)))))))))
        ;; If all alpha values are zero the alpha channel is ignored (stb_image behaviour)
        (when (and (= channels 4) all-alpha-zero)
          (loop for k from 3 below (length rgba) by 4 do (setf (aref rgba k) 255))))
      (make-image :data (%rgba->channels rgba (* width height) channels) :width width :height height
                  :mipmaps 1 :format (%channels->format channels)))))

(defun %qoi-hash (r g b a)
  (mod (+ (* r 3) (* g 5) (* b 7) (* a 11)) 64))

(defun %load-qoi (data)
  "Decode a QOI file (https://qoiformat.org)"
  (unless (and (>= (length data) 22) (string= (map 'string #'code-char (subseq data 0 4)) "qoif"))
    (return-from %load-qoi nil))
  (let* ((width (%u32-be data 4))
         (height (%u32-be data 8))
         (channels (aref data 12))
         (pixels (%make-octets (* width height channels)))
         (index (make-array 64 :initial-element nil))
         (r 0) (g 0) (b 0) (a 255)
         (run 0)
         (p 14)
         (chunks-len (- (length data) 8)))
    (dotimes (i (* width height))
      (cond ((> run 0) (decf run))
            ((< p chunks-len)
             (let ((b1 (aref data p)))
               (incf p)
               (cond ((= b1 #xfe)                         ; QOI_OP_RGB
                      (setf r (aref data p) g (aref data (+ p 1)) b (aref data (+ p 2)))
                      (incf p 3))
                     ((= b1 #xff)                         ; QOI_OP_RGBA
                      (setf r (aref data p) g (aref data (+ p 1)) b (aref data (+ p 2)) a (aref data (+ p 3)))
                      (incf p 4))
                     ((= (logand b1 #xc0) #x00)           ; QOI_OP_INDEX
                      (destructuring-bind (ir ig ib ia) (or (aref index b1) '(0 0 0 0))
                        (setf r ir g ig b ib a ia)))
                     ((= (logand b1 #xc0) #x40)           ; QOI_OP_DIFF
                      (setf r (logand (+ r (- (ldb (byte 2 4) b1) 2)) #xff)
                            g (logand (+ g (- (ldb (byte 2 2) b1) 2)) #xff)
                            b (logand (+ b (- (ldb (byte 2 0) b1) 2)) #xff)))
                     ((= (logand b1 #xc0) #x80)           ; QOI_OP_LUMA
                      (let* ((b2 (aref data p))
                             (vg (- (logand b1 #x3f) 32)))
                        (incf p)
                        (setf r (logand (+ r vg -8 (ldb (byte 4 4) b2)) #xff)
                              g (logand (+ g vg) #xff)
                              b (logand (+ b vg -8 (ldb (byte 4 0) b2)) #xff))))
                     (t (setf run (logand b1 #x3f))))     ; QOI_OP_RUN
               (setf (aref index (%qoi-hash r g b a)) (list r g b a)))))
      (let ((o (* i channels)))
        (setf (aref pixels o) r (aref pixels (+ o 1)) g (aref pixels (+ o 2)) b)
        (when (= channels 4) (setf (aref pixels (+ o 3)) a))))
    (make-image :data pixels :width width :height height :mipmaps 1
                :format (if (= channels 4) +pixelformat-uncompressed-r8g8b8a8+ +pixelformat-uncompressed-r8g8b8+))))

(defun %encode-qoi (data width height channels)
  "Encode pixel DATA (3 or 4 channels) as a QOI file"
  (let ((out (make-array (+ 22 (* width height (1+ channels))) :element-type '(unsigned-byte 8)
                                                               :fill-pointer 0 :adjustable t))
        (index (make-array 64 :initial-element nil))
        (pr 0) (pg 0) (pb 0) (pa 255)
        (run 0)
        (count (* width height)))
    (flet ((put (&rest bytes) (dolist (b bytes) (vector-push-extend b out)))
           (put32 (v) (dotimes (k 4) (vector-push-extend (ldb (byte 8 (- 24 (* k 8))) v) out))))
      (put (char-code #\q) (char-code #\o) (char-code #\i) (char-code #\f))
      (put32 width) (put32 height)
      (put channels 0)                  ; QOI_SRGB
      (dotimes (i count)
        (let* ((o (* i channels))
               (r (aref data o)) (g (aref data (+ o 1))) (b (aref data (+ o 2)))
               (a (if (= channels 4) (aref data (+ o 3)) pa)))
          (if (and (= r pr) (= g pg) (= b pb) (= a pa))
              (progn
                (incf run)
                (when (or (= run 62) (= i (1- count)))
                  (put (logior #xc0 (1- run)))
                  (setf run 0)))
              (let ((h (%qoi-hash r g b a)))
                (when (> run 0)
                  (put (logior #xc0 (1- run)))
                  (setf run 0))
                (if (equal (aref index h) (list r g b a))
                    (put h)
                    (progn
                      (setf (aref index h) (list r g b a))
                      (if (= a pa)
                          (let* ((vr (- (logand (- r pr) #xff) (if (> (logand (- r pr) #xff) 127) 256 0)))
                                 (vg (- (logand (- g pg) #xff) (if (> (logand (- g pg) #xff) 127) 256 0)))
                                 (vb (- (logand (- b pb) #xff) (if (> (logand (- b pb) #xff) 127) 256 0)))
                                 (vg-r (- vr vg))
                                 (vg-b (- vb vg)))
                            (cond ((and (< -3 vr 2) (< -3 vg 2) (< -3 vb 2))
                                   (put (logior #x40 (ash (+ vr 2) 4) (ash (+ vg 2) 2) (+ vb 2))))
                                  ((and (< -9 vg-r 8) (< -33 vg 32) (< -9 vg-b 8))
                                   (put (logior #x80 (+ vg 32)) (logior (ash (+ vg-r 8) 4) (+ vg-b 8))))
                                  (t (put #xfe r g b))))
                          (put #xff r g b a))))))
          (setf pr r pg g pb b pa a)))
      (put 0 0 0 0 0 0 0 1))           ; Padding
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun %gif-frames (data &optional (max-frames most-positive-fixnum))
  "Decode GIF frames composited like stb_image (R8G8B8A8), returns (values frames width height)"
  (let* ((stream (flexi-streams:with-input-from-sequence (in data) (skippy:read-data-stream in)))
         (width (skippy:width stream))
         (height (skippy:height stream))
         (canvas (%make-octets (* width height 4)))
         (frames nil))
    (loop for gif-image across (skippy:images stream)
          for n from 0 below max-frames
          do (let* ((table (or (skippy:color-table gif-image) (skippy:color-table stream)))
                    (transparent (skippy:transparency-index gif-image))
                    (left (skippy:left-position gif-image))
                    (top (skippy:top-position gif-image))
                    (w (skippy:width gif-image))
                    (h (skippy:height gif-image))
                    (pixels (skippy:image-data gif-image))
                    (previous (when (eq (skippy:disposal-method gif-image) :restore-previous)
                                (copy-seq canvas))))
               (dotimes (y h)
                 (dotimes (x w)
                   (let ((index (aref pixels (+ (* y w) x)))
                         (cx (+ left x)) (cy (+ top y)))
                     (when (and (< cx width) (< cy height) (not (eql index transparent)))
                       (multiple-value-bind (r g b) (skippy:color-rgb (skippy:color-table-entry table index))
                         (%put-rgba canvas (+ (* cy width) cx) r g b 255))))))
               (push (copy-seq canvas) frames)
               ;; Frame disposal
               (case (skippy:disposal-method gif-image)
                 (:restore-background
                  (dotimes (y h)
                    (dotimes (x w)
                      (let ((cx (+ left x)) (cy (+ top y)))
                        (when (and (< cx width) (< cy height))
                          (%put-rgba canvas (+ (* cy width) cx) 0 0 0 0))))))
                 (:restore-previous (setf canvas previous)))))
    (values (nreverse frames) width height)))

(defun %load-gif (data)
  (multiple-value-bind (frames width height) (%gif-frames data 1)
    (when frames
      (make-image :data (first frames) :width width :height height :mipmaps 1
                  :format +pixelformat-uncompressed-r8g8b8a8+))))

(defun %file-type-p (file-type &rest extensions)
  (and file-type (member file-type extensions :test #'string-equal)))

;;;------------------------------------------------------------------------------------
;;; Image loading functions
;;;------------------------------------------------------------------------------------

(defun load-image (file-name)
  "Load image from file into CPU memory (RAM)"
  (multiple-value-bind (file-data data-size) (load-file-data file-name)
    (if file-data
        (load-image-from-memory (get-file-extension file-name) file-data data-size)
        (make-image :width 0 :height 0 :mipmaps 0 :format 0))))

(defun load-image-raw (file-name width height format header-size)
  "Load image from RAW file data"
  (let ((image (make-image :width 0 :height 0 :mipmaps 0 :format 0)))
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (when file-data
        (let ((size (get-pixel-data-size width height format))
              (offset 0))
          (when (<= size data-size)       ; Security check
            ;; Offset file data to expected raw image by header size
            (when (and (> header-size 0) (<= (+ header-size size) data-size))
              (setf offset header-size))
            (setf image (make-image :data (subseq file-data offset (+ offset size))
                                    :width width :height height :mipmaps 1 :format format))))))
    image))

;; NOTE: Image data includes all frames: [image#0][image#1][image#2][...], all frames in RGBA,
;; frames delay data is discarded; returns the image and the number of frames
(defun load-image-anim (file-name)
  "Load image sequence from file (frames appended to image.data)"
  (if (is-file-extension file-name ".gif")
      (multiple-value-bind (file-data data-size) (load-file-data file-name)
        (if file-data
            (load-image-anim-from-memory ".gif" file-data data-size)
            (values (make-image :width 0 :height 0 :mipmaps 0 :format 0) 0)))
      (values (load-image file-name) 1)))

(defun load-image-anim-from-memory (file-type file-data data-size)
  "Load image sequence from memory buffer, returns the image and the number of frames"
  (when (or (null file-type) (null file-data) (zerop data-size)) ; Security check
    (return-from load-image-anim-from-memory (values (make-image :width 0 :height 0 :mipmaps 0 :format 0) 0)))
  (if (%file-type-p file-type ".gif")
      (multiple-value-bind (frames width height) (%gif-frames file-data)
        (let ((data (%make-octets (* width height 4 (length frames)))))
          (loop for frame in frames
                for offset from 0 by (* width height 4)
                do (replace data frame :start1 offset))
          (values (make-image :data data :width width :height height :mipmaps 1
                              :format +pixelformat-uncompressed-r8g8b8a8+)
                  (length frames))))
      (values (load-image-from-memory file-type file-data data-size) 1)))

;; WARNING: File extension must be provided in lower-case (other cases also accepted here)
(defun load-image-from-memory (file-type file-data data-size)
  "Load image from memory buffer, fileType refers to extension: i.e. '.png'"
  (let ((image nil))
    ;; Security checks for input data
    (when (or (null file-data) (zerop data-size))
      (trace-log-warning "IMAGE: Invalid file data")
      (return-from load-image-from-memory (make-image :width 0 :height 0 :mipmaps 0 :format 0)))
    (when (null file-type)
      (trace-log-warning "IMAGE: Missing file extension")
      (return-from load-image-from-memory (make-image :width 0 :height 0 :mipmaps 0 :format 0)))
    (let ((file-data (coerce file-data '(simple-array (unsigned-byte 8) (*)))))
      (handler-case
          (cond ((%file-type-p file-type ".png")
                 (setf image (%load-png file-data)))
                ((%file-type-p file-type ".bmp")
                 (setf image (%load-bmp file-data)))
                ((%file-type-p file-type ".gif")
                 (setf image (%load-gif file-data)))
                ((%file-type-p file-type ".qoi")
                 (setf image (%load-qoi file-data)))
                ;; NOTE: Formats disabled by default in raylib config.h, supported through imago
                ((%file-type-p file-type ".jpg" ".jpeg")
                 (let ((im (%read-imago-from-memory #'imago:read-jpg-from-stream file-data)))
                   (setf image (%imago->image im (if (typep im 'imago:grayscale-image) 1 3)))))
                ((%file-type-p file-type ".tga")
                 (setf image (%imago->image (%read-imago-from-memory #'imago:read-tga-from-stream file-data)
                                            (if (= (aref file-data 16) 32) 4 3))))
                ((%file-type-p file-type ".ppm" ".pgm")
                 (setf image (%imago->image (%read-imago-from-memory #'imago:read-pnm-from-stream file-data)
                                            (if (%file-type-p file-type ".pgm") 1 3))))
                (t (trace-log-warning "IMAGE: Data format not supported")))
        (error (e)
          (trace-log-warning "IMAGE: Failed to decode image data: ~a" e)
          (setf image nil))))
    (if (and image (image-data image))
        (progn
          (trace-log-info "IMAGE: Data loaded successfully (~dx~d | ~a | ~d mipmaps)"
                          (image-width image) (image-height image)
                          (rl-get-pixel-format-name (image-pixel-format image)) (image-mipmap-count image))
          image)
        (progn
          (trace-log-warning "IMAGE: Failed to load image data")
          (make-image :width 0 :height 0 :mipmaps 0 :format 0)))))

;; NOTE: Compressed texture formats not supported
(defun load-image-from-texture (texture)
  "Load image from GPU texture data"
  (if (< (texture-format texture) +pixelformat-compressed-dxt1-rgb+)
      (let ((image (get-texture-data texture)))
        (if image
            (progn (trace-log-info "TEXTURE: [ID ~d] Pixel data retrieved successfully" (texture-id texture))
                   image)
            (progn (trace-log-warning "TEXTURE: [ID ~d] Failed to retrieve pixel data" (texture-id texture))
                   (make-image :width 0 :height 0 :mipmaps 0 :format 0))))
      (progn (trace-log-warning "TEXTURE: [ID ~d] Failed to retrieve compressed pixel data" (texture-id texture))
             (make-image :width 0 :height 0 :mipmaps 0 :format 0))))

(defun load-image-from-screen ()
  "Load image from screen buffer and (screenshot)"
  (let* ((width (get-render-width))
         (height (get-render-height))
         (data (%make-octets (* width height 4))))
    ;; NOTE: glReadPixels() returns image flipped vertically -> (0,0) is the bottom left corner of the framebuffer
    (gl:read-pixels 0 0 width height :rgba :unsigned-byte data)
    (let ((flipped (%make-octets (* width height 4)))
          (row (* width 4)))
      (dotimes (y height)
        (replace flipped data :start1 (* y row) :start2 (* (- height 1 y) row) :end2 (* (- height y) row)))
      ;; NOTE: Alpha value has already been applied to RGB in framebuffer, not needed anymore
      (loop for k from 3 below (length flipped) by 4 do (setf (aref flipped k) 255))
      (make-image :data flipped :width width :height height :mipmaps 1
                  :format +pixelformat-uncompressed-r8g8b8a8+))))

(defun unload-image (image)
  "Unload image from CPU memory (RAM)"
  (when (and image (image-p image))
    ;; NOTE: Memory is managed by the GC, data reference is just cleared
    (setf (image-data image) nil))
  nil)

;;; Image export

(defun %image-export-data (image)
  "Pixel data and channels used to export IMAGE (Color array for non 8-bit formats)"
  (let ((channels (%format->channels (image-pixel-format image))))
    (if (zerop channels)
        (values (%load-image-colors image) 4)
        (values (image-data image) channels))))

(defun %encode-png (data width height channels)
  (let ((png (make-instance 'zpng:png :width width :height height
                                      :color-type (ecase channels
                                                    (1 :grayscale) (2 :grayscale-alpha)
                                                    (3 :truecolor) (4 :truecolor-alpha))
                                      :image-data (subseq data 0 (* width height channels)))))
    (flexi-streams:with-output-to-sequence (out)
      (zpng:write-png-stream png out))))

(defun %encode-bmp (data width height channels)
  "Encode a BMP like stbi_write_bmp(): 24bpp, or 32bpp BGRA with V4 header for 4 channels"
  (let* ((bpp (if (= channels 4) 32 24))
         (header-size (if (= channels 4) 108 40))
         (stride (* 4 (ceiling (* width bpp) 32)))
         (pixel-offset (+ 14 header-size))
         (file-size (+ pixel-offset (* stride height)))
         (out (%make-octets file-size)))
    (flet ((put16 (offset v) (setf (aref out offset) (ldb (byte 8 0) v) (aref out (1+ offset)) (ldb (byte 8 8) v)))
           (put32 (offset v) (dotimes (k 4) (setf (aref out (+ offset k)) (ldb (byte 8 (* k 8)) v)))))
      (setf (aref out 0) (char-code #\B) (aref out 1) (char-code #\M))
      (put32 2 file-size) (put32 10 pixel-offset)
      (put32 14 header-size) (put32 18 width) (put32 22 height)
      (put16 26 1) (put16 28 bpp)
      (put32 30 (if (= channels 4) 3 0))           ; BI_BITFIELDS for alpha bitmaps
      (put32 34 (* stride height))
      (when (= channels 4)
        (put32 54 #x00ff0000) (put32 58 #x0000ff00) (put32 62 #x000000ff) (put32 66 #xff000000)
        (put32 70 #x73524742))                     ; 'sRGB' color space
      (dotimes (row height)
        (let ((y (- height 1 row)))                ; Bottom-up rows
          (dotimes (x width)
            (let* ((s (* (+ (* y width) x) channels))
                   (d (+ pixel-offset (* row stride) (* x (floor bpp 8))))
                   (r (aref data s))
                   (g (if (< channels 3) r (aref data (+ s 1))))
                   (b (if (< channels 3) r (aref data (+ s 2)))))
              (setf (aref out d) b (aref out (+ d 1)) g (aref out (+ d 2)) r)
              (when (= channels 4) (setf (aref out (+ d 3)) (aref data (+ s 3)))))))))
    out))

;; NOTE: File format depends on fileName extension
(defun export-image (image file-name)
  "Export image data to file, returns true on success"
  (let ((result nil))
    (when (or (zerop (image-width image)) (zerop (image-height image)) (null (image-data image)))
      (return-from export-image nil))    ; Security check
    (multiple-value-bind (data channels) (%image-export-data image)
      (let ((w (image-width image)) (h (image-height image)))
        (handler-case
            (cond ((is-file-extension file-name ".png")
                   (let ((file-data (%encode-png data w h channels)))
                     (setf result (save-file-data file-name file-data (length file-data)))))
                  ((is-file-extension file-name ".bmp")
                   (let ((file-data (%encode-bmp data w h channels)))
                     (setf result (save-file-data file-name file-data (length file-data)))))
                  ((is-file-extension file-name ".qoi")
                   (let ((qoi-channels (alexandria:switch ((image-pixel-format image))
                                         (+pixelformat-uncompressed-r8g8b8+ 3)
                                         (+pixelformat-uncompressed-r8g8b8a8+ 4)
                                         (t (trace-log-warning "IMAGE: Image pixel format must be R8G8B8 or R8G8B8A8")
                                            0))))
                     (when (member qoi-channels '(3 4))
                       (let ((file-data (%encode-qoi (image-data image) w h qoi-channels)))
                         (setf result (save-file-data file-name file-data (length file-data)))))))
                  ;; NOTE: JPG export is disabled by default in raylib config.h, supported through imago
                  ((or (is-file-extension file-name ".jpg") (is-file-extension file-name ".jpeg"))
                   (let ((rgb (imago:make-rgb-image w h)))
                     (dotimes (y h)
                       (dotimes (x w)
                         (let ((s (* (+ (* y w) x) channels)))
                           (setf (imago:image-pixel rgb x y)
                                 (if (< channels 3)
                                     (imago:make-color (aref data s) (aref data s) (aref data s))
                                     (imago:make-color (aref data s) (aref data (+ s 1)) (aref data (+ s 2))))))))
                     (imago:write-jpg rgb file-name)
                     (setf result t)))
                  ((is-file-extension file-name ".raw")
                   ;; Export raw pixel data (without header)
                   ;; NOTE: It's up to the user to track image parameters
                   (setf result (save-file-data file-name (image-data image)
                                                (get-pixel-data-size w h (image-pixel-format image)))))
                  (t (trace-log-warning "IMAGE: Export image format requested not supported")))
          (error (e)
            (trace-log-warning "IMAGE: Failed to encode image: ~a" e)
            (setf result nil)))))
    (if result
        (trace-log-info "FILEIO: [~a] Image exported successfully" file-name)
        (trace-log-warning "FILEIO: [~a] Failed to export image" file-name))
    result))

;; NOTE: Returns the file data and its size
(defun export-image-to-memory (image file-type)
  "Export image to memory buffer"
  (when (or (zerop (image-width image)) (zerop (image-height image)) (null (image-data image)))
    (return-from export-image-to-memory (values nil 0))) ; Security check
  (let ((channels (%format->channels (image-pixel-format image))))
    (if (and (%file-type-p file-type ".png") (> channels 0))
        (let ((file-data (%encode-png (image-data image) (image-width image) (image-height image) channels)))
          (values file-data (length file-data)))
        (values nil 0))))

(defun export-image-as-code (image file-name)
  "Export image as code file defining an array of bytes, returns true on success"
  (let* ((text-bytes-per-line 20)
         (data-size (get-pixel-data-size (image-width image) (image-height image) (image-pixel-format image)))
         ;; Get file name from path and convert variable name to uppercase
         (var-file-name (string-upcase (get-file-name-without-ext file-name)))
         (data (image-data image))
         (text
           (with-output-to-string (s)
             (format s "////////////////////////////////////////////////////////////////////////////////////////~%")
             (format s "//                                                                                    //~%")
             (format s "// ImageAsCode exporter v1.0 - Image pixel data exported as an array of bytes         //~%")
             (format s "//                                                                                    //~%")
             (format s "// more info and bugs-report:  github.com/raysan5/raylib                              //~%")
             (format s "// feedback and support:       ray[at]raylib.com                                      //~%")
             (format s "//                                                                                    //~%")
             (format s "// Copyright (c) 2018-2026 Ramon Santamaria (@raysan5)                                //~%")
             (format s "//                                                                                    //~%")
             (format s "////////////////////////////////////////////////////////////////////////////////////////~%~%")
             ;; Add image information
             (format s "// Image data information~%")
             (format s "#define ~a_WIDTH    ~d~%" var-file-name (image-width image))
             (format s "#define ~a_HEIGHT   ~d~%" var-file-name (image-height image))
             (format s "#define ~a_FORMAT   ~d          // raylib internal pixel format~%~%"
                     var-file-name (image-pixel-format image))
             (format s "static unsigned char ~a_DATA[~d] = { " var-file-name data-size)
             (dotimes (i (1- data-size))
               (format s (if (zerop (mod i text-bytes-per-line)) "0x~(~x~),~%" "0x~(~x~), ") (aref data i)))
             (format s "0x~(~x~) };~%" (aref data (1- data-size)))))
         (result (save-file-text file-name text)))
    (if result
        (trace-log-info "FILEIO: [~a] Image as code exported successfully" file-name)
        (trace-log-warning "FILEIO: [~a] Failed to export image as code" file-name))
    result))

(defun is-image-valid (image)
  "Check if an image is valid (data and parameters)"
  (and image
       (image-data image)                ; Validate pixel data available
       (> (image-width image) 0)         ; Validate image width
       (> (image-height image) 0)        ; Validate image height
       (> (image-pixel-format image) 0)  ; Validate image format
       (> (image-mipmap-count image) 0)       ; Validate image mipmaps (at least 1 for basic mipmap level)
       t))

;;;------------------------------------------------------------------------------------
;;; Image generation functions
;;;------------------------------------------------------------------------------------

(defun %make-rgba8-image (width height pixels)
  (make-image :data pixels :width width :height height :mipmaps 1
              :format +pixelformat-uncompressed-r8g8b8a8+))

(defun %put-rgba (pixels index r g b a)
  (let ((k (* index 4)))
    (setf (aref pixels k) r (aref pixels (+ k 1)) g (aref pixels (+ k 2)) b (aref pixels (+ k 3)) a)))

(defun gen-image-color (width height color)
  "Generate image: plain color"
  (destructuring-bind (r g b a) (%col color)
    (let ((pixels (%make-octets (get-pixel-data-size width height +pixelformat-uncompressed-r8g8b8a8+))))
      (dotimes (i (* width height)) (%put-rgba pixels i r g b a))
      (%make-rgba8-image width height pixels))))

(defun %blend-channels (start end factor)
  "(int)((float)end*factor + (float)start*(1.0f - factor)) for every channel"
  (mapcar (lambda (s e) (%u8 (+ (* e factor) (* s (- 1.0 factor))))) start end))

;; The direction value specifies the direction of the gradient (in degrees)
;; with 0 being vertical (from top to bottom), 90 being horizontal (from left to right)
;; The gradient effectively rotates counter-clockwise by the specified amount
(defun gen-image-gradient-linear (width height direction start end)
  "Generate image: linear gradient"
  (let* ((start (%col start)) (end (%col end))
         (pixels (%make-octets (* width height 4)))
         (radian-direction (* (/ (float (- 90 direction)) 180.0) 3.14159))
         (cos-dir (cos radian-direction))
         (sin-dir (sin radian-direction))
         ;; Calculate how far the top-left pixel is along the gradient direction from the center of said gradient
         (starting-pos (- 0.5 (/ (* cos-dir width) 2) (/ (* sin-dir height) 2)))
         ;; With directions that lie in the first or third quadrant (i.e. from top-left to
         ;; bottom-right or vice-versa), pixel (0, 0) is the farthest point on the gradient
         ;; (i.e. the pixel which should become one of the gradient's ends color); while for
         ;; directions that lie in the second or fourth quadrant, that point is pixel (width, 0)
         (max-pos-value (if (eq (minusp (float-sign sin-dir)) (minusp (float-sign cos-dir)))
                            (abs starting-pos)
                            (abs (+ starting-pos (* width cos-dir))))))
    (dotimes (i width)
      (dotimes (j height)
        ;; Calculate the relative position of the pixel along the gradient direction
        (let* ((pos (if (zerop max-pos-value) 0.0
                        (/ (+ starting-pos (+ (* i cos-dir) (* j sin-dir))) max-pos-value)))
               (factor (+ (/ (max -1.0 (min 1.0 pos)) 2.0) 0.5)))
          ;; Generate the color for this pixel
          (destructuring-bind (r g b a) (%blend-channels start end factor)
            (%put-rgba pixels (+ (* j width) i) r g b a)))))
    (%make-rgba8-image width height pixels)))

(defun %ratio-factor (numerator denominator)
  "numerator/denominator clamped to [0..1], following C float semantics for a zero denominator"
  (if (zerop denominator)
      (if (> numerator 0) 1.0 0.0)
      (max 0.0 (min 1.0 (/ numerator denominator)))))

(defun gen-image-gradient-radial (width height density inner outer)
  "Generate image: radial gradient"
  (let* ((inner (%col inner)) (outer (%col outer))
         (pixels (%make-octets (* width height 4)))
         (radius (if (< width height) (/ width 2.0) (/ height 2.0)))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0)))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((dist (sqrt (+ (expt (- x center-x) 2) (expt (- y center-y) 2))))
               ;; Distance can be bigger than radius, so it needs to be checked
               (factor (%ratio-factor (- dist (* radius density)) (* radius (- 1.0 density)))))
          (destructuring-bind (r g b a) (%blend-channels inner outer factor)
            (%put-rgba pixels (+ (* y width) x) r g b a)))))
    (%make-rgba8-image width height pixels)))

(defun gen-image-gradient-square (width height density inner outer)
  "Generate image: square gradient"
  (let* ((inner (%col inner)) (outer (%col outer))
         (pixels (%make-octets (* width height 4)))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0)))
    (dotimes (y height)
      (dotimes (x width)
        ;; Calculate the Manhattan distance from the center, normalized by the dimensions
        (let* ((normalized-dist-x (/ (abs (- x center-x)) center-x))
               (normalized-dist-y (/ (abs (- y center-y)) center-y))
               (manhattan-dist (max normalized-dist-x normalized-dist-y))
               ;; Subtract the density from the manhattanDist, then divide by (1 - density)
               ;; This makes the gradient start from the center when density is 0, and from the edge when density is 1
               (factor (%ratio-factor (- manhattan-dist density) (- 1.0 density))))
          (destructuring-bind (r g b a) (%blend-channels inner outer factor)
            (%put-rgba pixels (+ (* y width) x) r g b a)))))
    (%make-rgba8-image width height pixels)))

(defun gen-image-checked (width height checks-x checks-y col1 col2)
  "Generate image: checked"
  (let ((col1 (%col col1)) (col2 (%col col2))
        (pixels (%make-octets (* width height 4))))
    (dotimes (y height)
      (dotimes (x width)
        (apply #'%put-rgba pixels (+ (* y width) x)
               (if (evenp (+ (floor x checks-x) (floor y checks-y))) col1 col2))))
    (%make-rgba8-image width height pixels)))

;; NOTE: It requires GetRandomValue(), defined in [rcore]
(defun gen-image-white-noise (width height factor)
  "Generate image: white noise"
  (let ((pixels (%make-octets (* width height 4))))
    (dotimes (i (* width height))
      (apply #'%put-rgba pixels i
             (if (< (get-random-value 0 99) (truncate (* factor 100.0))) +white+ +black+)))
    (%make-rgba8-image width height pixels)))

(defun gen-image-perlin-noise (width height offset-x offset-y scale)
  "Generate image: perlin noise"
  (let ((pixels (%make-octets (* width height 4)))
        (aspect-ratio (/ (float width) height)))
    (dotimes (y height)
      (dotimes (x width)
        (let ((nx (* (float (+ x offset-x)) (/ scale (float width))))
              (ny (* (float (+ y offset-y)) (/ scale (float height)))))
          ;; Apply aspect ratio compensation to wider side
          (if (> width height)
              (setf nx (* nx aspect-ratio))
              (setf ny (/ ny aspect-ratio)))
          ;; Calculate a better perlin noise using fbm (fractal brownian motion)
          (let* ((p (max -1.0 (min 1.0 (%perlin-fbm-noise3 nx ny 1.0 2.0 0.5 6))))
                 ;; Data needs to be normalized from [-1..1] to [0..1]
                 (np (/ (+ p 1.0) 2.0))
                 (intensity (%u8 (* np 255.0))))
            (%put-rgba pixels (+ (* y width) x) intensity intensity intensity 255)))))
    (%make-rgba8-image width height pixels)))

(defun gen-image-cellular (width height tile-size)
  "Generate image: cellular algorithm, bigger tileSize means bigger cells"
  (let* ((pixels (%make-octets (* width height 4)))
         (seeds-per-row (floor width tile-size))
         (seeds-per-col (floor height tile-size))
         (seed-count (* seeds-per-row seeds-per-col))
         (seeds (make-array seed-count)))
    (dotimes (i seed-count)
      (let ((y (+ (* (floor i seeds-per-row) tile-size) (get-random-value 0 (1- tile-size))))
            (x (+ (* (mod i seeds-per-row) tile-size) (get-random-value 0 (1- tile-size)))))
        (setf (aref seeds i) (cons x y))))
    (dotimes (y height)
      (let ((tile-y (floor y tile-size)))
        (dotimes (x width)
          (let ((tile-x (floor x tile-size))
                (min-distance 65536.0))
            ;; Check all adjacent tiles
            (loop for i from -1 below 2
                  unless (or (< (+ tile-x i) 0) (>= (+ tile-x i) seeds-per-row))
                    do (loop for j from -1 below 2
                             unless (or (< (+ tile-y j) 0) (>= (+ tile-y j) seeds-per-col))
                               do (let ((neighbor-seed (aref seeds (+ (* (+ tile-y j) seeds-per-row) tile-x i))))
                                    (setf min-distance
                                          (min min-distance
                                               (float (sqrt (+ (expt (- x (car neighbor-seed)) 2)
                                                               (expt (- y (cdr neighbor-seed)) 2)))))))))
            ;; This approach seems to give good results at all tile sizes
            (let ((intensity (min 255 (truncate (/ (* min-distance 256.0) tile-size)))))
              (%put-rgba pixels (+ (* y width) x) intensity intensity intensity 255))))))
    (%make-rgba8-image width height pixels)))

(defun gen-image-text (width height text)
  "Generate image: grayscale image from text data"
  (let* ((image-size (* width height))
         (data (%make-octets image-size)))
    (when text
      (let ((bytes (babel:string-to-octets text :encoding :utf-8)))
        (replace data bytes :end2 (min (length bytes) image-size))))
    (make-image :data data :width width :height height :mipmaps 1
                :format +pixelformat-uncompressed-grayscale+)))

;;;------------------------------------------------------------------------------------
;;; Image manipulation functions
;;;------------------------------------------------------------------------------------

(defun image-copy (image)
  "Create an image duplicate (useful for transformations)"
  (let ((width (image-width image))
        (height (image-height image))
        (size 0))
    (dotimes (i (image-mipmap-count image))
      (incf size (get-pixel-data-size width height (image-pixel-format image)))
      (setf width (max 1 (floor width 2))
            height (max 1 (floor height 2))))
    (let ((data (%make-octets size)))
      ;; NOTE: Size must be provided in bytes
      (replace data (image-data image))
      (make-image :data data :width (image-width image) :height (image-height image)
                  :mipmaps (image-mipmap-count image) :format (image-pixel-format image)))))

(defun image-from-image (image rec)
  "Create an image from another image piece"
  (let ((result (make-image :width 0 :height 0 :mipmaps 0 :format 0)))
    ;; Security check to avoid program crash
    (unless (%image-ready-p image) (return-from image-from-image result))
    (if (%compressed-format-p (image-pixel-format image))
        (trace-log-warning "IMAGE: Image manipulation not supported for compressed formats")
        (multiple-value-bind (rx ry rw rh) (%rec rec)
          ;; Basic rectangle validation: size smaller than image size
          (if (and (>= rx 0) (>= ry 0) (> rw 0) (> rh 0)
                   (<= (+ (truncate rx) (truncate rw)) (image-width image))
                   (<= (+ (truncate ry) (truncate rh)) (image-height image)))
              (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
                     (w (truncate rw)) (h (truncate rh))
                     (data (%make-octets (* w h bpp))))
                (dotimes (y h)
                  (replace data (image-data image)
                           :start1 (* y w bpp)
                           :start2 (* (+ (* (+ y (truncate ry)) (image-width image)) (truncate rx)) bpp)
                           :end2 (+ (* (+ (* (+ y (truncate ry)) (image-width image)) (truncate rx)) bpp) (* w bpp))))
                (setf result (make-image :data data :width w :height h :mipmaps 1
                                         :format (image-pixel-format image))))
              (trace-log-warning "IMAGE: ImageFromImage(), rectangle provided not valid"))))
    result))

;; NOTE: Security checks are performed in case rectangle goes out of bounds
(defun image-crop (image crop)
  "Crop an image to a defined rectangle"
  (unless (%image-ready-p image) (return-from image-crop nil))
  (multiple-value-bind (cx cy cw ch) (%rec crop)
    ;; Security checks to validate crop rectangle
    (when (< cx 0) (incf cw cx) (setf cx 0.0))
    (when (< cy 0) (incf ch cy) (setf cy 0.0))
    (when (> (+ cx cw) (image-width image)) (setf cw (- (image-width image) cx)))
    (when (> (+ cy ch) (image-height image)) (setf ch (- (image-height image) cy)))
    (when (or (> cx (image-width image)) (> cy (image-height image)))
      (trace-log-warning "IMAGE: Failed to crop, rectangle out of bounds")
      (return-from image-crop nil))
    (when (> (image-mipmap-count image) 1)
      (trace-log-warning "Image manipulation only applied to base mipmap level"))
    (if (%compressed-format-p (image-pixel-format image))
        (trace-log-warning "Image manipulation not supported for compressed formats")
        (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
               (w (truncate cw)) (h (truncate ch))
               (cropped (%make-octets (* (truncate (* cw ch)) bpp)))
               (offset 0))
          ;; Move cropped data line-by-line
          (loop for y from (truncate cy) below (truncate (+ cy ch))
                do (let ((src (* (+ (* y (image-width image)) (truncate cx)) bpp)))
                     (replace cropped (image-data image) :start1 offset :start2 src :end2 (+ src (* w bpp)))
                     (incf offset (* w bpp))))
          (setf (image-data image) cropped
                (image-width image) w
                (image-height image) h))))
  nil)

(defun image-format (image &optional (new-format nil new-format-p))
  "Convert image data to desired format (raylib ImageFormat)
   NOTE: Called with only IMAGE it returns the image pixel format"
  (unless new-format-p
    (return-from image-format (image-pixel-format image)))
  ;; Security check to avoid program crash
  (unless (%image-ready-p image) (return-from image-format nil))
  (when (and (/= new-format 0) (/= (image-pixel-format image) new-format))
    (if (and (not (%compressed-format-p (image-pixel-format image)))
             (not (%compressed-format-p new-format)))
        (let* ((pixels (%load-image-data-normalized image)) ; Supports 8 to 32 bit per channel
               (count (* (image-width image) (image-height image)))
               (data nil))
          ;; WARNING! Loosing mipmaps data --> Regenerated at the end
          (setf (image-pixel-format image) new-format)
          (macrolet ((px (k c) `(aref pixels (+ (* ,k 4) ,c))))
            (flet ((gray (k) (+ (* (px k 0) 0.299) (* (px k 1) 0.587) (* (px k 2) 0.114))))
              (alexandria:switch (new-format)
                (+pixelformat-uncompressed-grayscale+
                 (setf data (%make-octets count))
                 (dotimes (i count) (setf (aref data i) (%u8 (* (gray i) 255.0)))))
                (+pixelformat-uncompressed-gray-alpha+
                 (setf data (%make-octets (* count 2)))
                 (dotimes (k count)
                   (setf (aref data (* k 2)) (%u8 (* (gray k) 255.0))
                         (aref data (1+ (* k 2))) (%u8 (* (px k 3) 255.0)))))
                (+pixelformat-uncompressed-r5g6b5+
                 (setf data (%make-octets (* count 2)))
                 (dotimes (i count)
                   (let ((r (%u8 (%c-round (* (px i 0) 31.0))))
                         (g (%u8 (%c-round (* (px i 1) 63.0))))
                         (b (%u8 (%c-round (* (px i 2) 31.0)))))
                     (setf (%u16 data (* i 2)) (logand #xffff (logior (ash r 11) (ash g 5) b))))))
                (+pixelformat-uncompressed-r8g8b8+
                 (setf data (%make-octets (* count 3)))
                 (dotimes (k count)
                   (dotimes (c 3) (setf (aref data (+ (* k 3) c)) (%u8 (* (px k c) 255.0))))))
                (+pixelformat-uncompressed-r5g5b5a1+
                 (setf data (%make-octets (* count 2)))
                 (dotimes (i count)
                   (let ((r (%u8 (%c-round (* (px i 0) 31.0))))
                         (g (%u8 (%c-round (* (px i 1) 31.0))))
                         (b (%u8 (%c-round (* (px i 2) 31.0))))
                         (a (if (> (px i 3) (/ (float +pixelformat-uncompressed-r5g5b5a1-alpha-threshold+) 255.0)) 1 0)))
                     (setf (%u16 data (* i 2)) (logand #xffff (logior (ash r 11) (ash g 6) (ash b 1) a))))))
                (+pixelformat-uncompressed-r4g4b4a4+
                 (setf data (%make-octets (* count 2)))
                 (dotimes (i count)
                   (let ((r (%u8 (%c-round (* (px i 0) 15.0))))
                         (g (%u8 (%c-round (* (px i 1) 15.0))))
                         (b (%u8 (%c-round (* (px i 2) 15.0))))
                         (a (%u8 (%c-round (* (px i 3) 15.0)))))
                     (setf (%u16 data (* i 2)) (logand #xffff (logior (ash r 12) (ash g 8) (ash b 4) a))))))
                (+pixelformat-uncompressed-r8g8b8a8+
                 (setf data (%make-octets (* count 4)))
                 (dotimes (k count)
                   (dotimes (c 4) (setf (aref data (+ (* k 4) c)) (%u8 (* (px k c) 255.0))))))
                (+pixelformat-uncompressed-r32+
                 ;; WARNING: Image is converted to GRAYSCALE equivalent 32bit
                 (setf data (%make-octets (* count 4)))
                 (dotimes (i count) (setf (%f32 data (* i 4)) (gray i))))
                (+pixelformat-uncompressed-r32g32b32+
                 (setf data (%make-octets (* count 12)))
                 (dotimes (k count)
                   (dotimes (c 3) (setf (%f32 data (+ (* k 12) (* c 4))) (px k c)))))
                (+pixelformat-uncompressed-r32g32b32a32+
                 (setf data (%make-octets (* count 16)))
                 (dotimes (k count)
                   (dotimes (c 4) (setf (%f32 data (+ (* k 16) (* c 4))) (px k c)))))
                (+pixelformat-uncompressed-r16+
                 ;; WARNING: Image is converted to GRAYSCALE equivalent 16bit
                 (setf data (%make-octets (* count 2)))
                 (dotimes (i count) (setf (%u16 data (* i 2)) (%float-to-half (gray i)))))
                (+pixelformat-uncompressed-r16g16b16+
                 (setf data (%make-octets (* count 6)))
                 (dotimes (k count)
                   (dotimes (c 3) (setf (%u16 data (+ (* k 6) (* c 2))) (%float-to-half (px k c))))))
                (+pixelformat-uncompressed-r16g16b16a16+
                 (setf data (%make-octets (* count 8)))
                 (dotimes (k count)
                   (dotimes (c 4) (setf (%u16 data (+ (* k 8) (* c 2))) (%float-to-half (px k c)))))))))
          (setf (image-data image) data)
          ;; In case original image had mipmaps, generate mipmaps for formatted image
          ;; NOTE: Original mipmaps are replaced by new ones, if custom mipmaps were used, they are lost
          (when (> (image-mipmap-count image) 1)
            (setf (image-mipmap-count image) 1)
            (when (image-data image) (image-mipmaps image))))
        (trace-log-warning "IMAGE: Data format is compressed, can not be converted")))
  nil)

(defun (setf image-format) (format image)
  "Set the image pixel format field (no data conversion)"
  (setf (image-pixel-format image) format))

(defun image-text (text font-size color)
  "Create an image from text (default font)"
  (let* ((default-font-size 10)                         ; Default Font chars height in pixel
         (font-size (max font-size default-font-size))
         (spacing (floor font-size default-font-size)))
    (image-text-ex (get-font-default) text (float font-size) (float spacing) color)))

(defun image-text-ex (font text font-size spacing tint)
  "Create an image from text (custom sprite font)"
  (when (null text) (return-from image-text-ex (make-image :width 0 :height 0 :mipmaps 0 :format 0)))
  (let* ((text-offset-x 0)                  ; Image drawing position X
         (text-offset-y 0)                  ; Offset between lines (on linebreak '\n')
         ;; NOTE: Text image is generated at font base size, later scaled to desired font size
         (im-size (measure-text-ex font text (float (font-base-size font)) spacing))
         (text-size (measure-text-ex font text font-size spacing))
         (im-text (gen-image-color (truncate (vx im-size)) (truncate (vy im-size)) +blank+)))
    (loop for ch across text
          for codepoint = (char-code ch)
          for index = (or (get-glyph-index font codepoint) 0)
          for glyph = (aref (font-glyphs font) index)
          for glyph-rec = (aref (font-recs font) index)
          do (if (= codepoint (char-code #\Newline))
                 ;; NOTE: Fixed line spacing of 1.5 line-height
                 (progn (incf text-offset-y (+ (font-base-size font) (floor (font-base-size font) 2)))
                        (setf text-offset-x 0))
                 (progn
                   (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab))
                              (glyph-info-image glyph))
                     (let ((rec (make-rectangle :x (float (+ text-offset-x (glyph-info-offset-x glyph)))
                                                :y (float (+ text-offset-y (glyph-info-offset-y glyph)))
                                                :width (rectangle-width glyph-rec)
                                                :height (rectangle-height glyph-rec)))
                           (glyph-image (glyph-info-image glyph)))
                       (image-draw-image-pro im-text glyph-image
                                             (make-rectangle :width (float (image-width glyph-image))
                                                             :height (float (image-height glyph-image)))
                                             rec (vec2 0.0 0.0) 0.0 tint)))
                   (if (zerop (glyph-info-advance-x glyph))
                       (incf text-offset-x (truncate (+ (rectangle-width glyph-rec) spacing)))
                       (incf text-offset-x (+ (glyph-info-advance-x glyph) (truncate spacing)))))))
    ;; Scale image depending on text size
    (when (/= (vy text-size) (vy im-size))
      (let ((scale-factor (/ (vy text-size) (vy im-size))))
        (trace-log-info "IMAGE: Text scaled by factor: ~f" scale-factor)
        ;; Using nearest-neighbor scaling algorithm for default font
        (if (eq font (get-font-default))
            (image-resize-nn im-text (truncate (* (vx im-size) scale-factor)) (truncate (* (vy im-size) scale-factor)))
            (image-resize im-text (truncate (* (vx im-size) scale-factor)) (truncate (* (vy im-size) scale-factor))))))
    im-text))

(defun image-from-channel (image selected-channel)
  "Create an image from a selected channel of another image (GRAYSCALE)"
  (let ((result (make-image :width 0 :height 0 :mipmaps 0 :format 0))
        (format (image-pixel-format image)))
    (unless (%image-ready-p image) (return-from image-from-channel result))
    ;; Check selected channel is valid
    (when (< selected-channel 0)
      (trace-log-warning "Channel cannot be negative. Setting channel to 0.")
      (setf selected-channel 0))
    (cond ((member format (list +pixelformat-uncompressed-grayscale+ +pixelformat-uncompressed-r32+
                                +pixelformat-uncompressed-r16+))
           (when (> selected-channel 0)
             (trace-log-warning "This image has only 1 channel. Setting channel to it.")
             (setf selected-channel 0)))
          ((= format +pixelformat-uncompressed-gray-alpha+)
           (when (> selected-channel 1)
             (trace-log-warning "This image has only 2 channels. Setting channel to alpha.")
             (setf selected-channel 1)))
          ((member format (list +pixelformat-uncompressed-r5g6b5+ +pixelformat-uncompressed-r8g8b8+
                                +pixelformat-uncompressed-r32g32b32+ +pixelformat-uncompressed-r16g16b16+))
           (when (> selected-channel 2)
             (trace-log-warning "This image has only 3 channels. Setting channel to red.")
             (setf selected-channel 0))))
    ;; Check for RGBA formats
    (when (> selected-channel 3)
      (trace-log-warning "ImageFromChannel supports channels 0 to 3 (RGBA). Setting channel to alpha.")
      (setf selected-channel 3))
    (let* ((count (* (image-width image) (image-height image)))
           (pixels (%make-octets count))    ; Values from 0 to 255
           (data (image-data image))
           (c selected-channel))
      (if (%compressed-format-p format)
          (trace-log-warning "IMAGE: Pixel data retrieval not supported for compressed image formats")
          (dotimes (i count)
            (let ((pixel-value
                    (alexandria:switch (format)
                      (+pixelformat-uncompressed-grayscale+ (/ (aref data (+ i c)) 255.0))
                      (+pixelformat-uncompressed-gray-alpha+ (/ (aref data (+ (* i 2) c)) 255.0))
                      (+pixelformat-uncompressed-r5g5b5a1+
                       (let ((p (%u16 data (* i 2))))
                         (case c
                           (0 (* (ldb (byte 5 11) p) (/ 1.0 31)))
                           (1 (* (ldb (byte 5 6) p) (/ 1.0 31)))
                           (2 (* (ldb (byte 5 1) p) (/ 1.0 31)))
                           (t (if (zerop (logand p 1)) 0.0 1.0)))))
                      (+pixelformat-uncompressed-r5g6b5+
                       (let ((p (%u16 data (* i 2))))
                         (case c
                           (0 (* (ldb (byte 5 11) p) (/ 1.0 31)))
                           (1 (* (ldb (byte 6 5) p) (/ 1.0 63)))
                           (t (* (ldb (byte 5 0) p) (/ 1.0 31))))))
                      (+pixelformat-uncompressed-r4g4b4a4+
                       (let ((p (%u16 data (* i 2))))
                         (* (ldb (byte 4 (- 12 (* c 4))) p) (/ 1.0 15))))
                      (+pixelformat-uncompressed-r8g8b8a8+ (/ (aref data (+ (* i 4) c)) 255.0))
                      (+pixelformat-uncompressed-r8g8b8+ (/ (aref data (+ (* i 3) c)) 255.0))
                      (+pixelformat-uncompressed-r32+ (%f32 data (* i 4)))
                      (+pixelformat-uncompressed-r32g32b32+ (%f32 data (* (+ (* i 3) c) 4)))
                      (+pixelformat-uncompressed-r32g32b32a32+ (%f32 data (* (+ (* i 4) c) 4)))
                      (+pixelformat-uncompressed-r16+ (%half-to-float (%u16 data (* i 2))))
                      (+pixelformat-uncompressed-r16g16b16+ (%half-to-float (%u16 data (* (+ (* i 3) c) 2))))
                      (+pixelformat-uncompressed-r16g16b16a16+ (%half-to-float (%u16 data (* (+ (* i 4) c) 2))))
                      (t -1.0))))
              (setf (aref pixels i) (%u8 (* pixel-value 255))))))
      (make-image :data pixels :width (image-width image) :height (image-height image) :mipmaps 1
                  :format +pixelformat-uncompressed-grayscale+))))

;; NOTE: Uses Nearest-Neighbor scaling algorithm
(defun image-resize-nn (image new-width new-height)
  "Resize image (Nearest-Neighbor scaling algorithm)"
  (unless (%image-ready-p image) (return-from image-resize-nn nil))
  (let* ((pixels (%load-image-colors image))
         (output (%make-octets (* new-width new-height 4)))
         ;; EDIT: added +1 to account for an early rounding problem
         (x-ratio (1+ (floor (ash (image-width image) 16) new-width)))
         (y-ratio (1+ (floor (ash (image-height image) 16) new-height))))
    (dotimes (y new-height)
      (dotimes (x new-width)
        (let ((x2 (ash (* x x-ratio) -16))
              (y2 (ash (* y y-ratio) -16)))
          (replace output pixels :start1 (* (+ (* y new-width) x) 4)
                                 :start2 (* (+ (* y2 (image-width image)) x2) 4)
                                 :end2 (+ (* (+ (* y2 (image-width image)) x2) 4) 4)))))
    (setf (image-width image) new-width
          (image-height image) new-height)
    (%set-image-rgba8 image output))      ; Reformat 32bit RGBA image to original format
  nil)

;; NOTE: Uses stb default scaling filters (both bicubic):
;; STBIR_DEFAULT_FILTER_UPSAMPLE    STBIR_FILTER_CATMULLROM
;; STBIR_DEFAULT_FILTER_DOWNSAMPLE  STBIR_FILTER_MITCHELL   (high-quality Catmull-Rom)
(defun image-resize (image new-width new-height)
  "Resize image (Bicubic scaling algorithm)"
  (unless (%image-ready-p image) (return-from image-resize nil))
  (let ((format (image-pixel-format image)))
    ;; Check if we can use a fast path on image scaling
    ;; It can be for 8 bit per channel images with 1 to 4 channels per pixel
    (if (member format (list +pixelformat-uncompressed-grayscale+ +pixelformat-uncompressed-gray-alpha+
                             +pixelformat-uncompressed-r8g8b8+ +pixelformat-uncompressed-r8g8b8a8+))
        (let ((bpp (%bytes-per-pixel format)))
          (setf (image-data image)
                (%resize-uint8 (image-data image) (image-width image) (image-height image) bpp
                               new-width new-height (= bpp 4))
                (image-width image) new-width
                (image-height image) new-height))
        ;; Get data as Color pixels array to work with it
        (let ((output (%resize-uint8 (%load-image-colors image) (image-width image) (image-height image) 4
                                     new-width new-height t)))
          (setf (image-width image) new-width
                (image-height image) new-height)
          (%set-image-rgba8 image output))))  ; Reformat 32bit RGBA image to original format
  nil)

;; NOTE: Resize offset is relative to the top-left corner of the original image
(defun image-resize-canvas (image new-width new-height offset-x offset-y fill)
  "Resize canvas and fill with color"
  (unless (%image-ready-p image) (return-from image-resize-canvas nil))
  (when (> (image-mipmap-count image) 1)
    (trace-log-warning "Image manipulation only applied to base mipmap level"))
  (cond ((%compressed-format-p (image-pixel-format image))
         (trace-log-warning "Image manipulation not supported for compressed formats"))
        ((or (/= new-width (image-width image)) (/= new-height (image-height image)))
         (let ((src-x 0) (src-y 0)
               (src-w (image-width image)) (src-h (image-height image))
               (dst-x offset-x) (dst-y offset-y))
           (cond ((< offset-x 0)
                  (setf src-x (- offset-x)) (incf src-w offset-x) (setf dst-x 0))
                 ((> (+ offset-x (image-width image)) new-width)
                  (setf src-w (- new-width offset-x))))
           (cond ((< offset-y 0)
                  (setf src-y (- offset-y)) (incf src-h offset-y) (setf dst-y 0))
                 ((> (+ offset-y (image-height image)) new-height)
                  (setf src-h (- new-height offset-y))))
           (when (< new-width src-w) (setf src-w new-width))
           (when (< new-height src-h) (setf src-h new-height))
           (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
                  (resized (%make-octets (* new-width new-height bpp))))
             ;; Fill resized canvas with fill color
             (set-pixel-color resized fill (image-pixel-format image))
             (loop for x from 1 below new-width
                   do (replace resized resized :start1 (* x bpp) :start2 0 :end2 bpp))
             (loop for y from 1 below new-height
                   do (replace resized resized :start1 (* y new-width bpp) :start2 0 :end2 (* new-width bpp)))
             ;; Copy old image to fill color background
             (let ((dst-offset (* (+ (* dst-y new-width) dst-x) bpp)))
               (dotimes (y src-h)
                 (let ((src (* (+ (* (+ y src-y) (image-width image)) src-x) bpp)))
                   (replace resized (image-data image) :start1 dst-offset :start2 src :end2 (+ src (* src-w bpp))))
                 (incf dst-offset (* new-width bpp))))
             (setf (image-data image) resized
                   (image-width image) new-width
                   (image-height image) new-height)))))
  nil)

(defun image-to-pot (image fill)
  "Convert image to POT (power-of-two)"
  (unless (%image-ready-p image) (return-from image-to-pot nil))
  ;; Calculate next power-of-two values
  ;; NOTE: Just add the required amount of pixels at the right and bottom sides of image...
  (let ((pot-width (truncate (expt 2.0 (fceiling (/ (log (float (image-width image))) (log 2.0))))))
        (pot-height (truncate (expt 2.0 (fceiling (/ (log (float (image-height image))) (log 2.0)))))))
    ;; Check if POT texture generation is required (if texture is not already POT)
    (when (or (/= pot-width (image-width image)) (/= pot-height (image-height image)))
      (image-resize-canvas image pot-width pot-height 0 0 fill)))
  nil)

(defun image-alpha-crop (image threshold)
  "Crop image depending on alpha value"
  (unless (%image-ready-p image) (return-from image-alpha-crop nil))
  (let ((crop (get-image-alpha-border image threshold)))
    ;; Crop if rectangle is valid
    (when (and (/= (truncate (rectangle-width crop)) 0) (/= (truncate (rectangle-height crop)) 0))
      (image-crop image crop)))
  nil)

;; NOTE: Threshold defines the alpha limit, 0.0f to 1.0f
(defun image-alpha-clear (image color threshold)
  "Clear alpha channel to desired color"
  (unless (%image-ready-p image) (return-from image-alpha-clear nil))
  (when (> (image-mipmap-count image) 1)
    (trace-log-warning "Image manipulation only applied to base mipmap level"))
  (if (%compressed-format-p (image-pixel-format image))
      (trace-log-warning "Image manipulation not supported for compressed formats")
      (destructuring-bind (cr cg cb ca) (%col color)
        (let ((data (image-data image))
              (count (* (image-width image) (image-height image))))
          (alexandria:switch ((image-pixel-format image))
            (+pixelformat-uncompressed-gray-alpha+
             (let ((threshold-value (%u8 (* threshold 255.0))))
               (loop for i from 1 below (* count 2) by 2
                     when (<= (aref data i) threshold-value)
                       do (setf (aref data (1- i)) cr
                                (aref data i) ca))))
            (+pixelformat-uncompressed-r5g5b5a1+
             (let ((threshold-value (if (< threshold 0.5) 0 1))
                   (r (%u8 (%c-round (* cr 31.0))))
                   (g (%u8 (%c-round (* cg 31.0))))
                   (b (%u8 (%c-round (* cb 31.0))))
                   (a (if (< ca 128) 0 1)))
               (dotimes (i count)
                 (when (<= (logand (%u16 data (* i 2)) 1) threshold-value)
                   (setf (%u16 data (* i 2)) (logand #xffff (logior (ash r 11) (ash g 6) (ash b 1) a)))))))
            (+pixelformat-uncompressed-r4g4b4a4+
             (let ((threshold-value (%u8 (* threshold 15.0)))
                   (r (%u8 (%c-round (* cr 15.0))))
                   (g (%u8 (%c-round (* cg 15.0))))
                   (b (%u8 (%c-round (* cb 15.0))))
                   (a (%u8 (%c-round (* ca 15.0)))))
               (dotimes (i count)
                 (when (<= (logand (%u16 data (* i 2)) #x000f) threshold-value)
                   (setf (%u16 data (* i 2)) (logand #xffff (logior (ash r 12) (ash g 8) (ash b 4) a)))))))
            (+pixelformat-uncompressed-r8g8b8a8+
             (let ((threshold-value (%u8 (* threshold 255.0))))
               (loop for i from 3 below (* count 4) by 4
                     when (<= (aref data i) threshold-value)
                       do (setf (aref data (- i 3)) cr (aref data (- i 2)) cg
                                (aref data (- i 1)) cb (aref data i) ca))))
            (+pixelformat-uncompressed-r32g32b32a32+
             (loop for i from 3 below (* count 4) by 4
                   when (<= (%f32 data (* i 4)) threshold)
                     do (setf (%f32 data (* (- i 3) 4)) (/ cr 255.0)
                              (%f32 data (* (- i 2) 4)) (/ cg 255.0)
                              (%f32 data (* (- i 1) 4)) (/ cb 255.0)
                              (%f32 data (* i 4)) (/ ca 255.0))))
            (+pixelformat-uncompressed-r16g16b16a16+
             (loop for i from 3 below (* count 4) by 4
                   when (<= (%half-to-float (%u16 data (* i 2))) threshold)
                     do (setf (%u16 data (* (- i 3) 2)) (%float-to-half (/ cr 255.0))
                              (%u16 data (* (- i 2) 2)) (%float-to-half (/ cg 255.0))
                              (%u16 data (* (- i 1) 2)) (%float-to-half (/ cb 255.0))
                              (%u16 data (* i 2)) (%float-to-half (/ ca 255.0)))))))))
  nil)

;; NOTE: alphaMask should be same size as image
(defun image-alpha-mask (image alpha-mask)
  "Apply alpha mask to image"
  (cond ((or (/= (image-width image) (image-width alpha-mask))
             (/= (image-height image) (image-height alpha-mask)))
         (trace-log-warning "IMAGE: Alpha mask must be same size as image"))
        ((%compressed-format-p (image-pixel-format image))
         (trace-log-warning "IMAGE: Alpha mask can not be applied to compressed data formats"))
        (t
         ;; Force mask to be Grayscale
         (let ((mask (image-copy alpha-mask))
               (count (* (image-width image) (image-height image))))
           (unless (= (image-pixel-format mask) +pixelformat-uncompressed-grayscale+)
             (image-format mask +pixelformat-uncompressed-grayscale+))
           ;; In case image is only grayscale, we just add alpha channel
           (if (= (image-pixel-format image) +pixelformat-uncompressed-grayscale+)
               (let ((data (%make-octets (* count 2))))
                 ;; Apply alpha mask to alpha channel
                 (dotimes (i count)
                   (setf (aref data (* i 2)) (aref (image-data image) i)
                         (aref data (1+ (* i 2))) (aref (image-data mask) i)))
                 (setf (image-data image) data
                       (image-pixel-format image) +pixelformat-uncompressed-gray-alpha+))
               (progn
                 ;; Convert image to RGBA
                 (unless (= (image-pixel-format image) +pixelformat-uncompressed-r8g8b8a8+)
                   (image-format image +pixelformat-uncompressed-r8g8b8a8+))
                 ;; Apply alpha mask to alpha channel
                 (dotimes (i count)
                   (setf (aref (image-data image) (+ (* i 4) 3)) (aref (image-data mask) i))))))))
  nil)

;; NOTE: Premultiply alpha to colors
(defun image-alpha-premultiply (image)
  "Premultiply alpha channel"
  (unless (%image-ready-p image) (return-from image-alpha-premultiply nil))
  (let ((pixels (%load-image-colors image)))
    (dotimes (i (* (image-width image) (image-height image)))
      (let* ((k (* i 4))
             (a (aref pixels (+ k 3))))
        (cond ((= a 0)
               (setf (aref pixels k) 0 (aref pixels (+ k 1)) 0 (aref pixels (+ k 2)) 0))
              ((< a 255)
               (let ((alpha (/ a 255.0)))
                 (dotimes (c 3)
                   (setf (aref pixels (+ k c)) (%u8 (* (aref pixels (+ k c)) alpha)))))))))
    (%set-image-rgba8 image pixels))
  nil)

(defun image-blur-gaussian (image blur-size)
  "Apply Gaussian blur using a box blur approximation"
  (unless (%image-ready-p image) (return-from image-blur-gaussian nil))
  (image-alpha-premultiply image)
  (let* ((w (image-width image))
         (h (image-height image))
         (pixels (%load-image-colors image))
         (copy1 (make-array (* w h 4) :element-type 'single-float))
         (copy2 (make-array (* w h 4) :element-type 'single-float :initial-element 0.0)))
    ;; Loop switches between pixelsCopy1 and pixelsCopy2
    (dotimes (i (* w h 4)) (setf (aref copy1 i) (float (aref pixels i))))
    (dotimes (j +gaussian-blur-iterations+)
      ;; Horizontal motion blur
      (dotimes (row h)
        (let ((avg (make-array 4 :element-type 'single-float :initial-element 0.0))
              (convolution-size blur-size))
          (dotimes (i blur-size)
            (dotimes (c 4) (incf (aref avg c) (aref copy1 (+ (* (+ (* row w) i) 4) c)))))
          (dotimes (x w)
            (when (>= (- x blur-size 1) 0)
              (dotimes (c 4) (decf (aref avg c) (aref copy1 (+ (* (+ (* row w) (- x blur-size 1)) 4) c))))
              (decf convolution-size))
            (when (< (+ x blur-size) w)
              (dotimes (c 4) (incf (aref avg c) (aref copy1 (+ (* (+ (* row w) x blur-size) 4) c))))
              (incf convolution-size))
            (dotimes (c 4)
              (setf (aref copy2 (+ (* (+ (* row w) x) 4) c)) (/ (aref avg c) convolution-size))))))
      ;; Vertical motion blur
      (dotimes (col w)
        (let ((avg (make-array 4 :element-type 'single-float :initial-element 0.0))
              (convolution-size blur-size))
          (dotimes (i blur-size)
            (dotimes (c 4) (incf (aref avg c) (aref copy2 (+ (* (+ (* i w) col) 4) c)))))
          (dotimes (y h)
            (when (>= (- y blur-size 1) 0)
              (dotimes (c 4) (decf (aref avg c) (aref copy2 (+ (* (+ (* (- y blur-size 1) w) col) 4) c))))
              (decf convolution-size))
            (when (< (+ y blur-size) h)
              (dotimes (c 4) (incf (aref avg c) (aref copy2 (+ (* (+ (* (+ y blur-size) w) col) 4) c))))
              (incf convolution-size))
            (dotimes (c 4)
              (setf (aref copy1 (+ (* (+ (* y w) col) 4) c))
                    (float (%u8 (/ (aref avg c) convolution-size)))))))))
    ;; Reverse premultiply
    (dotimes (i (* w h))
      (let* ((k (* i 4))
             (a (aref copy1 (+ k 3))))
        (cond ((= a 0.0)
               (fill pixels 0 :start k :end (+ k 4)))
              ((<= a 255.0)
               (let ((alpha (/ a 255.0)))
                 (dotimes (c 3)
                   (setf (aref pixels (+ k c)) (%u8 (min (/ (aref copy1 (+ k c)) alpha) 255.0))))
                 (setf (aref pixels (+ k 3)) (%u8 a)))))))
    (%set-image-rgba8 image pixels))
  nil)

(defun image-kernel-convolution (image kernel kernel-size)
  "Apply custom square convolution kernel to image
   NOTE: The convolution kernel matrix is expected to be square"
  (when (or (not (%image-ready-p image)) (null kernel)) (return-from image-kernel-convolution nil))
  (let ((kernel-width (truncate (sqrt (float kernel-size))))
        (kernel (coerce kernel 'simple-vector)))
    (unless (= (* kernel-width kernel-width) kernel-size)
      (trace-log-warning "IMAGE: Convolution kernel must be square to be applied")
      (return-from image-kernel-convolution nil))
    (let* ((w (image-width image))
           (h (image-height image))
           (pixels (%load-image-colors image))
           (result (%make-octets (* w h 4)))
           (start-range (- (truncate kernel-width 2)))
           (end-range (if (evenp kernel-width) (truncate kernel-width 2) (1+ (truncate kernel-width 2)))))
      (dotimes (x h)
        (dotimes (y w)
          (let ((res (make-array 4 :element-type 'single-float :initial-element 0.0)))
            (loop for xk from start-range below end-range
                  for row = (max 0 (min (1- h) (+ x xk)))
                  do (loop for yk from start-range below end-range
                           for col = (max 0 (min (1- w) (+ y yk)))
                           for kv = (float (svref kernel (+ (* kernel-width (+ xk (truncate kernel-width 2)))
                                                            (+ yk (truncate kernel-width 2))))
                                           1.0)
                           for index = (* (+ (* w row) col) 4)
                           do (dotimes (c 4)
                                (incf (aref res c) (* (/ (aref pixels (+ index c)) 255.0) kv)))))
            (dotimes (c 4)
              (setf (aref result (+ (* (+ (* w x) y) 4) c))
                    (%u8 (* (max 0.0 (min 1.0 (aref res c))) 255.0)))))))
      (%set-image-rgba8 image result)))
  nil)

;; NOTE: Mipmaps are stored right after the base level in the image data
(defun image-mipmaps (image)
  "Compute all mipmap levels for a provided image"
  (unless (%image-ready-p image) (return-from image-mipmaps nil))
  (let* ((format (image-pixel-format image))
         (mip-count 1)                                 ; Required mipmap levels count (including base level)
         (mip-width (image-width image))               ; Base image width
         (mip-height (image-height image))             ; Base image height
         (mip-size (get-pixel-data-size mip-width mip-height format)))
    ;; Count mipmap levels required
    (loop while (or (/= mip-width 1) (/= mip-height 1))
          do (unless (= mip-width 1) (setf mip-width (floor mip-width 2)))
             (unless (= mip-height 1) (setf mip-height (floor mip-height 2)))
             (setf mip-width (max mip-width 1) mip-height (max mip-height 1))
             (incf mip-count)
             (incf mip-size (get-pixel-data-size mip-width mip-height format)))
    (if (< (image-mipmap-count image) mip-count)
        (let ((data (%make-octets mip-size))
              (next-mip 0))
          (replace data (image-data image) :end2 (get-pixel-data-size (image-width image) (image-height image) format))
          (setf (image-data image) data
                mip-width (image-width image)
                mip-height (image-height image)
                mip-size (get-pixel-data-size mip-width mip-height format))
          (let ((im-copy (image-copy image)))
            (loop for i from 1 below mip-count
                  do (incf next-mip mip-size)
                     (setf mip-width (max 1 (floor mip-width 2))
                           mip-height (max 1 (floor mip-height 2))
                           mip-size (get-pixel-data-size mip-width mip-height format))
                     (when (>= i (image-mipmap-count image))
                       (image-resize im-copy mip-width mip-height) ; Uses internally Mitchell cubic downscale filter
                       (replace data (image-data im-copy) :start1 next-mip :end2 mip-size))))
          (setf (image-mipmap-count image) mip-count))
        (trace-log-warning "IMAGE: Mipmaps already available")))
  nil)

;; NOTE: In case selected bpp do not represent a known 16bit format,
;; dithered data is stored in the LSB part of the unsigned short
(defun image-dither (image r-bpp g-bpp b-bpp a-bpp)
  "Dither image data to 16bpp or lower (Floyd-Steinberg dithering)"
  (unless (%image-ready-p image) (return-from image-dither nil))
  (when (%compressed-format-p (image-pixel-format image))
    (trace-log-warning "IMAGE: Compressed data formats can not be dithered")
    (return-from image-dither nil))
  (if (> (+ r-bpp g-bpp b-bpp a-bpp) 16)
      (trace-log-warning "IMAGE: Unsupported dithering bpps (~dbpp), only 16bpp or lower modes supported"
                         (+ r-bpp g-bpp b-bpp a-bpp))
      (let* ((w (image-width image))
             (h (image-height image))
             (pixels (%load-image-colors image))
             (data (%make-octets (* w h 2))))
        (unless (member (image-pixel-format image) (list +pixelformat-uncompressed-r8g8b8+
                                                         +pixelformat-uncompressed-r8g8b8a8+))
          (trace-log-warning "IMAGE: Format is already 16bpp or lower, dithering could have no effect"))
        ;; Define new image format, check if desired bpp match internal known format
        (setf (image-pixel-format image)
              (cond ((and (= r-bpp 5) (= g-bpp 6) (= b-bpp 5) (= a-bpp 0)) +pixelformat-uncompressed-r5g6b5+)
                    ((and (= r-bpp 5) (= g-bpp 5) (= b-bpp 5) (= a-bpp 1)) +pixelformat-uncompressed-r5g5b5a1+)
                    ((and (= r-bpp 4) (= g-bpp 4) (= b-bpp 4) (= a-bpp 4)) +pixelformat-uncompressed-r4g4b4a4+)
                    (t (trace-log-warning "IMAGE: Unsupported dithered OpenGL internal format: ~dbpp (R~dG~dB~dA~d)"
                                          (+ r-bpp g-bpp b-bpp a-bpp) r-bpp g-bpp b-bpp a-bpp)
                       0)))
        (setf (image-data image) data)
        (flet ((spread (index errors factor)
                 (dotimes (c 3)
                   (setf (aref pixels (+ (* index 4) c))
                         (logand #xff (min (+ (aref pixels (+ (* index 4) c))
                                              (truncate (/ (* (float (aref errors c)) factor) 16)))
                                           #xff))))))
          (dotimes (y h)
            (dotimes (x w)
              (let* ((i (+ (* y w) x))
                     (old-r (aref pixels (* i 4)))
                     (old-g (aref pixels (+ (* i 4) 1)))
                     (old-b (aref pixels (+ (* i 4) 2)))
                     (old-a (aref pixels (+ (* i 4) 3)))
                     (new-r (ash old-r (- (- 8 r-bpp))))      ; R bits
                     (new-g (ash old-g (- (- 8 g-bpp))))      ; G bits
                     (new-b (ash old-b (- (- 8 b-bpp))))      ; B bits
                     (new-a (ash old-a (- (- 8 a-bpp))))      ; A bits (not used on dithering)
                     ;; NOTE: Error must be computed between new and old pixel but using same number of bits!
                     ;; We want to know how much color precision we have lost...
                     (errors (vector (- old-r (logand #xff (ash new-r (- 8 r-bpp))))
                                   (- old-g (logand #xff (ash new-g (- 8 g-bpp))))
                                   (- old-b (logand #xff (ash new-b (- 8 b-bpp)))))))
                (%put-rgba pixels i new-r new-g new-b new-a)
                ;; NOTE: Some cases are out of the array and should be ignored
                (when (< x (1- w)) (spread (1+ i) errors 7.0))
                (when (and (> x 0) (< y (1- h))) (spread (+ i w -1) errors 3.0))
                (when (< y (1- h)) (spread (+ i w) errors 5.0))
                (when (and (< x (1- w)) (< y (1- h))) (spread (+ i w 1) errors 1.0))
                (setf (%u16 data (* i 2))
                      (logand #xffff (logior (ash new-r (+ g-bpp b-bpp a-bpp))
                                             (ash new-g (+ b-bpp a-bpp))
                                             (ash new-b a-bpp)
                                             new-a)))))))))
  nil)

(defun %image-check-manipulation (image)
  "Common checks of the flip/rotate functions, returns T when the image can be processed"
  (when (%image-ready-p image)
    (when (> (image-mipmap-count image) 1)
      (trace-log-warning "Image manipulation only applied to base mipmap level"))
    (if (%compressed-format-p (image-pixel-format image))
        (progn (trace-log-warning "Image manipulation not supported for compressed formats") nil)
        t)))

(defun image-flip-vertical (image)
  "Flip image vertically"
  (when (%image-check-manipulation image)
    (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
           (row (* (image-width image) bpp))
           (flipped (%make-octets (* row (image-height image)))))
      (loop for i from (1- (image-height image)) downto 0
            for offset from 0 by row
            do (replace flipped (image-data image) :start1 offset :start2 (* i row) :end2 (* (1+ i) row)))
      (setf (image-data image) flipped)))
  nil)

(defun image-flip-horizontal (image)
  "Flip image horizontally"
  (when (%image-check-manipulation image)
    (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
           (w (image-width image))
           (data (image-data image))
           (flipped (%make-octets (* w (image-height image) bpp))))
      (dotimes (y (image-height image))
        (dotimes (x w)
          (replace flipped data :start1 (* (+ (* y w) x) bpp)
                                :start2 (* (+ (* y w) (- w 1 x)) bpp)
                                :end2 (* (+ (* y w) (- w x)) bpp))))
      (setf (image-data image) flipped)))
  nil)

(defun image-rotate (image degrees)
  "Rotate image by input angle in degrees (-359 to 359)"
  (when (%image-check-manipulation image)
    (let* ((rad (/ (* degrees +pi+) 180.0))
           (sin-radius (sin rad))
           (cos-radius (cos rad))
           (iw (image-width image))
           (ih (image-height image))
           (width (truncate (+ (abs (* iw cos-radius)) (abs (* ih sin-radius)))))
           (height (truncate (+ (abs (* ih cos-radius)) (abs (* iw sin-radius)))))
           (bpp (%bytes-per-pixel (image-pixel-format image)))
           (data (image-data image))
           (rotated (%make-octets (get-pixel-data-size width height (image-pixel-format image)))))
      (dotimes (y height)
        (dotimes (x width)
          (let ((old-x (+ (+ (* (- x (/ width 2.0)) cos-radius) (* (- y (/ height 2.0)) sin-radius)) (/ iw 2.0)))
                (old-y (+ (- (* (- y (/ height 2.0)) cos-radius) (* (- x (/ width 2.0)) sin-radius)) (/ ih 2.0))))
            (when (and (>= old-x 0) (< old-x iw) (>= old-y 0) (< old-y ih))
              (let* ((x1 (floor old-x))
                     (y1 (floor old-y))
                     (x2 (min (1+ x1) (1- iw)))
                     (y2 (min (1+ y1) (1- ih)))
                     (px (- old-x x1))
                     (py (- old-y y1)))
                (dotimes (i bpp)
                  (let ((f1 (aref data (+ (* (+ (* y1 iw) x1) bpp) i)))
                        (f2 (aref data (+ (* (+ (* y1 iw) x2) bpp) i)))
                        (f3 (aref data (+ (* (+ (* y2 iw) x1) bpp) i)))
                        (f4 (aref data (+ (* (+ (* y2 iw) x2) bpp) i))))
                    (setf (aref rotated (+ (* (+ (* y width) x) bpp) i))
                          (%u8 (+ (* f1 (- 1 px) (- 1 py)) (* f2 px (- 1 py))
                                  (* f3 (- 1 px) py) (* f4 px py)))))))))))
      (setf (image-data image) rotated
            (image-width image) width
            (image-height image) height)))
  nil)

(defun image-rotate-cw (image)
  "Rotate image clockwise 90deg"
  (when (%image-check-manipulation image)
    (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
           (w (image-width image))
           (h (image-height image))
           (rotated (%make-octets (* w h bpp))))
      (dotimes (y h)
        (dotimes (x w)
          (replace rotated (image-data image) :start1 (* (+ (* x h) (- h y 1)) bpp)
                                              :start2 (* (+ (* y w) x) bpp)
                                              :end2 (* (+ (* y w) x 1) bpp))))
      (setf (image-data image) rotated
            (image-width image) h
            (image-height image) w)))
  nil)

(defun image-rotate-ccw (image)
  "Rotate image counter-clockwise 90deg"
  (when (%image-check-manipulation image)
    (let* ((bpp (%bytes-per-pixel (image-pixel-format image)))
           (w (image-width image))
           (h (image-height image))
           (rotated (%make-octets (* w h bpp))))
      (dotimes (y h)
        (dotimes (x w)
          (replace rotated (image-data image) :start1 (* (+ (* x h) y) bpp)
                                              :start2 (* (+ (* y w) (- w x 1)) bpp)
                                              :end2 (* (+ (* y w) (- w x)) bpp))))
      (setf (image-data image) rotated
            (image-width image) h
            (image-height image) w)))
  nil)

(defun image-color-tint (image color)
  "Modify image color: tint"
  (unless (%image-ready-p image) (return-from image-color-tint nil))
  (let ((pixels (%load-image-colors image))
        (tint (%col color)))
    (dotimes (i (* (image-width image) (image-height image)))
      (loop for c from 0 below 4
            for tc in tint
            do (setf (aref pixels (+ (* i 4) c)) (%u8 (floor (* (aref pixels (+ (* i 4) c)) tc) 255)))))
    (%set-image-rgba8 image pixels))
  nil)

(defun image-color-invert (image)
  "Modify image color: invert"
  (unless (%image-ready-p image) (return-from image-color-invert nil))
  (let ((pixels (%load-image-colors image)))
    (dotimes (i (* (image-width image) (image-height image)))
      (dotimes (c 3)
        (setf (aref pixels (+ (* i 4) c)) (- 255 (aref pixels (+ (* i 4) c))))))
    (%set-image-rgba8 image pixels))
  nil)

(defun image-color-grayscale (image)
  "Modify image color: grayscale"
  (image-format image +pixelformat-uncompressed-grayscale+))

;; NOTE: Contrast values between -100 and 100
(defun image-color-contrast (image contrast)
  "Modify image color: contrast (-100 to 100)"
  (unless (%image-ready-p image) (return-from image-color-contrast nil))
  (let* ((contrast (max -100 (min 100 contrast)))
         (factor (/ (float (+ 100.0 contrast)) 100.0))
         (factor (* factor factor))
         (pixels (%load-image-colors image)))
    (dotimes (i (* (image-width image) (image-height image)))
      (dotimes (c 3)
        (let ((p (* (+ (* (- (/ (aref pixels (+ (* i 4) c)) 255.0) 0.5) factor) 0.5) 255)))
          (setf (aref pixels (+ (* i 4) c)) (%u8 (max 0 (min 255 p)))))))
    (%set-image-rgba8 image pixels))
  nil)

;; NOTE: Brightness values between -255 and 255
(defun image-color-brightness (image brightness)
  "Modify image color: brightness (-255 to 255)"
  (unless (%image-ready-p image) (return-from image-color-brightness nil))
  (let ((brightness (max -255 (min 255 brightness)))
        (pixels (%load-image-colors image)))
    (dotimes (i (* (image-width image) (image-height image)))
      (dotimes (c 3)
        (let ((v (+ (aref pixels (+ (* i 4) c)) brightness)))
          (setf (aref pixels (+ (* i 4) c)) (cond ((< v 0) 1) ((> v 255) 255) (t v))))))
    (%set-image-rgba8 image pixels))
  nil)

(defun image-color-replace (image color replace)
  "Modify image color: replace color"
  (unless (%image-ready-p image) (return-from image-color-replace nil))
  (let ((pixels (%load-image-colors image))
        (color (%col color))
        (replace (%col replace))
        (format (image-pixel-format image)))
    (dotimes (i (* (image-width image) (image-height image)))
      (let ((k (* i 4)))
        (when (and (= (aref pixels k) (first color)) (= (aref pixels (+ k 1)) (second color))
                   (= (aref pixels (+ k 2)) (third color)) (= (aref pixels (+ k 3)) (fourth color)))
          (%put-rgba pixels i (first replace) (second replace) (third replace) (fourth replace)))))
    (setf (image-data image) pixels
          (image-pixel-format image) +pixelformat-uncompressed-r8g8b8a8+)
    ;; Only convert back to the original format if it has no alpha (alpha may have been replaced)
    (when (member format (list +pixelformat-uncompressed-r8g8b8+ +pixelformat-uncompressed-r5g6b5+
                               +pixelformat-uncompressed-grayscale+ +pixelformat-uncompressed-r32g32b32+
                               +pixelformat-uncompressed-r16g16b16+ +pixelformat-compressed-dxt1-rgb+
                               +pixelformat-compressed-etc1-rgb+ +pixelformat-compressed-etc2-rgb+
                               +pixelformat-compressed-pvrt-rgb+))
      (image-format image format)))
  nil)

;; NOTE: Returns a vector of colors (r g b a) in row-major order
(defun load-image-colors (image)
  "Load color data from image as a Color array (RGBA - 32bit)"
  (when (or (zerop (image-width image)) (zerop (image-height image)))  ; Security check
    (return-from load-image-colors nil))
  (let* ((pixels (%load-image-colors image))
         (count (* (image-width image) (image-height image)))
         (colors (make-array count)))
    (dotimes (i count colors)
      (let ((k (* i 4)))
        (setf (svref colors i) (list (aref pixels k) (aref pixels (+ k 1))
                                     (aref pixels (+ k 2)) (aref pixels (+ k 3))))))))

;; NOTE: Returns the palette (MAX-PALETTE-SIZE colors, unused ones BLANK) and the color count
(defun load-image-palette (image max-palette-size)
  "Load colors palette from image as a Color array (RGBA - 32bit)"
  (let ((pal-count 0)
        (palette nil)
        (pixels (load-image-colors image)))
    (when pixels
      (setf palette (make-array max-palette-size :initial-element +blank+))
      (loop for pixel across pixels
            when (> (fourth pixel) 0)
              ;; Check if the color is already on palette
              do (unless (find pixel palette :test #'equal)
                   ;; Store color if not on the palette
                   (setf (svref palette pal-count) pixel)
                   (incf pal-count)
                   ;; Reached the limit of colors supported by palette
                   (when (>= pal-count max-palette-size)
                     (trace-log-warning "IMAGE: Palette is greater than ~d colors" max-palette-size)
                     (loop-finish)))))
    (values palette pal-count)))

(defun unload-image-colors (colors)
  "Unload color data loaded with LoadImageColors()"
  (declare (ignore colors))
  nil)

(defun unload-image-palette (colors)
  "Unload colors palette loaded with LoadImagePalette()"
  (declare (ignore colors))
  nil)

;; NOTE: Threshold is defined as a percentage: 0.0f -> 1.0f
(defun get-image-alpha-border (image threshold)
  "Get image alpha border rectangle"
  (let ((crop (make-rectangle)))
    (when (%image-ready-p image)
      (let ((pixels (%load-image-colors image))
            (x-min 65536) (x-max 0)            ; Define a big enough number
            (y-min 65536) (y-max 0)
            (limit (%u8 (* threshold 255.0))))
        (dotimes (y (image-height image))
          (dotimes (x (image-width image))
            (when (> (aref pixels (+ (* (+ (* y (image-width image)) x) 4) 3)) limit)
              (setf x-min (min x-min x) x-max (max x-max x)
                    y-min (min y-min y) y-max (max y-max y)))))
        ;; Check for empty blank image
        (when (and (/= x-min 65536) (/= x-max 65536))
          (setf crop (make-rectangle :x (float x-min) :y (float y-min)
                                     :width (float (- (1+ x-max) x-min))
                                     :height (float (- (1+ y-max) y-min)))))))
    crop))

(defun get-image-color (image x y)
  "Get image pixel color at (x, y) position"
  (if (and (>= x 0) (< x (image-width image)) (>= y 0) (< y (image-height image)))
      (if (%compressed-format-p (image-pixel-format image))
          (progn (trace-log-warning "Compressed image format does not support color reading")
                 (list 0 0 0 0))
          (%image-color (image-data image) (+ (* y (image-width image)) x) (image-pixel-format image)))
      (progn (trace-log-warning "Requested image pixel (~d, ~d) out of bounds" x y)
             (list 0 0 0 0))))

;;;------------------------------------------------------------------------------------
;;; Image drawing functions
;;;------------------------------------------------------------------------------------

(defun image-clear-background (dst color)
  "Clear image background with given color"
  (unless (%image-ready-p dst) (return-from image-clear-background nil))
  ;; Fill in first pixel based on image format
  (image-draw-pixel dst 0 0 color)
  (let* ((data (image-data dst))
         (bpp (%bytes-per-pixel (image-pixel-format dst)))
         (total-pixels (* (image-width dst) (image-height dst))))
    ;; Repeat the first pixel data throughout the image, doubling the pixels copied on each loop
    (loop for i = 1 then (* i 2)
          while (< i total-pixels)
          do (let ((pixels-to-copy (min i (- total-pixels i))))
               (replace data data :start1 (* i bpp) :start2 0 :end2 (* pixels-to-copy bpp)))))
  nil)

(defun image-draw-pixel (dst x y color)
  "Draw pixel within an image"
  (let ((x (truncate x)) (y (truncate y)))
    ;; Security check to avoid program crash
    (when (or (null (image-data dst)) (< x 0) (>= x (image-width dst)) (< y 0) (>= y (image-height dst)))
      (return-from image-draw-pixel nil))
    (destructuring-bind (r g b a) (%col color)
      (let* ((data (image-data dst))
             (i (+ (* y (image-width dst)) x))
             (nr (/ r 255.0)) (ng (/ g 255.0)) (nb (/ b 255.0)) (na (/ a 255.0)))
        (alexandria:switch ((image-pixel-format dst))
          (+pixelformat-uncompressed-grayscale+
           (setf (aref data i) (%gray r g b)))
          (+pixelformat-uncompressed-gray-alpha+
           (setf (aref data (* i 2)) (%gray r g b)
                 (aref data (1+ (* i 2))) a))
          (+pixelformat-uncompressed-r5g6b5+
           (setf (%u16 data (* i 2))
                 (logand #xffff (logior (ash (%u8 (%c-round (* nr 31.0))) 11)
                                        (ash (%u8 (%c-round (* ng 63.0))) 5)
                                        (%u8 (%c-round (* nb 31.0)))))))
          (+pixelformat-uncompressed-r5g5b5a1+
           (setf (%u16 data (* i 2))
                 (logand #xffff (logior (ash (%u8 (%c-round (* nr 31.0))) 11)
                                        (ash (%u8 (%c-round (* ng 31.0))) 6)
                                        (ash (%u8 (%c-round (* nb 31.0))) 1)
                                        (if (> na (/ (float +pixelformat-uncompressed-r5g5b5a1-alpha-threshold+) 255.0)) 1 0)))))
          (+pixelformat-uncompressed-r4g4b4a4+
           (setf (%u16 data (* i 2))
                 (logand #xffff (logior (ash (%u8 (%c-round (* nr 15.0))) 12)
                                        (ash (%u8 (%c-round (* ng 15.0))) 8)
                                        (ash (%u8 (%c-round (* nb 15.0))) 4)
                                        (%u8 (%c-round (* na 15.0)))))))
          (+pixelformat-uncompressed-r8g8b8+
           (setf (aref data (* i 3)) r (aref data (+ (* i 3) 1)) g (aref data (+ (* i 3) 2)) b))
          (+pixelformat-uncompressed-r8g8b8a8+
           (%put-rgba data i r g b a))
          (+pixelformat-uncompressed-r32+
           (setf (%f32 data (* i 4)) (+ (* nr 0.299) (* ng 0.587) (* nb 0.114))))
          (+pixelformat-uncompressed-r32g32b32+
           (setf (%f32 data (* i 12)) nr (%f32 data (+ (* i 12) 4)) ng (%f32 data (+ (* i 12) 8)) nb))
          (+pixelformat-uncompressed-r32g32b32a32+
           (setf (%f32 data (* i 16)) nr (%f32 data (+ (* i 16) 4)) ng
                 (%f32 data (+ (* i 16) 8)) nb (%f32 data (+ (* i 16) 12)) na))
          (+pixelformat-uncompressed-r16+
           (setf (%u16 data (* i 2)) (%float-to-half (+ (* nr 0.299) (* ng 0.587) (* nb 0.114)))))
          (+pixelformat-uncompressed-r16g16b16+
           (setf (%u16 data (* i 6)) (%float-to-half nr) (%u16 data (+ (* i 6) 2)) (%float-to-half ng)
                 (%u16 data (+ (* i 6) 4)) (%float-to-half nb)))
          (+pixelformat-uncompressed-r16g16b16a16+
           (setf (%u16 data (* i 8)) (%float-to-half nr) (%u16 data (+ (* i 8) 2)) (%float-to-half ng)
                 (%u16 data (+ (* i 8) 4)) (%float-to-half nb) (%u16 data (+ (* i 8) 6)) (%float-to-half na)))))))
  nil)

(defun image-draw-pixel-v (dst position color)
  "Draw pixel within an image (Vector version)"
  (image-draw-pixel dst (truncate (%x position)) (truncate (%y position)) color))

(defun image-draw-line (dst start-pos-x start-pos-y end-pos-x end-pos-y color)
  "Draw line within an image"
  ;; Calculate differences in coordinates
  (let* ((start-pos-x (truncate start-pos-x)) (start-pos-y (truncate start-pos-y))
         (short-len (- (truncate end-pos-y) start-pos-y))
         (long-len (- (truncate end-pos-x) start-pos-x))
         (y-longer nil))
    ;; Determine if the line is more vertical than horizontal
    (when (> (abs short-len) (abs long-len))
      (rotatef short-len long-len)
      (setf y-longer t))
    ;; Initialize variables for drawing loop
    (let ((end-val (1+ long-len))
          (sgn-inc 1))
      ;; Adjust direction increment based on long-len sign
      (when (< long-len 0)
        (setf long-len (- long-len)
              sgn-inc -1)
        (decf end-val 2))
      ;; Calculate fixed-point increment for shorter length
      (let ((dec-inc (if (zerop long-len) 0 (truncate (ash short-len 16) long-len))))
        ;; Draw the line pixel by pixel
        (loop for i = 0 then (+ i sgn-inc)
              for j = (ash 1 15) then (+ j dec-inc)
              until (= i end-val)
              do (if y-longer
                     ;; If line is more vertical, iterate over y-axis
                     (image-draw-pixel dst (+ start-pos-x (ash j -16)) (+ start-pos-y i) color)
                     ;; If line is more horizontal, iterate over x-axis
                     (image-draw-pixel dst (+ start-pos-x i) (+ start-pos-y (ash j -16)) color))))))
  nil)

(defun image-draw-line-v (dst start end color)
  "Draw line within an image (Vector version)"
  ;; Round start and end positions to nearest integer coordinates
  (image-draw-line dst (truncate (+ (%x start) 0.5)) (truncate (+ (%y start) 0.5))
                   (truncate (+ (%x end) 0.5)) (truncate (+ (%y end) 0.5)) color))

(defun image-draw-line-ex (dst start end thick color)
  "Draw a line defining thickness within an image"
  ;; Round start and end positions to nearest integer coordinates
  (let* ((x1 (truncate (+ (%x start) 0.5)))
         (y1 (truncate (+ (%y start) 0.5)))
         (x2 (truncate (+ (%x end) 0.5)))
         (y2 (truncate (+ (%y end) 0.5)))
         ;; Calculate differences in x and y coordinates
         (dx (- x2 x1))
         (dy (- y2 y1)))
    ;; Draw the main line between (x1, y1) and (x2, y2), then the thickness lines around it
    (cond ((and (/= dx 0) (< (abs (truncate dy dx)) 1))
           ;; Line is more horizontal: draw additional lines vertically
           (let ((wy (1- thick)))
             (loop for i from 0 to (truncate (1+ wy) 2)
                   do (image-draw-line dst x1 (+ y1 i) x2 (+ y2 i) color))
             (loop for i from 1 to (truncate wy 2)
                   do (image-draw-line dst x1 (- y1 i) x2 (- y2 i) color))))
          ((/= dy 0)
           ;; Line is more vertical or perfectly horizontal: draw additional lines horizontally
           (let ((wx (1- thick)))
             (loop for i from 0 to (truncate (1+ wx) 2)
                   do (image-draw-line dst (+ x1 i) y1 (+ x2 i) y2 color))
             (loop for i from 1 to (truncate wx 2)
                   do (image-draw-line dst (- x1 i) y1 (- x2 i) y2 color))))))
  nil)

(defun image-draw-line-strip (dst points point-count color)
  "Draw a lines sequence within an image"
  (let ((points (coerce points 'simple-vector)))
    (dotimes (i (1- point-count))
      (image-draw-line-v dst (svref points i) (svref points (1+ i)) color)))
  nil)

(defun %triangle-setup (dst v1 v2 v3)
  "Bounding box and edge function steps shared by ImageDrawTriangle*()"
  (let* ((x1 (%x v1)) (y1 (%y v1)) (x2 (%x v2)) (y2 (%y v2)) (x3 (%x v3)) (y3 (%y v3))
         ;; Calculate the 2D bounding box of the triangle
         ;; Determine the minimum and maximum x and y coordinates of the triangle vertices
         (x-min (max 0 (truncate (min x1 x2 x3))))
         (y-min (max 0 (truncate (min y1 y2 y3))))
         (x-max (min (image-width dst) (truncate (max x1 x2 x3))))
         (y-max (min (image-height dst) (truncate (max y1 y2 y3))))
         ;; Check the order of the vertices to determine if it's a front or back face
         (signed-area (- (* (- x2 x1) (- y3 y1)) (* (- x3 x1) (- y2 y1))))
         (is-back-face (> signed-area 0))
         ;; Barycentric interpolation setup
         ;; Calculate the step increments for the barycentric coordinates
         (w1-x-step (truncate (- y3 y2))) (w1-y-step (truncate (- x2 x3)))
         (w2-x-step (truncate (- y1 y3))) (w2-y-step (truncate (- x3 x1)))
         (w3-x-step (truncate (- y2 y1))) (w3-y-step (truncate (- x1 x2))))
    ;; If the triangle is a back face, invert the steps
    (when is-back-face
      (setf w1-x-step (- w1-x-step) w1-y-step (- w1-y-step)
            w2-x-step (- w2-x-step) w2-y-step (- w2-y-step)
            w3-x-step (- w3-x-step) w3-y-step (- w3-y-step)))
    ;; Calculate the initial barycentric coordinates for the top-left point of the bounding box
    (values x-min y-min x-max y-max
            w1-x-step w1-y-step w2-x-step w2-y-step w3-x-step w3-y-step
            (truncate (+ (* (- x-min x2) w1-x-step) (* w1-y-step (- y-min y2))))
            (truncate (+ (* (- x-min x3) w2-x-step) (* w2-y-step (- y-min y3))))
            (truncate (+ (* (- x-min x1) w3-x-step) (* w3-y-step (- y-min y1)))))))

(defun image-draw-triangle (dst v1 v2 v3 color)
  "Draw triangle within an image"
  (multiple-value-bind (x-min y-min x-max y-max w1xs w1ys w2xs w2ys w3xs w3ys w1-row w2-row w3-row)
      (%triangle-setup dst v1 v2 v3)
    ;; Rasterization loop
    ;; Iterate through each pixel in the bounding box
    (loop for y from y-min to y-max
          do (let ((w1 w1-row) (w2 w2-row) (w3 w3-row))
               (loop for x from x-min to x-max
                     ;; Check if the pixel is inside the triangle using barycentric coordinates
                     ;; If it is then we can draw the pixel with the given color
                     do (when (>= (logior w1 w2 w3) 0) (image-draw-pixel dst x y color))
                        ;; Increment the barycentric coordinates for the next pixel
                        (incf w1 w1xs) (incf w2 w2xs) (incf w3 w3xs))
               ;; Move to the next row in the bounding box
               (incf w1-row w1ys) (incf w2-row w2ys) (incf w3-row w3ys))))
  nil)

(defun image-draw-triangle-gradient (dst v1 v2 v3 c1 c2 c3)
  "Draw triangle with interpolated colors within an image"
  (multiple-value-bind (x-min y-min x-max y-max w1xs w1ys w2xs w2ys w3xs w3ys w1-row w2-row w3-row)
      (%triangle-setup dst v1 v2 v3)
    (let* ((c1 (%col c1)) (c2 (%col c2)) (c3 (%col c3))
           ;; Calculate the inverse of the sum of the barycentric coordinates for normalization
           ;; NOTE 1: Here, we act as if we multiply by 255 the reciprocal, which avoids additional
           ;;         calculations in the loop. This is acceptable because we are only interpolating colors
           ;; NOTE 2: This sum remains constant throughout the triangle
           (sum (+ w1-row w2-row w3-row))
           (w-inv-sum (if (zerop sum) 0.0 (/ 255.0 sum))))
      (loop for y from y-min to y-max
            do (let ((w1 w1-row) (w2 w2-row) (w3 w3-row))
                 (loop for x from x-min to x-max
                       do (when (>= (logior w1 w2 w3) 0)
                            ;; Compute the normalized barycentric coordinates
                            (let ((aw1 (%u8 (* w1 w-inv-sum)))
                                  (aw2 (%u8 (* w2 w-inv-sum)))
                                  (aw3 (%u8 (* w3 w-inv-sum))))
                              ;; Interpolate the color using the barycentric coordinates
                              (image-draw-pixel dst x y
                                                (mapcar (lambda (a b c) (%u8 (floor (+ (* a aw1) (* b aw2) (* c aw3)) 255)))
                                                        c1 c2 c3))))
                          (incf w1 w1xs) (incf w2 w2xs) (incf w3 w3xs))
                 (incf w1-row w1ys) (incf w2-row w2ys) (incf w3-row w3ys)))))
  nil)

(defun image-draw-triangle-lines (dst v1 v2 v3 color)
  "Draw triangle outline within an image"
  (image-draw-line dst (truncate (%x v1)) (truncate (%y v1)) (truncate (%x v2)) (truncate (%y v2)) color)
  (image-draw-line dst (truncate (%x v2)) (truncate (%y v2)) (truncate (%x v3)) (truncate (%y v3)) color)
  (image-draw-line dst (truncate (%x v3)) (truncate (%y v3)) (truncate (%x v1)) (truncate (%y v1)) color))

;; NOTE: First vertex provided is the center, shared by all triangles
(defun image-draw-triangle-fan (dst points point-count color)
  "Draw a triangle fan defined by points within an image (first vertex is the center)"
  (when (>= point-count 3)
    (let ((points (coerce points 'simple-vector)))
      (loop for i from 1 below (1- point-count)
            do (image-draw-triangle dst (svref points 0) (svref points i) (svref points (1+ i)) color))))
  nil)

;; NOTE: Every new vertex connects with previous two
(defun image-draw-triangle-strip (dst points point-count color)
  "Draw a triangle strip defined by points within an image"
  (when (>= point-count 3)
    (let ((points (coerce points 'simple-vector)))
      (loop for i from 2 below point-count
            do (if (evenp i)
                   (image-draw-triangle dst (svref points i) (svref points (- i 2)) (svref points (- i 1)) color)
                   (image-draw-triangle dst (svref points i) (svref points (- i 1)) (svref points (- i 2)) color)))))
  nil)

(defun image-draw-rectangle (dst pos-x pos-y width height color)
  "Draw rectangle within an image"
  (image-draw-rectangle-rec dst (make-rectangle :x (float pos-x) :y (float pos-y)
                                                :width (float width) :height (float height))
                            color))

(defun image-draw-rectangle-v (dst position size color)
  "Draw rectangle within an image (Vector version)"
  (image-draw-rectangle dst (truncate (%x position)) (truncate (%y position))
                        (truncate (%x size)) (truncate (%y size)) color))

(defun image-draw-rectangle-rec (dst rec color)
  "Draw rectangle within an image"
  ;; Security check to avoid program crash
  (unless (%image-ready-p dst) (return-from image-draw-rectangle-rec nil))
  (multiple-value-bind (rx ry rw rh) (%rec rec)
    ;; Security check to avoid drawing out of bounds in case of bad user data
    (when (< rx 0) (incf rw rx) (setf rx 0.0))
    (when (< ry 0) (incf rh ry) (setf ry 0.0))
    (when (< rw 0) (setf rw 0.0))
    (when (< rh 0) (setf rh 0.0))
    ;; Clamp the size the the image bounds
    (when (>= (+ rx rw) (image-width dst)) (setf rw (- (image-width dst) rx)))
    (when (>= (+ ry rh) (image-height dst)) (setf rh (- (image-height dst) ry)))
    ;; Check if the rect is even inside the image
    (when (or (>= rx (image-width dst)) (>= ry (image-height dst))) (return-from image-draw-rectangle-rec nil))
    (when (or (<= (+ rx rw) 0) (<= (+ ry rh) 0)) (return-from image-draw-rectangle-rec nil))
    (let* ((sy (truncate ry))
           (sx (truncate rx))
           (w (truncate rw))
           (h (truncate rh))
           (bpp (%bytes-per-pixel (image-pixel-format dst)))
           (data (image-data dst)))
      ;; Fill in the first pixel of the first row based on image format
      (image-draw-pixel dst sx sy color)
      (let ((src (* (+ (* sy (image-width dst)) sx) bpp)))
        ;; Repeat the first pixel data throughout the row
        (loop for x = 1 then (* x 2)
              while (< x w)
              do (let ((pixels-to-copy (min x (- w x))))
                   (replace data data :start1 (+ src (* x bpp)) :start2 src :end2 (+ src (* pixels-to-copy bpp)))))
        ;; Repeat the first row data for all other rows
        (let ((bytes-per-row (* bpp w)))
          (loop for y from 1 below h
                do (replace data data :start1 (+ src (* y (image-width dst) bpp))
                                      :start2 src :end2 (+ src bytes-per-row)))))))
  nil)

(defun image-draw-rectangle-pro (dst rec origin rotation color)
  "Draw rectangle with rotation and origin within an image"
  (multiple-value-bind (rx ry rw rh) (%rec rec)
    (when (or (null dst) (null (image-data dst)) (<= rw 0) (<= rh 0))
      (return-from image-draw-rectangle-pro nil))
    (let* ((cos-angle (cos (* rotation +deg2rad+)))
           (sin-angle (sin (* rotation +deg2rad+)))
           (orx (%x origin)) (ory (%y origin))
           ;; Origin point in image space
           (ox (+ rx orx))
           (oy (+ ry ory))
           ;; Rectangle corners relative to origin
           (x1 (- orx)) (y1 (- ory))
           (x2 (- rw orx)) (y2 (- rh ory))
           (corners-x (list x1 x2 x2 x1))
           (corners-y (list y1 y1 y2 y2))
           (min-x 65536.0) (min-y 65536.0) (max-x -65536.0) (max-y -65536.0))
      ;; Compute bounding box of rotated rectangle
      (loop for cx in corners-x
            for cy in corners-y
            do (let ((px (+ (- (* cx cos-angle) (* cy sin-angle)) ox))
                     (py (+ (+ (* cx sin-angle) (* cy cos-angle)) oy)))
                 (setf min-x (min min-x px) min-y (min min-y py)
                       max-x (max max-x px) max-y (max max-y py))))
      (let ((x0 (max 0 (floor min-x)))
            (y0 (max 0 (floor min-y)))
            (x-end (min (image-width dst) (ceiling max-x)))
            (y-end (min (image-height dst) (ceiling max-y))))
        (when (or (>= x0 x-end) (>= y0 y-end)) (return-from image-draw-rectangle-pro nil))
        ;; For each pixel in bounding box, check if inside rotated rectangle
        (loop for y from y0 below y-end
              do (loop for x from x0 below x-end
                       do (let* ((px (- (+ x 0.5) ox))
                                 (py (- (+ y 0.5) oy))
                                 ;; Inverse rotate
                                 (local-x (+ (* px cos-angle) (* py sin-angle)))
                                 (local-y (+ (* (- px) sin-angle) (* py cos-angle))))
                            (when (and (>= local-x (- orx)) (< local-x (- rw orx))
                                       (>= local-y (- ory)) (< local-y (- rh ory)))
                              (image-draw-pixel dst x y color))))))))
  nil)

(defun image-draw-rectangle-lines (dst pos-x pos-y width height color)
  "Draw rectangle lines within an image"
  (image-draw-rectangle-lines-ex dst (make-rectangle :x (float pos-x) :y (float pos-y)
                                                     :width (float width) :height (float height))
                                 1 color))

(defun image-draw-rectangle-lines-ex (dst rec thick color)
  "Draw rectangle lines with line thickness within an image"
  (multiple-value-bind (x y w h) (%rec rec)
    (image-draw-rectangle dst (truncate x) (truncate y) (truncate w) thick color)
    (image-draw-rectangle dst (truncate x) (truncate (+ y thick)) thick (truncate (- h (* thick 2))) color)
    (image-draw-rectangle dst (truncate (- (+ x w) thick)) (truncate (+ y thick)) thick (truncate (- h (* thick 2))) color)
    (image-draw-rectangle dst (truncate x) (truncate (- (+ y h) thick)) (truncate w) thick color)))

(defun image-draw-rectangle-gradient-ex (dst rec col1 col2 col3 col4)
  "Draw rectangle with gradient within an image (colors counter-clockwise from top-left)"
  (multiple-value-bind (rx ry rw rh) (%rec rec)
    (when (or (null dst) (null (image-data dst)) (<= rw 0) (<= rh 0))
      (return-from image-draw-rectangle-gradient-ex nil))
    (let ((x0 (max 0 (floor rx)))
          (y0 (max 0 (floor ry)))
          (x1 (min (image-width dst) (ceiling (+ rx rw))))
          (y1 (min (image-height dst) (ceiling (+ ry rh))))
          (col1 (%col col1)) (col2 (%col col2)) (col3 (%col col3)) (col4 (%col col4)))
      (when (or (>= x0 x1) (>= y0 y1)) (return-from image-draw-rectangle-gradient-ex nil))
      (loop for y from y0 below y1
            do (let ((ty (max 0.0 (min 1.0 (/ (- y ry) rh)))))
                 (loop for x from x0 below x1
                       do (let* ((tx (max 0.0 (min 1.0 (/ (- x rx) rw))))
                                 ;; Bilinear interpolation weights
                                 (w1 (* (- 1.0 tx) (- 1.0 ty)))  ; top-left
                                 (w2 (* (- 1.0 tx) ty))          ; bottom-left
                                 (w3 (* tx ty))                  ; bottom-right
                                 (w4 (* tx (- 1.0 ty))))         ; top-right
                            (image-draw-pixel dst x y
                                              (mapcar (lambda (a b c d)
                                                        (%u8 (+ (* a w1) (* b w2) (* c w3) (* d w4))))
                                                      col1 col2 col3 col4))))))))
  nil)

;; NOTE: Based on https://en.wikipedia.org/wiki/Midpoint_circle_algorithm
(defun image-draw-circle (dst center-x center-y radius color)
  "Draw a filled circle within an image"
  (let ((x 0)
        (y radius)
        (decesion-parameter (- 3 (* 2 radius))))
    (loop while (>= y x)
          do (image-draw-rectangle dst (- center-x x) (+ center-y y) (1+ (* x 2)) 1 color)
             (image-draw-rectangle dst (- center-x x) (- center-y y) (1+ (* x 2)) 1 color)
             (image-draw-rectangle dst (- center-x y) (+ center-y x) (1+ (* y 2)) 1 color)
             (image-draw-rectangle dst (- center-x y) (- center-y x) (1+ (* y 2)) 1 color)
             (incf x)
             (if (> decesion-parameter 0)
                 (progn (decf y)
                        (setf decesion-parameter (+ decesion-parameter (* 4 (- x y)) 10)))
                 (setf decesion-parameter (+ decesion-parameter (* 4 x) 6))))))

(defun image-draw-circle-v (dst center radius color)
  "Draw a filled circle within an image (Vector version)"
  (image-draw-circle dst (truncate (%x center)) (truncate (%y center)) radius color))

(defun image-draw-circle-lines (dst center-x center-y radius color)
  "Draw circle outline within an image"
  (let ((x 0)
        (y radius)
        (decesion-parameter (- 3 (* 2 radius))))
    (loop while (>= y x)
          do (image-draw-pixel dst (+ center-x x) (+ center-y y) color)
             (image-draw-pixel dst (- center-x x) (+ center-y y) color)
             (image-draw-pixel dst (+ center-x x) (- center-y y) color)
             (image-draw-pixel dst (- center-x x) (- center-y y) color)
             (image-draw-pixel dst (+ center-x y) (+ center-y x) color)
             (image-draw-pixel dst (- center-x y) (+ center-y x) color)
             (image-draw-pixel dst (+ center-x y) (- center-y x) color)
             (image-draw-pixel dst (- center-x y) (- center-y x) color)
             (incf x)
             (if (> decesion-parameter 0)
                 (progn (decf y)
                        (setf decesion-parameter (+ decesion-parameter (* 4 (- x y)) 10)))
                 (setf decesion-parameter (+ decesion-parameter (* 4 x) 6))))))

(defun image-draw-circle-lines-v (dst center radius color)
  "Draw circle outline within an image (Vector version)"
  (image-draw-circle-lines dst (truncate (%x center)) (truncate (%y center)) radius color))

(defun image-draw-circle-gradient (dst center radius inner outer)
  "Draw a gradient-filled circle within an image"
  (when (or (null dst) (null (image-data dst)) (<= radius 0.0))
    (return-from image-draw-circle-gradient nil))
  (let* ((cx (%x center)) (cy (%y center))
         (x0 (max 0 (floor (- cx radius))))
         (y0 (max 0 (floor (- cy radius))))
         (x1 (min (image-width dst) (ceiling (+ cx radius))))
         (y1 (min (image-height dst) (ceiling (+ cy radius))))
         (inner (%col inner)) (outer (%col outer))
         (radius-sq (* radius radius)))
    (when (or (>= x0 x1) (>= y0 y1)) (return-from image-draw-circle-gradient nil))
    (loop for y from y0 below y1
          do (loop for x from x0 below x1
                   do (let* ((dx (- (+ x 0.5) cx))
                             (dy (- (+ y 0.5) cy))
                             (dist-sq (+ (* dx dx) (* dy dy))))
                        (unless (> dist-sq radius-sq)
                          (let ((tt (/ (sqrt dist-sq) radius)))
                            (image-draw-pixel dst x y
                                              (mapcar (lambda (i o) (%u8 (+ i (* (- o i) tt)))) inner outer))))))))
  nil)

(defun image-draw-image (dst src pos-x pos-y tint)
  "Draw a source image within a destination image (tint applied to source)"
  (let ((src-rec (make-rectangle :width (float (image-width src)) :height (float (image-height src)))))
    (image-draw-image-pro dst src src-rec
                          (make-rectangle :x (float pos-x) :y (float pos-y)
                                          :width (rectangle-width src-rec) :height (rectangle-height src-rec))
                          (vec2 0.0 0.0) 0.0 tint)))

(defun image-draw-image-ex (dst src position rotation scale tint)
  "Draw a source image within a destination image with rotation and scale"
  (when (or (null dst) (null (image-data dst)) (<= (image-width dst) 0) (<= (image-height dst) 0)
            (<= (image-width src) 0) (<= (image-height src) 0))
    (return-from image-draw-image-ex nil))
  (let* ((cos-a (cos (* rotation +deg2rad+)))
         (sin-a (sin (* rotation +deg2rad+)))
         (px0 (%x position)) (py0 (%y position))
         (sw (* (image-width src) scale))
         (sh (* (image-height src) scale))
         (min-x 65536.0) (min-y 65536.0) (max-x -65536.0) (max-y -65536.0)
         (tint (%col tint)))
    ;; Scaled source corners, rotated and translated to compute the bounding box
    (loop for cx in (list 0.0 sw sw 0.0)
          for cy in (list 0.0 0.0 sh sh)
          do (let ((rx (+ (- (* cx cos-a) (* cy sin-a)) px0))
                   (ry (+ (+ (* cx sin-a) (* cy cos-a)) py0)))
               (setf min-x (min min-x rx) min-y (min min-y ry)
                     max-x (max max-x rx) max-y (max max-y ry))))
    (let ((x0 (max 0 (floor min-x)))
          (y0 (max 0 (floor min-y)))
          (x1 (min (image-width dst) (ceiling max-x)))
          (y1 (min (image-height dst) (ceiling max-y))))
      (when (or (>= x0 x1) (>= y0 y1)) (return-from image-draw-image-ex nil))
      (loop for y from y0 below y1
            do (loop for x from x0 below x1
                     do (let* ((dx (- (+ x 0.5) px0))
                               (dy (- (+ y 0.5) py0))
                               ;; Inverse rotation, then inverse scale
                               (sx (/ (+ (* dx cos-a) (* dy sin-a)) scale))
                               (sy (/ (+ (* (- dx) sin-a) (* dy cos-a)) scale)))
                          (unless (or (< sx 0.0) (< sy 0.0) (>= sx (image-width src)) (>= sy (image-height src)))
                            (let ((src-color (mapcar (lambda (c tc) (%u8 (/ (* c tc) 255.0)))
                                                     (get-image-color src (truncate sx) (truncate sy)) tint)))
                              (unless (zerop (fourth src-color))
                                (let* ((dst-color (get-image-color dst x y))
                                       (alpha (fourth src-color))
                                       (inv-a (- 255 alpha)))
                                  (image-draw-pixel dst x y
                                                    (list (%u8 (floor (+ (* (first src-color) alpha) (* (first dst-color) inv-a)) 255))
                                                          (%u8 (floor (+ (* (second src-color) alpha) (* (second dst-color) inv-a)) 255))
                                                          (%u8 (floor (+ (* (third src-color) alpha) (* (third dst-color) inv-a)) 255))
                                                          (%u8 (+ alpha (floor (* (fourth dst-color) inv-a) 255))))))))))))))
  nil)

(defun image-draw-image-rec (dst src src-rec position tint)
  "Draw a source image piece within a destination image"
  (multiple-value-bind (sx sy sw sh) (%rec src-rec)
    (declare (ignore sx sy))
    (image-draw-image-pro dst src src-rec
                          (make-rectangle :x (%x position) :y (%y position) :width sw :height sh)
                          (vec2 0.0 0.0) 0.0 tint)))

;; NOTE: Color tint is applied to source image; ORIGIN and ROTATION are currently not used by raylib
(defun image-draw-image-pro (dst src src-rec dst-rec origin rotation tint)
  "Draw a source image within a destination image with pro parameters"
  (declare (ignore origin rotation))
  ;; Security check to avoid program crash
  (when (or (null dst) (null (image-data dst)) (zerop (image-width dst)) (zerop (image-height dst))
            (null (image-data src)) (zerop (image-width src)) (zerop (image-height src)))
    (return-from image-draw-image-pro nil))
  (when (%compressed-format-p (image-pixel-format dst))
    (trace-log-warning "Image drawing not supported for compressed formats")
    (return-from image-draw-image-pro nil))
  (let ((tint (%col tint))
        (src-ptr src))                  ; Pointer to source image
    (multiple-value-bind (sx sy sw sh) (%rec src-rec)
      (multiple-value-bind (dx dy dw dh) (%rec dst-rec)
        ;; Source rectangle out-of-bounds security checks
        (when (< sx 0) (incf sw sx) (setf sx 0.0))
        (when (< sy 0) (incf sh sy) (setf sy 0.0))
        (when (> (+ sx sw) (image-width src)) (setf sw (- (image-width src) sx)))
        (when (> (+ sy sh) (image-height src)) (setf sh (- (image-height src) sy)))
        ;; Check if source rectangle needs to be resized to destination rectangle
        ;; In that case, we make a copy of source, and we apply all required transform
        (when (or (/= (truncate sw) (truncate dw)) (/= (truncate sh) (truncate dh)))
          (setf src-ptr (image-from-image src (make-rectangle :x sx :y sy :width sw :height sh)))
          (image-resize src-ptr (truncate dw) (truncate dh)) ; Resize to destination rectangle
          (setf sx 0.0 sy 0.0
                sw (float (image-width src-ptr)) sh (float (image-height src-ptr))))
        ;; Destination rectangle out-of-bounds security checks
        (cond ((< dx 0) (decf sx dx) (incf sw dx) (setf dx 0.0))
              ((> (+ dx sw) (image-width dst)) (setf sw (- (image-width dst) dx))))
        (cond ((< dy 0) (decf sy dy) (incf sh dy) (setf dy 0.0))
              ((> (+ dy sh) (image-height dst)) (setf sh (- (image-height dst) dy))))
        (when (< (image-width dst) sw) (setf sw (float (image-width dst))))
        (when (< (image-height dst) sh) (setf sh (float (image-height dst))))
        ;; Blit and blend: color tint applied to source; blending is not required for opaque sources
        (let* ((src-format (image-pixel-format src-ptr))
               (dst-format (image-pixel-format dst))
               (blend-required (not (and (= (fourth tint) 255)
                                         (member src-format (list +pixelformat-uncompressed-grayscale+
                                                                  +pixelformat-uncompressed-r5g6b5+
                                                                  +pixelformat-uncompressed-r8g8b8+
                                                                  +pixelformat-uncompressed-r32+
                                                                  +pixelformat-uncompressed-r32g32b32+
                                                                  +pixelformat-uncompressed-r16+
                                                                  +pixelformat-uncompressed-r16g16b16+)))))
               (stride-dst (get-pixel-data-size (image-width dst) 1 dst-format))
               (bpp-dst (floor stride-dst (image-width dst)))
               (stride-src (get-pixel-data-size (image-width src-ptr) 1 src-format))
               (bpp-src (floor stride-src (image-width src-ptr)))
               (src-data (image-data src-ptr))
               (dst-data (image-data dst))
               (src-base (* (+ (* (truncate sy) (image-width src-ptr)) (truncate sx)) bpp-src))
               (dst-base (* (+ (* (truncate dy) (image-width dst)) (truncate dx)) bpp-dst)))
          (dotimes (y (truncate sh))
            (if (and (not blend-required) (= src-format dst-format))
                (replace dst-data src-data :start1 dst-base :start2 src-base
                                           :end2 (+ src-base (* (truncate sw) bpp-src)))
                (loop for x from 0 below (truncate sw)
                      for p-src from src-base by bpp-src
                      for p-dst from dst-base by bpp-dst
                      do (let* ((col-src (get-pixel-color src-data src-format p-src))
                                (col-dst (get-pixel-color dst-data dst-format p-dst))
                                (col-blend (if blend-required (color-alpha-blend col-dst col-src tint) col-src)))
                           (set-pixel-color dst-data col-blend dst-format p-dst))))
            (incf src-base stride-src)
            (incf dst-base stride-dst)))
        ;; Draw on the next mipmap level, if both images have mipmaps
        (when (and (> (image-mipmap-count dst) 1) (> (image-mipmap-count src) 1))
          (let* ((dst-offset (get-pixel-data-size (image-width dst) (image-height dst) (image-pixel-format dst)))
                 (src-offset (get-pixel-data-size (image-width src) (image-height src) (image-pixel-format src)))
                 (mipmap-dst (make-image :data (subseq (image-data dst) dst-offset)
                                         :width (floor (image-width dst) 2) :height (floor (image-height dst) 2)
                                         :mipmaps (1- (image-mipmap-count dst)) :format (image-pixel-format dst)))
                 (mipmap-src (make-image :data (subseq (image-data src) src-offset)
                                         :width (floor (image-width src) 2) :height (floor (image-height src) 2)
                                         :mipmaps (1- (image-mipmap-count src)) :format (image-pixel-format src))))
            (image-draw-image-pro mipmap-dst mipmap-src
                                  (make-rectangle :x (/ sx 2) :y (/ sy 2) :width (/ sw 2) :height (/ sh 2))
                                  (make-rectangle :x (/ dx 2) :y (/ dy 2) :width (/ dw 2) :height (/ dh 2))
                                  (vec2 0.0 0.0) 0.0 tint)
            (replace (image-data dst) (image-data mipmap-dst) :start1 dst-offset))))))
  nil)

(defun image-draw-text (dst text pos-x pos-y font-size color)
  "Draw text (using default font) within an image (destination)"
  ;; Make sure default font is loaded to be used on image text drawing
  (when (or (null (get-font-default)) (null (font-texture (get-font-default)))
            (zerop (texture-id (font-texture (get-font-default)))))
    (load-font-default))
  (image-draw-text-ex dst (get-font-default) text (vec2 (float pos-x) (float pos-y))
                      (float font-size) 1.0 color))

(defun image-draw-text-ex (dst font text position font-size spacing tint)
  "Draw text (custom sprite font) within an image (destination)"
  (let ((im-text (image-text-ex font text font-size spacing tint)))
    (image-draw-image-pro dst im-text
                          (make-rectangle :width (float (image-width im-text)) :height (float (image-height im-text)))
                          (make-rectangle :x (%x position) :y (%y position)
                                          :width (float (image-width im-text)) :height (float (image-height im-text)))
                          (vec2 0.0 0.0) 0.0 +white+)
    (unload-image im-text))
  nil)

(defun image-draw-text-pro (dst font text position origin rotation font-size spacing tint)
  "Draw text (custom sprite font) within an image with rotation (not implemented by raylib yet)"
  (declare (ignore dst font text position origin rotation font-size spacing tint))
  nil)

;;;------------------------------------------------------------------------------------
;;; Texture loading functions
;;;------------------------------------------------------------------------------------

;;; Initialize texture system
(defun init-texture-system ()
  "Initialize the texture system and create default texture"
  (unless *default-texture*
    (setf *default-texture* (create-default-texture))))

(defun create-default-texture ()
  "Create a default 1x1 white texture"
  (let ((white-image (gen-image-color 1 1 +white+)))
    (load-texture-from-image white-image)))

(defun load-texture (file-name)
  "Load texture from file into GPU memory (VRAM)"
  (let ((texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0))
        (image (load-image file-name)))
    (when (image-data image)
      (setf texture (load-texture-from-image image))
      (unload-image image))
    texture))

;; NOTE: image is not unloaded, it must be done manually
(defun load-texture-from-image (image)
  "Load texture from image data"
  (let ((texture (make-texture :id 0 :width (image-width image) :height (image-height image)
                               :mipmaps (image-mipmap-count image) :format (image-pixel-format image))))
    (if (and (/= (image-width image) 0) (/= (image-height image) 0))
        (setf (texture-id texture) (rl-load-texture (image-data image) (image-width image) (image-height image)
                                                    (image-pixel-format image) (image-mipmap-count image)))
        (trace-log-warning "IMAGE: Data is not valid to load texture"))
    ;; Register texture for cleanup-texture-system
    (when (> (texture-id texture) 0)
      (setf (gethash (texture-id texture) *texture-registry*) texture))
    texture))

(defun load-texture-cubemap (image layout)
  "Load cubemap from image, multiple image cubemap layouts supported"
  (let ((cubemap (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0))
        (iw (image-width image))
        (ih (image-height image)))
    (if (= layout +cubemap-layout-auto-detect+)   ; Try to automatically guess layout type
        ;; Check image width/height to determine the type of cubemap provided
        (cond ((> iw ih)
               (cond ((= (floor iw 6) ih)
                      (setf layout +cubemap-layout-line-horizontal+ (texture-width cubemap) (floor iw 6)))
                     ((= (floor iw 4) (floor ih 3))
                      (setf layout +cubemap-layout-cross-four-by-three+ (texture-width cubemap) (floor iw 4)))))
              ((> ih iw)
               (cond ((= (floor ih 6) iw)
                      (setf layout +cubemap-layout-line-vertical+ (texture-width cubemap) (floor ih 6)))
                     ((= (floor iw 3) (floor ih 4))
                      (setf layout +cubemap-layout-cross-three-by-four+ (texture-width cubemap) (floor iw 3))))))
        (setf (texture-width cubemap)
              (alexandria:switch (layout)
                (+cubemap-layout-line-vertical+ (floor ih 6))
                (+cubemap-layout-line-horizontal+ (floor iw 6))
                (+cubemap-layout-cross-three-by-four+ (floor iw 3))
                (+cubemap-layout-cross-four-by-three+ (floor iw 4))
                (t 0))))
    (setf (texture-height cubemap) (texture-width cubemap))
    ;; Layout provided or already auto-detected
    (if (/= layout +cubemap-layout-auto-detect+)
        (let* ((size (texture-width cubemap))
               (face-recs (loop repeat 6 collect (make-rectangle :width (float size) :height (float size))))
               (faces nil))
          (if (= layout +cubemap-layout-line-vertical+)
              (setf faces (image-copy image))        ; Image data already follows expected convention
              (progn
                (flet ((set-rec (i x y)
                         (setf (rectangle-x (nth i face-recs)) (float (* size x))
                               (rectangle-y (nth i face-recs)) (float (* size y)))))
                  (alexandria:switch (layout)
                    (+cubemap-layout-line-horizontal+ (dotimes (i 6) (set-rec i i 0)))
                    (+cubemap-layout-cross-three-by-four+
                     (set-rec 0 1 1) (set-rec 1 1 3) (set-rec 2 1 0) (set-rec 3 1 2) (set-rec 4 0 1) (set-rec 5 2 1))
                    (+cubemap-layout-cross-four-by-three+
                     (set-rec 0 2 1) (set-rec 1 0 1) (set-rec 2 1 0) (set-rec 3 1 2) (set-rec 4 1 1) (set-rec 5 3 1))))
                ;; Convert image data to 6 faces in a vertical column, that's the optimum layout for loading
                ;; NOTE: Image formatting does not work with compressed textures
                (setf faces (gen-image-color size (* size 6) +magenta+))
                (image-format faces (image-pixel-format image))
                (let ((mipmapped (image-copy image)))
                  (when (> (image-mipmap-count image) 1)
                    (image-mipmaps mipmapped)
                    (image-mipmaps faces))
                  (dotimes (i 6)
                    (image-draw-image-pro faces mipmapped (nth i face-recs)
                                          (make-rectangle :y (float (* size i)) :width (float size) :height (float size))
                                          (vec2 0.0 0.0) 0.0 +white+))
                  (unload-image mipmapped))))
          ;; NOTE: Cubemap data is expected to be provided as 6 images in a single data array,
          ;; one after the other (that's a vertical image), following convention: +X, -X, +Y, -Y, +Z, -Z
          (setf (texture-id cubemap) (rl-load-texture-cubemap (image-data faces) size (image-pixel-format faces)
                                                              (image-mipmap-count faces)))
          (if (/= (texture-id cubemap) 0)
              (setf (texture-format cubemap) (image-pixel-format faces)
                    (texture-mipmaps cubemap) (image-mipmap-count faces))
              (trace-log-warning "IMAGE: Failed to load cubemap image"))
          (unload-image faces))
        (trace-log-warning "IMAGE: Failed to detect cubemap image layout"))
    cubemap))

(defun load-render-texture (width height)
  "Load texture for rendering (framebuffer)"
  (load-render-texture-ex width height +pixelformat-uncompressed-r8g8b8a8+))

;; NOTE: Render texture is loaded by default with RGBA color attachment and depth RenderBuffer
(defun load-render-texture-ex (width height format)
  "Load texture for rendering (framebuffer), with specific format"
  (let ((target (make-render-texture)))
    (when (>= format +pixelformat-compressed-dxt1-rgb+)
      (trace-log-warning "FBO: Render texture format not supported")
      (return-from load-render-texture-ex target))
    (setf (render-texture-id target) (rl-load-framebuffer width height)) ; Load an empty framebuffer
    (if (> (render-texture-id target) 0)
        (progn
          (rl-enable-framebuffer (render-texture-id target))
          ;; Create color texture (default to RGBA)
          (setf (render-texture-texture target)
                (make-texture :id (rl-load-texture nil width height format 1)
                              :width width :height height :format format :mipmaps 1))
          ;; Create depth renderbuffer/texture
          (setf (render-texture-depth target) (rl-load-texture-depth width height t))
          ;; Attach color texture and depth renderbuffer/texture to FBO
          (rl-framebuffer-attach (render-texture-id target) (texture-id (render-texture-texture target))
                                 +rl-attachment-color-channel0+ +rl-attachment-texture2d+ 0)
          (rl-framebuffer-attach (render-texture-id target) (texture-id (render-texture-depth target))
                                 +rl-attachment-depth+ +rl-attachment-renderbuffer+ 0)
          ;; Check if fbo is complete with attachments (valid)
          (when (rl-framebuffer-complete (render-texture-id target))
            (trace-log-info "FBO: [ID ~d] Framebuffer object created successfully" (render-texture-id target)))
          (rl-disable-framebuffer))
        (trace-log-warning "FBO: Framebuffer object can not be created"))
    target))

(defun is-texture-valid (texture)
  "Check if a texture is valid (loaded in GPU)"
  (and texture
       (texture-p texture)
       (> (texture-id texture) 0)        ; Validate OpenGL id (texture uploaded to GPU)
       (> (texture-width texture) 0)     ; Validate texture width
       (> (texture-height texture) 0)    ; Validate texture height
       (> (texture-format texture) 0)    ; Validate texture pixel format
       (> (texture-mipmaps texture) 0)   ; Validate texture mipmaps (at least 1 for basic mipmap level)
       t))

(defun unload-texture (texture)
  "Unload texture from GPU memory (VRAM)"
  (when (and texture (> (texture-id texture) 0))
    (rl-unload-texture (texture-id texture))
    (remhash (texture-id texture) *texture-registry*)
    (trace-log-info "TEXTURE: [ID ~d] Unloaded texture data from VRAM (GPU)" (texture-id texture))
    (setf (texture-id texture) 0))
  nil)

(defun is-render-texture-valid (target)
  "Check if a render texture is valid (loaded in GPU)"
  (and target
       (> (render-texture-id target) 0)                    ; Validate OpenGL id (loaded on GPU)
       (is-texture-valid (render-texture-depth target))    ; Validate FBO depth texture/renderbuffer attachment
       (is-texture-valid (render-texture-texture target))  ; Validate FBO texture attachment
       t))

(defun unload-render-texture (target)
  "Unload render texture from GPU memory (VRAM)"
  (when (and target (> (render-texture-id target) 0))
    (when (and (render-texture-texture target) (> (texture-id (render-texture-texture target)) 0))
      ;; Color texture attached to FBO is deleted
      (rl-unload-texture (texture-id (render-texture-texture target))))
    ;; NOTE: Depth renderbuffer is deleted before deleting framebuffer
    (when (and (render-texture-depth target) (> (texture-id (render-texture-depth target)) 0))
      (gl:delete-renderbuffers (list (texture-id (render-texture-depth target)))))
    (gl:delete-framebuffers (list (render-texture-id target)))
    (trace-log-info "FBO: [ID ~d] Unloaded framebuffer from VRAM (GPU)" (render-texture-id target))
    (setf (render-texture-id target) 0))
  nil)

(defun %pixel-bytes (pixels)
  "Pixel data as a byte vector: byte vectors pass through, images give their data,
   a vector of colors (as returned by load-image-colors) is packed as R8G8B8A8"
  (cond ((image-p pixels) (image-data pixels))
        ((typep pixels '(simple-array (unsigned-byte 8) (*))) pixels)
        (t (let ((out (%make-octets (* 4 (length pixels)))))
             (loop for color across pixels
                   for i from 0
                   do (apply #'%put-rgba out i (%col color)))
             out))))

;; NOTE 1: pixels data must match texture.format (a byte vector, an image or a Color vector)
;; NOTE 2: pixels data must contain at least as many pixels as texture
(defun update-texture (texture pixels)
  "Update GPU texture with new data"
  (rl-update-texture (texture-id texture) 0 0 (texture-width texture) (texture-height texture)
                     (texture-format texture) (%pixel-bytes pixels)))

;; NOTE 3: rec must fit completely within texture's width and height
(defun update-texture-rec (texture rec pixels)
  "Update GPU texture rectangle with new data"
  (multiple-value-bind (x y w h) (%rec rec)
    (rl-update-texture (texture-id texture) (truncate x) (truncate y) (truncate w) (truncate h)
                       (texture-format texture) (%pixel-bytes pixels))))

;;;------------------------------------------------------------------------------------
;;; Texture configuration functions
;;;------------------------------------------------------------------------------------

;; NOTE: Updates the texture mipmaps count (C passes Texture2D *)
(defun gen-texture-mipmaps (texture)
  "Generate GPU mipmaps for a texture"
  (setf (texture-mipmaps texture)
        (rl-gen-texture-mipmaps (texture-id texture) (texture-width texture) (texture-height texture)
                                (texture-format texture)))
  nil)

(defun set-texture-filter (texture filter)
  "Set texture scaling filter mode"
  (let ((id (texture-id texture))
        (mipmapped (> (texture-mipmaps texture) 1)))
    (alexandria:switch (filter)
      (+texture-filter-point+
       (if mipmapped
           ;; RL_TEXTURE_FILTER_MIP_NEAREST - tex filter: POINT, mipmaps filter: POINT (sharp switching between mipmaps)
           (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-mip-nearest+)
           ;; RL_TEXTURE_FILTER_NEAREST - tex filter: POINT (no filter), no mipmaps
           (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-nearest+))
       (rl-texture-parameters id +rl-texture-mag-filter+ +rl-texture-filter-nearest+))
      (+texture-filter-bilinear+
       (if mipmapped
           ;; RL_TEXTURE_FILTER_LINEAR_MIP_NEAREST - tex filter: BILINEAR, mipmaps filter: POINT (sharp switching between mipmaps)
           (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-linear-mip-nearest+)
           ;; RL_TEXTURE_FILTER_LINEAR - tex filter: BILINEAR, no mipmaps
           (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-linear+))
       (rl-texture-parameters id +rl-texture-mag-filter+ +rl-texture-filter-linear+))
      (+texture-filter-trilinear+
       (if mipmapped
           ;; RL_TEXTURE_FILTER_MIP_LINEAR - tex filter: BILINEAR, mipmaps filter: BILINEAR (smooth transition between mipmaps)
           (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-mip-linear+)
           (progn
             (trace-log-warning "TEXTURE: [ID ~d] No mipmaps available for TRILINEAR texture filtering" id)
             ;; RL_TEXTURE_FILTER_LINEAR - tex filter: BILINEAR, no mipmaps
             (rl-texture-parameters id +rl-texture-min-filter+ +rl-texture-filter-linear+)))
       (rl-texture-parameters id +rl-texture-mag-filter+ +rl-texture-filter-linear+))
      (+texture-filter-anisotropic-4x+ (rl-texture-parameters id +rl-texture-filter-anisotropic+ 4))
      (+texture-filter-anisotropic-8x+ (rl-texture-parameters id +rl-texture-filter-anisotropic+ 8))
      (+texture-filter-anisotropic-16x+ (rl-texture-parameters id +rl-texture-filter-anisotropic+ 16))))
  nil)

(defun set-texture-wrap (texture wrap)
  "Set texture wrapping mode"
  (let ((id (texture-id texture))
        (mode (alexandria:switch (wrap)
                ;; NOTE: It only works if NPOT textures are supported, i.e. OpenGL ES 2.0 could not support it
                (+texture-wrap-repeat+ +rl-texture-wrap-repeat+)
                (+texture-wrap-clamp+ +rl-texture-wrap-clamp+)
                (+texture-wrap-mirror-repeat+ +rl-texture-wrap-mirror-repeat+)
                (+texture-wrap-mirror-clamp+ +rl-texture-wrap-mirror-clamp+)
                (t nil))))
    (when mode
      (rl-texture-parameters id +rl-texture-wrap-s+ mode)
      (rl-texture-parameters id +rl-texture-wrap-t+ mode)))
  nil)

;;;------------------------------------------------------------------------------------
;;; Texture drawing functions
;;;------------------------------------------------------------------------------------

(defun bind-texture-safe (texture)
  "Bind texture for drawing with safety checks (not part of raylib)"
  (let ((texture-id (if (and texture (is-texture-valid texture))
                        (texture-id texture)
                        (if *default-texture* (texture-id *default-texture*) 0))))
    (unless (= texture-id *current-texture-id*)
      (gl:bind-texture :texture-2d texture-id)
      (setf *current-texture-id* texture-id))))

(defun setup-texture-drawing ()
  "Setup OpenGL state for texture drawing (not part of raylib)"
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))

(defun draw-texture (texture pos-x pos-y tint)
  "Draw a Texture2D"
  (draw-texture-ex texture (vec2 (float pos-x 1.0) (float pos-y 1.0)) 0.0 1.0 tint))

(defun draw-texture-v (texture position tint)
  "Draw a Texture2D with position defined as Vector2"
  (draw-texture-ex texture position 0.0 1.0 tint))

(defun draw-texture-ex (texture position rotation scale tint)
  "Draw a Texture2D with extended parameters"
  (let ((source (make-rectangle :width (float (texture-width texture)) :height (float (texture-height texture))))
        (dest (make-rectangle :x (%x position) :y (%y position)
                              :width (float (* (texture-width texture) scale))
                              :height (float (* (texture-height texture) scale)))))
    (draw-texture-pro texture source dest (vec2 0.0 0.0) rotation tint)))

(defun draw-texture-rec (texture source position tint)
  "Draw a part of a texture defined by a rectangle"
  (multiple-value-bind (sx sy sw sh) (%rec source)
    (declare (ignore sx sy))
    (draw-texture-pro texture source
                      (make-rectangle :x (%x position) :y (%y position) :width (abs sw) :height (abs sh))
                      (vec2 0.0 0.0) 0.0 tint)))

(defun %tex-vertex (u v x y)
  (rl-tex-coord2f (float u 1.0) (float v 1.0))
  (rl-vertex2f (float x 1.0) (float y 1.0)))

;; NOTE: origin is relative to destination rectangle size
(defun draw-texture-pro (texture source dest origin rotation tint)
  "Draw a part of a texture defined by a rectangle with 'pro' parameters"
  ;; Check if texture is valid
  (when (and texture (> (texture-id texture) 0))
    (multiple-value-bind (sx sy sw sh) (%rec source)
      (multiple-value-bind (dx dy dw dh) (%rec dest)
        (let ((width (float (texture-width texture)))
              (height (float (texture-height texture)))
              (flip-x nil)
              (ox (%x origin)) (oy (%y origin))
              tl-x tl-y tr-x tr-y bl-x bl-y br-x br-y)
          (when (< sw 0) (setf flip-x t sw (- sw)))
          (when (< sh 0) (decf sy sh))
          (when (< dw 0) (setf dw (- dw)))
          (when (< dh 0) (setf dh (- dh)))
          ;; Only calculate rotation if needed
          (if (zerop rotation)
              (let ((x (- dx ox)) (y (- dy oy)))
                (setf tl-x x tl-y y
                      tr-x (+ x dw) tr-y y
                      bl-x x bl-y (+ y dh)
                      br-x (+ x dw) br-y (+ y dh)))
              (let ((sin-r (sin (* rotation +deg2rad+)))
                    (cos-r (cos (* rotation +deg2rad+)))
                    (rx (- ox)) (ry (- oy)))
                (setf tl-x (+ dx (* rx cos-r) (- (* ry sin-r)))
                      tl-y (+ dy (* rx sin-r) (* ry cos-r))
                      tr-x (+ dx (* (+ rx dw) cos-r) (- (* ry sin-r)))
                      tr-y (+ dy (* (+ rx dw) sin-r) (* ry cos-r))
                      bl-x (+ dx (* rx cos-r) (- (* (+ ry dh) sin-r)))
                      bl-y (+ dy (* rx sin-r) (* (+ ry dh) cos-r))
                      br-x (+ dx (* (+ rx dw) cos-r) (- (* (+ ry dh) sin-r)))
                      br-y (+ dy (* (+ rx dw) sin-r) (* (+ ry dh) cos-r)))))
          (rl-set-texture (texture-id texture))
          (rl-begin +rl-quads+)
          (destructuring-bind (r g b a) (keyword-to-color tint)
            (rl-color4ub r g b a))
          (rl-normal3f 0.0 0.0 1.0)                 ; Normal vector pointing towards viewer
          (let ((u0 (/ sx width)) (u1 (/ (+ sx sw) width))
                (v0 (/ sy height)) (v1 (/ (+ sy sh) height)))
            (when flip-x (rotatef u0 u1))
            ;; Top-left corner for texture and quad
            (%tex-vertex u0 v0 tl-x tl-y)
            ;; Bottom-left corner for texture and quad
            (%tex-vertex u0 v1 bl-x bl-y)
            ;; Bottom-right corner for texture and quad
            (%tex-vertex u1 v1 br-x br-y)
            ;; Top-right corner for texture and quad
            (%tex-vertex u1 v0 tr-x tr-y))
          (rl-end)
          (rl-set-texture 0)))))
  nil)

(defun draw-texture-npatch (texture npatch-info dest origin rotation tint)
  "Draws a texture (or part of it) that stretches or shrinks nicely"
  (when (and texture (> (texture-id texture) 0))
    (multiple-value-bind (sx sy sw sh) (%rec (npatch-info-source npatch-info))
      (multiple-value-bind (dx dy dw dh) (%rec dest)
        (let* ((width (float (texture-width texture)))
               (height (float (texture-height texture)))
               (layout (npatch-info-layout npatch-info))
               (patch-width (if (<= (truncate dw) 0) 0.0 dw))
               (patch-height (if (<= (truncate dh) 0) 0.0 dh))
               (draw-center t)
               (draw-middle t)
               (left-border (float (npatch-info-left npatch-info)))
               (top-border (float (npatch-info-top npatch-info)))
               (right-border (float (npatch-info-right npatch-info)))
               (bottom-border (float (npatch-info-bottom npatch-info))))
          (when (< sw 0) (decf sx sw))
          (when (< sh 0) (decf sy sh))
          (when (= layout +npatch-three-patch-horizontal+) (setf patch-height sh))
          (when (= layout +npatch-three-patch-vertical+) (setf patch-width sw))
          ;; Adjust the lateral (left and right) border widths in case patchWidth < texture.width
          (when (and (<= patch-width (+ left-border right-border)) (/= layout +npatch-three-patch-vertical+))
            (setf draw-center nil
                  left-border (* (/ left-border (+ left-border right-border)) patch-width)
                  right-border (- patch-width left-border)))
          ;; Adjust the lateral (top and bottom) border heights in case patchHeight < texture.height
          (when (and (<= patch-height (+ top-border bottom-border)) (/= layout +npatch-three-patch-horizontal+))
            (setf draw-middle nil
                  top-border (* (/ top-border (+ top-border bottom-border)) patch-height)
                  bottom-border (- patch-height top-border)))
          (let (;; Vertices: outer left/top, inner left/top, inner right/bottom, outer right/bottom
                (ax 0.0) (ay 0.0)
                (bx left-border) (by top-border)
                (cx (- patch-width right-border)) (cy (- patch-height bottom-border))
                (ex patch-width) (ey patch-height)
                ;; Texture coordinates
                (tax (/ sx width)) (tay (/ sy height))
                (tbx (/ (+ sx left-border) width)) (tby (/ (+ sy top-border) height))
                (tcx (/ (+ sx (- sw right-border)) width)) (tcy (/ (+ sy (- sh bottom-border)) height))
                (tdx (/ (+ sx sw) width)) (tdy (/ (+ sy sh) height)))
            (flet ((quad (u0 v0 x0 y0 u1 v1 x1 y1)
                     ;; Bottom-left, bottom-right, top-right, top-left corners for texture and quad
                     (%tex-vertex u0 v1 x0 y1)
                     (%tex-vertex u1 v1 x1 y1)
                     (%tex-vertex u1 v0 x1 y0)
                     (%tex-vertex u0 v0 x0 y0)))
              (rl-set-texture (texture-id texture))
              (rl-push-matrix)
              (rl-translatef dx dy 0.0)
              (rl-rotatef (float rotation 1.0) 0.0 0.0 1.0)
              (rl-translatef (- (%x origin)) (- (%y origin)) 0.0)
              (rl-begin +rl-quads+)
              (destructuring-bind (r g b a) (keyword-to-color tint)
                (rl-color4ub r g b a))
              (rl-normal3f 0.0 0.0 1.0)            ; Normal vector pointing towards viewer
              (alexandria:switch (layout)
                (+npatch-nine-patch+
                 ;; TOP-LEFT QUAD, TOP-CENTER QUAD, TOP-RIGHT QUAD
                 (quad tax tay ax ay tbx tby bx by)
                 (when draw-center (quad tbx tay bx ay tcx tby cx by))
                 (quad tcx tay cx ay tdx tby ex by)
                 (when draw-middle
                   ;; MIDDLE-LEFT QUAD, MIDDLE-CENTER QUAD, MIDDLE-RIGHT QUAD
                   (quad tax tby ax by tbx tcy bx cy)
                   (when draw-center (quad tbx tby bx by tcx tcy cx cy))
                   (quad tcx tby cx by tdx tcy ex cy))
                 ;; BOTTOM-LEFT QUAD, BOTTOM-CENTER QUAD, BOTTOM-RIGHT QUAD
                 (quad tax tcy ax cy tbx tdy bx ey)
                 (when draw-center (quad tbx tcy bx cy tcx tdy cx ey))
                 (quad tcx tcy cx cy tdx tdy ex ey))
                (+npatch-three-patch-vertical+
                 ;; TOP QUAD, MIDDLE QUAD, BOTTOM QUAD
                 (quad tax tay ax ay tdx tby ex by)
                 (when draw-center (quad tax tby ax by tdx tcy ex cy))
                 (quad tax tcy ax cy tdx tdy ex ey))
                (+npatch-three-patch-horizontal+
                 ;; LEFT QUAD, CENTER QUAD, RIGHT QUAD
                 (quad tax tay ax ay tbx tdy bx ey)
                 (when draw-center (quad tbx tay bx ay tcx tdy cx ey))
                 (quad tcx tay cx ay tdx tdy ex ey)))
              (rl-end)
              (rl-pop-matrix)
              (rl-set-texture 0)))))))
  nil)

;;; Render texture functions (will be implemented later in this file)

;;; Utility functions

(defun get-texture-data (texture)
  "Get pixel data from texture (download from GPU)"
  (when (is-texture-valid texture)
    (let* ((width (texture-width texture))
          (height (texture-height texture))
          (texture-id (texture-id texture))
          (data (make-array (* width height 4) :element-type '(unsigned-byte 8))))
      
      ;; Use framebuffer approach (compatible with both Desktop OpenGL and OpenGL ES)
      (read-texture-via-framebuffer texture-id width height data)
      
      ;; Create image from data
      (make-image :data data :width width :height height 
                  :format +pixelformat-uncompressed-rgba+))))

(defun read-texture-via-framebuffer (texture-id width height data)
  "Read texture data using framebuffer (for OpenGL ES compatibility)"
  (let ((fbo (gl:gen-framebuffer)))
    (unwind-protect
        (progn
          ;; Bind framebuffer
          (gl:bind-framebuffer :framebuffer fbo)
          
          ;; Attach texture as color attachment
          (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture-id 0)
          
          ;; Check framebuffer completeness
          (unless (eq (gl:check-framebuffer-status :framebuffer) :framebuffer-complete)
            (error "Framebuffer not complete for texture reading"))
          
          ;; Read pixels from framebuffer
          (gl:read-pixels 0 0 width height :rgba :unsigned-byte data)
          
          ;; Unbind framebuffer
          (gl:bind-framebuffer :framebuffer 0)
          
          (trace-log-info "read-texture-via-framebuffer: Successfully read texture data"))
      
      ;; Cleanup framebuffer
      (gl:delete-framebuffer fbo))))

(defun get-texture-format (texture)
  "Get texture internal format"
  (if (is-texture-valid texture)
      (texture-format texture)
      0))

;;; Cleanup functions

(defun cleanup-texture-system ()
  "Cleanup all loaded textures"
  (loop for texture being the hash-values of *texture-registry* do
    (when (is-texture-valid texture)
      (gl:delete-texture (texture-id texture))))
  (clrhash *texture-registry*)
  (when *default-texture*
    (setf *default-texture* nil))
  (setf *texture-id-counter* 1)
  (setf *current-texture-id* 0))

;;; RenderTexture System (from raylib.h and rcore.c)
;;; Used for render-to-texture functionality - strictly following raylib C implementation

;;; RenderTexture structure (from raylib.h lines 287-291)
;;; typedef struct RenderTexture {
;;;     unsigned int id;        // OpenGL framebuffer object id
;;;     Texture texture;        // Color buffer attachment texture
;;;     Texture depth;          // Depth buffer attachment texture
;;; } RenderTexture;
;; RenderTexture structure is defined in raylib.lisp

;;; Global render texture state (following raylib CORE.Window state)
(defvar *current-fbo* nil "Currently bound framebuffer")
(defvar *current-fbo-width* 0 "Current framebuffer width")
(defvar *current-fbo-height* 0 "Current framebuffer height")
(defvar *using-fbo* nil "Whether currently using framebuffer")

;;; Pixel format constants (from raylib.h)

;;; Attachment constants (from rlgl.h)
(defconstant +rl-attachment-color-channel0+ 0 "Color attachment 0")
(defconstant +rl-attachment-depth+ 100 "Depth attachment")
(defconstant +rl-attachment-texture2d+ 100 "Texture2D attachment type")
(defconstant +rl-attachment-renderbuffer+ 200 "Renderbuffer attachment type")

;;; Helper functions for rlgl-style operations (following raylib rlgl.c patterns)
(defun rl-load-framebuffer (width height)
  "Load framebuffer (following rlLoadFramebuffer from rlgl.c)"
  (declare (ignore width height))
  (let ((fbo-id (first (gl:gen-framebuffers 1))))
    (format t "INFO: FBO: [ID ~d] Framebuffer object created successfully~%" fbo-id)
    fbo-id))

(defun rl-load-texture-depth (width height use-renderbuffer)
  "Load depth texture/renderbuffer (following rlLoadTextureDepth from rlgl.c)"
  (if use-renderbuffer
    ;; Create depth renderbuffer (as in raylib)
    (let ((depth-id (first (gl:gen-renderbuffers 1))))
      (gl:bind-renderbuffer :renderbuffer depth-id)
      (gl:renderbuffer-storage :renderbuffer :depth-component width height)
      (gl:bind-renderbuffer :renderbuffer 0)
      (format t "INFO: TEXTURE: [ID ~d] Depth renderbuffer loaded successfully (~dx~d)~%" 
              depth-id width height)
      ;; Return texture structure for depth renderbuffer
      (make-texture :id depth-id 
                    :width width 
                    :height height 
                    :mipmaps 1 
                    :format 19)) ; DEPTH_COMPONENT_24BIT format
    ;; Create depth texture (alternative)
    (let ((depth-id (first (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d depth-id)
      (gl:tex-image-2d :texture-2d 0 :depth-component width height 0 :depth-component :unsigned-int (cffi:null-pointer))
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      (gl:bind-texture :texture-2d 0)
      (make-texture :id depth-id :width width :height height :mipmaps 1 :format 19))))

;;; Note: rl-framebuffer-attach and rl-framebuffer-complete functions are now in gl.lisp

;;; Render texture loading and management (following raylib LoadRenderTexture exactly)
;;; Helper functions for rlgl-style rendering operations
(defun rl-draw-render-batch-active ()
  "Flush any pending draw calls (following rlDrawRenderBatchActive from rlgl.c)"
  ;; In raylib this flushes batched geometry
  ;; For now we ensure OpenGL state is consistent
  (gl:flush))

;;; Note: rl-enable-framebuffer and rl-disable-framebuffer functions are now in gl.lisp

;; rl-viewport, rl-load-identity, rl-ortho are now defined in gl.lisp
;; setup-viewport is now defined in core.lisp to match raylib's rcore.c organization

;;; Render texture mode functions (following raylib exactly)
(defun begin-texture-mode (target)
  "Begin drawing to render texture (raylib BeginTextureMode)"
  (when (is-render-texture-valid target)
    ;; Flush any pending draw calls (rlDrawRenderBatchActive)
    (rl-draw-render-batch-active)
    
    ;; Bind framebuffer (rlEnableFramebuffer)
    (rl-enable-framebuffer (render-texture-id target))
    
    ;; Set viewport to render texture size (rlViewport)
    (rl-viewport 0 0 
                 (texture-width (render-texture-texture target))
                 (texture-height (render-texture-texture target)))
    
    ;; Update internal state (rlSetFramebufferWidth/Height)
    (setf *current-fbo-width* (texture-width (render-texture-texture target)))
    (setf *current-fbo-height* (texture-height (render-texture-texture target)))
    
    ;; Setup projection matrix
    (rl-matrix-mode 0) ; Projection mode
    (rl-load-identity)
    (rl-ortho 0.0d0 (coerce (texture-width (render-texture-texture target)) 'double-float)
              (coerce (texture-height (render-texture-texture target)) 'double-float) 0.0d0 0.0d0 1.0d0)
    
    ;; Setup modelview matrix
    (rl-matrix-mode 1) ; Modelview mode
    (rl-load-identity)
    
    ;; Update global state (CORE.Window state)
    (setf *using-fbo* t)))

(defun end-texture-mode ()
  "End drawing to render texture (raylib EndTextureMode)"
  ;; Flush any pending draw calls (rlDrawRenderBatchActive)
  (rl-draw-render-batch-active)
  
  ;; Disable framebuffer (rlDisableFramebuffer) 
  (rl-disable-framebuffer)
  
  ;; Restore viewport and projection (SetupViewport)
  (setup-viewport (core-data-window-screen-width *core*) (core-data-window-screen-height *core*))
  
  ;; Restore modelview matrix
  (rl-matrix-mode :modelview)
  (rl-load-identity)
  ;; Apply screen scaling (in raylib: rlMultMatrixf(MatrixToFloat(CORE.Window.screenScale)))
  ;; For now we skip screen scaling transformation
  
  ;; Update global state
  (setf *current-fbo-width* (core-data-window-screen-width *core*))
  (setf *current-fbo-height* (core-data-window-screen-height *core*))
  (setf *using-fbo* nil))

;;; Utility functions for render textures
(defun get-render-texture-texture (render-texture)
  "Get the color texture from render texture"
  (when (is-render-texture-valid render-texture)
    (render-texture-texture render-texture)))

(defun get-render-texture-depth (render-texture)
  "Get the depth texture from render texture"
  (when (is-render-texture-valid render-texture)
    (render-texture-depth render-texture)))

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition - Color/pixel related functions
;;; NOTE: The Color* functions are defined in color.lisp
;;;----------------------------------------------------------------------------------

;; NOTE: SRC is a byte vector, OFFSET the byte position of the pixel (C pointer arithmetic)
(defun get-pixel-color (src format &optional (offset 0))
  "Get Color from a source pixel pointer of certain format"
  (alexandria:switch (format)
    (+pixelformat-uncompressed-grayscale+
     (let ((v (aref src offset))) (list v v v 255)))
    (+pixelformat-uncompressed-gray-alpha+
     (let ((v (aref src offset))) (list v v v (aref src (1+ offset)))))
    (+pixelformat-uncompressed-r5g6b5+
     (let ((p (%u16 src offset)))
       (list (floor (* (ash p -11) 255) 31)
             (floor (* (logand (ash p -5) #b111111) 255) 63)
             (floor (* (logand p #b11111) 255) 31)
             255)))
    (+pixelformat-uncompressed-r5g5b5a1+
     (let ((p (%u16 src offset)))
       (list (floor (* (ash p -11) 255) 31)
             (floor (* (logand (ash p -6) #b11111) 255) 31)
             (floor (* (logand (ash p -1) #b11111) 255) 31)
             (if (zerop (logand p 1)) 0 255))))
    (+pixelformat-uncompressed-r4g4b4a4+
     (let ((p (%u16 src offset)))
       (list (floor (* (ash p -12) 255) 15)
             (floor (* (logand (ash p -8) #b1111) 255) 15)
             (floor (* (logand (ash p -4) #b1111) 255) 15)
             (floor (* (logand p #b1111) 255) 15))))
    (+pixelformat-uncompressed-r8g8b8a8+
     (list (aref src offset) (aref src (+ offset 1)) (aref src (+ offset 2)) (aref src (+ offset 3))))
    (+pixelformat-uncompressed-r8g8b8+
     (list (aref src offset) (aref src (+ offset 1)) (aref src (+ offset 2)) 255))
    ;; NOTE: Pixel normalized float value is converted to [0..255]
    (+pixelformat-uncompressed-r32+
     (let ((v (%u8 (* (%f32 src offset) 255.0)))) (list v v v 255)))
    (+pixelformat-uncompressed-r32g32b32+
     (list (%u8 (* (%f32 src offset) 255.0)) (%u8 (* (%f32 src (+ offset 4)) 255.0))
           (%u8 (* (%f32 src (+ offset 8)) 255.0)) 255))
    (+pixelformat-uncompressed-r32g32b32a32+
     (list (%u8 (* (%f32 src offset) 255.0)) (%u8 (* (%f32 src (+ offset 4)) 255.0))
           (%u8 (* (%f32 src (+ offset 8)) 255.0)) (%u8 (* (%f32 src (+ offset 12)) 255.0))))
    (+pixelformat-uncompressed-r16+
     (let ((v (%u8 (* (%half-to-float (%u16 src offset)) 255.0)))) (list v v v 255)))
    (+pixelformat-uncompressed-r16g16b16+
     (list (%u8 (* (%half-to-float (%u16 src offset)) 255.0))
           (%u8 (* (%half-to-float (%u16 src (+ offset 2))) 255.0))
           (%u8 (* (%half-to-float (%u16 src (+ offset 4))) 255.0)) 255))
    (+pixelformat-uncompressed-r16g16b16a16+
     (list (%u8 (* (%half-to-float (%u16 src offset)) 255.0))
           (%u8 (* (%half-to-float (%u16 src (+ offset 2))) 255.0))
           (%u8 (* (%half-to-float (%u16 src (+ offset 4))) 255.0))
           (%u8 (* (%half-to-float (%u16 src (+ offset 6))) 255.0))))
    (t (list 0 0 0 0))))

;; NOTE: DST is a byte vector, OFFSET the byte position of the pixel (C pointer arithmetic)
(defun set-pixel-color (dst color format &optional (offset 0))
  "Set color formatted into destination pixel pointer"
  (destructuring-bind (r g b a) (%col color)
    (let ((nr (/ r 255.0)) (ng (/ g 255.0)) (nb (/ b 255.0)) (na (/ a 255.0)))
      (alexandria:switch (format)
        (+pixelformat-uncompressed-grayscale+
         ;; NOTE: Calculate grayscale equivalent color
         (setf (aref dst offset) (%gray r g b)))
        (+pixelformat-uncompressed-gray-alpha+
         (setf (aref dst offset) (%gray r g b)
               (aref dst (1+ offset)) a))
        (+pixelformat-uncompressed-r5g6b5+
         ;; NOTE: Calculate R5G6B5 equivalent color
         (setf (%u16 dst offset)
               (logand #xffff (logior (ash (%u8 (%c-round (* nr 31.0))) 11)
                                      (ash (%u8 (%c-round (* ng 63.0))) 5)
                                      (%u8 (%c-round (* nb 31.0)))))))
        (+pixelformat-uncompressed-r5g5b5a1+
         ;; NOTE: Calculate R5G5B5A1 equivalent color
         (setf (%u16 dst offset)
               (logand #xffff (logior (ash (%u8 (%c-round (* nr 31.0))) 11)
                                      (ash (%u8 (%c-round (* ng 31.0))) 6)
                                      (ash (%u8 (%c-round (* nb 31.0))) 1)
                                      (if (> na (/ (float +pixelformat-uncompressed-r5g5b5a1-alpha-threshold+) 255.0)) 1 0)))))
        (+pixelformat-uncompressed-r4g4b4a4+
         ;; NOTE: Calculate R4G4B4A4 equivalent color
         (setf (%u16 dst offset)
               (logand #xffff (logior (ash (%u8 (%c-round (* nr 15.0))) 12)
                                      (ash (%u8 (%c-round (* ng 15.0))) 8)
                                      (ash (%u8 (%c-round (* nb 15.0))) 4)
                                      (%u8 (%c-round (* na 15.0)))))))
        (+pixelformat-uncompressed-r8g8b8+
         (setf (aref dst offset) r (aref dst (+ offset 1)) g (aref dst (+ offset 2)) b))
        (+pixelformat-uncompressed-r8g8b8a8+
         (setf (aref dst offset) r (aref dst (+ offset 1)) g (aref dst (+ offset 2)) b (aref dst (+ offset 3)) a)))))
  nil)

;; NOTE: Size can be requested for Image or Texture data
(defun get-pixel-data-size (width height format)
  "Get pixel data size in bytes for certain format"
  (let* ((bpp (alexandria:switch (format)                ; Bits per pixel
                (+pixelformat-uncompressed-grayscale+ 8)
                (+pixelformat-uncompressed-gray-alpha+ 16)
                (+pixelformat-uncompressed-r5g6b5+ 16)
                (+pixelformat-uncompressed-r5g5b5a1+ 16)
                (+pixelformat-uncompressed-r4g4b4a4+ 16)
                (+pixelformat-uncompressed-r8g8b8a8+ 32)
                (+pixelformat-uncompressed-r8g8b8+ 24)
                (+pixelformat-uncompressed-r32+ 32)
                (+pixelformat-uncompressed-r32g32b32+ (* 32 3))
                (+pixelformat-uncompressed-r32g32b32a32+ (* 32 4))
                (+pixelformat-uncompressed-r16+ 16)
                (+pixelformat-uncompressed-r16g16b16+ (* 16 3))
                (+pixelformat-uncompressed-r16g16b16a16+ (* 16 4))
                (+pixelformat-compressed-dxt1-rgb+ 4)
                (+pixelformat-compressed-dxt1-rgba+ 4)
                (+pixelformat-compressed-etc1-rgb+ 4)
                (+pixelformat-compressed-etc2-rgb+ 4)
                (+pixelformat-compressed-pvrt-rgb+ 4)
                (+pixelformat-compressed-pvrt-rgba+ 4)
                (+pixelformat-compressed-dxt3-rgba+ 8)
                (+pixelformat-compressed-dxt5-rgba+ 8)
                (+pixelformat-compressed-etc2-eac-rgba+ 8)
                (+pixelformat-compressed-astc-4x4-rgba+ 8)
                (+pixelformat-compressed-astc-8x8-rgba+ 2)
                (t 0)))
         (data-size-bytes (ash (* width height bpp) -3)) ; Get size in bytes (dividing by 8)
         (data-size 0))
    (when (< data-size-bytes most-positive-fixnum)
      (setf data-size data-size-bytes)
      ;; Most compressed formats works on 4x4 blocks,
      ;; if texture is smaller, minimum dataSize is 8 or 16
      (when (and (< width 4) (< height 4))
        (cond ((and (>= format +pixelformat-compressed-dxt1-rgb+) (< format +pixelformat-compressed-dxt3-rgba+))
               (setf data-size 8))
              ((and (>= format +pixelformat-compressed-dxt3-rgba+) (< format +pixelformat-compressed-astc-8x8-rgba+))
               (setf data-size 16)))))
    data-size))
