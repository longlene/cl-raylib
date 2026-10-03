(in-package #:cl-raylib)

;;;===================================================================================
;;; rtext - Basic functions to load fonts and draw text
;;; Port of raylib/src/rtext.c
;;;
;;; NOTE: TTF/OTF fonts are rasterized with the stb_truetype port in truetype.lisp
;;; NOTE: Text strings are Lisp strings: text functions work on characters (codepoints)
;;; instead of UTF-8 bytes, functions returning C out-parameters return multiple values
;;; NOTE: SUPPORT_FILEFORMAT_BDF is enabled (disabled by default in raylib config.h)
;;;===================================================================================

;;;----------------------------------------------------------------------------------
;;; Defines and Macros
;;;----------------------------------------------------------------------------------
(defconstant +max-text-buffer-length+ 1024 "Size of internal static buffers used on some functions")
(defconstant +max-textsplit-count+ 128 "Maximum number of substrings to split: TextSplit()")
(defconstant +font-atlas-corner-rec-size+ 3 "Size of white rectangle drawn on font atlas on font loading")

;;; Font type, defines generation method (FontType enum)
(defconstant +font-default+ 0 "Default font generation, anti-aliased")
(defconstant +font-bitmap+ 1 "Bitmap font generation, no anti-aliasing")
(defconstant +font-sdf+ 2 "SDF font generation, requires external shader")

;; Default values for ttf font generation
(defconstant +font-ttf-default-size+ 32 "TTF font generation default char size (char-height)")
(defconstant +font-ttf-default-numchars+ 95 "TTF font generation default charset: 95 glyphs (ASCII 32..126)")
(defconstant +font-ttf-default-first-char+ 32 "TTF font generation default first char for image sprite font (32-Space)")
(defconstant +font-ttf-default-chars-padding+ 4 "TTF font generation default glyphs padding")

;; SDF generation default values
(defconstant +font-sdf-char-padding+ 4 "SDF font generation char padding")
(defconstant +font-sdf-on-edge-value+ 128 "SDF font generation on edge value")
(defconstant +font-sdf-pixel-dist-scale+ 64.0 "SDF font generation pixel distance scale")
(defconstant +font-bitmap-alpha-threshold+ 80 "Bitmap (B&W) font generation alpha threshold")

;;;----------------------------------------------------------------------------------
;;; Global variables
;;;----------------------------------------------------------------------------------

;; Default font provided by raylib
;; NOTE: Default font is loaded on InitWindow() and disposed on CloseWindow() [module: core]
(defvar *default-font* (make-font :texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0)))

;; Text vertical line spacing in pixels (between lines)
(defvar *text-line-spacing* 2)

;; Default font data (generated from a sprite font image)
(defparameter *raylib-default-font-data*
  #(#x00000000 #x00000000 #x00000000 #x00000000 #x00200020 #x0001b000 #x00000000 #x00000000 #x8ef92520 #x00020a00 #x7dbe8000 #x1f7df45f
    #x4a2bf2a0 #x0852091e #x41224000 #x10041450 #x2e292020 #x08220812 #x41222000 #x10041450 #x10f92020 #x3efa084c #x7d22103c #x107df7de
    #xe8a12020 #x08220832 #x05220800 #x10450410 #xa4a3f000 #x08520832 #x05220400 #x10450410 #xe2f92020 #x0002085e #x7d3e0281 #x107df41f
    #x00200000 #x8001b000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #xc0000fbe #xfbf7e00f #x5fbf7e7d #x0050bee8 #x440808a2 #x0a142fe8 #x50810285 #x0050a048
    #x49e428a2 #x0a142828 #x40810284 #x0048a048 #x10020fbe #x09f7ebaf #xd89f3e84 #x0047a04f #x09e48822 #x0a142aa1 #x50810284 #x0048a048
    #x04082822 #x0a142fa0 #x50810285 #x0050a248 #x00008fbe #xfbf42021 #x5f817e7d #x07d09ce8 #x00008000 #x00000fe0 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x000c0180
    #xdfbf4282 #x0bfbf7ef #x42850505 #x004804bf #x50a142c6 #x08401428 #x42852505 #x00a808a0 #x50a146aa #x08401428 #x42852505 #x00081090
    #x5fa14a92 #x0843f7e8 #x7e792505 #x00082088 #x40a15282 #x08420128 #x40852489 #x00084084 #x40a16282 #x0842022a #x40852451 #x00088082
    #xc0bf4282 #xf843f42f #x7e85fc21 #x3e0900bf #x00000000 #x00000004 #x00000000 #x000c0180 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x04000402 #x41482000 #x00000000 #x00000800
    #x04000404 #x4100203c #x00000000 #x00000800 #xf7df7df0 #x514bef85 #xbefbefbe #x04513bef #x14414500 #x494a2885 #xa28a28aa #x04510820
    #xf44145f0 #x474a289d #xa28a28aa #x04510be0 #x14414510 #x494a2884 #xa28a28aa #x02910a00 #xf7df7df0 #xd14a2f85 #xbefbe8aa #x011f7be0
    #x00000000 #x00400804 #x20080000 #x00000000 #x00000000 #x00600f84 #x20080000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #xac000000 #x00000f01 #x00000000 #x00000000 #x24000000 #x00000f01 #x00000000 #x06000000 #x24000000 #x00000f01 #x00000000 #x09108000
    #x24fa28a2 #x00000f01 #x00000000 #x013e0000 #x2242252a #x00000f52 #x00000000 #x038a8000 #x2422222a #x00000f29 #x00000000 #x010a8000
    #x2412252a #x00000f01 #x00000000 #x010a8000 #x24fbe8be #x00000f01 #x00000000 #x0ebe8000 #xac020000 #x00000f01 #x00000000 #x00048000
    #x0003e000 #x00000f00 #x00000000 #x00008000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000038 #x8443b80e #x00203a03
    #x02bea080 #xf0000020 #xc452208a #x04202b02 #xf8029122 #x07f0003b #xe44b388e #x02203a02 #x081e8a1c #x0411e92a #xf4420be0 #x01248202
    #xe8140414 #x05d104ba #xe7c3b880 #x00893a0a #x283c0e1c #x04500902 #xc4400080 #x00448002 #xe8208422 #x04500002 #x80400000 #x05200002
    #x083e8e00 #x04100002 #x804003e0 #x07000042 #xf8008400 #x07f00003 #x80400000 #x04000022 #x00000000 #x00000000 #x80400000 #x04000002
    #x00000000 #x00000000 #x00000000 #x00000000 #x00800702 #x1848a0c2 #x84010000 #x02920921 #x01042642 #x00005121 #x42023f7f #x00291002
    #xefc01422 #x7efdfbf7 #xefdfa109 #x03bbbbf7 #x28440f12 #x42850a14 #x20408109 #x01111010 #x28440408 #x42850a14 #x2040817f #x01111010
    #xefc78204 #x7efdfbf7 #xe7cf8109 #x011111f3 #x2850a932 #x42850a14 #x2040a109 #x01111010 #x2850b840 #x42850a14 #xefdfbf79 #x03bbbbf7
    #x001fa020 #x00000000 #x00001000 #x00000000 #x00002070 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x08022800 #x00012283 #x02430802 #x01010001 #x8404147c #x20000144 #x80048404 #x00823f08 #xdfbf4284 #x7e03f7ef #x142850a1 #x0000210a
    #x50a14684 #x528a1428 #x142850a1 #x03efa17a #x50a14a9e #x52521428 #x142850a1 #x02081f4a #x50a15284 #x4a221428 #xf42850a1 #x03efa14b
    #x50a16284 #x4a521428 #x042850a1 #x0228a17a #xdfbf427c #x7e8bf7ef #xf7efdfbf #x03efbd0b #x00000000 #x04000000 #x00000000 #x00000008
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00200508 #x00840400 #x11458122 #x00014210
    #x00514294 #x51420800 #x20a22a94 #x0050a508 #x00200000 #x00000000 #x00050000 #x08000000 #xfefbefbe #xfbefbefb #xfbeb9114 #x00fbefbe
    #x20820820 #x8a28a20a #x8a289114 #x3e8a28a2 #xfefbefbe #xfbefbe0b #x8a289114 #x008a28a2 #x228a28a2 #x08208208 #x8a289114 #x088a28a2
    #xfefbefbe #xfbefbefb #xfa2f9114 #x00fbefbe #x00000000 #x00000040 #x00000000 #x00000000 #x00000000 #x00000020 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00210100 #x00000004 #x00000000 #x00000000 #x14508200 #x00001402 #x00000000 #x00000000
    #x00000010 #x00000020 #x00000000 #x00000000 #xa28a28be #x00002228 #x00000000 #x00000000 #xa28a28aa #x000022e8 #x00000000 #x00000000
    #xa28a28aa #x000022a8 #x00000000 #x00000000 #xa28a28aa #x000022e8 #x00000000 #x00000000 #xbefbefbe #x00003e2f #x00000000 #x00000000
    #x00000004 #x00002028 #x00000000 #x00000000 #x80000000 #x00003e0f #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
  "Exact raylib default font bitmap data - 512 unsigned 32-bit integers")

;;; Exact raylib character widths (224 characters) - from rtext.c
(defparameter *raylib-chars-width*
  #(3 1 4 6 5 7 6 2 3 3 5 5 2 4 1 7 5 2 5 5 5 5 5 5 5 5 1 1 3 4 3 6
    7 6 6 6 6 6 6 6 6 3 5 6 5 7 6 6 6 6 6 6 7 6 7 7 6 6 6 2 7 2 3 5
    2 5 5 5 5 5 4 5 5 1 2 5 2 5 5 5 5 5 5 5 4 5 5 5 5 5 5 3 1 3 4 4
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    1 1 5 5 5 7 1 5 3 7 3 5 4 1 7 4 3 5 3 3 2 5 6 1 2 2 3 5 6 6 6 6
    6 6 6 6 6 6 7 6 6 6 6 6 3 3 3 3 7 6 6 6 6 6 6 5 6 6 6 6 6 6 4 6
    5 5 5 5 5 5 9 5 5 5 5 5 2 2 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5)
  "Exact raylib character widths for all 224 glyphs")

;;;----------------------------------------------------------------------------------
;;; Module Functions Definition
;;;----------------------------------------------------------------------------------

(defun %empty-image ()
  (make-image :data nil :width 0 :height 0 :mipmaps 0 :format 0))

(defun load-font-default ()
  "Load raylib default font"
  ;; Check to see if the font for an image has already been allocated,
  ;; and if no need to upload, then return
  (when (font-glyphs *default-font*) (return-from load-font-default))
  ;; NOTE: Using UTF-8 encoding table for Unicode U+0000..U+00FF Basic Latin + Latin-1 Supplement
  (setf (font-glyph-count *default-font*) 224 ; Number of glyphs included in our default font
        (font-glyph-padding *default-font*) 0) ; Characters padding
  (let* ((chars-height 10)
         (chars-divisor 1)              ; Every char is separated from the consecutive by a 1 pixel divisor, horizontally and vertically
         ;; Re-construct image from defaultFontData and generate OpenGL texture
         (im-font (make-image :data (make-array (* 128 128 2) :element-type '(unsigned-byte 8) :initial-element 0)
                              :width 128 :height 128 :mipmaps 1
                              :format +pixelformat-uncompressed-gray-alpha+))
         (data (image-data im-font)))
    ;; Fill image.data with defaultFontData (convert from bit to pixel!)
    (loop for i from 0 below (* 128 128) by 32
          for counter from 0
          do (loop for j from 31 downto 0
                   do (if (logbitp j (aref *raylib-default-font-data* counter))
                          ;; NOTE: Unreferencing data as short, so,
                          ;; considering data as little-endian (alpha + gray)
                          (setf (aref data (* (+ i j) 2)) #xff
                                (aref data (+ (* (+ i j) 2) 1)) #xff)
                          (setf (aref data (* (+ i j) 2)) #xff
                                (aref data (+ (* (+ i j) 2) 1)) #x00))))
    (setf (font-texture *default-font*) (load-texture-from-image im-font))
    ;; Reconstruct charSet using charsWidth[], charsHeight, charsDivisor, glyphCount
    ;;------------------------------------------------------------------------------
    (let* ((glyph-count (font-glyph-count *default-font*))
           (glyphs (make-array glyph-count))
           (recs (make-array glyph-count))
           (current-line 0)
           (current-pos-x chars-divisor)
           (test-pos-x chars-divisor))
      (dotimes (i glyph-count)
        (let ((glyph (make-glyph-info :value (+ 32 i))) ; First char is 32
              (rec (make-rectangle :x (float current-pos-x 1.0)
                                   :y (float (+ chars-divisor (* current-line (+ chars-height chars-divisor))) 1.0)
                                   :width (float (aref *raylib-chars-width* i) 1.0)
                                   :height (float chars-height 1.0))))
          (incf test-pos-x (truncate (+ (rectangle-width rec) (float chars-divisor 1.0))))
          (if (>= test-pos-x (image-width im-font))
              (progn
                (incf current-line)
                (setf current-pos-x (+ (* 2 chars-divisor) (aref *raylib-chars-width* i)))
                (setf test-pos-x current-pos-x)
                (setf (rectangle-x rec) (float chars-divisor 1.0)
                      (rectangle-y rec) (float (+ chars-divisor (* current-line (+ chars-height chars-divisor))) 1.0)))
              (setf current-pos-x test-pos-x))
          ;; NOTE: On default font character offsets and xAdvance are not required
          (setf (glyph-info-offset-x glyph) 0
                (glyph-info-offset-y glyph) 0
                (glyph-info-advance-x glyph) 0)
          ;; Fill character image data from fontClear data
          (setf (glyph-info-image glyph) (image-from-image im-font rec))
          (setf (aref glyphs i) glyph
                (aref recs i) rec)))
      (setf (font-glyphs *default-font*) glyphs
            (font-recs *default-font*) recs)
      (setf (font-base-size *default-font*) (truncate (rectangle-height (aref recs 0))))))
  (trace-log-info "FONT: Default font loaded successfully (~d glyphs)" (font-glyph-count *default-font*))
  (values))

(defun unload-font-default ()
  "Unload raylib default font"
  (dotimes (i (font-glyph-count *default-font*))
    (unload-image (glyph-info-image (aref (font-glyphs *default-font*) i))))
  (unload-texture (font-texture *default-font*))
  (setf (font-glyph-count *default-font*) 0
        (font-glyphs *default-font*) nil
        (font-recs *default-font*) nil)
  (values))

(defun get-font-default ()
  "Get the default font, useful to be used with extended parameters"
  *default-font*)

(defun load-font (file-name)
  "Load font from file into GPU memory (VRAM)"
  (let ((font (make-font :texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0))))
    (cond
      ((or (is-file-extension file-name ".ttf") (is-file-extension file-name ".otf"))
       (setf font (load-font-ex file-name +font-ttf-default-size+ nil +font-ttf-default-numchars+)))
      ((is-file-extension file-name ".fnt")
       (setf font (%load-bm-font file-name)))
      ((is-file-extension file-name ".bdf")
       (setf font (load-font-ex file-name +font-ttf-default-size+ nil +font-ttf-default-numchars+)))
      (t
       (let ((image (load-image file-name)))
         (if (and image (image-data image))
             (setf font (load-font-from-image image +magenta+ +font-ttf-default-first-char+))
             (setf font (get-font-default)))
         (unload-image image))))
    (if (or (null (font-texture font)) (= (texture-id (font-texture font)) 0))
        (trace-log-warning "FONT: [~a] Failed to load font texture -> Using default font" file-name)
        (progn
          (set-texture-filter (font-texture font) +texture-filter-point+) ; By default, set point filter (the best performance)
          (trace-log-info "FONT: Data loaded successfully (~d pixel size | ~d glyphs)"
                          (font-base-size font) (font-glyph-count font))))
    font))

(defun load-font-ex (file-name font-size &optional codepoints (codepoint-count (length codepoints)))
  "Load font from file with defined codepoints and generation size
NOTE: NIL for codepoints and 0 for codepointCount to load the default character set (32..126),
font size is provided in pixels height"
  (let ((font (make-font :texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0))))
    ;; Loading file to memory
    (multiple-value-bind (file-data data-size) (load-file-data file-name)
      (when file-data
        ;; Loading font from memory data
        (setf font (load-font-from-memory (get-file-extension file-name) file-data data-size
                                          font-size codepoints codepoint-count))))
    font))

(defun load-font-from-image (image key first-char)
  "Load font from Image (XNA style)"
  (let* ((key (keyword-to-color key))
         (font (get-font-default))
         (max-glyphs-from-image 256)    ; Maximum number of glyphs supported on image scan
         (char-spacing 0)
         (line-spacing 0)
         ;; Allocate a temporal arrays for glyphs data measures,
         ;; once the actual number of glyphs is obtained, copy data to a sized array
         (temp-char-values (make-array max-glyphs-from-image :initial-element 0))
         (temp-char-recs (make-array max-glyphs-from-image :initial-element nil))
         (pixels (%load-image-colors image))
         (width (image-width image))
         (height (image-height image))
         (x 0) (y 0))
    (flet ((key-p (i)
             (and (= (aref pixels (* i 4)) (first key))
                  (= (aref pixels (+ (* i 4) 1)) (second key))
                  (= (aref pixels (+ (* i 4) 2)) (third key))
                  (= (aref pixels (+ (* i 4) 3)) (fourth key)))))
      ;; Parse image data to get charSpacing and lineSpacing
      (block scan
        (loop for yy from 0 below height
              do (setf y yy)
                 (setf x 0)
                 (loop while (and (< x width) (key-p (+ (* y width) x))) do (incf x))
                 ;; NOTE: As in C, after a full key-colored row this tests the first pixel of the next row
                 (when (and (< (+ (* y width) x) (* width height)) (not (key-p (+ (* y width) x))))
                   (return-from scan)))
        (setf y height))
      ;; Security check
      (when (or (= x 0) (= y 0))
        (return-from load-font-from-image font))
      (setf char-spacing x
            line-spacing y)
      (let ((char-height 0) (j 0))
        (loop while (and (< (+ line-spacing j) height)
                         (not (key-p (+ (* (+ line-spacing j) width) char-spacing))))
              do (incf j))
        (setf char-height j)
        ;; Check array values to get characters: value, x, y, w, h
        (let ((index 0)
              (line-to-read 0)
              (x-pos-to-read char-spacing))
          ;; Parse image data to get rectangle sizes
          (loop while (< (+ line-spacing (* line-to-read (+ char-height line-spacing))) height)
                do (loop while (and (< x-pos-to-read width)
                                    (not (key-p (+ (* (+ line-spacing (* (+ char-height line-spacing) line-to-read)) width)
                                                   x-pos-to-read))))
                         do (setf (aref temp-char-values index) (+ first-char index))
                            (let ((char-width 0))
                              (loop while (and (< (+ x-pos-to-read char-width) width)
                                               (not (key-p (+ (* (+ line-spacing (* (+ char-height line-spacing) line-to-read)) width)
                                                              x-pos-to-read char-width))))
                                    do (incf char-width))
                              (setf (aref temp-char-recs index)
                                    (make-rectangle :x (float x-pos-to-read 1.0)
                                                    :y (float (+ line-spacing (* line-to-read (+ char-height line-spacing))) 1.0)
                                                    :width (float char-width 1.0)
                                                    :height (float char-height 1.0)))
                              (incf index)
                              (incf x-pos-to-read (+ char-width char-spacing))))
                   (incf line-to-read)
                   (setf x-pos-to-read char-spacing))
          ;; NOTE: Key color borders need to be removed from image to avoid weird
          ;; artifacts on texture scaling when using TEXTURE_FILTER_BILINEAR or TEXTURE_FILTER_TRILINEAR
          (dotimes (i (* height width))
            (when (key-p i) (fill pixels 0 :start (* i 4) :end (+ (* i 4) 4))))
          ;; Create a new image with the processed color data (key color replaced by BLANK)
          (let ((font-clear (make-image :data pixels :width width :height height :mipmaps 1
                                        :format +pixelformat-uncompressed-r8g8b8a8+))
                (new-font (make-font)))
            ;; Set font with all data parsed from image
            (setf (font-texture new-font) (load-texture-from-image font-clear) ; Convert processed image to OpenGL texture
                  (font-glyph-count new-font) index
                  (font-glyph-padding new-font) 0)
            ;; Populate tempCharValues and tempCharsRecs with glyphs data
            ;; Move temp data to sized charValues and charRecs arrays
            (let ((glyphs (make-array index))
                  (recs (make-array index)))
              (dotimes (i index)
                ;; Get character rectangle in the font atlas texture
                (setf (aref recs i) (aref temp-char-recs i))
                ;; NOTE: On image based fonts (XNA style), character offsets and xAdvance are not required (set to 0)
                (setf (aref glyphs i)
                      (make-glyph-info :value (aref temp-char-values i)
                                       :offset-x 0 :offset-y 0 :advance-x 0
                                       ;; Fill character image data from fontClear data
                                       :image (image-from-image font-clear (aref temp-char-recs i)))))
              (setf (font-glyphs new-font) glyphs
                    (font-recs new-font) recs))
            (unload-image font-clear)   ; Unload processed image once converted to texture
            (setf (font-base-size new-font) (truncate (rectangle-height (aref (font-recs new-font) 0))))
            new-font))))))

(defun load-font-from-memory (file-type file-data data-size font-size &optional codepoints (codepoint-count (length codepoints)))
  "Load font from memory buffer, fileType refers to extension: i.e. '.ttf'"
  (let ((font (make-font :texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0)))
        (file-ext-lower (text-to-lower (or file-type ""))))
    (setf (font-base-size font) font-size
          (font-glyph-padding font) 0)
    (cond
      ((or (text-is-equal file-ext-lower ".ttf") (text-is-equal file-ext-lower ".otf"))
       (multiple-value-bind (glyphs glyph-count)
           (load-font-data file-data data-size (font-base-size font) codepoints
                           (if (> codepoint-count 0) codepoint-count 95) +font-default+)
         (setf (font-glyphs font) glyphs
               (font-glyph-count font) glyph-count)))
      ((text-is-equal file-ext-lower ".bdf")
       (multiple-value-bind (glyphs font-size*)
           (%load-font-data-bdf file-data data-size codepoints (if (> codepoint-count 0) codepoint-count 95) (font-base-size font))
         (setf (font-glyphs font) glyphs
               (font-base-size font) font-size*
               (font-glyph-count font) (if (> codepoint-count 0) codepoint-count 95))))
      (t (setf (font-glyphs font) nil)))
    (if (font-glyphs font)
        (progn
          (setf (font-glyph-padding font) +font-ttf-default-chars-padding+)
          (multiple-value-bind (atlas recs)
              (gen-image-font-atlas (font-glyphs font) (font-glyph-count font) (font-base-size font)
                                    (font-glyph-padding font) 0)
            (setf (font-recs font) recs)
            (setf (font-texture font) (load-texture-from-image atlas))
            ;; Update glyphs[i].image to use alpha, required to be used on ImageDrawText()
            (dotimes (i (font-glyph-count font))
              (let ((glyph (aref (font-glyphs font) i)))
                (unload-image (glyph-info-image glyph))
                (setf (glyph-info-image glyph) (image-from-image atlas (aref recs i)))))
            (unload-image atlas))
          (trace-log-info "FONT: Data loaded successfully (~d pixel size | ~d glyphs)"
                          (font-base-size font) (font-glyph-count font)))
        (progn
          (trace-log-warning "FONT: Font is not supported by LoadFontEx/LoadFontFromMemory or no glyphs found, reverted to default font")
          (setf font (get-font-default))))
    font))

(defun is-font-valid (font)
  "Check if font is valid (font data loaded)
WARNING: GPU texture not checked"
  (and font
       (> (font-base-size font) 0)      ; Validate font size
       (> (font-glyph-count font) 0)    ; Validate font contains some glyph
       (font-recs font)                 ; Validate font recs defining glyphs on texture atlas
       (font-glyphs font)               ; Validate glyph data is loaded
       t))

(defun %space-codepoint-p (cp)
  (member cp '(#x20 #xA0 #x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006
               #x2007 #x2008 #x2009 #x200A #x202F #x205F #x3000)))

(defun load-font-data (file-data data-size font-size codepoints codepoint-count type)
  "Load font data for further use
NOTE: Requires TTF font memory data and can generate SDF data
Returns (values glyphs glyph-count)"
  (declare (ignore data-size))
  (let ((glyphs nil)
        (glyph-counter 0))
    ;; Load font data (including pixel data) from TTF memory file
    ;; NOTE: Loaded information should be enough to generate font image atlas, using any packaging method
    (when file-data
      (let ((font-info (stbtt-init-font file-data 0)) ; Initialize font for data reading
            (required-codepoints codepoints))
        (if font-info
            (let ((scale-factor (stbtt-scale-for-pixel-height font-info (float font-size 1.0))))
              ;; Calculate font basic metrics
              ;; NOTE: ascent is equivalent to font baseline
              (multiple-value-bind (ascent descent line-gap) (stbtt-get-font-v-metrics font-info)
                (declare (ignore descent line-gap))
                ;; In case no chars count provided, default to 95
                (setf codepoint-count (if (> codepoint-count 0) codepoint-count 95))
                ;; Fill fontChars in case not provided externally
                ;; NOTE: By default filling glyphCount consecutively, starting at 32 (Space)
                (if (null required-codepoints)
                    (progn
                      (setf required-codepoints (make-array codepoint-count))
                      (dotimes (i codepoint-count) (setf (aref required-codepoints i) (+ i 32))))
                    (setf required-codepoints (coerce required-codepoints 'simple-vector)))
                ;; Check available glyphs on provided font before loading them
                (dotimes (i codepoint-count)
                  (when (> (stbtt-find-glyph-index font-info (aref required-codepoints i)) 0)
                    (incf glyph-counter)))
                ;; WARNING: Allocating space for maximum number of codepoints
                (setf glyphs (make-array glyph-counter))
                (setf glyph-counter 0)  ; Reset to reuse
                (let ((k 0))
                  (dotimes (i codepoint-count)
                    (let* ((cp-width 0) (cp-height 0) ; Codepoint width and height (on generation)
                           (cp (aref required-codepoints i)) ; Codepoint value to get info for
                           ;; Check if glyph is available in the font
                           ;; WARNING: if (index == 0), glyph not found, it could fallback to default .notdef glyph (if defined in font)
                           (index (stbtt-find-glyph-index font-info cp)))
                      (if (> index 0)
                          (let ((glyph (make-glyph-info :value cp :image (%empty-image))))
                            ;; NOTE: Only storing glyphs for codepoints found in the font
                            (setf (aref glyphs k) glyph)
                            (case type
                              ((#.+font-default+ #.+font-bitmap+)
                               (multiple-value-bind (data w h xoff yoff)
                                   (stbtt-get-codepoint-bitmap font-info scale-factor scale-factor cp)
                                 (setf (image-data (glyph-info-image glyph)) data
                                       cp-width w cp-height h
                                       (glyph-info-offset-x glyph) xoff
                                       (glyph-info-offset-y glyph) yoff)))
                              (#.+font-sdf+
                               (when (/= cp 32)
                                 (multiple-value-bind (data w h xoff yoff)
                                     (stbtt-get-codepoint-sdf font-info scale-factor cp
                                                              +font-sdf-char-padding+ +font-sdf-on-edge-value+ +font-sdf-pixel-dist-scale+)
                                   (setf (image-data (glyph-info-image glyph)) data
                                         cp-width w cp-height h
                                         (glyph-info-offset-x glyph) xoff
                                         (glyph-info-offset-y glyph) yoff)))))
                            (when (image-data (glyph-info-image glyph)) ; Glyph data has been found in the font
                              (setf (glyph-info-advance-x glyph) (stbtt-get-codepoint-h-metrics font-info cp))
                              (setf (glyph-info-advance-x glyph)
                                    (truncate (* (float (glyph-info-advance-x glyph) 1.0) scale-factor)))
                              ;; WARNING: If requested SDF font, sdf-glyph height is definitely bigger than fontSize due to FONT_SDF_CHAR_PADDING
                              (when (and (/= type +font-sdf+) (> cp-height font-size))
                                (trace-log-warning "FONT: [0x~(~4,'0x~)] Glyph height is bigger than requested font size: ~d > ~d"
                                                   cp cp-height (truncate font-size)))
                              ;; Load glyph image
                              (let ((image (glyph-info-image glyph)))
                                (setf (image-width image) cp-width
                                      (image-height image) cp-height
                                      (image-mipmap-count image) 1
                                      (image-pixel-format image) +pixelformat-uncompressed-grayscale+))
                              (incf (glyph-info-offset-y glyph) (truncate (* (float ascent 1.0) scale-factor))))
                            ;; Create an empty image for Unicode space characters, useful for sprite font generation
                            (when (%space-codepoint-p cp)
                              (setf (glyph-info-advance-x glyph) (stbtt-get-codepoint-h-metrics font-info cp))
                              (setf (glyph-info-advance-x glyph)
                                    (truncate (* (float (glyph-info-advance-x glyph) 1.0) scale-factor)))
                              (let ((im-space (make-image :data nil :width (glyph-info-advance-x glyph) :height font-size
                                                          :mipmaps 1 :format +pixelformat-uncompressed-grayscale+)))
                                ;; Only allocate space image if required
                                (if (> (glyph-info-advance-x glyph) 0)
                                    (setf (image-data im-space)
                                          (make-array (* (glyph-info-advance-x glyph) font-size)
                                                      :element-type '(unsigned-byte 8) :initial-element 0))
                                    (setf (glyph-info-advance-x glyph) 0))
                                (setf (glyph-info-image glyph) im-space)))
                            (when (= type +font-bitmap+)
                              ;; Aliased bitmap (black & white) font generation, avoiding anti-aliasing
                              ;; NOTE: For optimum results, bitmap font should be generated at base pixel size
                              (let ((data (image-data (glyph-info-image glyph))))
                                (dotimes (p (* cp-width cp-height))
                                  (setf (aref data p) (if (< (aref data p) +font-bitmap-alpha-threshold+) 0 255)))))
                            (incf k)
                            (incf glyph-counter))
                          ;; WARNING: Glyph not found on font, optionally use a fallback glyph
                          nil)))
                  (when (< glyph-counter codepoint-count)
                    (trace-log-warning "FONT: Requested codepoints glyphs found: [~d/~d]" k codepoint-count)))))
            (trace-log-warning "FONT: Failed to process TTF font data"))))
    (values glyphs glyph-counter)))

(defun gen-image-font-atlas (glyphs glyph-count font-size padding pack-method)
  "Generate image font atlas using chars info
NOTE: Packing method: 0-Default, 1-Skyline
Returns (values atlas-image glyph-recs)"
  (let ((atlas (make-image :data nil :width 0 :height 0 :mipmaps 0 :format 0)))
    (when (null glyphs)
      (trace-log-warning "FONT: Provided glyphs info not valid, returning empty image atlas")
      (return-from gen-image-font-atlas (values atlas nil)))
    ;; In case no chars count provided, suppose default of 95
    (setf glyph-count (if (> glyph-count 0) glyph-count 95))
    ;; NOTE: Rectangles memory is loaded here!
    (let ((recs (make-array glyph-count))
          (total-width 0))
      (flet ((glyph-image (i) (glyph-info-image (aref glyphs i))))
        ;; Calculate image size based on total glyph width and glyph row count
        (dotimes (i glyph-count)
          (incf total-width (+ (image-width (glyph-image i)) (* 2 padding))))
        (let* ((padded-font-size (+ font-size (* 2 padding)))
               ;; Estimate image atlas size from available data
               ;; NOTE: Multiplying total expected area by 1.2f scale factor but in case
               ;; some glyphs do not fit, the atlas height is scaled x2 to fit them
               (total-area (* (float (* total-width padded-font-size) 1.0) 1.2))
               (image-min-size (sqrt total-area))
               (image-size (truncate (expt 2.0 (fceiling (/ (log image-min-size) (log 2.0)))))))
          (if (< total-area (float (floor (* image-size image-size) 2) 1.0))
              (setf (image-width atlas) image-size ; Atlas bitmap width
                    (image-height atlas) (floor image-size 2)) ; Atlas bitmap height
              (setf (image-width atlas) image-size
                    (image-height atlas) image-size))
          (let ((atlas-data-size (* (image-width atlas) (image-height atlas)))) ; Save total size for bounds checking
            (setf (image-data atlas) (make-array atlas-data-size :element-type '(unsigned-byte 8) :initial-element 0)) ; Create a bitmap to store characters (8 bpp)
            (setf (image-pixel-format atlas) +pixelformat-uncompressed-grayscale+
                  (image-mipmap-count atlas) 1)
            (flet ((copy-glyph (i dest-x0 dest-y0)
                     ;; Copy pixel data from glyph image to atlas
                     (let* ((image (glyph-image i))
                            (src (image-data image))
                            (dst (image-data atlas)))
                       (dotimes (y (image-height image))
                         (dotimes (x (image-width image))
                           (let ((dest-x (+ dest-x0 x))
                                 (dest-y (+ dest-y0 y)))
                             ;; Security: check both lower and upper bounds
                             (when (and (>= dest-x 0) (< dest-x (image-width atlas))
                                        (>= dest-y 0) (< dest-y (image-height atlas)))
                               (setf (aref dst (+ (* dest-y (image-width atlas)) dest-x))
                                     (aref src (+ (* y (image-width image)) x))))))))))
              (cond
                ((= pack-method 0)      ; Use basic packing algorithm
                 (let ((offset-x padding)
                       (offset-y padding))
                   ;; NOTE: Using simple packaging, one char after another
                   (dotimes (i glyph-count)
                     ;; Check remaining space for glyph
                     (when (>= offset-x (- (image-width atlas) (image-width (glyph-image i)) (* 2 padding)))
                       (setf offset-x padding)
                       ;; NOTE: Be careful on offsetY for SDF fonts, by default SDF
                       ;; use an internal padding of 4 pixels, it means char rectangle
                       ;; height is bigger than fontSize, it could be up to (fontSize + 8)
                       (incf offset-y (+ font-size (* 2 padding)))
                       (when (> offset-y (- (image-height atlas) font-size padding))
                         (trace-log-warning "FONT: Updating atlas size to fit all characters")
                         ;; Update atlas size to fit all characters
                         (let* ((updated-atlas-height (* (image-height atlas) 2))
                                (updated-atlas-data-size (* (image-width atlas) updated-atlas-height))
                                (updated-atlas-data (make-array updated-atlas-data-size :element-type '(unsigned-byte 8) :initial-element 0)))
                           (replace updated-atlas-data (image-data atlas) :end2 atlas-data-size)
                           (setf (image-data atlas) updated-atlas-data
                                 (image-height atlas) updated-atlas-height
                                 atlas-data-size updated-atlas-data-size))))
                     (copy-glyph i offset-x offset-y)
                     ;; Fill chars rectangles in atlas info
                     (setf (aref recs i) (make-rectangle :x (float offset-x 1.0) :y (float offset-y 1.0)
                                                         :width (float (image-width (glyph-image i)) 1.0)
                                                         :height (float (image-height (glyph-image i)) 1.0)))
                     ;; Move atlas position X for next character drawing
                     (incf offset-x (+ (image-width (glyph-image i)) (* 2 padding))))))
                ((= pack-method 1)      ; Use Skyline rect packing algorithm (stb_pack_rect)
                 (let ((context (stbrp-init-target (image-width atlas) (image-height atlas) glyph-count))
                       (rects (make-array glyph-count)))
                   ;; Fill rectangles for packaging
                   (dotimes (i glyph-count)
                     (setf (aref rects i) (make-stbrp-rect :id i
                                                           :w (+ (image-width (glyph-image i)) (* 2 padding))
                                                           :h (+ (image-height (glyph-image i)) (* 2 padding)))))
                   ;; Package rectangles into atlas
                   (stbrp-pack-rects context rects)
                   (dotimes (i glyph-count)
                     (let ((r (aref rects i)))
                       ;; It returns char rectangles in atlas
                       (setf (aref recs i) (make-rectangle :x (+ (float (stbrp-rect-x r) 1.0) (float padding 1.0))
                                                           :y (+ (float (stbrp-rect-y r) 1.0) (float padding 1.0))
                                                           :width (float (image-width (glyph-image i)) 1.0)
                                                           :height (float (image-height (glyph-image i)) 1.0)))
                       (if (/= (stbrp-rect-was-packed r) 0)
                           ;; Copy pixel data from fc.data to atlas
                           (copy-glyph i (+ (stbrp-rect-x r) padding) (+ (stbrp-rect-y r) padding))
                           (trace-log-warning "FONT: Failed to package glyph (0x~(~2,'0x~))" (glyph-info-value (aref glyphs i))))))))))
            ;; Add a 3x3 white rectangle at the bottom-right corner of the generated atlas,
            ;; useful to use as the white texture to draw shapes with raylib
            ;; Security: ensure the atlas is large enough to hold a 3x3 rectangle
            (when (and (> +font-atlas-corner-rec-size+ 0) (>= (image-width atlas) 3) (>= (image-height atlas) 3))
              (let ((k (- (* (image-width atlas) (image-height atlas)) 1))
                    (data (image-data atlas)))
                (dotimes (i +font-atlas-corner-rec-size+)
                  (setf (aref data (- k 0)) 255
                        (aref data (- k 1)) 255
                        (aref data (- k 2)) 255)
                  (decf k (image-width atlas)))))
            ;; Convert image data from GRAYSCALE to GRAY_ALPHA
            (let* ((count (* (image-width atlas) (image-height atlas)))
                   (data-gray-alpha (make-array (* count 2) :element-type '(unsigned-byte 8))) ; Two channels
                   (data (image-data atlas)))
              (dotimes (i count)
                (setf (aref data-gray-alpha (* i 2)) 255
                      (aref data-gray-alpha (+ (* i 2) 1)) (aref data i)))
              (setf (image-data atlas) data-gray-alpha
                    (image-pixel-format atlas) +pixelformat-uncompressed-gray-alpha+))
            (values atlas recs)))))))

(defun unload-font-data (glyphs glyph-count)
  "Unload font glyphs info data (RAM)"
  (when glyphs
    (dotimes (i glyph-count) (unload-image (glyph-info-image (aref glyphs i)))))
  (values))

(defun unload-font (font)
  "Unload font from GPU memory (VRAM)"
  ;; NOTE: Make sure font is not default font (fallback)
  (when (and font (font-texture font)
             (/= (texture-id (font-texture font)) (texture-id (font-texture (get-font-default)))))
    (unload-font-data (font-glyphs font) (font-glyph-count font))
    (unload-texture (font-texture font))
    (setf (font-recs font) nil)
    (trace-log-debug "FONT: Unloaded font data from RAM and VRAM"))
  (values))

(defun export-font-as-code (font file-name)
  "Export font as code file, returns true on success"
  (let* ((text-bytes-per-line 20)
         ;; Get file name from path
         (file-name-pascal (text-to-pascal (get-file-name-without-ext file-name)))
         ;; Get font atlas image and size, required to estimate code file size
         ;; NOTE: This mechanism is highly coupled to raylib
         (image (load-image-from-texture (font-texture font)))
         (image-data-size 0)
         (result nil))
    (when (/= (image-pixel-format image) +pixelformat-uncompressed-gray-alpha+)
      (trace-log-warning "Font export as code: Font image format is not GRAY+ALPHA!"))
    (setf image-data-size (get-pixel-data-size (image-width image) (image-height image) (image-pixel-format image)))
    (let ((txt-data
            (with-output-to-string (out)
              (format out "////////////////////////////////////////////////////////////////////////////////////////~%")
              (format out "//                                                                                    //~%")
              (format out "// FontAsCode exporter v1.0 - Font data exported as an array of bytes                 //~%")
              (format out "//                                                                                    //~%")
              (format out "// more info and bugs-report:  github.com/raysan5/raylib                              //~%")
              (format out "// feedback and support:       ray[at]raylib.com                                      //~%")
              (format out "//                                                                                    //~%")
              (format out "// Copyright (c) 2018-2026 Ramon Santamaria (@raysan5)                                //~%")
              (format out "//                                                                                    //~%")
              (format out "// ---------------------------------------------------------------------------------- //~%")
              (format out "//                                                                                    //~%")
              (format out "// TODO: Fill the information and license of the exported font here:                  //~%")
              (format out "//                                                                                    //~%")
              (format out "// Font name:    ....                                                                 //~%")
              (format out "// Font creator: ....                                                                 //~%")
              (format out "// Font LICENSE: ....                                                                 //~%")
              (format out "//                                                                                    //~%")
              (format out "////////////////////////////////////////////////////////////////////////////////////////~%~%")
              ;; WARNING: Data is compressed using raylib CompressData() DEFLATE,
              ;; it requires to be decompressed with raylib DecompressData(), that requires
              ;; compiling raylib with SUPPORT_COMPRESSION_API config flag enabled
              ;; Compress font image data
              (multiple-value-bind (comp-data comp-data-size) (compress-data (image-data image) image-data-size)
                ;; Save font image data (compressed)
                (format out "#define COMPRESSED_DATA_SIZE_FONT_~a ~d~%~%" (text-to-upper file-name-pascal) comp-data-size)
                (format out "// Font image pixels data compressed (DEFLATE)~%")
                (format out "// NOTE: Original pixel data simplified to GRAYSCALE~%")
                (format out "static unsigned char fontData_~a[COMPRESSED_DATA_SIZE_FONT_~a] = { " file-name-pascal (text-to-upper file-name-pascal))
                (dotimes (i (- comp-data-size 1))
                  (format out (if (= (mod i text-bytes-per-line) 0) "0x~(~2,'0x~),~%    " "0x~(~2,'0x~), ") (aref comp-data i)))
                (format out "0x~(~2,'0x~) };~%~%" (aref comp-data (- comp-data-size 1))))
              ;; Save font recs data
              (format out "// Font characters rectangles data~%")
              (format out "static Rectangle fontRecs_~a[~d] = {~%" file-name-pascal (font-glyph-count font))
              (dotimes (i (font-glyph-count font))
                (let ((rec (aref (font-recs font) i)))
                  (format out "~a" (%sprintf "    { %1.0f, %1.0f, %1.0f , %1.0f },~%" (rectangle-x rec) (rectangle-y rec)
                                             (rectangle-width rec) (rectangle-height rec)))))
              (format out "};~%~%")
              ;; Save font glyphs data
              ;; NOTE: Glyphs image data not saved (grayscale pixels), it could be generated from image and recs
              (format out "// Font glyphs info data~%")
              (format out "// NOTE: No glyphs.image data provided~%")
              (format out "static GlyphInfo fontGlyphs_~a[~d] = {~%" file-name-pascal (font-glyph-count font))
              (dotimes (i (font-glyph-count font))
                (let ((glyph (aref (font-glyphs font) i)))
                  (format out "    { ~d, ~d, ~d, ~d, { 0 }},~%" (glyph-info-value glyph) (glyph-info-offset-x glyph)
                          (glyph-info-offset-y glyph) (glyph-info-advance-x glyph))))
              (format out "};~%~%")
              ;; Custom font loading function
              (format out "// Font loading function: ~a~%" file-name-pascal)
              (format out "static Font LoadFont_~a(void)~%{~%" file-name-pascal)
              (format out "    Font font = { 0 };~%~%")
              (format out "    font.baseSize = ~d;~%" (font-base-size font))
              (format out "    font.glyphCount = ~d;~%" (font-glyph-count font))
              (format out "    font.glyphPadding = ~d;~%~%" (font-glyph-padding font))
              (format out "    // Custom font loading~%")
              (format out "    // NOTE: Compressed font image data (DEFLATE), it requires DecompressData() function~%")
              (format out "    int fontDataSize_~a = 0;~%" file-name-pascal)
              (format out "    unsigned char *data = DecompressData(fontData_~a, COMPRESSED_DATA_SIZE_FONT_~a, &fontDataSize_~a);~%"
                      file-name-pascal (text-to-upper file-name-pascal) file-name-pascal)
              (format out "    Image imFont = { data, ~d, ~d, 1, ~d };~%~%" (image-width image) (image-height image) (image-pixel-format image))
              (format out "    // Load texture from image~%")
              (format out "    font.texture = LoadTextureFromImage(imFont);~%")
              (format out "    UnloadImage(imFont);  // Uncompressed data can be unloaded from memory~%~%")
              ;; There are two possible mechanisms to assign font.recs and font.glyphs data,
              ;; that data is already available as global arrays, two options to assign that data:
              ;;  - 1. Data copy. This option consumes more memory and Font MUST be unloaded by user, requiring additional code
              ;;  - 2. Data assignment. This option consumes less memory and Font MUST NOT be unloaded by user because data is on protected DATA segment
              (format out "    // Assign glyph recs and info data directly~%")
              (format out "    // WARNING: This font data must not be unloaded~%")
              (format out "    font.recs = fontRecs_~a;~%" file-name-pascal)
              (format out "    font.glyphs = fontGlyphs_~a;~%~%" file-name-pascal)
              (format out "    return font;~%")
              (format out "}~%"))))
      (unload-image image)
      ;; NOTE: Text data size exported is determined by '\0' (NULL) character
      (setf result (save-file-text file-name txt-data)))
    (if result
        (trace-log-info "FILEIO: [~a] Font as code exported successfully" file-name)
        (trace-log-warning "FILEIO: [~a] Failed to export font as code" file-name))
    result))

(defun draw-fps (pos-x pos-y)
  "Draw current FPS
NOTE: Uses default font"
  (let* ((fps (get-fps))
         (color (cond ((and (< fps 30) (>= fps 15)) +orange+) ; Warning FPS
                      ((< fps 15) +red+)                     ; Low FPS
                      (t +lime+))))                          ; Good FPS
    (draw-text (text-format "%2i FPS" fps) pos-x pos-y 20 color)))

(defun draw-text (text pos-x pos-y font-size color)
  "Draw text (using default font)
NOTE: fontSize work like in any drawing program but if fontSize is lower than font-base-size, then font-base-size is used
NOTE: chars spacing is proportional to fontSize"
  ;; Check if default font has been loaded
  (when (/= (texture-id (font-texture (get-font-default))) 0)
    (let ((position (vec2 (float pos-x 1.0) (float pos-y 1.0)))
          (default-font-size 10))       ; Default Font chars height in pixel
      (when (< font-size default-font-size) (setf font-size default-font-size))
      (let ((spacing (truncate font-size default-font-size)))
        (draw-text-ex (get-font-default) text position (float font-size 1.0) (float spacing 1.0) color)))))

(defun draw-text-ex (font text position font-size spacing tint)
  "Draw text using Font
NOTE: chars spacing is NOT proportional to fontSize"
  (when (or (null font) (null (font-texture font)) (= (texture-id (font-texture font)) 0))
    (setf font (get-font-default)))     ; Security check in case of not valid font
  (let* ((font-size (float font-size 1.0))
         (spacing (float spacing 1.0))
         (pos-x (%x position)) (pos-y (%y position))
         (text-offset-y 0.0)            ; Offset between lines (on linebreak '\n')
         (text-offset-x 0.0)            ; Offset X to next character to draw
         (scale-factor (/ font-size (font-base-size font)))) ; Character quad scaling factor
    (loop for ch across text
          do (let* ((codepoint (char-code ch))
                    (index (get-glyph-index font codepoint)))
               (if (= codepoint 10)     ; '\n'
                   (progn
                     ;; NOTE: Line spacing is a global variable, use SetTextLineSpacing() to setup
                     (incf text-offset-y (+ font-size *text-line-spacing*))
                     (setf text-offset-x 0.0))
                   (progn
                     (when (and (/= codepoint 32) (/= codepoint 9))
                       (draw-text-codepoint font codepoint (vec2 (+ pos-x text-offset-x) (+ pos-y text-offset-y)) font-size tint))
                     (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                         (incf text-offset-x (+ (* (rectangle-width (aref (font-recs font) index)) scale-factor) spacing))
                         (incf text-offset-x (+ (* (float (glyph-info-advance-x (aref (font-glyphs font) index)) 1.0) scale-factor) spacing)))))))
    (values)))

(defun draw-text-pro (font text position origin rotation font-size spacing tint)
  "Draw text using Font and pro parameters (rotation)"
  (rl-push-matrix)
  (rl-translatef (%x position) (%y position) 0.0)
  (rl-rotatef (float rotation 1.0) 0.0 0.0 1.0)
  (rl-translatef (- (%x origin)) (- (%y origin)) 0.0)
  (draw-text-ex font text (vec2 0.0 0.0) font-size spacing tint)
  (rl-pop-matrix)
  (values))

(defun draw-text-codepoint (font codepoint position font-size tint)
  "Draw one character (codepoint)"
  ;; Character index position in sprite font
  ;; NOTE: In case a codepoint is not available in the font, index returned points to '?'
  (let* ((index (get-glyph-index font codepoint))
         (font-size (float font-size 1.0))
         (scale-factor (/ font-size (font-base-size font))) ; Character quad scaling factor
         (glyph (aref (font-glyphs font) index))
         (rec (aref (font-recs font) index))
         (padding (float (font-glyph-padding font) 1.0))
         ;; Character destination rectangle on screen
         ;; NOTE: Considering glyph padding on drawing
         (dst-rec (make-rectangle :x (- (+ (%x position) (* (glyph-info-offset-x glyph) scale-factor)) (* padding scale-factor))
                                  :y (- (+ (%y position) (* (glyph-info-offset-y glyph) scale-factor)) (* padding scale-factor))
                                  :width (* (+ (rectangle-width rec) (* 2.0 (font-glyph-padding font))) scale-factor)
                                  :height (* (+ (rectangle-height rec) (* 2.0 (font-glyph-padding font))) scale-factor)))
         ;; Character source rectangle from font texture atlas
         ;; NOTE: Considering glyphs padding when drawing, it could be required for outline/glow shader effects
         (src-rec (make-rectangle :x (- (rectangle-x rec) padding) :y (- (rectangle-y rec) padding)
                                  :width (+ (rectangle-width rec) (* 2.0 (font-glyph-padding font)))
                                  :height (+ (rectangle-height rec) (* 2.0 (font-glyph-padding font))))))
    ;; Draw the character texture on the screen
    (draw-texture-pro (font-texture font) src-rec dst-rec (vec2 0.0 0.0) 0.0 tint))
  (values))

(defun draw-text-codepoints (font codepoints codepoint-count position font-size spacing tint)
  "Draw multiple characters (codepoints)"
  (let* ((font-size (float font-size 1.0))
         (spacing (float spacing 1.0))
         (text-offset-y 0.0)            ; Offset between lines (on linebreak '\n')
         (text-offset-x 0.0)            ; Offset X to next character to draw
         (scale-factor (/ font-size (font-base-size font)))) ; Character quad scaling factor
    (dotimes (i codepoint-count)
      (let* ((cp (elt codepoints i))
             (index (get-glyph-index font cp)))
        (if (= cp 10)
            (progn
              ;; NOTE: Line spacing is a global variable, use SetTextLineSpacing() to setup
              (incf text-offset-y (+ font-size *text-line-spacing*))
              (setf text-offset-x 0.0))
            (progn
              (when (and (/= cp 32) (/= cp 9))
                (draw-text-codepoint font cp (vec2 (+ (%x position) text-offset-x) (+ (%y position) text-offset-y)) font-size tint))
              (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                  (incf text-offset-x (+ (* (rectangle-width (aref (font-recs font) index)) scale-factor) spacing))
                  (incf text-offset-x (+ (* (float (glyph-info-advance-x (aref (font-glyphs font) index)) 1.0) scale-factor) spacing)))))))
    (values)))

(defun set-text-line-spacing (spacing)
  "Set vertical line spacing when drawing with line-breaks"
  (setf *text-line-spacing* spacing)
  (values))

(defun measure-text (text font-size)
  "Measure string width for default font"
  (let ((text-size (vec2 0.0 0.0)))
    ;; Check if default font has been loaded
    (when (/= (texture-id (font-texture (get-font-default))) 0)
      (let ((default-font-size 10))     ; Default Font glyphs height in pixel
        (when (< font-size default-font-size) (setf font-size default-font-size))
        (let ((spacing (truncate font-size default-font-size)))
          (setf text-size (measure-text-ex (get-font-default) text (float font-size 1.0) (float spacing 1.0))))))
    (truncate (vx text-size))))

(defun measure-text-ex (font text font-size spacing)
  "Measure string size for Font"
  (when (or (null font) (null (font-texture font)) (= (texture-id (font-texture font)) 0)
            (null text) (= (length text) 0))
    (return-from measure-text-ex (vec2 0.0 0.0))) ; Security check
  (let* ((font-size (float font-size 1.0))
         (spacing (float spacing 1.0))
         (temp-byte-counter 0)          ; Used to count longer text line num chars
         (byte-counter 0)
         (text-width 0.0)
         (temp-text-width 0.0)          ; Used to count longer text line width
         (text-height font-size)
         (scale-factor (/ font-size (float (font-base-size font) 1.0))))
    (loop for ch across text
          do (incf byte-counter)
             (let* ((letter (char-code ch))
                    (index (get-glyph-index font letter))
                    (glyph (aref (font-glyphs font) index)))
               (if (/= letter 10)
                   (if (> (glyph-info-advance-x glyph) 0)
                       (incf text-width (glyph-info-advance-x glyph))
                       (incf text-width (+ (rectangle-width (aref (font-recs font) index)) (glyph-info-offset-x glyph))))
                   (progn
                     (when (< temp-text-width text-width) (setf temp-text-width text-width))
                     (setf byte-counter 0)
                     (setf text-width 0.0)
                     ;; NOTE: Line spacing is a global variable, use SetTextLineSpacing() to setup
                     (incf text-height (+ font-size *text-line-spacing*))))
               (when (< temp-byte-counter byte-counter) (setf temp-byte-counter byte-counter))))
    (when (< temp-text-width text-width) (setf temp-text-width text-width))
    (vec2 (+ (* temp-text-width scale-factor) (float (* (- temp-byte-counter 1) spacing) 1.0))
          text-height)))

(defun measure-text-codepoints (font codepoints length font-size spacing)
  "Measure string size for an existing array of codepoints for Font"
  (when (or (null font) (null (font-texture font)) (= (texture-id (font-texture font)) 0)
            (null codepoints) (= length 0))
    (return-from measure-text-codepoints (vec2 0.0 0.0))) ; Security check
  (let* ((font-size (float font-size 1.0))
         (spacing (float spacing 1.0))
         (text-width 0.0)
         (temp-text-width 0.0)          ; Used to count longer text line width
         (temp-glyph-counter 0)         ; Used to count longer text line num chars
         (glyph-counter 0)
         (text-height font-size)
         (scale-factor (/ font-size (float (font-base-size font) 1.0))))
    (dotimes (i length)
      (let* ((letter (elt codepoints i))
             (index (get-glyph-index font letter))
             (glyph (aref (font-glyphs font) index)))
        (if (/= letter 10)
            (progn
              (incf glyph-counter)
              (if (> (glyph-info-advance-x glyph) 0)
                  (incf text-width (glyph-info-advance-x glyph))
                  (incf text-width (+ (rectangle-width (aref (font-recs font) index)) (glyph-info-offset-x glyph)))))
            (progn
              (when (< temp-text-width text-width) (setf temp-text-width text-width))
              (setf text-width 0.0)
              (setf glyph-counter 0)
              ;; NOTE: Line spacing is a global variable, use SetTextLineSpacing() to setup
              (incf text-height (+ font-size *text-line-spacing*))))
        (when (< temp-glyph-counter glyph-counter) (setf temp-glyph-counter glyph-counter))))
    (when (< temp-text-width text-width) (setf temp-text-width text-width))
    (vec2 (+ (* temp-text-width scale-factor) (float (* (- temp-glyph-counter 1) spacing) 1.0))
          text-height)))

(defun get-glyph-index (font codepoint)
  "Get index position for a unicode character on font
NOTE: If codepoint is not found in the font it fallbacks to '?'"
  (let ((index 0))
    (unless (is-font-valid font) (return-from get-glyph-index index))
    ;; SUPPORT_UNORDERED_CHARSET
    (let ((fallback-index 0)            ; Get index of fallback glyph '?'
          (glyphs (font-glyphs font)))
      ;; Look for character index in the unordered charset
      (dotimes (i (font-glyph-count font))
        (when (= (glyph-info-value (aref glyphs i)) 63) (setf fallback-index i))
        (when (= (glyph-info-value (aref glyphs i)) codepoint)
          (setf index i)
          (return)))
      (when (and (= index 0) (/= (glyph-info-value (aref glyphs 0)) codepoint))
        (setf index fallback-index)))
    index))

(defun get-glyph-info (font codepoint)
  "Get glyph font info data for a codepoint (unicode character)
NOTE: If codepoint is not found in the font it fallbacks to '?'"
  (aref (font-glyphs font) (get-glyph-index font codepoint)))

(defun get-glyph-atlas-rec (font codepoint)
  "Get glyph rectangle in font atlas for a codepoint (unicode character)
NOTE: If codepoint is not found in the font it fallbacks to '?'"
  (aref (font-recs font) (get-glyph-index font codepoint)))

;;;----------------------------------------------------------------------------------
;;; C printf() formatting, used by TextFormat()
;;;----------------------------------------------------------------------------------

(defun %c-format-fixed (x precision)
  "C %.Nf of a double (absolute value): exact decimal expansion, ties rounded to even as glibc does"
  (let* ((scaled (round (* (abs (rational x)) (expt 10 precision)))) ; ROUND rounds ties to even
         (digits (format nil "~d" scaled)))
    (if (= precision 0)
        digits
        (progn
          (when (<= (length digits) precision)
            (setf digits (concatenate 'string (make-string (- (1+ precision) (length digits)) :initial-element #\0) digits)))
          (concatenate 'string (subseq digits 0 (- (length digits) precision)) "."
                       (subseq digits (- (length digits) precision)))))))

(defun %c-format-exp (x precision upper)
  "C %.Ne of a double, returns (values mantissa-exponent-string)"
  (let ((ax (abs x)) (e 0))
    (unless (zerop ax)
      (setf e (floor (log ax 10d0)))
      ;; correct floating rounding of the exponent
      (when (>= (/ ax (expt 10d0 e)) 10d0) (incf e))
      (when (< (/ ax (expt 10d0 e)) 1d0) (decf e)))
    (let* ((m (if (zerop ax) 0d0 (/ ax (expt 10d0 e))))
           (ms (%c-format-fixed m precision)))
      ;; mantissa rounding can carry to 10.0
      (when (and (>= (length ms) 2) (string= (subseq ms 0 2) "10"))
        (incf e)
        (setf ms (%c-format-fixed (/ ax (expt 10d0 e)) precision)))
      (format nil "~a~a~a~2,'0d" ms (if upper "E" "e") (if (< e 0) "-" "+") (abs e)))))

(defun %sprintf (control &rest args)
  "Format ARGS according to the C printf() CONTROL string
Supported: flags -+ #0, width, precision (* too), length modifiers (ignored),
conversions d i u x X o c s f F e E g G %"
  (with-output-to-string (out)
    (let ((i 0) (n (length control)))
      (flet ((next-arg () (pop args)))
        (loop while (< i n)
              do (let ((c (char control i)))
                   (if (char/= c #\%)
                       (progn (write-char c out) (incf i))
                       (let ((left nil) (plus nil) (space nil) (alt nil) (zero nil)
                             (width nil) (precision nil))
                         (incf i)
                         ;; Flags
                         (loop while (and (< i n) (find (char control i) "-+ #0"))
                               do (case (char control i)
                                    (#\- (setf left t)) (#\+ (setf plus t)) (#\Space (setf space t))
                                    (#\# (setf alt t)) (#\0 (setf zero t)))
                                  (incf i))
                         ;; Width
                         (if (and (< i n) (char= (char control i) #\*))
                             (progn (setf width (next-arg)) (incf i)
                                    (when (< width 0) (setf left t width (- width))))
                             (loop while (and (< i n) (digit-char-p (char control i)))
                                   do (setf width (+ (* (or width 0) 10) (digit-char-p (char control i))))
                                      (incf i)))
                         ;; Precision
                         (when (and (< i n) (char= (char control i) #\.))
                           (incf i)
                           (setf precision 0)
                           (if (and (< i n) (char= (char control i) #\*))
                               (progn (setf precision (next-arg)) (incf i))
                               (loop while (and (< i n) (digit-char-p (char control i)))
                                     do (setf precision (+ (* precision 10) (digit-char-p (char control i))))
                                        (incf i))))
                         ;; Length modifiers
                         (loop while (and (< i n) (find (char control i) "hlLqjzt")) do (incf i))
                         (when (>= i n) (return))
                         (let* ((conv (char control i))
                                (body
                                  (case conv
                                    (#\% "%")
                                    ((#\d #\i #\u)
                                     (let* ((v (next-arg))
                                            (v (if (integerp v) v (truncate v)))
                                            (digits (format nil "~d" (abs v))))
                                       (when (and precision (= precision 0) (= v 0)) (setf digits ""))
                                       (when (and precision (< (length digits) precision))
                                         (setf digits (concatenate 'string (make-string (- precision (length digits)) :initial-element #\0) digits)))
                                       (concatenate 'string (cond ((< v 0) "-") (plus "+") (space " ") (t "")) digits)))
                                    ((#\x #\X #\o)
                                     (let* ((v (next-arg))
                                            (v (logand (if (integerp v) v (truncate v)) #xffffffff))
                                            (digits (format nil (if (char= conv #\o) "~o" "~x") v)))
                                       (when (char= conv #\x) (setf digits (string-downcase digits)))
                                       (when (and precision (< (length digits) precision))
                                         (setf digits (concatenate 'string (make-string (- precision (length digits)) :initial-element #\0) digits)))
                                       (if (and alt (/= v 0))
                                           (concatenate 'string (case conv (#\x "0x") (#\X "0X") (t "0")) digits)
                                           digits)))
                                    (#\c (let ((v (next-arg))) (string (if (characterp v) v (code-char v)))))
                                    (#\s (let* ((v (next-arg))
                                                (s (if (stringp v) v (princ-to-string v))))
                                           (if (and precision (< precision (length s))) (subseq s 0 precision) s)))
                                    ((#\f #\F #\e #\E #\g #\G)
                                     ;; NOTE: C varargs promote float to double
                                     (let* ((v (float (next-arg) 1d0))
                                            (prec (or precision 6))
                                            ;; NOTE: Sign bit is used (C prints -0.000000 and -nan)
                                            (sign (cond ((minusp (float-sign v)) "-") (plus "+") (space " ") (t "")))
                                            (digits
                                              (cond
                                                ((sb-ext:float-nan-p v) (if (upper-case-p conv) "NAN" "nan"))
                                                ((sb-ext:float-infinity-p v) (if (upper-case-p conv) "INF" "inf"))
                                                (t
                                              (case conv
                                                ((#\f #\F) (%c-format-fixed v prec))
                                                ((#\e #\E) (%c-format-exp v prec (char= conv #\E)))
                                                (t
                                                 (let* ((p (if (= prec 0) 1 prec))
                                                        (es (%c-format-exp v (- p 1) nil))
                                                        (x (parse-integer es :start (1+ (position #\e es)))))
                                                   (let ((s (if (and (< x p) (>= x -4))
                                                                (%c-format-fixed v (- p 1 x))
                                                                (%c-format-exp v (- p 1) (char= conv #\G)))))
                                                     ;; Remove trailing zeros unless '#'
                                                     (unless alt
                                                       (let* ((epos (position-if (lambda (ch) (char-equal ch #\e)) s))
                                                              (mant (subseq s 0 (or epos (length s))))
                                                              (expo (if epos (subseq s epos) "")))
                                                         (when (find #\. mant)
                                                           (setf mant (string-right-trim "0" mant))
                                                           (setf mant (string-right-trim "." mant)))
                                                         (setf s (concatenate 'string mant expo))))
                                                     s))))))))
                                       (concatenate 'string sign digits)))
                                    (t (string conv)))))
                           (incf i)
                           ;; Padding
                           (when (and width (< (length body) width) (char/= conv #\%))
                             (let ((pad (- width (length body))))
                               (cond (left (setf body (concatenate 'string body (make-string pad :initial-element #\Space))))
                                     ((and zero (find conv "diuxXofFeEgG") (not (and precision (find conv "diuxXo"))))
                                      ;; zero padding goes after the sign/prefix
                                      (let ((prefix-len (cond ((and (> (length body) 0) (find (char body 0) "+- ")) 1)
                                                              ((and (> (length body) 1) (string-equal (subseq body 0 2) "0x")) 2)
                                                              (t 0))))
                                        (setf body (concatenate 'string (subseq body 0 prefix-len)
                                                                (make-string pad :initial-element #\0)
                                                                (subseq body prefix-len)))))
                                     (t (setf body (concatenate 'string (make-string pad :initial-element #\Space) body))))))
                           (write-string body out))))))))))

;;;----------------------------------------------------------------------------------
;;; Text strings management functions
;;;----------------------------------------------------------------------------------

(defun load-text-lines (text)
  "Load text as separate lines ('\\n'), returns (values lines line-count)"
  (if (null text)
      (values nil 0)
      (let ((lines (uiop:split-string text :separator '(#\Newline))))
        (values lines (length lines)))))

(defun unload-text-lines (lines line-count)
  "Unload text lines"
  (declare (ignore lines line-count))
  (values))

(defun text-length (text)
  "Get text length (characters), check for \\0 character"
  (if (null text)
      0
      (or (position (code-char 0) text) (length text))))

(defun text-format (text &rest args)
  "Text formatting with variables (sprintf() style)
NOTE: Lisp FORMAT control strings (with ~ directives and no % directives) are also accepted"
  (if (null text)
      ""
      (let ((result (if (and (find #\~ text) (not (find #\% text)))
                        (apply #'format nil text args)
                        (apply #'%sprintf text args))))
        ;; If requiredByteCount is larger than the MAX_TEXT_BUFFER_LENGTH, then overflow occurred
        (if (>= (length result) +max-text-buffer-length+)
            ;; Inserting "..." at the end of the string to mark as truncated
            (concatenate 'string (subseq result 0 (- +max-text-buffer-length+ 4)) "...")
            result))))

(defun text-to-integer (text)
  "Get integer value from text
NOTE: This function replaces atoi() [stdlib.h]"
  (let ((value 0) (sign 1) (i 0))
    (when text
      (when (and (> (length text) 0) (member (char text 0) '(#\+ #\-)))
        (when (char= (char text 0) #\-) (setf sign -1))
        (incf i))
      (loop while (and (< i (length text)) (char<= #\0 (char text i) #\9))
            do (setf value (logand (+ (* value 10) (- (char-code (char text i)) 48)) #xffffffff))
               (incf i))
      (when (>= value #x80000000) (decf value #x100000000)))
    (* value sign)))

(defun text-to-float (text)
  "Get float value from text
NOTE: This function replaces atof() [stdlib.h]
WARNING: Only '.' character is understood as decimal point"
  (let ((value 0.0) (sign 1.0) (i 0))
    (when text
      (when (and (> (length text) 0) (member (char text 0) '(#\+ #\-)))
        (when (char= (char text 0) #\-) (setf sign -1.0))
        (incf i))
      (loop while (and (< i (length text)) (char<= #\0 (char text i) #\9))
            do (setf value (+ (* value 10.0) (float (- (char-code (char text i)) 48) 1.0)))
               (incf i))
      (when (and (< i (length text)) (char= (char text i) #\.))
        (incf i)
        (let ((divisor 10.0))
          (loop while (and (< i (length text)) (char<= #\0 (char text i) #\9))
                do (incf value (/ (float (- (char-code (char text i)) 48) 1.0) divisor))
                   (setf divisor (* divisor 10.0))
                   (incf i)))))
    (* value sign)))

(defun text-copy (dst src)
  "Copy one string to another, returns characters copied
NOTE: DST must be a string with enough space or an adjustable string with a fill pointer"
  (let ((bytes 0))
    (when (and src dst)
      (if (array-has-fill-pointer-p dst)
          (progn
            (setf (fill-pointer dst) 0)
            (loop for c across src do (vector-push-extend c dst)))
          (replace dst src))
      (setf bytes (length src)))
    bytes))

(defun text-is-equal (text1 text2)
  "Check if two text string are equal"
  (and text1 text2 (string= text1 text2)))

(defun text-subtext (text position length)
  "Get a piece of a text string"
  (if (and text (>= position 0) (> length 0))
      (let ((text-length (text-length text)))
        (if (< position text-length)
            (let ((max-length (- text-length position)))
              (when (> length max-length) (setf length max-length))
              (when (>= length +max-text-buffer-length+) (setf length (- +max-text-buffer-length+ 1)))
              (subseq text position (+ position length)))
            ""))
      ""))

(defun text-remove-spaces (text)
  "Remove text spaces, concat words"
  (if text
      (remove #\Space (subseq text 0 (min (length text) (- +max-text-buffer-length+ 1))))
      ""))

(defun get-text-between (text begin end)
  "Get text between two strings"
  (let ((begin-index (text-find-index text begin)))
    (if (> begin-index -1)
        (let* ((begin-len (text-length begin))
               (end-index (text-find-index (subseq text (+ begin-index begin-len)) end)))
          (if (> end-index -1)
              (let* ((end-index (+ end-index begin-index begin-len))
                     (len (- end-index begin-index begin-len)))
                (if (< len (- +max-text-buffer-length+ 1))
                    (subseq text (+ begin-index begin-len) end-index)
                    (let ((rest (subseq text (+ begin-index begin-len))))
                      (subseq rest 0 (min (length rest) (- +max-text-buffer-length+ 1))))))
              ""))
        "")))

(defun %text-replace (text search replacement)
  (with-output-to-string (out)
    (let ((start 0) (search-len (length search)))
      (loop for pos = (search search text :start2 start)
            while pos
            do (write-string text out :start start :end pos)
               (write-string replacement out)
               (setf start (+ pos search-len)))
      (write-string text out :start start))))

(defun text-replace (text search replacement)
  "Replace text string
NOTE: Limited text replace functionality, using static string"
  (if (and text search (> (length search) 0))
      (let* ((replacement (or replacement ""))
             (count (loop with start = 0
                          for pos = (search search text :start2 start)
                          while pos count t do (setf start (+ pos (length search))))))
        (if (< (+ (length text) (* count (- (length replacement) (length search)))) (- +max-text-buffer-length+ 1))
            (%text-replace text search replacement)
            (progn
              (trace-log-warning "Text with replacement is longer than internal buffer, use TextReplaceAlloc()")
              "")))
      ""))

(defun text-replace-alloc (text search replacement)
  "Replace text string
WARNING: Allocated memory must be manually freed"
  (when (and text search (> (length search) 0))
    (%text-replace text search (or replacement ""))))

(defun text-replace-between (text begin end replacement)
  "Replace text between two specific strings
NOTE: If (replacement == NULL) removes \"begin\"[ ]\"end\" text"
  (let ((result (text-replace-between-alloc text begin end replacement)))
    (cond ((null result) "")
          ((>= (length result) (- +max-text-buffer-length+ 1))
           (trace-log-warning "TEXT: Text with replaced string is longer than internal buffer (MAX_TEXT_BUFFER_LENGTH)")
           "")
          (t result))))

(defun text-replace-between-alloc (text begin end replacement)
  "Replace text between two specific strings
NOTE: If (replacement == NULL) remove \"begin\"[ ]\"end\" text"
  (when (and text begin end)
    (let ((begin-index (text-find-index text begin)))
      (when (> begin-index -1)
        (let* ((begin-len (text-length begin))
               (end-index (text-find-index (subseq text (+ begin-index begin-len)) end)))
          (when (> end-index -1)
            (let ((end-index (+ end-index begin-index begin-len)))
              (concatenate 'string (subseq text 0 (+ begin-index begin-len))
                           (or replacement "")
                           (subseq text end-index)))))))))

(defun text-insert (text insert position)
  "Insert text in a specific position, moves all text forward"
  (let ((text-len (text-length text)))
    (if (and text insert (>= position 0))
        (progn
          (when (> position text-len) (setf position text-len)) ; End of text string
          (if (< (+ text-len (length insert)) (- +max-text-buffer-length+ 1))
              (concatenate 'string (subseq text 0 position) insert (subseq text position text-len))
              (progn
                (trace-log-warning "Text with inserted string is longer than internal buffer, use TextInserExt()")
                "")))
        "")))

(defun text-insert-alloc (text insert position)
  "Insert text in a specific position, moves all text forward"
  (let ((text-len (text-length text)))
    (when (and text insert (>= position 0))
      (when (> position text-len) (setf position text-len)) ; End of text string
      (concatenate 'string (subseq text 0 position) insert (subseq text position text-len)))))

(defun text-join (text-list count delimiter)
  "Join text strings with delimiter"
  (let ((total-length 0)
        (delimiter-len (text-length delimiter)))
    (with-output-to-string (out)
      (loop for i from 0 below count
            for text = (elt text-list i)
            do (let ((text-length (text-length text)))
                 ;; Make sure joined text could fit inside MAX_TEXT_BUFFER_LENGTH
                 (when (< (+ total-length text-length delimiter-len) +max-text-buffer-length+)
                   (write-string text out :end text-length)
                   (incf total-length text-length)
                   (when (and (> delimiter-len 0) (< i (- count 1)))
                     (write-string delimiter out)
                     (incf total-length delimiter-len))))))))

(defun text-split (text delimiter)
  "Split string into multiple strings, returns (values strings count)"
  (let ((delimiter (if (characterp delimiter) delimiter (char (string delimiter) 0))))
    (if (null text)
        (values (list "") 0)
        ;; NOTE: Last buffer byte is reserved to terminate the last substring
        (let ((text (subseq text 0 (min (text-length text) (- +max-text-buffer-length+ 1))))
              (result '())
              (start 0)
              (counter 1))
          (loop for i from 0 below (length text)
                do (when (char= (char text i) delimiter)
                     (push (subseq text start i) result)
                     (setf start (1+ i))
                     (incf counter)
                     (when (= counter +max-textsplit-count+)
                       (return))))
          (push (subseq text start (if (= counter +max-textsplit-count+)
                                       (or (position delimiter text :start start) (length text))
                                       (length text)))
                result)
          (values (nreverse result) counter)))))

(defun text-append (text append position)
  "Append text at specific position and move cursor
Returns (values new-text new-position)"
  (if (and text append)
      (values (concatenate 'string (subseq text 0 (min position (length text))) append)
              (+ position (text-length append)))
      (values text position)))

(defun text-find-index (text search)
  "Find first text occurrence within a string, -1 if not found"
  (let ((position -1))
    (when text
      (let ((ptr (search search text)))
        (when ptr (setf position ptr))))
    position))

(defun %text-limit (text)
  (subseq text 0 (min (length text) (- +max-text-buffer-length+ 1))))

(defun text-to-upper (text)
  "Get upper case version of provided string
WARNING: Limited functionality, only basic characters set"
  (if text
      (map 'string (lambda (c) (if (char<= #\a c #\z) (code-char (- (char-code c) 32)) c)) (%text-limit text))
      ""))

(defun text-to-lower (text)
  "Get lower case version of provided string
WARNING: Limited functionality, only basic characters set"
  (if text
      (map 'string (lambda (c) (if (char<= #\A c #\Z) (code-char (+ (char-code c) 32)) c)) (%text-limit text))
      ""))

(defun %text-to-pascal-or-camel (text first-upper)
  (if (or (null text) (= (length text) 0))
      ""
      (with-output-to-string (out)
        ;; Upper (or lower) case first character
        (let ((c0 (char text 0)))
          (write-char (cond ((and first-upper (char<= #\a c0 #\z)) (code-char (- (char-code c0) 32)))
                            ((and (not first-upper) (char<= #\A c0 #\Z)) (code-char (+ (char-code c0) 32)))
                            (t c0))
                      out))
        ;; Check for next separator to upper case another character
        (let ((i 1) (j 1) (n (length text)))
          (loop while (and (< i (- +max-text-buffer-length+ 1)) (< j n))
                do (if (char/= (char text j) #\_)
                       (write-char (char text j) out)
                       (progn
                         (loop while (and (< j n) (char= (char text j) #\_)) do (incf j)) ; Skip one or more separators
                         (when (>= j n) (return)) ; Text ends on a separator, nothing left to copy
                         (let ((c (char text j)))
                           (write-char (if (char<= #\a c #\z) (code-char (- (char-code c) 32)) c) out))))
                   (incf i) (incf j))))))

(defun text-to-pascal (text)
  "Get Pascal case notation version of provided string
WARNING: Limited functionality, only basic characters set"
  (%text-to-pascal-or-camel text t))

(defun text-to-camel (text)
  "Get Camel case notation version of provided string
WARNING: Limited functionality, only basic characters set"
  (%text-to-pascal-or-camel text nil))

(defun text-to-snake (text)
  "Get snake case notation version of provided string
WARNING: Limited functionality, only basic characters set"
  (if (null text)
      ""
      (let ((buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
            (n (length text)))
        (flet ((last-char () (when (> (fill-pointer buffer) 0) (char buffer (1- (fill-pointer buffer)))))
               (at (k) (if (and (>= k 0) (< k n)) (char text k) (code-char 0))))
          (loop for j from 0 below n
                while (< (fill-pointer buffer) (- +max-text-buffer-length+ 1))
                do (let ((c (char text j)))
                     (cond
                       ((char= c #\Space)
                        (when (and (> (fill-pointer buffer) 0) (char/= (last-char) #\_))
                          (vector-push-extend #\_ buffer)))
                       ((char<= #\A c #\Z)
                        (when (and (> (fill-pointer buffer) 0) (char/= (last-char) #\_))
                          (let ((prev (at (1- j)))
                                (next (at (1+ j))))
                            ;; Considering multiple cap leters to be on single word (HTTPRequest --> http_request)
                            (when (or (char<= #\a prev #\z)
                                      (and (char<= #\A prev #\Z) (char<= #\a next #\z)))
                              (when (< (fill-pointer buffer) (- +max-text-buffer-length+ 2))
                                (vector-push-extend #\_ buffer)))))
                        (vector-push-extend (code-char (+ (char-code c) 32)) buffer))
                       (t (vector-push-extend c buffer))))))
        (coerce buffer 'simple-string))))

(defun load-utf8 (codepoints length)
  "Encode text codepoint into UTF-8 text"
  (when (and codepoints (> length 0))
    (let ((out (make-string length)))
      (dotimes (i length out)
        (setf (char out i) (code-char (elt codepoints i)))))))

(defun unload-utf8 (text)
  "Unload UTF-8 text encoded from codepoints array"
  (declare (ignore text))
  (values))

(defun load-codepoints (text)
  "Load all codepoints from a UTF-8 text string, returns (values codepoints count)"
  (if (null text)
      (values nil 0)
      (let* ((length (text-length text))
             (codepoints (make-array length)))
        (dotimes (i length)
          (setf (aref codepoints i) (char-code (char text i))))
        (values codepoints length))))

(defun unload-codepoints (codepoints)
  "Unload codepoints data from memory"
  (declare (ignore codepoints))
  (values))

(defun get-codepoint-count (text)
  "Get total number of codepoints in a UTF-8 encoded string"
  (text-length text))

(defun %utf8-size (codepoint)
  (cond ((<= codepoint #x7f) 1)
        ((<= codepoint #x7ff) 2)
        ((<= codepoint #xffff) 3)
        ((<= codepoint #x10ffff) 4)
        (t 0)))

(defun codepoint-to-utf8 (codepoint)
  "Encode codepoint into utf8 text, returns (values string utf8-size)"
  (let ((size (%utf8-size codepoint)))
    (values (if (> size 0) (string (code-char codepoint)) "") size)))

(defun get-codepoint (text &optional (position 0))
  "Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
Returns (values codepoint codepoint-size), the size is given in UTF-8 bytes"
  (cond ((null text) (values #x3f 1))
        ((>= position (length text)) (values 0 1))
        (t (let ((codepoint (char-code (char text position))))
             (values codepoint (%utf8-size codepoint))))))

(defun get-codepoint-next (text &optional (position 0))
  "Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
Returns (values codepoint codepoint-size)"
  (get-codepoint text position))

(defun get-codepoint-previous (text position)
  "Get previous codepoint before POSITION in a UTF-8 encoded string, 0x3f('?') is returned on failure
Returns (values codepoint codepoint-size)"
  (if (or (null text) (<= position 0))
      (values #x3f 0)
      (let ((codepoint (char-code (char text (1- position)))))
        (values codepoint (if (/= codepoint 0) (%utf8-size codepoint) 0)))))

;;;----------------------------------------------------------------------------------
;;; Module Internal Functions Definition
;;;----------------------------------------------------------------------------------

(defun %get-line (text start max-length)
  "Read a line from memory, returns (values line bytes-read)"
  (let ((count 0))
    (loop while (and (< count (- max-length 1)) (< (+ start count) (length text))
                     (char/= (char text (+ start count)) #\Newline))
          do (incf count))
    (values (subseq text start (+ start count)) count)))

(defun %scan-key-ints (line keys)
  "sscanf() helper for \"key=%i key2=%i ...\" patterns starting at the first key,
returns the list of values read (stops at the first missing one)"
  (let ((pos 0) (values '()))
    (dolist (key keys (nreverse values))
      (let* ((pattern (concatenate 'string key "="))
             (found (search pattern line :start2 pos)))
        (unless found (return (nreverse values)))
        (multiple-value-bind (v end) (parse-integer line :start (+ found (length pattern)) :junk-allowed t)
          (unless v (return (nreverse values)))
          (push v values)
          (setf pos end))))))

(defun %load-bm-font (file-name)
  "Load a BMFont file (AngelCode font file)"
  (let* ((max-buffer-size 256)
         (max-font-image-pages 8)
         (font (make-font :texture (make-texture :id 0 :width 0 :height 0 :mipmaps 0 :format 0)))
         (file-text (load-file-text file-name))
         (ptr 0)
         font-size im-width im-height (page-count 1) glyph-count
         (im-file-names '()))
    (when (null file-text) (return-from %load-bm-font font))
    (flet ((next-line ()
             (multiple-value-bind (line count) (%get-line file-text ptr max-buffer-size)
               (incf ptr (+ count 1))
               line)))
      ;; NOTE: Skip first line, it contains no useful information
      (next-line)
      ;; Read line data
      (let* ((line (next-line))
             (search-point (search "lineHeight" line))
             (vals (and search-point
                        (%scan-key-ints (subseq line search-point) '("lineHeight" "base" "scaleW" "scaleH" "pages")))))
        (when (< (length vals) 4) (return-from %load-bm-font font)) ; Some data not available, file malformed
        (setf font-size (first vals)
              im-width (third vals)
              im-height (fourth vals))
        (when (fifth vals) (setf page-count (fifth vals))))
      (when (> page-count max-font-image-pages)
        (trace-log-warning "FONT: [~a] Font defines more pages than supported: ~d/~d" file-name page-count max-font-image-pages)
        (setf page-count max-font-image-pages))
      (dotimes (i page-count)
        (let* ((line (next-line))
               (search-point (search "file=\"" line))
               (end (and search-point (position #\" line :start (+ search-point 6)))))
          (when (or (null search-point) (null end) (= end (+ search-point 6)))
            (return-from %load-bm-font font)) ; No fileName read
          (push (subseq line (+ search-point 6) (min end (+ search-point 6 128))) im-file-names)))
      (setf im-file-names (nreverse im-file-names))
      (let* ((line (next-line))
             (search-point (search "count" line))
             (vals (and search-point (%scan-key-ints (subseq line search-point) '("count")))))
        (when (null vals) (return-from %load-bm-font font)) ; No glyphCount read
        (setf glyph-count (first vals)))
      ;; Load all required images for further compose
      (let ((im-fonts (make-array page-count))) ; Font atlases, multiple images
        (dotimes (i page-count)
          (setf (aref im-fonts i) (load-image (format nil "~a/~a" (get-directory-path file-name) (nth i im-file-names))))
          (when (= (image-pixel-format (aref im-fonts i)) +pixelformat-uncompressed-grayscale+)
            ;; Convert image to GRAYSCALE + ALPHA, using the mask as the alpha channel
            (let* ((im (aref im-fonts i))
                   (count (* (image-width im) (image-height im)))
                   (data (make-array (* count 2) :element-type '(unsigned-byte 8) :initial-element 0)))
              (dotimes (px count)
                (setf (aref data (* px 2)) #xff
                      (aref data (+ (* px 2) 1)) (aref (image-data im) px)))
              (unload-image im)
              (setf (aref im-fonts i) (make-image :data data :width (image-width im) :height (image-height im)
                                                  :mipmaps 1 :format +pixelformat-uncompressed-gray-alpha+)))))
        (let ((full-font (aref im-fonts 0)))
          ;; If multiple atlas, then merge atlas
          ;; NOTE: WARNING: This process could be really slow!
          (when (> page-count 1)
            ;; Resize font atlas to draw additional images
            (image-resize-canvas full-font im-width (* im-height page-count) 0 0 +black+)
            (loop for i from 1 below page-count
                  do (image-draw-image-pro full-font (aref im-fonts i)
                                           (make-rectangle :x 0.0 :y 0.0 :width (float im-width 1.0) :height (float im-height 1.0))
                                           (make-rectangle :x 0.0 :y (* (float im-height 1.0) (float i 1.0))
                                                           :width (float im-width 1.0) :height (float im-height 1.0))
                                           (vec2 0.0 0.0) 0.0 +white+)))
          (loop for i from 1 below page-count do (unload-image (aref im-fonts i)))
          (setf (font-texture font) (load-texture-from-image full-font))
          ;; Fill font characters info data
          (setf (font-base-size font) font-size
                (font-glyph-count font) glyph-count
                (font-glyph-padding font) 0
                (font-glyphs font) (make-array glyph-count)
                (font-recs font) (make-array glyph-count))
          (dotimes (i glyph-count)
            (let* ((line (next-line))
                   (vals (and (eql 0 (search "char id=" line))
                              (%scan-key-ints line '("id" "x" "y" "width" "height" "xoffset" "yoffset" "xadvance" "page")))))
              (if (= (length vals) 9)   ; Make sure all char data has been properly read
                  (destructuring-bind (char-id char-x char-y char-width char-height char-offset-x char-offset-y char-advance-x page-id) vals
                    ;; Get character rectangle in the font atlas texture
                    (setf (aref (font-recs font) i)
                          (make-rectangle :x (float char-x 1.0)
                                          :y (+ (float char-y 1.0) (* (float im-height 1.0) page-id))
                                          :width (float char-width 1.0) :height (float char-height 1.0)))
                    ;; Save data properly in sprite font
                    (setf (aref (font-glyphs font) i)
                          (make-glyph-info :value char-id :offset-x char-offset-x :offset-y char-offset-y
                                           :advance-x char-advance-x
                                           ;; Fill character image data from full font data
                                           :image (image-from-image full-font (aref (font-recs font) i)))))
                  (progn
                    (setf (aref (font-recs font) i) (make-rectangle))
                    (setf (aref (font-glyphs font) i)
                          (make-glyph-info :image (gen-image-color 0 0 +black+)))
                    (trace-log-warning "FONT: [~a] Some characters data not correctly provided" file-name)))))
          (unload-image full-font))))
    (if (= (texture-id (font-texture font)) 0)
        (progn
          (unload-font font)
          (setf font (get-font-default))
          (trace-log-warning "FONT: [~a] Failed to load texture, reverted to default font" file-name))
        (trace-log-info "FONT: [~a] Font loaded successfully (~d glyphs)" file-name (font-glyph-count font)))
    font))

(defun %hex-to-int (hex)
  "Convert hexadecimal to decimal (single digit)"
  (or (digit-char-p hex 16) 0))

(defun %load-font-data-bdf (file-data data-size codepoints codepoint-count out-font-size)
  "Load font data for further use
NOTE: Requires BDF font memory data
Returns (values glyphs font-size)"
  (let* ((max-buffer-size 256)
         (glyphs nil)
         (out-glyph nil)                ; Pointer to output glyph info (NULL if not set)
         (total-read-bytes 0)           ; Data bytes read (total)
         (file-text nil)
         (ptr 0)
         (font-malformed nil)           ; Is the font malformed
         (font-started nil)             ; Has font started (STARTFONT)
         (font-bbw 0) (font-bbh 0) (font-bbxoff0 0) (font-byoff0 0)
         (font-ascent 0)                ; Font ascent
         (char-started nil)             ; Has character started (STARTCHAR)
         (char-bitmap-started nil)      ; Has bitmap data started (BITMAP)
         (char-bitmap-next-row 0)       ; Y position for the next row of bitmap data
         (char-encoding -1)             ; The unicode value of the character (-1 if not set)
         (char-bbw 0) (char-bbh 0) (char-bbxoff0 0) (char-byoff0 0)
         (char-dwidth-x 0)              ; Character advance X
         required-codepoints)
    (declare (ignorable font-bbw font-bbxoff0))
    (when (null file-data) (return-from %load-font-data-bdf (values glyphs out-font-size)))
    (setf file-text (map 'string #'code-char file-data))
    ;; In case no chars count provided, default to 95
    (setf codepoint-count (if (> codepoint-count 0) codepoint-count 95))
    (setf required-codepoints (make-array codepoint-count))
    (if (null codepoints)
        ;; Fill internal codepoints array in case not provided externally
        ;; NOTE: By default, filling glyph count consecutively, starting at 32 (Space)
        (dotimes (i codepoint-count) (setf (aref required-codepoints i) (+ i 32)))
        (dotimes (i codepoint-count) (setf (aref required-codepoints i) (elt codepoints i))))
    (setf glyphs (make-array codepoint-count))
    (dotimes (i codepoint-count) (setf (aref glyphs i) (make-glyph-info :image (%empty-image))))
    (flet ((ints (line prefix count)
             ;; sscanf(buffer, "PREFIX %i ...") only matches at the beginning of the line
             (let ((pos (and (eql 0 (search prefix line)) 0)))
               (when pos
                 (let ((start (+ pos (length prefix))) (vals '()))
                   (dotimes (k count (nreverse vals))
                     (multiple-value-bind (v end) (parse-integer line :start start :junk-allowed t)
                       (unless v (return (nreverse vals)))
                       (push v vals)
                       (setf start end))))))))
      (block parse
        (loop while (<= total-read-bytes data-size)
              do (multiple-value-bind (buffer read-bytes) (%get-line file-text ptr max-buffer-size)
                   (incf total-read-bytes (+ read-bytes 1))
                   (incf ptr (+ read-bytes 1))
                   (cond
                     ;; Line: COMMENT
                     ((search "COMMENT" buffer) nil) ; Ignore line
                     (char-started
                      (cond
                        ;; Line: ENDCHAR
                        ((search "ENDCHAR" buffer)
                         (setf char-started nil))
                        (char-bitmap-started
                         (when out-glyph
                           (let ((pixel-y char-bitmap-next-row)
                                 (image (glyph-info-image out-glyph)))
                             (incf char-bitmap-next-row)
                             (when (>= pixel-y (image-height image)) (return-from parse))
                             (dotimes (x read-bytes)
                               (let ((byte (%hex-to-int (char buffer x))))
                                 (dotimes (bit-x 4)
                                   (let ((pixel-x (+ (* x 4) bit-x)))
                                     (when (>= pixel-x (image-width image)) (return))
                                     (when (> (logand byte (ash 8 (- bit-x))) 0)
                                       (setf (aref (image-data image) (+ (* pixel-y (image-width image)) pixel-x)) 255)))))))))
                        ;; Line: ENCODING
                        ((search "ENCODING" buffer)
                         (let ((v (ints buffer "ENCODING " 1))) (when v (setf char-encoding (first v)))))
                        ;; Line: BBX
                        ((search "BBX" buffer)
                         (let ((v (ints buffer "BBX " 4)))
                           (when (>= (length v) 1) (setf char-bbw (first v)))
                           (when (>= (length v) 2) (setf char-bbh (second v)))
                           (when (>= (length v) 3) (setf char-bbxoff0 (third v)))
                           (when (>= (length v) 4) (setf char-byoff0 (fourth v)))))
                        ;; Line: DWIDTH
                        ((search "DWIDTH" buffer)
                         (let ((v (ints buffer "DWIDTH " 2))) (when v (setf char-dwidth-x (first v)))))
                        ;; Line: BITMAP
                        ((search "BITMAP" buffer)
                         ;; Search for glyph index in codepoints
                         (setf out-glyph nil)
                         (dotimes (index codepoint-count)
                           (when (= (aref required-codepoints index) char-encoding)
                             (setf out-glyph (aref glyphs index))
                             (return)))
                         ;; Init glyph info
                         (when out-glyph
                           (setf (glyph-info-value out-glyph) char-encoding
                                 ;; BBX offsets place the glyph bitmap relative to the pen position on the baseline,
                                 ;; raylib offsets are measured from the top of the line, fontAscent above the baseline
                                 (glyph-info-offset-x out-glyph) char-bbxoff0
                                 (glyph-info-offset-y out-glyph) (- font-ascent (+ char-bbh char-byoff0))
                                 (glyph-info-advance-x out-glyph) char-dwidth-x
                                 (glyph-info-image out-glyph)
                                 (make-image :data (make-array (* char-bbw char-bbh) :element-type '(unsigned-byte 8) :initial-element 0)
                                             :width char-bbw :height char-bbh :mipmaps 1
                                             :format +pixelformat-uncompressed-grayscale+)))
                         (setf char-bitmap-started t
                               char-bitmap-next-row 0))))
                     (font-started
                      (cond
                        ;; Line: ENDFONT
                        ((search "ENDFONT" buffer)
                         (setf font-started nil)
                         (return-from parse))
                        ;; Line: SIZE
                        ((search "SIZE" buffer)
                         ;; NOTE: As in C, "PIXEL_SIZE" lines also match strstr("SIZE") and sscanf("SIZE %i") reads nothing
                         (let ((v (ints buffer "SIZE " 1)))
                           (when v (setf out-font-size (first v)))))
                        ;; FONTBOUNDINGBOX
                        ((search "FONTBOUNDINGBOX" buffer)
                         (let ((v (ints buffer "FONTBOUNDINGBOX " 4)))
                           (when (>= (length v) 4)
                             (setf font-bbw (first v) font-bbh (second v) font-bbxoff0 (third v) font-byoff0 (fourth v))))
                         (setf font-ascent (+ font-bbh font-byoff0))) ; Default if FONT_ASCENT property is not provided
                        ;; FONT_ASCENT
                        ((search "FONT_ASCENT" buffer)
                         (let ((v (ints buffer "FONT_ASCENT " 1))) (when v (setf font-ascent (first v)))))
                        ;; STARTCHAR
                        ((search "STARTCHAR" buffer)
                         (setf char-started t
                               char-encoding -1
                               out-glyph nil
                               char-bbw 0 char-bbh 0 char-bbxoff0 0 char-byoff0 0
                               char-dwidth-x 0
                               char-bitmap-started nil
                               char-bitmap-next-row 0))))
                     (t
                      ;; STARTFONT
                      (when (search "STARTFONT" buffer)
                        (if font-started
                            (progn (setf font-malformed t) (return-from parse))
                            (setf font-started t)))))))))
    (when font-malformed (setf glyphs nil))
    (values glyphs out-font-size)))
