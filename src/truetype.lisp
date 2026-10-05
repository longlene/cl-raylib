(in-package #:cl-raylib)

;;;===================================================================================
;;; stb_truetype - Load TTF/OTF font data and rasterize glyphs
;;; Port of the parts of raylib/src/external/stb_truetype.h (v1.26) used by rtext.c:
;;;   stbtt_InitFont, stbtt_FindGlyphIndex, stbtt_GetGlyphShape (TrueType + CFF),
;;;   stbtt_GetGlyphHMetrics, stbtt_GetFontVMetrics, stbtt_ScaleForPixelHeight,
;;;   stbtt_GetGlyphBitmapBox, stbtt_GetGlyphBitmap, stbtt_MakeGlyphBitmap,
;;;   stbtt_GetGlyphSDF and the version 2 rasterizer
;;; Port of raylib/src/external/stb_rect_pack.h (skyline packer) used by GenImageFontAtlas()
;;;
;;; NOTE: Float arithmetic follows C: single-float where C uses float,
;;; double-float where C calls double functions (sqrt, pow, fmod, cos, acos)
;;;===================================================================================

(deftype %octets () '(simple-array (unsigned-byte 8) (*)))

(defmacro %with-c-floats (&body body)
  "IEEE float semantics as in C (division by zero gives inf/NaN instead of signaling)"
  `(float-features:with-float-traps-masked t ,@body))

;;;----------------------------------------------------------------------------------
;;; Accessors to parse data from file
;;;----------------------------------------------------------------------------------

(declaim (inline %s16 %s8 tt-byte tt-char tt-ushort tt-short tt-ulong tt-long))
(defun %s16 (v) (let ((v (logand v #xffff))) (if (>= v #x8000) (- v #x10000) v)))
(defun %s8 (v) (let ((v (logand v #xff))) (if (>= v #x80) (- v #x100) v)))
(defun tt-byte (data p) (aref data p))
(defun tt-char (data p) (%s8 (aref data p)))
(defun tt-ushort (data p) (+ (* (aref data p) 256) (aref data (+ p 1))))
(defun tt-short (data p) (%s16 (tt-ushort data p)))
(defun tt-ulong (data p) (logior (ash (aref data p) 24) (ash (aref data (+ p 1)) 16)
                                 (ash (aref data (+ p 2)) 8) (aref data (+ p 3))))
(defun tt-long (data p) (let ((v (tt-ulong data p))) (if (>= v #x80000000) (- v #x100000000) v)))

(defun %tt-tag-p (data p tag)
  (and (<= (+ p 4) (length data))
       (loop for i from 0 below 4
             always (= (aref data (+ p i))
                       (let ((c (elt tag i))) (if (characterp c) (char-code c) c))))))

;;;----------------------------------------------------------------------------------
;;; stbtt__buf helpers to parse data from file
;;;----------------------------------------------------------------------------------

(defstruct (tt-buf (:constructor %make-tt-buf (data offset size &optional (cursor 0))))
  (data nil)
  (offset 0 :type fixnum)
  (size 0 :type fixnum)
  (cursor 0 :type fixnum))

(defun %tt-new-buf (data offset size) (%make-tt-buf data offset size))
(defun %tt-null-buf () (%make-tt-buf nil 0 0))
(defun %tt-buf-copy (b) (copy-tt-buf b))

(defun %tt-buf-get8 (b)
  (let ((i (+ (tt-buf-offset b) (tt-buf-cursor b))))
    (if (or (>= (tt-buf-cursor b) (tt-buf-size b))
            (null (tt-buf-data b)) (>= i (length (tt-buf-data b))))
        (progn (when (< (tt-buf-cursor b) (tt-buf-size b)) (incf (tt-buf-cursor b))) 0)
        (progn (incf (tt-buf-cursor b)) (aref (tt-buf-data b) i)))))

(defun %tt-buf-peek8 (b)
  (let ((i (+ (tt-buf-offset b) (tt-buf-cursor b))))
    (if (or (>= (tt-buf-cursor b) (tt-buf-size b))
            (null (tt-buf-data b)) (>= i (length (tt-buf-data b))))
        0
        (aref (tt-buf-data b) i))))

(defun %tt-buf-seek (b o)
  (setf (tt-buf-cursor b) (if (or (> o (tt-buf-size b)) (< o 0)) (tt-buf-size b) o)))

(defun %tt-buf-skip (b o) (%tt-buf-seek b (+ (tt-buf-cursor b) o)))

(defun %tt-buf-get (b n)
  (let ((v 0))
    (dotimes (i n v) (setf v (logand (logior (ash v 8) (%tt-buf-get8 b)) #xffffffff)))))

(defun %tt-buf-get16 (b) (%tt-buf-get b 2))
(defun %tt-buf-get32 (b) (%tt-buf-get b 4))

(defun %tt-buf-range (b o s)
  (if (or (< o 0) (< s 0) (> o (tt-buf-size b)) (> s (- (tt-buf-size b) o)))
      (%tt-null-buf)
      (%make-tt-buf (tt-buf-data b) (+ (tt-buf-offset b) o) s)))

(defun %tt-cff-get-index (b)
  (let ((start (tt-buf-cursor b))
        (count (%tt-buf-get16 b)))
    (when (/= count 0)
      (let ((offsize (%tt-buf-get8 b)))
        (%tt-buf-skip b (* offsize count))
        (%tt-buf-skip b (- (%tt-buf-get b offsize) 1))))
    (%tt-buf-range b start (- (tt-buf-cursor b) start))))

(defun %tt-cff-int (b)
  "stbtt__cff_int (returns the signed value, C stores it in a uint32)"
  (let ((b0 (%tt-buf-get8 b)))
    (cond ((and (>= b0 32) (<= b0 246)) (- b0 139))
          ((and (>= b0 247) (<= b0 250)) (+ (* (- b0 247) 256) (%tt-buf-get8 b) 108))
          ((and (>= b0 251) (<= b0 254)) (- (- (* (- b0 251) 256)) (%tt-buf-get8 b) 108))
          ((= b0 28) (%tt-buf-get16 b))
          ((= b0 29) (%tt-buf-get32 b))
          (t 0))))

(defun %tt-cff-skip-operand (b)
  (let ((b0 (%tt-buf-peek8 b)))
    (if (= b0 30)
        (progn
          (%tt-buf-skip b 1)
          (loop while (< (tt-buf-cursor b) (tt-buf-size b))
                do (let ((v (%tt-buf-get8 b)))
                     (when (or (= (logand v #xF) #xF) (= (ash v -4) #xF)) (return)))))
        (%tt-cff-int b))))

(defun %tt-dict-get (b key)
  (%tt-buf-seek b 0)
  (loop while (< (tt-buf-cursor b) (tt-buf-size b))
        do (let ((start (tt-buf-cursor b)) end op)
             (loop while (>= (%tt-buf-peek8 b) 28) do (%tt-cff-skip-operand b))
             (setf end (tt-buf-cursor b))
             (setf op (%tt-buf-get8 b))
             (when (= op 12) (setf op (logior (%tt-buf-get8 b) #x100)))
             (when (= op key) (return-from %tt-dict-get (%tt-buf-range b start (- end start))))))
  (%tt-buf-range b 0 0))

(defun %tt-dict-get-ints (b key outcount out)
  "Fill vector OUT with up to OUTCOUNT uint32 operands of KEY"
  (let ((operands (%tt-dict-get b key)))
    (loop for i from 0 below outcount
          while (< (tt-buf-cursor operands) (tt-buf-size operands))
          do (setf (aref out i) (logand (%tt-cff-int operands) #xffffffff)))
    out))

(defun %tt-cff-index-count (b)
  (%tt-buf-seek b 0)
  (%tt-buf-get16 b))

(defun %tt-cff-index-get (b i)
  (let ((b (%tt-buf-copy b)))
    (%tt-buf-seek b 0)
    (let* ((count (%tt-buf-get16 b))
           (offsize (%tt-buf-get8 b)))
      (%tt-buf-skip b (* i offsize))
      (let* ((start (%tt-buf-get b offsize))
             (end (%tt-buf-get b offsize)))
        (%tt-buf-range b (+ 2 (* (+ count 1) offsize) start) (- end start))))))

;;;----------------------------------------------------------------------------------
;;; Font info
;;;----------------------------------------------------------------------------------

(defstruct (tt-fontinfo (:conc-name tt-))
  (data nil)                            ; Pointer to .ttf file
  (fontstart 0 :type fixnum)            ; Offset of start of font
  (num-glyphs 0 :type fixnum)           ; Number of glyphs, needed for range checking
  ;; Table locations as offset from start of .ttf
  (loca 0 :type fixnum) (head 0 :type fixnum) (glyf 0 :type fixnum) (hhea 0 :type fixnum)
  (hmtx 0 :type fixnum) (kern 0 :type fixnum) (gpos 0 :type fixnum) (svg 0 :type fixnum)
  (index-map 0 :type fixnum)            ; A cmap mapping for our chosen character encoding
  (index-to-loc-format 0 :type fixnum)  ; Format needed to map from glyph index to glyph
  (cff (%tt-null-buf))                  ; Cff font data
  (charstrings (%tt-null-buf))          ; The charstring index
  (gsubrs (%tt-null-buf))               ; Global charstring subroutines index
  (subrs (%tt-null-buf))                ; Private charstring subroutines index
  (fontdicts (%tt-null-buf))            ; Array of font dicts
  (fdselect (%tt-null-buf)))            ; Map from glyph to fontdict

(defun %tt-find-table (data fontstart tag)
  (let ((num-tables (tt-ushort data (+ fontstart 4)))
        (tabledir (+ fontstart 12)))
    (dotimes (i num-tables 0)
      (let ((loc (+ tabledir (* 16 i))))
        (when (%tt-tag-p data loc tag)
          (return (tt-ulong data (+ loc 8))))))))

(defun %tt-get-subrs (cff fontdict)
  (let ((private-loc (make-array 2 :initial-element 0))
        (subrsoff (make-array 1 :initial-element 0)))
    (%tt-dict-get-ints fontdict 18 2 private-loc)
    (when (or (= (aref private-loc 1) 0) (= (aref private-loc 0) 0))
      (return-from %tt-get-subrs (%tt-null-buf)))
    (let ((pdict (%tt-buf-range cff (aref private-loc 1) (aref private-loc 0))))
      (%tt-dict-get-ints pdict 19 1 subrsoff)
      (when (= (aref subrsoff 0) 0) (return-from %tt-get-subrs (%tt-null-buf)))
      (let ((cff (%tt-buf-copy cff)))
        (%tt-buf-seek cff (+ (aref private-loc 1) (aref subrsoff 0)))
        (%tt-cff-get-index cff)))))

(defconstant +stbtt-platform-id-unicode+ 0)
(defconstant +stbtt-platform-id-microsoft+ 3)
(defconstant +stbtt-ms-eid-unicode-bmp+ 1)
(defconstant +stbtt-ms-eid-unicode-full+ 10)

(defun stbtt-init-font (data fontstart)
  "Given an offset into the file that defines a font, returns a font info structure, NIL on failure"
  (let ((info (make-tt-fontinfo :data data :fontstart fontstart)))
    (handler-case
        (let ((cmap (%tt-find-table data fontstart "cmap"))) ; required
          (setf (tt-loca info) (%tt-find-table data fontstart "loca") ; required
                (tt-head info) (%tt-find-table data fontstart "head") ; required
                (tt-glyf info) (%tt-find-table data fontstart "glyf") ; required
                (tt-hhea info) (%tt-find-table data fontstart "hhea") ; required
                (tt-hmtx info) (%tt-find-table data fontstart "hmtx") ; required
                (tt-kern info) (%tt-find-table data fontstart "kern") ; not required
                (tt-gpos info) (%tt-find-table data fontstart "GPOS")) ; not required
          (when (or (= cmap 0) (= (tt-head info) 0) (= (tt-hhea info) 0) (= (tt-hmtx info) 0))
            (return-from stbtt-init-font nil))
          (if (/= (tt-glyf info) 0)
              ;; Required for truetype
              (when (= (tt-loca info) 0) (return-from stbtt-init-font nil))
              ;; Initialization for CFF / Type2 fonts (OTF)
              (let ((cstype (make-array 1 :initial-element 2))
                    (charstrings (make-array 1 :initial-element 0))
                    (fdarrayoff (make-array 1 :initial-element 0))
                    (fdselectoff (make-array 1 :initial-element 0))
                    (cff (%tt-find-table data fontstart "CFF ")))
                (when (= cff 0) (return-from stbtt-init-font nil))
                (setf (tt-fontdicts info) (%tt-null-buf)
                      (tt-fdselect info) (%tt-null-buf))
                ;; @TODO this should use size from table (not 512MB)
                (setf (tt-cff info) (%tt-new-buf data cff (* 512 1024 1024)))
                (let ((b (%tt-buf-copy (tt-cff info))) topdict topdictidx)
                  ;; Read the header
                  (%tt-buf-skip b 2)
                  (%tt-buf-seek b (%tt-buf-get8 b)) ; hdrsize
                  ;; @TODO the name INDEX could list multiple fonts,
                  ;; but we just use the first one
                  (%tt-cff-get-index b)  ; name INDEX
                  (setf topdictidx (%tt-cff-get-index b))
                  (setf topdict (%tt-cff-index-get topdictidx 0))
                  (%tt-cff-get-index b)  ; string INDEX
                  (setf (tt-gsubrs info) (%tt-cff-get-index b))
                  (%tt-dict-get-ints topdict 17 1 charstrings)
                  (%tt-dict-get-ints topdict (logior #x100 6) 1 cstype)
                  (%tt-dict-get-ints topdict (logior #x100 36) 1 fdarrayoff)
                  (%tt-dict-get-ints topdict (logior #x100 37) 1 fdselectoff)
                  (setf (tt-subrs info) (%tt-get-subrs b topdict))
                  ;; We only support Type 2 charstrings
                  (when (/= (aref cstype 0) 2) (return-from stbtt-init-font nil))
                  (when (= (aref charstrings 0) 0) (return-from stbtt-init-font nil))
                  (when (/= (aref fdarrayoff 0) 0)
                    ;; Looks like a CID font
                    (when (= (aref fdselectoff 0) 0) (return-from stbtt-init-font nil))
                    (%tt-buf-seek b (aref fdarrayoff 0))
                    (setf (tt-fontdicts info) (%tt-cff-get-index b))
                    (setf (tt-fdselect info) (%tt-buf-range b (aref fdselectoff 0) (- (tt-buf-size b) (aref fdselectoff 0)))))
                  (%tt-buf-seek b (aref charstrings 0))
                  (setf (tt-charstrings info) (%tt-cff-get-index b)))))
          (let ((tab (%tt-find-table data fontstart "maxp")))
            (setf (tt-num-glyphs info) (if (/= tab 0) (tt-ushort data (+ tab 4)) #xffff)))
          (setf (tt-svg info) -1)
          ;; Find a cmap encoding table we understand *now* to avoid searching
          ;; later. (todo: could make this installable)
          ;; The same regardless of glyph.
          (let ((num-tables (tt-ushort data (+ cmap 2))))
            (setf (tt-index-map info) 0)
            (dotimes (i num-tables)
              (let ((encoding-record (+ cmap 4 (* 8 i))))
                ;; Find an encoding we understand:
                (case (tt-ushort data encoding-record)
                  (#.+stbtt-platform-id-microsoft+
                   (case (tt-ushort data (+ encoding-record 2))
                     ((#.+stbtt-ms-eid-unicode-bmp+ #.+stbtt-ms-eid-unicode-full+)
                      ;; MS/Unicode
                      (setf (tt-index-map info) (+ cmap (tt-ulong data (+ encoding-record 4)))))))
                  (#.+stbtt-platform-id-unicode+
                   ;; Mac/iOS has these
                   ;; all the encodingIDs are unicode, so we don't bother to check it
                   (setf (tt-index-map info) (+ cmap (tt-ulong data (+ encoding-record 4)))))))))
          (when (= (tt-index-map info) 0) (return-from stbtt-init-font nil))
          (setf (tt-index-to-loc-format info) (tt-ushort data (+ (tt-head info) 50)))
          info)
      ;; Out of range reads on malformed data (undefined behaviour in C)
      (error () nil))))

(defun stbtt-find-glyph-index (info unicode-codepoint)
  "If you're going to perform multiple operations on the same character
and you want a speed-up, call this function with the character you're
going to process, then use glyph-based functions instead of the
codepoint-based functions. Returns 0 if the character codepoint is not defined in the font."
  (let* ((data (tt-data info))
         (index-map (tt-index-map info))
         (format (tt-ushort data index-map)))
    (cond
      ((= format 0)                     ; apple byte encoding
       (let ((bytes (tt-ushort data (+ index-map 2))))
         (if (< unicode-codepoint (- bytes 6))
             (tt-byte data (+ index-map 6 unicode-codepoint))
             0)))
      ((= format 6)
       (let ((first (tt-ushort data (+ index-map 6)))
             (count (tt-ushort data (+ index-map 8))))
         (if (and (>= unicode-codepoint first) (< unicode-codepoint (+ first count)))
             (tt-ushort data (+ index-map 10 (* (- unicode-codepoint first) 2)))
             0)))
      ((= format 2) 0)                  ; @TODO: high-byte mapping for japanese/chinese/korean
      ((= format 4)                     ; standard mapping for windows fonts: binary search collection of ranges
       (let* ((segcount (ash (tt-ushort data (+ index-map 6)) -1))
              (search-range (ash (tt-ushort data (+ index-map 8)) -1))
              (entry-selector (tt-ushort data (+ index-map 10)))
              (range-shift (ash (tt-ushort data (+ index-map 12)) -1))
              ;; do a binary search of the segments
              (end-count (+ index-map 14))
              (search end-count))
         (when (> unicode-codepoint #xffff) (return-from stbtt-find-glyph-index 0))
         ;; they lie from endCount .. endCount + segCount
         ;; but searchRange is the nearest power of two, so...
         (when (>= unicode-codepoint (tt-ushort data (+ search (* range-shift 2))))
           (incf search (* range-shift 2)))
         ;; now decrement to bias correctly to find smallest
         (decf search 2)
         (loop while (/= entry-selector 0)
               do (setf search-range (ash search-range -1))
                  (let ((end (tt-ushort data (+ search (* search-range 2)))))
                    (when (> unicode-codepoint end) (incf search (* search-range 2))))
                  (decf entry-selector))
         (incf search 2)
         (let* ((item (logand (ash (- search end-count) -1) #xffff))
                (start (tt-ushort data (+ index-map 14 (* segcount 2) 2 (* 2 item))))
                (last (tt-ushort data (+ end-count (* 2 item)))))
           (if (or (< unicode-codepoint start) (> unicode-codepoint last))
               0
               (let ((offset (tt-ushort data (+ index-map 14 (* segcount 6) 2 (* 2 item)))))
                 (if (= offset 0)
                     (logand (+ unicode-codepoint (tt-short data (+ index-map 14 (* segcount 4) 2 (* 2 item)))) #xffff)
                     (tt-ushort data (+ offset (* (- unicode-codepoint start) 2) index-map 14 (* segcount 6) 2 (* 2 item)))))))))
      ((or (= format 12) (= format 13))
       (let ((ngroups (tt-ulong data (+ index-map 12)))
             (low 0) (high 0))
         (setf high ngroups)
         ;; Binary search the right group.
         (loop while (< low high)
               do (let* ((mid (+ low (ash (- high low) -1))) ; rounds down, so low <= mid < high
                         (start-char (tt-ulong data (+ index-map 16 (* mid 12))))
                         (end-char (tt-ulong data (+ index-map 16 (* mid 12) 4))))
                    (cond ((< unicode-codepoint start-char) (setf high mid))
                          ((> unicode-codepoint end-char) (setf low (+ mid 1)))
                          (t (let ((start-glyph (tt-ulong data (+ index-map 16 (* mid 12) 8))))
                               (return-from stbtt-find-glyph-index
                                 (if (= format 12)
                                     (+ start-glyph (- unicode-codepoint start-char))
                                     start-glyph)))))))
         0))                            ; not found
      ;; @TODO
      (t 0))))

;;;----------------------------------------------------------------------------------
;;; Glyph shapes
;;;----------------------------------------------------------------------------------

(defconstant +stbtt-vmove+ 1)
(defconstant +stbtt-vline+ 2)
(defconstant +stbtt-vcurve+ 3)
(defconstant +stbtt-vcubic+ 4)

(defstruct (tt-vertex (:constructor %make-tt-vertex (type x y cx cy &optional (cx1 0) (cy1 0))))
  (type 0 :type fixnum)
  (x 0 :type fixnum) (y 0 :type fixnum)
  (cx 0 :type fixnum) (cy 0 :type fixnum)
  (cx1 0 :type fixnum) (cy1 0 :type fixnum))

(defun %tt-setvertex (type x y cx cy)
  (%make-tt-vertex type (%s16 x) (%s16 y) (%s16 cx) (%s16 cy)))

(defun %tt-get-glyf-offset (info glyph-index)
  (let ((data (tt-data info)) g1 g2)
    (when (>= glyph-index (tt-num-glyphs info)) (return-from %tt-get-glyf-offset -1)) ; glyph index out of range
    (when (>= (tt-index-to-loc-format info) 2) (return-from %tt-get-glyf-offset -1)) ; unknown index->glyph map format
    (if (= (tt-index-to-loc-format info) 0)
        (setf g1 (+ (tt-glyf info) (* (tt-ushort data (+ (tt-loca info) (* glyph-index 2))) 2))
              g2 (+ (tt-glyf info) (* (tt-ushort data (+ (tt-loca info) (* glyph-index 2) 2)) 2)))
        (setf g1 (+ (tt-glyf info) (tt-ulong data (+ (tt-loca info) (* glyph-index 4))))
              g2 (+ (tt-glyf info) (tt-ulong data (+ (tt-loca info) (* glyph-index 4) 4)))))
    (if (= g1 g2) -1 g1)))              ; if length is 0, return -1

(defun stbtt-get-glyph-box (info glyph-index)
  "Gets the bounding box of the visible part of the glyph, in unscaled coordinates
Returns (values found-p x0 y0 x1 y1)"
  (if (/= (tt-buf-size (tt-cff info)) 0)
      (multiple-value-bind (count x0 y0 x1 y1) (%tt-get-glyph-info-t2 info glyph-index)
        (declare (ignore count))
        (values t x0 y0 x1 y1))
      (let ((g (%tt-get-glyf-offset info glyph-index))
            (data (tt-data info)))
        (if (< g 0)
            (values nil 0 0 0 0)
            (values t (tt-short data (+ g 2)) (tt-short data (+ g 4))
                    (tt-short data (+ g 6)) (tt-short data (+ g 8)))))))

(defun %tt-close-shape (vertices was-off start-off sx sy scx scy cx cy)
  (if start-off
      (progn
        (when was-off
          (vector-push-extend (%tt-setvertex +stbtt-vcurve+ (ash (+ cx scx) -1) (ash (+ cy scy) -1) cx cy) vertices))
        (vector-push-extend (%tt-setvertex +stbtt-vcurve+ sx sy scx scy) vertices))
      (if was-off
          (vector-push-extend (%tt-setvertex +stbtt-vcurve+ sx sy cx cy) vertices)
          (vector-push-extend (%tt-setvertex +stbtt-vline+ sx sy 0 0) vertices))))

(defun %tt-get-glyph-shape-tt (info glyph-index)
  (let* ((data (tt-data info))
         (vertices (make-array 0 :adjustable t :fill-pointer 0))
         (g (%tt-get-glyf-offset info glyph-index)))
    (when (< g 0) (return-from %tt-get-glyph-shape-tt vertices))
    (let ((number-of-contours (tt-short data g)))
      (cond
        ((> number-of-contours 0)
         (let* ((end-pts-of-contours (+ g 10))
                (ins (tt-ushort data (+ g 10 (* number-of-contours 2))))
                (points (+ g 10 (* number-of-contours 2) 2 ins))
                (n (+ 1 (tt-ushort data (+ end-pts-of-contours (* number-of-contours 2) -2))))
                (pflags (make-array (1+ n) :initial-element 0))
                (px (make-array (1+ n) :initial-element 0))
                (py (make-array (1+ n) :initial-element 0))
                (flags 0) (flagcount 0) (x 0) (y 0)
                (next-move 0) (was-off nil) (start-off nil) (j 0)
                (sx 0) (sy 0) (cx 0) (cy 0) (scx 0) (scy 0))
           ;; in first pass, we load uninterpreted data into the allocated array
           ;; above, shifted to the end of the array so we won't overwrite it when
           ;; we create our final data starting from the front
           ;; first load flags
           (dotimes (i n)
             (if (= flagcount 0)
                 (progn
                   (setf flags (aref data points)) (incf points)
                   (when (logtest flags 8)
                     (setf flagcount (aref data points)) (incf points)))
                 (decf flagcount))
             (setf (aref pflags i) flags))
           ;; now load x coordinates
           (dotimes (i n)
             (setf flags (aref pflags i))
             (if (logtest flags 2)
                 (let ((dx (aref data points)))
                   (incf points)
                   (setf x (if (logtest flags 16) (+ x dx) (- x dx))))
                 (unless (logtest flags 16)
                   (setf x (+ x (%s16 (+ (* (aref data points) 256) (aref data (+ points 1))))))
                   (incf points 2)))
             (setf (aref px i) (%s16 x)))
           ;; now load y coordinates
           (dotimes (i n)
             (setf flags (aref pflags i))
             (if (logtest flags 4)
                 (let ((dy (aref data points)))
                   (incf points)
                   (setf y (if (logtest flags 32) (+ y dy) (- y dy))))
                 (unless (logtest flags 32)
                   (setf y (+ y (%s16 (+ (* (aref data points) 256) (aref data (+ points 1))))))
                   (incf points 2)))
             (setf (aref py i) (%s16 y)))
           ;; now convert them to our format
           (let ((i 0))
             (loop while (< i n)
                   do (setf flags (aref pflags i)
                            x (aref px i)
                            y (aref py i))
                      (if (= next-move i)
                          (progn
                            (when (/= i 0)
                              (%tt-close-shape vertices was-off start-off sx sy scx scy cx cy))
                            ;; now start the new one
                            (setf start-off (not (logtest flags 1)))
                            (if start-off
                                (progn
                                  ;; if we start off with an off-curve point, then when we need to find a point on the curve
                                  ;; where we can start, and we need to save some state for when we wraparound.
                                  (setf scx x scy y)
                                  (if (not (logtest (aref pflags (1+ i)) 1))
                                      ;; next point is also a curve point, so interpolate an on-point curve
                                      (setf sx (ash (+ x (aref px (1+ i))) -1)
                                            sy (ash (+ y (aref py (1+ i))) -1))
                                      ;; otherwise just use the next point as our start point
                                      (progn
                                        (setf sx (aref px (1+ i))
                                              sy (aref py (1+ i)))
                                        (incf i)))) ; we're using point i+1 as the starting point, so skip it
                                (setf sx x sy y))
                            (vector-push-extend (%tt-setvertex +stbtt-vmove+ sx sy 0 0) vertices)
                            (setf was-off nil)
                            (setf next-move (+ 1 (tt-ushort data (+ end-pts-of-contours (* j 2)))))
                            (incf j))
                          (if (not (logtest flags 1)) ; if it's a curve
                              (progn
                                (when was-off ; two off-curve control points in a row means interpolate an on-curve midpoint
                                  (vector-push-extend (%tt-setvertex +stbtt-vcurve+ (ash (+ cx x) -1) (ash (+ cy y) -1) cx cy) vertices))
                                (setf cx x cy y was-off t))
                              (progn
                                (if was-off
                                    (vector-push-extend (%tt-setvertex +stbtt-vcurve+ x y cx cy) vertices)
                                    (vector-push-extend (%tt-setvertex +stbtt-vline+ x y 0 0) vertices))
                                (setf was-off nil))))
                      (incf i)))
           (%tt-close-shape vertices was-off start-off sx sy scx scy cx cy)))
        ((< number-of-contours 0)
         ;; Compound shapes
         (let ((more t)
               (comp (+ g 10)))
           (loop while more
                 do (let ((mtx (make-array 6 :element-type 'single-float :initial-contents '(1.0 0.0 0.0 1.0 0.0 0.0)))
                          flags gidx)
                      (setf flags (logand (tt-short data comp) #xffff)) (incf comp 2)
                      (setf gidx (logand (tt-short data comp) #xffff)) (incf comp 2)
                      (if (logtest flags 2)   ; XY values
                          (if (logtest flags 1) ; shorts
                              (progn
                                (setf (aref mtx 4) (float (tt-short data comp) 1.0)) (incf comp 2)
                                (setf (aref mtx 5) (float (tt-short data comp) 1.0)) (incf comp 2))
                              (progn
                                (setf (aref mtx 4) (float (tt-char data comp) 1.0)) (incf comp 1)
                                (setf (aref mtx 5) (float (tt-char data comp) 1.0)) (incf comp 1)))
                          ;; @TODO handle matching point
                          nil)
                      (cond
                        ((logtest flags (ash 1 3)) ; WE_HAVE_A_SCALE
                         (setf (aref mtx 0) (/ (float (tt-short data comp) 1.0) 16384.0)
                               (aref mtx 3) (aref mtx 0))
                         (incf comp 2)
                         (setf (aref mtx 1) 0.0 (aref mtx 2) 0.0))
                        ((logtest flags (ash 1 6)) ; WE_HAVE_AN_X_AND_YSCALE
                         (setf (aref mtx 0) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2)
                         (setf (aref mtx 1) 0.0 (aref mtx 2) 0.0)
                         (setf (aref mtx 3) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2))
                        ((logtest flags (ash 1 7)) ; WE_HAVE_A_TWO_BY_TWO
                         (setf (aref mtx 0) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2)
                         (setf (aref mtx 1) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2)
                         (setf (aref mtx 2) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2)
                         (setf (aref mtx 3) (/ (float (tt-short data comp) 1.0) 16384.0)) (incf comp 2)))
                      ;; Find transformation scales
                      (let ((m (float (sqrt (float (+ (* (aref mtx 0) (aref mtx 0)) (* (aref mtx 1) (aref mtx 1))) 1d0)) 1.0))
                            (n (float (sqrt (float (+ (* (aref mtx 2) (aref mtx 2)) (* (aref mtx 3) (aref mtx 3))) 1d0)) 1.0))
                            (comp-verts (stbtt-get-glyph-shape info gidx)))
                        ;; Get indexed glyph
                        (when (> (length comp-verts) 0)
                          ;; Transform vertices
                          (loop for v across comp-verts
                                do (let ((x (tt-vertex-x v)) (y (tt-vertex-y v)))
                                     (setf (tt-vertex-x v) (%s16 (truncate (+ (+ (* (aref mtx 0) x) (* (aref mtx 2) y)) (* (aref mtx 4) m))))
                                           (tt-vertex-y v) (%s16 (truncate (+ (+ (* (aref mtx 1) x) (* (aref mtx 3) y)) (* (aref mtx 5) n))))))
                                   (let ((x (tt-vertex-cx v)) (y (tt-vertex-cy v)))
                                     (setf (tt-vertex-cx v) (%s16 (truncate (+ (+ (* (aref mtx 0) x) (* (aref mtx 2) y)) (* (aref mtx 4) m))))
                                           (tt-vertex-cy v) (%s16 (truncate (+ (+ (* (aref mtx 1) x) (* (aref mtx 3) y)) (* (aref mtx 5) n))))))
                                   (vector-push-extend v vertices))))
                      ;; More components ?
                      (setf more (logtest flags (ash 1 5)))))))
        (t ;; numberOfCounters == 0, do nothing
         nil)))
    vertices))

;;; CFF charstring interpreter context (stbtt__csctx)
(defstruct (tt-csctx (:constructor %make-tt-csctx (bounds)))
  (bounds nil)
  (started nil)
  (first-x 0.0 :type single-float) (first-y 0.0 :type single-float)
  (x 0.0 :type single-float) (y 0.0 :type single-float)
  (min-x 0) (max-x 0) (min-y 0) (max-y 0)
  (vertices (make-array 0 :adjustable t :fill-pointer 0))
  (num-vertices 0 :type fixnum))

(defun %tt-track-vertex (c x y)
  (when (or (> x (tt-csctx-max-x c)) (not (tt-csctx-started c))) (setf (tt-csctx-max-x c) x))
  (when (or (> y (tt-csctx-max-y c)) (not (tt-csctx-started c))) (setf (tt-csctx-max-y c) y))
  (when (or (< x (tt-csctx-min-x c)) (not (tt-csctx-started c))) (setf (tt-csctx-min-x c) x))
  (when (or (< y (tt-csctx-min-y c)) (not (tt-csctx-started c))) (setf (tt-csctx-min-y c) y))
  (setf (tt-csctx-started c) t))

(defun %tt-csctx-v (c type x y cx cy cx1 cy1)
  (if (tt-csctx-bounds c)
      (progn
        (%tt-track-vertex c x y)
        (when (= type +stbtt-vcubic+)
          (%tt-track-vertex c cx cy)
          (%tt-track-vertex c cx1 cy1)))
      (let ((v (%tt-setvertex type x y cx cy)))
        (setf (tt-vertex-cx1 v) (%s16 cx1)
              (tt-vertex-cy1 v) (%s16 cy1))
        (vector-push-extend v (tt-csctx-vertices c))))
  (incf (tt-csctx-num-vertices c)))

(defun %tt-csctx-close-shape (ctx)
  (when (or (/= (tt-csctx-first-x ctx) (tt-csctx-x ctx)) (/= (tt-csctx-first-y ctx) (tt-csctx-y ctx)))
    (%tt-csctx-v ctx +stbtt-vline+ (truncate (tt-csctx-first-x ctx)) (truncate (tt-csctx-first-y ctx)) 0 0 0 0)))

(defun %tt-csctx-rmove-to (ctx dx dy)
  (%tt-csctx-close-shape ctx)
  (setf (tt-csctx-x ctx) (+ (tt-csctx-x ctx) dx)
        (tt-csctx-first-x ctx) (tt-csctx-x ctx)
        (tt-csctx-y ctx) (+ (tt-csctx-y ctx) dy)
        (tt-csctx-first-y ctx) (tt-csctx-y ctx))
  (%tt-csctx-v ctx +stbtt-vmove+ (truncate (tt-csctx-x ctx)) (truncate (tt-csctx-y ctx)) 0 0 0 0))

(defun %tt-csctx-rline-to (ctx dx dy)
  (incf (tt-csctx-x ctx) dx)
  (incf (tt-csctx-y ctx) dy)
  (%tt-csctx-v ctx +stbtt-vline+ (truncate (tt-csctx-x ctx)) (truncate (tt-csctx-y ctx)) 0 0 0 0))

(defun %tt-csctx-rccurve-to (ctx dx1 dy1 dx2 dy2 dx3 dy3)
  (let* ((cx1 (+ (tt-csctx-x ctx) dx1))
         (cy1 (+ (tt-csctx-y ctx) dy1))
         (cx2 (+ cx1 dx2))
         (cy2 (+ cy1 dy2)))
    (setf (tt-csctx-x ctx) (+ cx2 dx3)
          (tt-csctx-y ctx) (+ cy2 dy3))
    (%tt-csctx-v ctx +stbtt-vcubic+ (truncate (tt-csctx-x ctx)) (truncate (tt-csctx-y ctx))
                 (truncate cx1) (truncate cy1) (truncate cx2) (truncate cy2))))

(defun %tt-get-subr (idx n)
  (let* ((idx (%tt-buf-copy idx))
         (count (%tt-cff-index-count idx))
         (bias 107))
    (cond ((>= count 33900) (setf bias 32768))
          ((>= count 1240) (setf bias 1131)))
    (incf n bias)
    (if (or (< n 0) (>= n count))
        (%tt-null-buf)
        (%tt-cff-index-get idx n))))

(defun %tt-cid-get-glyph-subrs (info glyph-index)
  (let ((fdselect (%tt-buf-copy (tt-fdselect info)))
        (fdselector -1))
    (%tt-buf-seek fdselect 0)
    (let ((fmt (%tt-buf-get8 fdselect)))
      (cond
        ((= fmt 0)
         ;; untested
         (%tt-buf-skip fdselect glyph-index)
         (setf fdselector (%tt-buf-get8 fdselect)))
        ((= fmt 3)
         (let ((nranges (%tt-buf-get16 fdselect))
               (start (%tt-buf-get16 fdselect)))
           (dotimes (i nranges)
             (let ((v (%tt-buf-get8 fdselect))
                   (end (%tt-buf-get16 fdselect)))
               (when (and (>= glyph-index start) (< glyph-index end))
                 (setf fdselector v)
                 (return))
               (setf start end)))))))
    ;; NOTE: C bug, missing return: if (fdselector == -1) stbtt__new_buf(NULL, 0);
    (%tt-get-subrs (tt-cff info) (%tt-cff-index-get (tt-fontdicts info) fdselector))))

(defun %tt-run-charstring (info glyph-index c)
  (let ((in-header t) (maskbits 0) (subr-stack-height 0) (sp 0)
        (has-subrs nil)
        (s (make-array 48 :element-type 'single-float :initial-element 0.0))
        (subr-stack (make-array 10))
        (subrs (tt-subrs info))
        (b (%tt-cff-index-get (tt-charstrings info) glyph-index)))
    (macrolet ((err () `(return-from %tt-run-charstring nil))
               (s (i) `(aref s ,i)))
      (loop while (< (tt-buf-cursor b) (tt-buf-size b))
            do (let ((i 0) (clear-stack t) (b0 (%tt-buf-get8 b)))
                 (block op
                   (case b0
                     ;; @TODO implement hinting
                     ((#x13 #x14)       ; hintmask, cntrmask
                      (when in-header
                        (incf maskbits (floor sp 2))) ; implicit "vstem"
                      (setf in-header nil)
                      (%tt-buf-skip b (floor (+ maskbits 7) 8)))
                     ((#x01 #x03 #x12 #x17) ; hstem, vstem, hstemhm, vstemhm
                      (incf maskbits (floor sp 2)))
                     (#x15              ; rmoveto
                      (setf in-header nil)
                      (when (< sp 2) (err))
                      (%tt-csctx-rmove-to c (s (- sp 2)) (s (- sp 1))))
                     (#x04              ; vmoveto
                      (setf in-header nil)
                      (when (< sp 1) (err))
                      (%tt-csctx-rmove-to c 0.0 (s (- sp 1))))
                     (#x16              ; hmoveto
                      (setf in-header nil)
                      (when (< sp 1) (err))
                      (%tt-csctx-rmove-to c (s (- sp 1)) 0.0))
                     (#x05              ; rlineto
                      (when (< sp 2) (err))
                      (loop while (< (+ i 1) sp)
                            do (%tt-csctx-rline-to c (s i) (s (+ i 1)))
                               (incf i 2)))
                     ;; hlineto/vlineto and vhcurveto/hvcurveto alternate horizontal and vertical
                     ;; starting from a different place.
                     ((#x07 #x06)       ; vlineto, hlineto
                      (when (< sp 1) (err))
                      (let ((vertical (= b0 #x07)))
                        (loop
                          (when (>= i sp) (return))
                          (if vertical
                              (%tt-csctx-rline-to c 0.0 (s i))
                              (%tt-csctx-rline-to c (s i) 0.0))
                          (incf i)
                          (setf vertical (not vertical)))))
                     ((#x1F #x1E)       ; hvcurveto, vhcurveto
                      (when (< sp 4) (err))
                      (let ((hv (= b0 #x1F)))
                        (loop
                          (when (>= (+ i 3) sp) (return))
                          (if hv
                              (%tt-csctx-rccurve-to c (s i) 0.0 (s (+ i 1)) (s (+ i 2))
                                                    (if (= (- sp i) 5) (s (+ i 4)) 0.0) (s (+ i 3)))
                              (%tt-csctx-rccurve-to c 0.0 (s i) (s (+ i 1)) (s (+ i 2)) (s (+ i 3))
                                                    (if (= (- sp i) 5) (s (+ i 4)) 0.0)))
                          (incf i 4)
                          (setf hv (not hv)))))
                     (#x08              ; rrcurveto
                      (when (< sp 6) (err))
                      (loop while (< (+ i 5) sp)
                            do (%tt-csctx-rccurve-to c (s i) (s (+ i 1)) (s (+ i 2)) (s (+ i 3)) (s (+ i 4)) (s (+ i 5)))
                               (incf i 6)))
                     (#x18              ; rcurveline
                      (when (< sp 8) (err))
                      (loop while (< (+ i 5) (- sp 2))
                            do (%tt-csctx-rccurve-to c (s i) (s (+ i 1)) (s (+ i 2)) (s (+ i 3)) (s (+ i 4)) (s (+ i 5)))
                               (incf i 6))
                      (when (>= (+ i 1) sp) (err))
                      (%tt-csctx-rline-to c (s i) (s (+ i 1))))
                     (#x19              ; rlinecurve
                      (when (< sp 8) (err))
                      (loop while (< (+ i 1) (- sp 6))
                            do (%tt-csctx-rline-to c (s i) (s (+ i 1)))
                               (incf i 2))
                      (when (>= (+ i 5) sp) (err))
                      (%tt-csctx-rccurve-to c (s i) (s (+ i 1)) (s (+ i 2)) (s (+ i 3)) (s (+ i 4)) (s (+ i 5))))
                     ((#x1A #x1B)       ; vvcurveto, hhcurveto
                      (when (< sp 4) (err))
                      (let ((f 0.0))
                        (when (logtest sp 1) (setf f (s i)) (incf i))
                        (loop while (< (+ i 3) sp)
                              do (if (= b0 #x1B)
                                     (%tt-csctx-rccurve-to c (s i) f (s (+ i 1)) (s (+ i 2)) (s (+ i 3)) 0.0)
                                     (%tt-csctx-rccurve-to c f (s i) (s (+ i 1)) (s (+ i 2)) 0.0 (s (+ i 3))))
                                 (setf f 0.0)
                                 (incf i 4))))
                     ((#x0A #x1D)       ; callsubr, callgsubr
                      (when (and (= b0 #x0A) (not has-subrs))
                        (when (/= (tt-buf-size (tt-fdselect info)) 0)
                          (setf subrs (%tt-cid-get-glyph-subrs info glyph-index)))
                        (setf has-subrs t))
                      ;; FALLTHROUGH
                      (when (< sp 1) (err))
                      (let ((v (truncate (s (decf sp)))))
                        (when (>= subr-stack-height 10) (err))
                        (setf (aref subr-stack subr-stack-height) b)
                        (incf subr-stack-height)
                        (setf b (%tt-get-subr (if (= b0 #x0A) subrs (tt-gsubrs info)) v))
                        (when (= (tt-buf-size b) 0) (err))
                        (setf (tt-buf-cursor b) 0)
                        (setf clear-stack nil)))
                     (#x0B              ; return
                      (when (<= subr-stack-height 0) (err))
                      (setf b (aref subr-stack (decf subr-stack-height)))
                      (setf clear-stack nil))
                     (#x0E              ; endchar
                      (%tt-csctx-close-shape c)
                      (return-from %tt-run-charstring t))
                     (#x0C              ; two-byte escape
                      (let ((b1 (%tt-buf-get8 b)))
                        (case b1
                          ;; @TODO These "flex" implementations ignore the flex-depth and resolution,
                          ;; and always draw beziers.
                          (#x22         ; hflex
                           (when (< sp 7) (err))
                           (let ((dx1 (s 0)) (dx2 (s 1)) (dy2 (s 2)) (dx3 (s 3)) (dx4 (s 4)) (dx5 (s 5)) (dx6 (s 6)))
                             (%tt-csctx-rccurve-to c dx1 0.0 dx2 dy2 dx3 0.0)
                             (%tt-csctx-rccurve-to c dx4 0.0 dx5 (- dy2) dx6 0.0)))
                          (#x23         ; flex
                           (when (< sp 13) (err))
                           (%tt-csctx-rccurve-to c (s 0) (s 1) (s 2) (s 3) (s 4) (s 5))
                           (%tt-csctx-rccurve-to c (s 6) (s 7) (s 8) (s 9) (s 10) (s 11)))
                          (#x24         ; hflex1
                           (when (< sp 9) (err))
                           (let ((dx1 (s 0)) (dy1 (s 1)) (dx2 (s 2)) (dy2 (s 3)) (dx3 (s 4))
                                 (dx4 (s 5)) (dx5 (s 6)) (dy5 (s 7)) (dx6 (s 8)))
                             (%tt-csctx-rccurve-to c dx1 dy1 dx2 dy2 dx3 0.0)
                             (%tt-csctx-rccurve-to c dx4 0.0 dx5 dy5 dx6 (- (+ dy1 dy2 dy5)))))
                          (#x25         ; flex1
                           (when (< sp 11) (err))
                           (let* ((dx1 (s 0)) (dy1 (s 1)) (dx2 (s 2)) (dy2 (s 3)) (dx3 (s 4)) (dy3 (s 5))
                                  (dx4 (s 6)) (dy4 (s 7)) (dx5 (s 8)) (dy5 (s 9))
                                  (dx6 (s 10)) (dy6 (s 10))
                                  (dx (+ dx1 dx2 dx3 dx4 dx5))
                                  (dy (+ dy1 dy2 dy3 dy4 dy5)))
                             (if (> (abs dx) (abs dy))
                                 (setf dy6 (- dy))
                                 (setf dx6 (- dx)))
                             (%tt-csctx-rccurve-to c dx1 dy1 dx2 dy2 dx3 dy3)
                             (%tt-csctx-rccurve-to c dx4 dy4 dx5 dy5 dx6 dy6)))
                          (t (err)))))
                     (t
                      (when (and (/= b0 255) (/= b0 28) (< b0 32)) (err)) ; reserved operator
                      ;; push immediate
                      (let ((f (if (= b0 255)
                                   (/ (float (let ((v (%tt-buf-get32 b))) (if (>= v #x80000000) (- v #x100000000) v)) 1.0)
                                      (float #x10000 1.0))
                                   (progn
                                     (%tt-buf-skip b -1)
                                     (float (%s16 (%tt-cff-int b)) 1.0)))))
                        (when (>= sp 48) (err))
                        (setf (s sp) f)
                        (incf sp)
                        (setf clear-stack nil)))))
                 (when clear-stack (setf sp 0))))
      ;; no endchar
      nil)))

(defun %tt-get-glyph-shape-t2 (info glyph-index)
  ;; runs the charstring twice, once to count and once to output (to avoid realloc)
  (let ((output-ctx (%make-tt-csctx nil)))
    (if (%tt-run-charstring info glyph-index output-ctx)
        (tt-csctx-vertices output-ctx)
        (make-array 0 :adjustable t :fill-pointer 0))))

(defun %tt-get-glyph-info-t2 (info glyph-index)
  "Returns (values num-vertices x0 y0 x1 y1)"
  (let* ((c (%make-tt-csctx t))
         (r (%tt-run-charstring info glyph-index c)))
    (values (if r (tt-csctx-num-vertices c) 0)
            (if r (tt-csctx-min-x c) 0) (if r (tt-csctx-min-y c) 0)
            (if r (tt-csctx-max-x c) 0) (if r (tt-csctx-max-y c) 0))))

(defun stbtt-get-glyph-shape (info glyph-index)
  "Returns a vector of tt-vertex describing the glyph outline"
  (if (= (tt-buf-size (tt-cff info)) 0)
      (%tt-get-glyph-shape-tt info glyph-index)
      (%tt-get-glyph-shape-t2 info glyph-index)))

;;;----------------------------------------------------------------------------------
;;; Metrics
;;;----------------------------------------------------------------------------------

(defun stbtt-get-glyph-h-metrics (info glyph-index)
  "Returns (values advance-width left-side-bearing) in unscaled coordinates"
  (let* ((data (tt-data info))
         (num-of-long-hor-metrics (tt-ushort data (+ (tt-hhea info) 34))))
    (if (< glyph-index num-of-long-hor-metrics)
        (values (tt-short data (+ (tt-hmtx info) (* 4 glyph-index)))
                (tt-short data (+ (tt-hmtx info) (* 4 glyph-index) 2)))
        (values (tt-short data (+ (tt-hmtx info) (* 4 (- num-of-long-hor-metrics 1))))
                (tt-short data (+ (tt-hmtx info) (* 4 num-of-long-hor-metrics) (* 2 (- glyph-index num-of-long-hor-metrics))))))))

(defun stbtt-get-codepoint-h-metrics (info codepoint)
  (stbtt-get-glyph-h-metrics info (stbtt-find-glyph-index info codepoint)))

(defun stbtt-get-font-v-metrics (info)
  "Returns (values ascent descent line-gap) in unscaled coordinates"
  (let ((data (tt-data info)))
    (values (tt-short data (+ (tt-hhea info) 4))
            (tt-short data (+ (tt-hhea info) 6))
            (tt-short data (+ (tt-hhea info) 8)))))

(defun stbtt-scale-for-pixel-height (info height)
  "Computes a scale factor to produce a font whose height is HEIGHT pixels tall"
  (let* ((data (tt-data info))
         (fheight (- (tt-short data (+ (tt-hhea info) 4)) (tt-short data (+ (tt-hhea info) 6)))))
    (/ (float height 1.0) (float fheight 1.0))))

;;;----------------------------------------------------------------------------------
;;; Bitmap rendering
;;;----------------------------------------------------------------------------------

(defun stbtt-get-glyph-bitmap-box-subpixel (font glyph scale-x scale-y shift-x shift-y)
  "Returns (values ix0 iy0 ix1 iy1)"
  (multiple-value-bind (found x0 y0 x1 y1) (stbtt-get-glyph-box font glyph)
    (if (not found)
        ;; e.g. space character
        (values 0 0 0 0)
        ;; move to integral bboxes (treating pixels as little squares, what pixels get touched)?
        (values (floor (+ (* (float x0 1.0) scale-x) shift-x))
                (floor (+ (* (float (- y1) 1.0) scale-y) shift-y))
                (ceiling (+ (* (float x1 1.0) scale-x) shift-x))
                (ceiling (+ (* (float (- y0) 1.0) scale-y) shift-y))))))

(defun stbtt-get-glyph-bitmap-box (font glyph scale-x scale-y)
  (stbtt-get-glyph-bitmap-box-subpixel font glyph scale-x scale-y 0.0 0.0))

;;; Rasterizer (STBTT_RASTERIZER_VERSION 2)

(defstruct (tt-edge (:constructor %make-tt-edge ()))
  (x0 0.0 :type single-float) (y0 0.0 :type single-float)
  (x1 0.0 :type single-float) (y1 0.0 :type single-float)
  (invert nil))

(defstruct (tt-active-edge (:constructor %make-tt-active-edge ()))
  (next nil)
  (fx 0.0 :type single-float) (fdx 0.0 :type single-float) (fdy 0.0 :type single-float)
  (direction 0.0 :type single-float)
  (sy 0.0 :type single-float) (ey 0.0 :type single-float))

(defun %tt-new-active (e off-x start-point)
  (let ((z (%make-tt-active-edge))
        (dxdy (/ (- (tt-edge-x1 e) (tt-edge-x0 e)) (- (tt-edge-y1 e) (tt-edge-y0 e)))))
    (setf (tt-active-edge-fdx z) dxdy
          (tt-active-edge-fdy z) (if (/= dxdy 0.0) (/ 1.0 dxdy) 0.0)
          (tt-active-edge-fx z) (+ (tt-edge-x0 e) (* dxdy (- start-point (tt-edge-y0 e)))))
    (setf (tt-active-edge-fx z) (- (tt-active-edge-fx z) (float off-x 1.0)))
    (setf (tt-active-edge-direction z) (if (tt-edge-invert e) 1.0 -1.0)
          (tt-active-edge-sy z) (tt-edge-y0 e)
          (tt-active-edge-ey z) (tt-edge-y1 e)
          (tt-active-edge-next z) nil)
    z))

(declaim (inline %tt-handle-clipped-edge))
(defun %tt-handle-clipped-edge (scanline base x e x0 y0 x1 y1)
  "The edge passed in here does not cross the vertical line at x or the vertical line at x+1
(i.e. it has already been clipped to those)"
  (declare (type (simple-array single-float (*)) scanline)
           (type fixnum base x)
           (type single-float x0 y0 x1 y1))
  (when (= y0 y1) (return-from %tt-handle-clipped-edge))
  (when (> y0 (tt-active-edge-ey e)) (return-from %tt-handle-clipped-edge))
  (when (< y1 (tt-active-edge-sy e)) (return-from %tt-handle-clipped-edge))
  (when (< y0 (tt-active-edge-sy e))
    (incf x0 (/ (* (- x1 x0) (- (tt-active-edge-sy e) y0)) (- y1 y0)))
    (setf y0 (tt-active-edge-sy e)))
  (when (> y1 (tt-active-edge-ey e))
    (incf x1 (/ (* (- x1 x0) (- (tt-active-edge-ey e) y1)) (- y1 y0)))
    (setf y1 (tt-active-edge-ey e)))
  (let ((fx (float x 1.0))
        (fx1 (float (+ x 1) 1.0)))
    (cond ((and (<= x0 fx) (<= x1 fx))
           (incf (aref scanline (+ base x)) (* (tt-active-edge-direction e) (- y1 y0))))
          ((and (>= x0 fx1) (>= x1 fx1)) nil)
          (t
           ;; coverage = 1 - average x position
           (incf (aref scanline (+ base x))
                 (* (* (tt-active-edge-direction e) (- y1 y0))
                    (- 1.0 (/ (+ (- x0 fx) (- x1 fx)) 2.0))))))))

(declaim (inline %tt-sized-trapezoid-area %tt-position-trapezoid-area %tt-sized-triangle-area))
(defun %tt-sized-trapezoid-area (height top-width bottom-width)
  (* (/ (+ top-width bottom-width) 2.0) height))
(defun %tt-position-trapezoid-area (height tx0 tx1 bx0 bx1)
  (%tt-sized-trapezoid-area height (- tx1 tx0) (- bx1 bx0)))
(defun %tt-sized-triangle-area (height width)
  (/ (* height width) 2.0))

(defun %tt-fill-active-edges-new (scanline fill len e y-top)
  "SCANLINE is indexed from 0, scanline_fill from FILL (scanline_fill-1 is FILL-1)"
  (declare (type (simple-array single-float (*)) scanline)
           (type fixnum fill len)
           (type single-float y-top))
  (let ((y-bottom (+ y-top 1.0)))
    (loop while e
          do (if (= (tt-active-edge-fdx e) 0.0)
                 ;; brute force every pixel
                 (let ((x0 (tt-active-edge-fx e)))
                   (when (< x0 len)
                     (if (>= x0 0.0)
                         (progn
                           (%tt-handle-clipped-edge scanline 0 (truncate x0) e x0 y-top x0 y-bottom)
                           (%tt-handle-clipped-edge scanline (1- fill) (+ (truncate x0) 1) e x0 y-top x0 y-bottom))
                         (%tt-handle-clipped-edge scanline (1- fill) 0 e x0 y-top x0 y-bottom))))
                 (let* ((x0 (tt-active-edge-fx e))
                        (dx (tt-active-edge-fdx e))
                        (xb (+ x0 dx))
                        (dy (tt-active-edge-fdy e))
                        x-top x-bottom sy0 sy1)
                   (declare (type single-float x0 dx xb dy))
                   ;; compute endpoints of line segment clipped to this scanline (if the
                   ;; line segment starts on this scanline. x0 is the intersection of the
                   ;; line with y_top, but that may be off the line segment.
                   (if (> (tt-active-edge-sy e) y-top)
                       (setf x-top (+ x0 (* dx (- (tt-active-edge-sy e) y-top)))
                             sy0 (tt-active-edge-sy e))
                       (setf x-top x0
                             sy0 y-top))
                   (if (< (tt-active-edge-ey e) y-bottom)
                       (setf x-bottom (+ x0 (* dx (- (tt-active-edge-ey e) y-top)))
                             sy1 (tt-active-edge-ey e))
                       (setf x-bottom xb
                             sy1 y-bottom))
                   (if (and (>= x-top 0) (>= x-bottom 0) (< x-top len) (< x-bottom len))
                       ;; from here on, we don't have to range check x values
                       (if (= (truncate x-top) (truncate x-bottom))
                           ;; simple case, only spans one pixel
                           (let* ((x (truncate x-top))
                                  (height (* (- sy1 sy0) (tt-active-edge-direction e))))
                             (incf (aref scanline x)
                                   (%tt-position-trapezoid-area height x-top (+ x 1.0) x-bottom (+ x 1.0)))
                             (incf (aref scanline (+ fill x)) height)) ; everything right of this pixel is filled
                           ;; covers 2+ pixels
                           (let (x1 x2 y-crossing y-final step sign area)
                             (when (> x-top x-bottom)
                               ;; flip scanline vertically; signed area is the same
                               (setf sy0 (- y-bottom (- sy0 y-top))
                                     sy1 (- y-bottom (- sy1 y-top)))
                               (rotatef sy0 sy1)
                               (rotatef x-bottom x-top)
                               (setf dx (- dx)
                                     dy (- dy))
                               (rotatef x0 xb))
                             (setf x1 (truncate x-top)
                                   x2 (truncate x-bottom))
                             ;; compute intersection with y axis at x1+1
                             (setf y-crossing (+ y-top (* dy (- (float (+ x1 1) 1.0) x0))))
                             ;; compute intersection with y axis at x2
                             (setf y-final (+ y-top (* dy (- (float x2 1.0) x0))))
                             (when (> y-crossing y-bottom)
                               (setf y-crossing y-bottom))
                             (setf sign (tt-active-edge-direction e))
                             ;; area of the rectangle covered from sy0..y_crossing
                             (setf area (* sign (- y-crossing sy0)))
                             ;; area of the triangle (x_top,sy0), (x1+1,sy0), (x1+1,y_crossing)
                             (incf (aref scanline x1) (%tt-sized-triangle-area area (- (float (+ x1 1) 1.0) x-top)))
                             ;; check if final y_crossing is blown up; no test case for this
                             (when (> y-final y-bottom)
                               (setf y-final y-bottom)
                               (setf dy (/ (- y-final y-crossing) (float (- x2 (+ x1 1)) 1.0)))) ; if denom=0, y_final = y_crossing, so y_final <= y_bottom
                             ;; in second pixel, area covered by line segment found in first pixel
                             ;; is always a rectangle 1 wide * the height of that line segment; this
                             ;; is exactly what the variable 'area' stores. it also gets a contribution
                             ;; from the line segment within it. the THIRD pixel will get the first
                             ;; pixel's rectangle contribution, the second pixel's rectangle contribution,
                             ;; and its own contribution. the 'own contribution' is the same in every pixel except
                             ;; the leftmost and rightmost, a trapezoid that slides down in each pixel.
                             ;; the second pixel's contribution to the third pixel will be the
                             ;; rectangle 1 wide times the height change in the second pixel, which is dy.
                             (setf step (* (* sign dy) 1.0)) ; dy is dy/dx, change in y for every 1 change in x,
                             ;; which multiplied by 1-pixel-width is how much pixel area changes for each step in x
                             ;; so the area advances by 'step' every time
                             (loop for x from (+ x1 1) below x2
                                   do (incf (aref scanline x) (+ area (/ step 2.0))) ; area of trapezoid is 1*step/2
                                      (incf area step))
                             ;; area covered in the last pixel is the rectangle from all the pixels to the left,
                             ;; plus the trapezoid filled by the line segment in this pixel all the way to the right edge
                             (incf (aref scanline x2)
                                   (+ area (* sign (%tt-position-trapezoid-area (- sy1 y-final) (float x2 1.0) (+ x2 1.0) x-bottom (+ x2 1.0)))))
                             ;; the rest of the line is filled based on the total height of the line segment in this pixel
                             (incf (aref scanline (+ fill x2)) (* sign (- sy1 sy0)))))
                       ;; if edge goes outside of box we're drawing, we require
                       ;; clipping logic. since this does not match the intended use
                       ;; of this library, we use a different, very slow brute
                       ;; force implementation
                       ;; note though that this does happen some of the time because
                       ;; x_top and x_bottom can be extrapolated at the top & bottom of
                       ;; the shape and actually lie outside the bounding box
                       (dotimes (x len)
                         ;; cases:
                         ;;
                         ;; there can be up to two intersections with the pixel. any intersection
                         ;; with left or right edges can be handled by splitting into two (or three)
                         ;; regions. intersections with top & bottom do not necessitate case-wise logic.
                         ;;
                         ;; the old way of doing this found the intersections with the left & right edges,
                         ;; then used some simple logic to produce up to three segments in sorted order
                         ;; from top-to-bottom. however, this had a problem: if an x edge was epsilon
                         ;; across the x border, then the corresponding y position might not be distinct
                         ;; from the other y segment, and it might ignored as an empty segment. to avoid
                         ;; that, we need to explicitly produce segments based on x positions.
                         ;; rename variables to clearly-defined pairs
                         (let* ((y0 y-top)
                                (x1 (float x 1.0))
                                (x2 (float (+ x 1) 1.0))
                                (x3 xb)
                                (y3 y-bottom)
                                ;; x = e->x + e->dx * (y-y_top)
                                ;; (y-y_top) = (x - e->x) / e->dx
                                ;; y = (x - e->x) / e->dx + y_top
                                (y1 (+ (/ (- (float x 1.0) x0) dx) y-top))
                                (y2 (+ (/ (- (float (+ x 1) 1.0) x0) dx) y-top)))
                           (cond
                             ((and (< x0 x1) (> x3 x2)) ; three segments descending down-right
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x1 y1)
                              (%tt-handle-clipped-edge scanline 0 x e x1 y1 x2 y2)
                              (%tt-handle-clipped-edge scanline 0 x e x2 y2 x3 y3))
                             ((and (< x3 x1) (> x0 x2)) ; three segments descending down-left
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x2 y2)
                              (%tt-handle-clipped-edge scanline 0 x e x2 y2 x1 y1)
                              (%tt-handle-clipped-edge scanline 0 x e x1 y1 x3 y3))
                             ((and (< x0 x1) (> x3 x1)) ; two segments across x, down-right
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x1 y1)
                              (%tt-handle-clipped-edge scanline 0 x e x1 y1 x3 y3))
                             ((and (< x3 x1) (> x0 x1)) ; two segments across x, down-left
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x1 y1)
                              (%tt-handle-clipped-edge scanline 0 x e x1 y1 x3 y3))
                             ((and (< x0 x2) (> x3 x2)) ; two segments across x+1, down-right
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x2 y2)
                              (%tt-handle-clipped-edge scanline 0 x e x2 y2 x3 y3))
                             ((and (< x3 x2) (> x0 x2)) ; two segments across x+1, down-left
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x2 y2)
                              (%tt-handle-clipped-edge scanline 0 x e x2 y2 x3 y3))
                             (t                ; one segment
                              (%tt-handle-clipped-edge scanline 0 x e x0 y0 x3 y3))))))))
             (setf e (tt-active-edge-next e)))))

(defun %tt-rasterize-sorted-edges (pixels w h stride edges n off-x off-y)
  "Directly AA rasterize edges w/o supersampling"
  (let* ((scanline (make-array (+ (* w 2) 1) :element-type 'single-float :initial-element 0.0))
         (scanline2 w)                  ; scanline2 = scanline + result->w
         (active nil)
         (y off-y)
         (j 0)
         (ei 0))
    (setf (tt-edge-y0 (aref edges n)) (+ (float (+ off-y h) 1.0) 1.0))
    (loop while (< j h)
          do ;; find center of pixel for this scanline
             (let ((scan-y-top (+ (float y 1.0) 0.0))
                   (scan-y-bottom (+ (float y 1.0) 1.0)))
               (fill scanline 0.0 :start 0 :end w)
               (fill scanline 0.0 :start scanline2 :end (+ scanline2 w 1))
               ;; update all active edges;
               ;; remove all active edges that terminate before the top of this scanline
               (let ((prev nil) (z active))
                 (loop while z
                       do (if (<= (tt-active-edge-ey z) scan-y-top)
                              (progn ; delete from list
                                (if prev
                                    (setf (tt-active-edge-next prev) (tt-active-edge-next z))
                                    (setf active (tt-active-edge-next z)))
                                (setf (tt-active-edge-direction z) 0.0))
                              (setf prev z)) ; advance through list
                          (setf z (tt-active-edge-next z))))
               ;; insert all edges that start before the bottom of this scanline
               (loop while (<= (tt-edge-y0 (aref edges ei)) scan-y-bottom)
                     do (let ((e (aref edges ei)))
                          (when (/= (tt-edge-y0 e) (tt-edge-y1 e))
                            (let ((z (%tt-new-active e off-x scan-y-top)))
                              (when (and (= j 0) (/= off-y 0))
                                (when (< (tt-active-edge-ey z) scan-y-top)
                                  ;; this can happen due to subpixel positioning and some kind of fp rounding error i think
                                  (setf (tt-active-edge-ey z) scan-y-top)))
                              ;; insert at front
                              (setf (tt-active-edge-next z) active)
                              (setf active z))))
                        (incf ei))
               ;; now process all active edges
               (when active
                 (%tt-fill-active-edges-new scanline (+ scanline2 1) w active scan-y-top))
               (let ((sum 0.0))
                 (declare (type single-float sum))
                 (dotimes (i w)
                   (let (k m)
                     (incf sum (aref scanline (+ scanline2 i)))
                     (setf k (+ (aref scanline i) sum))
                     (setf k (+ (* (abs k) 255.0) 0.5))
                     (setf m (truncate k))
                     (when (> m 255) (setf m 255))
                     (setf (aref pixels (+ (* j stride) i)) m))))
               ;; advance all the edges
               (let ((z active))
                 (loop while z
                       do (incf (tt-active-edge-fx z) (tt-active-edge-fdx z)) ; advance to position for current scanline
                          (setf z (tt-active-edge-next z))))
               (incf y)
               (incf j)))))

(defmacro %tt-compare (a b) `(< (tt-edge-y0 ,a) (tt-edge-y0 ,b)))

(defun %tt-sort-edges-ins-sort (p start n)
  (loop for i from 1 below n
        do (let ((tt (aref p (+ start i)))
                 (j i))
             (loop while (> j 0)
                   do (let ((b (aref p (+ start j -1))))
                        (unless (%tt-compare tt b) (return))
                        (setf (aref p (+ start j)) (aref p (+ start j -1)))
                        (decf j)))
             (when (/= i j)
               (setf (aref p (+ start j)) tt)))))

(defun %tt-sort-edges-quicksort (p start n)
  ;; threshold for transitioning to insertion sort
  (loop while (> n 12)
        do (let (c01 c12 c m i j)
             ;; compute median of three
             (setf m (ash n -1))
             (setf c01 (%tt-compare (aref p start) (aref p (+ start m))))
             (setf c12 (%tt-compare (aref p (+ start m)) (aref p (+ start n -1))))
             ;; if 0 >= mid >= end, or 0 < mid < end, then use mid
             (unless (eq c01 c12)
               ;; otherwise, we'll need to swap something else to middle
               (setf c (%tt-compare (aref p start) (aref p (+ start n -1))))
               ;; 0>mid && mid<n:  0>n => n; 0<n => 0
               ;; 0<mid && mid>n:  0>n => 0; 0<n => n
               (let ((z (if (eq c c12) 0 (- n 1))))
                 (rotatef (aref p (+ start z)) (aref p (+ start m)))))
             ;; now p[m] is the median-of-three
             ;; swap it to the beginning so it won't move around
             (rotatef (aref p start) (aref p (+ start m)))
             ;; partition loop
             (setf i 1 j (- n 1))
             (loop
               ;; handling of equality is crucial here
               ;; for sentinels & efficiency with duplicates
               (loop (unless (%tt-compare (aref p (+ start i)) (aref p start)) (return)) (incf i))
               (loop (unless (%tt-compare (aref p start) (aref p (+ start j))) (return)) (decf j))
               ;; make sure we haven't crossed
               (when (>= i j) (return))
               (rotatef (aref p (+ start i)) (aref p (+ start j)))
               (incf i)
               (decf j))
             ;; recurse on smaller side, iterate on larger
             (if (< j (- n i))
                 (progn
                   (%tt-sort-edges-quicksort p start j)
                   (incf start i)
                   (setf n (- n i)))
                 (progn
                   (%tt-sort-edges-quicksort p (+ start i) (- n i))
                   (setf n j))))))

(defun %tt-sort-edges (p n)
  (%tt-sort-edges-quicksort p 0 n)
  (%tt-sort-edges-ins-sort p 0 n))

(defun %tt-rasterize (pixels w h stride points-x points-y wcount scale-x scale-y shift-x shift-y off-x off-y invert)
  (let* ((y-scale-inv (if invert (- scale-y) scale-y))
         (vsubsample 1)
         (n (reduce #'+ wcount))
         ;; now we have to blow out the windings into explicit edge lists
         (edges (make-array (+ n 1)))   ; add an extra one as a sentinel
         (m 0))
    (dotimes (i (+ n 1)) (setf (aref edges i) (%make-tt-edge)))
    (setf n 0)
    (dolist (count wcount)
      (let ((p m))
        (incf m count)
        (let ((j (- count 1)))
          (loop for k from 0 below count
                do (let ((a k) (b j))
                     ;; skip the edge if horizontal
                     (unless (= (aref points-y (+ p j)) (aref points-y (+ p k)))
                       ;; add edge from j to k to the list
                       (let ((e (aref edges n)))
                         (setf (tt-edge-invert e) nil)
                         (when (if invert
                                   (> (aref points-y (+ p j)) (aref points-y (+ p k)))
                                   (< (aref points-y (+ p j)) (aref points-y (+ p k))))
                           (setf (tt-edge-invert e) t
                                 a j b k))
                         (setf (tt-edge-x0 e) (+ (* (aref points-x (+ p a)) scale-x) shift-x)
                               (tt-edge-y0 e) (* (+ (* (aref points-y (+ p a)) y-scale-inv) shift-y) vsubsample)
                               (tt-edge-x1 e) (+ (* (aref points-x (+ p b)) scale-x) shift-x)
                               (tt-edge-y1 e) (* (+ (* (aref points-y (+ p b)) y-scale-inv) shift-y) vsubsample))
                         (incf n))))
                   (setf j k)))))
    ;; now sort the edges by their highest point (should snap to integer, and then by x)
    (%tt-sort-edges edges n)
    ;; now, traverse the scanlines and find the intersections on each scanline, use xor winding rule
    (%tt-rasterize-sorted-edges pixels w h stride edges n off-x off-y)))

(defun %tt-tesselate-curve (px py x0 y0 x1 y1 x2 y2 objspace-flatness-squared n)
  "Tessellate until threshold p is happy... @TODO warped to compensate for non-linear stretching"
  (declare (type single-float x0 y0 x1 y1 x2 y2 objspace-flatness-squared))
  ;; midpoint
  (let* ((mx (/ (+ x0 (* 2.0 x1) x2) 4.0))
         (my (/ (+ y0 (* 2.0 y1) y2) 4.0))
         ;; versus directly drawn line
         (dx (- (/ (+ x0 x2) 2.0) mx))
         (dy (- (/ (+ y0 y2) 2.0) my)))
    (when (> n 16)                      ; 65536 segments on one curve better be enough!
      (return-from %tt-tesselate-curve 1))
    (if (> (+ (* dx dx) (* dy dy)) objspace-flatness-squared) ; half-pixel error allowed... need to be smaller if AA
        (progn
          (%tt-tesselate-curve px py x0 y0 (/ (+ x0 x1) 2.0) (/ (+ y0 y1) 2.0) mx my objspace-flatness-squared (+ n 1))
          (%tt-tesselate-curve px py mx my (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0) x2 y2 objspace-flatness-squared (+ n 1)))
        (progn
          (vector-push-extend x2 px)
          (vector-push-extend y2 py)))
    1))

(defun %tt-tesselate-cubic (px py x0 y0 x1 y1 x2 y2 x3 y3 objspace-flatness-squared n)
  (declare (type single-float x0 y0 x1 y1 x2 y2 x3 y3 objspace-flatness-squared))
  ;; @TODO this "flatness" calculation is just made-up nonsense that seems to work well enough
  (let* ((dx0 (- x1 x0)) (dy0 (- y1 y0))
         (dx1 (- x2 x1)) (dy1 (- y2 y1))
         (dx2 (- x3 x2)) (dy2 (- y3 y2))
         (dx (- x3 x0)) (dy (- y3 y0))
         (longlen (float (+ (sqrt (float (+ (* dx0 dx0) (* dy0 dy0)) 1d0))
                            (sqrt (float (+ (* dx1 dx1) (* dy1 dy1)) 1d0))
                            (sqrt (float (+ (* dx2 dx2) (* dy2 dy2)) 1d0)))
                         1.0))
         (shortlen (float (sqrt (float (+ (* dx dx) (* dy dy)) 1d0)) 1.0))
         (flatness-squared (- (* longlen longlen) (* shortlen shortlen))))
    (when (> n 16)                      ; 65536 segments on one curve better be enough!
      (return-from %tt-tesselate-cubic))
    (if (> flatness-squared objspace-flatness-squared)
        (let* ((x01 (/ (+ x0 x1) 2.0)) (y01 (/ (+ y0 y1) 2.0))
               (x12 (/ (+ x1 x2) 2.0)) (y12 (/ (+ y1 y2) 2.0))
               (x23 (/ (+ x2 x3) 2.0)) (y23 (/ (+ y2 y3) 2.0))
               (xa (/ (+ x01 x12) 2.0)) (ya (/ (+ y01 y12) 2.0))
               (xb (/ (+ x12 x23) 2.0)) (yb (/ (+ y12 y23) 2.0))
               (mx (/ (+ xa xb) 2.0)) (my (/ (+ ya yb) 2.0)))
          (%tt-tesselate-cubic px py x0 y0 x01 y01 xa ya mx my objspace-flatness-squared (+ n 1))
          (%tt-tesselate-cubic px py mx my xb yb x23 y23 x3 y3 objspace-flatness-squared (+ n 1)))
        (progn
          (vector-push-extend x3 px)
          (vector-push-extend y3 py)))))

(defun %tt-flatten-curves (vertices objspace-flatness)
  "Returns points (as x and y vectors) on the boundary of a shape
Returns (values points-x points-y contour-lengths)"
  (let ((px (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
        (py (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
        (contour-lengths '())
        (objspace-flatness-squared (* objspace-flatness objspace-flatness))
        (start 0)
        (x 0.0) (y 0.0))
    ;; count how many "moves" there are to get the contour count
    (when (zerop (count +stbtt-vmove+ vertices :key #'tt-vertex-type))
      (return-from %tt-flatten-curves nil))
    (let ((n -1))
      (loop for v across vertices
            do (case (tt-vertex-type v)
                 (#.+stbtt-vmove+
                  ;; start the next contour
                  (when (>= n 0) (push (- (fill-pointer px) start) contour-lengths))
                  (incf n)
                  (setf start (fill-pointer px))
                  (setf x (float (tt-vertex-x v) 1.0) y (float (tt-vertex-y v) 1.0))
                  (vector-push-extend x px) (vector-push-extend y py))
                 (#.+stbtt-vline+
                  (setf x (float (tt-vertex-x v) 1.0) y (float (tt-vertex-y v) 1.0))
                  (vector-push-extend x px) (vector-push-extend y py))
                 (#.+stbtt-vcurve+
                  (%tt-tesselate-curve px py x y
                                       (float (tt-vertex-cx v) 1.0) (float (tt-vertex-cy v) 1.0)
                                       (float (tt-vertex-x v) 1.0) (float (tt-vertex-y v) 1.0)
                                       objspace-flatness-squared 0)
                  (setf x (float (tt-vertex-x v) 1.0) y (float (tt-vertex-y v) 1.0)))
                 (#.+stbtt-vcubic+
                  (%tt-tesselate-cubic px py x y
                                       (float (tt-vertex-cx v) 1.0) (float (tt-vertex-cy v) 1.0)
                                       (float (tt-vertex-cx1 v) 1.0) (float (tt-vertex-cy1 v) 1.0)
                                       (float (tt-vertex-x v) 1.0) (float (tt-vertex-y v) 1.0)
                                       objspace-flatness-squared 0)
                  (setf x (float (tt-vertex-x v) 1.0) y (float (tt-vertex-y v) 1.0)))))
      (push (- (fill-pointer px) start) contour-lengths))
    (values (coerce px '(simple-array single-float (*)))
            (coerce py '(simple-array single-float (*)))
            (nreverse contour-lengths))))

(defun stbtt-rasterize (pixels w h stride flatness-in-pixels vertices scale-x scale-y shift-x shift-y x-off y-off invert)
  "Rasterize a shape with quadratic beziers into a bitmap"
  (let ((scale (if (> scale-x scale-y) scale-y scale-x)))
    (multiple-value-bind (points-x points-y winding-lengths)
        (%tt-flatten-curves vertices (/ flatness-in-pixels scale))
      (when points-x
        (%tt-rasterize pixels w h stride points-x points-y winding-lengths
                       scale-x scale-y shift-x shift-y x-off y-off invert)))))

(defun stbtt-get-glyph-bitmap-subpixel (info scale-x scale-y shift-x shift-y glyph)
  "Allocates a large-enough single-channel 8bpp bitmap and renders the glyph into it
Returns (values pixels width height xoff yoff), pixels NIL for empty glyphs"
  (%with-c-floats
   (let ((vertices (stbtt-get-glyph-shape info glyph)))
    (when (= scale-x 0) (setf scale-x scale-y))
    (when (= scale-y 0)
      (when (= scale-x 0)
        (return-from stbtt-get-glyph-bitmap-subpixel (values nil 0 0 0 0)))
      (setf scale-y scale-x))
    (multiple-value-bind (ix0 iy0 ix1 iy1)
        (stbtt-get-glyph-bitmap-box-subpixel info glyph scale-x scale-y shift-x shift-y)
      ;; now we get the size
      (let* ((w (- ix1 ix0))
             (h (- iy1 iy0))
             (pixels nil))
        (when (and (/= w 0) (/= h 0))
          (setf pixels (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 0))
          (stbtt-rasterize pixels w h w 0.35 vertices scale-x scale-y shift-x shift-y ix0 iy0 t))
        (values pixels w h ix0 iy0))))))

(defun stbtt-get-glyph-bitmap (info scale-x scale-y glyph)
  (stbtt-get-glyph-bitmap-subpixel info scale-x scale-y 0.0 0.0 glyph))

(defun stbtt-get-codepoint-bitmap (info scale-x scale-y codepoint)
  "Returns (values pixels width height xoff yoff)"
  (stbtt-get-glyph-bitmap-subpixel info scale-x scale-y 0.0 0.0 (stbtt-find-glyph-index info codepoint)))

(defun stbtt-make-glyph-bitmap-subpixel (info output out-w out-h out-stride scale-x scale-y shift-x shift-y glyph)
  "Same as stbtt-get-glyph-bitmap-subpixel, but you pass in storage for the bitmap"
  (%with-c-floats
    (let ((vertices (stbtt-get-glyph-shape info glyph)))
      (multiple-value-bind (ix0 iy0) (stbtt-get-glyph-bitmap-box-subpixel info glyph scale-x scale-y shift-x shift-y)
        (when (and (/= out-w 0) (/= out-h 0))
          (stbtt-rasterize output out-w out-h out-stride 0.35 vertices scale-x scale-y shift-x shift-y ix0 iy0 t))))))

(defun stbtt-make-codepoint-bitmap (info output out-w out-h out-stride scale-x scale-y codepoint)
  (stbtt-make-glyph-bitmap-subpixel info output out-w out-h out-stride scale-x scale-y 0.0 0.0
                                    (stbtt-find-glyph-index info codepoint)))

;;;----------------------------------------------------------------------------------
;;; Signed distance field rendering
;;;----------------------------------------------------------------------------------

(defun %tt-ray-intersect-bezier (orig-x orig-y ray-x ray-y q0x q0y q1x q1y q2x q2y)
  "Returns (values num-hits hit0-x hit0-y hit1-x hit1-y)"
  (declare (type single-float orig-x orig-y ray-x ray-y q0x q0y q1x q1y q2x q2y))
  (let* ((q0perp (- (* q0y ray-x) (* q0x ray-y)))
         (q1perp (- (* q1y ray-x) (* q1x ray-y)))
         (q2perp (- (* q2y ray-x) (* q2x ray-y)))
         (roperp (- (* orig-y ray-x) (* orig-x ray-y)))
         (a (+ (- q0perp (* 2.0 q1perp)) q2perp))
         (b (- q1perp q0perp))
         (c (- q0perp roperp))
         (s0 0.0) (s1 0.0)
         (num-s 0))
    (declare (type single-float s0 s1))
    (if (/= a 0.0)
        (let ((discr (- (* b b) (* a c))))
          (when (> discr 0.0)
            (let ((rcpna (/ -1.0 a))
                  (d (float (sqrt (float discr 1d0)) 1.0)))
              (setf s0 (* (+ b d) rcpna)
                    s1 (* (- b d) rcpna))
              (when (and (>= s0 0.0) (<= s0 1.0))
                (setf num-s 1))
              (when (and (> d 0.0) (>= s1 0.0) (<= s1 1.0))
                (when (= num-s 0) (setf s0 s1))
                (incf num-s)))))
        ;; 2*b*s + c = 0
        ;; s = -c / (2*b)
        (progn
          (setf s0 (/ c (* -2.0 b)))
          (when (and (>= s0 0.0) (<= s0 1.0))
            (setf num-s 1))))
    (if (= num-s 0)
        (values 0 0.0 0.0 0.0 0.0)
        (let* ((rcp-len2 (/ 1.0 (+ (* ray-x ray-x) (* ray-y ray-y))))
               (rayn-x (* ray-x rcp-len2))
               (rayn-y (* ray-y rcp-len2))
               (q0d (+ (* q0x rayn-x) (* q0y rayn-y)))
               (q1d (+ (* q1x rayn-x) (* q1y rayn-y)))
               (q2d (+ (* q2x rayn-x) (* q2y rayn-y)))
               (rod (+ (* orig-x rayn-x) (* orig-y rayn-y)))
               (q10d (- q1d q0d))
               (q20d (- q2d q0d))
               (q0rd (- q0d rod))
               (h00 (+ (+ q0rd (* (* s0 (- 2.0 (* 2.0 s0))) q10d)) (* (* s0 s0) q20d)))
               (h01 (+ (* a s0) b)))
          (if (> num-s 1)
              (values 2 h00 h01
                      (+ (+ q0rd (* (* s1 (- 2.0 (* 2.0 s1))) q10d)) (* (* s1 s1) q20d))
                      (+ (* a s1) b))
              (values 1 h00 h01 0.0 0.0))))))

(defun %tt-compute-crossings-x (x y verts)
  (declare (type single-float x y))
  (let* ((nverts (length verts))
         (winding 0)
         (y-frac (float (mod* (float y 1d0) 1d0) 1.0)))
    ;; make sure y never passes through a vertex of the shape
    (cond ((< y-frac 0.01) (incf y 0.01))
          ((> y-frac 0.99) (decf y 0.01)))
    ;; test a ray from (-infinity,y) to (x,y)
    (flet ((line-crossing (x0 y0 x1 y1)
             (when (and (> y (float (min y0 y1) 1.0)) (< y (float (max y0 y1) 1.0)) (> x (float (min x0 x1) 1.0)))
               (let ((x-inter (+ (* (/ (- y (float y0 1.0)) (float (- y1 y0) 1.0)) (float (- x1 x0) 1.0)) (float x0 1.0))))
                 (when (< x-inter x)
                   (incf winding (if (< y0 y1) 1 -1)))))))
      (dotimes (i nverts)
        (let ((v (aref verts i)))
          (when (= (tt-vertex-type v) +stbtt-vline+)
            (let ((p (aref verts (1- i))))
              (line-crossing (tt-vertex-x p) (tt-vertex-y p) (tt-vertex-x v) (tt-vertex-y v))))
          (when (= (tt-vertex-type v) +stbtt-vcurve+)
            (let* ((p (aref verts (1- i)))
                   (x0 (tt-vertex-x p)) (y0 (tt-vertex-y p))
                   (x1 (tt-vertex-cx v)) (y1 (tt-vertex-cy v))
                   (x2 (tt-vertex-x v)) (y2 (tt-vertex-y v))
                   (ax (min x0 (min x1 x2))) (ay (min y0 (min y1 y2)))
                   (by (max y0 (max y1 y2))))
              (when (and (> y (float ay 1.0)) (< y (float by 1.0)) (> x (float ax 1.0)))
                (if (or (and (= x0 x1) (= y0 y1)) (and (= x1 x2) (= y1 y2)))
                    (line-crossing x0 y0 x2 y2)
                    (multiple-value-bind (num-hits h00 h01 h10 h11)
                        (%tt-ray-intersect-bezier x y 1.0 0.0
                                                  (float x0 1.0) (float y0 1.0)
                                                  (float x1 1.0) (float y1 1.0)
                                                  (float x2 1.0) (float y2 1.0))
                      (when (>= num-hits 1)
                        (when (< h00 0)
                          (incf winding (if (< h01 0) -1 1))))
                      (when (>= num-hits 2)
                        (when (< h10 0)
                          (incf winding (if (< h11 0) -1 1))))))))))))
    winding))

(defun mod* (x y)
  "C fmod(): result has the sign of X
   NOTE: libm call, CL rem is not exact on every implementation (ECL)"
  (float-features:with-float-traps-masked t
    (cffi:foreign-funcall "fmod" :double (float x 1d0) :double (float y 1d0) :double)))

(defun %tt-cuberoot (x)
  (declare (type single-float x))
  (if (< x 0)
      (- (float (expt (float (- x) 1d0) (float (/ 1.0 3.0) 1d0)) 1.0))
      (float (expt (float x 1d0) (float (/ 1.0 3.0) 1d0)) 1.0)))

(defun %tt-solve-cubic (a b c)
  "x^3 + a*x^2 + b*x + c = 0, returns (values count r0 r1 r2)"
  (declare (type single-float a b c))
  (let* ((s (/ (- a) 3.0))
         (p (- b (/ (* a a) 3.0)))
         (q (+ (/ (* a (- (* 2.0 a a) (* 9.0 b))) 27.0) c))
         (p3 (* p p p))
         (d (+ (* q q) (/ (* 4.0 p3) 27.0))))
    (if (>= d 0)
        (let* ((z (float (sqrt (float d 1d0)) 1.0))
               (u (/ (+ (- q) z) 2.0))
               (v (/ (- (- q) z) 2.0)))
          (setf u (%tt-cuberoot u)
                v (%tt-cuberoot v))
          (values 1 (+ s u v) 0.0 0.0))
        (let* ((acos-arg (/ (* (- (sqrt (float (/ -27.0 p3) 1d0))) q) 2d0))
               (u (if (> (abs acos-arg) 1d0)
                      ;; C acos() returns NaN here and the NaN roots are all rejected by the caller
                      (return-from %tt-solve-cubic (values 0 0.0 0.0 0.0))
                      (float (sqrt (float (/ (- p) 3.0) 1d0)) 1.0)))
               ;; p3 must be negative, since d is negative
               ;; NOTE: C casts bind tighter: ((float)acos(...))/3 and ((float)cos(...))*1.732050808f
               (v (/ (float (acos (/ (* (- (sqrt (float (/ -27.0 p3) 1d0))) q) 2d0)) 1.0) 3.0))
               (m (float (cos (float v 1d0)) 1.0))
               (n (* (float (cos (- (float v 1d0) (/ 3.141592d0 2d0))) 1.0) 1.732050808)))
          (values 3
                  (+ s (* (* u 2.0) m))
                  (- s (* u (+ m n)))
                  (- s (* u (- m n))))))))

(defun stbtt-get-glyph-sdf (info scale glyph padding onedge-value pixel-dist-scale)
  "Signed distance field glyph, returns (values pixels width height xoff yoff), pixels NIL for empty glyphs"
  (let ((scale-x scale) (scale-y scale))
    (when (= scale 0) (return-from stbtt-get-glyph-sdf (values nil 0 0 0 0)))
    (multiple-value-bind (ix0 iy0 ix1 iy1) (stbtt-get-glyph-bitmap-box-subpixel info glyph scale scale 0.0 0.0)
      ;; if empty, return NULL
      (when (or (= ix0 ix1) (= iy0 iy1))
        (return-from stbtt-get-glyph-sdf (values nil 0 0 0 0)))
      (decf ix0 padding) (decf iy0 padding)
      (incf ix1 padding) (incf iy1 padding)
      (let* ((w (- ix1 ix0))
             (h (- iy1 iy0))
             (data (make-array (* w h) :element-type '(unsigned-byte 8) :initial-element 0))
             (eps (float (/ 1d0 1024d0) 1.0))
             (eps2 (* eps eps))
             (verts (stbtt-get-glyph-shape info glyph))
             (num-verts (length verts))
             (precompute (make-array num-verts :element-type 'single-float :initial-element 0.0)))
        ;; invert for y-downwards bitmaps
        (setf scale-y (- scale-y))
        (macrolet ((vx* (i) `(* (float (tt-vertex-x (aref verts ,i)) 1.0) scale-x))
                   (vy* (i) `(* (float (tt-vertex-y (aref verts ,i)) 1.0) scale-y))
                   (vcx* (i) `(* (float (tt-vertex-cx (aref verts ,i)) 1.0) scale-x))
                   (vcy* (i) `(* (float (tt-vertex-cy (aref verts ,i)) 1.0) scale-y)))
          (loop for i from 0 below num-verts
                for j = (1- num-verts) then (1- i)
                do (let ((type (tt-vertex-type (aref verts i))))
                     (cond
                       ((= type +stbtt-vline+)
                        (let* ((x0 (vx* i)) (y0 (vy* i))
                               (x1 (vx* j)) (y1 (vy* j))
                               (dist (float (sqrt (float (+ (* (- x1 x0) (- x1 x0)) (* (- y1 y0) (- y1 y0))) 1d0)) 1.0)))
                          (setf (aref precompute i) (if (< dist eps) 0.0 (/ 1.0 dist)))))
                       ((= type +stbtt-vcurve+)
                        (let* ((x2 (vx* j)) (y2 (vy* j))
                               (x1 (vcx* i)) (y1 (vcy* i))
                               (x0 (vx* i)) (y0 (vy* i))
                               (bx (+ (- x0 (* 2.0 x1)) x2)) (by (+ (- y0 (* 2.0 y1)) y2))
                               (len2 (+ (* bx bx) (* by by))))
                          (setf (aref precompute i) (if (>= len2 eps2) (/ 1.0 len2) 0.0))))
                       (t (setf (aref precompute i) 0.0)))))
          (loop for y from iy0 below iy1
                do (loop for x from ix0 below ix1
                         do (let* ((min-dist 999999.0)
                                   (sx (+ (float x 1.0) 0.5))
                                   (sy (+ (float y 1.0) 0.5))
                                   (x-gspace (/ sx scale-x))
                                   (y-gspace (/ sy scale-y))
                                   ;; @OPTIMIZE: this could just be a rasterization, but needs to be line vs. non-tesselated curves so a new path
                                   (winding (%tt-compute-crossings-x x-gspace y-gspace verts))
                                   val)
                              (declare (type single-float min-dist))
                              (flet ((check (px py)
                                       (let ((dist2 (+ (* (- px sx) (- px sx)) (* (- py sy) (- py sy)))))
                                         (when (< dist2 (* min-dist min-dist))
                                           (setf min-dist (float (sqrt (float dist2 1d0)) 1.0))))))
                                (dotimes (i num-verts)
                                  (let ((x0 (vx* i)) (y0 (vy* i))
                                        (type (tt-vertex-type (aref verts i))))
                                    (cond
                                      ((and (= type +stbtt-vline+) (/= (aref precompute i) 0.0))
                                       (let* ((x1 (vx* (1- i))) (y1 (vy* (1- i)))
                                              (dist 0.0))
                                         ;; check position along line
                                         ;; x' = x0 + t*(x1-x0), y' = y0 + t*(y1-y0)
                                         ;; minimize (x'-sx)*(x'-sx)+(y'-sy)*(y'-sy)
                                         (check x0 y0)
                                         ;; coarse culling against bbox
                                         (setf dist (* (float (abs (float (- (* (- x1 x0) (- y0 sy)) (* (- y1 y0) (- x0 sx))) 1d0)) 1.0)
                                                       (aref precompute i)))
                                         (when (< dist min-dist)
                                           ;; check position along line
                                           (let* ((dx (- x1 x0)) (dy (- y1 y0))
                                                  (px (- x0 sx)) (py (- y0 sy))
                                                  ;; minimize (px+t*dx)^2 + (py+t*dy)^2 = px*px + 2*px*dx*t + t^2*dx*dx + py*py + 2*py*dy*t + t^2*dy*dy
                                                  ;; derivative: 2*px*dx + 2*py*dy + (2*dx*dx+2*dy*dy)*t, set to 0 and solve
                                                  (tt (/ (- (+ (* px dx) (* py dy))) (+ (* dx dx) (* dy dy)))))
                                             (when (and (>= tt 0.0) (<= tt 1.0))
                                               (setf min-dist dist))))))
                                      ((= type +stbtt-vcurve+)
                                       (let* ((x2 (vx* (1- i))) (y2 (vy* (1- i)))
                                              (x1 (vcx* i)) (y1 (vcy* i))
                                              (box-x0 (min (min x0 x1) x2))
                                              (box-y0 (min (min y0 y1) y2))
                                              (box-x1 (max (max x0 x1) x2))
                                              (box-y1 (max (max y0 y1) y2)))
                                         ;; coarse culling against bbox to avoid computing cubic unnecessarily
                                         (when (and (> sx (- box-x0 min-dist)) (< sx (+ box-x1 min-dist))
                                                    (> sy (- box-y0 min-dist)) (< sy (+ box-y1 min-dist)))
                                           (let* ((num 0)
                                                  (ax (- x1 x0)) (ay (- y1 y0))
                                                  (bx (+ (- x0 (* 2.0 x1)) x2)) (by (+ (- y0 (* 2.0 y1)) y2))
                                                  (mx (- x0 sx)) (my (- y0 sy))
                                                  (res (make-array 3 :element-type 'single-float :initial-element 0.0))
                                                  (a-inv (aref precompute i)))
                                             (if (= a-inv 0.0) ; if a_inv is 0, it's 2nd degree so use quadratic formula
                                                 (let ((a (* 3.0 (+ (* ax bx) (* ay by))))
                                                       (b (+ (* 2.0 (+ (* ax ax) (* ay ay))) (+ (* mx bx) (* my by))))
                                                       (c (+ (* mx ax) (* my ay))))
                                                   (if (< (abs a) eps2) ; if a is 0, it's linear
                                                       (when (>= (abs b) eps2)
                                                         (setf (aref res num) (/ (- c) b))
                                                         (incf num))
                                                       (let ((discriminant (- (* b b) (* (* 4.0 a) c))))
                                                         (if (< discriminant 0)
                                                             (setf num 0)
                                                             (let ((root (float (sqrt (float discriminant 1d0)) 1.0)))
                                                               (setf (aref res 0) (/ (- (- b) root) (* 2.0 a))
                                                                     (aref res 1) (/ (+ (- b) root) (* 2.0 a)))
                                                               (setf num 2)))))) ; don't bother distinguishing 1-solution case, as code below will still work
                                                 (let ((b (* (* 3.0 (+ (* ax bx) (* ay by))) a-inv)) ; could precompute this as it doesn't depend on sample point
                                                       (c (* (+ (* 2.0 (+ (* ax ax) (* ay ay))) (+ (* mx bx) (* my by))) a-inv))
                                                       (d (* (+ (* mx ax) (* my ay)) a-inv)))
                                                   (multiple-value-bind (count r0 r1 r2) (%tt-solve-cubic b c d)
                                                     (setf num count
                                                           (aref res 0) r0 (aref res 1) r1 (aref res 2) r2))))
                                             (check x0 y0)
                                             (flet ((root (k)
                                                      (let* ((tt (aref res k)) (it (- 1.0 tt))
                                                             (px (+ (+ (* (* it it) x0) (* (* (* 2.0 tt) it) x1)) (* (* tt tt) x2)))
                                                             (py (+ (+ (* (* it it) y0) (* (* (* 2.0 tt) it) y1)) (* (* tt tt) y2))))
                                                        (check px py))))
                                               (when (and (>= num 1) (>= (aref res 0) 0.0) (<= (aref res 0) 1.0)) (root 0))
                                               (when (and (>= num 2) (>= (aref res 1) 0.0) (<= (aref res 1) 1.0)) (root 1))
                                               (when (and (>= num 3) (>= (aref res 2) 0.0) (<= (aref res 2) 1.0)) (root 2)))))))))))
                              (when (= winding 0)
                                (setf min-dist (- min-dist))) ; if outside the shape, value is negative
                              (setf val (+ (float onedge-value 1.0) (* pixel-dist-scale min-dist)))
                              (cond ((< val 0) (setf val 0.0))
                                    ((> val 255) (setf val 255.0)))
                              (setf (aref data (+ (* (- y iy0) w) (- x ix0))) (truncate val))))))
        (values data w h ix0 iy0)))))

(defun stbtt-get-codepoint-sdf (info scale codepoint padding onedge-value pixel-dist-scale)
  (%with-c-floats (stbtt-get-glyph-sdf info scale (stbtt-find-glyph-index info codepoint) padding onedge-value pixel-dist-scale)))

;;;===================================================================================
;;; stb_rect_pack - Rectangle packing (skyline bottom-left)
;;; Port of raylib/src/external/stb_rect_pack.h (v1.01)
;;;===================================================================================

(defconstant +stbrp-maxval+ #x7fffffff)

(defstruct (stbrp-rect (:constructor make-stbrp-rect (&key id w h)))
  (id 0) (w 0) (h 0) (x 0) (y 0) (was-packed 0))

(defstruct (stbrp-node (:constructor %make-stbrp-node ()))
  (x 0) (y 0) (next nil))

(defstruct (stbrp-context (:constructor %make-stbrp-context ()))
  (width 0) (height 0) (align 0) (init-mode 0) (heuristic 0) (num-nodes 0)
  (active-head nil) (free-head nil)
  (extra (vector (%make-stbrp-node) (%make-stbrp-node)))) ; we allocate two extra nodes so optimal user-node-count is 'width' not 'width+2'

(defun %stbrp-setup-allow-out-of-mem (context allow-out-of-mem)
  (if allow-out-of-mem
      ;; if it's ok to run out of memory, then don't bother aligning them;
      ;; this gives better packing, but may fail due to OOM (even though
      ;; the rectangles easily fit). @TODO a smarter approach would be to only
      ;; quantize once we've hit OOM, then we could get rid of this parameter.
      (setf (stbrp-context-align context) 1)
      ;; if it's not ok to run out of memory, then quantize the widths
      ;; so that num_nodes is always enough nodes.
      (setf (stbrp-context-align context)
            (floor (+ (stbrp-context-width context) (stbrp-context-num-nodes context) -1)
                   (stbrp-context-num-nodes context)))))

(defun stbrp-init-target (width height num-nodes)
  "Initialize a rectangle packer, returns the context"
  (let ((context (%make-stbrp-context))
        (nodes (make-array num-nodes)))
    (dotimes (i num-nodes) (setf (aref nodes i) (%make-stbrp-node)))
    (dotimes (i (1- num-nodes))
      (setf (stbrp-node-next (aref nodes i)) (aref nodes (1+ i))))
    (setf (stbrp-node-next (aref nodes (1- num-nodes))) nil)
    (setf (stbrp-context-init-mode context) 1 ; STBRP__INIT_skyline
          (stbrp-context-heuristic context) 0 ; STBRP_HEURISTIC_Skyline_default
          (stbrp-context-free-head context) (aref nodes 0)
          (stbrp-context-active-head context) (aref (stbrp-context-extra context) 0)
          (stbrp-context-width context) width
          (stbrp-context-height context) height
          (stbrp-context-num-nodes context) num-nodes)
    (%stbrp-setup-allow-out-of-mem context nil)
    ;; node 0 is the full width, node 1 is the sentinel (lets us not store width explicitly)
    (let ((e0 (aref (stbrp-context-extra context) 0))
          (e1 (aref (stbrp-context-extra context) 1)))
      (setf (stbrp-node-x e0) 0 (stbrp-node-y e0) 0 (stbrp-node-next e0) e1
            (stbrp-node-x e1) width (stbrp-node-y e1) (ash 1 30) (stbrp-node-next e1) nil))
    context))

(defun %stbrp-skyline-find-min-y (first x0 width)
  "Find minimum y position if it starts at x1, returns (values min-y waste)"
  (let ((node first)
        (x1 (+ x0 width))
        (min-y 0) (visited-width 0) (waste-area 0))
    (loop while (< (stbrp-node-x node) x1)
          do (if (> (stbrp-node-y node) min-y)
                 (progn
                   ;; raise min_y higher.
                   ;; we've accounted for all waste up to min_y,
                   ;; but we'll now add more waste for everything we've visited
                   (incf waste-area (* visited-width (- (stbrp-node-y node) min-y)))
                   (setf min-y (stbrp-node-y node))
                   ;; the first time through, visited_width might be reduced
                   (if (< (stbrp-node-x node) x0)
                       (incf visited-width (- (stbrp-node-x (stbrp-node-next node)) x0))
                       (incf visited-width (- (stbrp-node-x (stbrp-node-next node)) (stbrp-node-x node)))))
                 ;; add waste area
                 (let ((under-width (- (stbrp-node-x (stbrp-node-next node)) (stbrp-node-x node))))
                   (when (> (+ under-width visited-width) width)
                     (setf under-width (- width visited-width)))
                   (incf waste-area (* under-width (- min-y (stbrp-node-y node))))
                   (incf visited-width under-width)))
             (setf node (stbrp-node-next node)))
    (values min-y waste-area)))

(defun %stbrp-skyline-find-best-pos (c width height)
  "Returns (values x y prev-link) where prev-link is (node . slot) or NIL"
  (let ((best-waste (ash 1 30)) (best-x 0) (best-y (ash 1 30))
        (best nil))
    ;; align to multiple of c->align
    (setf width (+ width (stbrp-context-align c) -1))
    (decf width (mod width (stbrp-context-align c)))
    ;; if it can't possibly fit, bail immediately
    (when (or (> width (stbrp-context-width c)) (> height (stbrp-context-height c)))
      (return-from %stbrp-skyline-find-best-pos (values 0 0 nil)))
    ;; prev links are represented as the node owning the 'next' slot, :head for context->active_head
    (let ((node (stbrp-context-active-head c))
          (prev :head))
      (loop while (<= (+ (stbrp-node-x node) width) (stbrp-context-width c))
            do (multiple-value-bind (y waste) (%stbrp-skyline-find-min-y node (stbrp-node-x node) width)
                 (if (= (stbrp-context-heuristic c) 0) ; STBRP_HEURISTIC_Skyline_BL_sortHeight: actually just want to test BL
                     ;; bottom left
                     (when (< y best-y)
                       (setf best-y y
                             best prev))
                     ;; best-fit
                     (when (<= (+ y height) (stbrp-context-height c))
                       ;; can only use it if it first vertically
                       (when (or (< y best-y) (and (= y best-y) (< waste best-waste)))
                         (setf best-y y
                               best-waste waste
                               best prev)))))
               (setf prev node
                     node (stbrp-node-next node))))
    (flet ((deref (link) (if (eq link :head) (stbrp-context-active-head c) (stbrp-node-next link))))
      (setf best-x (if (null best) 0 (stbrp-node-x (deref best)))))
    ;; NOTE: STBRP_HEURISTIC_Skyline_BF_sortHeight is not used by raylib (default heuristic is BL)
    (values best-x best-y best)))

(defun %stbrp-skyline-pack-rectangle (context width height)
  "Returns (values x y packed-p)"
  ;; find best position according to heuristic
  (multiple-value-bind (res-x res-y prev-link) (%stbrp-skyline-find-best-pos context width height)
    (flet ((deref (link) (if (eq link :head) (stbrp-context-active-head context) (stbrp-node-next link)))
           (set-link (link node) (if (eq link :head)
                                     (setf (stbrp-context-active-head context) node)
                                     (setf (stbrp-node-next link) node))))
      ;; bail if:
      ;;    1. it failed
      ;;    2. the best node doesn't fit (we don't always check this)
      ;;    3. we're out of memory
      (when (or (null prev-link) (> (+ res-y height) (stbrp-context-height context))
                (null (stbrp-context-free-head context)))
        (return-from %stbrp-skyline-pack-rectangle (values res-x res-y nil)))
      ;; on success, create new node
      (let ((node (stbrp-context-free-head context))
            cur)
        (setf (stbrp-node-x node) res-x
              (stbrp-node-y node) (+ res-y height))
        (setf (stbrp-context-free-head context) (stbrp-node-next node))
        ;; insert the new node into the right starting point, and
        ;; let 'cur' point to the remaining nodes needing to be
        ;; stiched back in
        (setf cur (deref prev-link))
        (if (< (stbrp-node-x cur) res-x)
            ;; preserve the existing one, so start testing with the next one
            (let ((next (stbrp-node-next cur)))
              (setf (stbrp-node-next cur) node)
              (setf cur next))
            (set-link prev-link node))
        ;; from here, traverse cur and free the nodes, until we get to one
        ;; that shouldn't be freed
        (loop while (and (stbrp-node-next cur) (<= (stbrp-node-x (stbrp-node-next cur)) (+ res-x width)))
              do (let ((next (stbrp-node-next cur)))
                   ;; move the current node to the free list
                   (setf (stbrp-node-next cur) (stbrp-context-free-head context))
                   (setf (stbrp-context-free-head context) cur)
                   (setf cur next)))
        ;; stitch the list back in
        (setf (stbrp-node-next node) cur)
        (when (< (stbrp-node-x cur) (+ res-x width))
          (setf (stbrp-node-x cur) (+ res-x width)))
        (values res-x res-y t)))))

(defun %stbrp-rect-height-compare (p q)
  "qsort comparator rect_height_compare"
  (cond ((> (stbrp-rect-h p) (stbrp-rect-h q)) -1)
        ((< (stbrp-rect-h p) (stbrp-rect-h q)) 1)
        ((> (stbrp-rect-w p) (stbrp-rect-w q)) -1)
        ((< (stbrp-rect-w p) (stbrp-rect-w q)) 1)
        (t 0)))

(defun stbrp-pack-rects (context rects)
  "Assign packed locations to rectangles (vector of stbrp-rect), returns T if all rects were packed"
  (let ((all-rects-packed t)
        (num-rects (length rects)))
    ;; we use the 'was_packed' field internally to allow sorting/unsorting
    (dotimes (i num-rects)
      (setf (stbrp-rect-was-packed (aref rects i)) i))
    ;; sort according to heuristic
    ;; NOTE: glibc qsort() is a merge sort (stable) for these sizes
    (let ((sorted (stable-sort (copy-seq rects) (lambda (a b) (< (%stbrp-rect-height-compare a b) 0)))))
      (replace rects sorted))
    (dotimes (i num-rects)
      (let ((r (aref rects i)))
        (if (or (= (stbrp-rect-w r) 0) (= (stbrp-rect-h r) 0))
            (setf (stbrp-rect-x r) 0 (stbrp-rect-y r) 0) ; empty rect needs no space
            (multiple-value-bind (x y packed) (%stbrp-skyline-pack-rectangle context (stbrp-rect-w r) (stbrp-rect-h r))
              (if packed
                  (setf (stbrp-rect-x r) x (stbrp-rect-y r) y)
                  (setf (stbrp-rect-x r) +stbrp-maxval+ (stbrp-rect-y r) +stbrp-maxval+))))))
    ;; unsort
    (let ((sorted (stable-sort (copy-seq rects) #'< :key #'stbrp-rect-was-packed)))
      (replace rects sorted))
    ;; set was_packed flags and all_rects_packed status
    (dotimes (i num-rects)
      (let ((r (aref rects i)))
        (setf (stbrp-rect-was-packed r)
              (if (and (= (stbrp-rect-x r) +stbrp-maxval+) (= (stbrp-rect-y r) +stbrp-maxval+)) 0 1))
        (when (= (stbrp-rect-was-packed r) 0)
          (setf all-rects-packed nil))))
    ;; return the all_rects_packed status
    all-rects-packed))
