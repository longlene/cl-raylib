(in-package #:cl-raylib)

;;;===================================================================================
;;; stb_image_write - image writers used by rtextures.c ExportImage()/ExportImageToMemory()
;;; Port of raylib/src/external/stb_image_write.h: PNG writer (with its zlib compressor)
;;; and BMP writer (PNG and BMP are the export formats enabled in raylib config.h)
;;;===================================================================================

(defparameter *stbi-write-png-compression-level* 8 "PNG zlib compression level (stbi_write_png_compression_level)")
(defparameter *stbi-write-force-png-filter* -1 "Forced PNG filter, -1 to pick the best one per row (stbi_write_force_png_filter)")

(deftype %octets () '(simple-array (unsigned-byte 8) (*)))

;;----------------------------------------------------------------------------------
;; zlib compressor (fixed huffman codes)
;;----------------------------------------------------------------------------------

(defconstant +stbiw-zhash+ 16384)

(defun %stbiw-zlib-bitrev (code codebits)
  (let ((res 0))
    (dotimes (k codebits res)
      (setf res (logior (ash res 1) (logand code 1))
            code (ash code -1)))))

(declaim (inline %stbiw-zhash))
(defun %stbiw-zhash (data p)
  (declare (type %octets data) (type fixnum p))
  (let ((hash (logand (+ (aref data p) (ash (aref data (+ p 1)) 8) (ash (aref data (+ p 2)) 16)) #xffffffff)))
    (setf hash (logxor hash (logand (ash hash 3) #xffffffff)))
    (setf hash (logand (+ hash (ash hash -5)) #xffffffff))
    (setf hash (logxor hash (logand (ash hash 4) #xffffffff)))
    (setf hash (logand (+ hash (ash hash -17)) #xffffffff))
    (setf hash (logxor hash (logand (ash hash 25) #xffffffff)))
    (setf hash (logand (+ hash (ash hash -6)) #xffffffff))
    hash))

(defun %stbiw-zlib-countm (data a b limit)
  (declare (type %octets data) (type fixnum a b limit))
  (let ((i 0))
    (declare (type fixnum i))
    (loop while (and (< i limit) (< i 258) (= (aref data (+ a i)) (aref data (+ b i))))
          do (incf i))
    i))

(alexandria:define-constant +stbiw-lengthc+ #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 259)
  :test #'equalp)
(alexandria:define-constant +stbiw-lengtheb+ #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0) :test #'equalp)
(alexandria:define-constant +stbiw-distc+ #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049 3073 4097
                                            6145 8193 12289 16385 24577 32768)
  :test #'equalp)
(alexandria:define-constant +stbiw-disteb+ #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13) :test #'equalp)

(defun stbi-zlib-compress (data data-len quality)
  "zlib stream of DATA (stbi_zlib_compress())"
  (declare (type %octets data) (type fixnum data-len quality))
  (let ((out (make-array (+ 64 (floor data-len 2)) :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (bitbuf 0) (bitcount 0)
        ;; hash chains: positions in DATA
        (hash-table (make-array +stbiw-zhash+ :initial-element nil))
        (i 0))
    (declare (type fixnum bitbuf bitcount i))
    (when (< quality 5) (setf quality 5))
    (labels ((push-byte (b) (vector-push-extend b out))
             (zlib-add (code codebits)
               (setf bitbuf (logior bitbuf (ash code bitcount)))
               (incf bitcount codebits)
               (loop while (>= bitcount 8)
                     do (push-byte (logand bitbuf #xff))
                        (setf bitbuf (ash bitbuf -8))
                        (decf bitcount 8)))
             (huffa (b c) (zlib-add (%stbiw-zlib-bitrev b c) c))
             (huff (n)
               (cond ((<= n 143) (huffa (+ #x30 n) 8))
                     ((<= n 255) (huffa (+ #x190 (- n 144)) 9))
                     ((<= n 279) (huffa (- n 256) 7))
                     (t (huffa (+ #xc0 (- n 280)) 8))))
             (huffb (n) (if (<= n 143) (huffa (+ #x30 n) 8) (huffa (+ #x190 (- n 144)) 9)))
             (hlist (h)
               (or (svref hash-table h)
                   (setf (svref hash-table h) (make-array (1+ quality) :element-type 'fixnum :adjustable t :fill-pointer 0)))))
      (push-byte #x78)                  ; DEFLATE 32K window
      (push-byte #x5e)                  ; FLEVEL = 1
      (zlib-add 1 1)                    ; BFINAL = 1
      (zlib-add 1 2)                    ; BTYPE = 1 -- fixed huffman

      (loop while (< i (- data-len 3))
            do (let* ((h (logand (%stbiw-zhash data i) (1- +stbiw-zhash+)))
                      (best 3)
                      (bestloc -1)
                      (list (svref hash-table h)))
                 (declare (type fixnum h best bestloc))
                 ;; hash next 3 bytes of data to be compressed
                 (when list
                   (loop for loc across list
                         do (when (> loc (- i 32768)) ; if entry lies within window
                              (let ((d (%stbiw-zlib-countm data loc i (- data-len i))))
                                (when (>= d best) (setf best d bestloc loc))))))
                 ;; when hash table entry is too long, delete half the entries
                 (when (and list (= (fill-pointer list) (* 2 quality)))
                   (replace list list :start2 quality :end2 (* 2 quality))
                   (setf (fill-pointer list) quality))
                 (vector-push-extend i (hlist h))

                 (when (>= bestloc 0)
                   ;; "lazy matching" - check match at *next* byte, and if it's better, do cur byte as literal
                   (let ((list2 (svref hash-table (logand (%stbiw-zhash data (1+ i)) (1- +stbiw-zhash+)))))
                     (when list2
                       (loop for loc across list2
                             do (when (> loc (- i 32767))
                                  (let ((e (%stbiw-zlib-countm data loc (1+ i) (- data-len i 1))))
                                    (when (> e best) ; if next match is better, bail on current match
                                      (setf bestloc -1)
                                      (return))))))))

                 (if (>= bestloc 0)
                     (let ((d (- i bestloc))    ; distance back
                           (j 0))
                       (loop while (> best (1- (svref +stbiw-lengthc+ (1+ j)))) do (incf j))
                       (huff (+ j 257))
                       (when (/= (svref +stbiw-lengtheb+ j) 0) (zlib-add (- best (svref +stbiw-lengthc+ j)) (svref +stbiw-lengtheb+ j)))
                       (setf j 0)
                       (loop while (> d (1- (svref +stbiw-distc+ (1+ j)))) do (incf j))
                       (zlib-add (%stbiw-zlib-bitrev j 5) 5)
                       (when (/= (svref +stbiw-disteb+ j) 0) (zlib-add (- d (svref +stbiw-distc+ j)) (svref +stbiw-disteb+ j)))
                       (incf i best))
                     (progn
                       (huffb (aref data i))
                       (incf i)))))
      ;; write out final bytes
      (loop while (< i data-len) do (huffb (aref data i)) (incf i))
      (huff 256)                        ; end of block
      ;; pad with 0 bits to byte boundary
      (loop while (/= bitcount 0) do (zlib-add 0 1))

      ;; store uncompressed instead if compression was worse
      (when (> (fill-pointer out) (+ data-len 2 (* (floor (+ data-len 32766) 32767) 5)))
        (setf (fill-pointer out) 2)     ; truncate to DEFLATE 32K window and FLEVEL = 1
        (let ((j 0))
          (loop while (< j data-len)
                do (let ((blocklen (min (- data-len j) 32767)))
                     (push-byte (if (= (- data-len j) blocklen) 1 0)) ; BFINAL = ?, BTYPE = 0 -- no compression
                     (push-byte (logand blocklen #xff)) ; LEN
                     (push-byte (logand (ash blocklen -8) #xff))
                     (push-byte (logand (lognot blocklen) #xff)) ; NLEN
                     (push-byte (logand (ash (lognot blocklen) -8) #xff))
                     (loop for k from j below (+ j blocklen) do (push-byte (aref data k)))
                     (incf j blocklen)))))

      ;; compute adler32 on input
      (let ((s1 1) (s2 0) (blocklen (mod data-len 5552)) (j 0))
        (loop while (< j data-len)
              do (dotimes (k blocklen)
                   (setf s1 (+ s1 (aref data (+ j k))) s2 (+ s2 s1)))
                 (setf s1 (mod s1 65521) s2 (mod s2 65521))
                 (incf j blocklen)
                 (setf blocklen 5552))
        (push-byte (logand (ash s2 -8) #xff))
        (push-byte (logand s2 #xff))
        (push-byte (logand (ash s1 -8) #xff))
        (push-byte (logand s1 #xff)))
      (coerce out '%octets))))

;;----------------------------------------------------------------------------------
;; PNG writer
;;----------------------------------------------------------------------------------

(defun %stbiw-crc32 (buffer start len)
  (let ((crc #xffffffff)
        (crc-table (load-time-value
                    (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
                      (dotimes (n 256 table)
                        (let ((c n))
                          (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
                          (setf (aref table n) c)))))))
    (loop for i from start below (+ start len)
          do (setf crc (logxor (ash crc -8) (aref crc-table (logand (logxor (aref buffer i) crc) #xff)))))
    (logand (lognot crc) #xffffffff)))

(defun %stbiw-paeth (a b c)
  (let* ((p (- (+ a b) c)) (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) (logand a #xff))
          ((<= pb pc) (logand b #xff))
          (t (logand c #xff)))))

(defun %stbiw-encode-png-line (pixels stride-bytes width y n filter-type line-buffer)
  "Filter row Y into LINE-BUFFER (octets, read as signed char for the estimation)"
  (declare (type %octets pixels line-buffer) (type fixnum stride-bytes width y n filter-type))
  (let* ((type (svref (if (/= y 0) #(0 1 2 3 4) #(0 1 0 5 6)) filter-type))
         (z (* stride-bytes y))
         (count (* width n)))
    (declare (type fixnum type z count))
    (macrolet ((px (k) `(aref pixels (+ z ,k)))
               (up (k) `(aref pixels (- (+ z ,k) stride-bytes)))
               (out (k v) `(setf (aref line-buffer ,k) (logand ,v #xff))))
      (if (= type 0)
          (replace line-buffer pixels :start2 z :end2 (+ z count))
          (progn
            ;; first loop isn't optimized since it's just one pixel
            (dotimes (i n)
              (case type
                ((1 5 6) (out i (px i)))
                (2 (out i (- (px i) (up i))))
                (3 (out i (- (px i) (ash (up i) -1))))
                (4 (out i (- (px i) (%stbiw-paeth 0 (up i) 0))))))
            (case type
              (1 (loop for i from n below count do (out i (- (px i) (px (- i n))))))
              (2 (loop for i from n below count do (out i (- (px i) (up i)))))
              (3 (loop for i from n below count do (out i (- (px i) (ash (+ (px (- i n)) (up i)) -1)))))
              (4 (loop for i from n below count do (out i (- (px i) (%stbiw-paeth (px (- i n)) (up i) (up (- i n)))))))
              (5 (loop for i from n below count do (out i (- (px i) (ash (px (- i n)) -1)))))
              (6 (loop for i from n below count do (out i (- (px i) (%stbiw-paeth (px (- i n)) 0 0)))))))))))

(defun stbi-write-png-to-mem (pixels stride-bytes x y n)
  "PNG file data of a 8 bit image with N components (stbi_write_png_to_mem())"
  (let* ((pixels (coerce pixels '%octets))
         (force-filter *stbi-write-force-png-filter*)
         (stride-bytes (if (zerop stride-bytes) (* x n) stride-bytes))
         (row (1+ (* x n)))
         (filt (make-array (* row y) :element-type '(unsigned-byte 8)))
         (line-buffer (make-array (* x n) :element-type '(unsigned-byte 8))))
    (when (>= force-filter 5) (setf force-filter -1))
    (dotimes (j y)
      (let ((filter-type 0))
        (if (> force-filter -1)
            (progn
              (setf filter-type force-filter)
              (%stbiw-encode-png-line pixels stride-bytes x j n force-filter line-buffer))
            ;; Estimate the best filter by running through all of them
            (let ((best-filter 0) (best-filter-val #x7fffffff))
              (dotimes (ft 5)
                (%stbiw-encode-png-line pixels stride-bytes x j n ft line-buffer)
                ;; Estimate the entropy of the line using this filter; the less, the better
                (let ((est (loop for v across line-buffer sum (abs (if (>= v 128) (- v 256) v)))))
                  (when (< est best-filter-val)
                    (setf best-filter-val est best-filter ft))))
              ;; If the last iteration already got us the best filter, don't redo it (never with 5 filters)
              (%stbiw-encode-png-line pixels stride-bytes x j n best-filter line-buffer)
              (setf filter-type best-filter)))
        ;; when we get here, filter_type contains the filter type, and line_buffer contains the data
        (setf (aref filt (* j row)) filter-type)
        (replace filt line-buffer :start1 (1+ (* j row)))))
    (let* ((zlib (stbi-zlib-compress filt (* y row) *stbi-write-png-compression-level*))
           (zlen (length zlib))
           (out (make-array (+ 8 12 13 12 zlen 12) :element-type '(unsigned-byte 8)))
           (o 0))
      (labels ((put (b) (setf (aref out o) (logand b #xff)) (incf o))
               (wp32 (v) (put (ash v -24)) (put (ash v -16)) (put (ash v -8)) (put v))
               (wptag (s) (loop for c across s do (put (char-code c))))
               (wpcrc (len) (wp32 (%stbiw-crc32 out (- o len 4) (+ len 4)))))
        (dolist (b '(137 80 78 71 13 10 26 10)) (put b))
        (wp32 13)                       ; header length
        (wptag "IHDR")
        (wp32 x)
        (wp32 y)
        (put 8)
        (put (svref #(-1 0 4 2 6) n))
        (put 0) (put 0) (put 0)
        (wpcrc 13)

        (wp32 zlen)
        (wptag "IDAT")
        (replace out zlib :start1 o)
        (incf o zlen)
        (wpcrc zlen)

        (wp32 0)
        (wptag "IEND")
        (wpcrc 0))
      out)))

;;----------------------------------------------------------------------------------
;; BMP writer
;;----------------------------------------------------------------------------------

(defun stbi-write-bmp-to-mem (x y comp data)
  "BMP file data (stbi_write_bmp()): 24 bpp, or 32 bpp with a V4 header for 4 components"
  (let ((out (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((w1 (v) (vector-push-extend (logand v #xff) out))
             (w2 (v) (w1 v) (w1 (ash v -8)))
             (w4 (v) (w2 v) (w2 (ash v -16)))
             (write-pixels (write-alpha pad)
               ;; rgb_dir -1, vdir -1: bottom-up BGR rows, monochrome expanded
               (loop for j downfrom (1- y) to 0
                     do (dotimes (i x)
                          (let ((d (* (+ (* j x) i) comp)))
                            (case comp
                              ((1 2) (w1 (aref data d)) (w1 (aref data d)) (w1 (aref data d)))
                              ((3 4) (w1 (aref data (+ d 2))) (w1 (aref data (+ d 1))) (w1 (aref data d))))
                            (when (> write-alpha 0) (w1 (aref data (+ d comp -1))))))
                        (dotimes (k pad) (w1 0)))))
      (when (or (< y 0) (< x 0)) (return-from stbi-write-bmp-to-mem nil))
      (if (/= comp 4)
          ;; write RGB bitmap
          (let ((pad (logand (- (* x 3)) 3)))
            (w1 (char-code #\B)) (w1 (char-code #\M)) (w4 (+ 14 40 (* (+ (* x 3) pad) y))) (w2 0) (w2 0) (w4 (+ 14 40)) ; file header
            (w4 40) (w4 x) (w4 y) (w2 1) (w2 24) (dotimes (k 6) (w4 0))                                                   ; bitmap header
            (write-pixels 0 pad))
          ;; RGBA bitmaps need a v4 header
          ;; use BI_BITFIELDS mode with 32bpp and alpha mask
          (progn
            (w1 (char-code #\B)) (w1 (char-code #\M)) (w4 (+ 14 108 (* x y 4))) (w2 0) (w2 0) (w4 (+ 14 108)) ; file header
            (w4 108) (w4 x) (w4 y) (w2 1) (w2 32) (w4 3) (dotimes (k 5) (w4 0))                               ; bitmap V4 header
            (w4 #xff0000) (w4 #xff00) (w4 #xff) (w4 #xff000000)
            (dotimes (k 13) (w4 0))
            (write-pixels 1 0)))
      (coerce out '%octets))))

;;----------------------------------------------------------------------------------
;; TGA writer
;;----------------------------------------------------------------------------------

(defparameter *stbi-write-tga-with-rle* t "Write RLE compressed TGA files (stbi_write_tga_with_rle)")

(defun stbi-write-tga-to-mem (x y comp data)
  "TGA file data (stbi_write_tga()): bottom-up rows, RLE compressed by default"
  (let* ((out (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (has-alpha (if (or (= comp 2) (= comp 4)) 1 0))
         (colorbytes (if (= has-alpha 1) (1- comp) comp))
         (format (if (< colorbytes 2) 3 2)))   ; 3 color channels (RGB/RGBA) = 2, 1 color channel (Y/YA) = 3
    (labels ((w1 (v) (vector-push-extend (logand v #xff) out))
             (w2 (v) (w1 v) (w1 (ash v -8)))
             (write-pixel (d)
               ;; stbiw__write_pixel() with rgb_dir -1, write_alpha has_alpha, no mono expansion
               (case comp
                 ((1 2) (w1 (aref data d)))
                 ((3 4) (w1 (aref data (+ d 2))) (w1 (aref data (+ d 1))) (w1 (aref data d))))
               (when (= has-alpha 1) (w1 (aref data (+ d comp -1)))))
             (pixel= (a b)
               (loop for k below comp always (= (aref data (+ a k)) (aref data (+ b k)))))
             (header (image-type)
               (w1 0) (w1 0) (w1 image-type) (w2 0) (w2 0) (w1 0) (w2 0) (w2 0) (w2 x) (w2 y)
               (w1 (* (+ colorbytes has-alpha) 8)) (w1 (* has-alpha 8))))
      (when (or (< y 0) (< x 0)) (return-from stbi-write-tga-to-mem nil))
      (if (not *stbi-write-tga-with-rle*)
          (progn
            (header format)
            (loop for j downfrom (1- y) to 0
                  do (dotimes (i x) (write-pixel (* (+ (* j x) i) comp)))))
          (progn
            (header (+ format 8))
            (loop for j downfrom (1- y) to 0
                  do (let ((row (* j x comp)) (len 0))
                       (do ((i 0 (+ i len))) ((>= i x))
                         (let ((begin (+ row (* i comp)))
                               (diff t))
                           (setf len 1)
                           (when (< i (1- x))
                             (incf len)
                             (setf diff (not (pixel= begin (+ row (* (1+ i) comp)))))
                             (if diff
                                 (let ((prev begin))
                                   (loop for k from (+ i 2) below x
                                         while (< len 128)
                                         do (if (not (pixel= prev (+ row (* k comp))))
                                                (progn (incf prev comp) (incf len))
                                                (progn (decf len) (return)))))
                                 (loop for k from (+ i 2) below x
                                       while (< len 128)
                                       do (if (pixel= begin (+ row (* k comp)))
                                              (incf len)
                                              (return)))))
                           (if diff
                               (progn
                                 (w1 (1- len))
                                 (dotimes (k len) (write-pixel (+ begin (* k comp)))))
                               (progn
                                 (w1 (- len 129))
                                 (write-pixel begin)))))))))
      (coerce out '%octets))))

;;----------------------------------------------------------------------------------
;; JPEG writer
;; This is based on Jon Olick's jo_jpeg.cpp:
;; public domain Simple, Minimalistic JPEG writer - http://www.jonolick.com/code.html
;; NOTE: Single float arithmetic in the C evaluation order, output matches C built without FMA contraction
;;----------------------------------------------------------------------------------

(alexandria:define-constant +stbiw-jpg-zigzag+
  (coerce #(0 1 5 6 14 15 27 28 2 4 7 13 16 26 29 42 3 8 12 17 25 30 41 43 9 11 18
            24 31 40 44 53 10 19 23 32 39 45 52 54 20 22 33 38 46 51 55 60 21 34 37 47 50 56 59 61 35 36 48 49 57 58 62 63)
          '(simple-array (unsigned-byte 8) (*)))
  :test #'equalp)

(defun %stbiw-jpg-huffman-table (pairs)
  "256 entry Huffman table (code, length) from the leading PAIRS, the rest {0,0}"
  (let ((table (make-array '(256 2) :element-type '(unsigned-byte 16) :initial-element 0)))
    (loop for (code len) in pairs
          for i from 0
          do (setf (aref table i 0) code (aref table i 1) len))
    table))

(defparameter *stbiw-ydc-ht*
  (%stbiw-jpg-huffman-table '((0 2) (2 3) (3 3) (4 3) (5 3) (6 3) (14 4) (30 5) (62 6) (126 7) (254 8) (510 9))))
(defparameter *stbiw-uvdc-ht*
  (%stbiw-jpg-huffman-table '((0 2) (1 2) (2 2) (6 3) (14 4) (30 5) (62 6) (126 7) (254 8) (510 9) (1022 10) (2046 11))))
(defparameter *stbiw-yac-ht*
  (%stbiw-jpg-huffman-table
   '((10 4) (0 2) (1 2) (4 3) (11 4) (26 5) (120 7) (248 8) (1014 10) (65410 16) (65411 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (12 4) (27 5) (121 7) (502 9) (2038 11) (65412 16) (65413 16) (65414 16) (65415 16) (65416 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (28 5) (249 8) (1015 10) (4084 12) (65417 16) (65418 16) (65419 16) (65420 16) (65421 16) (65422 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (58 6) (503 9) (4085 12) (65423 16) (65424 16) (65425 16) (65426 16) (65427 16) (65428 16) (65429 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (59 6) (1016 10) (65430 16) (65431 16) (65432 16) (65433 16) (65434 16) (65435 16) (65436 16) (65437 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (122 7) (2039 11) (65438 16) (65439 16) (65440 16) (65441 16) (65442 16) (65443 16) (65444 16) (65445 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (123 7) (4086 12) (65446 16) (65447 16) (65448 16) (65449 16) (65450 16) (65451 16) (65452 16) (65453 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (250 8) (4087 12) (65454 16) (65455 16) (65456 16) (65457 16) (65458 16) (65459 16) (65460 16) (65461 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (504 9) (32704 15) (65462 16) (65463 16) (65464 16) (65465 16) (65466 16) (65467 16) (65468 16) (65469 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (505 9) (65470 16) (65471 16) (65472 16) (65473 16) (65474 16) (65475 16) (65476 16) (65477 16) (65478 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (506 9) (65479 16) (65480 16) (65481 16) (65482 16) (65483 16) (65484 16) (65485 16) (65486 16) (65487 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (1017 10) (65488 16) (65489 16) (65490 16) (65491 16) (65492 16) (65493 16) (65494 16) (65495 16) (65496 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (1018 10) (65497 16) (65498 16) (65499 16) (65500 16) (65501 16) (65502 16) (65503 16) (65504 16) (65505 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (2040 11) (65506 16) (65507 16) (65508 16) (65509 16) (65510 16) (65511 16) (65512 16) (65513 16) (65514 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (65515 16) (65516 16) (65517 16) (65518 16) (65519 16) (65520 16) (65521 16) (65522 16) (65523 16) (65524 16) (0 0) (0 0) (0 0) (0 0) (0 0)
     (2041 11) (65525 16) (65526 16) (65527 16) (65528 16) (65529 16) (65530 16) (65531 16) (65532 16) (65533 16) (65534 16) (0 0) (0 0) (0 0) (0 0) (0 0))))
(defparameter *stbiw-uvac-ht*
  (%stbiw-jpg-huffman-table
   '((0 2) (1 2) (4 3) (10 4) (24 5) (25 5) (56 6) (120 7) (500 9) (1014 10) (4084 12) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (11 4) (57 6) (246 8) (501 9) (2038 11) (4085 12) (65416 16) (65417 16) (65418 16) (65419 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (26 5) (247 8) (1015 10) (4086 12) (32706 15) (65420 16) (65421 16) (65422 16) (65423 16) (65424 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (27 5) (248 8) (1016 10) (4087 12) (65425 16) (65426 16) (65427 16) (65428 16) (65429 16) (65430 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (58 6) (502 9) (65431 16) (65432 16) (65433 16) (65434 16) (65435 16) (65436 16) (65437 16) (65438 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (59 6) (1017 10) (65439 16) (65440 16) (65441 16) (65442 16) (65443 16) (65444 16) (65445 16) (65446 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (121 7) (2039 11) (65447 16) (65448 16) (65449 16) (65450 16) (65451 16) (65452 16) (65453 16) (65454 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (122 7) (2040 11) (65455 16) (65456 16) (65457 16) (65458 16) (65459 16) (65460 16) (65461 16) (65462 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (249 8) (65463 16) (65464 16) (65465 16) (65466 16) (65467 16) (65468 16) (65469 16) (65470 16) (65471 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (503 9) (65472 16) (65473 16) (65474 16) (65475 16) (65476 16) (65477 16) (65478 16) (65479 16) (65480 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (504 9) (65481 16) (65482 16) (65483 16) (65484 16) (65485 16) (65486 16) (65487 16) (65488 16) (65489 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (505 9) (65490 16) (65491 16) (65492 16) (65493 16) (65494 16) (65495 16) (65496 16) (65497 16) (65498 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (506 9) (65499 16) (65500 16) (65501 16) (65502 16) (65503 16) (65504 16) (65505 16) (65506 16) (65507 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (2041 11) (65508 16) (65509 16) (65510 16) (65511 16) (65512 16) (65513 16) (65514 16) (65515 16) (65516 16) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
     (16352 14) (65517 16) (65518 16) (65519 16) (65520 16) (65521 16) (65522 16) (65523 16) (65524 16) (65525 16) (0 0) (0 0) (0 0) (0 0) (0 0)
     (1018 10) (32707 15) (65526 16) (65527 16) (65528 16) (65529 16) (65530 16) (65531 16) (65532 16) (65533 16) (65534 16) (0 0) (0 0) (0 0) (0 0) (0 0))))

(defun %stbiw-jpg-dct (d i0 stride)
  "In place 8 point DCT of D[i0], D[i0+stride], ..., D[i0+7*stride] (stbiw__jpg_DCT)"
  (declare (type (simple-array single-float (*)) d) (type fixnum i0 stride))
  (macrolet ((at (k) `(aref d (+ i0 (* ,k stride)))))
    (let* ((d0 (at 0)) (d1 (at 1)) (d2 (at 2)) (d3 (at 3)) (d4 (at 4)) (d5 (at 5)) (d6 (at 6)) (d7 (at 7))
           (tmp0 (+ d0 d7)) (tmp7 (- d0 d7))
           (tmp1 (+ d1 d6)) (tmp6 (- d1 d6))
           (tmp2 (+ d2 d5)) (tmp5 (- d2 d5))
           (tmp3 (+ d3 d4)) (tmp4 (- d3 d4))
           ;; Even part
           (tmp10 (+ tmp0 tmp3))       ; phase 2
           (tmp13 (- tmp0 tmp3))
           (tmp11 (+ tmp1 tmp2))
           (tmp12 (- tmp1 tmp2)))
      (declare (type single-float d0 d1 d2 d3 d4 d5 d6 d7 tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp10 tmp11 tmp12 tmp13))
      (setf d0 (+ tmp10 tmp11)         ; phase 3
            d4 (- tmp10 tmp11))
      (let ((z1 (* (+ tmp12 tmp13) 0.707106781f0))) ; c4
        (setf d2 (+ tmp13 z1)          ; phase 5
              d6 (- tmp13 z1)))
      ;; Odd part
      (setf tmp10 (+ tmp4 tmp5)        ; phase 2
            tmp11 (+ tmp5 tmp6)
            tmp12 (+ tmp6 tmp7))
      ;; The rotator is modified from fig 4-8 to avoid extra negations.
      (let* ((z5 (* (- tmp10 tmp12) 0.382683433f0)) ; c6
             (z2 (+ (* tmp10 0.541196100f0) z5))    ; c2-c6
             (z4 (+ (* tmp12 1.306562965f0) z5))    ; c2+c6
             (z3 (* tmp11 0.707106781f0))           ; c4
             (z11 (+ tmp7 z3))                      ; phase 5
             (z13 (- tmp7 z3)))
        (setf (at 5) (+ z13 z2)                     ; phase 6
              (at 3) (- z13 z2)
              (at 1) (+ z11 z4)
              (at 7) (- z11 z4)))
      (setf (at 0) d0 (at 2) d2 (at 4) d4 (at 6) d6))))

(defun %stbiw-jpg-calc-bits (val)
  "Returns the (bits, length) pair for VAL (stbiw__jpg_calcBits)"
  (let* ((tmp1 (abs val))
         (val (if (< val 0) (1- val) val))
         (len 1))
    (loop while (> (setf tmp1 (ash tmp1 -1)) 0) do (incf len))
    (values (logand val (1- (ash 1 len))) len)))

(defun stbi-write-jpg-to-mem (width height comp data quality)
  "JPEG file data (stbi_write_jpg()), QUALITY between 1 and 100"
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (bit-buf 0) (bit-cnt 0))
    (labels ((putc (c) (vector-push-extend c out))
             (write-bits (code len)
               (incf bit-cnt len)
               (setf bit-buf (logand (logior bit-buf (ash code (- 24 bit-cnt))) #xffffff))
               (loop while (>= bit-cnt 8)
                     do (let ((c (logand (ash bit-buf -16) 255)))
                          (putc c)
                          (when (= c 255) (putc 0))
                          (setf bit-buf (logand (ash bit-buf 8) #xffffff))
                          (decf bit-cnt 8))))
             (write-ht (ht i) (write-bits (aref ht i 0) (aref ht i 1)))
             (process-du (cdu offset du-stride fdtbl dc htdc htac)
               (let ((du (make-array 64 :element-type 'fixnum)))
                 ;; DCT rows
                 (loop for data-off from offset below (+ offset (* du-stride 8)) by du-stride
                       do (%stbiw-jpg-dct cdu data-off 1))
                 ;; DCT columns
                 (loop for data-off from offset below (+ offset 8)
                       do (%stbiw-jpg-dct cdu data-off du-stride))
                 ;; Quantize/descale/zigzag the coefficients
                 (let ((j 0))
                   (dotimes (y 8)
                     (dotimes (x 8)
                       (let ((v (* (aref cdu (+ offset (* y du-stride) x)) (aref fdtbl j))))
                         (declare (type single-float v))
                         (setf (aref du (aref +stbiw-jpg-zigzag+ j))
                               (truncate (if (< v 0) (- v 0.5f0) (+ v 0.5f0)))))
                       (incf j))))
                 ;; Encode DC
                 (let ((diff (- (aref du 0) dc)))
                   (if (= diff 0)
                       (write-ht htdc 0)
                       (multiple-value-bind (bits len) (%stbiw-jpg-calc-bits diff)
                         (write-ht htdc len)
                         (write-bits bits len))))
                 ;; Encode ACs
                 (let ((end0pos 63))
                   (loop while (and (> end0pos 0) (= (aref du end0pos) 0)) do (decf end0pos))
                   ;; end0pos = first element in reverse order !=0
                   (when (= end0pos 0)
                     (write-ht htac #x00)    ; EOB
                     (return-from process-du (aref du 0)))
                   (let ((i 1))
                     (loop while (<= i end0pos)
                           do (let ((startpos i))
                                (loop while (and (= (aref du i) 0) (<= i end0pos)) do (incf i))
                                (let ((nrzeroes (- i startpos)))
                                  (when (>= nrzeroes 16)
                                    (dotimes (nrmarker (ash nrzeroes -4))
                                      (write-ht htac #xf0)) ; M16zeroes
                                    (setf nrzeroes (logand nrzeroes 15)))
                                  (multiple-value-bind (bits len) (%stbiw-jpg-calc-bits (aref du i))
                                    (write-ht htac (+ (ash nrzeroes 4) len))
                                    (write-bits bits len))))
                              (incf i)))
                   (when (/= end0pos 63)
                     (write-ht htac #x00)))  ; EOB
                 (aref du 0))))
      (when (or (null data) (zerop width) (zerop height) (> comp 4) (< comp 1))
        (return-from stbi-write-jpg-to-mem nil))
      (let* ((quality (if (zerop quality) 90 quality))
             (subsample (<= quality 90))
             (quality (max 1 (min quality 100)))
             (quality (if (< quality 50) (floor 5000 quality) (- 200 (* quality 2))))
             (yqt #(16 11 10 16 24 40 51 61 12 12 14 19 26 58 60 55 14 13 16 24 40 57 69 56 14 17 22 29 51 87 80 62 18 22
                    37 56 68 109 103 77 24 35 55 64 81 104 113 92 49 64 78 87 103 121 120 101 72 92 95 98 112 100 103 99))
             (uvqt #(17 18 24 47 99 99 99 99 18 21 26 66 99 99 99 99 24 26 56 99 99 99 99 99 47 66 99 99 99 99 99 99
                     99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99 99))
             (aasf (map '(simple-array single-float (*)) (lambda (f) (* f 2.828427125f0))
                        '(1.0f0 1.387039845f0 1.306562965f0 1.175875602f0 1.0f0 0.785694958f0 0.541196100f0 0.275899379f0)))
             (ytable (make-array 64 :element-type '(unsigned-byte 8)))
             (uvtable (make-array 64 :element-type '(unsigned-byte 8)))
             (fdtbl-y (make-array 64 :element-type 'single-float))
             (fdtbl-uv (make-array 64 :element-type 'single-float)))
        (dotimes (i 64)
          (let ((yti (floor (+ (* (aref yqt i) quality) 50) 100))
                (uvti (floor (+ (* (aref uvqt i) quality) 50) 100)))
            (setf (aref ytable (aref +stbiw-jpg-zigzag+ i)) (max 1 (min yti 255))
                  (aref uvtable (aref +stbiw-jpg-zigzag+ i)) (max 1 (min uvti 255)))))
        (let ((k 0))
          (dotimes (row 8)
            (dotimes (col 8)
              (setf (aref fdtbl-y k) (/ 1.0f0 (* (* (float (aref ytable (aref +stbiw-jpg-zigzag+ k)) 1.0f0) (aref aasf row)) (aref aasf col)))
                    (aref fdtbl-uv k) (/ 1.0f0 (* (* (float (aref uvtable (aref +stbiw-jpg-zigzag+ k)) 1.0f0) (aref aasf row)) (aref aasf col))))
              (incf k))))
        ;; Write Headers
        (flet ((put-all (bytes) (map nil #'putc bytes)))
          (put-all #(#xFF #xD8 #xFF #xE0 0 #x10 74 70 73 70 0 1 1 0 0 1 0 1 0 0 #xFF #xDB 0 #x84 0)) ; head0 ("JFIF")
          (put-all ytable)
          (putc 1)
          (put-all uvtable)
          (put-all (vector #xFF #xC0 0 #x11 8 (logand (ash height -8) 255) (logand height 255)
                           (logand (ash width -8) 255) (logand width 255)
                           3 1 (if subsample #x22 #x11) 0 2 #x11 1 3 #x11 1 #xFF #xC4 #x01 #xA2 0)) ; head1
          (put-all #(0 1 5 1 1 1 1 1 1 0 0 0 0 0 0 0))                    ; std_dc_luminance_nrcodes
          (put-all #(0 1 2 3 4 5 6 7 8 9 10 11))                           ; std_dc_luminance_values
          (putc #x10)                                                      ; HTYACinfo
          (put-all #(0 2 1 3 3 2 4 3 5 5 4 4 0 0 1 #x7d))                  ; std_ac_luminance_nrcodes
          (put-all #(#x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12 #x21 #x31 #x41 #x06 #x13 #x51 #x61 #x07 #x22 #x71 #x14 #x32 #x81 #x91 #xa1 #x08
                     #x23 #x42 #xb1 #xc1 #x15 #x52 #xd1 #xf0 #x24 #x33 #x62 #x72 #x82 #x09 #x0a #x16 #x17 #x18 #x19 #x1a #x25 #x26 #x27 #x28
                     #x29 #x2a #x34 #x35 #x36 #x37 #x38 #x39 #x3a #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x53 #x54 #x55 #x56 #x57 #x58 #x59
                     #x5a #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6a #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7a #x83 #x84 #x85 #x86 #x87 #x88 #x89
                     #x8a #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9a #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 #xa8 #xa9 #xaa #xb2 #xb3 #xb4 #xb5 #xb6
                     #xb7 #xb8 #xb9 #xba #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 #xd8 #xd9 #xda #xe1 #xe2
                     #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8 #xf9 #xfa)) ; std_ac_luminance_values
          (putc 1)                                                         ; HTUDCinfo
          (put-all #(0 3 1 1 1 1 1 1 1 1 1 0 0 0 0 0))                     ; std_dc_chrominance_nrcodes
          (put-all #(0 1 2 3 4 5 6 7 8 9 10 11))                           ; std_dc_chrominance_values
          (putc #x11)                                                      ; HTUACinfo
          (put-all #(0 2 1 2 4 4 3 4 7 5 4 4 0 1 2 #x77))                  ; std_ac_chrominance_nrcodes
          (put-all #(#x00 #x01 #x02 #x03 #x11 #x04 #x05 #x21 #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71 #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91
                     #xa1 #xb1 #xc1 #x09 #x23 #x33 #x52 #xf0 #x15 #x62 #x72 #xd1 #x0a #x16 #x24 #x34 #xe1 #x25 #xf1 #x17 #x18 #x19 #x1a #x26
                     #x27 #x28 #x29 #x2a #x35 #x36 #x37 #x38 #x39 #x3a #x43 #x44 #x45 #x46 #x47 #x48 #x49 #x4a #x53 #x54 #x55 #x56 #x57 #x58
                     #x59 #x5a #x63 #x64 #x65 #x66 #x67 #x68 #x69 #x6a #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7a #x82 #x83 #x84 #x85 #x86 #x87
                     #x88 #x89 #x8a #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9a #xa2 #xa3 #xa4 #xa5 #xa6 #xa7 #xa8 #xa9 #xaa #xb2 #xb3 #xb4
                     #xb5 #xb6 #xb7 #xb8 #xb9 #xba #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xd2 #xd3 #xd4 #xd5 #xd6 #xd7 #xd8 #xd9 #xda
                     #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8 #xf9 #xfa)) ; std_ac_chrominance_values
          (put-all #(#xFF #xDA 0 #xC 3 1 0 2 #x11 3 #x11 0 #x3F 0)))       ; head2
        ;; Encode 8x8 macroblocks
        (let ((dcy 0) (dcu 0) (dcv 0)
              ;; comp == 2 is grey+alpha (alpha is ignored)
              (ofs-g (if (> comp 2) 1 0)) (ofs-b (if (> comp 2) 2 0))
              (block-size (if subsample 16 8)))
          (let ((ys (make-array (* block-size block-size) :element-type 'single-float))
                (us (make-array (* block-size block-size) :element-type 'single-float))
                (vs (make-array (* block-size block-size) :element-type 'single-float))
                (sub-u (make-array 64 :element-type 'single-float))
                (sub-v (make-array 64 :element-type 'single-float)))
            (loop for y from 0 below height by block-size
                  do (loop for x from 0 below width by block-size
                           do (let ((pos 0))
                                (loop for row from y below (+ y block-size)
                                      do (let* ((clamped-row (if (< row height) row (1- height))) ; row >= height => use last input row
                                                (base-p (* clamped-row width comp)))
                                           (loop for col from x below (+ x block-size)
                                                 do (let* ((p (+ base-p (* (if (< col width) col (1- width)) comp))) ; col >= width => last column
                                                           (r (float (aref data p) 1.0f0))
                                                           (g (float (aref data (+ p ofs-g)) 1.0f0))
                                                           (b (float (aref data (+ p ofs-b)) 1.0f0)))
                                                      (setf (aref ys pos) (- (+ (+ (* 0.29900f0 r) (* 0.58700f0 g)) (* 0.11400f0 b)) 128)
                                                            (aref us pos) (+ (- (* -0.16874f0 r) (* 0.33126f0 g)) (* 0.50000f0 b))
                                                            (aref vs pos) (- (- (* 0.50000f0 r) (* 0.41869f0 g)) (* 0.08131f0 b)))
                                                      (incf pos)))))
                              (if subsample
                                  (progn
                                    (setf dcy (process-du ys 0 16 fdtbl-y dcy *stbiw-ydc-ht* *stbiw-yac-ht*)
                                          dcy (process-du ys 8 16 fdtbl-y dcy *stbiw-ydc-ht* *stbiw-yac-ht*)
                                          dcy (process-du ys 128 16 fdtbl-y dcy *stbiw-ydc-ht* *stbiw-yac-ht*)
                                          dcy (process-du ys 136 16 fdtbl-y dcy *stbiw-ydc-ht* *stbiw-yac-ht*))
                                    ;; subsample U,V
                                    (let ((pos 0))
                                      (dotimes (yy 8)
                                        (dotimes (xx 8)
                                          (let ((j (+ (* yy 32) (* xx 2))))
                                            (setf (aref sub-u pos) (* (+ (+ (+ (aref us j) (aref us (+ j 1))) (aref us (+ j 16))) (aref us (+ j 17))) 0.25f0)
                                                  (aref sub-v pos) (* (+ (+ (+ (aref vs j) (aref vs (+ j 1))) (aref vs (+ j 16))) (aref vs (+ j 17))) 0.25f0))
                                            (incf pos)))))
                                    (setf dcu (process-du sub-u 0 8 fdtbl-uv dcu *stbiw-uvdc-ht* *stbiw-uvac-ht*)
                                          dcv (process-du sub-v 0 8 fdtbl-uv dcv *stbiw-uvdc-ht* *stbiw-uvac-ht*)))
                                  (setf dcy (process-du ys 0 8 fdtbl-y dcy *stbiw-ydc-ht* *stbiw-yac-ht*)
                                        dcu (process-du us 0 8 fdtbl-uv dcu *stbiw-uvdc-ht* *stbiw-uvac-ht*)
                                        dcv (process-du vs 0 8 fdtbl-uv dcv *stbiw-uvdc-ht* *stbiw-uvac-ht*)))))))
          ;; Do the bit alignment of the EOI marker
          (write-bits #x7F 7)))
      ;; EOI
      (putc #xFF)
      (putc #xD9)
      (coerce out '%octets))))
