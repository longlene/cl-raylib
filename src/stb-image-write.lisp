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
