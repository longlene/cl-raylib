(in-package #:cl-raylib)

;;;===================================================================================
;;; stb_image_resize2 - image resampler used by rtextures.c ImageResize()
;;; Port of raylib/src/external/stb_image_resize2.h (v2.x), the subset reached by
;;; stbir_resize_uint8_linear(): 8-bit linear data, 1/2/3 channels or RGBA (non premultiplied,
;;; "fancy" alpha weighting), STBIR_EDGE_CLAMP, default filters, whole image, one split.
;;;
;;; NOTE: stb_image_resize2 is built with SSE2 on x86-64; its SIMD paths are designed to be bit
;;; identical to the scalar ones (no FMA by default), so the scalar code is ported here, keeping
;;; the exact order of the float operations (accumulator lanes, coefficient order)
;;; NOTE: C pointers into the coefficient/scanline buffers are (array, index) pairs here
;;;===================================================================================

(defconstant +stbir-input-callback-padding+ 3)
(defconstant +stbir-force-gather-filter-scanlines-amount+ 32)
(defconstant +stbir-merge-runs-pixel-threshold+ 16)
(defconstant +stbir-small-float+ (scale-float 1.0 -120)) ; (float)1 / (1 << 20) / ... (6 times)
(defconstant +stbir-float-empty-marker+ 3.0e+38)
(defconstant +stbir-max-uint8-as-float+ 255.0)
(defconstant +stbir-max-uint8-as-float-inverted+ 3.9215689e-03) ; (1.0f/255.0f)

;; stbir_filter
(defconstant +stbir-filter-catmullrom+ 4)
(defconstant +stbir-filter-mitchell+ 5)
(defconstant +stbir-filter-point-sample+ 6)

(deftype %stbir-floats () '(simple-array single-float (*)))
(deftype %stbir-contribs () '(simple-array fixnum (*))) ; stbir__contributors[]: n0, n1 pairs

(defun %stbir-make-floats (n) (make-array (max n 0) :element-type 'single-float :initial-element 0.0))
(defun %stbir-make-contribs (n) (make-array (* 2 (max n 0)) :element-type 'fixnum :initial-element 0))

(declaim (inline %stbir-n0 %stbir-n1 (setf %stbir-n0) (setf %stbir-n1)))
(defun %stbir-n0 (c i) (aref (the %stbir-contribs c) (* 2 i)))
(defun %stbir-n1 (c i) (aref (the %stbir-contribs c) (1+ (* 2 i))))
(defun (setf %stbir-n0) (v c i) (setf (aref (the %stbir-contribs c) (* 2 i)) v))
(defun (setf %stbir-n1) (v c i) (setf (aref (the %stbir-contribs c) (1+ (* 2 i))) v))

(declaim (inline %stbir-floorf %stbir-ceilf))
(defun %stbir-floorf (x) (values (floor (the single-float x))))   ; (int)STBIR_FLOORF(x)
(defun %stbir-ceilf (x) (values (ceiling (the single-float x))))  ; (int)STBIR_CEILF(x)

;; stbir__sampler (with its stbir__scale_info and stbir__filter_extent_info)
(defstruct (%stbir-sampler (:conc-name %samp-))
  (contributors nil)
  (coefficients nil)
  (gather-prescatter-contributors nil)
  (gather-prescatter-coefficients nil)
  ;; scale info
  (input-full-size 0 :type fixnum)
  (output-sub-size 0 :type fixnum)
  (scale 0.0 :type single-float)
  (inv-scale 0.0 :type single-float)
  (pixel-shift 0.0 :type single-float)      ; starting shift in output pixel space (in pixels)
  (scale-is-rational nil)
  (scale-numerator 0 :type fixnum)
  (scale-denominator 0 :type fixnum)
  (filter-enum 0 :type fixnum)
  (filter-kernel nil)
  (filter-support nil)
  (coefficient-width 0 :type fixnum)
  (filter-pixel-width 0 :type fixnum)
  (filter-pixel-margin 0 :type fixnum)
  (num-contributors 0 :type fixnum)
  ;; extent info
  (lowest 0 :type fixnum)                   ; First sample index for whole filter
  (highest 0 :type fixnum)                  ; Last sample index for whole filter
  (widest 0 :type fixnum)                   ; widest single set of samples for an output
  (is-gather 0 :type fixnum)                ; 0 = scatter, 1 = gather with scale >= 1, 2 = gather with scale < 1
  (gather-prescatter-num-contributors 0 :type fixnum)
  (gather-prescatter-coefficient-width 0 :type fixnum))

;; stbir__info (with its single stbir__per_split_info)
(defstruct (%stbir-info (:conc-name %info-))
  horizontal vertical
  (input-data nil) (input-stride-bytes 0 :type fixnum)
  (output-data nil) (output-stride-bytes 0 :type fixnum)
  (ring-buffer-num-entries 0 :type fixnum)
  ;; scanline extents: conservative range and span 0 (the only span with edge clamp)
  (conservative-n0 0 :type fixnum) (conservative-n1 0 :type fixnum)
  (span-n0 0 :type fixnum) (span-n1 -1 :type fixnum) (span-pixel-offset-for-input 0 :type fixnum)
  (decode-scaled nil)                       ; decode/encode in 0..1 (scaled) or 0..255
  (alpha-weight nil) (alpha-unweight nil)   ; fancy RGBA alpha weighting
  (vertical-first nil)
  (channels 0 :type fixnum)
  (effective-channels 0 :type fixnum)
  ;; per split info
  (decode-buffer nil)
  (ring-buffer-first-scanline 0 :type fixnum)
  (ring-buffer-last-scanline 0 :type fixnum)
  (ring-buffer-begin-index 0 :type fixnum)
  (start-output-y 0 :type fixnum) (end-output-y 0 :type fixnum)
  (start-input-y 0 :type fixnum) (end-input-y 0 :type fixnum)
  (ring-buffers nil)                        ; one float array per ring buffer entry
  (vertical-buffer nil))

;;----------------------------------------------------------------------------------
;; Filters
;;----------------------------------------------------------------------------------

(defun %stbir-filter-catmullrom (x s)
  (declare (single-float x) (ignore s))
  (when (< x 0.0) (setf x (- x)))
  (cond ((< x 1.0) (- 1.0 (* x x (- 2.5 (* 1.5 x)))))
        ((< x 2.0) (- 2.0 (* x (+ 4.0 (* x (- (* 0.5 x) 2.5))))))
        (t 0.0)))

(defun %stbir-filter-mitchell (x s)
  (declare (single-float x) (ignore s))
  (when (< x 0.0) (setf x (- x)))
  (cond ((< x 1.0) (/ (+ 16.0 (* x x (- (* 21.0 x) 36.0))) 18.0))
        ((< x 2.0) (/ (+ 32.0 (* x (+ -60.0 (* x (- 36.0 (* 7.0 x)))))) 18.0))
        (t 0.0)))

(defun %stbir-filter-point (x s)
  (declare (ignore x s))
  1.0)

(defun %stbir-support-zeropoint5 (s) (declare (ignore s)) 0.5)
(defun %stbir-support-two (s) (declare (ignore s)) 2.0)

;; This is the maximum number of input samples that can affect an output sample
;; with the given filter from the output pixel's perspective
(defun %stbir-get-filter-pixel-width (support scale)
  (declare (single-float scale))
  (if (>= scale (- 1.0 +stbir-small-float+)) ; upscale
      (%stbir-ceilf (* (funcall support (/ 1.0 scale)) 2.0))
      (%stbir-ceilf (/ (* (funcall support scale) 2.0) scale))))

;; this is how many coefficents per run of the filter (which is different
;;   from the filter_pixel_width depending on if we are scattering or gathering)
(defun %stbir-get-coefficient-width (samp is-gather)
  (let ((scale (%samp-scale samp))
        (support (%samp-filter-support samp)))
    (ecase is-gather
      (1 (%stbir-ceilf (* (funcall support (/ 1.0 scale)) 2.0)))
      (2 (%stbir-ceilf (/ (* (funcall support scale) 2.0) scale)))
      (0 (%stbir-ceilf (* (funcall support scale) 2.0))))))

(defun %stbir-get-contributors (samp is-gather)
  (if (/= is-gather 0)
      (%samp-output-sub-size samp)
      (+ (%samp-input-full-size samp) (* (%samp-filter-pixel-margin samp) 2))))

;; NOTE: ImageResize always uses STBIR_EDGE_CLAMP
(declaim (inline %stbir-edge-wrap))
(defun %stbir-edge-wrap (n max)
  (cond ((< n 0) 0)
        ((>= n max) (1- max))
        (t n)))

;; get information on the extents of a sampler
(defun %stbir-get-extents (samp info)
  (let* ((min-n #x7fffffff) (max-n (- #x7fffffff))
         (min-left #x7fffffff) (max-left (- #x7fffffff))
         (min-right #x7fffffff) (max-right (- #x7fffffff))
         (contributors (%samp-contributors samp))
         (output-sub-size (%samp-output-sub-size samp))
         (input-full-size (%samp-input-full-size samp))
         (filter-pixel-margin (%samp-filter-pixel-margin samp))
         (left-margin 0) (right-margin 0)
         (stop output-sub-size))
    (let ((j 0))
      (loop while (< j stop)
            do (when (< (%stbir-n0 contributors j) min-n)
                 (setf min-n (%stbir-n0 contributors j))
                 (setf stop (+ j filter-pixel-margin)) ; if we find a new min, only scan another filter width
                 (when (> stop output-sub-size) (setf stop output-sub-size)))
               (incf j)))
    (setf stop 0)
    (let ((j (1- output-sub-size)))
      (loop while (>= j stop)
            do (when (> (%stbir-n1 contributors j) max-n)
                 (setf max-n (%stbir-n1 contributors j))
                 (setf stop (- j filter-pixel-margin)) ; if we find a new max, only scan another filter width
                 (when (< stop 0) (setf stop 0)))
               (decf j)))

    ;; now calculate how much into the margins we really read
    (when (< min-n 0)
      (setf left-margin (- min-n)
            min-n 0))
    (when (>= max-n input-full-size)
      (setf right-margin (1+ (- max-n input-full-size))
            max-n (1- input-full-size)))

    ;; index 2 is pixels read from the input
    (setf (%info-span-n0 info) min-n
          (%info-span-n1 info) max-n
          (%info-span-pixel-offset-for-input info) min-n)

    ;; convert margin pixels to the pixels within the input (min and max)
    (loop for j from (- left-margin) below 0
          do (let ((p (%stbir-edge-wrap j input-full-size)))
               (when (< p min-left) (setf min-left p))
               (when (> p max-left) (setf max-left p))))
    (loop for j from input-full-size below (+ input-full-size right-margin)
          do (let ((p (%stbir-edge-wrap j input-full-size)))
               (when (< p min-right) (setf min-right p))
               (when (> p max-right) (setf max-right p))))

    ;; merge the left margin pixel region if it connects within 4 pixels of main pixel region
    ;; NOTE: with edge clamp the margins are the edge pixels, so they always merge into span 0
    (when (/= min-left #x7fffffff)
      (when (or (and (<= min-left min-n) (>= (+ max-left +stbir-merge-runs-pixel-threshold+) min-n))
                (and (<= min-n min-left) (>= (+ max-n +stbir-merge-runs-pixel-threshold+) max-left)))
        (setf min-n (min min-n min-left)
              max-n (max max-n max-left))
        (setf (%info-span-n0 info) min-n
              (%info-span-n1 info) max-n
              (%info-span-pixel-offset-for-input info) min-n)))
    (when (/= min-right #x7fffffff)
      (when (or (and (<= min-right min-n) (>= (+ max-right +stbir-merge-runs-pixel-threshold+) min-n))
                (and (<= min-n min-right) (>= (+ max-n +stbir-merge-runs-pixel-threshold+) max-right)))
        (setf min-n (min min-n min-right)
              max-n (max max-n max-right))
        (setf (%info-span-n0 info) min-n
              (%info-span-n1 info) max-n
              (%info-span-pixel-offset-for-input info) min-n)))))

(defun %stbir-calculate-in-pixel-range (out-pixel-center out-filter-radius inv-scale out-shift)
  (declare (single-float out-pixel-center out-filter-radius inv-scale out-shift))
  (let* ((out-pixel-influence-lowerbound (- out-pixel-center out-filter-radius))
         (out-pixel-influence-upperbound (+ out-pixel-center out-filter-radius))
         (in-pixel-influence-lowerbound (* (+ out-pixel-influence-lowerbound out-shift) inv-scale))
         (in-pixel-influence-upperbound (* (+ out-pixel-influence-upperbound out-shift) inv-scale))
         (first (%stbir-floorf (+ in-pixel-influence-lowerbound 0.5)))
         (last (%stbir-floorf (- in-pixel-influence-upperbound 0.5))))
    (when (< last first) (setf last first)) ; point sample mode can span a value *right* at 0.5, and cause these to cross
    (values first last)))

(defun %stbir-calculate-coefficients-for-gather-upsample (out-filter-radius kernel samp num-contributors contributors coefficient-group coefficient-width)
  (let* ((inv-scale (%samp-inv-scale samp))
         (out-shift (%samp-pixel-shift samp))
         (numerator (%samp-scale-numerator samp))
         (polyphase (and (%samp-scale-is-rational samp) (< numerator num-contributors)))
         (end (if polyphase numerator num-contributors))
         (group 0))
    (dotimes (n end)
      (let* ((out-pixel-center (+ (float n) 0.5))
             (in-center-of-out (* (+ out-pixel-center out-shift) inv-scale))
             (last-non-zero -1))
        (multiple-value-bind (in-first-pixel in-last-pixel)
            (%stbir-calculate-in-pixel-range out-pixel-center out-filter-radius inv-scale out-shift)
          ;; make sure we never generate a range larger than our precalculated coeff width
          ;;   this only happens in point sample mode, but it's a good safe thing to do anyway
          (when (> (1+ (- in-last-pixel in-first-pixel)) coefficient-width)
            (setf in-last-pixel (+ in-first-pixel coefficient-width -1)))

          (let ((i 0))
            (loop while (<= i (- in-last-pixel in-first-pixel))
                  do (let* ((in-pixel-center (+ (float (+ i in-first-pixel)) 0.5))
                            (coeff (funcall kernel (- in-center-of-out in-pixel-center) inv-scale)))
                       (block continue
                         ;; kill denormals
                         (if (and (< coeff +stbir-small-float+) (> coeff (- +stbir-small-float+)))
                             (progn
                               (when (= i 0) ; if we're at the front, just eat zero contributors
                                 (incf in-first-pixel)
                                 (return-from continue))
                               (setf coeff 0.0)) ; make sure is fully zero (should keep denormals away)
                             (setf last-non-zero i))
                         (setf (aref coefficient-group (+ group i)) coeff)
                         (incf i)))))

          (setf in-last-pixel (+ last-non-zero in-first-pixel)) ; kills trailing zeros
          (setf (%stbir-n0 contributors n) in-first-pixel
                (%stbir-n1 contributors n) in-last-pixel)
          (incf group coefficient-width))))))

;; NOTE: CONTRIBS is (vector, index) and COEFFS (array, offset) as the C pointers
(defun %stbir-insert-coeff (contribs ci coeffs co new-pixel new-coeff max-width)
  (cond ((< (%stbir-n1 contribs ci) (%stbir-n0 contribs ci)) ; this first clause should never happen, but handle in case
         (setf (%stbir-n0 contribs ci) new-pixel
               (%stbir-n1 contribs ci) new-pixel
               (aref coeffs co) new-coeff))
        ((<= new-pixel (%stbir-n1 contribs ci)) ; before the end
         (if (< new-pixel (%stbir-n0 contribs ci)) ; before the front?
             (when (<= (1+ (- (%stbir-n1 contribs ci) new-pixel)) max-width)
               (let ((o (- (%stbir-n0 contribs ci) new-pixel)))
                 (loop for j from (- (%stbir-n1 contribs ci) (%stbir-n0 contribs ci)) downto 0
                       do (setf (aref coeffs (+ co j o)) (aref coeffs (+ co j))))
                 (loop for j from 1 below o
                       do (setf (aref coeffs (+ co j)) 0.0))
                 (setf (aref coeffs co) new-coeff)
                 (setf (%stbir-n0 contribs ci) new-pixel)))
             ;; add new weight to existing coeff if already there
             (incf (aref coeffs (+ co (- new-pixel (%stbir-n0 contribs ci)))) new-coeff)))
        (t
         (when (<= (1+ (- new-pixel (%stbir-n0 contribs ci))) max-width)
           (let ((e (- new-pixel (%stbir-n0 contribs ci))))
             (loop for j from (1+ (- (%stbir-n1 contribs ci) (%stbir-n0 contribs ci))) below e ; clear in-betweens coeffs if there are any
                   do (setf (aref coeffs (+ co j)) 0.0))
             (setf (aref coeffs (+ co e)) new-coeff)
             (setf (%stbir-n1 contribs ci) new-pixel))))))

(defun %stbir-calculate-out-pixel-range (in-pixel-center in-pixels-radius scale out-shift out-size)
  (declare (single-float in-pixel-center in-pixels-radius scale out-shift))
  (let* ((in-pixel-influence-lowerbound (- in-pixel-center in-pixels-radius))
         (in-pixel-influence-upperbound (+ in-pixel-center in-pixels-radius))
         (out-pixel-influence-lowerbound (- (* in-pixel-influence-lowerbound scale) out-shift))
         (out-pixel-influence-upperbound (- (* in-pixel-influence-upperbound scale) out-shift))
         (out-first-pixel (%stbir-floorf (+ out-pixel-influence-lowerbound 0.5)))
         (out-last-pixel (%stbir-floorf (- out-pixel-influence-upperbound 0.5))))
    (when (< out-first-pixel 0) (setf out-first-pixel 0))
    (when (>= out-last-pixel out-size) (setf out-last-pixel (1- out-size)))
    (values out-first-pixel out-last-pixel)))

(defun %stbir-calculate-coefficients-for-gather-downsample (start end in-pixels-radius kernel samp coefficient-width contributors coefficient-group)
  (let* ((first-out-inited -1)
         (scale (%samp-scale samp))
         (out-shift (%samp-pixel-shift samp))
         (out-size (%samp-output-sub-size samp))
         (numerator (%samp-scale-numerator samp))
         (polyphase (and (%samp-scale-is-rational samp) (< numerator out-size))))
    ;; Loop through the input pixels
    (loop for in-pixel from start below end
          do (let* ((in-pixel-center (+ (float in-pixel) 0.5))
                    (out-center-of-in (- (* in-pixel-center scale) out-shift)))
               (multiple-value-bind (out-first-pixel out-last-pixel)
                   (%stbir-calculate-out-pixel-range in-pixel-center in-pixels-radius scale out-shift out-size)
                 (unless (> out-first-pixel out-last-pixel)
                   ;; clamp or exit if we are using polyphase filtering, and the limit is up
                   (when polyphase
                     ;; when polyphase, you only have to do coeffs up to the numerator count
                     (when (= out-first-pixel numerator)
                       (return))
                     ;; don't do any extra work, clamp last pixel at numerator too
                     (when (>= out-last-pixel numerator)
                       (setf out-last-pixel (1- numerator))))

                   (loop for i from 0 to (- out-last-pixel out-first-pixel)
                         do (let* ((out-pixel-center (+ (float (+ i out-first-pixel)) 0.5))
                                   (x (- out-pixel-center out-center-of-in))
                                   (coeff (* (funcall kernel x scale) scale)))
                              ;; kill the coeff if it's too small (avoid denormals)
                              (when (and (< coeff +stbir-small-float+) (> coeff (- +stbir-small-float+)))
                                (setf coeff 0.0))

                              (let* ((out (+ i out-first-pixel))
                                     (coeffs (* out coefficient-width)))
                                ;; is this the first time this output pixel has been seen?  Init it.
                                (if (> out first-out-inited)
                                    (progn
                                      (setf first-out-inited out)
                                      (setf (%stbir-n0 contributors out) in-pixel
                                            (%stbir-n1 contributors out) in-pixel)
                                      (setf (aref coefficient-group coeffs) coeff))
                                    (progn
                                      ;; insert on end (always in order)
                                      (when (= (aref coefficient-group coeffs) 0.0) ; if the first coefficent is zero, then zap it for this coeffs
                                        (setf (%stbir-n0 contributors out) in-pixel))
                                      (setf (%stbir-n1 contributors out) in-pixel)
                                      (setf (aref coefficient-group (+ coeffs (- in-pixel (%stbir-n0 contributors out)))) coeff))))))))))))

(defun %stbir-cleanup-gathered-coefficients (samp num-contributors contributors coefficient-group coefficient-width)
  (let* ((input-size (%samp-input-full-size samp))
         (input-last-n1 (1- input-size))
         (lowest #x7fffffff)
         (highest (- #x7fffffff))
         (widest -1)
         (numerator (%samp-scale-numerator samp))
         (denominator (%samp-scale-denominator samp))
         (polyphase (and (%samp-scale-is-rational samp) (< numerator num-contributors)))
         (end (if polyphase numerator num-contributors)))
    ;; weight all the coeffs for each sample
    (dotimes (n end)
      (let* ((coeffs (* n coefficient-width))
             (total-filter 0d0)             ; STBIR_RENORM_TYPE double
             (e (- (%stbir-n1 contributors n) (%stbir-n0 contributors n))))
        ;; add all contribs
        (loop for i from 0 to e
              do (incf total-filter (float (aref coefficient-group (+ coeffs i)) 1d0)))

        ;; rescale
        (if (and (< total-filter +stbir-small-float+) (> total-filter (- +stbir-small-float+)))
            (progn
              ;; all coeffs are extremely small, just zero it
              (setf (%stbir-n1 contributors n) (%stbir-n0 contributors n))
              (setf (aref coefficient-group coeffs) 0.0))
            ;; if the total isn't 1.0, rescale everything
            (when (or (< total-filter (float (- 1.0 +stbir-small-float+) 1d0))
                      (> total-filter (float (+ 1.0 +stbir-small-float+) 1d0)))
              (let ((filter-scale (/ 1d0 total-filter)))
                ;; scale them all
                (loop for i from 0 to e
                      do (setf (aref coefficient-group (+ coeffs i))
                               (float (* (aref coefficient-group (+ coeffs i)) filter-scale) 1.0))))))))

    ;; if we have a rational for the scale, we can exploit the polyphaseness to not calculate
    ;;   most of the coefficients, so we copy them here
    (when polyphase
      (loop for n from numerator below num-contributors
            for prev from 0
            do (setf (%stbir-n0 contributors n) (+ (%stbir-n0 contributors prev) denominator)
                     (%stbir-n1 contributors n) (+ (%stbir-n1 contributors prev) denominator)))
      ;; stbir_overlapping_memcpy(): forward copy, so the first NUMERATOR rows repeat
      (let ((dest (* numerator coefficient-width)))
        (dotimes (j (* (- num-contributors numerator) coefficient-width))
          (setf (aref coefficient-group (+ dest j)) (aref coefficient-group j)))))

    (dotimes (n num-contributors)
      (let ((coeffs (* n coefficient-width)))
        ;; for clamp and reflect, calculate the true inbounds position (based on edge type) and just add that to the existing weight

        ;; right hand side first
        (when (> (%stbir-n1 contributors n) input-last-n1)
          (let ((start (%stbir-n0 contributors n))
                (endi (%stbir-n1 contributors n)))
            (setf (%stbir-n1 contributors n) input-last-n1)
            (loop for i from input-size to endi
                  do (%stbir-insert-coeff contributors n coefficient-group coeffs
                                          (%stbir-edge-wrap i input-size)
                                          (aref coefficient-group (+ coeffs (- i start))) coefficient-width))))

        ;; now check left hand edge
        (when (< (%stbir-n0 contributors n) 0)
          (let ((c (- coeffs (1+ (%stbir-n0 contributors n)))))
            ;; reinsert the coeffs with it reflected or clamped (insert accumulates, if the coeffs exist)
            (loop for i from -1 above (%stbir-n0 contributors n)
                  do (%stbir-insert-coeff contributors n coefficient-group coeffs
                                          (%stbir-edge-wrap i input-size) (aref coefficient-group c) coefficient-width)
                     (decf c))
            (let ((save-n0 (%stbir-n0 contributors n))
                  ;; save it, since we didn't do the final one (i==n0), because there might be too many coeffs to hold (before we resize)!
                  (save-n0-coeff (aref coefficient-group c)))
              ;; now slide all the coeffs down (since we have accumulated them in the positive contribs) and reset the first contrib
              (setf (%stbir-n0 contributors n) 0)
              (loop for i from 0 to (%stbir-n1 contributors n)
                    do (setf (aref coefficient-group (+ coeffs i)) (aref coefficient-group (+ coeffs (- i save-n0)))))

              ;; now that we have shrunk down the contribs, we insert the first one safely
              (%stbir-insert-coeff contributors n coefficient-group coeffs
                                   (%stbir-edge-wrap save-n0 input-size) save-n0-coeff coefficient-width))))

        (when (<= (%stbir-n0 contributors n) (%stbir-n1 contributors n))
          (let ((diff (1+ (- (%stbir-n1 contributors n) (%stbir-n0 contributors n)))))
            (loop while (and (/= diff 0) (= (aref coefficient-group (+ coeffs (1- diff))) 0.0))
                  do (decf diff))

            (setf (%stbir-n1 contributors n) (+ (%stbir-n0 contributors n) diff -1))

            (when (<= (%stbir-n0 contributors n) (%stbir-n1 contributors n))
              (when (< (%stbir-n0 contributors n) lowest) (setf lowest (%stbir-n0 contributors n)))
              (when (> (%stbir-n1 contributors n) highest) (setf highest (%stbir-n1 contributors n)))
              (when (> diff widest) (setf widest diff)))

            ;; re-zero out unused coefficients (if any)
            (loop for i from diff below coefficient-width
                  do (setf (aref coefficient-group (+ coeffs i)) 0.0))))))

    (setf (%samp-lowest samp) lowest
          (%samp-highest samp) highest
          (%samp-widest samp) widest)))

(defun %stbir-pack-coefficients (num-contributors contributors coefficents coefficient-width widest row1)
  (let ((row-end (1+ row1)))
    (when (/= coefficient-width widest)
      ;; compact the rows from COEFFICIENT-WIDTH to WIDEST floats (forward copy, destination before source)
      (dotimes (n num-contributors)
        (dotimes (j widest)
          (setf (aref coefficents (+ (* n widest) j)) (aref coefficents (+ (* n coefficient-width) j))))))

    ;; some horizontal routines read one float off the end (which is then masked off), so put in a sentinel so we don't read an snan or denormal
    (setf (aref coefficents (* widest num-contributors)) 8888.0)

    ;; the minimum we might read for unrolled filters widths is 12. So, we need to
    ;;   make sure we never read outside the decode buffer, by possibly moving
    ;;   the sample area back into the scanline, and putting zeros weights first.
    ;; we start on the right edge and check until we're well past the possible
    ;;   clip area (2*widest).
    (let ((contribs (1- num-contributors))
          (coeffs (* widest (1- num-contributors))))
      ;; go until no chance of clipping (this is usually less than 8 lops)
      (loop while (and (>= contribs 0) (>= (+ (%stbir-n0 contributors contribs) (* widest 2)) row-end))
            do ;; might we clip??
               (when (> (+ (%stbir-n0 contributors contribs) widest) row-end)
                 (let ((stop-range widest))
                   ;; if range is larger than 12, it will be handled by generic loops that can terminate on the exact length
                   ;;   of this contrib n1, instead of a fixed widest amount - so calculate this
                   (when (> widest 12)
                     (let ((mod (logand widest 3)))
                       ;; how far will be read in the n_coeff loop (which depends on the widest count mod4);
                       (setf stop-range (+ (logand (+ (- (1+ (- (%stbir-n1 contributors contribs) (%stbir-n0 contributors contribs))) mod) 3) (lognot 3)) mod))
                       ;; the n_coeff loops do a minimum amount of coeffs, so factor that in!
                       (when (< stop-range (+ 8 mod)) (setf stop-range (+ 8 mod)))))

                   ;; now see if we still clip with the refined range
                   (when (> (+ (%stbir-n0 contributors contribs) stop-range) row-end)
                     (let* ((new-n0 (- row-end stop-range))
                            (num (1+ (- (%stbir-n1 contributors contribs) (%stbir-n0 contributors contribs))))
                            (backup (- (%stbir-n0 contributors contribs) new-n0))
                            (from-co (+ coeffs num -1))
                            (to-co (+ from-co backup)))
                       ;; move the coeffs over
                       (loop while (/= num 0)
                             do (setf (aref coefficents to-co) (aref coefficents from-co))
                                (decf to-co) (decf from-co) (decf num))
                       ;; zero new positions
                       (loop while (>= to-co coeffs)
                             do (setf (aref coefficents to-co) 0.0)
                                (decf to-co))
                       ;; set new start point
                       (setf (%stbir-n0 contributors contribs) new-n0)))))
               (decf contribs)
               (decf coeffs widest)))
    widest))

(defun %stbir-calculate-filters (samp other-axis-for-pivot)
  (let ((scale (%samp-scale samp))
        (kernel (%samp-filter-kernel samp))
        (support (%samp-filter-support samp))
        (inv-scale (%samp-inv-scale samp))
        (input-full-size (%samp-input-full-size samp))
        (gather-num-contributors (%samp-num-contributors samp))
        (gather-contributors (%samp-contributors samp))
        (gather-coeffs (%samp-coefficients samp))
        (gather-coefficient-width (%samp-coefficient-width samp)))
    (ecase (%samp-is-gather samp)
      (1 ;; gather upsample
       (let ((out-pixels-radius (* (funcall support inv-scale) scale)))
         (%stbir-calculate-coefficients-for-gather-upsample out-pixels-radius kernel samp gather-num-contributors
                                                             gather-contributors gather-coeffs gather-coefficient-width)
         (%stbir-cleanup-gathered-coefficients samp gather-num-contributors gather-contributors gather-coeffs gather-coefficient-width)))
      ((0 2) ;; scatter downsample (only on vertical), gather downsample
       (let* ((in-pixels-radius (* (funcall support scale) inv-scale))
              (filter-pixel-margin (%samp-filter-pixel-margin samp))
              (input-end (+ input-full-size filter-pixel-margin))
              (pivot-only nil))
         ;; if this is a scatter, we do a downsample gather to get the coeffs, and then pivot after
         (when (= (%samp-is-gather samp) 0)
           ;; check if we are using the same gather downsample on the horizontal as this vertical,
           ;;   if so, then we don't have to generate them, we can just pivot from the horizontal.
           (if other-axis-for-pivot
               (setf gather-contributors (%samp-contributors other-axis-for-pivot)
                     gather-coeffs (%samp-coefficients other-axis-for-pivot)
                     gather-coefficient-width (%samp-coefficient-width other-axis-for-pivot)
                     gather-num-contributors (%samp-num-contributors other-axis-for-pivot)
                     (%samp-lowest samp) (%samp-lowest other-axis-for-pivot)
                     (%samp-highest samp) (%samp-highest other-axis-for-pivot)
                     (%samp-widest samp) (%samp-widest other-axis-for-pivot)
                     pivot-only t)
               (setf gather-contributors (%samp-gather-prescatter-contributors samp)
                     gather-coeffs (%samp-gather-prescatter-coefficients samp)
                     gather-coefficient-width (%samp-gather-prescatter-coefficient-width samp)
                     gather-num-contributors (%samp-gather-prescatter-num-contributors samp))))

         (unless pivot-only
           (%stbir-calculate-coefficients-for-gather-downsample (- filter-pixel-margin) input-end in-pixels-radius kernel samp
                                                                gather-coefficient-width gather-contributors gather-coeffs)
           (%stbir-cleanup-gathered-coefficients samp gather-num-contributors gather-contributors gather-coeffs gather-coefficient-width))

         (when (= (%samp-is-gather samp) 0)
           ;; if this is a scatter (vertical only), then we need to pivot the coeffs
           (let ((highest-set (1- (- filter-pixel-margin)))
                 (contributors (%samp-contributors samp))
                 (coefficients (%samp-coefficients samp))
                 (scatter-coefficient-width (%samp-coefficient-width samp)))
             (dotimes (n gather-num-contributors)
               (let* ((gn0 (%stbir-n0 gather-contributors n))
                      (gn1 (%stbir-n1 gather-contributors n))
                      (scatter-coeffs (* (+ gn0 filter-pixel-margin) scatter-coefficient-width))
                      (g-coeffs (* n gather-coefficient-width))
                      (scatter-contributors (+ gn0 filter-pixel-margin)))
                 (loop for k from gn0 to gn1
                       do (let ((gc (aref gather-coeffs g-coeffs)))
                            (incf g-coeffs)
                            ;; skip zero and denormals - must skip zeros to avoid adding coeffs beyond scatter_coefficient_width
                            ;;   (which happens when pivoting from horizontal, which might have dummy zeros)
                            (when (or (>= gc +stbir-small-float+) (<= gc (- +stbir-small-float+)))
                              (if (or (> k highest-set)
                                      (> (%stbir-n0 contributors scatter-contributors) (%stbir-n1 contributors scatter-contributors)))
                                  (progn
                                    ;; if we are skipping over several contributors, we need to clear the skipped ones
                                    (loop for clear from (+ highest-set filter-pixel-margin 1) below scatter-contributors
                                          do (setf (%stbir-n0 contributors clear) 0
                                                   (%stbir-n1 contributors clear) -1))
                                    (setf (%stbir-n0 contributors scatter-contributors) n
                                          (%stbir-n1 contributors scatter-contributors) n)
                                    (setf (aref coefficients scatter-coeffs) gc)
                                    (setf highest-set k))
                                  (%stbir-insert-coeff contributors scatter-contributors coefficients scatter-coeffs
                                                       n gc scatter-coefficient-width)))
                            (incf scatter-contributors)
                            (incf scatter-coeffs scatter-coefficient-width)))))

             ;; now clear any unset contribs
             (loop for clear from (+ highest-set filter-pixel-margin 1) below (%samp-num-contributors samp)
                   do (setf (%stbir-n0 contributors clear) 0
                            (%stbir-n1 contributors clear) -1)))))))))

;;----------------------------------------------------------------------------------
;; scanline decoders and encoders
;;----------------------------------------------------------------------------------

;; decode WIDTH-TIMES-CHANNELS bytes of INPUT (from IN) to floats at DECODE[D], returns the end index
(defun %stbir-decode-uint8-linear (decode d width-times-channels input in scaled)
  (declare (type %stbir-floats decode) (type %octets input) (fixnum d width-times-channels in)
           (optimize speed))
  (if scaled
      (dotimes (i width-times-channels)
        (setf (aref decode (+ d i)) (* (float (aref input (+ in i))) +stbir-max-uint8-as-float-inverted+)))
      (dotimes (i width-times-channels)
        (setf (aref decode (+ d i)) (float (aref input (+ in i))))))
  (+ d width-times-channels))

;; f = e (*255) + 0.5, clamped to 0..255 and truncated
(defun %stbir-encode-uint8-linear (output out width-times-channels encode e scaled)
  (declare (type %stbir-floats encode) (type %octets output) (fixnum out width-times-channels e)
           (optimize speed))
  (dotimes (i width-times-channels)
    (let ((f (if scaled
                 (+ (* (aref encode (+ e i)) +stbir-max-uint8-as-float+) 0.5)
                 (+ (aref encode (+ e i)) 0.5))))
      (declare (single-float f))
      (when (< f 0.0) (setf f 0.0))
      (when (> f 255.0) (setf f 255.0))
      (setf (aref output (+ out i)) (truncate f)))))

;; fancy alpha means we expand to keep both premultipied and non-premultiplied color channels
;; NOTE: fancy RGBA is stored internally as R G B A Rpm Gpm Bpm
(defun %stbir-fancy-alpha-weight-4ch (buffer start width-times-channels)
  (declare (type %stbir-floats buffer) (fixnum start width-times-channels) (optimize speed))
  (let* ((out start)
         (end-decode (+ start (* (floor width-times-channels 4) 7))) ; decode buffer aligned to end of out_buffer
         (decode (- end-decode width-times-channels)))
    (declare (fixnum out end-decode decode))
    (loop while (< decode end-decode)
          do (let ((r (aref buffer decode)) (g (aref buffer (+ decode 1)))
                   (b (aref buffer (+ decode 2))) (alpha (aref buffer (+ decode 3))))
               (setf (aref buffer out) r
                     (aref buffer (+ out 1)) g
                     (aref buffer (+ out 2)) b
                     (aref buffer (+ out 3)) alpha
                     (aref buffer (+ out 4)) (* r alpha)
                     (aref buffer (+ out 5)) (* g alpha)
                     (aref buffer (+ out 6)) (* b alpha))
               (incf out 7)
               (incf decode 4)))))

(defun %stbir-fancy-alpha-unweight-4ch (buffer start width-times-channels)
  (declare (type %stbir-floats buffer) (fixnum start width-times-channels) (optimize speed))
  (let ((encode start)
        (input start)
        (end-output (+ start width-times-channels)))
    (declare (fixnum encode input end-output))
    (loop
      (let ((alpha (aref buffer (+ input 3))))
        (if (< alpha +stbir-small-float+)
            (setf (aref buffer encode) (aref buffer input)
                  (aref buffer (+ encode 1)) (aref buffer (+ input 1))
                  (aref buffer (+ encode 2)) (aref buffer (+ input 2)))
            (let ((ialpha (/ 1.0 alpha)))
              (setf (aref buffer encode) (* (aref buffer (+ input 4)) ialpha)
                    (aref buffer (+ encode 1)) (* (aref buffer (+ input 5)) ialpha)
                    (aref buffer (+ encode 2)) (* (aref buffer (+ input 6)) ialpha))))
        (setf (aref buffer (+ encode 3)) alpha))
      (incf input 7)
      (incf encode 4)
      (unless (< encode end-output) (return)))))

;; NOTE: OUTPUT-BUFFER is (array, index), index 0 of it is the pixel scanline_extents.conservative.n0
(defun %stbir-decode-scanline (info n output-buffer ob)
  (let* ((channels (%info-channels info))
         (effective-channels (%info-effective-channels info))
         (row (%stbir-edge-wrap n (%samp-input-full-size (%info-vertical info))))
         (input-plane-data (* row (%info-input-stride-bytes info)))
         (full-decode-buffer (- ob (* (%info-conservative-n0 info) effective-channels)))
         (last-decoded 0))
    ;; NOTE: edge clamp only has one span (the margins merge into it)
    (when (<= (%info-span-n0 info) (%info-span-n1 info))
      (let* ((width (- (1+ (%info-span-n1 info)) (%info-span-n0 info)))
             (decode-buffer (+ full-decode-buffer (* (%info-span-n0 info) effective-channels)))
             (end-decode (+ full-decode-buffer (* (1+ (%info-span-n1 info)) effective-channels)))
             (width-times-channels (* width channels))
             ;; read directly out of input plane by default
             (input-data (+ input-plane-data (* (%info-span-pixel-offset-for-input info) channels))))
        ;; convert the pixels info the float decode_buffer, (we index from end_decode, so that when channels<effective_channels, we are right justified in the buffer)
        (setf last-decoded (%stbir-decode-uint8-linear output-buffer (- end-decode width-times-channels) width-times-channels
                                                        (%info-input-data info) input-data (%info-decode-scaled info)))
        (when (%info-alpha-weight info)
          (%stbir-fancy-alpha-weight-4ch output-buffer decode-buffer width-times-channels))))

    ;; some of the horizontal gathers read one float off the edge (which is masked out), but we force a zero here to make sure no NaNs leak in
    (setf (aref output-buffer last-decoded) 0.0)
    ;; we clear this extra float, because the final output pixel filter kernel might have used one less coeff than the max filter width
    (setf (aref output-buffer (1+ last-decoded)) 0.0)))

;;----------------------------------------------------------------------------------
;; Horizontal gathers
;;----------------------------------------------------------------------------------
;; NOTE: stb_image_resize2 has one function per channel count and coefficient count, they all
;; sum the products of one output channel in these accumulators:
;;   - less than 4 coeffs: one sum in coefficient order (2 channels, 3 coeffs: order 0 2 1)
;;   - 1-3 channels: 4 sums by coefficient index mod 4, then (s0+s2)+(s1+s3)
;;   - 4 and 7 channels: 2 sums by coefficient index mod 2, then s0+s1
;; Up to 12 coeffs they always read WIDEST coeffs, wider filters read the coefficients of each
;; contributor rounded up to the widest mod 4 remnant (the _with_n_coeffs_modX functions)

(defun %stbir-horizontal-gather-channels (channels widest output-buffer ob output-sub-size decode-buffer db contributors coefficients coefficient-width)
  (declare (type %stbir-floats output-buffer decode-buffer coefficients) (type %stbir-contribs contributors)
           (fixnum channels widest ob output-sub-size db coefficient-width)
           (optimize speed))
  (let ((output ob)
        (hc 0))
    (declare (fixnum output hc))
    (dotimes (o output-sub-size)
      (let* ((n0 (aref contributors (* 2 o)))
             (n1 (aref contributors (1+ (* 2 o))))
             (decode (+ db (* n0 channels)))
             (count (if (<= widest 12)
                        widest
                        (let* ((mod (logand widest 3))
                               (n (ash (+ (- (1+ (- n1 n0)) (+ 4 mod)) 3) -2)))
                          (+ 4 (* 4 (max n 1)) mod)))))
        (declare (fixnum n0 n1 decode count))
        (dotimes (c channels)
          (flet ((term (k)
                   (declare (fixnum k))
                   (* (aref decode-buffer (+ decode c (* k channels))) (aref coefficients (+ hc k)))))
            (declare (inline term))
            (setf (aref output-buffer (+ output c))
                  (cond ((< count 4)
                         (if (and (= channels 2) (= count 3))
                             ;; this weird order of add matches the simd
                             (+ (+ (term 0) (term 2)) (term 1))
                             (let ((tot (term 0)))
                               (declare (single-float tot))
                               (loop for k fixnum from 1 below count do (incf tot (term k)))
                               tot)))
                        ((<= channels 3)
                         (let ((t0 (term 0)) (t1 (term 1)) (t2 (term 2)) (t3 (term 3)))
                           (declare (single-float t0 t1 t2 t3))
                           (loop for k fixnum from 4 below count
                                 do (case (logand k 3)
                                      (0 (incf t0 (term k)))
                                      (1 (incf t1 (term k)))
                                      (2 (incf t2 (term k)))
                                      (t (incf t3 (term k)))))
                           (+ (+ t0 t2) (+ t1 t3))))
                        (t
                         (let ((x (term 0)) (y (term 1)))
                           (declare (single-float x y))
                           (loop for k fixnum from 2 below count
                                 do (if (evenp k) (incf x (term k)) (incf y (term k))))
                           (+ x y)))))))
        (incf hc coefficient-width)
        (incf output channels)))))

(defun %stbir-resample-horizontal-gather (info output-buffer ob input-buffer ib)
  (let* ((horizontal (%info-horizontal info))
         (effective-channels (%info-effective-channels info))
         (decode-buffer (- ib (* (%info-conservative-n0 info) effective-channels))))
    (if (and (= (%samp-filter-enum horizontal) +stbir-filter-point-sample+) (= (%samp-scale horizontal) 1.0))
        (replace output-buffer input-buffer :start1 ob :start2 ib
                                            :end2 (+ ib (* (%samp-output-sub-size horizontal) effective-channels)))
        (%stbir-horizontal-gather-channels effective-channels (%samp-widest horizontal) output-buffer ob
                                           (%samp-output-sub-size horizontal) input-buffer decode-buffer
                                           (%samp-contributors horizontal) (%samp-coefficients horizontal)
                                           (%samp-coefficient-width horizontal)))))

;;----------------------------------------------------------------------------------
;; Vertical gathers and scatters (elementwise, up to 8 scanlines at once)
;;----------------------------------------------------------------------------------

;; stbir__vertical_gather_with_N_coeffs(_cont): OUTPUT = (OUTPUT +) sum INPUTS[i] * COEFFS[i]
(defun %stbir-vertical-gather (output ob coefficients co cnt inputs width-times-channels continue)
  (declare (type %stbir-floats output coefficients) (fixnum ob co cnt width-times-channels)
           (simple-vector inputs) (optimize speed))
  (let ((c0s (aref coefficients co)))
    ;; check single channel one weight
    (if (and (not continue) (= cnt 1) (>= c0s (- 1.0 0.000001)) (<= c0s (+ 1.0 0.000001)))
        (let ((input0 (svref inputs 0)))
          (replace output (the %stbir-floats (car input0)) :start1 ob :start2 (cdr input0)
                                                           :end2 (+ (the fixnum (cdr input0)) width-times-channels)))
        (dotimes (i width-times-channels)
          (let* ((input0 (svref inputs 0))
                 (o (* (aref (the %stbir-floats (car input0)) (+ (the fixnum (cdr input0)) i)) c0s)))
            (declare (single-float o))
            (when continue (setf o (+ (aref output (+ ob i)) o)))
            (loop for k fixnum from 1 below cnt
                  do (let ((input (svref inputs k)))
                       (incf o (* (aref (the %stbir-floats (car input)) (+ (the fixnum (cdr input)) i))
                                  (aref coefficients (+ co k))))))
            (setf (aref output (+ ob i)) o))))))

;; stbir__vertical_scatter_with_N_coeffs(_cont): OUTPUTS[i] = (OUTPUTS[i] +) INPUT * COEFFS[i]
(defun %stbir-vertical-scatter (outputs coefficients co cnt input ib width-times-channels continue)
  (declare (type %stbir-floats coefficients input) (fixnum co cnt ib width-times-channels)
           (simple-vector outputs) (optimize speed))
  (dotimes (k cnt)
    (let ((output (svref outputs k))
          (c (aref coefficients (+ co k))))
      (declare (type %stbir-floats output) (single-float c))
      (if continue
          (dotimes (i width-times-channels)
            (setf (aref output i) (+ (aref output i) (* (aref input (+ ib i)) c))))
          (dotimes (i width-times-channels)
            (setf (aref output i) (* (aref input (+ ib i)) c)))))))

(defun %stbir-encode-scanline (info row encode-buffer eb)
  (let* ((num-pixels (%samp-output-sub-size (%info-horizontal info)))
         (width-times-channels (* num-pixels (%info-channels info))))
    ;; un-alpha weight if we need to
    (when (%info-alpha-unweight info)
      (%stbir-fancy-alpha-unweight-4ch encode-buffer eb width-times-channels))
    ;; convert into the output buffer
    (%stbir-encode-uint8-linear (%info-output-data info) (* row (%info-output-stride-bytes info)) width-times-channels
                                encode-buffer eb (%info-decode-scaled info))))

;; Get the ring buffer for an index
(defun %stbir-get-ring-buffer-entry (info index)
  (svref (%info-ring-buffers info) index))

;; Get the specified scan line from the ring buffer
(defun %stbir-get-ring-buffer-scanline (info get-scanline)
  (%stbir-get-ring-buffer-entry info (mod (+ (%info-ring-buffer-begin-index info)
                                             (- get-scanline (%info-ring-buffer-first-scanline info)))
                                          (%info-ring-buffer-num-entries info))))

(defun %stbir-resample-vertical-gather (info n contrib-n0 contrib-n1 vertical-coefficients vc)
  (let* ((encode-buffer (%info-vertical-buffer info))
         (decode-buffer (%info-decode-buffer info))
         (vertical-first (%info-vertical-first info))
         (width (if vertical-first
                    (1+ (- (%info-conservative-n1 info) (%info-conservative-n0 info)))
                    (%samp-output-sub-size (%info-horizontal info))))
         (width-times-channels (* (%info-effective-channels info) width)))
    ;; loop over the contributing scanlines and scale into the buffer
    (let ((k 0)
          (total (1+ (- contrib-n1 contrib-n0))))
      (loop
        (let* ((cnt (min total 8))
               (inputs (make-array cnt)))
          (dotimes (i cnt)
            (setf (svref inputs i) (cons (%stbir-get-ring-buffer-scanline info (+ k i contrib-n0)) 0)))
          ;; call the N scanlines at a time function (up to 8 scanlines of blending at once)
          (%stbir-vertical-gather (if vertical-first decode-buffer encode-buffer) 0 vertical-coefficients (+ vc k) cnt inputs
                                  width-times-channels (/= k 0))
          (incf k cnt)
          (decf total cnt)
          (when (= total 0) (return)))))

    (when vertical-first
      ;; Now resample the gathered vertical data in the horizontal axis into the encode buffer
      (setf (aref decode-buffer width-times-channels) 0.0) ; clear two over for horizontals with a remnant of 3
      (setf (aref decode-buffer (1+ width-times-channels)) 0.0)
      (%stbir-resample-horizontal-gather info encode-buffer 0 decode-buffer 0))

    (%stbir-encode-scanline info n encode-buffer 0)))

(defun %stbir-decode-and-resample-for-vertical-gather-loop (info n)
  ;; Decode the nth scanline from the source image into the decode buffer.
  (%stbir-decode-scanline info n (%info-decode-buffer info) 0)
  ;; update new end scanline
  (setf (%info-ring-buffer-last-scanline info) n)
  ;; Now resample it into the ring buffer.
  (let ((ring-buffer (%stbir-get-ring-buffer-scanline info n)))
    (%stbir-resample-horizontal-gather info ring-buffer 0 (%info-decode-buffer info) 0)))

(defun %stbir-vertical-gather-loop (info)
  (let* ((vertical (%info-vertical info))
         (vertical-contributors (%samp-contributors vertical))
         (vertical-coefficients (%samp-coefficients vertical))
         (start-output-y (%info-start-output-y info))
         (end-output-y (%info-end-output-y info))
         (vc (* start-output-y (%samp-coefficient-width vertical))))
    ;; initialize the ring buffer for gathering
    (setf (%info-ring-buffer-begin-index info) 0
          (%info-ring-buffer-first-scanline info) (%stbir-n0 vertical-contributors start-output-y)
          (%info-ring-buffer-last-scanline info) (1- (%info-ring-buffer-first-scanline info))) ; means "empty"

    (loop for y from start-output-y below end-output-y
          do (let ((in-first-scanline (%stbir-n0 vertical-contributors y))
                   (in-last-scanline (%stbir-n1 vertical-contributors y)))
               ;; Load in new scanlines
               (loop while (> in-last-scanline (%info-ring-buffer-last-scanline info))
                     do ;; make sure there was room in the ring buffer when we add new scanlines
                        (when (= (1+ (- (%info-ring-buffer-last-scanline info) (%info-ring-buffer-first-scanline info)))
                                 (%info-ring-buffer-num-entries info))
                          (incf (%info-ring-buffer-first-scanline info))
                          (incf (%info-ring-buffer-begin-index info)))

                        (if (%info-vertical-first info)
                            (let ((ring-buffer (%stbir-get-ring-buffer-scanline info (incf (%info-ring-buffer-last-scanline info)))))
                              ;; Decode the nth scanline from the source image into the decode buffer.
                              (%stbir-decode-scanline info (%info-ring-buffer-last-scanline info) ring-buffer 0))
                            (%stbir-decode-and-resample-for-vertical-gather-loop info (1+ (%info-ring-buffer-last-scanline info)))))

               ;; Now all buffers should be ready to write a row of vertical sampling, so do it.
               (%stbir-resample-vertical-gather info y in-first-scanline in-last-scanline vertical-coefficients vc)
               (incf vc (%samp-coefficient-width vertical))))))

(defun %stbir-buffer-is-empty (buffer) (= (aref buffer 0) +stbir-float-empty-marker+))

(defun %stbir-encode-first-scanline-from-scatter (info)
  ;; evict a scanline out into the output buffer
  (let ((ring-buffer-entry (%stbir-get-ring-buffer-entry info (%info-ring-buffer-begin-index info))))
    ;; dump the scanline out
    (%stbir-encode-scanline info (%info-ring-buffer-first-scanline info) ring-buffer-entry 0)
    ;; mark it as empty
    (setf (aref ring-buffer-entry 0) +stbir-float-empty-marker+)
    ;; advance the first scanline
    (incf (%info-ring-buffer-first-scanline info))
    (when (= (incf (%info-ring-buffer-begin-index info)) (%info-ring-buffer-num-entries info))
      (setf (%info-ring-buffer-begin-index info) 0))))

(defun %stbir-horizontal-resample-and-encode-first-scanline-from-scatter (info)
  ;; evict a scanline out into the output buffer
  (let ((ring-buffer-entry (%stbir-get-ring-buffer-entry info (%info-ring-buffer-begin-index info))))
    ;; Now resample it into the buffer.
    (%stbir-resample-horizontal-gather info (%info-vertical-buffer info) 0 ring-buffer-entry 0)
    ;; dump the scanline out
    (%stbir-encode-scanline info (%info-ring-buffer-first-scanline info) (%info-vertical-buffer info) 0)
    ;; mark it as empty
    (setf (aref ring-buffer-entry 0) +stbir-float-empty-marker+)
    ;; advance the first scanline
    (incf (%info-ring-buffer-first-scanline info))
    (when (= (incf (%info-ring-buffer-begin-index info)) (%info-ring-buffer-num-entries info))
      (setf (%info-ring-buffer-begin-index info) 0))))

(defun %stbir-resample-vertical-scatter (info n0 n1 vertical-coefficients vc vertical-buffer width-times-channels)
  (let ((k 0)
        (total (1+ (- n1 n0))))
    (loop
      (let* ((n (min total 8))
             (outputs (make-array n)))
        (dotimes (i n)
          (setf (svref outputs i) (%stbir-get-ring-buffer-scanline info (+ k i n0)))
          ;; make sure runs are of the same type
          (when (and (/= i 0) (not (eq (%stbir-buffer-is-empty (svref outputs i)) (%stbir-buffer-is-empty (svref outputs 0)))))
            (setf n i)
            (return)))
        ;; call the scatter to N scanlines at a time function (up to 8 scanlines of scattering at once)
        (%stbir-vertical-scatter outputs vertical-coefficients (+ vc k) n vertical-buffer 0 width-times-channels
                                 (not (%stbir-buffer-is-empty (svref outputs 0))))
        (incf k n)
        (decf total n)
        (when (= total 0) (return))))))

(defun %stbir-vertical-scatter-loop (info)
  (let* ((vertical (%info-vertical info))
         (vertical-contributors (%samp-contributors vertical))
         (vertical-coefficients (%samp-coefficients vertical))
         (width (if (%info-vertical-first info)
                    (1+ (- (%info-conservative-n1 info) (%info-conservative-n0 info)))
                    (%samp-output-sub-size (%info-horizontal info))))
         (width-times-channels (* (%info-effective-channels info) width))
         (start-output-y (%info-start-output-y info))
         (end-output-y (%info-end-output-y info))
         (start-input-y (%info-start-input-y info))
         (end-input-y (%info-end-input-y info))
         ;; adjust for starting offset start_input_y
         (contrib (+ start-input-y (%samp-filter-pixel-margin vertical)))
         (vc (* (%samp-coefficient-width vertical) contrib))
         (handle-scanline-for-scatter (if (%info-vertical-first info)
                                          #'%stbir-horizontal-resample-and-encode-first-scanline-from-scatter
                                          #'%stbir-encode-first-scanline-from-scatter))
         (scanline-scatter-buffer (if (%info-vertical-first info) (%info-decode-buffer info) (%info-vertical-buffer info)))
         (on-first-input-y t)
         (last-input-y start-input-y))
    ;; initialize the ring buffer for scattering
    (setf (%info-ring-buffer-first-scanline info) start-output-y
          (%info-ring-buffer-last-scanline info) -1
          (%info-ring-buffer-begin-index info) -1)

    ;; mark all the buffers as empty to start
    (dotimes (y (%info-ring-buffer-num-entries info))
      (let ((decode-buffer (%stbir-get-ring-buffer-entry info y)))
        (setf (aref decode-buffer width-times-channels) 0.0) ; clear two over for horizontals with a remnant of 3
        (setf (aref decode-buffer (1+ width-times-channels)) 0.0)
        (setf (aref decode-buffer 0) +stbir-float-empty-marker+))) ; only used on scatter

    ;; do the loop in input space
    (loop for y from start-input-y below end-input-y
          do (let ((out-first-scanline (%stbir-n0 vertical-contributors contrib))
                   (out-last-scanline (%stbir-n1 vertical-contributors contrib)))
               (when (and (>= out-last-scanline out-first-scanline)
                          (or (and (>= out-first-scanline start-output-y) (< out-first-scanline end-output-y))
                              (and (>= out-last-scanline start-output-y) (< out-last-scanline end-output-y))))
                 (let ((v vc))
                   ;; keep track of the range actually seen for the next resize
                   (setf last-input-y y)
                   (when (and on-first-input-y (> y start-input-y))
                     (setf (%info-start-input-y info) y))
                   (setf on-first-input-y nil)

                   ;; clip the region
                   (when (< out-first-scanline start-output-y)
                     (incf v (- start-output-y out-first-scanline))
                     (setf out-first-scanline start-output-y))
                   (when (>= out-last-scanline end-output-y)
                     (setf out-last-scanline (1- end-output-y)))

                   ;; if very first scanline, init the index
                   (when (< (%info-ring-buffer-begin-index info) 0)
                     (setf (%info-ring-buffer-begin-index info) (- out-first-scanline start-output-y)))

                   ;; Decode the nth scanline from the source image into the decode buffer.
                   (%stbir-decode-scanline info y (%info-decode-buffer info) 0)

                   ;; When horizontal first, we resample horizontally into the vertical buffer before we scatter it out
                   (unless (%info-vertical-first info)
                     (%stbir-resample-horizontal-gather info (%info-vertical-buffer info) 0 (%info-decode-buffer info) 0))

                   ;; evict from the ringbuffer, if we need are full
                   (when (and (= (1+ (- (%info-ring-buffer-last-scanline info) (%info-ring-buffer-first-scanline info)))
                                 (%info-ring-buffer-num-entries info))
                              (> out-last-scanline (%info-ring-buffer-last-scanline info)))
                     (funcall handle-scanline-for-scatter info))

                   ;; Now the horizontal buffer is ready to write to all ring buffer rows, so do it.
                   (%stbir-resample-vertical-scatter info out-first-scanline out-last-scanline vertical-coefficients v
                                                     scanline-scatter-buffer width-times-channels)

                   ;; update the end of the buffer
                   (when (> out-last-scanline (%info-ring-buffer-last-scanline info))
                     (setf (%info-ring-buffer-last-scanline info) out-last-scanline))))
               (incf contrib)
               (incf vc (%samp-coefficient-width vertical))))

    ;; now evict the scanlines that are left over in the ring buffer
    (loop while (< (%info-ring-buffer-first-scanline info) end-output-y)
          do (funcall handle-scanline-for-scatter info))

    ;; update the end_input_y if we do multiple resizes with the same data
    (incf last-input-y)
    (when (> (%info-end-input-y info) last-input-y)
      (setf (%info-end-input-y info) last-input-y))))

;;----------------------------------------------------------------------------------
;; Samplers setup
;;----------------------------------------------------------------------------------

(defun %stbir-set-sampler (samp always-gather)
  ;; set filter (STBIR_FILTER_DEFAULT)
  (let ((filter +stbir-filter-mitchell+)) ; default to downsample
    (when (>= (%samp-scale samp) (- 1.0 +stbir-small-float+))
      (if (and (<= (%samp-scale samp) (+ 1.0 +stbir-small-float+))
               (= (fceiling (%samp-pixel-shift samp)) (%samp-pixel-shift samp)))
          (setf filter +stbir-filter-point-sample+)
          (setf filter +stbir-filter-catmullrom+)))
    (setf (%samp-filter-enum samp) filter)
    (setf (%samp-filter-kernel samp) (cond ((= filter +stbir-filter-catmullrom+) #'%stbir-filter-catmullrom)
                                           ((= filter +stbir-filter-mitchell+) #'%stbir-filter-mitchell)
                                           (t #'%stbir-filter-point)))
    (setf (%samp-filter-support samp) (if (= filter +stbir-filter-point-sample+) #'%stbir-support-zeropoint5 #'%stbir-support-two)))

  (setf (%samp-filter-pixel-width samp) (%stbir-get-filter-pixel-width (%samp-filter-support samp) (%samp-scale samp)))
  ;; Gather is always better, but in extreme downsamples, you have to most or all of the data in memory
  ;;    For horizontal, we always have all the pixels, so we always use gather here (always_gather==1).
  ;;    For vertical, we use gather if scaling up (which means we will have samp->filter_pixel_width
  ;;    scanlines in memory at once).
  (setf (%samp-is-gather samp) 0)
  (cond ((>= (%samp-scale samp) (- 1.0 +stbir-small-float+))
         (setf (%samp-is-gather samp) 1))
        ((or always-gather (<= (%samp-filter-pixel-width samp) +stbir-force-gather-filter-scanlines-amount+))
         (setf (%samp-is-gather samp) 2)))

  ;; pre calculate stuff based on the above
  (setf (%samp-coefficient-width samp) (%stbir-get-coefficient-width samp (%samp-is-gather samp)))

  ;; This is how much to expand buffers to account for filters seeking outside
  ;; the image boundaries.
  (setf (%samp-filter-pixel-margin samp) (floor (%samp-filter-pixel-width samp) 2))

  (setf (%samp-num-contributors samp) (%stbir-get-contributors samp (%samp-is-gather samp)))

  (when (= (%samp-is-gather samp) 0)
    (setf (%samp-gather-prescatter-coefficient-width samp) (%samp-filter-pixel-width samp))
    (setf (%samp-gather-prescatter-num-contributors samp) (%stbir-get-contributors samp 2))))

(defun %stbir-get-conservative-extents (samp)
  (let ((scale (%samp-scale samp))
        (out-shift (%samp-pixel-shift samp))
        (support (%samp-filter-support samp))
        (input-full-size (%samp-input-full-size samp))
        (inv-scale (%samp-inv-scale samp))
        (n0 0) (n1 0))
    (if (= (%samp-is-gather samp) 1)
        (let ((out-filter-radius (* (funcall support inv-scale) scale)))
          (setf n0 (%stbir-calculate-in-pixel-range 0.5 out-filter-radius inv-scale out-shift))
          (setf n1 (nth-value 1 (%stbir-calculate-in-pixel-range (+ (float (1- (%samp-output-sub-size samp))) 0.5)
                                                                 out-filter-radius inv-scale out-shift))))
        ;; downsample gather, refine
        (let* ((in-pixels-radius (* (funcall support scale) inv-scale))
               (filter-pixel-margin (%samp-filter-pixel-margin samp))
               (output-sub-size (%samp-output-sub-size samp)))
          ;; get a conservative area of the input range
          (setf n0 (%stbir-calculate-in-pixel-range 0.0 0.0 inv-scale out-shift))
          (setf n1 (nth-value 1 (%stbir-calculate-in-pixel-range (float output-sub-size) 0.0 inv-scale out-shift)))

          ;; now go through the margin to the start of area to find bottom
          (let ((n (1+ n0))
                (input-end (- filter-pixel-margin)))
            (loop while (>= n input-end)
                  do (multiple-value-bind (out-first-pixel out-last-pixel)
                         (%stbir-calculate-out-pixel-range (+ (float n) 0.5) in-pixels-radius scale out-shift output-sub-size)
                       (when (> out-first-pixel out-last-pixel) (return))
                       (when (or (< out-first-pixel output-sub-size) (>= out-last-pixel 0))
                         (setf n0 n))
                       (decf n))))

          ;; now go through the end of the area through the margin to find top
          (let* ((n (1- n1))
                 (input-end (+ n 1 filter-pixel-margin)))
            (loop while (<= n input-end)
                  do (multiple-value-bind (out-first-pixel out-last-pixel)
                         (%stbir-calculate-out-pixel-range (+ (float n) 0.5) in-pixels-radius scale out-shift output-sub-size)
                       (when (> out-first-pixel out-last-pixel) (return))
                       (when (or (< out-first-pixel output-sub-size) (>= out-last-pixel 0))
                         (setf n1 n))
                       (incf n))))))

    ;; for non-edge-wrap modes, we never read over the edge, so clamp
    (when (< n0 0) (setf n0 0))
    (when (>= n1 input-full-size) (setf n1 (1- input-full-size)))
    (values n0 n1)))

;; there are six resize classifications: 0 == vertical scatter, 1 == vertical gather < 1x scale, 2 == vertical gather 1x-2x scale, 4 == vertical gather < 3x scale, 4 == vertical gather > 3x scale, 5 == <=4 pixel height, 6 == <=4 pixel wide column
(defparameter +stbir-compute-weights+ ; 5 = 0=1chan, 1=2chan, 2=3chan, 3=4chan, 4=7chan
  #(#(#(1.00000 1.00000 0.31250 1.00000) #(0.56250 0.59375 0.00000 0.96875) #(1.00000 0.06250 0.00000 1.00000)
      #(0.00000 0.09375 1.00000 1.00000) #(1.00000 1.00000 0.31250 1.00000) #(0.03125 0.12500 1.00000 1.00000)
      #(1.00000 1.00000 0.06250 1.00000) #(0.00000 1.00000 0.00000 0.03125))
    #(#(0.00000 0.84375 0.00000 0.03125) #(0.09375 0.93750 0.00000 0.78125) #(0.87500 0.21875 0.00000 0.96875)
      #(0.09375 0.09375 1.00000 1.00000) #(0.00000 0.84375 0.00000 0.03125) #(0.03125 0.12500 1.00000 1.00000)
      #(1.00000 1.00000 0.06250 1.00000) #(0.00000 1.00000 0.00000 0.53125))
    #(#(0.00000 0.53125 0.00000 0.03125) #(0.06250 0.96875 0.00000 0.53125) #(0.87500 0.18750 0.00000 0.93750)
      #(0.00000 0.09375 1.00000 1.00000) #(0.00000 0.53125 0.00000 0.03125) #(0.03125 0.12500 1.00000 1.00000)
      #(1.00000 1.00000 0.06250 1.00000) #(0.00000 1.00000 0.00000 0.56250))
    #(#(0.00000 0.50000 0.00000 0.71875) #(0.06250 0.84375 0.00000 0.87500) #(1.00000 0.50000 0.50000 0.96875)
      #(1.00000 0.09375 0.31250 0.50000) #(0.00000 0.50000 0.00000 0.71875) #(1.00000 0.03125 0.03125 0.53125)
      #(1.00000 1.00000 0.06250 1.00000) #(0.00000 1.00000 0.03125 0.18750))
    #(#(0.00000 0.59375 0.00000 0.96875) #(0.06250 0.81250 0.06250 0.59375) #(0.75000 0.43750 0.12500 0.96875)
      #(0.87500 0.06250 0.18750 0.43750) #(0.00000 0.59375 0.00000 0.96875) #(0.15625 0.12500 1.00000 1.00000)
      #(1.00000 1.00000 0.06250 1.00000) #(0.00000 1.00000 0.03125 0.34375))))

;; Figure out whether to scale along the horizontal or vertical first.
(defun %stbir-should-do-vertical-first (weights-table horizontal-filter-pixel-width horizontal-scale horizontal-output-size
                                        vertical-filter-pixel-width vertical-scale vertical-output-size is-gather)
  (let* ((v-classification
           ;; categorize the resize into buckets
           (cond ((or (<= vertical-output-size 4) (<= horizontal-output-size 4))
                  (if (< vertical-output-size horizontal-output-size) 6 7))
                 ((and (not is-gather) (or (<= vertical-output-size 16) (<= horizontal-output-size 16)))
                  4)
                 ((<= vertical-scale 1.0) (if is-gather 1 0))
                 ((<= vertical-scale 2.0) 2)
                 ((<= vertical-scale 3.0) 3)
                 (t 5)))                ; everything bigger than 3x
         ;; use the right weights
         (weights (svref weights-table v-classification))
         ;; this is the costs when you don't take into account modern CPUs with high ipc and simd and caches - wish we had a better estimate
         (h-cost (+ (* (float horizontal-filter-pixel-width) (svref weights 0))
                    (* horizontal-scale (float vertical-filter-pixel-width) (svref weights 1))))
         (v-cost (+ (* (float vertical-filter-pixel-width) (svref weights 2))
                    (* vertical-scale (float horizontal-filter-pixel-width) (svref weights 3)))))
    ;; use computation estimate to decide vertical first or not
    (<= v-cost h-cost)))

;; converts a double to a rational that has less than one float bit of error (returns nil if unable to do so)
(defun %stbir-double-to-rational (f limit limit-denom)
  (let ((top (truncate (* f (float (ash 1 25) 1d0)))) ; scale to past float error range
        (bot (ash 1 25))
        (numer-last 0) (denom-last 1)
        (numer-estimate 1) (denom-estimate 0)
        (epsilon (/ 1d0 (float (ash 1 24) 1d0))))
    ;; keep refining, but usually stops in a few loops - usually 5 for bad cases
    (loop
      ;; hit limit, break out and do best full range estimate
      (when (>= (if limit-denom denom-estimate numer-estimate) limit)
        (return))
      ;; is the current error less than 1 bit of a float? if so, we're done
      (when (/= denom-estimate 0)
        (when (< (abs (- (/ (float numer-estimate 1d0) (float denom-estimate 1d0)) f)) epsilon)
          ;; yup, found it
          (return-from %stbir-double-to-rational (values t numer-estimate denom-estimate))))
      ;; no more refinement bits left? break out and do full range estimate
      (when (= bot 0) (return))
      ;; gcd the estimate bits
      (multiple-value-bind (est temp) (floor top bot)
        (setf top bot bot temp)
        ;; move remainders
        (psetf denom-estimate (+ (* est denom-estimate) denom-last) denom-last denom-estimate)
        (psetf numer-estimate (+ (* est numer-estimate) numer-last) numer-last numer-estimate)))

    ;; we didn't find anything good enough for float, use a full range estimate
    (if limit-denom
        (setf numer-estimate (truncate (+ (* f (float limit 1d0)) 0.5d0))
              denom-estimate limit)
        (setf numer-estimate limit
              denom-estimate (truncate (+ (/ (float limit 1d0) f) 0.5d0))))
    (let* ((numer (ldb (byte 32 0) numer-estimate))
           (denom (ldb (byte 32 0) denom-estimate))
           (err (if (/= denom-estimate 0) (abs (- (/ (float numer 1d0) (float denom 1d0)) f)) 1d0)))
      (values (< err epsilon) numer denom))))

;; NOTE: the region is always the whole input onto the whole output (input_s0 = 0, input_s1 = 1)
(defun %stbir-calculate-region-transform (samp output-full-range input-full-range)
  (let* ((output-range (float output-full-range 1d0))
         (input-range (float input-full-range 1d0))
         (output-s (/ (float output-full-range 1d0) output-range))
         (input-s 1d0)
         ;; figure out the scaling to use
         (ratio (/ output-s input-s))
         ;; save scale before clipping
         (scale (* (/ output-range input-range) ratio)))
    (setf (%samp-scale samp) (float scale 1.0)
          (%samp-inv-scale samp) (float (/ 1d0 scale) 1.0))
    ;; calculate and store the starting source offsets in output pixel space
    (setf (%samp-pixel-shift samp) (float (* 0d0 ratio output-range) 1.0))
    (multiple-value-bind (rational numer denom)
        (%stbir-double-to-rational scale (if (<= scale 1d0) output-full-range input-full-range) (>= scale 1d0))
      (setf (%samp-scale-is-rational samp) rational
            (%samp-scale-numerator samp) numer
            (%samp-scale-denominator samp) denom))
    (setf (%samp-input-full-size samp) input-full-range
          (%samp-output-sub-size samp) output-full-range)))

;;----------------------------------------------------------------------------------
;; stbir_resize_uint8_linear()
;;----------------------------------------------------------------------------------

;; CHANNELS is the stbir_pixel_layout raylib passes: 1-3 channels, or 4 for STBIR_RGBA
(defun %stbir-resize-uint8-linear (input-pixels input-w input-h output-w output-h channels)
  "Resize 8-bit image data (stbir_resize_uint8_linear), returns the new pixel data"
  (let* ((output (make-array (* (max output-w 0) (max output-h 0) channels) :element-type '(unsigned-byte 8) :initial-element 0))
         (horizontal (make-%stbir-sampler))
         (vertical (make-%stbir-sampler))
         (info (make-%stbir-info)))
    (when (or (<= output-w 0) (<= output-h 0) (<= input-w 0) (<= input-h 0))
      (return-from %stbir-resize-uint8-linear output))

    ;; do horizontal and vertical scale calcs
    (%stbir-calculate-region-transform horizontal output-w input-w)
    (%stbir-calculate-region-transform vertical output-h input-h)

    (%stbir-set-sampler horizontal t)
    (multiple-value-bind (conservative-n0 conservative-n1) (%stbir-get-conservative-extents horizontal)
      (%stbir-set-sampler vertical nil)

      ;; stbir__alloc_internal_mem_and_build_samplers()
      (let* ((alpha (and (= channels 4) ; first figure out what type of alpha weighting to use (if any)
                         (or (/= (%samp-filter-enum horizontal) +stbir-filter-point-sample+)
                             (/= (%samp-filter-enum vertical) +stbir-filter-point-sample+)))) ; no alpha weighting on point sampling
             (effective-channels (if alpha 7 channels))
             (vertical-first (%stbir-should-do-vertical-first
                              (svref +stbir-compute-weights+ (ecase effective-channels (1 0) (2 1) (3 2) (4 3) (7 4)))
                              (%samp-filter-pixel-width horizontal) (%samp-scale horizontal) (%samp-output-sub-size horizontal)
                              (%samp-filter-pixel-width vertical) (%samp-scale vertical) (%samp-output-sub-size vertical)
                              (/= (%samp-is-gather vertical) 0)))
             ;; extra floats for input callback stagger
             (decode-buffer-size (+ (* (1+ (- conservative-n1 conservative-n0)) effective-channels) +stbir-input-callback-padding+))
             (ring-buffer-length (if vertical-first
                                     decode-buffer-size
                                     (+ (* (%samp-output-sub-size horizontal) effective-channels) +stbir-input-callback-padding+)))
             ;; One extra entry because floating point precision problems sometimes cause an extra to be necessary.
             (alloc-ring-buffer-num-entries (1+ (%samp-filter-pixel-width vertical)))
             (vertical-buffer-size (1+ (* (%samp-output-sub-size horizontal) effective-channels)))
             (copy-horizontal nil)
             (possibly-use-horizontal-for-pivot nil))
        ;; we never need more ring buffer entries than the scanlines we're outputting when in scatter mode
        (when (and (= (%samp-is-gather vertical) 0) (> alloc-ring-buffer-num-entries output-h))
          (setf alloc-ring-buffer-num-entries output-h))

        (setf (%info-channels info) channels
              (%info-effective-channels info) effective-channels
              (%info-vertical-first info) vertical-first
              (%info-alpha-weight info) alpha
              (%info-alpha-unweight info) alpha
              ;; decode/encode 0-255.0 instead of 0-1.0 when not alpha weighting
              (%info-decode-scaled info) alpha)

        ;; get all the per-split buffers (NOTE: 2 extra floats, horizontal gathers may read past the scanline)
        (setf (%info-decode-buffer info) (%stbir-make-floats (+ decode-buffer-size 2)))
        (setf (%info-ring-buffers info) (coerce (loop repeat alloc-ring-buffer-num-entries
                                                      collect (%stbir-make-floats (+ ring-buffer-length 2)))
                                                'simple-vector))
        (setf (%info-vertical-buffer info) (%stbir-make-floats (+ vertical-buffer-size 2)))

        ;; alloc memory for to-be-pivoted coeffs (if necessary)
        (when (= (%samp-is-gather vertical) 0)
          (setf (%samp-gather-prescatter-contributors vertical) (%stbir-make-contribs (%samp-gather-prescatter-num-contributors vertical))
                (%samp-gather-prescatter-coefficients vertical)
                (%stbir-make-floats (* (%samp-gather-prescatter-num-contributors vertical) (%samp-gather-prescatter-coefficient-width vertical)))))

        (setf (%samp-contributors horizontal) (%stbir-make-contribs (%samp-num-contributors horizontal))
              (%samp-coefficients horizontal) (%stbir-make-floats (+ (* (%samp-num-contributors horizontal) (%samp-coefficient-width horizontal))
                                                                     +stbir-input-callback-padding+)))

        ;; are the two filters identical?? (happens a lot with mipmap generation)
        (when (and (eq (%samp-filter-kernel horizontal) (%samp-filter-kernel vertical))
                   (eq (%samp-filter-support horizontal) (%samp-filter-support vertical))
                   (= (%samp-output-sub-size horizontal) (%samp-output-sub-size vertical)))
          (let ((diff-scale (abs (- (%samp-scale horizontal) (%samp-scale vertical))))
                (diff-shift (abs (- (%samp-pixel-shift horizontal) (%samp-pixel-shift vertical)))))
            (when (and (<= diff-scale +stbir-small-float+) (<= diff-shift +stbir-small-float+))
              (if (= (%samp-is-gather horizontal) (%samp-is-gather vertical))
                  (setf copy-horizontal t)
                  ;; everything matches, but vertical is scatter, horizontal is gather, use horizontal coeffs for vertical pivot coeffs
                  (setf possibly-use-horizontal-for-pivot horizontal)))))

        (unless copy-horizontal
          (setf (%samp-contributors vertical) (%stbir-make-contribs (%samp-num-contributors vertical))
                (%samp-coefficients vertical) (%stbir-make-floats (+ (* (%samp-num-contributors vertical) (%samp-coefficient-width vertical))
                                                                     +stbir-input-callback-padding+))))

        (%stbir-calculate-filters horizontal nil)

        (setf (%info-conservative-n0 info) conservative-n0
              (%info-conservative-n1 info) conservative-n1)

        ;; get exact extents
        (%stbir-get-extents horizontal info)

        ;; pack the horizontal coeffs
        (setf (%samp-coefficient-width horizontal)
              (%stbir-pack-coefficients (%samp-num-contributors horizontal) (%samp-contributors horizontal) (%samp-coefficients horizontal)
                                        (%samp-coefficient-width horizontal) (%samp-widest horizontal) conservative-n1))

        (setf (%info-horizontal info) horizontal)
        (if copy-horizontal
            (setf (%info-vertical info) (copy-%stbir-sampler horizontal))
            (progn
              (%stbir-calculate-filters vertical possibly-use-horizontal-for-pivot)
              (setf (%info-vertical info) vertical)))

        ;; setup the vertical split ranges (one split)
        (let ((vertical (%info-vertical info)))
          (setf (%info-start-output-y info) 0
                (%info-end-output-y info) (%samp-output-sub-size vertical)
                ;; scatter range (updated to minimum as you run it)
                (%info-start-input-y info) (- (%samp-filter-pixel-margin vertical))
                (%info-end-input-y info) (+ (%samp-input-full-size vertical) (%samp-filter-pixel-margin vertical)))

          ;; now we know precisely how many entries we need
          (setf (%info-ring-buffer-num-entries info) (%samp-widest vertical))
          ;; we never need more ring buffer entries than the scanlines we're outputting
          (when (and (= (%samp-is-gather vertical) 0) (> (%info-ring-buffer-num-entries info) output-h))
            (setf (%info-ring-buffer-num-entries info) output-h))))

      ;; stbir__update_info_from_resize()
      (setf (%info-input-data info) input-pixels
            (%info-input-stride-bytes info) (* channels input-w)
            (%info-output-data info) output
            (%info-output-stride-bytes info) (* channels output-w))

      ;; stbir__perform_resize()
      (if (/= (%samp-is-gather (%info-vertical info)) 0)
          (%stbir-vertical-gather-loop info)
          (%stbir-vertical-scatter-loop info)))
    output))
