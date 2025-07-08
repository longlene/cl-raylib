(in-package #:cl-raylib)

;;; Image and Texture System
;;; This module handles image loading, processing, and GPU texture management

;;; Constants for image formats
(defconstant +pixelformat-uncompressed-grayscale+ 1)
(defconstant +pixelformat-uncompressed-gray-alpha+ 2)
(defconstant +pixelformat-uncompressed-r5g6b5+ 3)
(defconstant +pixelformat-uncompressed-rgb+ 4)
(defconstant +pixelformat-uncompressed-rgba+ 5)

;;; Image generation functions

(defun gen-image-color (width height color)
  "Generate image: plain color"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)
                          :initial-element 0))
         (r (color-r color))
         (g (color-g color))
         (b (color-b color))
         (a (color-a color)))
    ;; Fill image with specified color
    (loop for i from 0 below pixel-count do
      (let ((base (* i 4)))
        (setf (aref data base) r)
        (setf (aref data (+ base 1)) g)
        (setf (aref data (+ base 2)) b)
        (setf (aref data (+ base 3)) a)))
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-gradient-linear (width height direction start-color end-color)
  "Generate image: linear gradient, direction in degrees [0..360], 0=Vertical gradient"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (angle (* direction +deg2rad+))
         (cos-a (cos angle))
         (sin-a (sin angle)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((nx (/ x (1- width)))
               (ny (/ y (1- height)))
               ;; Calculate gradient factor based on direction
               (factor (+ (* nx cos-a) (* ny sin-a)))
               (factor (clamp factor 0.0 1.0))
               (inv-factor (- 1.0 factor))
               
               ;; Interpolate colors
               (r (round (+ (* (color-r start-color) inv-factor)
                           (* (color-r end-color) factor))))
               (g (round (+ (* (color-g start-color) inv-factor)
                           (* (color-g end-color) factor))))
               (b (round (+ (* (color-b start-color) inv-factor)
                           (* (color-b end-color) factor))))
               (a (round (+ (* (color-a start-color) inv-factor)
                           (* (color-a end-color) factor))))
               
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) r)
          (setf (aref data (+ base 1)) g)
          (setf (aref data (+ base 2)) b)
          (setf (aref data (+ base 3)) a))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-gradient-radial (width height density inner-color outer-color)
  "Generate image: radial gradient"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0))
         (max-radius (* (min width height) 0.5 density)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((dx (- x center-x))
               (dy (- y center-y))
               (distance (sqrt (+ (* dx dx) (* dy dy))))
               (factor (clamp (/ distance max-radius) 0.0 1.0))
               (inv-factor (- 1.0 factor))
               
               ;; Interpolate colors
               (r (round (+ (* (color-r inner-color) inv-factor)
                           (* (color-r outer-color) factor))))
               (g (round (+ (* (color-g inner-color) inv-factor)
                           (* (color-g outer-color) factor))))
               (b (round (+ (* (color-b inner-color) inv-factor)
                           (* (color-b outer-color) factor))))
               (a (round (+ (* (color-a inner-color) inv-factor)
                           (* (color-a outer-color) factor))))
               
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) r)
          (setf (aref data (+ base 1)) g)
          (setf (aref data (+ base 2)) b)
          (setf (aref data (+ base 3)) a))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

(defun gen-image-checked (width height checks-x checks-y col1 col2)
  "Generate image: checked pattern"
  (let* ((pixel-count (* width height))
         (data (make-array (* pixel-count 4) 
                          :element-type '(unsigned-byte 8)))
         (check-width (/ width checks-x))
         (check-height (/ height checks-y)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below width do
        (let* ((check-x (floor (/ x check-width)))
               (check-y (floor (/ y check-height)))
               (color (if (evenp (+ check-x check-y)) col1 col2))
               (base (* (+ (* y width) x) 4)))
          
          (setf (aref data base) (color-r color))
          (setf (aref data (+ base 1)) (color-g color))
          (setf (aref data (+ base 2)) (color-b color))
          (setf (aref data (+ base 3)) (color-a color)))))
    
    (make-image :data data
                :width width
                :height height
                :format +pixelformat-uncompressed-rgba+)))

;;; Image manipulation functions

(defun image-copy (image)
  "Create an image duplicate"
  (let ((new-data (make-array (length (image-data image))
                             :element-type '(unsigned-byte 8))))
    ;; Copy data
    (replace new-data (image-data image))
    
    (make-image :data new-data
                :width (image-width image)
                :height (image-height image)
                :mipmaps (image-mipmaps image)
                :format (image-format image))))

(defun image-from-image (image rec)
  "Create an image from another image piece"
  (let* ((src-width (image-width image))
         (src-height (image-height image))
         (src-data (image-data image))
         (crop-x (max 0 (min (round (rectangle-x rec)) (1- src-width))))
         (crop-y (max 0 (min (round (rectangle-y rec)) (1- src-height))))
         (crop-width (max 1 (min (round (rectangle-width rec)) (- src-width crop-x))))
         (crop-height (max 1 (min (round (rectangle-height rec)) (- src-height crop-y))))
         (new-data (make-array (* crop-width crop-height 4)
                              :element-type '(unsigned-byte 8))))
    
    ;; Copy cropped region
    (loop for y from 0 below crop-height do
      (loop for x from 0 below crop-width do
        (let ((src-idx (* (+ (* (+ crop-y y) src-width) (+ crop-x x)) 4))
              (dst-idx (* (+ (* y crop-width) x) 4)))
          (setf (aref new-data dst-idx) (aref src-data src-idx))
          (setf (aref new-data (+ dst-idx 1)) (aref src-data (+ src-idx 1)))
          (setf (aref new-data (+ dst-idx 2)) (aref src-data (+ src-idx 2)))
          (setf (aref new-data (+ dst-idx 3)) (aref src-data (+ src-idx 3))))))
    
    (make-image :data new-data
                :width crop-width
                :height crop-height
                :format (image-format image))))

;;; Color manipulation functions

(defun image-color-tint (image color)
  "Apply color tint to image (modifies original)"
  (let ((data (image-data image))
        (tint-r (/ (color-r color) 255.0))
        (tint-g (/ (color-g color) 255.0))
        (tint-b (/ (color-b color) 255.0))
        (tint-a (/ (color-a color) 255.0)))
    
    (loop for i from 0 below (length data) by 4 do
      (setf (aref data i) (round (* (aref data i) tint-r)))
      (setf (aref data (+ i 1)) (round (* (aref data (+ i 1)) tint-g)))
      (setf (aref data (+ i 2)) (round (* (aref data (+ i 2)) tint-b)))
      (setf (aref data (+ i 3)) (round (* (aref data (+ i 3)) tint-a))))
    
    image))

(defun image-color-grayscale (image)
  "Convert image to grayscale (modifies original)"
  (let ((data (image-data image)))
    (loop for i from 0 below (length data) by 4 do
      (let* ((r (aref data i))
             (g (aref data (+ i 1)))
             (b (aref data (+ i 2)))
             ;; Standard grayscale conversion
             (gray (round (+ (* r 0.299) (* g 0.587) (* b 0.114)))))
        (setf (aref data i) gray)
        (setf (aref data (+ i 1)) gray)
        (setf (aref data (+ i 2)) gray)))
    image))

(defun image-flip-vertical (image)
  "Flip image vertically (modifies original)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image))
         (row-size (* width 4)))
    
    (loop for y from 0 below (floor height 2) do
      (let ((top-start (* y row-size))
            (bottom-start (* (- height y 1) row-size)))
        ;; Swap rows
        (loop for i from 0 below row-size do
          (rotatef (aref data (+ top-start i))
                   (aref data (+ bottom-start i))))))
    image))

(defun image-flip-horizontal (image)
  "Flip image horizontally (modifies original)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image)))
    
    (loop for y from 0 below height do
      (loop for x from 0 below (floor width 2) do
        (let ((left-start (* (+ (* y width) x) 4))
              (right-start (* (+ (* y width) (- width x 1)) 4)))
          ;; Swap pixels
          (loop for i from 0 below 4 do
            (rotatef (aref data (+ left-start i))
                     (aref data (+ right-start i)))))))
    image))

;;; Utility functions

(defun unload-image (image)
  "Unload image from RAM"
  ;; In Lisp, we rely on garbage collection
  ;; But we can clear the data reference for immediate cleanup
  (setf (image-data image) nil)
  image)

(defun get-image-alpha-border (image threshold)
  "Get image alpha border for cropping"
  (declare (ignore threshold))
  ;; This would return a rectangle defining the alpha border
  ;; Implementation simplified for now
  (make-rectangle :x 0.0 :y 0.0
                  :width (float (image-width image))
                  :height (float (image-height image))))

;;; Helper functions for texture drawing (will be expanded)
(defun get-pixel (image x y)
  "Get pixel color from image at position (x, y)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image)))
    (if (and (>= x 0) (< x width) (>= y 0) (< y height))
        (let ((base (* (+ (* y width) x) 4)))
          (make-color (aref data base)
                      (aref data (+ base 1))
                      (aref data (+ base 2))
                      (aref data (+ base 3))))
        +blank+)))

(defun set-pixel (image x y color)
  "Set pixel color in image at position (x, y)"
  (let* ((data (image-data image))
         (width (image-width image))
         (height (image-height image)))
    (when (and (>= x 0) (< x width) (>= y 0) (< y height))
      (let ((base (* (+ (* y width) x) 4)))
        (setf (aref data base) (color-r color))
        (setf (aref data (+ base 1)) (color-g color))
        (setf (aref data (+ base 2)) (color-b color))
        (setf (aref data (+ base 3)) (color-a color)))))
  image)
