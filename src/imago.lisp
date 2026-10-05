(in-package #:cl-raylib)

;;;; Optional image formats through imago (system cl-raylib/imago)
;;;;
;;;; raylib config.h disables JPG, TGA and PNM by default (SUPPORT_FILEFORMAT_JPG...); loading this
;;;; system registers them in *image-loaders* / *image-exporters* (textures.lisp), like building
;;;; raylib with those formats enabled. Not part of the core system: imago pulls in serapeum,
;;;; which does not build on every implementation.

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

(defun %read-imago-from-memory (reader file-data)
  "Decode FILE-DATA with an imago stream reader
   NOTE: imago readers require a file stream (they use file-length), so data goes through a temporary file"
  (uiop:with-temporary-file (:stream out :pathname path :element-type '(unsigned-byte 8))
    (write-sequence file-data out)
    (finish-output out)
    (with-open-file (in path :element-type '(unsigned-byte 8))
      (funcall reader in))))

(defun %load-imago-jpg (file-data)
  (let ((im (%read-imago-from-memory #'imago:read-jpg-from-stream file-data)))
    (%imago->image im (if (typep im 'imago:grayscale-image) 1 3))))

(defun %load-imago-tga (file-data)
  (%imago->image (%read-imago-from-memory #'imago:read-tga-from-stream file-data)
                 (if (= (aref file-data 16) 32) 4 3)))

(defun %load-imago-pnm (channels)
  (lambda (file-data)
    (%imago->image (%read-imago-from-memory #'imago:read-pnm-from-stream file-data) channels)))

(defun %export-imago-jpg (file-name data w h channels)
  (let ((rgb (imago:make-rgb-image w h)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((s (* (+ (* y w) x) channels)))
          (setf (imago:image-pixel rgb x y)
                (if (< channels 3)
                    (imago:make-color (aref data s) (aref data s) (aref data s))
                    (imago:make-color (aref data s) (aref data (+ s 1)) (aref data (+ s 2))))))))
    (imago:write-jpg rgb file-name)
    t))

(loop for (ext . fn) in (list (cons ".jpg" #'%load-imago-jpg) (cons ".jpeg" #'%load-imago-jpg)
                              (cons ".tga" #'%load-imago-tga)
                              (cons ".ppm" (%load-imago-pnm 3)) (cons ".pgm" (%load-imago-pnm 1)))
      do (setf *image-loaders* (cons (cons ext fn) (remove ext *image-loaders* :key #'car :test #'string-equal))))

(loop for ext in '(".jpg" ".jpeg")
      do (setf *image-exporters* (cons (cons ext #'%export-imago-jpg)
                                       (remove ext *image-exporters* :key #'car :test #'string-equal))))
