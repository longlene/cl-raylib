(in-package #:cl-raylib)

;;;===================================================================================
;;; win32_clipboard - Clipboard image access on Windows, used by GetClipboardImage()
;;; Port of raylib/src/external/win32_clipboard.h
;;;
;;; NOTE: The clipboard DIB (BITMAPINFOHEADER + pixels) is returned as the bytes of a .bmp file:
;;; a BITMAPFILEHEADER is prepended, so it can be decoded with LoadImageFromMemory(".bmp", ...)
;;;===================================================================================

#+windows
(progn
  (cffi:define-foreign-library %kernel32 (:windows "kernel32.dll"))
  (cffi:define-foreign-library %user32 (:windows "user32.dll"))

  (defconstant +cf-dib+ 8)
  (defconstant +bi-bitfields+ #x0003)
  (defconstant +bi-alphabitfields+ #x0006)
  (defconstant +bitmap-file-header-size+ 14) ; sizeof(BITMAPFILEHEADER), packed
  (defconstant +bitmap-info-header-size+ 40) ; sizeof(BITMAPINFOHEADER), packed
  (defconstant +rgbquad-size+ 4)

  (defun %win32-load-libraries ()
    (unless (cffi:foreign-library-loaded-p '%kernel32) (cffi:load-foreign-library '%kernel32))
    (unless (cffi:foreign-library-loaded-p '%user32) (cffi:load-foreign-library '%user32)))

  ;; Open clipboard with a number of retries
  (defun %open-clipboard-retrying (hwnd)
    (let ((max-tries 20)
          (sleep-time-ms 60))
      (dotimes (i max-tries nil)
        (when (/= 0 (cffi:foreign-funcall "OpenClipboard" :pointer hwnd :int)) (return t))
        (cffi:foreign-funcall "Sleep" :uint32 sleep-time-ms :void))))

  ;; Get pixel data offset from DIB image
  ;; NOTE: BIH points to a BITMAPINFOHEADER: biSize(0) biWidth(4) biHeight(8) biPlanes(12) biBitCount(14)
  ;; biCompression(16) biSizeImage(20) biXPelsPerMeter(24) biYPelsPerMeter(28) biClrUsed(32) biClrImportant(36)
  (defun %get-pixel-data-offset (bih)
    (let ((offset 0)
          (bi-size (cffi:mem-ref bih :uint32 0))
          (bi-bit-count (cffi:mem-ref bih :uint16 14))
          (bi-compression (cffi:mem-ref bih :uint32 16))
          (bi-clr-used (cffi:mem-ref bih :uint32 32)))
      (when (= bi-size +bitmap-info-header-size+)
        (when (> bi-bit-count 8)
          (cond ((= bi-compression +bi-bitfields+) (incf offset (* 3 +rgbquad-size+)))
                ((= bi-compression +bi-alphabitfields+) (incf offset (* 4 +rgbquad-size+))))) ; Not widely supported, but valid
        (if (> bi-clr-used 0)
            (incf offset (* bi-clr-used +rgbquad-size+))
            (when (< bi-bit-count 16)
              (setf offset (+ offset (ash +rgbquad-size+ bi-bit-count))))))
      (+ bi-size offset)))

  (defun win32-get-clipboard-image-data ()
    "Clipboard image as the bytes of a .bmp file, returns (values bmp-data width height) or NIL"
    (%win32-load-libraries)
    (let ((bmp-data nil) (width 0) (height 0))
      (if (%open-clipboard-retrying (cffi:null-pointer))
          (let ((clip-handle (cffi:foreign-funcall "GetClipboardData" :uint +cf-dib+ :pointer)))
            (if (not (cffi:null-pointer-p clip-handle))
                (let ((bmp-info-header (cffi:foreign-funcall "GlobalLock" :pointer clip-handle :pointer)))
                  (if (not (cffi:null-pointer-p bmp-info-header))
                      (let ((clip-data-size (cffi:foreign-funcall "GlobalSize" :pointer clip-handle :size)))
                        (setf width (cffi:mem-ref bmp-info-header :int32 4)
                              height (cffi:mem-ref bmp-info-header :int32 8))
                        (if (and (>= clip-data-size +bitmap-info-header-size+) (< clip-data-size #x7fffffff)) ; INT_MAX
                            (let* ((pixel-offset (%get-pixel-data-offset bmp-info-header))
                                   (bmp-file-size (+ +bitmap-file-header-size+ clip-data-size))
                                   (data (make-array bmp-file-size :element-type '(unsigned-byte 8) :initial-element 0)))
                              ;; BITMAPFILEHEADER: bfType, bfSize, bfReserved1, bfReserved2, bfOffBits (little endian)
                              (flet ((put (offset value bytes)
                                       (dotimes (i bytes) (setf (aref data (+ offset i)) (ldb (byte 8 (* 8 i)) value)))))
                                (put 0 #x4D42 2)                                      ; BMP file type constant
                                (put 2 bmp-file-size 4)                               ; Up to 4GB works fine
                                (put 10 (+ +bitmap-file-header-size+ pixel-offset) 4))
                              ;; Add BMP info header and pixel data
                              (dotimes (i clip-data-size)
                                (setf (aref data (+ +bitmap-file-header-size+ i)) (cffi:mem-aref bmp-info-header :uint8 i)))
                              (setf bmp-data data)
                              (cffi:foreign-funcall "GlobalUnlock" :pointer clip-handle :int)
                              (cffi:foreign-funcall "CloseClipboard" :int)
                              (trace-log +log-info+ "Clipboad image acquired successfully"))
                            (progn
                              (trace-log +log-warning+ "Clipboard data is not supported (>2GB?)")
                              (cffi:foreign-funcall "GlobalUnlock" :pointer clip-handle :int)
                              (cffi:foreign-funcall "CloseClipboard" :int))))
                      (progn
                        (trace-log +log-warning+ "Clipboard data failed to be locked")
                        (cffi:foreign-funcall "GlobalUnlock" :pointer clip-handle :int)
                        (cffi:foreign-funcall "CloseClipboard" :int))))
                (progn
                  (trace-log +log-warning+ "Clipboard data is not an image")
                  (cffi:foreign-funcall "CloseClipboard" :int))))
          (trace-log +log-warning+ "Clipboard can not be opened"))
      (values bmp-data width height))))
