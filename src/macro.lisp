(in-package #:cl-raylib)

;;; Utility macros for cleaner code
(defmacro with-window ((width height title) &body body)
  "Convenience macro for window management"
  `(unwind-protect
        (progn
          (init-window ,width ,height ,title)
          ,@body)
     (close-window)))

(defmacro with-drawing (&body body)
  "Convenience macro for drawing"
  `(progn
     (begin-drawing)
     ,@body
     (end-drawing)))

(defmacro with-mode-2d ((camera) &body body)
  "Convenience macro for 2D drawing with camera"
  `(unwind-protect
        (progn
          (begin-mode-2d ,camera)
          ,@body)
     (end-mode-2d)))

(defmacro with-mode-3d ((camera) &body body)
  "Convenience macro for 3D drawing"
  `(unwind-protect
        (progn
          (begin-mode-3d ,camera)
          ,@body)
     (end-mode-3d)))

;;; Convenience macro for render texture mode
(defmacro with-texture-mode ((render-texture) &body body)
  "Convenience macro for render texture mode usage"
  `(unwind-protect
        (progn
          (begin-texture-mode ,render-texture)
          ,@body)
     (end-texture-mode)))

(defmacro with-shader-mode ((shader) &body body)
 `(progn (begin-shader-mode ,shader)
         (unwind-protect (progn ,@body)
          (end-shader-mode))))

(defmacro with-blend-mode ((mode) &body body)
 `(progn (begin-blend-mode ,mode)
         (unwind-protect (progn ,@body)
          (end-blend-mode))))

(defmacro with-vr-simulator (&body body)
 `(progn (init-vr-simulator)
         (unwind-protect (progn ,@body)
           (close-vr-simulator))))

(defmacro with-vr-drawing (&body body)
 `(progn (begin-vr-drawing)
         (unwind-protect (progn ,@body)
           (end-vr-drawing))))

(defmacro with-audio-device (&body body)
 `(progn (init-audio-device)
         (unwind-protect (progn ,@body)
           (close-audio-device))))

(defmacro with-audio-stream ((stream sample-rate sample-size channels) &body body)
 `(let ((,stream (load-audio-stream ,sample-rate ,sample-size ,channels)))
    (unwind-protect (progn ,@body)
      (unload-audio-stream ,stream))))

(defmacro with-sound ((sound file-name) &body body)
  `(let ((,sound (load-sound ,file-name)))
     (unwind-protect (progn ,@body)
       (unload-sound ,sound))))
