(in-package #:cl-raylib)

(defmacro with-window ((width height title) &body body)
 `(progn (init-window ,width ,height ,title)
         (unwind-protect (progn ,@body)
          (close-window))))

(defmacro with-drawing (&body body)
 `(progn (begin-drawing)
         (unwind-protect (progn ,@body)
          (end-drawing))))

(defmacro with-mode-2d ((camera) &body body)
 `(progn (begin-mode-2d ,camera)
         (unwind-protect (progn ,@body)
          (end-mode-2d))))

(defmacro with-mode-3d ((camera) &body body)
 `(progn (begin-mode-3d ,camera)
         (unwind-protect (progn ,@body)
          (end-mode-3d))))

(defmacro with-texture-mode ((target) &body body)
 `(progn (begin-texture-mode ,target)
         (unwind-protect (progn ,@body)
          (end-texture-mode))))

(defmacro with-shader-mode ((shader) &body body)
 `(progn (begin-shader-mode ,shader)
         (unwind-protect (progn ,@body)
          (end-shader-mode))))

(defmacro with-blend-mode ((mode) &body body)
 `(progn (begin-blend-mode ,mode)
         (unwind-protect (progn ,@body)
          (end-blend-mode))))

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
