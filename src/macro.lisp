(in-package #:cl-raylib)

(defmacro with-window ((width height title) &body body)
 `(progn (init-window ,width ,height ,title)
         (unwind-protect (progn ,@body)
          (close-window))))

(defmacro with-drawing (&body body)
 `(progn (begin-drawing)
         (unwind-protect (progn ,@body)
          (end-drawing))))

(defmacro with-2d-mode ((camera) &body body)
 `(progn (begin-2d-mode ,camera)
         (unwind-protect (progn ,@body)
          (end-2d-mode))))

(defmacro with-3d-mode ((camera) &body body)
 `(progn (begin-3d-mode ,camera)
         (unwind-protect (progn ,@body)
          (end-3d-mode))))

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

