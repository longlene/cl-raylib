(in-package #:cl-raylib)

(defmacro with-window ((width height title) &body body)
 `(progn (init-window ,width ,height ,title)
         (unwind-protect (progn ,@body)
          (close-window))))

(defmacro with-drawing (&body body)
 `(progn (begin-drawing)
         (unwind-protect (progn ,@body)
          (end-drawing))))
