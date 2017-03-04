(in-package #:cl-raylib)

(define-foreign-library libraylib
    (:unix "libraylib")
  (:windows "raylib.dll")
  (t (:default "libraylib")))

(unless (foreign-library-loaded-p 'libraylib)
  (use-foreign-library libraylib))
