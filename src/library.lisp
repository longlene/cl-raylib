(in-package #:cl-raylib)
(define-foreign-library libraylib
  (:darwin "libraylib.dylib")
  (:unix "libraylib.so")
  (:windows (:or "raylib.dll" "libraylib.dll"))
  (t (:default "libraylib")))

(unless (foreign-library-loaded-p 'libraylib)
  (use-foreign-library libraylib))

