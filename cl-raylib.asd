#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(asdf:defsystem #:cl-raylib
  :version "0.0.1"
  :author "loong0"
  :license "MIT"
  :description "Common Lisp bindings of libraylib"
  :depends-on (#:cffi-libffi
               #:alexandria
               #:3d-vectors
               #:3d-matrices)
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "util")
   (:file "library")
   (:file "raylib")
   (:file "rlgl")
   (:file "macro")))
