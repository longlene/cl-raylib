(in-package #:cl-user)

#+sbcl
(declaim (sb-ext:muffle-conditions sb-kernel:character-decoding-error-in-comment))

(defpackage #:cl-raylib-asd
  (:use :cl :asdf))

(in-package :cl-raylib-asd)

(defsystem #:cl-raylib
  :version "0.0.1"
  :author "loong0"
  :license "MIT"
  :description "Common Lisp bindings of libraylib"
  :depends-on (#:cffi-libffi
	       #:alexandria)
  :serial t
  :pathname "src"
  :components
  ((:file "package")
   (:file "library")
   (:file "util")
   (:file "raylib")
   (:file "macro"))
  :in-order-to ((test-op (test-op cl-raylib-test))))
