(in-package #:cl-user)

(defpackage #:cl-raylib-asd
  (:use :cl :asdf))

(in-package :cl-raylib-asd)

(defsystem #:cl-raylib
  :version "0.0.1"
  :author "loong0"
  :license "MIT"
  :description "Common Lisp bindings of libraylib"
  :depends-on (#:cffi-libffi)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "library")
   (:file "type")
   (:file "raylib"))
  :in-order-to ((test-op (test-op cl-raylib-test))))
