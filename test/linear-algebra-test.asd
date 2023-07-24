;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(defsystem "linear-algebra-test"
  :description "Unit Tests for Linear Algebra in Common Lisp"
  :version "0.2.0"
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :maintainer "Steve Nunez <steve@symbolics.tech>"
  :maintainer "Brian Eberman <brian@tenfactorgrowth.com>"
  :license :MS-PL
  :depends-on ("clunit2" "num-utils" "linear-algebra")
  :components
  ((:file "pkgdcl")
   (:file "suite")
   (:file "utilities")
   (:file "asserts")
   ;; Linear algebra kernel tests
   (:module kernel
    :depends-on ("pkgdcl")
    :components
    ((:file "suite")
     (:file "utility" :depends-on ("suite"))
     (:file "permute" :depends-on ("suite"))
     (:file "unary-operations" :depends-on ("suite"))
     (:file "binary-operations" :depends-on ("suite"))
     (:file "rotation" :depends-on ("suite"))
     (:file "gauss" :depends-on ("suite"))
     (:file "cholesky" :depends-on ("suite"))
     (:file "conjugate-gradient" :depends-on ("suite"))
     (:file "tridiagonal" :depends-on ("suite"))))
   ;; Matrix operations
   (:module interface
    :depends-on ("pkgdcl")
    :components
    ((:file "suite")
     (:file "matrix" :depends-on ("suite"))
     (:file "identity-matrix" :depends-on ("suite"))
     (:file "permutation-matrix" :depends-on ("suite"))))
   ;; Common lisp sequence tests
   (:module sequence
    :depends-on ("pkgdcl")
    :depends-on ("linear-algebra-test")
    :components
    ((:file "suite")
     (:file "vector" :depends-on ("suite"))
     (:file "array" :depends-on ("suite"))))
   ;; Linear algebra tests
   (:module linear-algebra
    :depends-on ("pkgdcl")
    :components
    ((:file "suite")
     (:file "data-vector" :depends-on ("suite"))
     (:file "dense-matrix" :depends-on ("suite"))
     (:file "square-matrix" :depends-on ("suite"))
     (:file "hermitian-matrix" :depends-on ("suite"))
     (:file "symmetric-matrix" :depends-on ("suite"))
     ))
   ))

