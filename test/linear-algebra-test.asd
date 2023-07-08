;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(defsystem "linear-algebra-test"
  :description "Unit Tests for Linear Algebra in Common Lisp"
  :version "0.1.1"
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :maintainer "Steve Nunez <steve@symbolics.tech>"
  :maintainer "Brian Eberman <bseberman@gmail.com>"
  :license :MS-PL
  :depends-on ("lisp-unit" "linear-algebra")
  :components
  ((:file "pkgdcl")
   ;; Linear algebra kernel tests
   (:module kernel
    :depends-on ("pkgdcl")
    :components
    ((:file "utility")
     (:file "permute")
     (:file "unary-operations")
     (:file "binary-operations")
     (:file "rotation")
     (:file "gauss")
     (:file "cholesky")
     (:file "conjugate-gradient")
     (:file "tridiagonal")))
   ;; Linear algebra interface
   (:module interface
    :depends-on ("pkgdcl")
    :components
    ((:file "matrix")
     (:file "identity-matrix" :depends-on ("matrix"))
     (:file "permutation-matrix" :depends-on ("matrix"))))
   ;; Common lisp sequence tests
   (:module sequence
    :depends-on ("linear-algebra-test")
    :components
    ((:file "list")
     (:file "vector")
     (:file "array")))
   ;; Linear algebra tests
   (:file "data-vector" :depends-on ("pkgdcl"))
   (:file "dense-matrix" :depends-on ("interface"))
   (:file "square-matrix" :depends-on ("interface"))
   (:file "hermitian-matrix" :depends-on ("interface"))
   (:file "symmetric-matrix" :depends-on ("interface"))))
