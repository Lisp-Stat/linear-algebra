;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package :asdf)

(defsystem :linear-algebra
  :description "Linear Algebra in Common Lisp."
  :version "0.1.0"
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :license "MIT"
  :depends-on ("closer-mop" "floating-point")
  :components
  ((:file "linear-algebra" :depends-on ("kernel"))
   ;; Linear algebra kernel functions
   (:module kernel
    :components
    ((:file "linear-algebra-kernel")
     (:file "utility" :depends-on ("linear-algebra-kernel"))
     (:file "permute" :depends-on ("linear-algebra-kernel"))
     (:file "unary-operations"
      :depends-on ("linear-algebra-kernel"))
     (:file "binary-operations"
      :depends-on ("linear-algebra-kernel"))
     (:file "rotation" :depends-on ("unary-operations"))
     (:file "gauss" :depends-on ("linear-algebra-kernel"))
     (:file "cholesky" :depends-on ("unary-operations"))
     (:file "conjugate-gradient" :depends-on ("binary-operations"))
     (:file "tridiagonal" :depends-on ("linear-algebra-kernel"))))
   ;; Interface
   (:module interface
    :depends-on ("linear-algebra" "kernel")
    :components
    ((:file "fundamental-ops")
     (:file "vector" :depends-on ("fundamental-ops"))
     (:file "matrix" :depends-on ("fundamental-ops"))
     (:file "identity-matrix" :depends-on ("matrix"))
     (:file "permutation-matrix" :depends-on ("matrix"))))
   ;; Common Lisp sequences
   (:module sequence
    :depends-on ("interface")
    :components
    ((:file "list")
     (:file "vector")
     (:file "array")))
   ;; Linear algebra classes and operations
   (:file "data-vector" :depends-on ("interface"))
   (:file "dense-matrix" :depends-on ("data-vector"))
   (:file "square-matrix" :depends-on ("dense-matrix"))
   (:file "hermitian-matrix" :depends-on ("square-matrix"))
   (:file "symmetric-matrix" :depends-on ("square-matrix"))))

(defmethod perform :after
  ((operation load-op) (system (eql (find-system :linear-algebra))))
  "Update *FEATURES* if the system loads successfully."
  (pushnew :linear-algebra-kernel common-lisp:*features*)
  (pushnew :linear-algebra common-lisp:*features*))
