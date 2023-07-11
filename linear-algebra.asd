;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(defsystem "linear-algebra"
  :version "0.1.1"
  :license :MS-PL
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :maintainer "Steve Nunez <steve@symbolics.tech>"
  :maintainer "Brian Eberman <bseberman@gmail.com>"
  :long-name   "Linear Algebra for Common Lisp"
  :description "Linear Algebra for Common Lisp"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/docs/manuals/lla"
  :source-control (:git "https://github.com/Lisp-Stat/linear-algebra.git")
  :bug-tracker "https://github.com/Lisp-Stat/linear-algebra/issues"

  :pathname "src/"
  :depends-on ("closer-mop" "floating-point")
  :components
  ((:file "pkgdcl" :depends-on ("kernel"))
   ;; Linear algebra kernel functions
   (:module kernel
    :pathname "kernel/"
    :components
    ((:file "pkgdcl")
     (:file "utility" :depends-on ("pkgdcl"))
     (:file "permute" :depends-on ("pkgdcl"))
     (:file "unary-operations" :depends-on ("pkgdcl"))
     (:file "binary-operations" :depends-on ("pkgdcl"))
     (:file "rotation" :depends-on ("unary-operations"))
     (:file "gauss" :depends-on ("pkgdcl"))
     (:file "cholesky" :depends-on ("unary-operations"))
     (:file "conjugate-gradient" :depends-on ("binary-operations"))
     (:file "tridiagonal" :depends-on ("pkgdcl"))))
   ;; Interface
   (:module interface
    :pathname "interface/"
    :depends-on ("kernel")
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
   (:file "symmetric-matrix" :depends-on ("square-matrix")))
  :in-order-to ((test-op (test-op "linear-algebra/tests"))))

(defsystem "linear-algebra/tests"
  :version "1.0.0"
  :description "Unit tests for LINEAR-ALGEBRA."
  :author "Thomas M. Hermann <thomas.m.hermann@odonata-research.com>"
  :maintainer "Steve Nunez <steve@symbolics.tech>"
  :maintainer "Brian Eberman <brian@tenfactorgrowth.com>"
  :licence     :MS-PL
  :pathname "test/"
  :depends-on ("linear-algebra-test")
  :serial t
  :perform (test-op (o s)
		    (symbol-call :lisp-unit :run-tests :all :linear-algebra-test)))


;; Is this a misuse of *FEATURES*?
#+ignore
(defmethod perform :after
  ((operation load-op) (system (eql (find-system :linear-algebra))))
  "Update *FEATURES* if the system loads successfully."
  (pushnew :linear-algebra common-lisp:*features*)
  (pushnew :linear-algebra common-lisp:*features*))
