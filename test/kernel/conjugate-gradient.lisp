;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite conjugate-gradient (kernel))

(deftest conjugate-gradient-solver (conjugate-gradient)
  ;; 2x2 from NumAlgoC
  (assert-true (num= #(2.0 0.33333334)
		     (conjugate-gradient-solver (make-array '(2 2) :initial-contents '((2.0 0.0) (0.0 3.0)))
						(make-array 2 :initial-contents '(4.0 1.0)))))
  ;; 2x2
  (assert-true (num= #(3.265307 -1.3265313)
		     (conjugate-gradient-solver (make-array '(2 2) :initial-contents '((1.1 1.2) (1.2 2.2)))
						(make-array 2 :initial-contents '(2.0 1.0)))))
  ;; 3x3
  (assert-true (num= #(3.5856622 -2.3062859 0.7900801)
		     (conjugate-gradient-solver (make-array '(3 3) :initial-contents '((1.15 1.26 1.37)
										       (1.26 2.23 2.31)
										       (1.37 2.31 3.31)))
						(make-array 3 :initial-contents '(2.3 1.2 2.2))))))
