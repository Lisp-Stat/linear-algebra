;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

#+genera (setf *print-array* t)

(in-package #:linear-algebra-test)

(defsuite norms (kernel))

(defparameter data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
(defparameter zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                      #C(-2 3) #C(-3 1) #C(-1 0)))

(defparameter arr #2A((1.1 1.2 1.3 1.4)
		      (2.1 2.2 2.3 2.4)
		      (3.1 3.2 3.3 3.4)
		      (4.1 4.2 4.3 4.4)
		      (5.1 5.2 5.3 5.4)))

(defparameter a #(-4 -3 -2 -1  0  1  2  3  4))
(defparameter b (aops:reshape a '(3 3)))

;;; Taxicab norm

(deftest unary-norm-1-vector (norms)
  (assert-num= 36 (norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-num= 19.535658 (norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4) #C(-2 3) #C(-3 1) #C(-1 0)) 1)))

;;; Euclidean norm

(deftest unary-norm-2-vector (norms)
  (assert-num= 12.083046 (norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-num= 8.0 (norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4) #C(-2 3) #C(-3 1) #C(-1 0)) 2)))

;;; P-norm

(deftest unary-norm-p-vector (norms)
  (let ((data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4) #C(-2 3) #C(-3 1) #C(-1 0))))
    ;; norm
    (assert-num= 8.732892 (norm data 3))
    (assert-num= 6.064035 (norm zdata 3))))

;;; Infinity norm

(deftest unary-norm-infinity-vector (norms)
  (assert-num= 6   (norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) :inf))
  (assert-num= 4.0 (norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4) #C(-2 3) #C(-3 1) #C(-1 0)) :inf)))

(deftest unary-norm-array (norms)
  (let ((array #2A((1.1 1.2 1.3 1.4)
		   (2.1 2.2 2.3 2.4)
		   (3.1 3.2 3.3 3.4)
		   (4.1 4.2 4.3 4.4)
		   (5.1 5.2 5.3 5.4))))
    (assert-num= 17.0 (norm array 1))
    (assert-num= 15.858751 (norm array :frob))
    (assert-num= 21.0 (norm array :inf))))

;; See https://docs.scipy.org/doc/scipy/reference/generated/scipy.linalg.norm.html#scipy.linalg.norm
(deftest scipy-examples (norms)
  (assert-num= 7.745966692414834 (norm a))
  (assert-num= 7.745966692414834 (norm b :frob))
  (assert-num= 7.745966692414834 (norm b))
  (assert-num= 4 (norm a :inf))
  (assert-num= 9 (norm b :inf))
  (assert-num= 0 (norm a :-inf))
  (assert-num= 2 (norm b :-inf))

  (assert-num= 20 (norm a 1))
  (assert-num= 7  (norm b 1))
  (assert-num= 7.745966692414834 (norm a 2))
  ;; (assert-num= 9.797958971132712 (norm b :nuc)) ; need svd in CL for this
  (assert-num= 7.745966692414834 (norm a 2)))

