;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite rotation (kernel))

;;; Givens Rotation

(deftest givens-rotation (rotation)
  ;; g = 0
  (multiple-value-bind (c s r) (givens-rotation 0 0)
    (assert-true (num= 1 c))
    (assert-true (num= 0 s))
    (assert-true (num= 0 r)))
  ;; Real f
  (multiple-value-bind (c s r) (givens-rotation 1 0)
    (assert-true (num= 1 c))
    (assert-true (num= 0 s))
    (assert-true (num= 1 r)))
  ;; Imaginary f
  (multiple-value-bind (c s r) (givens-rotation #C(1 1) 0)
    (assert-true (num= 1 c))
    (assert-true (num= 0 s))
    (assert-true (num= #C(1 1) r)))
  ;; f = 0 , negative g
  (multiple-value-bind (c s r) (givens-rotation 0 -1)
    (assert-true (num=  0 c))
    (assert-true (num= -1 s))
    (assert-true (num=  1 r)))
  ;; f = 0 , Real g
  (multiple-value-bind (c s r) (givens-rotation 0 1)
    (assert-true (num= 0 c))
    (assert-true (num= 1 s))
    (assert-true (num= 1 r)))
  ;; f = 0 , Imaginary g
  (multiple-value-bind (c s r) (givens-rotation 0 #C(1 1))
    (assert-true (num= 0 c))
    (assert-true (num= #C(0.70710677 -0.70710677) s))
    (assert-true (num=  1.4142135 r)))
  ;; Rational f and g
  (multiple-value-bind (c s r) (givens-rotation 1 2)
    (assert-true (num= 0.4472136 c))
    (assert-true (num= 0.8944272 s))
    (assert-true (num= 2.236068  r)))
  ;; Float f and g
  (multiple-value-bind (c s r) (givens-rotation 1.1 2.3)
    (assert-true (num= 0.4314555 c))
    (assert-true (num= 0.9021342 s))
    (assert-true (num= 2.5495098 r)))
  ;; Complex rational f and g
  (multiple-value-bind (c s r) (givens-rotation #C(1 2) #C(3 4))
    (assert-true (num= 0.40824828 c))
    (assert-true (num= #C(0.8981462 0.16329929) s))
    (assert-true (num= #C(2.4494898 4.8989797) r)))
  ;; Complex float f and g
  (multiple-value-bind (c s r) (givens-rotation #C(1.2 2.3) #C(3.4 4.5))
    (assert-true (num= 0.4178801 c))
    (assert-true (num= #C(0.8959895 0.15026298) s))
    (assert-true (num= #C(2.8716373 5.503971) r))))

;;; Jacobi Rotation

(deftest jacobi-rotation (rotation)
  ;; Symmetric test
  (multiple-value-bind (a b c s) (jacobi-rotation 1.1 3.3 5.5)
    (assert-true (num= -0.66610646 a))
    (assert-true (num=  7.266106   b))
    (assert-true (num=  0.8816746  c))
    (assert-true (num= -0.4718579  s))))
  ;; Hermitian test
  (multiple-value-bind (a b c s) (jacobi-rotation 1.1 #C(3.3 7.7) 5.5)
    (assert-true (num= -5.3614073  a))
    (assert-true (num= 11.961407   b))
    (assert-true (num=  0.79183334 c))
    (assert-true (num= #C(-0.24058115 0.561356) s)))

;;; Householder Reflection

(deftest householder-reflection (rotation)
  (multiple-value-bind (beta tau vector) (householder-reflection #C(1.0 2.0) (vector 1.0 2.0 3.0 4.0 5.0))
    (assert-true (num= -7.745967 beta))
    (assert-true (num= #C(1.1290995 0.2581989) tau))
    (assert-true (num= #(#C(0.2 -0.4) #C(0.4 -0.8) #C(0.6 -1.2) #C(0.8 -1.6) #C(1.0 -2.0))
		       vector))))
