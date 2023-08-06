;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite unary-operations (kernel))

(deftest unary-sump (unary-operations)

  ;; Real
  (let ((data '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)))
    (multiple-value-bind (scale sump)
        (sump data 2)
      (assert-num=  6 scale)
      (assert-num=  73/18 sump))
    (multiple-value-bind (scale sump)
        (sump data 3)
      (assert-num=  6 scale)
      (assert-num=  1 sump)))

  ;; Complex
  (let ((data
          '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
            #C(-2 3) #C(-3 1) #C(-1 0))))
    (multiple-value-bind (scale sump) (sump data 2)
      (assert-num= 4.0 scale)
      (assert-num= #C(2.75 -1.125) sump))
    (multiple-value-bind (scale sump) (sump data 3)
      (assert-num= 4.0 scale)
      (assert-num= #C(2.6639833 0.54687494) sump)))

  ;; Real
  (multiple-value-bind (scale sump)
      (sump #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2 1 0)
    (assert-num= 6 scale)
    (assert-num= 73/18 sump))
  (multiple-value-bind (scale sump) (sump #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 3 1 0)
    (assert-num= 6 scale)
    (assert-num= 1 sump))

  ;; Complex
  (multiple-value-bind (scale sump) (sump #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					    #C(-2 3) #C(-3 1) #C(-1 0))
					  2 1 0)
    (assert-num= 4.0 scale)
    (assert-num= #C(2.75 -1.125) sump))
  (multiple-value-bind (scale sump) (sump #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					    #C(-2 3) #C(-3 1) #C(-1 0))
					  3 1 0)
    (assert-num= 4.0 scale)
    (assert-num= #C(2.6639833 0.54687494) sump))

  ;; Array
  (multiple-value-bind (scale sump) (sump #2A((1.1 1.2 1.3 1.4 1.5)
					      (2.1 2.2 2.3 2.4 2.5)
					      (3.1 3.2 3.3 3.4 3.5)
					      (4.1 4.2 4.3 4.4 4.5))
					  3.5 1 0)
    (assert-num= 4.5 scale)
    (assert-num= 6.540154 sump)))

(deftest unary-sumsq-row (unary-operations)
  (let ((array #2A((1.1 1.2 1.3 1.4 1.5)
		   (2.1 2.2 2.3 2.4 2.5)
		   (3.1 3.2 3.3 3.4 3.5)
		   (4.1 4.2 4.3 4.4 4.5))))
    ;; Row 0
    (multiple-value-bind (scale sumsq) (sumsq-row array 0 :scale 1 :sumsq 0)
      (assert-num= 1.5 scale)
      (assert-num= 3.8 sumsq))
    (multiple-value-bind (scale sumsq) (sumsq-row array 0 :start 1 :end 4)
      (assert-num= 1.4 scale)
      (assert-num= 2.5969386 sumsq))
    ;; Row 2
    (multiple-value-bind (scale sumsq) (sumsq-row array 2 :scale 1 :sumsq 0)
      (assert-num= 3.5 scale)
      (assert-num= 4.453061 sumsq))
    (multiple-value-bind (scale sumsq) (sumsq-row array 2 :start 2 :end 5)
      (assert-num= 3.5 scale)
      (assert-num= 2.832653 sumsq))))

(deftest unary-sumsq-column (unary-operations)
  (let ((array #2A((1.1 1.2 1.3 1.4 1.5)
		   (2.1 2.2 2.3 2.4 2.5)
		   (3.1 3.2 3.3 3.4 3.5)
		   (4.1 4.2 4.3 4.4 4.5))))
    ;; Column 0
    (multiple-value-bind (scale sumsq) (sumsq-column array 0 :scale 1 :sumsq 0)
      (assert-num= 4.1 scale)
      (assert-num= 1.9060084 sumsq))
    (multiple-value-bind (scale sumsq) (sumsq-column array 0 :start 1 :end 4)
      (assert-num= 4.1 scale)
      (assert-num= 1.8340273 sumsq))
    ;; Column 3
    (multiple-value-bind (scale sumsq) (sumsq-column array 3 :scale 1 :sumsq 0)
      (assert-num= 4.4 scale)
      (assert-num= 1.9958677 sumsq))
    (multiple-value-bind (scale sumsq) (sumsq-column array 3 :end 3)
      (assert-num= 3.4 scale)
      (assert-num= 1.6678201 sumsq))))
