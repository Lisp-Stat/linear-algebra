;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite unary-operations (kernel))
#+nil
(deftest sumsq2 (unary-operations)
  "sqrt |x|^2 + |y|^2"

  ;; Real values
  (dolist (args (cartesian-product '(-3.0 3.0) '(-4.0 4.0)))
    (assert-num= 5.0 (sqrt (sum (esquare (eabs (apply #'vec 'single-float args)))))))

  ;; Complex values
  (let ((args1 (mapcar (lambda (x) (apply #'complex x))
		       (cartesian-product '(-1.1 1.1) '(-2.2 2.2))))
        (args2 (mapcar (lambda (x) (apply #'complex x))
		       (cartesian-product '(-3.3 3.3) '(-4.4 4.4)))))
    (dolist (args (cartesian-product args1 args2))
      (assert-num= 6.024948 (sqrt (sum (esquare (eabs (apply #'vec 'complex args)))))))))
#+nil
(deftest sumsq3 (unary-operations)
  "sqrt |x|^2 + |y|^2 + |z|^2"

  ;; Real values
  (dolist (args (nary-product '(-2.0 2.0) '(-3.0 3.0) '(-4.0 4.0)))
    (assert-num= 5.3851647 (sqrt (sum (esquare (eabs (apply #'vec 'single-float args)))))))

  ;; Complex values
  (let ((args1 (mapcar (lambda (x) (apply #'complex x))
		       (cartesian-product '(-1.1 1.1) '(-2.2 2.2))))
        (args2 (mapcar (lambda (x) (apply #'complex x))
		       (cartesian-product '(-3.3 3.3) '(-4.4 4.4))))
        (args3 (mapcar (lambda (x) (apply #'complex x))
		       (cartesian-product '(-5.5 5.5) '(-6.6 6.6)))))
    (dolist (args (nary-product args1 args2 args3))
      (assert-num= 10.49333 (sqrt (sum (esquare (eabs (apply #'vec 'complex args)))))))))
#+nil
(deftest unary-sumsq (unary-operations)
  ;; Real
  (multiple-value-bind (scale sumsq) (sumsq '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
    (assert-num=  6 scale)
    (assert-num=  73/18 sumsq))
  ;; Complex
  (multiple-value-bind (scale sumsq) (sumsq '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					      #C(-2 3) #C(-3 1) #C(-1 0)))
    (assert-num= 4.0 scale)
    (assert-num= #C(2.75 -1.125) sumsq))
  ;; Real
  (multiple-value-bind (scale sumsq) (sumsq #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1 0)
    (assert-num= 6 scale)
    (assert-num= 73/18 sumsq))
  ;; Complex
  (multiple-value-bind (scale sumsq) (sumsq #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					      #C(-2 3) #C(-3 1) #C(-1 0))
					    1 0)
    (assert-num= 4.0 scale)
    (assert-num= #C (2.75 -1.125) sumsq))
  ;; Array
  (multiple-value-bind (scale sumsq) (sumsq #2A((1.1 1.2 1.3 1.4 1.5)
						(2.1 2.2 2.3 2.4 2.5)
						(3.1 3.2 3.3 3.4 3.5)
						(4.1 4.2 4.3 4.4 4.5))
					    1 0)
    (assert-num= 4.5 scale)
    (assert-num= 8.997532 sumsq)))

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

;;; Norm & supporting functions

(deftest %abs-vector-test (unary-operations)
  (assert-num= #(6 5 4 3 2 1 0 1 2 3 4 5)
      (linear-algebra-kernel::%abs-vector #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))))

;;; Taxicab norm

(deftest unary-norm-1-vector (unary-operations)
  (assert-num= 36 (norm-vector #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-num= 19.535658 (norm-vector #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					#C(-2 3) #C(-3 1) #C(-1 0))
				      1)))

;;; Euclidean norm

(deftest unary-norm-2-vector (unary-operations)
  (assert-num= 12.083046 (norm-vector #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-num= 8.0 (norm-vector  #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
				   #C(-2 3) #C(-3 1) #C(-1 0))
				 2)))

;;; P-norm

(deftest unary-norm-p-vector (unary-operations)
  (let ((data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                 #C(-2 3) #C(-3 1) #C(-1 0))))
    ;; norm
    (assert-num= 8.732892 (norm-vector data 3))
    (assert-num= 6.064035 (norm-vector zdata 3))))

;;; Infinity norm

(deftest unary-norm-infinity-vector (unary-operations)
  (assert-num= 6   (norm-vector #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) :infinity))
  (assert-num= 4.0 (norm-vector #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
				  #C(-2 3) #C(-3 1) #C(-1 0)) :infinity)))

(deftest unary-norm-array (unary-operations)
  (let ((array #2A((1.1 1.2 1.3 1.4)
		   (2.1 2.2 2.3 2.4)
		   (3.1 3.2 3.3 3.4)
		   (4.1 4.2 4.3 4.4)
		   (5.1 5.2 5.3 5.4))))
    (assert-num= 17.0 (norm-array array 1))
    (assert-num= 5.4 (norm-array array :max))
    (assert-num= 15.858751 (norm-array array :frobenius))
    (assert-num= 21.0 (norm-array array :infinity))))

