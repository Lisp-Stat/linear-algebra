;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(define-test sumsq2
  "sqrt |x|^2 + |y|^2"
  (:tag :kernel :unary :sumsq2)
  ;; Real values
  (dolist (args (cartesian-product '(-3.0 3.0) '(-4.0 4.0)))
    (assert-float-equal
     5.0 (apply #'linear-algebra-kernel:sumsq2 args)))
  ;; Complex values
  (let ((args1
         (mapcar
          (lambda (x) (apply #'complex x))
          (cartesian-product '(-1.1 1.1) '(-2.2 2.2))))
        (args2
         (mapcar
          (lambda (x) (apply #'complex x))
          (cartesian-product '(-3.3 3.3) '(-4.4 4.4)))))
    (dolist (args (cartesian-product args1 args2))
      (assert-float-equal
       6.024948 (apply #'linear-algebra-kernel:sumsq2 args)))))

(define-test sumsq3
  "sqrt |x|^2 + |y|^2 + |z|^2"
  (:tag :kernel :unary :sumsq2)
  ;; Real values
  (dolist (args (nary-product '(-2.0 2.0) '(-3.0 3.0) '(-4.0 4.0)))
    (assert-float-equal
     5.3851647 (apply #'linear-algebra-kernel:sumsq3 args)))
  ;; Complex values
  (let ((args1
         (mapcar
          (lambda (x) (apply #'complex x))
          (cartesian-product '(-1.1 1.1) '(-2.2 2.2))))
        (args2
         (mapcar
          (lambda (x) (apply #'complex x))
          (cartesian-product '(-3.3 3.3) '(-4.4 4.4))))
        (args3
         (mapcar
          (lambda (x) (apply #'complex x))
          (cartesian-product '(-5.5 5.5) '(-6.6 6.6)))))
    (dolist (args (nary-product args1 args2 args3))
      (assert-float-equal
       10.49333 (apply #'linear-algebra-kernel:sumsq3 args)))))

(define-test unary-sumsq
  (:tag :kernel :unary :sumsq)
  ;; Real
  (multiple-value-bind (scale sumsq)
      (linear-algebra-kernel:sumsq
       '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
    (assert-rational-equal 6 scale)
    (assert-rational-equal 73/18 sumsq))
  ;; Complex
  (multiple-value-bind (scale sumsq)
      (linear-algebra-kernel:sumsq
       '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
         #C(-2 3) #C(-3 1) #C(-1 0)))
    (assert-float-equal 4.0 scale)
    (assert-float-equal #C(2.75 -1.125) sumsq))  
  ;; Real
  (multiple-value-bind (scale sumsq)
      (linear-algebra-kernel:sumsq
       #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1 0)
    (assert-rational-equal 6 scale)
    (assert-rational-equal 73/18 sumsq))
  ;; Complex
  (multiple-value-bind (scale sumsq)
      (linear-algebra-kernel:sumsq
       #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
         #C(-2 3) #C(-3 1) #C(-1 0)) 1 0)
    (assert-float-equal 4.0 scale)
    (assert-float-equal #C (2.75 -1.125) sumsq))
  ;; Array
  (multiple-value-bind (scale sumsq)
      (linear-algebra-kernel:sumsq
       #2A((1.1 1.2 1.3 1.4 1.5)
           (2.1 2.2 2.3 2.4 2.5)
           (3.1 3.2 3.3 3.4 3.5)
           (4.1 4.2 4.3 4.4 4.5))
       1 0)
    (assert-float-equal 4.5 scale)
    (assert-float-equal 8.997532 sumsq)))

(define-test unary-sump
  (:tag :kernel :unary :sump)
  ;; Real
  (let ((data '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)))
    (multiple-value-bind (scale sump)
        (linear-algebra-kernel:sump data 2)
      (assert-rational-equal 6 scale)
      (assert-rational-equal 73/18 sump))
    (multiple-value-bind (scale sump)
        (linear-algebra-kernel:sump data 3)
      (assert-rational-equal 6 scale)
      (assert-rational-equal 1 sump)))
  ;; Complex
  (let ((data
         '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
           #C(-2 3) #C(-3 1) #C(-1 0))))
    (multiple-value-bind (scale sump)
        (linear-algebra-kernel:sump data 2)
      (assert-float-equal 4.0 scale)
      (assert-float-equal #C(2.75 -1.125) sump))
    (multiple-value-bind (scale sump)
        (linear-algebra-kernel:sump data 3)
      (assert-float-equal 4.0 scale)
      (assert-float-equal #C(2.6639833 0.54687494) sump)))
  ;; Real
  (multiple-value-bind (scale sump)
      (linear-algebra-kernel:sump
       #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2 1 0)
    (assert-rational-equal 6 scale)
    (assert-rational-equal 73/18 sump))
  (multiple-value-bind (scale sump)
      (linear-algebra-kernel:sump
       #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 3 1 0)
    (assert-rational-equal 6 scale)
    (assert-rational-equal 1 sump))
  ;; Complex
  (multiple-value-bind (scale sump)
      (linear-algebra-kernel:sump
       #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
         #C(-2 3) #C(-3 1) #C(-1 0))
       2 1 0)
    (assert-float-equal 4.0 scale)
    (assert-float-equal #C(2.75 -1.125) sump))
  (multiple-value-bind (scale sump)
      (linear-algebra-kernel:sump
       #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
         #C(-2 3) #C(-3 1) #C(-1 0))
       3 1 0)
    (assert-float-equal 4.0 scale)
    (assert-float-equal #C(2.6639833 0.54687494) sump))
  ;; Array
  (multiple-value-bind (scale sump)
      (linear-algebra-kernel:sump
       #2A((1.1 1.2 1.3 1.4 1.5)
           (2.1 2.2 2.3 2.4 2.5)
           (3.1 3.2 3.3 3.4 3.5)
           (4.1 4.2 4.3 4.4 4.5))
       3.5 1 0)
    (assert-float-equal 4.5 scale)
    (assert-float-equal 6.540154 sump)))

(define-test unary-sumsq-row
  (:tag :kernel :unary :sumsq)
  (let ((array
         #2A((1.1 1.2 1.3 1.4 1.5)
             (2.1 2.2 2.3 2.4 2.5)
             (3.1 3.2 3.3 3.4 3.5)
             (4.1 4.2 4.3 4.4 4.5))))
    ;; Row 0
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-row
         array 0 :scale 1 :sumsq 0)
      (assert-float-equal 1.5 scale)
      (assert-float-equal 3.8 sumsq))
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-row
         array 0 :start 1 :end 4)
      (assert-float-equal 1.4 scale)
      (assert-float-equal 2.5969386 sumsq))
    ;; Row 2
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-row
         array 2 :scale 1 :sumsq 0)
      (assert-float-equal 3.5 scale)
      (assert-float-equal 4.453061 sumsq))
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-row
         array 2 :start 2 :end 5)
      (assert-float-equal 3.5 scale)
      (assert-float-equal 2.832653 sumsq))))

(define-test unary-sumsq-column
  (:tag :kernel :unary :sumsq)
  (let ((array
         #2A((1.1 1.2 1.3 1.4 1.5)
             (2.1 2.2 2.3 2.4 2.5)
             (3.1 3.2 3.3 3.4 3.5)
             (4.1 4.2 4.3 4.4 4.5))))
    ;; Column 0
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-column
         array 0 :scale 1 :sumsq 0)
      (assert-float-equal 4.1 scale)
      (assert-float-equal 1.9060084 sumsq))
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-column
         array 0 :start 1 :end 4)
      (assert-float-equal 4.1 scale)
      (assert-float-equal 1.8340273 sumsq))
    ;; Column 3
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-column
         array 3 :scale 1 :sumsq 0)
      (assert-float-equal 4.4 scale)
      (assert-float-equal 1.9958677 sumsq))
    (multiple-value-bind (scale sumsq)
        (linear-algebra-kernel:sumsq-column
         array 3 :end 3)
      (assert-float-equal 3.4 scale)
      (assert-float-equal 1.6678201 sumsq))))

;;; Norm & supporting functions

(define-test %abs-vector
  (:tag :kernel :unary :norm)
  (assert-rational-equal
   #(6 5 4 3 2 1 0 1 2 3 4 5)
   (linear-algebra-kernel::%abs-vector
    #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))))

;;; Taxicab norm

(define-test unary-norm-1-vector
  (:tag :kernel :unary :norm)
  (assert-rational-equal
   36 (linear-algebra-kernel:norm-vector
       #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-float-equal
   19.535658
   (linear-algebra-kernel:norm-vector
    #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    1)))

;;; Euclidean norm

(define-test unary-norm-2-vector
  (:tag :kernel :unary :norm)
  (assert-float-equal
   12.083046
   (linear-algebra-kernel:norm-vector
    #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
    2))
  (assert-float-equal
   8.0
   (linear-algebra-kernel:norm-vector
    #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    2)))

;;; P-norm

(define-test unary-norm-p-vector
  (:tag :kernel :unary :norm)
  (let ((data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                 #C(-2 3) #C(-3 1) #C(-1 0))))
    ;; norm
    (assert-float-equal
     8.732892 (linear-algebra-kernel:norm-vector data 3))
    (assert-float-equal
     6.064035 (linear-algebra-kernel:norm-vector zdata 3))))

;;; Infinity norm

(define-test unary-norm-infinity-vector
  (:tag :kernel :unary :norm)
  (assert-rational-equal
   6 (linear-algebra-kernel:norm-vector
      #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
      :infinity))
  (assert-float-equal
   4.0 (linear-algebra-kernel:norm-vector
        #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
          #C(-2 3) #C(-3 1) #C(-1 0))
        :infinity)))

(define-test unary-norm-array
  (:tag :kernel :unary :norm)
  (let ((array
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-float-equal
     17.0 (linear-algebra-kernel:norm-array array 1))
    (assert-float-equal
     5.4 (linear-algebra-kernel:norm-array array :max))
    (assert-float-equal
     15.858751 (linear-algebra-kernel:norm-array array :frobenius))
    (assert-float-equal
     21.0 (linear-algebra-kernel:norm-array array :infinity))))
