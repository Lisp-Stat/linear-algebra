;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(define-test tridiagonal-factorization
  (:tag :kernel :tridiagonal)
  (assert-float-equal
   #2A((0.0  1.1        1.2727273)
       (1.4  0.4181819  5.499999)
       (2.3 -9.3499975 -0.3422461)
       (3.2  5.4951878  0.0))
   (linear-algebra-kernel::tridiagonal-factorization
    (make-array
     '(4 3) :initial-contents
     '((0.0 1.1 1.4) (1.4 2.2 2.3) (2.3 3.3 3.2) (3.2 4.4 0.0))))))

(define-test tridiagonal-update
  (:tag :kernel :tridiagonal)
  (assert-float-equal
   #(1.0909091 6.630434 1.3850269 -0.24240965)
   (linear-algebra-kernel::tridiagonal-update
    #2A((0.0  1.1        1.2727273)
        (1.4  0.4181819  5.499999)
        (2.3 -9.3499975 -0.3422461)
        (3.2  5.4951878  0.0))
    (make-array 4 :initial-contents '(1.2 4.3 2.3 3.1)))))

(define-test tridiagonal-backsubstitution
  (:tag :kernel :tridiagonal)
  (assert-float-equal
   #(1.7666159 -0.5309124 1.3020632 -0.24240965)
   (linear-algebra-kernel::tridiagonal-backsubstitution
    #2A((0.0  1.1        1.2727273)
        (1.4  0.4181819  5.499999)
        (2.3 -9.3499975 -0.3422461)
        (3.2  5.4951878  0.0))
    (make-array
     4 :initial-contents
     '(1.0909091 6.630434 1.3850269 -0.24240965)))))

(define-test tridiagonal-solver
  (:tag :kernel :tridiagonal)
  (assert-float-equal
   #(1.7666159 -0.5309124 1.3020632 -0.24240965)
   (linear-algebra-kernel:tridiagonal-solver
    (make-array
     '(4 3) :initial-contents
     '((0.0 1.1 1.4) (1.4 2.2 2.3) (2.3 3.3 3.2) (3.2 4.4 0.0)))
    (make-array 4 :initial-contents '(1.2 4.3 2.3 3.1)))))
