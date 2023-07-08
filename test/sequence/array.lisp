;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(define-test norm-array
  (:tag :array :norm)
  (let ((array
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-float-equal
     17.0 (linear-algebra:norm array))
    (assert-float-equal
     5.4 (linear-algebra:norm array :max))
    (assert-float-equal
     15.858751 (linear-algebra:norm array :frobenius))
    (assert-float-equal
     21.0 (linear-algebra:norm array :infinity))))

(define-test transpose-array
  (:tag :array :transpose)
  (assert-float-equal
   #2A((1.1 2.1 3.1 4.1 5.1)
       (1.2 2.2 3.2 4.2 5.2)
       (1.3 2.3 3.3 4.3 5.3)
       (1.4 2.4 3.4 4.4 5.4))
   (linear-algebra:transpose
    #2A((1.1 1.2 1.3 1.4)
        (2.1 2.2 2.3 2.4)
        (3.1 3.2 3.3 3.4)
        (4.1 4.2 4.3 4.4)
        (5.1 5.2 5.3 5.4))))
  (assert-float-equal
   #2A((#C(1.1 1.2) #C(2.1 2.2) #C(3.1 3.2) #C(4.1 4.2) #C(5.1 5.2))
       (#C(1.3 1.4) #C(2.3 2.4) #C(3.3 3.4) #C(4.3 4.4) #C(5.3 5.4)))
   (linear-algebra:transpose
    #2A((#C(1.1 1.2) #C(1.3 1.4))
        (#C(2.1 2.2) #C(2.3 2.4))
        (#C(3.1 3.2) #C(3.3 3.4))
        (#C(4.1 4.2) #C(4.3 4.4))
        (#C(5.1 5.2) #C(5.3 5.4))))))

(define-test ntranspose-array
  (:tag :array :ntranspose)
  (let ((original
         (make-array
          '(4 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4))))
        (transpose
         #2A((1.1 2.1 3.1 4.1)
             (1.2 2.2 3.2 4.2)
             (1.3 2.3 3.3 4.3)
             (1.4 2.4 3.4 4.4))))
    (assert-eq original (linear-algebra:ntranspose original))
    (assert-float-equal transpose original))
  (let ((original
         (make-array
          '(2 2) :initial-contents
          '((#C(1.1 1.2) #C(1.3 1.4))
            (#C(2.1 2.2) #C(2.3 2.4)))))
        (transpose
         #2A((#C(1.1 1.2) #C(2.1 2.2))
             (#C(1.3 1.4) #C(2.3 2.4)))))
    (assert-eq original (linear-algebra:ntranspose original))
    (assert-float-equal transpose original)))

(define-test permute-array
  (:tag :array :permute)
  (let ((array
         #2A((1.0 1.1 1.2 1.3 1.4)
             (2.0 2.1 2.2 2.3 2.4)
             (3.0 3.1 3.2 3.3 3.4)
             (4.0 4.1 4.2 4.3 4.4)
             (5.0 5.1 5.2 5.3 5.4)))
        (pmat
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)))))
    (assert-float-equal
     #2A((1.2 1.3 1.0 1.4 1.1)
         (2.2 2.3 2.0 2.4 2.1)
         (3.2 3.3 3.0 3.4 3.1)
         (4.2 4.3 4.0 4.4 4.1)
         (5.2 5.3 5.0 5.4 5.1))
     (linear-algebra:permute array pmat))
    (assert-float-equal
     #2A((3.0 3.1 3.2 3.3 3.4)
         (5.0 5.1 5.2 5.3 5.4)
         (1.0 1.1 1.2 1.3 1.4)
         (2.0 2.1 2.2 2.3 2.4)
         (4.0 4.1 4.2 4.3 4.4))
     (linear-algebra:permute pmat array))))

(define-test scale-array
  (:tag :array :scale)
  (assert-float-equal
   #2A(( 3.3  3.6  3.9  4.2)
       ( 6.3  6.6  6.9  7.2)
       ( 9.3  9.6  9.9 10.2)
       (12.3 12.6 12.9 13.2)
       (15.3 15.6 15.9 16.2))
   (linear-algebra:scale
    3.0
    #2A((1.1 1.2 1.3 1.4)
        (2.1 2.2 2.3 2.4)
        (3.1 3.2 3.3 3.4)
        (4.1 4.2 4.3 4.4)
        (5.1 5.2 5.3 5.4)))))

(define-test nscale-array
  (:tag :array :nscale)
  (let ((array
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4)))))
    (assert-eq array (linear-algebra:nscale 3.0 array))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     array)))

(define-test add-array
  (:tag :array :add)
  (let ((array
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    ;; No scalar
    (assert-float-equal
     #2A(( 2.2  2.4  2.6  2.8)
         ( 4.2  4.4  4.6  4.8)
         ( 6.2  6.4  6.6  6.8)
         ( 8.2  8.4  8.6  8.8)
         (10.2 10.4 10.6 10.8))
     (linear-algebra:add array array))
    ;; Scalar1
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:add array array :scalar1 2.0))
    ;; Scalar2
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:add array array :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-float-equal
     #2A(( 5.5  6.0  6.5  7.0)
         (10.5 11.0 11.5 12.0)
         (15.5 16.0 16.5 17.0)
         (20.5 21.0 21.5 22.0)
         (25.5 26.0 26.5 27.0))
     (linear-algebra:add array array :scalar1 2.0 :scalar2 3.0))))

(define-test nadd-array
  (:tag :array :nadd)
  ;; No scalar
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq array1 (linear-algebra:nadd array1 array2))
    (assert-float-equal
     #2A(( 2.2  2.4  2.6  2.8)
         ( 4.2  4.4  4.6  4.8)
         ( 6.2  6.4  6.6  6.8)
         ( 8.2  8.4  8.6  8.8)
         (10.2 10.4 10.6 10.8))
     array1))
  ;; Scalar1
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nadd array1 array2 :scalar1 2.0))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     array1))
  ;; Scalar2
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nadd array1 array2 :scalar2 2.0))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     array1))
  ;; Scalar1 & Scalar2
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nadd
             array1 array2 :scalar1 2.0 :scalar2 3.0))
    (assert-float-equal
     #2A(( 5.5  6.0  6.5  7.0)
         (10.5 11.0 11.5 12.0)
         (15.5 16.0 16.5 17.0)
         (20.5 21.0 21.5 22.0)
         (25.5 26.0 26.5 27.0))
     array1)))

(define-test subtract-array
  (:tag :array :subtract)
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (array1
         #2A(( 2.2  2.4  2.6  2.8)
             ( 4.2  4.4  4.6  4.8)
             ( 6.2  6.4  6.6  6.8)
             ( 8.2  8.4  8.6  8.8)
             (10.2 10.4 10.6 10.8)))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    ;; No scalar
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     (linear-algebra:subtract array1 array2))
    ;; Scalar1
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:subtract array1 array2 :scalar1 2.0))
    ;; Scalar2
    (assert-float-equal
     #2A((0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0))
     (linear-algebra:subtract array1 array2 :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     (linear-algebra:subtract array1 array2 :scalar1 2.0 :scalar2 3.0))))

(define-test nsubtract-array
  (:tag :array :nsubtract)
  ;; No scalar
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '(( 2.2  2.4  2.6  2.8)
            ( 4.2  4.4  4.6  4.8)
            ( 6.2  6.4  6.6  6.8)
            ( 8.2  8.4  8.6  8.8)
            (10.2 10.4 10.6 10.8))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq array1 (linear-algebra:nsubtract array1 array2))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     array1))
  ;; Scalar1
  (let ((array1
         (make-array
          '(5 4) :initial-contents
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4)
            (4.1 4.2 4.3 4.4)
            (5.1 5.2 5.3 5.4))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nsubtract array1 array2 :scalar1 2.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     array1))
  ;; Scalar2
  (let ((*epsilon* (* 4F0 single-float-epsilon))
        (array1
         (make-array
          '(5 4) :initial-contents
          '(( 3.3  3.6  3.9  4.2)
            ( 6.3  6.6  6.9  7.2)
            ( 9.3  9.6  9.9 10.2)
            (12.3 12.6 12.9 13.2)
            (15.3 15.6 15.9 16.2))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nsubtract array1 array2 :scalar2 2.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     array1))
  ;; Scalar1 & Scalar2
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (array1
         (make-array
          '(5 4) :initial-contents
          '(( 2.2  2.4  2.6  2.8)
            ( 4.2  4.4  4.6  4.8)
            ( 6.2  6.4  6.6  6.8)
            ( 8.2  8.4  8.6  8.8)
            (10.2 10.4 10.6 10.8))))
        (array2
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))
    (assert-eq
     array1 (linear-algebra:nsubtract
             array1 array2 :scalar1 2.0 :scalar2 3.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     array1)))

(define-test product-array
  (:tag :array :product)
  ;; Vector - array
  (assert-float-equal
   #(15.0 30.0 45.0)
   (linear-algebra:product
    #(1.0 2.0 3.0 4.0 5.0)
    #2A((1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0))))
  (assert-float-equal
   #(31.5 63.0 94.5)
   (linear-algebra:product
    #(1.0 2.0 3.0 4.0 5.0)
    #2A((1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0)
        (1.0 2.0 3.0))
    2.1))
  (assert-error
   'error
   (linear-algebra:product
    #(1.0 2.0 3.0 4.0 5.0 6.0)
    (make-array '(5 3) :initial-element 1.0)))
  ;; Array - vector
  (assert-float-equal
   #(15.0 30.0 45.0)
   (linear-algebra:product
    #2A((1.0 1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0 3.0))
    #(1.0 2.0 3.0 4.0 5.0)))
  (assert-float-equal
   #(31.5 63.0 94.5)
   (linear-algebra:product
    #2A((1.0 1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0 3.0))
    #(1.0 2.0 3.0 4.0 5.0)
    2.1))
  (assert-error
   'error
   (linear-algebra:product
    (make-array '(3 5) :initial-element 1.0)
    #(1.0 2.0 3.0 4.0 5.0 6.0)))
  ;; Array - Array
  (assert-float-equal
   #2A((15.0 15.0 15.0 15.0)
       (30.0 30.0 30.0 30.0)
       (45.0 45.0 45.0 45.0))
   (linear-algebra:product
    #2A((1.0 1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0 3.0))
    #2A((1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0)
        (4.0 4.0 4.0 4.0)
        (5.0 5.0 5.0 5.0))))
  (assert-float-equal
   #2A((31.5 31.5 31.5 31.5)
       (63.0 63.0 63.0 63.0)
       (94.5 94.5 94.5 94.5))
   (linear-algebra:product
    #2A((1.0 1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0 3.0))
    #2A((1.0 1.0 1.0 1.0)
        (2.0 2.0 2.0 2.0)
        (3.0 3.0 3.0 3.0)
        (4.0 4.0 4.0 4.0)
        (5.0 5.0 5.0 5.0))
    2.1)))

(define-test solve-array
  (:tag :array :solve)
  (let ((*epsilon* (* 64 single-float-epsilon))
        (vector2 (make-array 2 :initial-contents '(1.0 2.0)))
        (vector3 (make-array 3 :initial-contents '(2.3 1.2 2.2)))
        (array2
         (make-array
          '(2 2) :initial-contents
          '((1.1 1.2) (2.1 2.2))))
        (array3
         (make-array
          '(3 3) :initial-contents
          '((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31)))))
    ;; 2x2
    (assert-float-equal
     #(2.0 -1.0) (linear-algebra:solve array2 vector2))
    (assert-float-equal #(1.0 2.0) vector2)
    (assert-float-equal #2A((1.1 1.2) (2.1 2.2)) array2)
    ;; 3x3
    ;; Maxima : #(66.36628 -151.8314 85.6105)
    (assert-float-equal
     #(66.36775 -151.8342 85.6118)
     (linear-algebra:solve array3 vector3))
    (assert-float-equal #(2.3 1.2 2.2) vector3)
    (assert-float-equal
     #2A((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31))
     array3)))

(define-test nsolve-array
  (:tag :array :nsolve)
  (let ((*epsilon* (* 64 single-float-epsilon))
        (vector2 (make-array 2 :initial-contents '(1.0 2.0)))
        (vector3 (make-array 3 :initial-contents '(2.3 1.2 2.2)))
        (array2
         (make-array
          '(2 2) :initial-contents
          '((1.1 1.2) (2.1 2.2))))
        (array3
         (make-array
          '(3 3) :initial-contents
          '((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31)))))
    ;; 2x2
    (assert-float-equal
     #(2.0 -1.0) (linear-algebra:nsolve array2 vector2))
    ;; 3x3
    ;; Maxima : #(66.36628 -151.8314 85.6105)
    (assert-float-equal
     #(66.36775 -151.8342 85.6118)
     (linear-algebra:nsolve array3 vector3))))

(define-test invert-array
  (:tag :array :invert)
  ;; 2x2
  (let ((array
         (make-array
          '(2 2) :initial-contents
          '((1.1 1.2) (2.1 2.2)))))
    (assert-float-equal
     #2A((-22.000029 12.000016) (21.000027 -11.000015))
     (linear-algebra:invert array))
    (assert-float-equal #2A((1.1 1.2) (2.1 2.2)) array))
  ;; 3x3
  (let ((array
         (make-array
          '(3 3) :initial-contents
          '((1.1 0.12 0.13)
            (0.21 2.2 0.23)
            (0.31 0.32 3.3)))))
    (assert-float-equal
     #2A((0.9272161 -0.04572601 -0.03333973)
         (-0.08021406 0.4631565 -0.029120658)
         (-0.07932379 -0.04061667 0.30898604))
     (linear-algebra:invert array))
    (assert-float-equal
     #2A((1.1 0.12 0.13)
         (0.21 2.2 0.23)
         (0.31 0.32 3.3))
     array))
  ;; 4x4
  (let ((array
         (make-array
          '(4 4) :initial-contents
          '((10.0 0.12 0.13 0.14)
            (0.21 20.0 0.23 0.24)
            (0.31 0.32 30.0 0.34)
            (0.41 0.42 0.43 40.0)))))
    (assert-float-equal
     #2A((0.10003952 -5.862483e-4 -4.2409348e-4 -3.4301603e-4)
         (-0.0010267387 0.050018318 -3.748202e-4 -2.9333035e-4)
         (-0.001011414 -5.216503e-4 0.033345684 -2.7676846e-4)
         (-0.0010037516 -5.135755e-4 -3.5018355e-4 0.02500957))
     (linear-algebra:invert array))
    (assert-float-equal
     #2A((10.0 0.12 0.13 0.14)
         (0.21 20.0 0.23 0.24)
         (0.31 0.32 30.0 0.34)
         (0.41 0.42 0.43 40.0))
     array)))

(define-test ninvert-array
  (:tag :array :ninvert)
  ;; 2x2
  (let ((array
         (make-array
          '(2 2) :initial-contents
          '((1.1 1.2) (2.1 2.2)))))
    (assert-float-equal
     #2A((-22.000029 12.000016) (21.000027 -11.000015))
     (linear-algebra:ninvert array))
    (assert-float-equal
     #2A((2.1 2.2) (0.52380956 0.047618986)) array))
  ;; 3x3
  (let ((array
         (make-array
          '(3 3) :initial-contents
          '((1.1 0.12 0.13)
            (0.21 2.2 0.23)
            (0.31 0.32 3.3)))))
    (assert-float-equal
     #2A((0.9272161 -0.04572601 -0.03333973)
         (-0.08021406 0.4631565 -0.029120658)
         (-0.07932379 -0.04061667 0.30898604))
     (linear-algebra:ninvert array))
    (assert-float-equal
     #2A((1.1        0.12       0.13)
         (0.19090909 2.177091   0.20518182)
         (0.28181818 0.13145148 3.2363923))
     array))
  ;; 4x4
  (let ((array
         (make-array
          '(4 4) :initial-contents
          '((10.0 0.12 0.13 0.14)
            (0.21 20.0 0.23 0.24)
            (0.31 0.32 30.0 0.34)
            (0.41 0.42 0.43 40.0)))))
    (assert-float-equal
     #2A((0.10003952 -5.862483e-4 -4.2409348e-4 -3.4301603e-4)
         (-0.0010267387 0.050018318 -3.748202e-4 -2.9333035e-4)
         (-0.001011414 -5.216503e-4 0.033345684 -2.7676846e-4)
         (-0.0010037516 -5.135755e-4 -3.5018355e-4 0.02500957))
     (linear-algebra:ninvert array))
    (assert-float-equal
     #2A((10.0    0.12         0.13         0.14)
         ( 0.021 19.99748      0.22727      0.23706)
         ( 0.031  0.015815994 29.992375     0.33191067)
         ( 0.041  0.020756614  0.014001981 39.98469))
     array)))
