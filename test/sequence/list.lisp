;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite list-sequence-test (linear-algebra-sequence-test))


;;; Taxicab norm
(deftest %norm-1-list (list-sequence-test)
  (assert-rational-equal
   36 (linear-algebra::%norm
       '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-rational-equal
   36 (linear-algebra:norm
       '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-float-equal
   19.535658
   (linear-algebra::%norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    1))
  (assert-float-equal
   19.535658
   (linear-algebra:norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    1)))

;;; Euclidean norm

(deftest %norm-2-list (list-sequence-test)
  (assert-float-equal
   12.083046
   (linear-algebra::%norm
    '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-float-equal
   12.083046
   (linear-algebra:norm
    '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
    2))
  (assert-float-equal
   8.0
   (linear-algebra::%norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0)) 2))
  (assert-float-equal
   8.0
   (linear-algebra:norm
    '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
      #C(-2 3) #C(-3 1) #C(-1 0))
    2)))

;;; P-norm

(deftest %norm-p-list (list-sequence-test)
  (let ((data '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata
         '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
           #C(-2 3) #C(-3 1) #C(-1 0))))
    (assert-float-equal
     8.732892 (linear-algebra::%norm data 3))
    (assert-float-equal
     6.064035 (linear-algebra::%norm zdata 3))
    ;; norm
    (assert-float-equal
     8.732892 (linear-algebra:norm data 3))
    (assert-float-equal
     6.064035 (linear-algebra:norm zdata 3))))

;;; Infinity norm

(deftest %norm-infinity-list (list-sequence-test)
  (assert-rational-equal
   6 (linear-algebra::%norm
      '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
      :infinity))
  (assert-rational-equal
   6 (linear-algebra:norm
      '(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5)
      :infinity))
  (assert-float-equal
   4.0 (linear-algebra::%norm
        '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
          #C(-2 3) #C(-3 1) #C(-1 0))
        :infinity))
  (assert-float-equal
   4.0 (linear-algebra:norm
        '(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
          #C(-2 3) #C(-3 1) #C(-1 0))
        :infinity)))

;;; List transpose

(deftest transpose-list (list-sequence-test)
  (assert-float-equal
   '(1.0 2.0 3.0 4.0 5.0)
   (linear-algebra:transpose
    '(1.0 2.0 3.0 4.0 5.0))))

(deftest ntranspose-list (list-sequence-test)
  (let ((data (list 1.0 2.0 3.0 4.0 5.0)))
    (assert-equal
     data (linear-algebra:ntranspose data))))

;;; List permutation

(deftest permute-list (list-sequence-test)
  (let ((list (list 1.1 2.2 3.3 4.4 5.5))
        (pmat
         (linear-algebra:make-matrix
          5 5 :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)))))
    (assert-float-equal
     (list 3.3 4.4 1.1 5.5 2.2)
     (linear-algebra:permute list pmat))
    (assert-float-equal
     (list 3.3 5.5 1.1 2.2 4.4)
     (linear-algebra:permute pmat list))))

;;; List scale

(deftest scale-list (list-sequence-test)
  (assert-float-equal
   '(2.0 4.0 6.0 8.0 10.0)
   (linear-algebra:scale 2.0 '(1.0 2.0 3.0 4.0 5.0)))
  (assert-float-equal
   '(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))
   (linear-algebra:scale #C(1.0 1.0) '(1.0 2.0 3.0 4.0 5.0)))
  (assert-float-equal
   '(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0))
   (linear-algebra:scale 2.0
    '(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))))
  (assert-float-equal
   #(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0))
   (linear-algebra:scale
    #C(2.0 2.0)
    '(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0)))))

(deftest nscale-list (list-sequence-test)
  (let ((list (list 1.0 2.0 3.0 4.0 5.0)))
    (assert-eq list (linear-algebra:nscale 2.0 list))
    (assert-float-equal '(2.0 4.0 6.0 8.0 10.0) list))
  (let ((list (list 1.0 2.0 3.0 4.0 5.0)))
    (assert-eq list (linear-algebra:nscale #C(1.0 1.0) list))
    (assert-float-equal
     '(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))
     list))
  (let ((list (list #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0)
                    #C(4.0 4.0) #C(5.0 5.0))))
    (assert-eq list (linear-algebra:nscale 2.0 list))
    (assert-float-equal
     '(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0))
     list))
  (let ((list (list #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0)
                    #C(4.0 4.0) #C(5.0 5.0))))
    (assert-eq list (linear-algebra:nscale #C(2.0 2.0) list))
    (assert-float-equal
     '(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0))
     list)))

;;; List addition

(deftest add-list (list-sequence-test)
  ;; Real
  (let ((list1 '(1.1 2.2 3.3 4.4))
        (list2 '(1.1 2.2 3.3 4.4)))
    (assert-float-equal
     '(2.2 4.4 6.6 8.8) (linear-algebra:add list1 list2))
    (assert-float-equal
    '(3.3 6.6 9.9 13.2) (linear-algebra:add list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(3.3 6.6 9.9 13.2) (linear-algebra:add list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(4.4 8.8 13.2 17.6)
     (linear-algebra:add list1 list2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((list1 '(#C(1.1 2.2) #C(3.3 4.4)))
        (list2 '(#C(1.1 2.2) #C(3.3 4.4))))
    (assert-float-equal
     '(#C(2.2 4.4) #C(6.6 8.8)) (linear-algebra:add list1 list2))
    (assert-float-equal
     '(#C(3.3 6.6) #C(9.9 13.2))
     (linear-algebra:add list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(#C(3.3 6.6) #C(9.9 13.2))
     (linear-algebra:add list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(#C(4.4 8.8) #C(13.2 17.6))
     (linear-algebra:add list1 list2 :scalar1 2.0 :scalar2 2.0))))

;;; Destructive list addition

(deftest nadd-list (list-sequence-test)
  ;; Real
  (let ((list1 (list 1.1 2.2 3.3 4.4))
        (list2 (list 1.1 2.2 3.3 4.4)))
    (assert-eq list1 (linear-algebra:nadd list1 list2))
    (assert-float-equal '(2.2 4.4 6.6 8.8) list1)
    (assert-float-equal
     '(4.4 8.8 13.2 17.6)
     (linear-algebra:nadd list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(9.9 19.8 29.7 39.6)
     (linear-algebra:nadd list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(22.0 44.0 66.0 88.0)
     (linear-algebra:nadd list1 list2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((list1 (list #C(1.1 2.2) #C(3.3 4.4)))
        (list2 (list #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq list1 (linear-algebra:nadd list1 list2))
    (assert-float-equal '(#C(2.2 4.4) #C(6.6 8.8)) list1)
    (assert-float-equal
     '(#C(4.4 8.8) #C(13.2 17.6))
     (linear-algebra:nadd list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(#C(9.9 19.8) #C(29.7 39.6))
     (linear-algebra:nadd list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(#C(22.0 44.0) #C(66.0 88.0))
     (linear-algebra:nadd list1 list2 :scalar1 2.0 :scalar2 2.0))))

;;; List subtraction

(deftest subtract-list (list-sequence-test)
  ;; Real
  (let ((list1 '(1.1 2.2 3.3 4.4))
        (list2 '(1.1 2.2 3.3 4.4)))
    (assert-float-equal
     '(0.0 0.0 0.0 0.0)
     (linear-algebra:subtract list1 list2))
    (assert-float-equal
     '(1.1 2.2 3.3 4.4)
     (linear-algebra:subtract list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(-1.1 -2.2 -3.3 -4.4)
     (linear-algebra:subtract list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(0.0 0.0 0.0 0.0)
     (linear-algebra:subtract list1 list2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((list1 '(#C(1.1 2.2) #C(3.3 4.4)))
        (list2 '(#C(1.1 2.2) #C(3.3 4.4))))
    (assert-float-equal
     '(#C(0.0 0.0) #C(0.0 0.0))
     (linear-algebra:subtract list1 list2))
    (assert-float-equal
     '(#C(1.1 2.2) #C(3.3 4.4))
     (linear-algebra:subtract list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(#C(-1.1 -2.2) #C(-3.3 -4.4))
     (linear-algebra:subtract list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(#C(0.0 0.0) #C(0.0 0.0))
     (linear-algebra:subtract list1 list2 :scalar1 2.0 :scalar2 2.0))))

;;; Destructive list subtraction

(deftest nsubtract-list (list-sequence-test)
  ;; Real
  (let ((list1 (list 1.1 2.2 3.3 4.4))
        (list2 (list 1.1 2.2 3.3 4.4)))
    (assert-eq list1 (linear-algebra:nsubtract list1 list2))
    (assert-float-equal '(0.0 0.0 0.0 0.0) list1)
    (assert-float-equal
     '(-2.2 -4.4 -6.6 -8.8)
     (linear-algebra:nsubtract list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(-5.5 -11.0 -16.5 -22.0)
     (linear-algebra:nsubtract list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(-13.2 -26.4 -39.6 -52.8)
     (linear-algebra:nsubtract list1 list2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((list1 (list #C(1.1 2.2) #C(3.3 4.4)))
        (list2 (list #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq list1 (linear-algebra:nsubtract list1 list2))
    (assert-float-equal
     '(#C(0.0 0.0) #C(0.0 0.0)) list1)
    (assert-float-equal
     '(#C(-2.2 -4.4) #C(-6.6 -8.8))
     (linear-algebra:nsubtract list1 list2 :scalar2 2.0))
    (assert-float-equal
     '(#C(-5.5 -11.0) #C(-16.5 -22.0))
     (linear-algebra:nsubtract list1 list2 :scalar1 2.0))
    (assert-float-equal
     '(#C(-13.2 -26.4) #C(-39.6 -52.8))
     (linear-algebra:nsubtract list1 list2 :scalar1 2.0 :scalar2 2.0))))

;;; List dot product

(deftest product-list (list-sequence-test)
  ;; Real vectors
  (assert-rational-equal
   55 (linear-algebra:product '(1 2 3 4 5) '(1 2 3 4 5)))
  (assert-float-equal
   55F0
   (linear-algebra:product
    '(1.0 2.0 3.0 4.0 5.0) '(1.0 2.0 3.0 4.0 5.0)))
  (assert-float-equal
   55D0
   (linear-algebra:product
    '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)
    '(1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)))
  ;; Real vectors with conjugate keyword
  (assert-rational-equal
   55 (linear-algebra:product '(1 2 3 4 5) '(1 2 3 4 5)))
  ;; Complex vectors
  (assert-rational-equal
   #C(8 18)
   (linear-algebra:product
    '(#C(1 1) #C(2 1) #C(3 1))
    '(#C(1 2) #C(2 2) #C(3 2))))
  (assert-float-equal
   #C(8.0 18.0)
   (linear-algebra:product
    '(#C(1.0 1.0) #C(2.0 1.0) #C(3.0 1.0))
    '(#C(1.0 2.0) #C(2.0 2.0) #C(3.0 2.0))))
  (assert-float-equal
   #C(8.0d0 18.0d0)
   (linear-algebra:product
    '(#C(1.0d0 1.0d0) #C(2.0d0 1.0d0) #C(3.0d0 1.0d0))
    '(#C(1.0d0 2.0d0) #C(2.0d0 2.0d0) #C(3.0d0 2.0d0)))))
