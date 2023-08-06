;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite vector (sequence))

(deftest norm-vector (vector)
  ;; Taxicab norm
  (assert-num= 36 (linear-algebra:norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 1))
  (assert-num= 19.535658 (linear-algebra:norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
						#C(-2 3) #C(-3 1) #C(-1 0))
					      1))
  ;; Euclidean norm
  (assert-num= 12.083046 (linear-algebra:norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) 2))
  (assert-num= 8.0 (linear-algebra:norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4) #C(-2 3) #C(-3 1) #C(-1 0)) 2))
  ;; P-norm
  (let ((data #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5))
        (zdata #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
                 #C(-2 3) #C(-3 1) #C(-1 0))))
    (assert-num= 8.732892 (linear-algebra:norm data 3))
    (assert-num= 6.064035 (linear-algebra:norm zdata 3)))
  ;; Infinity norm
  (assert-num= 6 (linear-algebra:norm #(-6 -5 -4 -3 -2 -1 0 1 2 3 4 5) :infinity))
  (assert-num= 4.0 (linear-algebra:norm #(#C(1 0) #C(3 1) #C(2 3) #C(0 4)
					  #C(-2 3) #C(-3 1) #C(-1 0)) :infinity)))

;;; Vector transpose

(deftest transpose-vector (vector)
  (assert-num= #(1.0 2.0 3.0 4.0 5.0) (linear-algebra:transpose #(1.0 2.0 3.0 4.0 5.0))))

(deftest ntranspose-vector (vector)
  (let ((data (vector 1.0 2.0 3.0 4.0 5.0)))
    (assert-equal data (linear-algebra:ntranspose data))))

;;; Vector permutation

(deftest permute-vector (vector)
  (let ((vect (vector 1.1 2.2 3.3 4.4 5.5))
        (pmat (linear-algebra:make-matrix 5 5 :matrix-type 'linear-algebra:permutation-matrix
					      :initial-contents '((0 0 1 0 0)
								  (0 0 0 0 1)
								  (1 0 0 0 0)
								  (0 1 0 0 0)
								  (0 0 0 1 0)))))
    (assert-num= #(3.3 4.4 1.1 5.5 2.2) (linear-algebra:permute vect pmat))
    (assert-num= #(3.3 5.5 1.1 2.2 4.4) (linear-algebra:permute pmat vect)))
  (let ((vect (vector 1.1 2.2 3.3 4.4 5.5))
        (pmat (linear-algebra:make-matrix 5 5 :matrix-type 'linear-algebra:permutation-matrix
					      :initial-contents '((0 0 0 0 1)
								  (0 0 1 0 0)
								  (1 0 0 0 0)
								  (0 1 0 0 0)
								  (0 0 0 1 0)))))
    (assert-num= #(3.3 4.4 2.2 5.5 1.1) (linear-algebra:permute vect pmat))
    (assert-num= #(5.5 3.3 1.1 2.2 4.4) (linear-algebra:permute pmat vect))))

;;; Vector scale

(deftest scale-vector (vector)
  (assert-num= #(2.0 4.0 6.0 8.0 10.0) (linear-algebra:scale 2.0 #(1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))
      (linear-algebra:scale #C(1.0 1.0) #(1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0))
      (linear-algebra:scale 2.0 #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))))
  (assert-num= #(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0))
      (linear-algebra:scale #C(2.0 2.0) #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0)))))

(deftest nscale-vector (vector)
  ;; Real
  (let ((vector1 #(1.1 2.2 3.3 4.4))
        (vector2 #(1.1 2.2 3.3 4.4)))
    (assert-num= #(2.2 4.4 6.6 8.8) (linear-algebra:add vector1 vector2))
    (assert-num= #(3.3 6.6 9.9 13.2) (linear-algebra:add vector1 vector2 :scalar1 2.0))
    (assert-num= #(3.3 6.6 9.9 13.2) (linear-algebra:add vector1 vector2 :scalar2 2.0))
    (assert-num= #(4.4 8.8 13.2 17.6) (linear-algebra:add vector1 vector2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vector1 #(#C(1.1 2.2) #C(3.3 4.4)))
        (vector2 #(#C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) (linear-algebra:add vector1 vector2))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (linear-algebra:add vector1 vector2 :scalar1 2.0))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (linear-algebra:add vector1 vector2 :scalar2 2.0))
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6)) (linear-algebra:add vector1 vector2 :scalar1 2.0 :scalar2 2.0))))

;;; Destructive vector addition
#|
None of these test vectors, they test lists
(defun sequence-equal-p (seq1 seq2)
  "Assumes SEQ1 is a VECTOR and SEQ2 is a cons."
  (loop
    for x across seq1
    for y in seq2
    always (num= x y)))

(deftest nadd-vector  (vector)
  (let ((list (list 1.0 2.0 3.0 4.0 5.0)))
    (assert-eq list (linear-algebra:nscale 2.0 list))
    (assert-true (sequence-equal-p #(2.0 4.0 6.0 8.0 10.0) list)))
  (let ((list (list 1.0 2.0 3.0 4.0 5.0)))
    (assert-eq list (linear-algebra:nscale #C(1.0 1.0) list))
    (assert-true (sequence-equal-p #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0)) list)))
  (let ((list (list #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0)
                    #C(4.0 4.0) #C(5.0 5.0))))
    (assert-eq list (linear-algebra:nscale 2.0 list))
    (assert-true (sequence-equal-p #(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0)) list)))
  (let ((list (list #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0)
                    #C(4.0 4.0) #C(5.0 5.0))))
    (assert-eq list (linear-algebra:nscale #C(2.0 2.0) list))
    (assert-true (sequence-equal-p #(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0)) list))))
|#
;;; Vector addition

(deftest add-vector (vector)
  ;; Real
  (let ((vector1 (vector 1.1 2.2 3.3 4.4))
        (vector2 (vector 1.1 2.2 3.3 4.4)))
    (assert-eq vector1 (linear-algebra:nadd vector1 vector2))
    (assert-num= #(2.2 4.4 6.6 8.8) vector1)
    (assert-num= #(4.4 8.8 13.2 17.6) (linear-algebra:nadd vector1 vector2 :scalar2 2.0))
    (assert-num= #(9.9 19.8 29.7 39.6)(linear-algebra:nadd vector1 vector2 :scalar1 2.0))
    (assert-num= #(22.0 44.0 66.0 88.0) (linear-algebra:nadd vector1 vector2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vector1 (vector #C(1.1 2.2) #C(3.3 4.4)))
        (vector2 (vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vector1 (linear-algebra:nadd vector1 vector2))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) vector1)
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6)) (linear-algebra:nadd vector1 vector2 :scalar2 2.0))
    (assert-num= #(#C(9.9 19.8) #C(29.7 39.6))(linear-algebra:nadd vector1 vector2 :scalar1 2.0))
    (assert-num= #(#C(22.0 44.0) #C(66.0 88.0)) (linear-algebra:nadd vector1 vector2 :scalar1 2.0 :scalar2 2.0))))

;;; Vector subtraction

(deftest subtract-vector  (vector)
  ;; Real
  (let ((vector1 #(1.1 2.2 3.3 4.4))
        (vector2 #(1.1 2.2 3.3 4.4)))
    (assert-num= #(0.0 0.0 0.0 0.0) (linear-algebra:subtract vector1 vector2))
    (assert-num= #(1.1 2.2 3.3 4.4) (linear-algebra:subtract vector1 vector2 :scalar1 2.0))
    (assert-num= #(-1.1 -2.2 -3.3 -4.4) (linear-algebra:subtract vector1 vector2 :scalar2 2.0))
    (assert-num= #(0.0 0.0 0.0 0.0) (linear-algebra:subtract vector1 vector2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vector1 #(#C(1.1 2.2) #C(3.3 4.4)))
        (vector2 #(#C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (linear-algebra:subtract vector1 vector2))
    (assert-num= #(#C(1.1 2.2) #C(3.3 4.4)) (linear-algebra:subtract vector1 vector2 :scalar1 2.0))
    (assert-num= #(#C(-1.1 -2.2) #C(-3.3 -4.4)) (linear-algebra:subtract vector1 vector2 :scalar2 2.0))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (linear-algebra:subtract vector1 vector2 :scalar1 2.0 :scalar2 2.0))))

;;; Destructive vector subtraction

(deftest nsubtract-vector (vector)
  ;; Real
  (let ((vector1 (vector 1.1 2.2 3.3 4.4))
        (vector2 (vector 1.1 2.2 3.3 4.4)))
    (assert-eq vector1 (linear-algebra:nsubtract vector1 vector2))
    (assert-num= #(0.0 0.0 0.0 0.0) vector1)
    (assert-num= #(-2.2 -4.4 -6.6 -8.8) (linear-algebra:nsubtract vector1 vector2 :scalar2 2.0))
    (assert-num= #(-5.5 -11.0 -16.5 -22.0) (linear-algebra:nsubtract vector1 vector2 :scalar1 2.0))
    (assert-num= #(-13.2 -26.4 -39.6 -52.8) (linear-algebra:nsubtract vector1 vector2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vector1 (vector #C(1.1 2.2) #C(3.3 4.4)))
        (vector2 (vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vector1 (linear-algebra:nsubtract vector1 vector2))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) vector1)
    (assert-num= #(#C(-2.2 -4.4) #C(-6.6 -8.8)) (linear-algebra:nsubtract vector1 vector2 :scalar2 2.0))
    (assert-num= #(#C(-5.5 -11.0) #C(-16.5 -22.0)) (linear-algebra:nsubtract vector1 vector2 :scalar1 2.0))
    (assert-num= #(#C(-13.2 -26.4) #C(-39.6 -52.8)) (linear-algebra:nsubtract vector1 vector2 :scalar1 2.0 :scalar2 2.0))))

;;; Vector dot product

(deftest product-vector (vector)
  ;; Real vectors
  (assert-num= 55 (linear-algebra:product #(1 2 3 4 5) #(1 2 3 4 5)))
  (assert-num= 55F0 (linear-algebra:product  #(1.0 2.0 3.0 4.0 5.0) #(1.0 2.0 3.0 4.0 5.0)))
  (assert-num= 55D0 (linear-algebra:product #(1D0 2D0 3D0 4D0 5D0)
					    #(1D0 2D0 3D0 4D0 5D0)))
  ;; Real vectors with conjugate keyword
  (assert-num= 55 (linear-algebra:product #(1 2 3 4 5) #(1 2 3 4 5)))
  ;; Complex vectors
  (assert-num= #C(8 18) (linear-algebra:product #(#C(1 1) #C(2 1) #C(3 1))
						#(#C(1 2) #C(2 2) #C(3 2))))
  (assert-num= #C(8.0 18.0) (linear-algebra:product #(#C(1.0 1.0) #C(2.0 1.0) #C(3.0 1.0))
						    #(#C(1.0 2.0) #C(2.0 2.0) #C(3.0 2.0))))
  (assert-num= #C(8D0 18D0) (linear-algebra:product #(#C(1D0 1D0) #C(2D0 1D0) #C(3D0 1D0))
						    #(#C(1D0 2D0) #C(2D0 2D0) #C(3D0 2D0)))))
