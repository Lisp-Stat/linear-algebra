;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:linalg)

;;; Common Lisp implementation of generic linear algebra and functions


;;; Mathematical methods

;; See: https://numpy.org/doc/stable/reference/generated/numpy.matmul.html
(defgeneric multiply (a b)
  (:method ((a array) (b array))
    (each-index (i j)
      (sum-index k
	(* (aref A i k) (aref B k j)))))
  (:method ((a array) (b vector))
    (let+ (((&dims nrow &ign) a)
	   (new-b (recycle b :outer nrow)))
      (sub (multiply a new-b) 1)))	;remove the extra dimension with sub
  (:method ((a vector) (b array))
    (let+ (((&dims nrow &ign) b)
	   (new-a (recycle a :outer nrow)))
      (sub (multiply new-a b) 1)))
  (:method ((a vector) (b vector))
    (sum (e* a b)))
  (:documentation "Matrix product of two arrays.  If A or B is a vector, it is reshaped as a matrix before multiplication.  If both A and B are vectors, returns the scalar inner product."))

(defun add (&rest arrays)
  "Add arguments element-wise"
  (apply #'e+ arrays))

(defun subtract (&rest arrays)
  "Subtract arguments element-wise"
  (apply #'e- arrays))



;;; Linear algebra methods

;; See: https://numpy.org/doc/stable/reference/generated/numpy.outer.html
(defgeneric outer (a b)
  (:method ((a array) (b array))
    (aops:outer #'* (aops:flatten a) (aops:flatten b)))
  (:documentation "Compute the outer product of two vectors"))

;; (defgeneric transpose (array)
;;   (:documentation "Return an array with axes transposed"))

(defgeneric scale (array scalar)
  (:method (array scale)
    (e* array scale))
  (:documentation "Scale each element by the scalar."))

(defgeneric invert (matrix)
  (:documentation "Return the multiplicative inverse of MATRIX"))

(defgeneric transpose (array)
  (:method (array)
    (nu:transpose array))
  (:documentation "Transpose array"))

















