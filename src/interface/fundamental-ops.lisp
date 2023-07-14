;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Linear Algebra in Common Lisp

(in-package #:linear-algebra)

;;; External Interface

(defgeneric norm (vector-or-matrix &optional measure)
  (:documentation "Return the norm according to measure."))

(defgeneric transpose (vector-or-matrix)
  (:documentation "Transpose the vector or matrix."))

(defgeneric ntranspose (vector-or-matrix)
  (:documentation "Destructively transpose the vector or matrix."))

(defgeneric permute (vector-or-matrix-1 vector-or-matrix-2)
  (:documentation "Permute the vector or matrix."))

(defgeneric scale (scalar vector-or-matrix)
  (:documentation "Scale each element by the scalar."))

(defgeneric nscale (scalar vector-or-matrix)
  (:documentation "Destructively scale each element by the scalar."))

(defgeneric add
    (vector-or-matrix-1 vector-or-matrix-2 &key scalar1 scalar2)
  (:documentation "Vector or matrix binary addition."))

(defgeneric nadd
    (vector-or-matrix-1 vector-or-matrix-2 &key scalar1 scalar2)
  (:documentation "Destructive vector or matrix addition."))

(defgeneric subtract
    (vector-or-matrix-1 vector-or-matrix-2 &key scalar1 scalar2)
  (:documentation "Vector or matrix binary subtraction."))

(defgeneric nsubtract
    (vector-or-matrix-1 vector-or-matrix-2 &key scalar1 scalar2)
  (:documentation "Destructive vector or matrix subtraction."))

(defgeneric product
    (vector-or-matrix-1 vector-or-matrix-2 &optional scalar)
  (:documentation "Return the vector-vector, matrix-vector or matrix-matrix product."))

(defgeneric solve (matrix vector)
  (:documentation
   "Return the solution to the system of equations."))

(defgeneric nsolve (matrix vector)
  (:documentation "Return the solution to the system of equations in-place."))

(defgeneric invert (matrix)
  (:documentation "Return the invert of the matrix."))

(defgeneric ninvert (matrix)
  (:documentation "Return the invert of the matrix with in-place decomposition."))
