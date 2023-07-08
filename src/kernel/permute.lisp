;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Linear Algebra Permutation Kernel

(in-package :linear-algebra-kernel)

;;; Right permutation

(defgeneric right-permute (vector-or-array permutation)
  (:documentation
   "Permute the row vector or columns of the array."))

(defmethod right-permute ((data vector) (permutation vector))
  "Permute the row vector to create a column vector."
  (loop
   with result =
   (make-array (length data) :element-type (array-element-type data))
   for column across permutation
   and row = 0 then (1+ row)
   do (setf (aref result column) (aref data row))
   finally (return result)))

(defmethod right-permute ((data array) (permutation vector))
  "Permute the columns of the array."
  (loop
   with m-rows = (array-dimension data 0)
   with result =
   (make-array
    (array-dimensions data) :element-type (array-element-type data))
   for column across permutation
   and row = 0 then (1+ row)
   do
   (loop
    for irow below m-rows do
    (setf (aref result irow column) (aref data irow row)))
   finally (return result)))

;;; Left permutation

(defgeneric left-permute (permutation vector-or-array)
  (:documentation
   "Permute the column vector or rows of the array."))

(defmethod left-permute ((permutation vector) (data vector))
  "Permute the column vector to create a row vector."
  (loop
   with result =
   (make-array (length data) :element-type (array-element-type data))
   for column across permutation
   and row = 0 then (1+ row)
   do (setf (aref result row) (aref data column))
   finally (return result)))

(defmethod left-permute ((permutation vector) (data array))
  "Permute the rows of the array."
  (loop
   with n-columns = (array-dimension data 1)
   with result =
   (make-array
    (array-dimensions data) :element-type (array-element-type data))
   for column across permutation
   and row = 0 then (1+ row)
   do
   (loop
    for icol below n-columns do
    (setf (aref result row icol) (aref data column icol)))
   finally (return result)))
