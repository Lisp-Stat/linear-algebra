;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Fundamental Vector Operations

(in-package #:linear-algebra)

(defmethod norm ((data vector) &optional (measure 1))
  (norm-vector data measure))

(defmethod transpose ((data vector))
  "Return a row vector."
  (let ((result (make-array (length data) :element-type (array-element-type data))))
    (dotimes (index (length data) result)
      (setf (aref result index) (conjugate (aref data index))))))

(defmethod ntranspose ((data vector))
  "Return a row vector destructively."
  data)

(defmethod permute ((data vector) (matrix permutation-matrix))
  "Return the permutation of the list."
  (if (= (length data) (matrix-row-dimension matrix))
      (right-permute data (contents matrix))
      (error "Vector(~D) and permutation matrix~A are incompatible."
	     (length data) (matrix-dimensions matrix))))

(defmethod permute ((matrix permutation-matrix) (data vector))
  "Return the permutation of the list."
  (if (= (length data) (matrix-column-dimension matrix))
      (left-permute (contents matrix) data)
      (error "Permutation matrix~A and vector(~D) are incompatible."
	     (matrix-dimensions matrix) (length data))))

(defmethod scale ((scalar number) (data vector))
  "Return the vector scaled by scalar."
  (let ((result (make-array (length data) :element-type (array-element-type data))))
    (dotimes (index (length data) result)
      (setf (aref result index) (* scalar (aref data index))))))

(defmethod nscale ((scalar number) (data vector))
  "Return the vector destructively scaled by scalar."
  (dotimes (index (length data) data)
    (setf (aref data index) (* scalar (aref data index)))))

(defmethod add ((vector1 vector) (vector2 vector) &key scalar1 scalar2)
  "Return the addition of scalar1*vector1 with scalar2*vector2"
  (if (= (length vector1) (length vector2))
      (add-vector vector1 vector2 scalar1 scalar2)
      (error "VECTOR1(~D) and VECTOR2(~D) are not of equal length."
             (length vector1) (length vector2))))

(defmethod nadd ((vector1 vector) (vector2 vector) &key scalar1 scalar2)
  "Return the addition of scalar2*vector2 to scalar1*vector1."
  (if (= (length vector1) (length vector2))
      (nadd-vector vector1 vector2 scalar1 scalar2)
      (error "VECTOR1(~D) and VECTOR2(~D) are not of equal length."
             (length vector1) (length vector2))))

(defmethod subtract ((vector1 vector) (vector2 vector) &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (if (= (length vector1) (length vector2))
      (subtract-vector vector1 vector2 scalar1 scalar2)
      (error "VECTOR1(~D) and VECTOR2(~D) are not of equal length."
             (length vector1) (length vector2))))

(defmethod nsubtract ((vector1 vector) (vector2 vector) &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (if (= (length vector1) (length vector2))
      (nsubtract-vector vector1 vector2 scalar1 scalar2)
      (error "VECTOR1(~D) and VECTOR2(~D) are not of equal length."
             (length vector1) (length vector2))))

(defmethod product ((vector1 vector) (vector2 vector) &optional scalar)
  "Return the dot product of vector1 and vector2."
  (if (= (length vector1) (length vector2))
      (inner-product-vector vector1 vector2 scalar)
      (error "VECTOR1(~D) and VECTOR2(~D) are not of equal length."
             (length vector1) (length vector2))))
