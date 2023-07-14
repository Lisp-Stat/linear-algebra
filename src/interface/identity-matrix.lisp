;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra)

(defclass identity-matrix (matrix-object)
  ((size
    :type    fixnum
    :initarg :size
    :reader  size
    :reader  matrix-row-dimension
    :reader  matrix-column-dimension)
   (contents
    :type    (array * (2))
    :initarg :contents
    :reader  contents))
  (:documentation
   "Identity matrix object."))

(defun identity-matrix-p (object)
  "Return true if object is an identity-matrix."
  (typep object 'identity-matrix))

(defmethod initialize-instance :after
    ((self identity-matrix) &rest initargs
     &key dimensions element-type initial-element initial-contents)
  "Initialize the identity matrix."
  (declare (ignore initargs))
  (cond
   ((slot-boundp self 'contents))
   ((or initial-element initial-contents)
    (error "Initial data is invalid for an identity matrix."))
   ((not (apply #'= dimensions))
    (error "Rows and columns are not equal."))
   (t
    (setf
     (slot-value self 'size) (first dimensions)
     (slot-value self 'contents)
     (make-array
      2 :element-type element-type
      :initial-contents
      (list (coerce 0 element-type) (coerce 1 element-type)))))))

(defmethod matrix-in-bounds-p
    ((matrix identity-matrix) (row integer) (column integer))
  "Return true if row and column do not exceed the dimensions of matrix."
  (and
   (<= 0 row)    (< row    (size matrix))
   (<= 0 column) (< column (size matrix))))

(defmethod matrix-element-type ((matrix identity-matrix))
  "Return the element type of the identity matrix."
  (array-element-type (contents matrix)))

(defmethod matrix-dimensions ((matrix identity-matrix))
  "Return the number of rows and columns in matrix."
  (list (size matrix) (size matrix)))

(defmethod mref
    ((matrix identity-matrix) (row integer) (column integer))
  "Return the element of the matrix at row,column."
  (if (= row column)
      (aref (contents matrix) 1)
      (aref (contents matrix) 0)))

(defmethod copy-matrix ((matrix identity-matrix))
  "Return a copy of the matrix."
  (let ((element-type (matrix-element-type matrix)))
    (make-instance
     'identity-matrix
     :size (size matrix)
     :contents
     (make-array
      2 :element-type element-type
      :initial-contents
      (list (coerce 0 element-type) (coerce 1 element-type))))))

