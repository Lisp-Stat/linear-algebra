;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra)

;;; Matrix superclass

(defclass matrix-object ()
  ()
  (:documentation "A superclass for all matrices."))

;;; Matrix interface operations

(defgeneric initialize-matrix-contents (matrix initial-contents initargs)
  (:documentation "Initialize the matrix with data."))

(defun make-matrix (rows columns &key
				   (matrix-type 'dense-matrix)
				   (element-type 'number)
				   initial-element initial-contents)
  "Return a new matrix instance."
  (make-instance matrix-type :dimensions (list rows columns)
			     :element-type element-type
			     :initial-element initial-element
			     :initial-contents initial-contents))

(defun matrixp (object)
  "Return true if object is a matrix, NIL otherwise."
  (typep object 'matrix-object))

(defgeneric matrix-in-bounds-p (matrix row column)
  (:documentation "Return true if ROW and COLUMN do not exceed the dimensions of
MATRIX."))

(defgeneric matrix-element-type (matrix)
  (:documentation "Return the element type of MATRIX."))

(defgeneric matrix-dimensions (matrix)
  (:documentation "Return the number of rows and columns in MATRIX."))

(defgeneric matrix-row-dimension (matrix)
  (:documentation "Return the number of rows in MATRIX."))

(defgeneric matrix-column-dimension (matrix)
  (:documentation "Return the number of columns in MATRIX."))

(defgeneric mref (matrix row column)
  (:documentation "Return the matrix element at ROW,COLUMN."))

(defgeneric (setf mref) (data matrix row column)
  (:documentation "Set the element at row,column of matrix to data."))

(defgeneric copy-matrix (matrix)
  (:documentation "Return a copy of the matrix."))

(defgeneric submatrix (matrix start-row start-column &key end-row end-column)
  (:documentation "Return a submatrix of the matrix."))

(defgeneric (setf submatrix)
    (submatrix matrix start-row start-column &key end-row end-column)
  (:documentation "Set the submatrix of the matrix."))

(defgeneric replace-matrix (matrix1 matrix2 &key
					      start-row1 end-row1
					      start-column1 end-column1
					      start-row2 end-row2
					      start-column2 end-column2)
  (:documentation "Destructively replace elements of matrix1 with matrix2."))

(defun matrix-validated-range (matrix start-row start-column &optional end-row end-column)
  "Returns a validated range of rows and columns for the matrix."
  (let* ((row-dimension    (matrix-row-dimension matrix))
         (column-dimension (matrix-column-dimension matrix))
         (end-row    (or end-row row-dimension))
         (end-column (or end-column column-dimension)))
    (if (and (<= 0 start-row end-row row-dimension)
             (<= 0 start-column end-column column-dimension))
        (values start-row start-column end-row end-column)
        (error "The matrix range (~D:~D,~D:~D) is invalid."
               start-row start-column end-row end-column))))
