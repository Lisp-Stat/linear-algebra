;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra)

(defclass square-matrix (dense-matrix)
  ()
  (:documentation "Square matrix object."))

;;; Square matrix interface operations

(defun square-matrix-p (object)
  "Return true if OBJECT is a square matrix."
  (typep object 'square-matrix))

(defmethod initialize-matrix-contents :before
  ((matrix square-matrix) initial-contents initargs)
  "Verify that the number of rows and colums are equal."
  (unless (apply #'= (getf initargs :dimensions))
    (error "Number of rows must equal the number of columns.")))

(defmethod submatrix
    ((matrix square-matrix)
     (start-row integer) (start-column integer)
     &key end-row end-column)
  "Return a matrix created from the submatrix of matrix."
  (multiple-value-bind (start-row start-column end-row end-column)
      (matrix-validated-range
       matrix start-row start-column end-row end-column)
    (let* ((m-rows (- end-row start-row))
           (n-columns (- end-column start-column))
           (original (contents matrix))
           (contents
            (make-array
             (list m-rows n-columns)
             :element-type (matrix-element-type matrix))))
      (make-instance
       (if (= m-rows n-columns) 'square-matrix 'dense-matrix)
       :contents
       (dotimes (row m-rows contents)
         (dotimes (column n-columns)
           (setf
            (aref contents row column)
            (aref original
                  (+ start-row row)
                  (+ start-column column)))))))))

(defmethod compatible-dimensions-p
    ((operation (eql :solve))
     (matrix square-matrix)
     (vector column-vector))
  "Return true if the array dimensions are compatible for product."
  (= (matrix-column-dimension matrix) (vector-length vector)))

(defmethod invert ((matrix square-matrix))
  "Return the invert of the square matrix."
  (make-instance
   (class-of matrix)
   :contents
   (gauss-invert (copy-array (contents matrix)))))

(defmethod ninvert ((matrix square-matrix))
  "Return the invert of the square matrix."
  (make-instance
   (class-of matrix)
   :contents
   (gauss-invert (contents matrix))))
