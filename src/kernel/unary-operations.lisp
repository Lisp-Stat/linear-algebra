;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Linear Algebra Unary Operations Kernel

(in-package #:linear-algebra-kernel)

(defgeneric sump (vector-or-array p &optional scale sump)
  (:documentation "Return the scaling parameter and the sum of the P powers."))

(defmethod sump ((data list) (p real) &optional (scale 1) (sump 0))
  "Return the scaling parameter and the sum of the powers of p of the data."
  (let ((abs-val))
    (dolist (elm data (values scale sump))
      (when (plusp (setq abs-val (abs elm)))
        (if (< scale abs-val)
            (setq sump (1+ (* sump (expt (/ scale abs-val) p)))
		  scale abs-val)
            (setq sump (+ sump (expt (/ elm scale) p))))))))

(defmethod sump ((data vector) p &optional (scale 1) (sump 0))
  "Return the scaling parameter and the sum of the powers of p of the vector."
  (let ((abs-val))
    (dotimes (index (length data) (values scale sump))
      (when (plusp (setq abs-val (abs (aref data index))))
        (if (< scale abs-val)
            (setq sump (1+ (* sump (expt (/ scale abs-val) p)))
		  scale abs-val)
            (setq sump (+ sump (expt (/ (aref data index) scale) p))))))))

(defmethod sump ((data array) p &optional (scale 1) (sump 0))
  "Return the scaling parameter and the sum of the P powers of the matrix."
  (unless (plusp p) (error "The power(~A) must be positive." p))
  (let ((m-rows (array-dimension data 0))
        (n-columns (array-dimension data 1))
        (abs-val 0))
    (dotimes (row m-rows (values scale sump))
      (dotimes (column n-columns)
        (when (plusp (setq abs-val (abs (aref data row column))))
          (if (< scale abs-val)
              (setq sump (1+ (* sump (expt (/ scale abs-val) p)))
		    scale abs-val)
              (setq sump (+ sump (expt (/ (aref data row column) scale) p)))))))))

(defun sumsq-row (array row &key (scale 1) (sumsq 0) start end)
  "Return the scaling parameter and the sum of the squares of the array row."
  (loop
    with start = (or start 0)
    and end = (or end (array-dimension array 1))
    and abs-val = 0
    for column from start below end
    when (plusp (setq abs-val (abs (aref array row column)))) do
      (if (< scale abs-val)
	  (setq sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
		scale abs-val)
	  (setq sumsq (+ sumsq (expt (/ abs-val scale) 2))))
    finally (return (values scale sumsq))))

(defun sumsq-column (array column &key (scale 1) (sumsq 0) start end)
  "Return the scaling parameter and the sum of the squares of the array column."
  (loop
    with start = (or start 0)
    and end = (or end (array-dimension array 0))
    and abs-val = 0
    for row from start below end
    when (plusp (setq abs-val (abs (aref array row column)))) do
      (if (< scale abs-val)
	  (setq sumsq (1+ (* sumsq (expt (/ scale abs-val) 2)))
		scale abs-val)
	  (setq sumsq (+ sumsq (expt (/ abs-val scale) 2))))
    finally (return (values scale sumsq))))

;;; Norm

(defgeneric norm-vector (data measure)
  (:documentation "Return the norm of the vector according to the measure."))

(defgeneric norm-array (data measure)
  (:documentation "Return the norm of the array according to the measure."))

(defun %abs-vector (vector)
  "Return a vector containing absolute value of each element."
  (let ((result (make-array (length vector) :element-type (array-element-type vector))))
    (dotimes (index (length vector) result)
      (setf (aref result index) (abs (aref vector index))))))

(defmethod norm-vector ((data vector) (measure (eql 1)))
  "Return the Taxicab norm of the list."
  (loop for element across data sum (abs element)))

(defmethod norm-vector ((data vector) (ord (eql 2)))
  "Return the Euclidean norm of the vector."
  (expt (sum (eexpt (eabs data) ord)) (/ ord)))

(defmethod norm-vector ((data vector) (measure integer))
  "Return the p-norm of the vector."
  (multiple-value-bind (scale sump) (sump (%abs-vector data) measure)
    (* scale (expt sump (/ measure)))))

(defmethod norm-vector ((data vector) (measure (eql :infinity)))
  "Return the infinity, or maximum, norm of vector."
  (loop for element across data maximize (abs element)))

(defmethod norm-array ((data array) (measure (eql 1)))
  "Return the 1 norm of the array."
  (let ((m-rows (array-dimension data 0))
        (n-columns (array-dimension data 1))
        (norm 0)
        (sum 0))
    (dotimes (column n-columns norm)
      (setq sum 0)
      (dotimes (row m-rows)
        (setq sum (+ sum (abs (aref data row column)))))
      (setq norm (max sum norm)))))

(defmethod norm-array ((data array) (measure (eql :max)))
  "Return the max norm of the array."
  (let ((m-rows (array-dimension data 0))
        (n-columns (array-dimension data 1))
        (norm 0))
    (dotimes (row m-rows norm)
      (dotimes (column n-columns)
        (setq norm (max norm (abs (aref data row column))))))))

(defmethod norm-array ((data array) (measure (eql :frobenius)))
  "Return the Frobenius norm of the array."
  (expt (sum (eexpt (eabs (aops:flatten data)) 2)) (/ 2)))

(defmethod norm-array ((data array) (measure (eql :infinity)))
  "Return the infinity norm of the array."
  (let ((m-rows (array-dimension data 0))
        (n-columns (array-dimension data 1))
        (norm 0)
        (sum 0))
    (dotimes (row m-rows norm)
      (setq sum 0)
      (dotimes (column n-columns)
        (setq sum (+ sum (abs (aref data row column)))))
      (setq norm (max sum norm)))))
