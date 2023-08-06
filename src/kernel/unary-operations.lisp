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
