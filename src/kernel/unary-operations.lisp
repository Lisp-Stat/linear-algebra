;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Linear Algebra Unary Operations Kernel

(in-package #:linear-algebra-kernel)

;;; These functions are used in the various decompositions and it's
;;; easier to leave them here until we refactor and improve the
;;; decompositions.  This is scheduled to happen when we convert to
;;; using the super-classes from num-utils.

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
