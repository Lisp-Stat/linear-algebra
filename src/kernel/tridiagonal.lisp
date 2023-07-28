;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;;  Linear Algebra Tridiagonal Algorithm

;;; References
;;; [NumAlgoC] Gisela Engeln-Mullges and Frank Uhlig "Numerical
;;;            Algorithms with C", Springer, 1996
;;;            ISBN: 3-540-60530-4

(in-package #:linear-algebra-kernel)

;;; Algorithm 4.32; Step 1

(defun tridiagonal-factorization (array)
  "Return the factorization of the tridiagonal array."
  (loop
    with end = (1- (array-dimension array 0))
      initially
	 (setf (aref array 0 2) (/ (aref array 0 2) (aref array 0 1)))
    for row from 1 below end
    as alpha = (- (aref array row 1) (* (aref array row 0) (aref array (1- row) 2)))
    do (setf (aref array row 1) alpha
	     (aref array row 2) (/ (aref array row 2) alpha))
    finally (decf (aref array end 1)
		  (* (aref array end 0) (aref array (1- end) 2)))
	    (return array)))

;;; Algorithm 4.32; Step 2

(defun tridiagonal-update (array vector)
  "Update the solution vector using the factored array."
  (loop
    with end = (1- (array-dimension array 0))
      initially
	 (setf (aref vector 0) (/ (aref vector 0) (aref array 0 1)))
    for row from 1 upto end
    do (setf (aref vector row)
	     (/ (- (aref vector row)
		   (* (aref array row 0) (aref vector (1- row))))
		(aref array row 1)))
    finally (return vector)))

;;; Algorithm 4.32; Step 3

(defun tridiagonal-backsubstitution (array vector)
  "Perform backsubstitution to obtain the solution."
  (loop
    with end = (1- (array-dimension array 0))
    for row from (1- end) downto 0
    do (decf (aref vector row)
	     (* (aref array row 2) (aref vector (1+ row))))
    finally (return vector)))

;;; Algorithm 4.32, pg. 91
;;; Linear equation solver for tridiagonal A

(defun tridiagonal-solver (array vector)
  "Linear equation solver for a tridiagonal matrix."
  (progn
    (tridiagonal-factorization array)
    (tridiagonal-update array vector)
    (tridiagonal-backsubstitution array vector)))
