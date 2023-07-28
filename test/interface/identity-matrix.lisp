;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite identity (matrix))

(deftest make-identity-matrix (identity)
  ;; A default identity matrix
  (let ((matrix (make-matrix 10 10 :matrix-type 'identity-matrix)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'identity-matrix))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-num= 1 (aref (linear-algebra::contents matrix) 1)))
  ;; Specify the identity matrix element type
  (let ((matrix (make-matrix 10 10 :matrix-type 'identity-matrix :element-type 'single-float)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'identity-matrix))
    (assert-eq (array-element-type (linear-algebra::contents matrix))
	(array-element-type (make-array '(10 10) :element-type 'single-float)))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-num= 1.0 (aref (linear-algebra::contents matrix) 1)))
  ;; Specify the identity matrix initial element
  (assert-condition error (make-matrix 10 10 :matrix-type 'identity-matrix :initial-element 1.0))
  ;; Specify the identity matrix contents - Nested list
  (assert-condition error (make-matrix 3 3 :matrix-type 'identity-matrix
					   :initial-contents '((1.0 0.0 0.0)
							       (0.0 1.0 0.0)
							       (0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - Nested vector
  (assert-condition error (make-matrix 3 3 :matrix-type 'identity-matrix
					   :initial-contents #(#(1.0 0.0 0.0)
							       #(0.0 1.0 0.0)
							       #(0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - 2D array
  (assert-condition error (make-matrix 3 3 :matrix-type 'identity-matrix
					   :initial-contents #2A((1.0 0.0 0.0)
								 (0.0 1.0 0.0)
								 (0.0 0.0 1.0))))
  ;; Specify initial element and initial contents
  (assert-condition error (make-matrix 3 3 :initial-element 1.0
					   :matrix-type 'identity-matrix
					   :initial-contents '((1.0 0.0 0.0)
							       (0.0 1.0 0.0)
							       (0.0 0.0 1.0)))))

;;; Test the identity matrix predicate
(deftest identity-matrix-predicate (identity)
  (assert-true (identity-matrix-p (make-matrix 10 10 :matrix-type 'identity-matrix)))
  (assert-false (identity-matrix-p (make-array '(10 10)))))

;;; Test the identity matrix bounds
(deftest identity-matrix-in-bounds-p (identity)
  (test-matrix-in-bounds-p 'identity-matrix))

;;; Test the identity matrix element type
(deftest identity-matrix-element-type (identity)
  (test-matrix-element-type 'identity-matrix))

;;; Test the identity matrix dimensions
(deftest identity-matrix-dimensions (identity)
  (test-matrix-dimensions 'identity-matrix 7 7))

;;; Test the identity matrix row dimension
(deftest identity-matrix-row-dimension (identity)
  (test-matrix-row-dimension 'identity-matrix 7 7))

;;; Test the identity matrix column dimension
(deftest identity-matrix-column-dimension (identity)
  (test-matrix-column-dimension 'identity-matrix 7 7))

;;; Reference identity matrix elements
(deftest identity-matrix-mref (identity)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random-interior-index columns)
                    (random-interior-index columns)))
               ((/= i0 rowi) i0)))
         (matrix (make-matrix rows columns :matrix-type 'identity-matrix :element-type 'single-float)))
    (assert-num= 1.0 (mref matrix 0 0))
    (assert-num= 0.0 (mref matrix 0 cend))
    (assert-num= 0.0 (mref matrix rend 0))
    (assert-num= 1.0 (mref matrix rend cend))
    (assert-num= 1.0 (mref matrix rowi rowi))
    (assert-num= 1.0 (mref matrix coli coli))
    (assert-num= 0.0 (mref matrix rowi coli))))

;;; Set identity matrix elements
(deftest identity-matrix-setf-mref (identity)
  (assert-condition error (setf (mref (make-matrix 10 10 :matrix-type 'identity-matrix)
				      (random 10) (random 10))
				1.0)))

;;; Copy the identity matrix
(deftest copy-identity-matrix (identity)
  (let ((matrix (make-matrix 5 5 :matrix-type 'identity-matrix)))
    (assert-true (identity-matrix-p (copy-matrix matrix)))
    (assert-false (eq matrix (copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (copy-matrix matrix))))))

;;; Test the submatrix of a identity matrix
(deftest identity-submatrix (identity)
  (assert-condition error (submatrix (make-matrix 10 10 :matrix-type 'identity-matrix) 5 5)))

(deftest setf-identity-submatrix (identity)
  (assert-condition error (setf (submatrix (make-matrix 10 10 :matrix-type 'identity-matrix) 5 5)
				(unit-matrix 5 5))))

(deftest identity-matrix-replace (identity)
  ;; Replace the entire matrix
  (assert-condition error (replace-matrix (make-matrix 5 5 :matrix-type 'identity-matrix)
					  (unit-matrix 5 5))))

;;; Validate a range for an identity matrix.
(deftest identity-matrix-validated-range (identity)
  (test-matrix-validated-range
   'identity-matrix 10 10))
