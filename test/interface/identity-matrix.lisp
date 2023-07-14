;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite identity (matrix))

(deftest make-identity-matrix (identity)
  ;; A default identity matrix
  (let ((matrix (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:identity-matrix))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-true (num= 1 (aref (linear-algebra::contents matrix) 1))))
  ;; Specify the identity matrix element type
  (let ((matrix (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix :element-type 'single-float)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:identity-matrix))
    (assert-eq (array-element-type (linear-algebra::contents matrix))
	(array-element-type (make-array '(10 10) :element-type 'single-float)))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-true (num= 1.0 (aref (linear-algebra::contents matrix) 1))))
  ;; Specify the identity matrix initial element
  (assert-condition error (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix :initial-element 1.0))
  ;; Specify the identity matrix contents - Nested list
  (assert-condition error (linear-algebra:make-matrix 3 3 :matrix-type 'linear-algebra:identity-matrix
							  :initial-contents '((1.0 0.0 0.0)
									      (0.0 1.0 0.0)
									      (0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - Nested vector
  (assert-condition error (linear-algebra:make-matrix 3 3 :matrix-type 'linear-algebra:identity-matrix
							  :initial-contents #(#(1.0 0.0 0.0)
									      #(0.0 1.0 0.0)
									      #(0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - 2D array
  (assert-condition error (linear-algebra:make-matrix 3 3 :matrix-type 'linear-algebra:identity-matrix
							  :initial-contents #2A((1.0 0.0 0.0)
										(0.0 1.0 0.0)
										(0.0 0.0 1.0))))
  ;; Specify initial element and initial contents
  (assert-condition error (linear-algebra:make-matrix 3 3 :initial-element 1.0
							  :matrix-type 'linear-algebra:identity-matrix
							  :initial-contents '((1.0 0.0 0.0)
									      (0.0 1.0 0.0)
									      (0.0 0.0 1.0)))))

;;; Test the identity matrix predicate
(deftest identity-matrix-predicate (identity)
  (assert-true (linear-algebra:identity-matrix-p (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix)))
  (assert-false (linear-algebra:identity-matrix-p (make-array '(10 10)))))

;;; Test the identity matrix bounds
(deftest identity-matrix-in-bounds-p (identity)
  (test-matrix-in-bounds-p 'linear-algebra:identity-matrix))

;;; Test the identity matrix element type
(deftest identity-matrix-element-type (identity)
  (test-matrix-element-type 'linear-algebra:identity-matrix))

;;; Test the identity matrix dimensions
(deftest identity-matrix-dimensions (identity)
  (test-matrix-dimensions 'linear-algebra:identity-matrix 7 7))

;;; Test the identity matrix row dimension
(deftest identity-matrix-row-dimension (identity)
  (test-matrix-row-dimension 'linear-algebra:identity-matrix 7 7))

;;; Test the identity matrix column dimension
(deftest identity-matrix-column-dimension (identity)
  (test-matrix-column-dimension 'linear-algebra:identity-matrix 7 7))

;;; Reference identity matrix elements
(deftest identity-matrix-mref (identity)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli
          (do ((i0 (random-interior-index columns)
                   (random-interior-index columns)))
              ((/= i0 rowi) i0)))
         (matrix (linear-algebra:make-matrix rows columns
					     :matrix-type 'linear-algebra:identity-matrix
					     :element-type 'single-float)))
    (assert-true (num= 1.0 (linear-algebra:mref matrix 0 0)))
    (assert-true (num= 0.0 (linear-algebra:mref matrix 0 cend)))
    (assert-true (num= 0.0 (linear-algebra:mref matrix rend 0)))
    (assert-true (num= 1.0 (linear-algebra:mref matrix rend cend)))
    (assert-true (num= 1.0 (linear-algebra:mref matrix rowi rowi)))
    (assert-true (num= 1.0 (linear-algebra:mref matrix coli coli)))
    (assert-true (num= 0.0 (linear-algebra:mref matrix rowi coli)))))

;;; Set identity matrix elements
(deftest identity-matrix-setf-mref (identity)
  (assert-condition error (setf (linear-algebra:mref (linear-algebra:make-matrix 10 10
										 :matrix-type 'linear-algebra:identity-matrix)
						     (random 10) (random 10))
				1.0)))

;;; Copy the identity matrix
(deftest copy-identity-matrix (identity)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:identity-matrix)))
    (assert-true (linear-algebra:identity-matrix-p (linear-algebra:copy-matrix matrix)))
    (assert-false (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (linear-algebra:copy-matrix matrix))))))

;;; Test the submatrix of a identity matrix
(deftest identity-submatrix (identity)
  (assert-condition error (linear-algebra:submatrix (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix) 5 5)))

(deftest setf-identity-submatrix (identity)
  (assert-condition error (setf (linear-algebra:submatrix (linear-algebra:make-matrix 10 10 :matrix-type 'linear-algebra:identity-matrix) 5 5)
    (unit-matrix 5 5))))

(deftest identity-matrix-replace (identity)
  ;; Replace the entire matrix
  (assert-condition error (linear-algebra:replace-matrix (linear-algebra:make-matrix 5 5 :matrix-type 'linear-algebra:identity-matrix)
							 (unit-matrix 5 5))))

;;; Validate a range for an identity matrix.
(deftest identity-matrix-validated-range (identity)
  (test-matrix-validated-range
   'linear-algebra:identity-matrix 10 10))
