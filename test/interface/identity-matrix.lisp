;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(define-test make-identity-matrix
  (:tag :matrix :identity-matrix :make-matrix)
  ;; A default identity matrix
  (let ((matrix
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:identity-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:identity-matrix))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-rational-equal
     1 (aref (linear-algebra::contents matrix) 1)))
  ;; Specify the identity matrix element type
  (let ((matrix
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:identity-matrix
          :element-type 'single-float)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:identity-matrix))
    (assert-eq
     (array-element-type
      (linear-algebra::contents matrix))
     (array-element-type
      (make-array '(10 10) :element-type 'single-float)))
    (assert-true (zerop (aref (linear-algebra::contents matrix) 0)))
    (assert-float-equal
     1.0 (aref (linear-algebra::contents matrix) 1)))
  ;; Specify the identity matrix initial element
  (assert-error
   'error
   (linear-algebra:make-matrix
    10 10
    :matrix-type 'linear-algebra:identity-matrix
    :initial-element 1.0))
  ;; Specify the identity matrix contents - Nested list
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3
    :matrix-type 'linear-algebra:identity-matrix
    :initial-contents
    '((1.0 0.0 0.0)
      (0.0 1.0 0.0)
      (0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - Nested vector
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3
    :matrix-type 'linear-algebra:identity-matrix
    :initial-contents
    #(#(1.0 0.0 0.0)
      #(0.0 1.0 0.0)
      #(0.0 0.0 1.0))))
  ;; Specify the identity matrix contents - 2D array
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3
    :matrix-type 'linear-algebra:identity-matrix
    :initial-contents
    #2A((1.0 0.0 0.0)
        (0.0 1.0 0.0)
        (0.0 0.0 1.0))))
  ;; Specify initial element and initial contents
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :initial-element 1.0
    :matrix-type
    'linear-algebra:identity-matrix
    :initial-contents
    '((1.0 0.0 0.0)
      (0.0 1.0 0.0)
      (0.0 0.0 1.0)))))

;;; Test the identity matrix predicate
(define-test identity-matrix-predicate
  (:tag :matrix :identity-matrix)
  (assert-true
   (linear-algebra:identity-matrix-p
    (linear-algebra:make-matrix
     10 10 :matrix-type 'linear-algebra:identity-matrix)))
  (assert-false
   (linear-algebra:identity-matrix-p (make-array '(10 10)))))

;;; Test the identity matrix bounds
(define-test identity-matrix-in-bounds-p
  (:tag :matrix :identity-matrix :matrix-in-bounds-p)
  (test-matrix-in-bounds-p 'linear-algebra:identity-matrix))

;;; Test the identity matrix element type
(define-test identity-matrix-element-type
  (:tag :matrix :identity-matrix :matrix-element-type)
  (test-matrix-element-type 'linear-algebra:identity-matrix))

;;; Test the identity matrix dimensions
(define-test identity-matrix-dimensions
  (:tag :matrix :identity-matrix :matrix-dimensions)
  (test-matrix-dimensions 'linear-algebra:identity-matrix 7 7))

;;; Test the identity matrix row dimension
(define-test identity-matrix-row-dimension
  (:tag :matrix :identity-matrix :matrix-row-dimension)
  (test-matrix-row-dimension 'linear-algebra:identity-matrix 7 7))

;;; Test the identity matrix column dimension
(define-test identity-matrix-column-dimension
  (:tag :matrix :identity-matrix :matrix-column-dimension)
  (test-matrix-column-dimension 'linear-algebra:identity-matrix 7 7))

;;; Reference identity matrix elements
(define-test identity-matrix-mref
  (:tag :matrix :identity-matrix :mref)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli
          (do ((i0 (random-interior-index columns)
                   (random-interior-index columns)))
              ((/= i0 rowi) i0)))
         (matrix
          (linear-algebra:make-matrix
           rows columns
           :matrix-type 'linear-algebra:identity-matrix
           :element-type 'single-float)))
    (assert-float-equal
     1.0 (linear-algebra:mref matrix 0 0))
    (assert-float-equal
     0.0 (linear-algebra:mref matrix 0 cend))
    (assert-float-equal
     0.0 (linear-algebra:mref matrix rend 0))
    (assert-float-equal
     1.0 (linear-algebra:mref matrix rend cend))
    (assert-float-equal
     1.0 (linear-algebra:mref matrix rowi rowi))
    (assert-float-equal
     1.0 (linear-algebra:mref matrix coli coli))
    (assert-float-equal
     0.0 (linear-algebra:mref matrix rowi coli))))

;;; Set identity matrix elements
(define-test identity-matrix-setf-mref
  (:tag :matrix :identity-matrix :setf-mref)
  (assert-error
   'error
   (setf
    (linear-algebra:mref
     (linear-algebra:make-matrix
      10 10 :matrix-type
      'linear-algebra:identity-matrix)
     (random 10) (random 10))
    1.0)))

;;; Copy the identity matrix
(define-test copy-identity-matrix
  (:tag :matrix :identity-matrix :copy-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:identity-matrix)))
    (assert-true
     (linear-algebra:identity-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq
      (linear-algebra::contents matrix)
      (linear-algebra::contents
       (linear-algebra:copy-matrix matrix))))))

;;; Test the submatrix of a identity matrix
(define-test identity-submatrix
  (:tag :matrix :identity-matrix :submatrix)
  (assert-error
   'error
   (linear-algebra:submatrix
    (linear-algebra:make-matrix
     10 10 :matrix-type
     'linear-algebra:identity-matrix)
    5 5)))

(define-test setf-identity-submatrix
  (:tag :matrix :identity-matrix :setf-submatrix)
  (assert-error
   'error
   (setf
    (linear-algebra:submatrix
     (linear-algebra:make-matrix
      10 10 :matrix-type
      'linear-algebra:identity-matrix)
     5 5)
    (unit-matrix 5 5))))

(define-test identity-matrix-replace
  (:tag :matrix :identity-matrix :replace-matrix)
  ;; Replace the entire matrix
  (assert-error
   'error
   (linear-algebra:replace-matrix
    (linear-algebra:make-matrix
     5 5 :matrix-type 'linear-algebra:identity-matrix)
    (unit-matrix 5 5))))

;;; Validate a range for an identity matrix.
(define-test identity-matrix-validated-range
  (:tag :matrix :identity-matrix :matrix-validated-range)
  (test-matrix-validated-range
   'linear-algebra:identity-matrix 10 10))
