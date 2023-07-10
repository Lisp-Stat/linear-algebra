;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite permutation-matrix-interface-test (linear-algebra-interface-test))

(deftest make-permutation-matrix (permutation-matrix-interface-test)
  ;; A default permutation matrix
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(0 1 2 3 4)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - Nested list
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)
            (0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - Nested vector
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          #(#(0 0 0 0 1)
            #(1 0 0 0 0)
            #(0 1 0 0 0)
            #(0 0 0 1 0)
            #(0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - 2D array
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          #2A((0 0 0 0 1)
              (1 0 0 0 0)
              (0 1 0 0 0)
              (0 0 0 1 0)
              (0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Erroneous 2D array input data
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 4
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    #3A(((1.1 1.2) (2.1 2.2))
        ((3.1 3.2) (4.1 4.2))
        ((5.1 5.2) (6.1 6.2)))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 4
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    (random-permutation-array 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 3
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    (random-permutation-array 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :element-type 'single-float
    :matrix-type 'linear-algebra:permutation-matrix))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    #2A((0 1 0)
        (1 0 0)
        (1 0 1)))))

;;; Test the permutation matrix predicate
(deftest permutation-matrix-predicate (permutation-matrix-interface-test)
   (assert-true
    (linear-algebra:permutation-matrix-p
     (linear-algebra:make-matrix
      10 10 :matrix-type 'linear-algebra:permutation-matrix)))
   (assert-false
    (linear-algebra:permutation-matrix-p (make-array '(10 10)))))

;;; Test the permutation matrix bounds
(deftest permutation-matrix-in-bounds-p (permutation-matrix-interface-test)
  (test-matrix-in-bounds-p 'linear-algebra:permutation-matrix))

;;; Test the permutation matrix element type
(deftest permutation-matrix-element-type (permutation-matrix-interface-test)
  (assert-eq
   'fixnum
   (linear-algebra:matrix-element-type
    (linear-algebra:make-matrix
     5 5
     :matrix-type 'linear-algebra:permutation-matrix)))
  (dolist (element-type '(single-float double-float))
    (assert-error
     'error
     (linear-algebra:matrix-element-type
      (linear-algebra:make-matrix
       5 5
       :element-type element-type
       :matrix-type
       'linear-algebra:permutation-matrix)))))

;;; Test the permutation matrix dimensions
(deftest permutation-matrix-dimensions (permutation-matrix-interface-test)
  (test-matrix-dimensions 'linear-algebra:permutation-matrix 9 9))

;;; Test the permutation matrix row dimension
(deftest permutation-matrix-row-dimension (permutation-matrix-interface-test)
  (test-matrix-row-dimension 'linear-algebra:permutation-matrix 9 9))

;;; Test the permutation matrix column dimension
(deftest permutation-matrix-column-dimension (permutation-matrix-interface-test)
  (test-matrix-column-dimension
   'linear-algebra:permutation-matrix 9 9))

;;; Reference permutation matrix elements
(deftest permutation-matrix-mref (permutation-matrix-interface-test)
  (let ((pvec #(2 3 4 0 1))
        (matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 1 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)))))
    (do ((i0 0 (1+ i0)))
        ((>= i0 5))
      (do ((i1 0 (1+ i1)))
          ((>= i1 5))
        (if (= i1 (svref pvec i0))
            (assert-rational-equal
             1 (linear-algebra:mref matrix i0 i1))
            (assert-rational-equal
             0 (linear-algebra:mref matrix i0 i1)))))))

;;; Set permutation matrix elements
(deftest permutation-matrix-setf-mref (permutation-matrix-interface-test)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          (random-permutation-array 5))))
    (dotimes (i0 5)
      (dotimes (i1 5)
        (setf (linear-algebra:mref matrix i0 i1) 1)
        (assert-true
         (= 5 (length
               (remove-duplicates
                (linear-algebra::contents matrix))))
         i0 i1 (linear-algebra::contents matrix))))))

;;; Copy the permutation matrix
(deftest copy-permutation-matrix (permutation-matrix-interface-test)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          (random-permutation-array 5))))
    (assert-true
     (linear-algebra:permutation-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-rational-equal
     (linear-algebra::contents matrix)
     (linear-algebra::contents
      (linear-algebra:copy-matrix matrix)))))

(deftest permutation-matrix-transpose (permutation-matrix-interface-test)
  (loop
   for (permutation transpose) in (validated-permutation-transpose)
   do (assert-rational-equal
       transpose
       (linear-algebra::contents
        (linear-algebra:transpose
         (make-instance
          'linear-algebra:permutation-matrix
          :contents
          (coerce permutation '(vector fixnum)))))
       permutation)))

;;; Validate a range for a permutation matrix.

(deftest permutation-matrix-validated-range (permutation-matrix-interface-test)
  (test-matrix-validated-range
   'linear-algebra:permutation-matrix 10 10))

;;; Validated transposition

(defun validated-permutation-transpose ()
  "Return a list of transposes of permutation vectors."
  (list
   (list (vector 4 3 2 1 0) (vector 4 3 2 1 0))
   (list (vector 4 3 2 0 1) (vector 3 4 2 1 0))
   (list (vector 4 3 1 0 2) (vector 3 2 4 1 0))
   (list (vector 4 3 1 2 0) (vector 4 2 3 1 0))
   (list (vector 4 3 0 2 1) (vector 2 4 3 1 0))
   (list (vector 4 3 0 1 2) (vector 2 3 4 1 0))
   (list (vector 4 2 1 0 3) (vector 3 2 1 4 0))
   (list (vector 4 2 1 3 0) (vector 4 2 1 3 0))
   (list (vector 4 2 0 3 1) (vector 2 4 1 3 0))
   (list (vector 4 2 0 1 3) (vector 2 3 1 4 0))
   (list (vector 4 2 3 1 0) (vector 4 3 1 2 0))
   (list (vector 4 2 3 0 1) (vector 3 4 1 2 0))
   (list (vector 4 1 0 3 2) (vector 2 1 4 3 0))
   (list (vector 4 1 0 2 3) (vector 2 1 3 4 0))
   (list (vector 4 1 3 2 0) (vector 4 1 3 2 0))
   (list (vector 4 1 3 0 2) (vector 3 1 4 2 0))
   (list (vector 4 1 2 0 3) (vector 3 1 2 4 0))
   (list (vector 4 1 2 3 0) (vector 4 1 2 3 0))
   (list (vector 4 0 3 2 1) (vector 1 4 3 2 0))
   (list (vector 4 0 3 1 2) (vector 1 3 4 2 0))
   (list (vector 4 0 2 1 3) (vector 1 3 2 4 0))
   (list (vector 4 0 2 3 1) (vector 1 4 2 3 0))
   (list (vector 4 0 1 3 2) (vector 1 2 4 3 0))
   (list (vector 4 0 1 2 3) (vector 1 2 3 4 0))
   (list (vector 3 2 1 0 4) (vector 3 2 1 0 4))
   (list (vector 3 2 1 4 0) (vector 4 2 1 0 3))
   (list (vector 3 2 0 4 1) (vector 2 4 1 0 3))
   (list (vector 3 2 0 1 4) (vector 2 3 1 0 4))
   (list (vector 3 2 4 1 0) (vector 4 3 1 0 2))
   (list (vector 3 2 4 0 1) (vector 3 4 1 0 2))
   (list (vector 3 1 0 4 2) (vector 2 1 4 0 3))
   (list (vector 3 1 0 2 4) (vector 2 1 3 0 4))
   (list (vector 3 1 4 2 0) (vector 4 1 3 0 2))
   (list (vector 3 1 4 0 2) (vector 3 1 4 0 2))
   (list (vector 3 1 2 0 4) (vector 3 1 2 0 4))
   (list (vector 3 1 2 4 0) (vector 4 1 2 0 3))
   (list (vector 3 0 4 2 1) (vector 1 4 3 0 2))
   (list (vector 3 0 4 1 2) (vector 1 3 4 0 2))
   (list (vector 3 0 2 1 4) (vector 1 3 2 0 4))
   (list (vector 3 0 2 4 1) (vector 1 4 2 0 3))
   (list (vector 3 0 1 4 2) (vector 1 2 4 0 3))
   (list (vector 3 0 1 2 4) (vector 1 2 3 0 4))
   (list (vector 3 4 2 1 0) (vector 4 3 2 0 1))
   (list (vector 3 4 2 0 1) (vector 3 4 2 0 1))
   (list (vector 3 4 1 0 2) (vector 3 2 4 0 1))
   (list (vector 3 4 1 2 0) (vector 4 2 3 0 1))
   (list (vector 3 4 0 2 1) (vector 2 4 3 0 1))
   (list (vector 3 4 0 1 2) (vector 2 3 4 0 1))
   (list (vector 2 1 0 4 3) (vector 2 1 0 4 3))
   (list (vector 2 1 0 3 4) (vector 2 1 0 3 4))
   (list (vector 2 1 4 3 0) (vector 4 1 0 3 2))
   (list (vector 2 1 4 0 3) (vector 3 1 0 4 2))
   (list (vector 2 1 3 0 4) (vector 3 1 0 2 4))
   (list (vector 2 1 3 4 0) (vector 4 1 0 2 3))
   (list (vector 2 0 4 3 1) (vector 1 4 0 3 2))
   (list (vector 2 0 4 1 3) (vector 1 3 0 4 2))
   (list (vector 2 0 3 1 4) (vector 1 3 0 2 4))
   (list (vector 2 0 3 4 1) (vector 1 4 0 2 3))
   (list (vector 2 0 1 4 3) (vector 1 2 0 4 3))
   (list (vector 2 0 1 3 4) (vector 1 2 0 3 4))
   (list (vector 2 4 3 1 0) (vector 4 3 0 2 1))
   (list (vector 2 4 3 0 1) (vector 3 4 0 2 1))
   (list (vector 2 4 1 0 3) (vector 3 2 0 4 1))
   (list (vector 2 4 1 3 0) (vector 4 2 0 3 1))
   (list (vector 2 4 0 3 1) (vector 2 4 0 3 1))
   (list (vector 2 4 0 1 3) (vector 2 3 0 4 1))
   (list (vector 2 3 1 0 4) (vector 3 2 0 1 4))
   (list (vector 2 3 1 4 0) (vector 4 2 0 1 3))
   (list (vector 2 3 0 4 1) (vector 2 4 0 1 3))
   (list (vector 2 3 0 1 4) (vector 2 3 0 1 4))
   (list (vector 2 3 4 1 0) (vector 4 3 0 1 2))
   (list (vector 2 3 4 0 1) (vector 3 4 0 1 2))
   (list (vector 1 0 4 3 2) (vector 1 0 4 3 2))
   (list (vector 1 0 4 2 3) (vector 1 0 3 4 2))
   (list (vector 1 0 3 2 4) (vector 1 0 3 2 4))
   (list (vector 1 0 3 4 2) (vector 1 0 4 2 3))
   (list (vector 1 0 2 4 3) (vector 1 0 2 4 3))
   (list (vector 1 0 2 3 4) (vector 1 0 2 3 4))
   (list (vector 1 4 3 2 0) (vector 4 0 3 2 1))
   (list (vector 1 4 3 0 2) (vector 3 0 4 2 1))
   (list (vector 1 4 2 0 3) (vector 3 0 2 4 1))
   (list (vector 1 4 2 3 0) (vector 4 0 2 3 1))
   (list (vector 1 4 0 3 2) (vector 2 0 4 3 1))
   (list (vector 1 4 0 2 3) (vector 2 0 3 4 1))
   (list (vector 1 3 2 0 4) (vector 3 0 2 1 4))
   (list (vector 1 3 2 4 0) (vector 4 0 2 1 3))
   (list (vector 1 3 0 4 2) (vector 2 0 4 1 3))
   (list (vector 1 3 0 2 4) (vector 2 0 3 1 4))
   (list (vector 1 3 4 2 0) (vector 4 0 3 1 2))
   (list (vector 1 3 4 0 2) (vector 3 0 4 1 2))
   (list (vector 1 2 0 4 3) (vector 2 0 1 4 3))
   (list (vector 1 2 0 3 4) (vector 2 0 1 3 4))
   (list (vector 1 2 4 3 0) (vector 4 0 1 3 2))
   (list (vector 1 2 4 0 3) (vector 3 0 1 4 2))
   (list (vector 1 2 3 0 4) (vector 3 0 1 2 4))
   (list (vector 1 2 3 4 0) (vector 4 0 1 2 3))
   (list (vector 0 4 3 2 1) (vector 0 4 3 2 1))
   (list (vector 0 4 3 1 2) (vector 0 3 4 2 1))
   (list (vector 0 4 2 1 3) (vector 0 3 2 4 1))
   (list (vector 0 4 2 3 1) (vector 0 4 2 3 1))
   (list (vector 0 4 1 3 2) (vector 0 2 4 3 1))
   (list (vector 0 4 1 2 3) (vector 0 2 3 4 1))
   (list (vector 0 3 2 1 4) (vector 0 3 2 1 4))
   (list (vector 0 3 2 4 1) (vector 0 4 2 1 3))
   (list (vector 0 3 1 4 2) (vector 0 2 4 1 3))
   (list (vector 0 3 1 2 4) (vector 0 2 3 1 4))
   (list (vector 0 3 4 2 1) (vector 0 4 3 1 2))
   (list (vector 0 3 4 1 2) (vector 0 3 4 1 2))
   (list (vector 0 2 1 4 3) (vector 0 2 1 4 3))
   (list (vector 0 2 1 3 4) (vector 0 2 1 3 4))
   (list (vector 0 2 4 3 1) (vector 0 4 1 3 2))
   (list (vector 0 2 4 1 3) (vector 0 3 1 4 2))
   (list (vector 0 2 3 1 4) (vector 0 3 1 2 4))
   (list (vector 0 2 3 4 1) (vector 0 4 1 2 3))
   (list (vector 0 1 4 3 2) (vector 0 1 4 3 2))
   (list (vector 0 1 4 2 3) (vector 0 1 3 4 2))
   (list (vector 0 1 3 2 4) (vector 0 1 3 2 4))
   (list (vector 0 1 3 4 2) (vector 0 1 4 2 3))
   (list (vector 0 1 2 4 3) (vector 0 1 2 4 3))
   (list (vector 0 1 2 3 4) (vector 0 1 2 3 4))))
