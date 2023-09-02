;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2023 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(in-package #:linalg)

;;; Common Lisp implementation of generic linear algebra and functions

;;; Mathematical methods

;; TODO Consider what dimension we wish to increase when promoting a
;; vector to an array here.  The code assumes we're adding rows,
;; however that might not be correct in all cases.  Probably a better
;; way is to determine which array dimension matches the vector length
;; and then expand accordingly.

;; See: https://numpy.org/doc/stable/reference/generated/numpy.matmul.html
(defgeneric multiply (a b)
  (:method ((a array) (b array))
    (each-index (i j)
      (sum-index k
	(* (aref A i k) (aref B k j)))))
  (:method ((a array) (b vector))
    (let+ (((&dims nrow &ign) a)
	   (new-b (recycle b :outer nrow)))
      (sub (multiply a new-b) 1)))	;remove the extra dimension with sub
  (:method ((a vector) (b array))
    (let+ (((&dims nrow &ign) b)
	   (new-a (recycle a :outer nrow)))
      (sub (multiply new-a b) 1)))

  ;; (multiply vector t) is the dot product
  (:method ((a vector) (b vector))	;below here from LLA
    (assert (= (length a) (length b)))
    (loop for a-elt :across a
          for b-elt :across b
          summing (* a-elt (conjugate b-elt))))
  (:method ((a vector) (b (eql t)))
    (declare (inline absolute-square))
    (reduce #'+ a :key #'absolute-square))
  (:method ((a (eql t)) (b vector))
    (declare (inline absolute-square))
    (reduce #'+ b :key #'absolute-square))
  (:documentation "Matrix product of two arrays.  If A or B is a vector, it is reshaped as a matrix before multiplication.  If both A and B are vectors, returns the scalar inner product."))

(defun add (&rest arrays)
  "Add arguments element-wise"
  (apply #'e+ arrays))

(defun subtract (&rest arrays)
  "Subtract arguments element-wise"
  (apply #'e- arrays))



;;; Linear algebra methods

;; See: https://numpy.org/doc/stable/reference/generated/numpy.outer.html
;; This was taken from LLA
(defgeneric outer (a b)
  (:documentation "Return the outer product column(a) row(b)^H.  If either A or B is T, they are taken to be conjugate transposes of the other argument.")
  (:method ((a vector) (b (eql t)))
    (multiply (aops:reshape-col a) t))
  (:method ((a (eql t)) (b vector))
    (multiply (aops:reshape-col b) t))
  (:method ((a vector) (b vector))
    (let* ((a0 (length a))
           (b0 (length b))
           ;; !!! currently we are not using the narrowest element type
           (result (make-array (list a0 b0)))
           (index 0))
      (dotimes (a-index a0)
        (let ((a-elt (aref a a-index)))
          (dotimes (b-index b0)
            (setf (row-major-aref result index)
                  (* a-elt (conjugate (aref b b-index))))
            (incf index))))
      result)))

;; (defgeneric transpose (array)
;;   (:documentation "Return an array with axes transposed"))

(defgeneric scale (array scalar)
  (:method (array scale)
    (e* array scale))
  (:documentation "Scale each element by the scalar."))

(defgeneric invert (matrix)
  (:documentation "Return the multiplicative inverse of MATRIX"))

(defgeneric transpose (array)
  (:method (array)
    (nu:transpose array))
  (:documentation "Transpose array"))

















