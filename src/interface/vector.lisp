;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra)

;;; Vector interface operations

(defun make-vector
       (size &key
        (vector-type 'column-vector)
        (element-type 'number)
        initial-element initial-contents)
  "Create the data structure to represent a vector."
  (make-instance
   vector-type :size size
   :element-type element-type
   :initial-element initial-element
   :initial-contents initial-contents))

(defgeneric vector-in-bounds-p (vector index)
  (:documentation "Return true if index does not exceed the dimensions of vector."))

(defgeneric vector-element-type (vector)
  (:documentation "Return the element type of vector."))

(defgeneric vector-length (vector)
  (:documentation "Return the length of the vector."))

(defgeneric vref (vector index)
  (:documentation "Return the element of vector at index."))

(defgeneric (setf vref) (data vector index)
  (:documentation "Set the element of vector at index to data."))

(defgeneric copy-vector (vector)
  (:documentation "Return a copy of the vector."))

(defgeneric subvector (vector start &optional end)
  (:documentation "Return a new vector that is a subvector of the vector."))

(defgeneric (setf subvector) (subvector vector start &optional end)
  (:documentation "Set the subvector of the vector."))

(defgeneric replace-vector
    (vector1 vector2 &key start1 end1 start2 end2)
  (:documentation
   "Destructively replace the elements of vector1 with vector2."))

;;; Vector iteration operations

(defgeneric map-vector (result-type function first-vector &rest more-vectors)
  (:documentation "Calls function on successive sets of vector objects."))

(defgeneric map-into-vector (result-vector function &rest vectors)
  (:documentation "Destructively modifies the result vector with the result of applying the function to each element of the vectors."))

(defmacro dovector ((element vector &optional result) &body body)
  "Iterate over vector returning result."
  (let ((pos (gensym "POS-"))
        (end (gensym "END-")))
    `(let ((,end (vector-length ,vector))
           (,element nil))
      (dotimes (,pos ,end ,result)
        (setf ,element (vref ,vector ,pos))
        ,@body))))

;;; Vector transformations

(defgeneric apply-rotation (vector1 vector2 cc ss)
  (:documentation "Return the plane rotations of vector1 and vector2 by cc and ss."))

(defgeneric napply-rotation (vector1 vector2 cc ss)
  (:documentation "Return the plane rotations of vector1 and vector2 by cc and ss."))
