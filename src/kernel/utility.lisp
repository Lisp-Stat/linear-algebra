;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-kernel)

;;; Initialization

;;; TODO Remove or figure out what to do with epsilon in the functions that use it.
;; (defparameter *epsilon* 1d-5) ;same as *num=-tolerance*

(defun zero-vector (size &optional element-type)
  "Return a vector of zeros."
  (if element-type
      (make-array
       size
       :element-type element-type
       :initial-element (coerce 0 element-type))
      (make-array size :initial-element 0)))

(defun zero-array (rows columns &optional element-type)
  "Return an array of zeros."
  (if element-type
      (make-array
       (list rows columns)
       :element-type element-type
       :initial-element (coerce 0 element-type))
      (make-array (list rows columns) :initial-element 0)))

;;; Copy each element of the array

(defgeneric copy-array (array)
  (:documentation "Return an element-wise copy of the original array."))

(defmethod copy-array ((original vector))
  "Return an element-wise copy of the original vector."
  (let* ((size (length original))
         (new-vector
          (make-array
           size :element-type (array-element-type original))))
    (dotimes (index size new-vector)
      (setf (aref new-vector index) (aref original index)))))

(defmethod copy-array ((original array))
  "Return an element-wise copy of the original array."
  (let ((new-array
         (make-array
          (array-dimensions original)
          :element-type (array-element-type original))))
    (dotimes (row (array-dimension original 0) new-array)
      (dotimes (column (array-dimension original 1))
        (setf
         (aref new-array row column)
         (aref original row column))))))

;;; Class and type utilities

(defun common-class-of (object1 object2)
  "Return the common class of the 2 objects or default-class."
  (labels
      ((common-class (c-p-l-1 c-p-l-2)
         (let ((class1 (pop c-p-l-1))
               (class2 (pop c-p-l-2)))
           (cond
            ((eq class1 class2) class1)
            ((member class1 c-p-l-2) class1)
            ((member class2 c-p-l-1) class2)
            (t (common-class c-p-l-1 c-p-l-2))))))
    ;; First call copies the class precedence list
    (common-class
     (copy-list
      (closer-mop:class-precedence-list (class-of object1)))
     (copy-list
      (closer-mop:class-precedence-list (class-of object2))))))

(defun common-array-element-type (array1 array2)
  "Return the array type common to both arrays."
  (let ((type1 (array-element-type array1))
        (type2 (array-element-type array2)))
    (cond
     ((eq type1 type2) type1)
     ((subtypep type1 type2) type1)
     ((subtypep type2 type1) type2)
     ((and (subtypep type1 'number) (subtypep type2 'number))
      (upgraded-array-element-type
       (type-of (+ (coerce 1 type1) (coerce 1 type2)))))
     (t))))

(defun specific-array-element-type (array &rest subscripts)
  "Return the specific type of the element specified by subscripts."
  (type-of
   (apply
    #'aref array
    (or subscripts
        (make-list (array-rank array) :initial-element 0)))))

;;; Equality predicates

;;; (COMPLEX-EQUAL number1 number2) => true or false
(defun complex-equal (complex1 complex2 &optional (epsilon *num=-tolerance*))
  (num= complex1 complex2 epsilon))

;;; (NUMBER-EQUAL number1 number2) => true or false
(defun number-equal (number1 number2 &optional (epsilon *num=-tolerance*))
  (num= number1 number2 epsilon))
