;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-kernel)

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
