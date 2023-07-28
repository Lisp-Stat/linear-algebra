;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite utility (kernel))

(deftest copy-array (utility)
  (assert-num= #(1.1 2.2 3.3 4.4 5.5) (copy-array #(1.1 2.2 3.3 4.4 5.5)))
  (assert-num= #2A((1.1 1.2 1.3) (2.1 2.2 2.3) (3.1 3.2 3.3))
    (copy-array #2A((1.1 1.2 1.3) (2.1 2.2 2.3) (3.1 3.2 3.3)))))

(defclass class-0 () ())
(defclass class-1 (class-0) ())
(defclass class-a (class-1) ())
(defclass class-sub-a (class-a) ())
(defclass class-b (class-1) ())
(defclass class-sub-b (class-b) ())

(deftest common-class-of (utility)
  (let ((object-a (make-instance 'class-a))
        (object-sub-a (make-instance 'class-sub-a))
        (object-b (make-instance 'class-b))
        (object-sub-b (make-instance 'class-sub-b)))
    (assert-eq (find-class 'class-1) (common-class-of object-a object-b))
    (assert-eq (find-class 'class-1) (common-class-of object-sub-a object-b))
    (assert-eq (find-class 'class-1) (common-class-of object-a object-sub-b))))

(deftest common-array-element-type (utility)
  (let ((array-s (make-array 0 :element-type 'single-float))
        (array-d (make-array 0 :element-type 'double-float)))
    (assert-eq 'single-float (common-array-element-type array-s array-s))
    (assert-eq 'double-float (common-array-element-type array-s array-d))
    (assert-eq 'double-float (common-array-element-type array-d array-s))
    (assert-eq 'double-float (common-array-element-type array-d array-d))))

(deftest specific-array-element-type (utility)
  (flet ((genarray (element-type) (make-array 3 :element-type element-type
						:initial-element (coerce 1 element-type))))
    ;; Real float
    (assert-eq 'single-float (specific-array-element-type (genarray 'single-float)))
    (assert-eq 'double-float (specific-array-element-type (genarray 'double-float)))
    ;; Complex float
    (assert-equal (type-of (complex 1.0 0.0)) (specific-array-element-type (genarray '(complex single-float))))
    (assert-equal (type-of (complex 1D0 0D0)) (specific-array-element-type (genarray '(complex double-float))))))
