;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite kernel-utility-test (linear-algebra-kernel-test))


(deftest copy-array (kernel-utility-test)
  (assert-true
      (float-equal
       #(1.1 2.2 3.3 4.4 5.5)
       (linear-algebra-kernel:copy-array
        #(1.1 2.2 3.3 4.4 5.5))))
  (assert-true
      (float-equal
       #2A((1.1 1.2 1.3) (2.1 2.2 2.3) (3.1 3.2 3.3))
       (linear-algebra-kernel:copy-array
        #2A((1.1 1.2 1.3) (2.1 2.2 2.3) (3.1 3.2 3.3))))))

(defclass class-0 () ())
(defclass class-1 (class-0) ())
(defclass class-a (class-1) ())
(defclass class-sub-a (class-a) ())
(defclass class-b (class-1) ())
(defclass class-sub-b (class-b) ())

(deftest common-class-of (kernel-utility-test)
  (let ((object-a (make-instance 'class-a))
        (object-sub-a (make-instance 'class-sub-a))
        (object-b (make-instance 'class-b))
        (object-sub-b (make-instance 'class-sub-b)))
    (assert-eq
     (find-class 'class-1)
     (linear-algebra-kernel:common-class-of object-a object-b))
    (assert-eq
     (find-class 'class-1)
     (linear-algebra-kernel:common-class-of object-sub-a object-b))
    (assert-eq
     (find-class 'class-1)
     (linear-algebra-kernel:common-class-of object-a object-sub-b))))

(deftest common-array-element-type (kernel-utility-test)
  (let ((array-s (make-array 0 :element-type 'single-float))
        (array-d (make-array 0 :element-type 'double-float)))
    (assert-eq
     'single-float
     (linear-algebra-kernel:common-array-element-type array-s array-s))
    (assert-eq
     'double-float
     (linear-algebra-kernel:common-array-element-type array-s array-d))
    (assert-eq
     'double-float
     (linear-algebra-kernel:common-array-element-type array-d array-s))
    (assert-eq
     'double-float
     (linear-algebra-kernel:common-array-element-type array-d array-d))))

(deftest specific-array-element-type (kernel-utility-test)
  (flet ((genarray (element-type)
           (make-array
            3 :element-type element-type
            :initial-element (coerce 1 element-type))))
    ;; Real float
    (assert-eq
     'single-float
     (linear-algebra-kernel:specific-array-element-type
      (genarray 'single-float)))
    (assert-eq
     'double-float
     (linear-algebra-kernel:specific-array-element-type
      (genarray 'double-float)))
    ;; Complex float
    (assert-equal
     (type-of (complex 1.0 0.0))
     (linear-algebra-kernel:specific-array-element-type
      (genarray '(complex single-float))))
    (assert-equal
     (type-of (complex 1D0 0D0))
     (linear-algebra-kernel:specific-array-element-type
      (genarray '(complex double-float))))))

(deftest complex-equal (kernel-utility-test)
  ;; complex float
  (assert-true
   (linear-algebra-kernel:complex-equal #C(1.0 2.0) #C(1.0 2.0)))
  (assert-true
   (linear-algebra-kernel:complex-equal 1.0 #C(1.0 0.0)))
  (assert-true
   (linear-algebra-kernel:complex-equal #C(1.0 0.0) 1.0))
  (assert-false
   (linear-algebra-kernel:complex-equal #C(1.0 2.0) #C(2.0 1.0)))
  (assert-false
   (linear-algebra-kernel:complex-equal 1.0 #C(0.0 1.0)))
  (assert-false
   (linear-algebra-kernel:complex-equal #C(0.0 1.0) 1.0))
  ;; complex integer
  (assert-true
   (linear-algebra-kernel:complex-equal #C(1 2) #C(1 2)))
  ;; Error
  (assert-fail 'error (linear-algebra-kernel:complex-equal 1.0 1.0))
  (assert-fail 'error (linear-algebra-kernel:complex-equal 1 1)))

(deftest number-equal (kernel-utility-test)
  ;; float
  (assert-true (linear-algebra-kernel:number-equal 2.2 2.2))
  (assert-true (linear-algebra-kernel:number-equal 2 2.0))
  (assert-true (linear-algebra-kernel:number-equal 2.0 2))
  (assert-false (linear-algebra-kernel:number-equal 2 2.2))
  ;; rational
  (assert-true (linear-algebra-kernel:number-equal 1/3 1/3))
  (assert-true (linear-algebra-kernel:number-equal 3 3))
  (assert-false (linear-algebra-kernel:number-equal 1/3 3))
  ;; complex float
  (assert-true
   (linear-algebra-kernel:number-equal #C(1.1 2.2) #C(1.1 2.2)))
  (assert-true
   (linear-algebra-kernel:number-equal #C(1.0 2.0) #C(1 2)))
  (assert-true
   (linear-algebra-kernel:number-equal #C(1 2) #C(1.0 2.0)))
  (assert-false
   (linear-algebra-kernel:number-equal #C(1.1 2.2) #C(2.2 1.1)))
  ;; complex rational
  (assert-true
   (linear-algebra-kernel:number-equal #C(1 2) #C(1 2)))
  (assert-true
   (linear-algebra-kernel:number-equal #C(1/2 1/2) #C(1/2 1/2)))
  (assert-false
   (linear-algebra-kernel:number-equal #C(1 2) #C(1/2 1/2)))
  ;; error
  (assert-fail 'error (linear-algebra-kernel:number-equal 1 t))
  (assert-fail 'error (linear-algebra-kernel:number-equal t 1))
  (assert-fail 'error (linear-algebra-kernel:number-equal t t)))
