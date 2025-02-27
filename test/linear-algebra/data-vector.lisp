;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite data-vector (core))

(deftest make-data-vector (data-vector)
  ;; A default vector.
  (assert-true (typep (make-vector 10) 'data-vector))
  (assert-true (typep (make-vector 10) 'column-vector))
  (assert-num= (vector 0 0 0 0 0 0 0 0 0 0) (make-vector 10))

  ;; Specify the vector type
  (assert-true (typep (make-vector 10 :vector-type 'column-vector)
		      'column-vector))
  (assert-true (typep (make-vector 10 :vector-type 'row-vector)
		      'row-vector))

  ;; Specify the element type.
  (assert-true (subtypep (array-element-type (linear-algebra::contents (make-vector 10 :element-type 'single-float)))
			 (upgraded-array-element-type 'single-float)))
  (assert-true (subtypep (array-element-type (linear-algebra::contents (make-vector 10 :element-type '(complex single-float))))
			 (upgraded-array-element-type '(complex single-float))))

  ;; Specify the initial element.
  (assert-num= (vector 1.0 1.0 1.0 1.0 1.0)
      (make-vector 5 :initial-element 1.0))
  (assert-num= (vector #C(1.0 2.0) #C(1.0 2.0) #C(1.0 2.0) #C(1.0 2.0))
      (make-vector 4 :initial-element #C(1.0 2.0)))

  ;; Specify the initial contents.
  (let ((data (make-random-list 100 1.0)))
    (assert-num= (make-array 100 :initial-contents data)
	(make-vector 100 :initial-contents data))))


(deftest column-vector (data-vector)
  ;; Column vector
  (assert-true (typep (column-vector 1.0 2.0 3.0 4.0 5.0) 'column-vector))

  ;; Contents
  (assert-num=
      (vector 1.0 2.0 3.0 4.0 5.0)
      (column-vector 1.0 2.0 3.0 4.0 5.0)))

(deftest row-vector (data-vector)
  ;; row vector
  (assert-true (typep (row-vector 1.0 2.0 3.0 4.0 5.0) 'row-vector))
  ;; Contents
  (assert-num=
      (vector 1.0 2.0 3.0 4.0 5.0)
      (row-vector 1.0 2.0 3.0 4.0 5.0)))

(deftest column-vector-p (data-vector)
  (assert-true (column-vector-p (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-false (column-vector-p (row-vector 1 2 3 4 5))))

(deftest row-vector-p (data-vector)
  (assert-true (row-vector-p (row-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-false (row-vector-p (column-vector 1 2 3 4 5))))

(deftest data-vector-in-bounds-p (data-vector)
  ;; Column vector bounds
  (let ((cvec (column-vector 1 2 3 4 5)))
    (assert-true (vector-in-bounds-p cvec 0))
    (assert-true (vector-in-bounds-p cvec 4))
    (assert-true (vector-in-bounds-p cvec (random 5)))
    (assert-false (vector-in-bounds-p cvec 10)))
  ;; Row vector bounds
  (let ((rvec (column-vector 1 2 3 4 5)))
    (assert-true (vector-in-bounds-p rvec 0))
    (assert-true (vector-in-bounds-p rvec 4))
    (assert-true (vector-in-bounds-p rvec (random 5)))
    (assert-false (vector-in-bounds-p rvec 10))))

(deftest data-vector-element-type (data-vector)
  ;; Column vector
  (assert-true (subtypep (vector-element-type (make-vector 5 :element-type 'single-float
							     :vector-type 'column-vector))
			 'single-float))
  ;; Row vector
  (assert-true (subtypep (vector-element-type (make-vector 5 :element-type 'single-float
							     :vector-type 'row-vector))
			 'single-float)))

(deftest data-vector-length (data-vector)
  (assert-eq 5 (vector-length (column-vector 1 2 3 4 5)))
  (assert-eq 5 (vector-length (row-vector 1 2 3 4 5))))

(deftest data-vref (data-vector)
  ;; Reference the element of a vector.
  (let* ((size 1000)
         (index (random-interior-index size))
         (end   (1- size))
         (list   (make-random-list size 100.0))
         (vector (make-vector size :initial-contents list)))
    (assert-num= (nth 0 list) (vref vector 0))
    (assert-num= (nth index list) (vref vector index))
    (assert-num=
	(nth end list) (vref vector end)))
  ;; Setting an element of a vector.
  (let* ((size 1000)
         (val0  (random 100.0))
         (vali  (random 100.0))
         (valn  (random 100.0))
         (index (random-interior-index size))
         (end   (1- size))
         (list   (make-random-list size 100.0))
         (vector (make-vector size :initial-contents list)))
    (setf (vref vector 0)     val0)
    (setf (vref vector index) vali)
    (setf (vref vector end)   valn)
    (assert-num=
	val0 (aref (linear-algebra::contents vector) 0))
    (assert-num=
	vali (aref (linear-algebra::contents vector) index))
    (assert-num=
	valn (aref (linear-algebra::contents vector) end)))
  ;; VREF only works for vectors.
  (let ((list (make-list 1000 :initial-element 10.0))
        (index (random 1000)))
    (assert-condition error (vref list index))
    (assert-condition error (setf (vref list index) 42.0))))

(deftest copy-data-vector (data-vector)
  ;; Column vector
  (let* ((vec (column-vector 1 2 3 4 5))
         (new (copy-vector vec)))
    (assert-true (subtypep (type-of new) (type-of vec)))
    (assert-false (eq vec new))
    (assert-false (eq (linear-algebra::contents vec) (linear-algebra::contents new)))
    (assert-num= vec new))
  ;; Row vector
  (let* ((vec (row-vector 1 2 3 4 5))
         (new (copy-vector vec)))
    (assert-true (subtypep (type-of new) (type-of vec)))
    (assert-false (eq vec new))
    (assert-false (eq (linear-algebra::contents vec) (linear-algebra::contents new)))
    (assert-num= vec new)))

(deftest data-subvector (data-vector)
  ;; Get subvectors
  (let* ((data (list 0 1 2 3 4 5 6 7 8 9))
         (vec (column-vector 0 1 2 3 4 5 6 7 8 9))
         (index (random-interior-index 10))
         (subvec0 (subvector vec index))
         (subvec1 (subvector vec 0 index)))
    (assert-true (subtypep (type-of vec) (type-of subvec0)))
    (assert-true (subtypep (type-of vec) (type-of subvec1)))
    (assert-num=
	(subseq data index) subvec0)
    (assert-num=
	(subseq data 0 index) subvec1))
  ;; Set subvectors
  (let ((vec (make-vector 10))
        (data
          (make-vector
           5 :initial-element 1)))
    (assert-num=
	#(1 1 1 1 1)
	(setf (subvector vec 7) data))
    (assert-num=
	'(0 0 0 0 0 0 0 1 1 1) vec)))

(deftest replace-data-vector (data-vector)
  (flet ((zero-vector () (make-vector 10)))
    (let ((data (make-vector 5 :initial-element 1)))
      (assert-num=
	  #(1 1 1 1 1 0 0 0 0 0)
          (replace-vector (zero-vector) data))
      (assert-num=
	  #(0 0 1 1 1 1 1 0 0 0)
	  (replace-vector
           (zero-vector) data :start1 2))
      (assert-num=
	  #(0 0 0 0 0 0 0 0 1 1)
	  (replace-vector
           (zero-vector) data :start1 8))
      (assert-num=
	  #(1 1 1 0 0 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :end1 3))
      (assert-num=
	  #(0 0 1 1 0 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :start1 2 :end1 4))
      (assert-num=
	  #(1 1 1 0 0 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :start2 2))
      (assert-num=
	  #(1 1 0 0 0 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :end2 2))
      (assert-num=
	  #(1 1 1 0 0 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :start2 1 :end2 4))
      (assert-num=
	  #(0 0 1 1 1 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :start1 2 :start2 2))
      (assert-num=
	  #(0 0 1 1 1 0 0 0 0 0)
	  (replace-vector
           (zero-vector) data :start1 2 :end2 3)))))

(deftest %map-data-vector (data-vector)
  (let ((col-data (column-vector 1 1 1 1 1))
        (row-data (row-vector 1 1 1 1 1)))
    (assert-true (typep (linear-algebra::%map-data-vector 'column-vector #'identity col-data) 'column-vector))
    (assert-true (typep (linear-algebra::%map-data-vector 'row-vector    #'identity col-data) 'row-vector))
    (assert-true (typep (linear-algebra::%map-data-vector 'column-vector #'identity row-data) 'column-vector))
    (assert-true (typep (linear-algebra::%map-data-vector 'row-vector    #'identity row-data) 'row-vector))
    (assert-num=
	'(3 3 3 3 3)
	(linear-algebra::%map-data-vector
	 'column-vector
	 #'+ col-data col-data col-data))
    (assert-num=
	'(3 3 3 3 3)
	(linear-algebra::%map-data-vector
	 'row-vector
	 #'+ row-data row-data row-data))
    (assert-num=
	'(4 4 4 4 4)
	(linear-algebra::%map-data-vector
	 'column-vector
	 #'+ col-data row-data col-data row-data))))

(deftest map-data-vector (data-vector)
  (let ((col-data (column-vector 1 1 1 1 1))
        (row-data (row-vector 1 1 1 1 1)))
    (assert-condition error (map-vector 'vector #'identity col-data))
    (assert-condition error (map-vector 'column-vector #'identity col-data #(2 2 2 2 2)))
    (assert-num=
	'(3 3 3 3 3)
	(map-vector
	 'column-vector
	 #'+ col-data row-data col-data))))

(deftest %map-into-data-vector (data-vector)
  (let ((col-data (column-vector 1 1 1 1 1))
        (row-data (row-vector 1 1 1 1 1)))
    (assert-true (typep (linear-algebra::%map-into-data-vector col-data #'identity col-data) 'column-vector))
    (assert-true (typep (linear-algebra::%map-into-data-vector row-data #'identity col-data) 'row-vector))
    (assert-true (typep (linear-algebra::%map-into-data-vector col-data #'identity row-data) 'column-vector))
    (assert-true (typep (linear-algebra::%map-into-data-vector row-data #'identity row-data) 'row-vector))
    (assert-num=
	'(3 3 3 3 3)
	(linear-algebra::%map-into-data-vector
	 (column-vector 0 0 0 0 0)
	 #'+ col-data col-data col-data))
    (assert-num=
	'(3 3 3 3 3)
	(linear-algebra::%map-into-data-vector
	 (row-vector 0 0 0 0 0)
	 #'+ row-data row-data row-data))
    (assert-num=
	'(4 4 4 4 4)
	(linear-algebra::%map-into-data-vector
	 col-data #'+ col-data row-data col-data row-data))))

(deftest map-into-data-vector (data-vector)
  (let ((col-data (column-vector 1 1 1 1 1))
        (row-data (row-vector 1 1 1 1 1)))
    (assert-condition error (map-into-vector #(0 0 0 0 0) #'identity col-data))
    (assert-condition error (map-vector (column-vector 0 0 0 0 0) #'identity col-data))
    (assert-num= '(3 3 3 3 3)
	(map-into-vector
	 col-data #'+ col-data row-data col-data))))

;;; Apply rotation

(deftest apply-rotation-data-vector (data-vector)
  ;; Float
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents '(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float :initial-contents '(4.0 9.0 16.0 25.0 36.0))))
    ;; Position 0
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation
         (vref vec1 0) (vref vec2 0))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        (assert-num= rr (vref rvec1 0))
        (assert-num=
            #(5.0 10.200001 17.0 26.6 36.600002) rvec1)
        (assert-num=
            #(0.0 1.4000001 4.0000005 6.200001 11.2) rvec2)))
    ;; Position 1
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 1) (vref vec2 1))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        (assert-num= rr (vref rvec1 1))
        (assert-num=
            #(4.953558 10.2956295 17.386017 27.196003 37.78302) rvec1)
        (assert-num=
            #(-0.67990017 0.0 1.651186 2.525344 6.1191006) rvec2)))
    ;; Position 2
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 2) (vref vec2 2))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        (assert-num= rr (vref rvec1 2))
        (assert-num=
            #(4.867086 10.249511 17.46425 27.312943 38.19231) rvec1)
        (assert-num= 
            #(-1.1451968 -0.97341704 0.0 -0.05725956 2.519433) rvec2)))
    ;; Position 3
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 3) (vref vec2 3))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        (assert-num= rr (vref rvec1 3))
        (assert-num=
            #(4.8694763 10.251528 17.46421 27.313 38.186943) rvec1)
        (assert-num=
            #(-1.1349905 -0.95192766 0.036612988 9.536743e-7 2.599495)
            rvec2)))
    ;; Position 4
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 4) (vref vec2 4))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        ;; FIXME : The error should not be this large.
        (assert-num= rr (vref rvec1 4))
        (assert-num=
            #(4.781149 10.163207 17.426374 27.249937 38.27532) rvec1)
        (assert-num=
            #(-1.4630839 -1.6459692 -1.1495662 -1.8549814 0.0) rvec2))))
  ;; Complex
  (let ((vec1 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(3.0 5.0) #C(7.0 11.0))))
        (vec2 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(4.0 9.0) #C(16.0 25.0)))))
    ;; Position 0
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation  (vref vec1 0) (vref vec2 0))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss)
        (assert-num= rr (vref rvec1 0))
        (assert-num=
            #(#C(5.888673 9.814455) #C(19.85367 25.27784)) rvec1)
        (assert-num=
            #(#C(-4.7683716e-7 -4.7683716e-7) #C(3.326425 2.6071978))
            rvec2)))
    ;; Position 1
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 1) (vref vec2 1))
      (multiple-value-bind (rvec1 rvec2) (apply-rotation vec1 vec2 cc ss) ;FIXME rvec2 not used
        ;; FIXME : The error should not be this large.
	(declare (ignore rvec2))
        (assert-num= rr (vref rvec1 1))
        (assert-num=
            #(#C(4.8474817 10.2603855) #C(17.405037 27.350775)) rvec1)))))




(deftest napply-rotation-data-vector (data-vector)
  ;; Float, position 0
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents #(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float :initial-contents #(4.0 9.0 16.0 25.0 36.0))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 0) (vref vec2 0))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 0))
        (assert-num= #(5.0 10.200001 17.0 26.6 36.600002) vec1)
        (assert-num= #(0.0 1.4000001 4.0000005 6.200001 11.2) vec2))))
  ;; Float, position 1
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents #(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float  :initial-contents #(4.0 9.0 16.0 25.0 36.0))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 1) (vref vec2 1))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 1))
        (assert-num= #(4.953558 10.2956295 17.386017 27.196003 37.78302) vec1)
        (assert-num= #(-0.67990017 0.0 1.651186 2.525344 6.1191006) vec2))))
  ;; Float, position 2
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents #(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float :initial-contents #(4.0 9.0 16.0 25.0 36.0))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 2) (vref vec2 2))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 2))
        (assert-num= #(4.867086 10.249511 17.46425 27.312943 38.19231) vec1)
        (assert-num= #(-1.1451968 -0.97341704 0.0 -0.05725956 2.519433) vec2))))
  ;; Float, position 3
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents #(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float :initial-contents #(4.0 9.0 16.0 25.0 36.0))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 3) (vref vec2 3))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 3))
        (assert-num= #(4.8694763 10.251528 17.46421 27.313 38.186943) vec1)
        (assert-num= #(-1.1349905 -0.95192766 0.036612988 9.536743e-7 2.599495) vec2))))
  ;; Float, position 4
  (let ((vec1 (make-vector 5 :element-type 'single-float :initial-contents #(3.0 5.0 7.0 11.0 13.0)))
        (vec2 (make-vector 5 :element-type 'single-float :initial-contents #(4.0 9.0 16.0 25.0 36.0))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 4) (vref vec2 4))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 4))
        (assert-num= #(4.781149 10.163207 17.426374 27.249937 38.27532) vec1)
        (assert-num= #(-1.4630839 -1.6459692 -1.1495662 -1.8549814 0.0) vec2))))
  ;; Complex, position 0
  (let ((vec1 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(3.0 5.0) #C(7.0 11.0))))
        (vec2 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(4.0 9.0) #C(16.0 25.0)))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 0) (vref vec2 0))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        (assert-num= rr (vref vec1 0))
        (assert-num= #(#C(5.888673 9.814455) #C(19.85367 25.27784)) vec1)
        (assert-num= #(#C(-4.7683716e-7 -4.7683716e-7) #C(3.326425 2.6071978)) vec2))))
  ;; Complex, position 1
  (let ((vec1 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(3.0 5.0) #C(7.0 11.0))))
        (vec2 (make-vector 2 :element-type '(complex single-float) :initial-contents #(#C(4.0 9.0) #C(16.0 25.0)))))
    (multiple-value-bind (cc ss rr)
        (linear-algebra-kernel:givens-rotation (vref vec1 1) (vref vec2 1))
      (multiple-value-bind (rvec1 rvec2) (napply-rotation vec1 vec2 cc ss)
        (assert-eq vec1 rvec1)
        (assert-eq vec2 rvec2)
        ;; FIXME : Why is this error so large?
        (assert-num= rr (vref vec1 1))
        (assert-num=
            #(#C(4.8474817 10.2603855) #C(17.405037 27.350775)) vec1)
        (assert-num=
            #(#C(-1.1497688 -0.9510431) #C(0.0 0.0)) vec2)))))

;;; Fundamental operations

;;; Data vector transpose

(deftest transpose-data-vector (data-vector)
  (let ((col-data (column-vector 1.0 2.0 3.0 4.0 5.0))
        (row-data (row-vector 1.0 2.0 3.0 4.0 5.0)))
    (assert-true (typep (transpose col-data) 'row-vector))
    (assert-true (typep (transpose row-data) 'column-vector))
    (assert-num= col-data (transpose col-data))
    (assert-num= row-data (transpose row-data))))

(deftest ntranspose-data-vector (data-vector)
  (let ((col-data (column-vector 1.0 2.0 3.0 4.0 5.0))
        (row-data (row-vector 1.0 2.0 3.0 4.0 5.0)))
    (assert-true (typep (ntranspose col-data) 'row-vector))
    (assert-true (typep col-data 'row-vector))
    (assert-num= #(1.0 2.0 3.0 4.0 5.0) (transpose col-data))
    (assert-true (typep (ntranspose row-data) 'column-vector))
    (assert-true (typep row-data 'column-vector))
    (assert-num= #(1.0 2.0 3.0 4.0 5.0) (transpose row-data))))

;;; Data vector permutation

(deftest permute-data-vector (data-vector)
  (let ((rvec (row-vector 1.1 2.2 3.3 4.4 5.5))
        (cvec (column-vector 1.1 2.2 3.3 4.4 5.5))
        (rerr (row-vector 1.1 2.2 3.3 4.4 5.5 6.6))
        (cerr (column-vector 1.1 2.2 3.3 4.4 5.5 6.6))
        (pmat (make-matrix 5 5 :matrix-type 'permutation-matrix
			       :initial-contents '((0 0 1 0 0)
						   (0 0 0 0 1)
						   (1 0 0 0 0)
						   (0 1 0 0 0)
						   (0 0 0 1 0)))))
    (assert-num= #(3.3 4.4 1.1 5.5 2.2) (permute rvec pmat))
    (assert-num= #(3.3 5.5 1.1 2.2 4.4) (permute pmat cvec))
    (assert-condition error (permute rerr pmat))
    (assert-condition error (permute pmat cerr))))

;;; Data vector scale

(deftest scale-data-vector (data-vector)
  (assert-num= #(2.0 4.0 6.0 8.0 10.0) (scale 2.0 (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))
      (scale #C(1.0 1.0) (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0))
      (scale 2.0 (column-vector #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))))
  (assert-num= #(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0))
      (scale #C(2.0 2.0) (column-vector #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0)))))

(deftest nscale-data-vector (data-vector)
  (assert-num= #(2.0 4.0 6.0 8.0 10.0) (nscale 2.0 (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))
      (nscale #C(1.0 1.0) (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num= #(#C(2.0 2.0) #C(4.0 4.0) #C(6.0 6.0) #C(8.0 8.0) #C(10.0 10.0))
      (nscale 2.0 (column-vector #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0))))
  (assert-num= #(#C(0.0 4.0) #C(0.0 8.0) #C(0.0 12.0) #C(0.0 16.0) #C(0.0 20.0))
      (nscale #C(2.0 2.0) (column-vector #C(1.0 1.0) #C(2.0 2.0) #C(3.0 3.0) #C(4.0 4.0) #C(5.0 5.0)))))

;;; Vector addition

(deftest add-data-vector (data-vector)
  ;; Real
  (let ((vec1 (column-vector 1.1 2.2 3.3 4.4))
        (vec2 (column-vector 1.1 2.2 3.3 4.4)))
    (assert-num= #(2.2 4.4 6.6 8.8) (add vec1 vec2))
    (assert-num= #(3.3 6.6 9.9 13.2) (add vec1 vec2 :scalar1 2.0))
    (assert-num= #(3.3 6.6 9.9 13.2) (add vec1 vec2 :scalar2 2.0))
    (assert-num= #(4.4 8.8 13.2 17.6) (add vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector 1.1 2.2 3.3 4.4))
        (vec2 (row-vector 1.1 2.2 3.3 4.4)))
    (assert-num= #(2.2 4.4 6.6 8.8) (add vec1 vec2))
    (assert-num= #(3.3 6.6 9.9 13.2) (add vec1 vec2 :scalar1 2.0))
    (assert-num= #(3.3 6.6 9.9 13.2) (add vec1 vec2 :scalar2 2.0))
    (assert-num= #(4.4 8.8 13.2 17.6) (add vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vec1 (column-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (column-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) (add vec1 vec2))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (add vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (add vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6))
	(add vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (row-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) (add vec1 vec2))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (add vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(3.3 6.6) #C(9.9 13.2)) (add vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6)) (add vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Errors
  (assert-condition error (add (column-vector 1.1 2.2 3.3 4.4)
			       (row-vector 1.1 2.2 3.3 4.4)))
  (assert-condition error (add (row-vector 1.1 2.2 3.3 4.4)
			       (column-vector 1.1 2.2 3.3 4.4))))

;;; Destructive vector addition

(deftest nadd-data-vector (data-vector)
  ;; Real
  (let ((vec1 (column-vector 1.1 2.2 3.3 4.4))
        (vec2 (column-vector 1.1 2.2 3.3 4.4)))
    (assert-eq vec1 (nadd vec1 vec2))
    (assert-num= #(2.2 4.4 6.6 8.8) vec1)
    (assert-num= #(4.4 8.8 13.2 17.6) (nadd vec1 vec2 :scalar2 2.0))
    (assert-num= #(9.9 19.8 29.7 39.6) (nadd vec1 vec2 :scalar1 2.0))
    (assert-num= #(22.0 44.0 66.0 88.0) (nadd vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector 1.1 2.2 3.3 4.4))
        (vec2 (row-vector 1.1 2.2 3.3 4.4)))
    (assert-eq vec1 (nadd vec1 vec2))
    (assert-num= #(2.2 4.4 6.6 8.8) vec1)
    (assert-num= #(4.4 8.8 13.2 17.6)
	(nadd vec1 vec2 :scalar2 2.0))
    (assert-num= #(9.9 19.8 29.7 39.6) (nadd vec1 vec2 :scalar1 2.0))
    (assert-num= #(22.0 44.0 66.0 88.0) (nadd vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vec1 (column-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (column-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vec1 (nadd vec1 vec2))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) vec1)
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6)) (nadd vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(9.9 19.8) #C(29.7 39.6)) (nadd vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(22.0 44.0) #C(66.0 88.0)) (nadd vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (row-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vec1 (nadd vec1 vec2))
    (assert-num= #(#C(2.2 4.4) #C(6.6 8.8)) vec1)
    (assert-num= #(#C(4.4 8.8) #C(13.2 17.6)) (nadd vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(9.9 19.8) #C(29.7 39.6)) (nadd vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(22.0 44.0) #C(66.0 88.0)) (nadd vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Errors
  (assert-condition error (nadd (column-vector 1.1 2.2 3.3 4.4)
				(row-vector 1.1 2.2 3.3 4.4)))
  (assert-condition error (nadd (row-vector 1.1 2.2 3.3 4.4)
				(column-vector 1.1 2.2 3.3 4.4))))

;;; Vector subtraction

(deftest subtract-data-vector (data-vector)
  ;; Real
  (let ((vec1 (column-vector 1.1 2.2 3.3 4.4))
        (vec2 (column-vector 1.1 2.2 3.3 4.4)))
    (assert-num= #(0.0 0.0 0.0 0.0) (subtract vec1 vec2))
    (assert-num= #(1.1 2.2 3.3 4.4) (subtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(-1.1 -2.2 -3.3 -4.4) (subtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(0.0 0.0 0.0 0.0) (subtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector 1.1 2.2 3.3 4.4))
        (vec2 (row-vector 1.1 2.2 3.3 4.4)))
    (assert-num= #(0.0 0.0 0.0 0.0) (subtract vec1 vec2))
    (assert-num= #(1.1 2.2 3.3 4.4) (subtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(-1.1 -2.2 -3.3 -4.4) (subtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(0.0 0.0 0.0 0.0) (subtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vec1 (column-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (column-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (subtract vec1 vec2))
    (assert-num= #(#C(1.1 2.2) #C(3.3 4.4)) (subtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(-1.1 -2.2) #C(-3.3 -4.4)) (subtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (subtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (row-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (subtract vec1 vec2))
    (assert-num= #(#C(1.1 2.2) #C(3.3 4.4)) (subtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(-1.1 -2.2) #C(-3.3 -4.4)) (subtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) (subtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Errors
  (assert-condition error (subtract (column-vector 1.1 2.2 3.3 4.4)
				    (row-vector 1.1 2.2 3.3 4.4)))
  (assert-condition error (subtract (row-vector 1.1 2.2 3.3 4.4)
				    (column-vector 1.1 2.2 3.3 4.4))))

;;; Destructive vector subtraction

(deftest nsubtract-data-vector (data-vector)
  ;; Real
  (let ((vec1 (column-vector 1.1 2.2 3.3 4.4))
        (vec2 (column-vector 1.1 2.2 3.3 4.4)))
    (assert-eq vec1 (nsubtract vec1 vec2))
    (assert-num= #(0.0 0.0 0.0 0.0) vec1)
    (assert-num= #(-2.2 -4.4 -6.6 -8.8) (nsubtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(-5.5 -11.0 -16.5 -22.0) (nsubtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(-13.2 -26.4 -39.6 -52.8) (nsubtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector 1.1 2.2 3.3 4.4))
        (vec2 (row-vector 1.1 2.2 3.3 4.4)))
    (assert-eq vec1 (nsubtract vec1 vec2))
    (assert-num= #(0.0 0.0 0.0 0.0) vec1)
    (assert-num= #(-2.2 -4.4 -6.6 -8.8) (nsubtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(-5.5 -11.0 -16.5 -22.0) (nsubtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(-13.2 -26.4 -39.6 -52.8) (nsubtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Complex
  (let ((vec1 (column-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (column-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vec1 (nsubtract vec1 vec2))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) vec1)
    (assert-num= #(#C(-2.2 -4.4) #C(-6.6 -8.8)) (nsubtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(-5.5 -11.0) #C(-16.5 -22.0)) (nsubtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(-13.2 -26.4) #C(-39.6 -52.8)) (nsubtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  (let ((vec1 (row-vector #C(1.1 2.2) #C(3.3 4.4)))
        (vec2 (row-vector #C(1.1 2.2) #C(3.3 4.4))))
    (assert-eq vec1 (nsubtract vec1 vec2))
    (assert-num= #(#C(0.0 0.0) #C(0.0 0.0)) vec1)
    (assert-num= #(#C(-2.2 -4.4) #C(-6.6 -8.8)) (nsubtract vec1 vec2 :scalar2 2.0))
    (assert-num= #(#C(-5.5 -11.0) #C(-16.5 -22.0)) (nsubtract vec1 vec2 :scalar1 2.0))
    (assert-num= #(#C(-13.2 -26.4) #C(-39.6 -52.8)) (nsubtract vec1 vec2 :scalar1 2.0 :scalar2 2.0)))
  ;; Errors
  (assert-condition error (nsubtract (column-vector 1.1 2.2 3.3 4.4)
				     (row-vector 1.1 2.2 3.3 4.4)))
  (assert-condition error (nsubtract (row-vector 1.1 2.2 3.3 4.4)
				     (column-vector 1.1 2.2 3.3 4.4))))

;;; Vector data dot product

(deftest product-data-vector (data-vector)
  ;; Real vectors
  (assert-num= 55 (product (row-vector 1 2 3 4 5) (column-vector 1 2 3 4 5)))
  (assert-num= 55.0 (product (row-vector 1.0 2.0 3.0 4.0 5.0) (column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num= 55.0d0 (product (row-vector 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0) (column-vector 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0)))
  ;; Real vectors with conjugate keyword
  (assert-num= 55 (product (row-vector 1 2 3 4 5) (column-vector 1 2 3 4 5)))
  ;; Complex vectors
  (assert-num= #C(8 18) (product (row-vector #C(1 1) #C(2 1) #C(3 1)) (column-vector #C(1 2) #C(2 2) #C(3 2))))
  (assert-num= #C(8.0 18.0) (product (row-vector #C(1.0 1.0) #C(2.0 1.0) #C(3.0 1.0)) (column-vector #C(1.0 2.0) #C(2.0 2.0) #C(3.0 2.0))))
  (assert-num=  #C(8.0d0 18.0d0) (product (row-vector #C(1.0d0 1.0d0) #C(2.0d0 1.0d0) #C(3.0d0 1.0d0)) (column-vector #C(1.0d0 2.0d0) #C(2.0d0 2.0d0) #C(3.0d0 2.0d0))))
  ;; Errors
  (assert-condition error (product (row-vector 1 2 3)
				   (column-vector 1 2 3 4)))
  (assert-condition error (product (column-vector 1 2 3)
				   (column-vector 1 2 3)))
  (assert-condition error (product (row-vector 1 2 3)
				   (row-vector 1 2 3)))
  (assert-condition error (product (column-vector 1 2 3)
				   (row-vector 1 2 3))))

