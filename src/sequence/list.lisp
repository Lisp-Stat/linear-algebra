;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Fundamental List Operations

(in-package #:linear-algebra)

(defmethod %norm ((data list) (measure (eql 1)))
  "Return the Taxicab norm of the list."
  (loop for element in data sum (abs element)))

(defmethod %norm ((data list) (measure (eql 2)))
  "Return the Euclidean norm of the list."
  (multiple-value-bind (scale sumsq)
      (sumsq (loop for val in data collect (abs val)))
    (* scale (sqrt sumsq))))

(defmethod %norm ((data list) (measure integer))
  "Return the p-norm of the list."
  (multiple-value-bind (scale sump)
      (sump (loop for val in data collect (abs val)) measure)
    (* scale (expt sump (/ measure)))))

(defmethod %norm ((data list) (measure (eql :infinity)))
  "Return the infinity, or maximum, norm of vector."
  (loop for element in data maximize (abs element)))

(defmethod norm ((data list) &optional (measure 1))
  (%norm data measure))

(defmethod transpose ((data list))
  "Return a row vector."
  (copy-list data))

(defmethod ntranspose ((data list))
  "Return a row vector destructively."
  data)

(defmethod permute ((data list) (matrix permutation-matrix))
  "Return the permutation of the list."
  (if (= (length data) (matrix-row-dimension matrix))
      (loop
       with permuted = (make-list (length data))
       for column across (contents matrix)
       and row = 0 then (1+ row)
       do (setf (nth column permuted) (nth row data))
       finally (return permuted))
      (error "List(~D) and permutation~A matrix sizes are incompatible." (length data) (matrix-dimensions matrix))))

(defmethod permute ((matrix permutation-matrix) (data list))
  "Return the permutation of the list."
  (if (= (length data) (matrix-row-dimension matrix))
      (loop
       with permuted = (make-list (length data))
       for column across (contents matrix)
       and row = 0 then (1+ row)
       do (setf (nth row permuted) (nth column data))
       finally (return permuted))
      (error "Permutation matrix~A and list(~D) sizes are incompatible." (matrix-dimensions matrix) (length data))))

(defmethod scale ((scalar number) (data list))
  "Return the list scaled by scalar."
  (loop for item in data collect (* scalar item)))

(defmethod nscale ((scalar number) (data list))
  "Return the list destructively scaled by scalar."
  (map-into data (lambda (x) (* scalar x)) data))

(defmethod add ((list1 list) (list2 list) &key scalar1 scalar2)
  "Return the addition of scalar1*list1 with scalar2*list2"
  (if (= (length list1) (length list2))
      (loop
       with op = (scaled-binary-op #'+ scalar1 scalar2)
       for item1 in list1
       and item2 in list2
       collect (funcall op item1 item2))
      (error "LIST1(~D) and LIST2(~D) are not of equal length." (length list1) (length list2))))

(defmethod nadd ((list1 list) (list2 list) &key scalar1 scalar2)
  "Return the addition of scalar2*list2 to scalar1*list1."
  (if (= (length list1) (length list2))
      (map-into
       list1 (scaled-binary-op #'+ scalar1 scalar2)
       list1 list2)
      (error "LIST1(~D) and LIST2(~D) are not of equal length." (length list1) (length list2))))

(defmethod subtract ((list1 list) (list2 list) &key scalar1 scalar2)
  "Return the subraction of scalar2*list2 from scalar1*list1."
  (if (= (length list1) (length list2))
      (loop
       with op = (scaled-binary-op #'- scalar1 scalar2)
       for item1 in list1
       and item2 in list2
       collect (funcall op item1 item2))
      (error "LIST1(~D) and LIST2(~D) are not of equal length."
             (length list1) (length list2))))

(defmethod nsubtract ((list1 list) (list2 list) &key scalar1 scalar2)
  "Return the subraction of scalar2*list2 from scalar1*list1."
  (if (= (length list1) (length list2))
      (map-into
       list1 (scaled-binary-op #'- scalar1 scalar2)
       list1 list2)
      (error "LIST1(~D) and LIST2(~D) are not of equal length." (length list1) (length list2))))

(defmethod product
    ((list1 list) (list2 list) &optional scalar)
  "Return the dot product of list1 and list2."
  (if (= (length list1) (length list2))
      (loop
       for element1 in list1
       and element2 in list2
       sum (* element1 element2) into result
       finally
       (return (if scalar (* scalar result) result)))
      (error "LIST1(~D) and LIST2(~D) are not of equal length." (length list1) (length list2))))
