;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite interface (matrix))

;;; Matrix object predicate

(deftest matrixp (interface)
  (assert-true (linear-algebra:matrixp (make-instance 'linear-algebra:matrix-object)))
  (assert-false (linear-algebra:matrixp t)))

;;; Matrix bounds

(defmacro test-matrix-in-bounds-p
          (matrix-type &optional
           (initial-contents nil initial-contents-p))
  (let ((mat (gensym "MATRIX-")))
    `(let ((,mat
            (linear-algebra:make-matrix
             10 10 :matrix-type ,matrix-type
             ,@(when initial-contents-p
                 `(:initial-contents ,initial-contents)))))
      (assert-true (linear-algebra:matrix-in-bounds-p ,mat (random 10) (random 10)))
      (assert-false (linear-algebra:matrix-in-bounds-p ,mat -1 0))
      (assert-false (linear-algebra:matrix-in-bounds-p ,mat 0 -1))
      (assert-false (linear-algebra:matrix-in-bounds-p ,mat 10 0))
      (assert-false (linear-algebra:matrix-in-bounds-p ,mat 0 10)))))

;;; Matrix element type

(defmacro test-matrix-element-type (matrix-type &optional (test-real-p t) (test-complex-p t))
  (let ((numeric-types (gensym "NUMERIC-TYPES-"))
        (mtype (gensym "MTYPE-"))
        (ntype (gensym "NTYPE-")))
    `(let ((,mtype ,matrix-type)
           (,numeric-types
            '(integer fixnum bit
              short-float single-float double-float long-float)))
      (dolist (,ntype ,numeric-types)
        ;; Real
        ,(when
          test-real-p
          `(assert-true
            (subtypep
             (linear-algebra:matrix-element-type
              (linear-algebra:make-matrix
               2 2 :matrix-type ,mtype :element-type ,ntype))
             (array-element-type
              (make-array '(2 2) :element-type ,ntype)))))
        ;; Complex
        ,(when
          test-complex-p
          `(assert-true
            (subtypep
             (linear-algebra:matrix-element-type
              (linear-algebra:make-matrix
               2 2 :matrix-type ,mtype :element-type `(complex ,,ntype)))
             (array-element-type
              (make-array '(2 2) :element-type `(complex ,,ntype))))))))))

;;; Matrix dimensions

(defmacro test-matrix-dimensions (matrix-type rows columns)
  `(assert-equal
    (list ,rows ,columns)
    (linear-algebra:matrix-dimensions
     (linear-algebra:make-matrix
      ,rows ,columns :matrix-type ,matrix-type))))

;;; Matrix row dimension

(defmacro test-matrix-row-dimension (matrix-type rows columns)
  `(assert-eq ,rows
    (linear-algebra:matrix-row-dimension
     (linear-algebra:make-matrix
      ,rows ,columns :matrix-type ,matrix-type))))

;;; Matrix column dimension

(defmacro test-matrix-column-dimension (matrix-type rows columns)
  `(assert-eq ,columns
    (linear-algebra:matrix-column-dimension
     (linear-algebra:make-matrix
      ,rows ,columns :matrix-type ,matrix-type))))

;;; Return a matrix modified with (SETF SUBMATRIX)

(defmacro setf-submatrix
          (rows columns matrix-type submatrix-form data-form)
  "Return the matrix modified with (SETF SUBMATRIX)"
  (let ((matrix (second submatrix-form)))
    `(let ((,matrix (zero-matrix ,rows ,columns
                                 :matrix-type ,matrix-type)))
      (setf ,submatrix-form ,data-form)
      ,matrix)))

;;; Matrix validated range

(defmacro test-matrix-validated-range (matrix-type rows columns)
  "Test that matrix-validate-range executes correctly."
  (let ((matrix  (gensym "MATRIX-"))
        (row1 (gensym "ROW1-"))
        (row2 (gensym "ROW2-"))
        (col1 (gensym "COL1-"))
        (col2 (gensym "COL2-")))
    `(let ((,matrix
            (linear-algebra:make-matrix
             ,rows ,columns :matrix-type ,matrix-type))
           (,row1 (random ,rows))
           (,row2 (random ,rows))
           (,col1 (random ,columns))
           (,col2 (random ,columns)))
       (multiple-value-bind (v1 v2 v3 v4)
           (linear-algebra:matrix-validated-range
            ,matrix ,row1 ,col1)
         (assert-eql ,row1 v1)
         (assert-eql ,col1 v2)
         (assert-eql ,rows v3)
         (assert-eql ,columns v4))
       (multiple-value-bind (v1 v2 v3 v4)
           (linear-algebra:matrix-validated-range
            ,matrix (min ,row1 ,row2) ,col1 (max ,row1 ,row2))
         (assert-eql (min ,row1 ,row2) v1)
         (assert-eql ,col1 v2)
         (assert-eql (max ,row1 ,row2) v3)
         (assert-eql ,columns v4))
       (multiple-value-bind (v1 v2 v3 v4)
           (linear-algebra:matrix-validated-range
            ,matrix ,row1 (min ,col1 ,col2) nil (max ,col1 ,col2))
         (assert-eql ,row1 v1)
         (assert-eql (min ,col1 ,col2) v2)
         (assert-eql ,rows v3)
         (assert-eql (max ,col1 ,col2) v4))
       (multiple-value-bind (v1 v2 v3 v4)
           (linear-algebra:matrix-validated-range
            ,matrix
            (min ,row1 ,row2) (min ,col1 ,col2)
            (max ,row1 ,row2) (max ,col1 ,col2))
         (assert-eql (min ,row1 ,row2) v1)
         (assert-eql (min ,col1 ,col2) v2)
         (assert-eql (max ,row1 ,row2) v3)
         (assert-eql (max ,col1 ,col2) v4))
      (assert-condition
       error
       (linear-algebra:matrix-validated-range ,matrix (1+ ,rows) ,col1))
      (assert-condition
       error
       (linear-algebra:matrix-validated-range ,matrix ,row1 (1+ ,columns)))
      (assert-condition
       error
       (linear-algebra:matrix-validated-range
        ,matrix (1- ,rows) ,col1 1))
      (assert-condition
       error
       (linear-algebra:matrix-validated-range
        ,matrix ,row1 (1- ,columns) ,rows 1))
      (assert-condition
       error
       (linear-algebra:matrix-validated-range
        ,matrix
        (1- ,rows) (1- ,columns) 1 1)))))
