;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LL
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite dense-matrix (core))

;;; Test dense matrix data operations

(deftest make-dense-matrix (dense-matrix)
  ;; A default dense matrix
  (let ((matrix
         (linear-algebra:make-matrix
          10 15
          :matrix-type 'linear-algebra:dense-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-num=
     (make-array '(10 15) :initial-element 0)
     matrix))
  ;; Specify the dense matrix element type
  (let ((matrix
         (linear-algebra:make-matrix
          10 15
          :matrix-type 'linear-algebra:dense-matrix
          :element-type 'single-float)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-eq
     (array-element-type
      (linear-algebra::contents matrix))
     (array-element-type
      (make-array '(10 15) :element-type 'single-float)))
    (assert-num=
     (make-array '(10 15) :initial-element 0.0
                 :element-type 'single-float)
     matrix))
  ;; Specify the dense matrix initial element
  (let ((matrix
         (linear-algebra:make-matrix
          10 15
          :matrix-type 'linear-algebra:dense-matrix
          :initial-element 1.0)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-num=
     (make-array '(10 15) :initial-element 1.0)
     matrix))
  ;; Specify the dense matrix contents - Nested list
  (let* ((data
          '((1.1 1.2 1.3 1.4)
            (2.1 2.2 2.3 2.4)
            (3.1 3.2 3.3 3.4))) 
         (matrix
          (linear-algebra:make-matrix
           3 4
           :matrix-type 'linear-algebra:dense-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-num=
     (make-array '(3 4) :initial-contents data)
     matrix))
  ;; Specify the dense matrix contents - Nested vector
  (let* ((data
          #(#(1.1 1.2 1.3 1.4)
            #(2.1 2.2 2.3 2.4)
            #(3.1 3.2 3.3 3.4)))
         (matrix
          (linear-algebra:make-matrix
           3 4
           :matrix-type 'linear-algebra:dense-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-num=
     (make-array '(3 4) :initial-contents data)
     matrix))
  ;; Specify the dense matrix contents - 2D array
  (let* ((data
          (make-array
           '(3 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4))))
         (matrix
          (linear-algebra:make-matrix
           3 4
           :matrix-type 'linear-algebra:dense-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:dense-matrix))
    (assert-num= data matrix))
  ;; Erroneous 2D array input data
  (assert-condition
   error
   (linear-algebra:make-matrix
    3 4 :initial-contents
    #3A(((1.1 1.2) (2.1 2.2))
        ((3.1 3.2) (4.1 4.2))
        ((5.1 5.2) (6.1 6.2)))))
  (assert-condition
   error
   (linear-algebra:make-matrix
    2 3 :initial-contents
    (coordinate-array 0 0 3 4)))
  (assert-condition
   error
   (linear-algebra:make-matrix
    3 2 :initial-contents
    (coordinate-array 0 0 3 4)))
  (assert-condition
   error
   (linear-algebra:make-matrix
    2 3 :element-type 'single-float
    :initial-contents
    #2A((1 2 3) (4 5 6))))
  ;; Specify initial element and initial contents
  (assert-condition
   error
   (linear-algebra:make-matrix
    3 4 :initial-element 1.1
    :initial-contents
    (coordinate-array 0 0 3 4))))

;;; Test the dense matrix predicate

(deftest dense-matrix-predicate (dense-matrix)
  (assert-true
   (linear-algebra:dense-matrix-p
    (linear-algebra:make-matrix
     10 15 :matrix-type 'linear-algebra:dense-matrix)))
  (assert-false
   (linear-algebra:dense-matrix-p (make-array '(10 15)))))

;;; Test the dense matrix bounds

(deftest dense-matrix-in-bounds-p (dense-matrix)
  (test-matrix-in-bounds-p 'linear-algebra:dense-matrix))

;;; Test the dense matrix element type

(deftest dense-matrix-element-type (dense-matrix)
  (test-matrix-element-type 'linear-algebra:dense-matrix))

;;; Test the dense matrix dimensions

(deftest dense-matrix-dimensions (dense-matrix)
  (test-matrix-dimensions 'linear-algebra:dense-matrix 5 7))

;;; Test the dense matrix row dimension

(deftest dense-matrix-row-dimension (dense-matrix)
  (test-matrix-row-dimension 'linear-algebra:dense-matrix 5 7))

;;; Test the dense matrix column dimension

(deftest dense-matrix-column-dimension (dense-matrix)
  (test-matrix-column-dimension 'linear-algebra:dense-matrix 5 7))

;;; Reference dense matrix elements

(deftest dense-matrix-mref (dense-matrix)
  (let* ((initial-contents
          '((1.1 1.2 1.3 1.4 1.5)
            (2.1 2.2 2.3 2.4 2.5)
            (3.1 3.2 3.3 3.4 3.5)))
         (rows 3) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (random-interior-index columns))
         (data
          (make-array
           (list rows columns)
           :initial-contents
           initial-contents))
         (matrix
          (linear-algebra:make-matrix
           rows columns
           :matrix-type
           'linear-algebra:dense-matrix
           :initial-contents
           initial-contents)))
    (assert-num=
     (aref data 0 0)
     (linear-algebra:mref matrix 0 0))
    (assert-num=
     (aref data 0 cend)
     (linear-algebra:mref matrix 0 cend))
    (assert-num=
     (aref data rend 0)
     (linear-algebra:mref matrix rend 0))
    (assert-num=
     (aref data rend cend)
     (linear-algebra:mref matrix rend cend))
    (assert-num=
     (aref data rowi coli)
     (linear-algebra:mref matrix rowi coli))))

;;; Set dense matrix elements

(deftest dense-matrix-setf-mref (dense-matrix)
  (let* ((rows 3) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (random-interior-index columns))
         (matrix
          (linear-algebra:make-matrix
           rows columns
           :matrix-type 'linear-algebra:dense-matrix
           :initial-contents
           '((1.1 1.2 1.3 1.4 1.5)
             (2.1 2.2 2.3 2.4 2.5)
             (3.1 3.2 3.3 3.4 3.5)))))
    (destructuring-bind (val1 val2 val3 val4 val5)
        (make-random-list 5 1.0)
      (setf (linear-algebra:mref matrix 0 0)    val1)
      (setf (linear-algebra:mref matrix 0 cend) val2)
      (setf (linear-algebra:mref matrix rend 0) val3)
      (setf (linear-algebra:mref matrix rend cend) val4)
      (setf (linear-algebra:mref matrix rowi coli) val5)
      (assert-num= val1 (linear-algebra:mref matrix 0 0))
      (assert-num= val2 (linear-algebra:mref matrix 0 cend))
      (assert-num= val3 (linear-algebra:mref matrix rend 0))
      (assert-num= val4 (linear-algebra:mref matrix rend cend))
      (assert-num= val5 (linear-algebra:mref matrix rowi coli)))))

;;; Copy the dense matrix

(deftest copy-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          3 5
          :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((1.1 1.2 1.3 1.4 1.5)
            (2.1 2.2 2.3 2.4 2.5)
            (3.1 3.2 3.3 3.4 3.5)))))
    (assert-true
     (linear-algebra:dense-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-num=
     matrix (linear-algebra:copy-matrix matrix))))

;;; Test the submatrix of a dense matrix

(deftest dense-submatrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          7 10
          :matrix-type
          'linear-algebra:dense-matrix
          :initial-contents
          (coordinate-array 0 0 7))))
    ;; The entire matrix
    (assert-num=
     (coordinate-array 0 0 7)
     (linear-algebra:submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-num=
     (coordinate-array 3 3 7)
     (linear-algebra:submatrix matrix 3 3))
    ;; End row and column
    (assert-num=
     (coordinate-array 3 4 5 5)
     (linear-algebra:submatrix matrix 3 4 :end-row 5 :end-column 5))
    ;; Start row exceeds dimensions
    (assert-condition
     error (linear-algebra:submatrix matrix 8 5))
    ;; Start column exceeds dimensions
    (assert-condition
     error (linear-algebra:submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-condition
     error (linear-algebra:submatrix matrix 5 5 :end-row 8))
    ;; End column exceeds dimensions
    (assert-condition
     error (linear-algebra:submatrix matrix 5 5 :end-column 11))
    ;; Start row exceeds end row
    (assert-condition
     error (linear-algebra:submatrix matrix 7 7 :end-row 6))
    ;; Start column exceeds end column
    (assert-condition
     error (linear-algebra:submatrix matrix 7 7 :end-column 6))))

;;; Set the submatrix of a dense matrix

(deftest setf-dense-submatrix (dense-matrix)
  ;; Upper left submatrix
  (let ((array-ul
         (make-array
          '(5 5) :initial-contents
          '((1 1 0 0 0)
            (1 1 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)))))
    (assert-num=
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 0)
      (unit-matrix 2 2)))
    (assert-num=
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix
       matrix 0 0 :end-row 2 :end-column 2)
      (linear-algebra:submatrix (unit-matrix 5 5) 0 0)))
    (assert-num=
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 0)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 0 0 :end-row 2 :end-column 2)))
    (assert-num=
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 0 :end-row 2 :end-column 2)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 2 2 :end-row 4 :end-column 4))))
  ;; Upper right submatrix
  (let ((array-ur
         (make-array
          '(5 5) :initial-contents
          '((0 0 0 1 1)
            (0 0 0 1 1)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)))))
    (assert-num=
     array-ur
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 3)
      (unit-matrix 2 2)))
    (assert-num=
     array-ur
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 3)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 0 3 :end-row 2 :end-column 5)))
    (assert-num=
     array-ur
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 3 :end-row 2 :end-column 5)
      (unit-matrix 5 5)))
    (assert-num=
     array-ur
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 0 3 :end-row 2 :end-column 5)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 2 2 :end-row 4 :end-column 4))))
  ;; Lower left submatrix
  (let ((array-ll
         (make-array
          '(5 5) :initial-contents
          '((0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (1 1 0 0 0)
            (1 1 0 0 0)))))
    (assert-num=
     array-ll
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 0)
      (unit-matrix 2 2)))
    (assert-num=
     array-ll
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 0)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 0 3 :end-row 2 :end-column 5)))
    (assert-num=
     array-ll
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 0 :end-row 5 :end-column 2)
      (unit-matrix 5 5)))
    (assert-num=
     array-ll
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 0 :end-row 5 :end-column 2)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 2 2 :end-row 4 :end-column 4))))
  ;; Lower right submatrix
  (let ((array-lr
         (make-array
          '(5 5)
          :initial-contents
          '((0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 1 1)
            (0 0 0 1 1)))))
    (assert-num=
     array-lr
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 3)
      (unit-matrix 2 2)))
    (assert-num=
     array-lr
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 3)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 0 3 :end-row 2 :end-column 5)))
    (assert-num=
     array-lr
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 3 :end-row 5 :end-column 5)
      (unit-matrix 5 5)))
    (assert-num=
     array-lr
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 3 3 :end-row 5 :end-column 5)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 2 2 :end-row 4 :end-column 4))))
  ;; Middle submatrix
  (let ((array-mid
         (make-array
          '(5 5)
          :initial-contents
          '((0 0 0 0 0)
            (0 1 1 1 0)
            (0 1 1 1 0)
            (0 1 1 1 0)
            (0 0 0 0 0)))))
    (assert-num=
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 1 1)
      (unit-matrix 3 3)))
    (assert-num=
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 1 1)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 1 1 :end-row 4 :end-column 4)))
    (assert-num=
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 1 1 :end-row 4 :end-column 4)
      (unit-matrix 5 5)))
    (assert-num=
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:dense-matrix
      (linear-algebra:submatrix matrix 1 1 :end-row 4 :end-column 4)
      (linear-algebra:submatrix
       (unit-matrix 5 5) 1 1 :end-row 4 :end-column 4)))))

;;; Replace all or part of a dense matrix

(deftest dense-matrix-replace (dense-matrix)
  ;; Replace the entire matrix
  (assert-num=
   (unit-matrix 5 5)
   (linear-algebra:replace-matrix
    (zero-matrix 5 5) (unit-matrix 5 5)))
  ;; Replace the first 2 rows
  (let ((result
         (make-array
          '(5 5)
          :initial-contents
          '((1 1 1 1 1)
            (1 1 1 1 1)
            (0 0 0 0 0)
            (0 0 0 0 0)
            (0 0 0 0 0)))))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 2 5)))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 2 7)))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :start-row2 3))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :end-row1 2))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :end-row2 2)))
  ;; Replace the first 3 columns
  (let ((result
         (make-array
          '(5 5)
          :initial-contents
          '((1 1 1 0 0)
            (1 1 1 0 0)
            (1 1 1 0 0)
            (1 1 1 0 0)
            (1 1 1 0 0)))))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 3)))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 7 3)))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :start-column2 2))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :end-column1 3))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5) :end-column2 3)))
  ;; Replace the center
  (let ((result
         (make-array
          '(5 5)
          :initial-contents
          '((0 0 0 0 0)
            (0 1 1 1 0)
            (0 1 1 1 0)
            (0 1 1 1 0)
            (0 0 0 0 0)))))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 3 3) :start-row1 1 :start-column1 1))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5)
      :start-row1 1 :start-column1 1
      :end-row1 4 :end-column1 4))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5)
      :start-row1 1 :start-column1 1
      :end-row2 3 :end-column2 3))
    (assert-num=
     result
     (linear-algebra:replace-matrix
      (zero-matrix 5 5)
      (unit-matrix 5 5)
      :start-row1 1 :start-column1 1
      :start-row2 2 :start-column2 2))))

;;; Validate a range for a dense matrix.

(deftest dense-matrix-validated-range (dense-matrix)
  (test-matrix-validated-range
   'linear-algebra:dense-matrix 10 10))

;;; Test dense matrix fundamental operations

(deftest norm-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-num=
     17.0 (linear-algebra:norm matrix))
    (assert-num=
     17.0 (linear-algebra:norm matrix 1))
    (assert-num=
     5.4 (linear-algebra:norm matrix :max))
    (assert-num=
     15.858751 (linear-algebra:norm matrix :frobenius))
    (assert-num=
     21.0 (linear-algebra:norm matrix :infinity))
    (assert-condition
     error
     (linear-algebra:norm matrix :unknown))))

(deftest transpose-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4))))
        (transpose
         #2A((1.1 2.1 3.1 4.1 5.1)
             (1.2 2.2 3.2 4.2 5.2)
             (1.3 2.3 3.3 4.3 5.3)
             (1.4 2.4 3.4 4.4 5.4))))
    (assert-true
     (typep
      (linear-algebra:transpose matrix)
      'linear-algebra:dense-matrix))
    (assert-num=
     transpose (linear-algebra:transpose matrix))))

(deftest ntranspose-dense-matrix (dense-matrix)
  (assert-condition
   error
   (linear-algebra:ntranspose
    (linear-algebra:make-matrix
     5 4 :initial-contents
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4)))))
  (let ((matrix
         (linear-algebra:make-matrix
          4 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4))))
        (transpose
         #2A((1.1 2.1 3.1 4.1)
             (1.2 2.2 3.2 4.2)
             (1.3 2.3 3.3 4.3)
             (1.4 2.4 3.4 4.4))))
    (assert-eq matrix (linear-algebra:ntranspose matrix))
    (assert-num= transpose matrix)))

(deftest permute-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5 :initial-contents
          #2A((1.0 1.1 1.2 1.3 1.4)
              (2.0 2.1 2.2 2.3 2.4)
              (3.0 3.1 3.2 3.3 3.4)
              (4.0 4.1 4.2 4.3 4.4)
              (5.0 5.1 5.2 5.3 5.4))))
        (pmat
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)))))
    (assert-num=
     #2A((1.2 1.3 1.0 1.4 1.1)
         (2.2 2.3 2.0 2.4 2.1)
         (3.2 3.3 3.0 3.4 3.1)
         (4.2 4.3 4.0 4.4 4.1)
         (5.2 5.3 5.0 5.4 5.1))
     (linear-algebra:permute matrix pmat))
    (assert-num=
     #2A((3.0 3.1 3.2 3.3 3.4)
         (5.0 5.1 5.2 5.3 5.4)
         (1.0 1.1 1.2 1.3 1.4)
         (2.0 2.1 2.2 2.3 2.4)
         (4.0 4.1 4.2 4.3 4.4))
     (linear-algebra:permute pmat matrix))))

(deftest scale-dense-matrix (dense-matrix)
  (assert-num=
   #2A(( 3.3  3.6  3.9  4.2)
       ( 6.3  6.6  6.9  7.2)
       ( 9.3  9.6  9.9 10.2)
       (12.3 12.6 12.9 13.2)
       (15.3 15.6 15.9 16.2))
   (linear-algebra:scale
    3.0 (linear-algebra:make-matrix
         5 4 :initial-contents
         #2A((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4))))))

(deftest nscale-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq matrix (linear-algebra:nscale 3.0 matrix))
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     matrix)))

(deftest add-dense-matrix (dense-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    ;; No scalar
    (assert-num=
     #2A(( 2.2  2.4  2.6  2.8)
         ( 4.2  4.4  4.6  4.8)
         ( 6.2  6.4  6.6  6.8)
         ( 8.2  8.4  8.6  8.8)
         (10.2 10.4 10.6 10.8))
     (linear-algebra:add matrix matrix))
    ;; Scalar1
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:add matrix matrix :scalar1 2.0))
    ;; Scalar2
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:add matrix matrix :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num=
     #2A(( 5.5  6.0  6.5  7.0)
         (10.5 11.0 11.5 12.0)
         (15.5 16.0 16.5 17.0)
         (20.5 21.0 21.5 22.0)
         (25.5 26.0 26.5 27.0))
     (linear-algebra:add matrix matrix :scalar1 2.0 :scalar2 3.0))))

(deftest nadd-dense-matrix (dense-matrix)
  ;; No scalar
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq matrix1 (linear-algebra:nadd matrix1 matrix2))
    (assert-num=
     #2A(( 2.2  2.4  2.6  2.8)
         ( 4.2  4.4  4.6  4.8)
         ( 6.2  6.4  6.6  6.8)
         ( 8.2  8.4  8.6  8.8)
         (10.2 10.4 10.6 10.8))
     matrix1))
  ;; Scalar1
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nadd matrix1 matrix2 :scalar1 2.0))
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     matrix1))
  ;; Scalar2
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nadd matrix1 matrix2 :scalar2 2.0))
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     matrix1))
  ;; Scalar1 & Scalar2
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nadd
              matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num=
     #2A(( 5.5  6.0  6.5  7.0)
         (10.5 11.0 11.5 12.0)
         (15.5 16.0 16.5 17.0)
         (20.5 21.0 21.5 22.0)
         (25.5 26.0 26.5 27.0))
     matrix1)))

(deftest subtract-dense-matrix (dense-matrix)
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A(( 2.2  2.4  2.6  2.8)
              ( 4.2  4.4  4.6  4.8)
              ( 6.2  6.4  6.6  6.8)
              ( 8.2  8.4  8.6  8.8)
              (10.2 10.4 10.6 10.8))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    ;; No scalar
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     (linear-algebra:subtract matrix1 matrix2))
    ;; Scalar1
    (assert-num=
     #2A(( 3.3  3.6  3.9  4.2)
         ( 6.3  6.6  6.9  7.2)
         ( 9.3  9.6  9.9 10.2)
         (12.3 12.6 12.9 13.2)
         (15.3 15.6 15.9 16.2))
     (linear-algebra:subtract matrix1 matrix2 :scalar1 2.0))
    ;; Scalar2
    (assert-num=
     #2A((0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0))
     (linear-algebra:subtract matrix1 matrix2 :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     (linear-algebra:subtract
      matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))))

(deftest nsubtract-dense-matrix (dense-matrix)
  ;; No scalar
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '(( 2.2  2.4  2.6  2.8)
             ( 4.2  4.4  4.6  4.8)
             ( 6.2  6.4  6.6  6.8)
             ( 8.2  8.4  8.6  8.8)
             (10.2 10.4 10.6 10.8)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq matrix1 (linear-algebra:nsubtract matrix1 matrix2))
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     matrix1))
  ;; Scalar1
  (let ((matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (2.1 2.2 2.3 2.4)
             (3.1 3.2 3.3 3.4)
             (4.1 4.2 4.3 4.4)
             (5.1 5.2 5.3 5.4)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nsubtract matrix1 matrix2 :scalar1 2.0))
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     matrix1))
  ;; Scalar2
  (let ((*epsilon* (* 4F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '(( 3.3  3.6  3.9  4.2)
             ( 6.3  6.6  6.9  7.2)
             ( 9.3  9.6  9.9 10.2)
             (12.3 12.6 12.9 13.2)
             (15.3 15.6 15.9 16.2)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nsubtract matrix1 matrix2 :scalar2 2.0))
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     matrix1))
  ;; Scalar1 & Scalar2
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          5 4 :initial-contents
          (make-array
           '(5 4) :initial-contents
           '(( 2.2  2.4  2.6  2.8)
             ( 4.2  4.4  4.6  4.8)
             ( 6.2  6.4  6.6  6.8)
             ( 8.2  8.4  8.6  8.8)
             (10.2 10.4 10.6 10.8)))))
        (matrix2
         (linear-algebra:make-matrix
          5 4 :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (2.1 2.2 2.3 2.4)
              (3.1 3.2 3.3 3.4)
              (4.1 4.2 4.3 4.4)
              (5.1 5.2 5.3 5.4)))))
    (assert-eq
     matrix1 (linear-algebra:nsubtract
              matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num=
     #2A((1.1 1.2 1.3 1.4)
         (2.1 2.2 2.3 2.4)
         (3.1 3.2 3.3 3.4)
         (4.1 4.2 4.3 4.4)
         (5.1 5.2 5.3 5.4))
     matrix1)))

(deftest product-dense-matrix (dense-matrix)
  ;; Row vector - dense matrix
  (assert-true
   (typep (linear-algebra:product
           (linear-algebra:row-vector 1.0 2.0 3.0)
           (unit-matrix 3 5))
          'linear-algebra:row-vector))
  (assert-num=
   #(15.0 30.0 45.0)
   (linear-algebra:product
    (linear-algebra:row-vector 1.0 2.0 3.0 4.0 5.0)
    (linear-algebra:make-matrix
     5 3 :initial-contents
     #2A((1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)))))
  (assert-num=
   #(31.5 63.0 94.5)
   (linear-algebra:product
    (linear-algebra:row-vector 1.0 2.0 3.0 4.0 5.0)
    (linear-algebra:make-matrix
     5 3 :initial-contents
     #2A((1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)
         (1.0 2.0 3.0)))
    2.1))
  (assert-condition
   error
   (linear-algebra:product
    (linear-algebra:row-vector 1.0 2.0 3.0 4.0 5.0 6.0)
    (linear-algebra:make-matrix 5 3 :initial-element 1.0)))
  ;; Dense matrix - column vector
  (assert-true
   (typep (linear-algebra:product
           (unit-matrix 5 3)
           (linear-algebra:column-vector 1.0 2.0 3.0))
          'linear-algebra:column-vector))
  (assert-num=
   #(15.0 30.0 45.0)
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 5 :initial-contents
     #2A((1.0 1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0 3.0)))
    (linear-algebra:column-vector 1.0 2.0 3.0 4.0 5.0)))
  (assert-num=
   #(31.5 63.0 94.5)
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 5 :initial-contents
     #2A((1.0 1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0 3.0)))
    (linear-algebra:column-vector 1.0 2.0 3.0 4.0 5.0)
    2.1))
  (assert-condition
   error
   (linear-algebra:product
    (linear-algebra:make-matrix 3 5 :initial-element 1.0)
    (linear-algebra:column-vector 1.0 2.0 3.0 4.0 5.0 6.0)))
  ;; Dense matrix - matrix
  (assert-true
   (typep (linear-algebra:product
           (unit-matrix 3 5) (unit-matrix 5 4))
          'linear-algebra:dense-matrix))
  (assert-num=
   #2A((15.0 15.0 15.0 15.0)
       (30.0 30.0 30.0 30.0)
       (45.0 45.0 45.0 45.0))
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 5 :initial-contents
     #2A((1.0 1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0 3.0)))
    (linear-algebra:make-matrix
     5 4 :initial-contents
     #2A((1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0)
         (4.0 4.0 4.0 4.0)
         (5.0 5.0 5.0 5.0)))))
  (assert-num=
   #2A((31.5 31.5 31.5 31.5)
       (63.0 63.0 63.0 63.0)
       (94.5 94.5 94.5 94.5))
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 5 :initial-contents
     #2A((1.0 1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0 3.0)))
    (linear-algebra:make-matrix
     5 4 :initial-contents
     #2A((1.0 1.0 1.0 1.0)
         (2.0 2.0 2.0 2.0)
         (3.0 3.0 3.0 3.0)
         (4.0 4.0 4.0 4.0)
         (5.0 5.0 5.0 5.0)))
    2.1))
  (assert-condition
   error
   (linear-algebra:product (unit-matrix 3 5) (unit-matrix 6 7))))

(deftest solve-dense-matrix (dense-matrix)
  (let ((*epsilon* (* 64 single-float-epsilon))
        (vector2 (linear-algebra:column-vector 1.0 2.0))
        (vector3 (linear-algebra:column-vector 2.3 1.2 2.2))
        (matrix2
         (linear-algebra:make-matrix
          2 2 :initial-contents '((1.1 1.2) (2.1 2.2))))
        (matrix3
         (linear-algebra:make-matrix
          3 3 :initial-contents
          '((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31)))))
    ;; 2x2
    (assert-num=
     #(2.0 -1.0) (linear-algebra:solve matrix2 vector2))
    (assert-num= #(1.0 2.0) vector2)
    (assert-num= #2A((1.1 1.2) (2.1 2.2)) matrix2)
    ;; 3x3
    ;; Maxima : #(66.36628 -151.8314 85.6105)
    (assert-num=
     #(66.36775 -151.8342 85.6118)
     (linear-algebra:solve matrix3 vector3))
    (assert-num= #(2.3 1.2 2.2) vector3)
    (assert-num=
     #2A((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31))
     matrix3)))

(deftest nsolve-dense-matrix (dense-matrix)
  (let ((*epsilon* (* 64 single-float-epsilon))
        (vector2 (linear-algebra:column-vector 1.0 2.0))
        (vector3 (linear-algebra:column-vector 2.3 1.2 2.2))
        (matrix2
         (linear-algebra:make-matrix
          2 2 :initial-contents '((1.1 1.2) (2.1 2.2))))
        (matrix3
         (linear-algebra:make-matrix
          3 3 :initial-contents
          '((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31)))))
    ;; 2x2
    (assert-num=
     #(2.0 -1.0) (linear-algebra:nsolve matrix2 vector2))
    ;; 3x3
    ;; Maxima : #(66.36628 -151.8314 85.6105)
    (assert-num=
     #(66.36775 -151.8342 85.6118)
     (linear-algebra:nsolve matrix3 vector3))))

(deftest invert-dense-matrix (dense-matrix)
  ;; 2x2
  (let ((matrix
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((1.1 1.2) (2.1 2.2)))))
    (assert-num=
     #2A((-22.000029 12.000016) (21.000027 -11.000015))
     (linear-algebra:invert matrix))
    (assert-num= #2A((1.1 1.2) (2.1 2.2)) matrix))
  ;; 3x3
  (let ((matrix
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((1.1 0.12 0.13)
            (0.21 2.2 0.23)
            (0.31 0.32 3.3)))))
    (assert-num=
     #2A((0.9272161 -0.04572601 -0.03333973)
         (-0.08021406 0.4631565 -0.029120658)
         (-0.07932379 -0.04061667 0.30898604))
     (linear-algebra:invert matrix))
    (assert-num=
     #2A((1.1 0.12 0.13)
         (0.21 2.2 0.23)
         (0.31 0.32 3.3))
     matrix))
  ;; 4x4
  (let ((matrix
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((10.0 0.12 0.13 0.14)
            (0.21 20.0 0.23 0.24)
            (0.31 0.32 30.0 0.34)
            (0.41 0.42 0.43 40.0)))))
    (assert-num=
     #2A((0.10003952 -5.862483e-4 -4.2409348e-4 -3.4301603e-4)
         (-0.0010267387 0.050018318 -3.748202e-4 -2.9333035e-4)
         (-0.001011414 -5.216503e-4 0.033345684 -2.7676846e-4)
         (-0.0010037516 -5.135755e-4 -3.5018355e-4 0.02500957))
     (linear-algebra:invert matrix))
    (assert-num=
     #2A((10.0 0.12 0.13 0.14)
         (0.21 20.0 0.23 0.24)
         (0.31 0.32 30.0 0.34)
         (0.41 0.42 0.43 40.0))
     matrix)))

(deftest ninvert-dense-matrix (dense-matrix)
  ;; 2x2
  (let ((matrix
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((1.1 1.2) (2.1 2.2)))))
    (assert-num=
     #2A((-22.000029 12.000016) (21.000027 -11.000015))
     (linear-algebra:ninvert matrix))
    (assert-num=
     #2A((2.1 2.2) (0.52380956 0.047618986)) matrix))
  ;; 3x3
  (let ((matrix
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((1.1 0.12 0.13)
            (0.21 2.2 0.23)
            (0.31 0.32 3.3)))))
    (assert-num=
     #2A((0.9272161 -0.04572601 -0.03333973)
         (-0.08021406 0.4631565 -0.029120658)
         (-0.07932379 -0.04061667 0.30898604))
     (linear-algebra:ninvert matrix))
    (assert-num=
     #2A((1.1        0.12       0.13)
         (0.19090909 2.177091   0.20518182)
         (0.28181818 0.13145148 3.2363923))
     matrix))
  ;; 4x4
  (let ((matrix
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:dense-matrix
          :initial-contents
          '((10.0 0.12 0.13 0.14)
            (0.21 20.0 0.23 0.24)
            (0.31 0.32 30.0 0.34)
            (0.41 0.42 0.43 40.0)))))
    (assert-num=
     #2A((0.10003952 -5.862483e-4 -4.2409348e-4 -3.4301603e-4)
         (-0.0010267387 0.050018318 -3.748202e-4 -2.9333035e-4)
         (-0.001011414 -5.216503e-4 0.033345684 -2.7676846e-4)
         (-0.0010037516 -5.135755e-4 -3.5018355e-4 0.02500957))
     (linear-algebra:ninvert matrix))
    (assert-num=
     #2A((10.0    0.12         0.13         0.14)
         ( 0.021 19.99748      0.22727      0.23706)
         ( 0.031  0.015815994 29.992375     0.33191067)
         ( 0.041  0.020756614  0.014001981 39.98469))
     matrix)))
