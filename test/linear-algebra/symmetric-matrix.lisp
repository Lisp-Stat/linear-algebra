;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite symmetric-matrix-core-test (linear-algebra-core-test))

(defun symmetric-matrix (&optional (start 0) (end 10))
  (linear-algebra:make-matrix
   (- end start) (- end start)
   :matrix-type 'linear-algebra:symmetric-matrix
   :initial-contents (symmetric-array start end)))

(deftest make-symmetric-matrix (symmetric-matrix-core-test)
  ;; A default symmetric matrix
  (let ((matrix
         (linear-algebra:make-matrix
          10 10 :matrix-type 'linear-algebra:symmetric-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-rational-equal
     (make-array '(10 10) :initial-element 0)
     matrix))
  ;; Specify the symmetric matrix element type
  (let ((matrix
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:symmetric-matrix
          :element-type 'single-float)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-eq
     (array-element-type
      (linear-algebra::contents matrix))
     (array-element-type
      (make-array '(10 10) :element-type 'single-float)))
    (assert-float-equal
     (make-array '(10 10) :initial-element 0.0
                 :element-type 'single-float)
     matrix))
  ;; Specify the symmetric matrix initial element
  (let ((matrix
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:symmetric-matrix
          :initial-element 1.0)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-float-equal
     (make-array '(10 10) :initial-element 1.0)
     matrix))
  ;; Specify the symmetric matrix contents - Nested list
  (let* ((data
          '((1.1 1.2 1.3 1.4)
            (1.2 2.2 2.3 2.4)
            (1.3 2.3 3.3 3.4)
            (1.4 2.4 3.4 4.4))) 
         (matrix
          (linear-algebra:make-matrix
           4 4
           :matrix-type 'linear-algebra:symmetric-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the symmetric matrix contents - Nested vector
  (let* ((data
          #(#(1.1 1.2 1.3 1.4)
            #(1.2 2.2 2.3 2.4)
            #(1.3 2.3 3.3 3.4)
            #(1.4 2.4 3.4 4.4)))
         (matrix
          (linear-algebra:make-matrix
           4 4
           :matrix-type 'linear-algebra:symmetric-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the symmetric matrix contents - 2D array
  (let* ((data
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4))))
         (matrix
          (linear-algebra:make-matrix
           4 4
           :matrix-type 'linear-algebra:symmetric-matrix
           :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:symmetric-matrix))
    (assert-float-equal data matrix))
  ;; Erroneous 2D array input data
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 4
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    #3A(((1.1 1.2) (2.1 2.2))
        ((3.1 3.2) (4.1 4.2))
        ((5.1 5.2) (6.1 6.2)))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 4
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    (symmetric-array 0 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 3
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    (symmetric-array 0 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :element-type 'single-float
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    '((1.0 2.0 3.0) (4 5 6) (7 8 9))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :element-type 'single-float
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    #(#(1.0 2.0 3.0) #(4 5 6) #(7 8 9))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :element-type 'single-float
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    #2A((1.0 2.0 3.0) (4 5 6) (7 8 9))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    5 5
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-contents
    (coordinate-array 0 0 5 5)))
  ;; Specify initial element and initial contents
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 4
    :matrix-type 'linear-algebra:symmetric-matrix
    :initial-element 1.1
    :initial-contents
    (symmetric-array 0 4))))

;;; Test the symmetric matrix predicate
(deftest symmetric-matrix-predicate  (symmetric-matrix-core-test)
  (assert-true
   (linear-algebra:symmetric-matrix-p
    (linear-algebra:make-matrix
     10 10 :matrix-type 'linear-algebra:symmetric-matrix)))
  (assert-false
   (linear-algebra:symmetric-matrix-p (make-array '(10 10)))))

;;; Test the symmetric matrix bounds
(deftest symmetric-matrix-in-bounds-p (symmetric-matrix-core-test)
  (test-matrix-in-bounds-p 'linear-algebra:symmetric-matrix))

;;; Test the symmetric matrix element type
(deftest symmetric-matrix-element-type (symmetric-matrix-core-test)
  (test-matrix-element-type 'linear-algebra:symmetric-matrix t nil))

;;; Test the symmetric matrix dimensions
(deftest symmetric-matrix-dimensions (symmetric-matrix-core-test)
  (test-matrix-dimensions 'linear-algebra:symmetric-matrix 9 9))

;;; Test the symmetric matrix row dimension
(deftest symmetric-matrix-row-dimension (symmetric-matrix-core-test)
  (test-matrix-row-dimension 'linear-algebra:symmetric-matrix 9 9))

;;; Test the symmetric matrix column dimension
(deftest symmetric-matrix-column-dimension (symmetric-matrix-core-test)
  (test-matrix-column-dimension 'linear-algebra:symmetric-matrix 9 9))

;;; Reference symmetric matrix elements
(deftest symmetric-matrix-mref (symmetric-matrix-core-test)
  (let* ((initial-contents
          '((1.1 1.2 1.3 1.4 1.5)
            (1.2 2.2 2.3 2.4 2.5)
            (1.3 2.3 3.3 3.4 3.5)
            (1.4 2.4 3.4 4.4 4.5)
            (1.5 2.5 3.5 4.5 5.5)))
         (rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli
          (do ((i0 (random-interior-index columns)
                   (random-interior-index columns)))
              ((/= i0 rowi) i0)))
         (data
          (make-array
           (list rows columns)
           :initial-contents
           initial-contents))
         (matrix
          (linear-algebra:make-matrix
           rows columns
           :matrix-type
           'linear-algebra:symmetric-matrix
           :initial-contents
           initial-contents)))
    (assert-float-equal
     (aref data 0 0)
     (linear-algebra:mref matrix 0 0))
    (assert-float-equal
     (aref data 0 cend)
     (linear-algebra:mref matrix 0 cend))
    (assert-float-equal
     (aref data rend 0)
     (linear-algebra:mref matrix rend 0))
    (assert-float-equal
     (linear-algebra:mref matrix 0 cend)
     (linear-algebra:mref matrix rend 0))
    (assert-float-equal
     (aref data rend cend)
     (linear-algebra:mref matrix rend cend))
    (assert-float-equal
     (aref data rowi coli)
     (linear-algebra:mref matrix rowi coli))
    (assert-float-equal
     (linear-algebra:mref matrix rowi coli)
     (linear-algebra:mref matrix coli rowi))))

;;; Set symmetric matrix elements
(deftest symmetric-matrix-setf-mref (symmetric-matrix-core-test)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli
          (do ((i0 (random-interior-index columns)
                   (random-interior-index columns)))
              ((/= i0 rowi) i0)))
         (matrix
          (linear-algebra:make-matrix
           rows columns
           :matrix-type 'linear-algebra:symmetric-matrix
           :initial-contents
           '((1.1 1.2 1.3 1.4 1.5)
             (1.2 2.2 2.3 2.4 2.5)
             (1.3 2.3 3.3 3.4 3.5)
             (1.4 2.4 3.4 4.4 4.5)
             (1.5 2.5 3.5 4.5 5.5)))))
    (destructuring-bind (val1 val2 val3 val4)
        (make-random-list 4 1.0)
      (setf (linear-algebra:mref matrix 0 0)       val1)
      (setf (linear-algebra:mref matrix 0 cend)    val2)
      (setf (linear-algebra:mref matrix rend cend) val3)
      (setf (linear-algebra:mref matrix rowi coli) val4)
      (assert-float-equal val1 (linear-algebra:mref matrix 0 0))
      (assert-float-equal val2 (linear-algebra:mref matrix 0 cend))
      (assert-float-equal val2 (linear-algebra:mref matrix rend 0))
      (assert-float-equal val3 (linear-algebra:mref matrix rend cend))
      (assert-float-equal val4 (linear-algebra:mref matrix rowi coli))
      (assert-float-equal val4 (linear-algebra:mref matrix coli rowi)))))

;;; Copy the symmetric matrix
(deftest copy-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (symmetric-array 0 5))))
    (assert-true
     (linear-algebra:symmetric-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-float-equal
     matrix (linear-algebra:copy-matrix matrix))))

;;; Test the submatrix of a symmetric matrix
(deftest symmetric-submatrix (symmetric-matrix-core-test)
  (let ((matrix
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents (symmetric-array)))
        (submat
         (linear-algebra:make-matrix
          10 10
          :matrix-type 'linear-algebra:dense-matrix
          :initial-contents (symmetric-array))))
    ;; The entire matrix
    (assert-float-equal
     (symmetric-array)
     (linear-algebra:submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-float-equal
     (symmetric-array 3)
     (linear-algebra:submatrix matrix 3 3))
    ;; End row and column
    (assert-float-equal
     (symmetric-array 3 5)
     (linear-algebra:submatrix
      matrix 3 3 :end-row 5 :end-column 5))
    ;; Submatrix is a general matrix
    (assert-true
     (typep
      (linear-algebra:submatrix matrix 1 2)
      'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 2)
     (linear-algebra:submatrix matrix 1 2))
    (assert-true
     (typep
      (linear-algebra:submatrix matrix 1 1 :end-row 5)
      'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :end-row 5)
     (linear-algebra:submatrix matrix 1 1 :end-row 5))
    (assert-true
     (typep
      (linear-algebra:submatrix matrix 1 1 :end-column 8)
      'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :end-column 8)
     (linear-algebra:submatrix matrix 1 1 :end-column 8))
    ;; Start row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 11 5))
    ;; Start column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-row 11))
    ;; End column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-column 11))
    ;; Start row exceeds end row
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :end-row 6))
    ;; Start column exceeds end column
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :end-column 6))))

;;; Set the submatrix of a symmetric matrix
(deftest setf-symmetric-submatrix (symmetric-matrix-core-test)
  ;; Upper left submatrix
  (let ((array-ul
         (make-array
          '(5 5) :initial-contents
          '((0.0 1.0 2.0 0.0 0.0)
            (1.0 1.1 2.1 0.0 0.0)
            (2.0 2.1 2.2 0.0 0.0)
            (0.0 0.0 0.0 0.0 0.0)
            (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 0 0)
      (symmetric-matrix 0 3)))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 0 0 :end-row 3 :end-column 3)
      (symmetric-matrix))))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 1.0 2.0)
      (0.0 0.0 1.0 1.1 2.1)
      (0.0 0.0 2.0 2.1 2.2)))
   (setf-submatrix
    5 5 'linear-algebra:symmetric-matrix
    (linear-algebra:submatrix matrix 2 2)
    (symmetric-matrix)))
  ;; Middle submatrix
  (let ((array-mid
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 0.0 0.0)
            (0.0 1.1 2.1 3.1 0.0)
            (0.0 2.1 2.2 3.2 0.0)
            (0.0 3.1 3.2 3.3 0.0)
            (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 1 1)
      (symmetric-matrix 1 4)))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 1 1 :end-row 4 :end-column 4)
      (symmetric-matrix 1))))
  ;; Off diagonal submatrix
  (let ((array-off
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 1.0 2.0)
            (0.0 0.0 1.0 1.1 2.1)
            (0.0 1.0 2.0 2.1 2.2)
            (1.0 1.1 2.1 0.0 0.0)
            (2.0 2.1 2.2 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 0 2)
      (symmetric-matrix 0 3)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 0 2 :end-row 3)
      (symmetric-matrix))))
  (let ((array-off
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 0.0 0.0)
            (0.0 0.0 0.0 1.0 2.0)
            (0.0 0.0 1.0 1.1 2.1)
            (0.0 1.0 1.1 0.0 0.0)
            (0.0 2.0 2.1 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 1 2)
      (linear-algebra:submatrix
       (symmetric-matrix 0 3) 0 0 :end-row 2)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:symmetric-matrix
      (linear-algebra:submatrix matrix 1 2 :end-row 3)
      (symmetric-matrix))))
  ;; Asymmetric subsets
  (assert-error
   'error
   (setf
    (linear-algebra:submatrix
     (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
     0 1)
    (unit-matrix 5 3))))

;;; Replace all or part of a symmetric matrix
(deftest symmetric-matrix-replace (symmetric-matrix-core-test)
  ;; Replace the entire matrix
  (assert-float-equal
   (symmetric-matrix)
   (linear-algebra:replace-matrix
    (zero-matrix 10 10 :matrix-type 'linear-algebra:symmetric-matrix)
    (symmetric-matrix)))
  ;; Upper left submatrix
  (let ((array-ul
         (make-array
          '(5 5) :initial-contents
          '((0.0 1.0 2.0 0.0 0.0)
            (1.0 1.1 2.1 0.0 0.0)
            (2.0 2.1 2.2 0.0 0.0)
            (0.0 0.0 0.0 0.0 0.0)
            (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix 0 3)))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :end-row1 3 :end-column1 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :end-row2 3 :end-column1 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :end-row1 3 :end-column2 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :end-row2 3 :end-column2 3)))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 1.0 2.0)
      (0.0 0.0 1.0 1.1 2.1)
      (0.0 0.0 2.0 2.1 2.2)))
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
    (symmetric-matrix)
    :start-row1 2 :start-column1 2))
  ;; Middle submatrix
  (let ((array-mid
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 0.0 0.0)
            (0.0 0.0 1.0 2.0 0.0)
            (0.0 1.0 1.1 2.1 0.0)
            (0.0 2.0 2.1 2.2 0.0)
            (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix 0 3)
      :start-row1 1 :start-column1 1))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 1 :start-column1 1
      :end-row1 4 :end-column1 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 1 :start-column1 1
      :end-row2 3 :end-column1 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 1 :start-column1 1
      :end-row1 4 :end-column2 3))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 1 :start-column1 1
      :end-row2 3 :end-column2 3)))
  ;; Off diagonal submatrix
  (let ((array-off
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 1.0 2.0)
            (0.0 0.0 1.0 1.1 2.1)
            (0.0 1.0 2.0 2.1 2.2)
            (1.0 1.1 2.1 0.0 0.0)
            (2.0 2.1 2.2 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix 0 3)
      :start-row1 0 :start-column1 2))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 0 :start-column1 2
      :end-row1 3))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 0 :start-column1 2
      :end-row2 3)))
  (let ((array-off
         (make-array
          '(5 5) :initial-contents
          '((0.0 0.0 0.0 0.0 0.0)
            (0.0 0.0 0.0 1.0 2.0)
            (0.0 0.0 1.0 1.1 2.1)
            (0.0 1.0 1.1 0.0 0.0)
            (0.0 2.0 2.1 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix 0 3)
      :start-row1 1 :start-column1 2 :end-row2 2))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
      (symmetric-matrix)
      :start-row1 1 :start-column1 2 :end-row1 3)))
  ;; Asymmetric subsets
  (assert-error
   'error
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:symmetric-matrix)
    (unit-matrix 5 3)
    :start-column1 1)))

;;; Validate a range for a symmetric matrix.
(deftest symmetric-matrix-validated-range (symmetric-matrix-core-test)
  (test-matrix-validated-range
   'linear-algebra:symmetric-matrix 10 10))

;;; Symmetric matrix fundamental operations

(deftest norm-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix (symmetric-matrix)))
    (assert-float-equal
     94.5 (linear-algebra:norm matrix))
    (assert-float-equal
     94.5 (linear-algebra:norm matrix 1))
    (assert-float-equal
     9.9 (linear-algebra:norm matrix :max))
    (assert-float-equal
     68.94671 (linear-algebra:norm matrix :frobenius))
    (assert-float-equal
     94.5 (linear-algebra:norm matrix :infinity))
    (assert-error
     'error
     (linear-algebra:norm matrix :unknown))))

(deftest transpose-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix (symmetric-matrix))
        (transpose (symmetric-array)))
    (assert-true
     (typep
      (linear-algebra:transpose matrix)
      'linear-algebra:symmetric-matrix))
    (assert-float-equal
     transpose (linear-algebra:transpose matrix))))

(deftest ntranspose-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix (symmetric-matrix))
        (transpose (symmetric-array)))
    (assert-eq matrix (linear-algebra:ntranspose matrix))
    (assert-float-equal transpose matrix)))

(deftest permute-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix (symmetric-matrix 0 5))
        (pmat
         (linear-algebra:make-matrix
          5 5 :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)))))
    (assert-float-equal
     #2A((2.0 3.0 0.0 4.0 1.0)
         (2.1 3.1 1.0 4.1 1.1)
         (2.2 3.2 2.0 4.2 2.1)
         (3.2 3.3 3.0 4.3 3.1)
         (4.2 4.3 4.0 4.4 4.1))
     (linear-algebra:permute matrix pmat))
    (assert-float-equal
     #2A((2.0 2.1 2.2 3.2 4.2)
         (4.0 4.1 4.2 4.3 4.4)
         (0.0 1.0 2.0 3.0 4.0)
         (1.0 1.1 2.1 3.1 4.1)
         (3.0 3.1 3.2 3.3 4.3))
     (linear-algebra:permute pmat matrix))))

(deftest scale-symmetric-matrix (symmetric-matrix-core-test)
  (assert-float-equal
   #2A(( 3.3  3.6  3.9  4.2)
       ( 3.6  6.6  6.9  7.2)
       ( 3.9  6.9  9.9 10.2)
       ( 4.2  7.2 10.2 13.2))
   (linear-algebra:scale
    3.0 (linear-algebra:make-matrix
         4 4 :matrix-type 'linear-algebra:symmetric-matrix
         :initial-contents
         #2A((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4))))))

(deftest nscale-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix (linear-algebra:nscale 3.0 matrix))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     matrix)))

;;; FIXME : Add tests to cover addition/subtraction that results in a
;;; non-symmetric matrix

(deftest add-symmetric-matrix (symmetric-matrix-core-test)
  (let ((matrix
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    ;; No scalar
    (assert-float-equal
     #2A(( 2.2  2.4  2.6  2.8)
         ( 2.4  4.4  4.6  4.8)
         ( 2.6  4.6  6.6  6.8)
         ( 2.8  4.8  6.8  8.8))
     (linear-algebra:add matrix matrix))
    ;; Scalar1
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     (linear-algebra:add matrix matrix :scalar1 2.0))
    ;; Scalar2
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     (linear-algebra:add matrix matrix :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-float-equal
     #2A(( 5.5  6.0  6.5  7.0)
         ( 6.0 11.0 11.5 12.0)
         ( 6.5 11.5 16.5 17.0)
         ( 7.0 12.0 17.0 22.0))
     (linear-algebra:add matrix matrix :scalar1 2.0 :scalar2 3.0))))

(deftest nadd-symmetric-matrix (symmetric-matrix-core-test)
  ;; No scalar
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          '((1.1 1.2 1.3 1.4)
            (1.2 2.2 2.3 2.4)
            (1.3 2.3 3.3 3.4)
            (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (linear-algebra:nadd matrix1 matrix2))
    (assert-float-equal
     #2A((2.2 2.4 2.6 2.8)
         (2.4 4.4 4.6 4.8)
         (2.6 4.6 6.6 6.8)
         (2.8 4.8 6.8 8.8))
     matrix1))
  ;; Scalar1
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1 (linear-algebra:nadd matrix1 matrix2 :scalar1 2.0))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     matrix1))
  ;; Scalar2
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1 (linear-algebra:nadd matrix1 matrix2 :scalar2 2.0))
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     matrix1))
  ;; Scalar1 & Scalar2
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1
     (linear-algebra:nadd
      matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-float-equal
     #2A(( 5.5  6.0  6.5  7.0)
         ( 6.0 11.0 11.5 12.0)
         ( 6.5 11.5 16.5 17.0)
         ( 7.0 12.0 17.0 22.0))
     matrix1)))

(deftest subtract-symmetric-matrix (symmetric-matrix-core-test)
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A(( 2.2  2.4  2.6  2.8)
              ( 2.4  4.4  4.6  4.8)
              ( 2.6  4.6  6.6  6.8)
              ( 2.8  4.8  6.8  8.8))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    ;; No scalar
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     (linear-algebra:subtract matrix1 matrix2))
    ;; Scalar1
    (assert-float-equal
     #2A(( 3.3  3.6  3.9  4.2)
         ( 3.6  6.6  6.9  7.2)
         ( 3.9  6.9  9.9 10.2)
         ( 4.2  7.2 10.2 13.2))
     (linear-algebra:subtract matrix1 matrix2 :scalar1 2.0))
    ;; Scalar2
    (assert-float-equal
     #2A((0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0)
         (0.0 0.0 0.0 0.0))
     (linear-algebra:subtract matrix1 matrix2 :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     (linear-algebra:subtract
      matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))))

(deftest nsubtract-symmetric-matrix (symmetric-matrix-core-test)
  ;; No scalar
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '(( 2.2  2.4  2.6  2.8)
             ( 2.4  4.4  4.6  4.8)
             ( 2.6  4.6  6.6  6.8)
             ( 2.8  4.8  6.8  8.8)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (linear-algebra:nsubtract matrix1 matrix2))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     matrix1
     :no-scalar))
  ;; Scalar1
  (let ((matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '((1.1 1.2 1.3 1.4)
             (1.2 2.2 2.3 2.4)
             (1.3 2.3 3.3 3.4)
             (1.4 2.4 3.4 4.4)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1
     (linear-algebra:nsubtract matrix1 matrix2 :scalar1 2.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     matrix1
     :scalar1))
  ;; Scalar2
  (let ((*epsilon* (* 4F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '(( 3.3  3.6  3.9  4.2)
             ( 3.6  6.6  6.9  7.2)
             ( 3.9  6.9  9.9 10.2)
             ( 4.2  7.2 10.2 13.2)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1
     (linear-algebra:nsubtract matrix1 matrix2 :scalar2 2.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     matrix1
     :scalar2))
  ;; Scalar1 & Scalar2
  (let ((*epsilon* (* 3F0 single-float-epsilon))
        (matrix1
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          (make-array
           '(4 4) :initial-contents
           '(( 2.2  2.4  2.6  2.8)
             ( 2.4  4.4  4.6  4.8)
             ( 2.6  4.6  6.6  6.8)
             ( 2.8  4.8  6.8  8.8)))))
        (matrix2
         (linear-algebra:make-matrix
          4 4 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.1 1.2 1.3 1.4)
              (1.2 2.2 2.3 2.4)
              (1.3 2.3 3.3 3.4)
              (1.4 2.4 3.4 4.4)))))
    (assert-eq
     matrix1
     (linear-algebra:nsubtract
      matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-float-equal
     #2A((1.1 1.2 1.3 1.4)
         (1.2 2.2 2.3 2.4)
         (1.3 2.3 3.3 3.4)
         (1.4 2.4 3.4 4.4))
     matrix1
     :scalar1-&-scalar2)))

(deftest product-symmetric-matrix (symmetric-matrix-core-test)
  ;; Row vector - dense matrix
  (assert-true
   (typep
    (linear-algebra:product
     (linear-algebra:row-vector 1.0 2.0 3.0)
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:symmetric-matrix
      :initial-contents
      #2A((1.1 1.2 1.3)
          (1.2 2.2 2.3)
          (1.3 2.3 3.3))))
    'linear-algebra:row-vector))
  (assert-float-equal
   #(7.4 12.5 15.8)
   (linear-algebra:product
    (linear-algebra:row-vector 1.0 2.0 3.0)
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))))
  (let ((*epsilon* (* 4F0 single-float-epsilon)))
    (assert-float-equal
     #(15.54 26.25 33.18)
     (linear-algebra:product
      (linear-algebra:row-vector 1.0 2.0 3.0)
      (linear-algebra:make-matrix
       3 3 :matrix-type 'linear-algebra:symmetric-matrix
       :initial-contents
       #2A((1.1 1.2 1.3)
           (1.2 2.2 2.3)
           (1.3 2.3 3.3)))
      2.1)))
  (assert-error
   'error
   (linear-algebra:product
    (linear-algebra:row-vector 1.0 2.0 3.0 4.0 5.0 6.0)
    (linear-algebra:make-matrix
     3 3 :initial-element 1.0
     :matrix-type 'linear-algebra:symmetric-matrix)))
  ;; Dense matrix - column vector
  (assert-true
   (typep
    (linear-algebra:product
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:symmetric-matrix
      :initial-contents
      #2A((1.1 1.2 1.3)
          (1.2 2.2 2.3)
          (1.3 2.3 3.3)))
     (linear-algebra:column-vector 1.0 2.0 3.0))
    'linear-algebra:column-vector))
  (assert-float-equal
   #(7.4 12.5 15.8)
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))
    (linear-algebra:column-vector 1.0 2.0 3.0)))
  (let ((*epsilon* (* 4F0 single-float-epsilon)))
    (assert-float-equal
     #(15.54 26.25 33.18)
     (linear-algebra:product
      (linear-algebra:make-matrix
       3 3 :matrix-type 'linear-algebra:symmetric-matrix
       :initial-contents
       #2A((1.1 1.2 1.3)
           (1.2 2.2 2.3)
           (1.3 2.3 3.3)))
      (linear-algebra:column-vector 1.0 2.0 3.0)
      2.1)))
  (assert-error
   'error
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 3 :initial-element 1.0
     :matrix-type 'linear-algebra:square-matrix)
    (linear-algebra:column-vector 1.0 2.0 3.0 4.0 5.0 6.0)))
  ;; Dense matrix - matrix
  (assert-true
   (typep
    (linear-algebra:product
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:symmetric-matrix
      :initial-contents
      #2A((1.1 1.2 1.3)
          (1.2 2.2 2.3)
          (1.3 2.3 3.3)))
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:symmetric-matrix
      :initial-contents
      #2A((1.1 1.2 1.3)
          (1.2 2.2 2.3)
          (1.3 2.3 3.3))))
    'linear-algebra:symmetric-matrix))
  (assert-true
   (typep
    (linear-algebra:product
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:symmetric-matrix
      :initial-contents
      #2A((1.1 1.2 1.3)
          (1.2 2.2 2.3)
          (1.3 2.3 3.3)))
     (linear-algebra:make-matrix
      3 3 :matrix-type 'linear-algebra:square-matrix
      :initial-contents
      #2A((1.0 1.0 1.0)
          (2.0 2.0 2.0)
          (3.0 3.0 3.0))))
    'linear-algebra:square-matrix))
  (assert-float-equal
   #2A((4.34  6.95      8.48)
       (6.95 11.57     14.209999)
       (8.48 14.209999 17.869999))
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))))
  (assert-float-equal
   #2A(( 9.114    14.594999 17.807999)
       (14.594999 24.296999 29.840996)
       (17.807999 29.840996 37.526997))
   (linear-algebra:product
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))
    (linear-algebra:make-matrix
     3 3 :matrix-type 'linear-algebra:symmetric-matrix
     :initial-contents
     #2A((1.1 1.2 1.3)
         (1.2 2.2 2.3)
         (1.3 2.3 3.3)))
    2.1)))

(deftest solve-symmetric-matrix (symmetric-matrix-core-test)
  (let ((vector2 (linear-algebra:column-vector 2.0 1.0))
        (vector3 (linear-algebra:column-vector 2.3 1.2 2.2))
        (matrix2
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents #2A((1.1 1.2) (1.2 2.2))))
        (matrix3
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.15 1.26 1.37)
              (1.26 2.23 2.31)
              (1.37 2.31 3.31)))))
    ;; 2x2
    (assert-float-equal
     #(3.2653065 -1.3265308)
     (linear-algebra:solve matrix2 vector2))
    (assert-float-equal
     #2A((1.1 1.2) (1.2 2.2)) matrix2)
    (assert-float-equal #(2.0 1.0)vector2)
    ;; 3x3
    (assert-float-equal
     #(3.5856622 -2.306286 0.79007966)
     (linear-algebra:solve matrix3 vector3))
    (assert-float-equal
     #2A((1.15 1.26 1.37)
         (1.26 2.23 2.31)
         (1.37 2.31 3.31))
     matrix3)
    (assert-float-equal #(2.3 1.2 2.2) vector3)))

(deftest nsolve-symmetric-matrix (symmetric-matrix-core-test)
  (let ((vector2 (linear-algebra:column-vector 2.0 1.0))
        (vector3 (linear-algebra:column-vector 2.3 1.2 2.2))
        (matrix2
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents #2A((1.1 1.2) (1.2 2.2))))
        (matrix3
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          #2A((1.15 1.26 1.37)
              (1.26 2.23 2.31)
              (1.37 2.31 3.31)))))
    ;; 2x2
    (assert-float-equal
     #(3.2653065 -1.3265308)
     (linear-algebra:nsolve matrix2 vector2))
    (assert-float-equal
     #2A((1.1 1.0909091) (1.0909091 0.8909091)) matrix2)
    (assert-float-equal #(3.2653065 -1.3265308) vector2)
    ;; 3x3
    (assert-float-equal
     #(3.5856622 -2.306286 0.79007966)
     (linear-algebra:nsolve matrix3 vector3))
    (assert-float-equal
     #2A((1.15      1.0956522  1.1913043)
         (1.0956522 0.84947825 0.9522979)
         (1.1913043 0.9522979  0.90754557))
     matrix3)
    (assert-float-equal #(3.5856622 -2.306286 0.79007966) vector3)))

(deftest invert-symmetric-matrix (symmetric-matrix-core-test)
  ;; 2x2
  (let ((matrix
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          '((1.1 1.2) (1.2 2.2)))))
    (assert-float-equal
     #2A((2.2448979 -1.2244898) (-1.2244898 1.122449))
     (linear-algebra:invert matrix))
    (assert-float-equal #2A((1.1 1.2) (1.2 2.2)) matrix))
  ;; 3x3
  (let ((matrix
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          '((1.15 1.26 1.37)
            (1.26 2.23 2.31)
            (1.37 2.31 3.31)))))
    (assert-float-equal
     #2A((2.3068395 -1.1345832 -0.16298579)
         (-1.1345832 2.1764503 -1.0493114)
         (-0.16298579 -1.0493114 1.101873))
     (linear-algebra:invert matrix))
    (assert-float-equal
     #2A((1.15 1.26 1.37)
         (1.26 2.23 2.31)
         (1.37 2.31 3.31))
     matrix)))

(deftest ninvert-symmetric-matrix (symmetric-matrix-core-test)
  ;; 2x2
  (let ((matrix
         (linear-algebra:make-matrix
          2 2 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          '((1.1 1.2) (1.2 2.2)))))
    (assert-float-equal
     #2A((2.2448979 -1.2244898) (-1.2244898 1.122449))
     (linear-algebra:ninvert matrix))
    (assert-float-equal
     #2A((1.1 1.0909091) (1.0909091 0.8909091)) matrix))
  ;; 3x3
  (let ((matrix
         (linear-algebra:make-matrix
          3 3 :matrix-type 'linear-algebra:symmetric-matrix
          :initial-contents
          '((1.15 1.26 1.37)
            (1.26 2.23 2.31)
            (1.37 2.31 3.31)))))
    (assert-float-equal
     #2A((2.3068395 -1.1345832 -0.16298579)
         (-1.1345832 2.1764503 -1.0493114)
         (-0.16298579 -1.0493114 1.101873))
     (linear-algebra:ninvert matrix))
    (assert-float-equal
     #2A((1.15      1.0956522  1.1913043)
         (1.0956522 0.84947825 0.9522979)
         (1.1913043 0.9522979  0.90754557))
     matrix)))
