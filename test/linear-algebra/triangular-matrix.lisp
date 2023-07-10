;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite triangular-matrix-core-test (linear-algebra-core-test))

(defun upper-triangular-matrix (&optional (start 0) (end 10))
  (linear-algebra:make-matrix
   (- end start) (- end start)
   :matrix-type 'linear-algebra:upper-triangular-matrix
   :element-type 'single-float
   :initial-contents (upper-triangular-array start end)))

(defun lower-triangular-matrix (&optional (start 0) (end 10))
  (linear-algebra:make-matrix
   (- end start) (- end start)
   :matrix-type 'linear-algebra:lower-triangular-matrix
   :element-type 'single-float
   :initial-contents (lower-triangular-array start end)))

;;; Upper triangular matrix construction tests
(deftest make-upper-triangular-matrix (triangular-matrix-core-test)
  ;; A default upper triangular matrix
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:upper-triangular-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-rational-equal
     (make-array '(10 10) :initial-element 0)
     matrix))
  ;; Specify the upper triangular matrix element type
  (let* ((data '((1.1 1.2 1.3 1.4)
                 (0.0 2.2 2.3 2.4)
                 (0.0 0.0 3.3 3.4)
                 (0.0 0.0 0.0 4.4))) 
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:upper-triangular-matrix
                  :element-type 'single-float
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-eq (array-element-type
                (linear-algebra::contents matrix))
               (array-element-type
                (make-array '(4 4) :element-type 'single-float)))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the upper triangular matrix initial element
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-element 1.0)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-float-equal
     (upper-triangular-array)
     matrix))
  ;; Specify the upper triangular matrix contents - Nested list
  (let* ((data '((1.1 1.2 1.3 1.4)
                 (0.0 2.2 2.3 2.4)
                 (0.0 0.0 3.3 3.4)
                 (0.0 0.0 0.0 4.4))) 
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:upper-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the upper-triangular matrix contents - Nested vector
  (let* ((data #(#(1.1 1.2 1.3 1.4)
                 #(0.0 2.2 2.3 2.4)
                 #(0.0 0.0 3.3 3.4)
                 #(0.0 0.0 0.0 4.4)))
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:upper-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the upper triangular matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents
                           '((1.1 1.2 1.3 1.4)
                             (0.0 2.2 2.3 2.4)
                             (0.0 0.0 3.3 3.4)
                             (0.0 0.0 0.0 4.4))))
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:upper-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:upper-triangular-matrix))
    (assert-float-equal data matrix))
  ;; Erroneous 2D array input data
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 4
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-contents
                 #3A(((1.1 1.2) (2.1 2.2))
                     ((3.1 3.2) (4.1 4.2))
                     ((5.1 5.2) (6.1 6.2)))))
  (assert-error 'error
                (linear-algebra:make-matrix
                 3 4
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-contents
                 (upper-triangular-array 0 4)))
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 3
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-contents
                 (upper-triangular-array 0 4)))
  (assert-error 'error
                (linear-algebra:make-matrix
                 5 5
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-contents
                 (coordinate-array 0 0 5 5)))
  ;; Specify initial element and initial contents
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 4
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-element 1.1
                 :initial-contents
                 (upper-triangular-array 0 4))))

;;; Lower triangular matrix construction tests
(deftest make-lower-triangular-matrix (triangular-matrix-core-test)
  ;; A default lower triangular matrix
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:lower-triangular-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-rational-equal
     (make-array '(10 10) :initial-element 0)
     matrix))
  ;; Specify the lower triangular matrix element type
  (let* ((data '((1.1 0.0 0.0 0.0)
                 (1.2 2.2 0.0 0.0)
                 (1.3 2.3 3.3 0.0)
                 (1.4 2.4 3.4 4.4))) 
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:lower-triangular-matrix
                  :element-type 'single-float
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-eq (array-element-type
                (linear-algebra::contents matrix))
               (array-element-type
                (make-array '(4 4) :element-type 'single-float)))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the lower triangular matrix initial element
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-element 1.0)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-float-equal
     (lower-triangular-array)
     matrix))
  ;; Specify the lower triangular matrix contents - Nested list
  (let* ((data '((1.1 0.0 0.0 0.0)
                 (1.2 2.2 0.0 0.0)
                 (1.3 2.3 3.3 0.0)
                 (1.4 2.4 3.4 4.4))) 
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:lower-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the lower-triangular matrix contents - Nested vector
  (let* ((data #(#(1.1 0.0 0.0 0.0)
                 #(1.2 2.2 0.0 0.0)
                 #(1.3 2.3 3.3 0.0)
                 #(1.4 2.4 3.4 4.4)))
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:lower-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-float-equal
     (make-array '(4 4) :initial-contents data)
     matrix))
  ;; Specify the lower triangular matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents
                           '((1.1 0.0 0.0 0.0)
                             (1.2 2.2 0.0 0.0)
                             (1.3 2.3 3.3 0.0)
                             (1.4 2.4 3.4 4.4))))
         (matrix (linear-algebra:make-matrix
                  4 4
                  :matrix-type 'linear-algebra:lower-triangular-matrix
                  :initial-contents data)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:lower-triangular-matrix))
    (assert-float-equal data matrix))
  ;; Erroneous 2D array input data
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 4
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-contents
                 #3A(((1.1 1.2) (2.1 2.2))
                     ((3.1 3.2) (4.1 4.2))
                     ((5.1 5.2) (6.1 6.2)))))
  (assert-error 'error
                (linear-algebra:make-matrix
                 3 4
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-contents
                 (lower-triangular-array 0 4)))
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 3
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-contents
                 (lower-triangular-array 0 4)))
  (assert-error 'error
                (linear-algebra:make-matrix
                 5 5
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-contents
                 (coordinate-array 0 0 5 5)))
  ;; Specify initial element and initial contents
  (assert-error 'error
                (linear-algebra:make-matrix
                 4 4
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-element 1.1
                 :initial-contents
                 (lower-triangular-array 0 4))))

;;; Test the upper triangular matrix predicate
(deftest upper-triangular-matrix-predicate (triangular-matrix-core-test)
  (assert-true
   (linear-algebra:upper-triangular-matrix-p
    (linear-algebra:make-matrix
     10 10 :matrix-type 'linear-algebra:upper-triangular-matrix)))
  (assert-false
   (linear-algebra:upper-triangular-matrix-p (make-array '(10 10)))))

;;; Test the lower triangular matrix predicate
(deftest lower-triangular-matrix-predicate (triangular-matrix-core-test)
  (assert-true
   (linear-algebra:lower-triangular-matrix-p
    (linear-algebra:make-matrix
     10 10 :matrix-type 'linear-algebra:lower-triangular-matrix)))
  (assert-false
   (linear-algebra:lower-triangular-matrix-p (make-array '(10 10)))))

;;; Test the upper triangular matrix bounds
(deftest upper-triangular-matrix-in-bounds-p (triangular-matrix-core-test)
  (test-matrix-in-bounds-p
   'linear-algebra:upper-triangular-matrix))

;;; Test the lower triangular matrix bounds
(deftest lower-triangular-matrix-in-bounds-p (triangular-matrix-core-test)
  (test-matrix-in-bounds-p
   'linear-algebra:lower-triangular-matrix))

;;; Test the upper triangular matrix element type
(deftest upper-triangular-matrix-element-type (triangular-matrix-core-test)
  (test-matrix-element-type
   'linear-algebra:upper-triangular-matrix))

;;; Test the lower triangular matrix element type
(deftest lower-triangular-matrix-element-type (triangular-matrix-core-test)
  (test-matrix-element-type
   'linear-algebra:lower-triangular-matrix))

;;; Test the upper triangular matrix dimensions
(deftest upper-triangular-matrix-dimensions (triangular-matrix-core-test)
  (test-matrix-dimensions
   'linear-algebra:upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix dimensions
(deftest lower-triangular-matrix-dimensions (triangular-matrix-core-test)
  (test-matrix-dimensions
   'linear-algebra:lower-triangular-matrix 9 9))

;;; Test the upper triangular matrix row dimension 
(deftest upper-triangular-matrix-row-dimension (triangular-matrix-core-test)
  (test-matrix-row-dimension
   'linear-algebra:upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix row dimension
(deftest lower-triangular-matrix-row-dimension (triangular-matrix-core-test)
  (test-matrix-row-dimension
   'linear-algebra:lower-triangular-matrix 9 9))

;;; Test the upper triangular matrix column dimension
(deftest upper-triangular-matrix-column-dimension (triangular-matrix-core-test)
  (test-matrix-column-dimension
   'linear-algebra:upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix column dimension
(deftest lower-triangular-matrix-column-dimension (triangular-matrix-core-test)
  (test-matrix-column-dimension
   'linear-algebra:lower-triangular-matrix 9 9))

;;; Reference upper triangular matrix elements
(deftest upper-triangular-matrix-mref (triangular-matrix-core-test)
  (let* ((initial-contents
          '((1.1 1.2 1.3 1.4 1.5)
            (0.0 2.2 2.3 2.4 2.5)
            (0.0 0.0 3.3 3.4 3.5)
            (0.0 0.0 0.0 4.4 4.5)
            (0.0 0.0 0.0 0.0 5.5)))
         (rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((> i0 rowi) i0)))
         (data (make-array
                (list rows columns)
                :initial-contents
                initial-contents))
         (matrix (linear-algebra:make-matrix
                  rows columns
                  :matrix-type
                  'linear-algebra:upper-triangular-matrix
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
     (aref data rend cend)
     (linear-algebra:mref matrix rend cend))
    (assert-float-equal
     (aref data rowi coli)
     (linear-algebra:mref matrix rowi coli))
    (assert-float-equal
     0.0
     (linear-algebra:mref matrix coli rowi))))

;;; Reference lower triangular matrix elements
(deftest lower-triangular-matrix-mref (triangular-matrix-core-test)
  (let* ((initial-contents
          '((1.1 0.0 0.0 0.0 0.0)
            (1.2 2.2 0.0 0.0 0.0)
            (1.3 2.3 3.3 0.0 0.0)
            (1.4 2.4 3.4 4.4 0.0)
            (1.5 2.5 3.5 4.5 5.5)))
         (rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((< i0 rowi) i0)))
         (data (make-array
                (list rows columns)
                :initial-contents
                initial-contents))
         (matrix (linear-algebra:make-matrix
                  rows columns
                  :matrix-type
                  'linear-algebra:lower-triangular-matrix
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
     (aref data rend cend)
     (linear-algebra:mref matrix rend cend))
    (assert-float-equal
     (aref data rowi coli)
     (linear-algebra:mref matrix rowi coli))
    (assert-float-equal
     0.0
     (linear-algebra:mref matrix coli rowi))))

;;; Set upper triangular matrix elements
(deftest upper-triangular-matrix-setf-mref (triangular-matrix-core-test)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((> i0 rowi) i0)))
         (matrix (linear-algebra:make-matrix
                  rows columns
                  :matrix-type
                  'linear-algebra:upper-triangular-matrix
                  :initial-contents
                  '((1.1 1.2 1.3 1.4 1.5)
                    (0.0 2.2 2.3 2.4 2.5)
                    (0.0 0.0 3.3 3.4 3.5)
                    (0.0 0.0 0.0 4.4 4.5)
                    (0.0 0.0 0.0 0.0 5.5)))))
    (destructuring-bind (val1 val2 val3 val4)
        (make-random-list 4 1.0)
      (setf (linear-algebra:mref matrix 0 0)       val1)
      (setf (linear-algebra:mref matrix 0 cend)    val2)
      (setf (linear-algebra:mref matrix rend cend) val3)
      (setf (linear-algebra:mref matrix rowi coli) val4)
      (assert-float-equal val1 (linear-algebra:mref matrix 0 0))
      (assert-float-equal val2 (linear-algebra:mref matrix 0 cend))
      (assert-float-equal 0.0  (linear-algebra:mref matrix rend 0))
      (assert-float-equal val3 (linear-algebra:mref matrix rend cend))
      (assert-float-equal val4 (linear-algebra:mref matrix rowi coli))
      (assert-float-equal 0.0  (linear-algebra:mref matrix coli rowi))
      (assert-error
       'error
       (setf (linear-algebra:mref matrix coli rowi) 1.0)))))

;;; Set lower triangular matrix elements
(deftest lower-triangular-matrix-setf-mref (triangular-matrix-core-test)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((< i0 rowi) i0)))
         (matrix (linear-algebra:make-matrix
                  rows columns
                  :matrix-type
                  'linear-algebra:lower-triangular-matrix
                  :initial-contents
                  '((1.1 0.0 0.0 0.0 0.0)
                    (1.2 2.2 0.0 0.0 0.0)
                    (1.3 2.3 3.3 0.0 0.0)
                    (1.4 2.4 3.4 4.4 0.0)
                    (1.5 2.5 3.5 4.5 5.5)))))
    (destructuring-bind (val1 val2 val3 val4)
        (make-random-list 4 1.0)
      (setf (linear-algebra:mref matrix 0 0)       val1)
      (setf (linear-algebra:mref matrix rend 0)    val2)
      (setf (linear-algebra:mref matrix rend cend) val3)
      (setf (linear-algebra:mref matrix rowi coli) val4)
      (assert-float-equal val1 (linear-algebra:mref matrix 0 0))
      (assert-float-equal 0.0  (linear-algebra:mref matrix 0 cend))
      (assert-float-equal val2 (linear-algebra:mref matrix rend 0))
      (assert-float-equal val3 (linear-algebra:mref matrix rend cend))
      (assert-float-equal val4 (linear-algebra:mref matrix rowi coli))
      (assert-float-equal 0.0  (linear-algebra:mref matrix coli rowi))
      (assert-error
       'error
       (setf (linear-algebra:mref matrix coli rowi) 1.0)))))

;;; Copy the upper triangular matrix
(deftest copy-upper-triangular-matrix (triangular-matrix-core-test)
  (let ((matrix (linear-algebra:make-matrix
                 5 5 :element-type 'single-float
                 :matrix-type
                 'linear-algebra:upper-triangular-matrix
                 :initial-contents
                 (upper-triangular-array 0 5))))
    (assert-true
     (linear-algebra:upper-triangular-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-float-equal
     matrix (linear-algebra:copy-matrix matrix))))

;;; Copy the lower triangular matrix
(deftest copy-lower-triangular-matrix (triangular-matrix-core-test)
  (let ((matrix (linear-algebra:make-matrix
                 5 5 :element-type 'single-float
                 :matrix-type
                 'linear-algebra:lower-triangular-matrix
                 :initial-contents
                 (lower-triangular-array 0 5))))
    (assert-true
     (linear-algebra:lower-triangular-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-float-equal
     matrix (linear-algebra:copy-matrix matrix))))

;;; Test the submatrix of an upper triangular matrix
(deftest upper-triangular-submatrix (triangular-matrix-core-test)
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:upper-triangular-matrix
                 :initial-contents (upper-triangular-array)))
        (submat (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:dense-matrix
                 :initial-contents (upper-triangular-array))))
    ;; The entire matrix
    (assert-float-equal
     (upper-triangular-array)
     (linear-algebra:submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-float-equal
     (upper-triangular-array 3)
     (linear-algebra:submatrix matrix 3 3))
    ;; End row and column
    (assert-float-equal
     (upper-triangular-array 3 5)
     (linear-algebra:submatrix
      matrix 3 3 :row-end 5 :column-end 5))
    ;; Submatrix is a dense matrix
    (assert-true
     (typep (linear-algebra:submatrix matrix 1 2)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 2)
     (linear-algebra:submatrix matrix 1 2)
     (array-error
      (linear-algebra::contents
       (linear-algebra:submatrix submat 1 2))
      (linear-algebra::contents
       (linear-algebra:submatrix matrix 1 2))))
    (assert-true
     (typep (linear-algebra:submatrix matrix 1 1 :row-end 5)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :row-end 5)
     (linear-algebra:submatrix matrix 1 1 :row-end 5))
    (assert-true
     (typep (linear-algebra:submatrix matrix 1 1 :column-end 8)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :column-end 8)
     (linear-algebra:submatrix matrix 1 1 :column-end 8))
    ;; Start row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 11 5))
    ;; Start column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :row-end 11))
    ;; End column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :column-end 11))
    ;; Start row exceeds end row
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :row-end 6))
    ;; Start column exceeds end column
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :column-end 6))))

;;; Test the submatrix of an lower triangular matrix
(deftest lower-triangular-submatrix (triangular-matrix-core-test)
  (let ((matrix (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:lower-triangular-matrix
                 :initial-contents (lower-triangular-array)))
        (submat (linear-algebra:make-matrix
                 10 10
                 :matrix-type 'linear-algebra:dense-matrix
                 :initial-contents (lower-triangular-array))))
    ;; The entire matrix
    (assert-float-equal
     (lower-triangular-array)
     (linear-algebra:submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-float-equal
     (lower-triangular-array 3)
     (linear-algebra:submatrix matrix 3 3))
    ;; End row and column
    (assert-float-equal
     (lower-triangular-array 3 5)
     (linear-algebra:submatrix
      matrix 3 3 :row-end 5 :column-end 5))
    ;; Submatrix is a dense matrix
    (assert-true
     (typep (linear-algebra:submatrix matrix 2 1)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 2 1)
     (linear-algebra:submatrix matrix 2 1)
     (array-error
      (linear-algebra::contents
       (linear-algebra:submatrix submat 2 1))
      (linear-algebra::contents
       (linear-algebra:submatrix matrix 2 1))))
    (assert-true
     (typep (linear-algebra:submatrix matrix 1 1 :row-end 5)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :row-end 5)
     (linear-algebra:submatrix matrix 1 1 :row-end 5))
    (assert-true
     (typep (linear-algebra:submatrix matrix 1 1 :column-end 8)
            'linear-algebra:dense-matrix))
    (assert-float-equal
     (linear-algebra:submatrix submat 1 1 :column-end 8)
     (linear-algebra:submatrix matrix 1 1 :column-end 8))
    ;; Start row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 11 5))
    ;; Start column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :row-end 11))
    ;; End column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :column-end 11))
    ;; Start row exceeds end row
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :row-end 6))
    ;; Start column exceeds end column
    (assert-error
     'error (linear-algebra:submatrix matrix 7 7 :column-end 6))))

;;; Set the submatrix of an upper triangular matrix
(deftest setf-upper-triangular-submatrix (triangular-matrix-core-test)
  ;; Upper left submatrix
  (let ((array-ul (make-array
                   '(5 5) :initial-contents
                   '((1.0 1.0 1.0 0.0 0.0)
                     (0.0 1.0 1.0 0.0 0.0)
                     (0.0 0.0 1.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 0 0)
      (upper-triangular-matrix 0 3)))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 0 0 :row-end 3 :column-end 3)
      (upper-triangular-matrix 0 10))))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 1.0 1.0 1.0)
      (0.0 0.0 0.0 1.0 1.0)
      (0.0 0.0 0.0 0.0 1.0)))
   (setf-submatrix
    5 5 'linear-algebra:upper-triangular-matrix
    (linear-algebra:submatrix matrix 2 2)
    (upper-triangular-matrix)))
  ;; Middle submatrix
  (let ((array-mid (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 1.0 0.0)
                      (0.0 0.0 1.0 1.0 0.0)
                      (0.0 0.0 0.0 1.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 1 1)
      (upper-triangular-matrix 1 4)))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 1 1 :row-end 4 :column-end 4)
      (upper-triangular-matrix 1))))
  ;; Above diagonal submatrix
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 0.0 1.0 1.0)
                      (0.0 0.0 0.0 0.0 1.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 0 2)
      (upper-triangular-matrix 0 3)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 0 2 :row-end 3)
      (upper-triangular-matrix))))
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 1 2)
      (linear-algebra:submatrix (unit-matrix 3 3)
                                0 0 :row-end 2)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:upper-triangular-matrix
      (linear-algebra:submatrix matrix 1 2 :row-end 3)
      (unit-matrix 3 3))))
  ;; Non upper-triangular subsets
  (assert-error
   'error
   (setf (linear-algebra:submatrix
          (zero-matrix 5 5 :matrix-type
                       'linear-algebra:upper-triangular-matrix) 0 1)
         (unit-matrix 5 3))))

;;; Set the submatrix of a lower triangular matrix
(deftest setf-lower-triangular-submatrix (triangular-matrix-core-test)
  ;; Upper left submatrix
  (let ((array-ul (make-array
                   '(5 5) :initial-contents
                   '((1.0 0.0 0.0 0.0 0.0)
                     (1.0 1.0 0.0 0.0 0.0)
                     (1.0 1.0 1.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 0 0)
      (lower-triangular-matrix 0 3)))
    (assert-float-equal
     array-ul
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 0 0 :row-end 3 :column-end 3)
      (lower-triangular-matrix 0 10))))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 1.0 0.0 0.0)
      (0.0 0.0 1.0 1.0 0.0)
      (0.0 0.0 1.0 1.0 1.0)))
   (setf-submatrix
    5 5 'linear-algebra:lower-triangular-matrix
    (linear-algebra:submatrix matrix 2 2)
    (lower-triangular-matrix)))
  ;; Middle submatrix
  (let ((array-mid (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 1.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 1 1)
      (lower-triangular-matrix 1 4)))
    (assert-float-equal
     array-mid
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 1 1 :row-end 4 :column-end 4)
      (lower-triangular-matrix 1))))
  ;; Below diagonal submatrix
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (1.0 0.0 0.0 0.0 0.0)
                      (1.0 1.0 0.0 0.0 0.0)
                      (1.0 1.0 1.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 2 0)
      (lower-triangular-matrix 0 3)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 2 0 :column-end 3)
      (lower-triangular-matrix))))
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 2 1)
      (linear-algebra:submatrix (unit-matrix 3 3)
                                0 0 :column-end 2)))
    (assert-float-equal
     array-off
     (setf-submatrix
      5 5 'linear-algebra:lower-triangular-matrix
      (linear-algebra:submatrix matrix 2 1 :column-end 3)
      (unit-matrix 3 3))))
  ;; Non lower-triangular subsets
  (assert-error
   'error
   (setf (linear-algebra:submatrix
          (zero-matrix 5 5 :matrix-type
                       'linear-algebra:lower-triangular-matrix) 1 0)
         (unit-matrix 3 5))))

;;; Replace all or part of an upper triangular matrix
(deftest upper-triangular-matrix-replace (triangular-matrix-core-test)
  ;; Replace the entire matrix
  (assert-float-equal
   (upper-triangular-matrix)
   (linear-algebra:replace-matrix
    (zero-matrix 10 10 :matrix-type 'linear-algebra:upper-triangular-matrix)
    (upper-triangular-matrix)))
  ;; Upper left submatrix
  (let ((array-ul (make-array
                   '(5 5) :initial-contents
                   '((1.0 1.0 1.0 0.0 0.0)
                     (0.0 1.0 1.0 0.0 0.0)
                     (0.0 0.0 1.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix 0 3)))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1-end 3 :column1-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row2-end 3 :column1-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1-end 3 :column2-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row2-end 3 :column2-end 3)))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 1.0 1.0 1.0)
      (0.0 0.0 0.0 1.0 1.0)
      (0.0 0.0 0.0 0.0 1.0)))
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
    (upper-triangular-matrix)
    :row1 2 :column1 2))
  ;; Middle submatrix
  (let ((array-mid (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 1.0 0.0)
                      (0.0 0.0 1.0 1.0 0.0)
                      (0.0 0.0 0.0 1.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix 0 3)
      :row1 1 :column1 1))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 1 :column1 1
      :row1-end 4 :column1-end 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 1 :column1 1
      :row2-end 3 :column1-end 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 1 :column1 1
      :row1-end 4 :column2-end 3))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 1 :column1 1
      :row2-end 3 :column2-end 3)))
  ;; Above diagonal submatrix
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 0.0 1.0 1.0)
                      (0.0 0.0 0.0 0.0 1.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix 0 3)
      :row1 0 :column1 2))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 0 :column1 2
      :row1-end 3))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (upper-triangular-matrix)
      :row1 0 :column1 2
      :row2-end 3)))
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 1.0 1.0 1.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (unit-matrix 3 3)
      :row1 1 :column1 2 :row2-end 2))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
      (unit-matrix 10 10)
      :row1 1 :column1 2 :row1-end 3)))
  ;; Non upper triangular subsets
  (assert-error
   'error
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:upper-triangular-matrix)
    (unit-matrix 5 3)
    :column1 1)))

;;; Replace all or part of a lower triangular matrix
(deftest lower-triangular-matrix-replace (triangular-matrix-core-test)
  ;; Replace the entire matrix
  (assert-float-equal
   (lower-triangular-matrix)
   (linear-algebra:replace-matrix
    (zero-matrix 10 10 :matrix-type 'linear-algebra:lower-triangular-matrix)
    (lower-triangular-matrix)))
  ;; Upper left submatrix
  (let ((array-ul (make-array
                   '(5 5) :initial-contents
                   '((1.0 0.0 0.0 0.0 0.0)
                     (1.0 1.0 0.0 0.0 0.0)
                     (1.0 1.0 1.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)
                     (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix 0 3)))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1-end 3 :column1-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row2-end 3 :column1-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1-end 3 :column2-end 3))
    (assert-float-equal
     array-ul
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row2-end 3 :column2-end 3)))
  ;; Lower right submatrix
  (assert-float-equal
   (make-array
    '(5 5) :initial-contents
    '((0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 0.0 0.0 0.0)
      (0.0 0.0 1.0 0.0 0.0)
      (0.0 0.0 1.0 1.0 0.0)
      (0.0 0.0 1.0 1.0 1.0)))
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
    (lower-triangular-matrix)
    :row1 2 :column1 2))
  ;; Middle submatrix
  (let ((array-mid (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 1.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)))))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix 0 3)
      :row1 1 :column1 1))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 1 :column1 1
      :row1-end 4 :column1-end 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 1 :column1 1
      :row2-end 3 :column1-end 4))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 1 :column1 1
      :row1-end 4 :column2-end 3))
    (assert-float-equal
     array-mid
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 1 :column1 1
      :row2-end 3 :column2-end 3)))
  ;; Below diagonal submatrix
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (1.0 0.0 0.0 0.0 0.0)
                      (1.0 1.0 0.0 0.0 0.0)
                      (1.0 1.0 1.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix 0 3)
      :row1 2 :column1 0))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 2 :column1 0
      :column1-end 3))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (lower-triangular-matrix)
      :row1 2 :column1 0
      :column2-end 3)))
  (let ((array-off (make-array
                    '(5 5) :initial-contents
                    '((0.0 0.0 0.0 0.0 0.0)
                      (0.0 0.0 0.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)
                      (0.0 1.0 1.0 0.0 0.0)))))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (unit-matrix 3 3)
      :row1 2 :column1 1 :column2-end 2))
    (assert-float-equal
     array-off
     (linear-algebra:replace-matrix
      (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
      (unit-matrix 10 10)
      :row1 2 :column1 1 :column1-end 3)))
  ;; Non lower triangular subsets
  (assert-error
   'error
   (linear-algebra:replace-matrix
    (zero-matrix 5 5 :matrix-type 'linear-algebra:lower-triangular-matrix)
    (unit-matrix 5 3)
    :row1 1)))

;;; Validate a range for an upper triangular matrix.
(deftest upper-triangular-matrix-validated-range (triangular-matrix-core-test)
  (test-matrix-validated-range
   'linear-algebra:upper-triangular-matrix 10 10))

;;; Validate a range for an lower triangular matrix.
(deftest lower-triangular-matrix-validated-range (triangular-matrix-core-test)
  (test-matrix-validated-range
   'linear-algebra:lower-triangular-matrix 10 10))

