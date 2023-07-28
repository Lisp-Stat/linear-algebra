;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite triangular-matrix (core))

(defun upper-triangular-matrix (&optional (start 0) (end 10))
  (make-matrix (- end start) (- end start) :matrix-type 'upper-triangular-matrix
					   :element-type 'single-float
					   :initial-contents (upper-triangular-array start end)))

(defun lower-triangular-matrix (&optional (start 0) (end 10))
  (make-matrix (- end start) (- end start) :matrix-type 'lower-triangular-matrix
					   :element-type 'single-float
					   :initial-contents (lower-triangular-array start end)))

;;; Upper triangular matrix construction tests
(deftest make-upper-triangular-matrix (triangular-matrix)
  ;; A default upper triangular matrix
  (let ((matrix (make-matrix 10 10 :matrix-type 'upper-triangular-matrix)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-num= (make-array '(10 10) :initial-element 0) matrix))
  ;; Specify the upper triangular matrix element type
  (let* ((data '((1.1 1.2 1.3 1.4)
                 (0.0 2.2 2.3 2.4)
                 (0.0 0.0 3.3 3.4)
                 (0.0 0.0 0.0 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'upper-triangular-matrix :element-type 'single-float :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-eq (array-element-type (linear-algebra::contents matrix))
        (array-element-type (make-array '(4 4) :element-type 'single-float)))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the upper triangular matrix initial element
  (let ((matrix (make-matrix 10 10 :matrix-type 'upper-triangular-matrix :initial-element 1.0)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-num= matrix (upper-triangular-array)))

  ;; Specify the upper triangular matrix contents - Nested list
  (let* ((data '((1.1 1.2 1.3 1.4)
                 (0.0 2.2 2.3 2.4)
                 (0.0 0.0 3.3 3.4)
                 (0.0 0.0 0.0 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'upper-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the upper-triangular matrix contents - Nested vector
  (let* ((data #(#(1.1 1.2 1.3 1.4)
                 #(0.0 2.2 2.3 2.4)
                 #(0.0 0.0 3.3 3.4)
                 #(0.0 0.0 0.0 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'upper-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the upper triangular matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
						      (0.0 2.2 2.3 2.4)
						      (0.0 0.0 3.3 3.4)
						      (0.0 0.0 0.0 4.4))))
         (matrix (make-matrix 4 4 :matrix-type 'upper-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'upper-triangular-matrix))
    (assert-num= data matrix))

  ;; Erroneous 2D array input data
  (assert-condition error (make-matrix 4 4 :matrix-type 'upper-triangular-matrix
					   :initial-contents #3A(((1.1 1.2) (2.1 2.2))
								 ((3.1 3.2) (4.1 4.2))
								 ((5.1 5.2) (6.1 6.2)))))
  (assert-condition error (make-matrix 3 4 :matrix-type 'upper-triangular-matrix
					   :initial-contents (upper-triangular-array 0 4)))
  (assert-condition error (make-matrix 4 3 :matrix-type 'upper-triangular-matrix
					   :initial-contents (upper-triangular-array 0 4)))
  ;; (assert-condition error (make-matrix 5 5 :matrix-type 'upper-triangular-matrix :initial-contents (coordinate-array 0 0 5 5))) ;make-matrix never raises an error

  ;; Specify initial element and initial contents
  (assert-condition error (make-matrix 4 4 :matrix-type 'upper-triangular-matrix :initial-element 1.1
					   :initial-contents (upper-triangular-array 0 4))))

;;; Lower triangular matrix construction tests
(deftest make-lower-triangular-matrix (triangular-matrix)
  ;; A default lower triangular matrix
  (let ((matrix (make-matrix 10 10 :matrix-type 'lower-triangular-matrix)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-num= (make-array '(10 10) :initial-element 0) matrix))

  ;; Specify the lower triangular matrix element type
  (let* ((data '((1.1 0.0 0.0 0.0)
                 (1.2 2.2 0.0 0.0)
                 (1.3 2.3 3.3 0.0)
                 (1.4 2.4 3.4 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'lower-triangular-matrix :element-type 'single-float
				  :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-eq (array-element-type (linear-algebra::contents matrix))
        (array-element-type (make-array '(4 4) :element-type 'single-float)))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the lower triangular matrix initial element
  (let ((matrix (make-matrix 10 10 :matrix-type 'lower-triangular-matrix :initial-element 1.0)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-num= matrix (lower-triangular-array)))

  ;; Specify the lower triangular matrix contents - Nested list
  (let* ((data '((1.1 0.0 0.0 0.0)
                 (1.2 2.2 0.0 0.0)
                 (1.3 2.3 3.3 0.0)
                 (1.4 2.4 3.4 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'lower-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the lower-triangular matrix contents - Nested vector
  (let* ((data #(#(1.1 0.0 0.0 0.0)
                 #(1.2 2.2 0.0 0.0)
                 #(1.3 2.3 3.3 0.0)
                 #(1.4 2.4 3.4 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'lower-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))

  ;; Specify the lower triangular matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents '((1.1 0.0 0.0 0.0)
						      (1.2 2.2 0.0 0.0)
						      (1.3 2.3 3.3 0.0)
						      (1.4 2.4 3.4 4.4))))
         (matrix (make-matrix 4 4 :matrix-type 'lower-triangular-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'lower-triangular-matrix))
    (assert-num= data matrix))

  ;; Erroneous 2D array input data
  (assert-condition error (make-matrix 4 4 :matrix-type 'lower-triangular-matrix
					   :initial-contents #3A(((1.1 1.2) (2.1 2.2))
								 ((3.1 3.2) (4.1 4.2))
								 ((5.1 5.2) (6.1 6.2)))))
  (assert-condition error (make-matrix 3 4 :matrix-type 'lower-triangular-matrix
					   :initial-contents (lower-triangular-array 0 4)))
  (assert-condition error (make-matrix 4 3 :matrix-type 'lower-triangular-matrix
					   :initial-contents (lower-triangular-array 0 4)))
  ;; (assert-condition error (make-matrix 5 5 :matrix-type 'lower-triangular-matrix :initial-contents (coordinate-array 0 0 5 5)))

  ;; Specify initial element and initial contents
  (assert-condition error (make-matrix 4 4 :matrix-type 'lower-triangular-matrix :initial-element 1.1
					   :initial-contents (lower-triangular-array 0 4))))

;;; Test the upper triangular matrix predicate
(deftest upper-triangular-matrix-predicate (triangular-matrix)
  (assert-true (upper-triangular-matrix-p (make-matrix 10 10 :matrix-type 'upper-triangular-matrix)))
  (assert-false (upper-triangular-matrix-p (make-array '(10 10)))))

;;; Test the lower triangular matrix predicate
(deftest lower-triangular-matrix-predicate (triangular-matrix)
  (assert-true (lower-triangular-matrix-p (make-matrix 10 10 :matrix-type 'lower-triangular-matrix)))
  (assert-false (lower-triangular-matrix-p (make-array '(10 10)))))

;;; Test the upper triangular matrix bounds
(deftest upper-triangular-matrix-in-bounds-p (triangular-matrix)
  (test-matrix-in-bounds-p 'upper-triangular-matrix))

;;; Test the lower triangular matrix bounds
(deftest lower-triangular-matrix-in-bounds-p (triangular-matrix)
  (test-matrix-in-bounds-p 'lower-triangular-matrix))

;;; Test the upper triangular matrix element type
(deftest upper-triangular-matrix-element-type (triangular-matrix)
  (test-matrix-element-type 'upper-triangular-matrix))

;;; Test the lower triangular matrix element type
(deftest lower-triangular-matrix-element-type (triangular-matrix)
  (test-matrix-element-type 'lower-triangular-matrix))

;;; Test the upper triangular matrix dimensions
(deftest upper-triangular-matrix-dimensions (triangular-matrix)
  (test-matrix-dimensions 'upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix dimensions
(deftest lower-triangular-matrix-dimensions (triangular-matrix)
  (test-matrix-dimensions 'lower-triangular-matrix 9 9))

;;; Test the upper triangular matrix row dimension
(deftest upper-triangular-matrix-row-dimension (triangular-matrix)
  (test-matrix-row-dimension 'upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix row dimension
(deftest lower-triangular-matrix-row-dimension (triangular-matrix)
  (test-matrix-row-dimension 'lower-triangular-matrix 9 9))

;;; Test the upper triangular matrix column dimension
(deftest upper-triangular-matrix-column-dimension (triangular-matrix)
  (test-matrix-column-dimension 'upper-triangular-matrix 9 9))

;;; Test the lower triangular matrix column dimension
(deftest lower-triangular-matrix-column-dimension (triangular-matrix)
  (test-matrix-column-dimension 'lower-triangular-matrix 9 9))

;;; Reference upper triangular matrix elements
(deftest upper-triangular-matrix-mref (triangular-matrix)
  (let* ((initial-contents '((1.1 1.2 1.3 1.4 1.5)
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
         (data (make-array (list rows columns) :initial-contents initial-contents))
         (matrix (make-matrix rows columns :matrix-type 'upper-triangular-matrix :initial-contents initial-contents)))
    (assert-num= (aref data 0 0) (mref matrix 0 0))
    (assert-num= (aref data 0 cend) (mref matrix 0 cend))
    (assert-num= (aref data rend 0) (mref matrix rend 0))
    (assert-num= (aref data rend cend) (mref matrix rend cend))
    (assert-num= (aref data rowi coli) (mref matrix rowi coli))
    (assert-num= 0.0 (mref matrix coli rowi))))

;;; Reference lower triangular matrix elements
(deftest lower-triangular-matrix-mref (triangular-matrix)
  (let* ((initial-contents '((1.1 0.0 0.0 0.0 0.0)
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
         (data (make-array (list rows columns) :initial-contents initial-contents))
         (matrix (make-matrix rows columns :matrix-type 'lower-triangular-matrix
					   :initial-contents initial-contents)))
    (assert-num= (aref data 0 0) (mref matrix 0 0))
    (assert-num= (aref data 0 cend) (mref matrix 0 cend))
    (assert-num= (aref data rend 0) (mref matrix rend 0))
    (assert-num= (aref data rend cend) (mref matrix rend cend))
    (assert-num= (aref data rowi coli) (mref matrix rowi coli))
    (assert-num= 0.0 (mref matrix coli rowi))))

;;; Set upper triangular matrix elements
(deftest upper-triangular-matrix-setf-mref (triangular-matrix)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((> i0 rowi) i0)))
         (matrix (make-matrix rows columns :matrix-type 'upper-triangular-matrix
					   :initial-contents '((1.1 1.2 1.3 1.4 1.5)
							       (0.0 2.2 2.3 2.4 2.5)
							       (0.0 0.0 3.3 3.4 3.5)
							       (0.0 0.0 0.0 4.4 4.5)
							       (0.0 0.0 0.0 0.0 5.5)))))
    (destructuring-bind (val1 val2 val3 val4) (make-random-list 4 1.0)
      (setf (mref matrix 0 0)       val1)
      (setf (mref matrix 0 cend)    val2)
      (setf (mref matrix rend cend) val3)
      (setf (mref matrix rowi coli) val4)
      (assert-num= val1 (mref matrix 0 0))
      (assert-num= val2 (mref matrix 0 cend))
      (assert-num= 0.0  (mref matrix rend 0))
      (assert-num= val3 (mref matrix rend cend))
      (assert-num= val4 (mref matrix rowi coli))
      (assert-num= 0.0  (mref matrix coli rowi))
      (assert-condition error (setf (mref matrix coli rowi) 1.0)))))

;;; Set lower triangular matrix elements
(deftest lower-triangular-matrix-setf-mref (triangular-matrix)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random columns)
                        (random columns)))
                   ((< i0 rowi) i0)))
         (matrix (make-matrix rows columns :matrix-type 'lower-triangular-matrix
					   :initial-contents '((1.1 0.0 0.0 0.0 0.0)
							       (1.2 2.2 0.0 0.0 0.0)
							       (1.3 2.3 3.3 0.0 0.0)
							       (1.4 2.4 3.4 4.4 0.0)
							       (1.5 2.5 3.5 4.5 5.5)))))
    (destructuring-bind (val1 val2 val3 val4) (make-random-list 4 1.0)
      (setf (mref matrix 0 0)       val1)
      (setf (mref matrix rend 0)    val2)
      (setf (mref matrix rend cend) val3)
      (setf (mref matrix rowi coli) val4)
      (assert-num= val1 (mref matrix 0 0))
      (assert-num= 0.0  (mref matrix 0 cend))
      (assert-num= val2 (mref matrix rend 0))
      (assert-num= val3 (mref matrix rend cend))
      (assert-num= val4 (mref matrix rowi coli))
      (assert-num= 0.0  (mref matrix coli rowi))
      (assert-condition error (setf (mref matrix coli rowi) 1.0)))))

;;; Copy the upper triangular matrix
(deftest copy-upper-triangular-matrix (triangular-matrix)
  (let ((matrix (make-matrix 5 5 :element-type 'single-float :matrix-type 'upper-triangular-matrix
				 :initial-contents (upper-triangular-array 0 5))))
    (assert-true (upper-triangular-matrix-p (copy-matrix matrix)))
    (assert-false (eq matrix (copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (copy-matrix matrix))))
    (assert-num= matrix (copy-matrix matrix))))

;;; Copy the lower triangular matrix
(deftest copy-lower-triangular-matrix (triangular-matrix)
  (let ((matrix (make-matrix 5 5 :element-type 'single-float :matrix-type 'lower-triangular-matrix
				 :initial-contents (lower-triangular-array 0 5))))
    (assert-true (lower-triangular-matrix-p (copy-matrix matrix)))
    (assert-false (eq matrix (copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (copy-matrix matrix))))
    (assert-num= matrix (copy-matrix matrix))))

(defgeneric submatrix (matrix start-row start-column &key end-row end-column)
  (:documentation "Return a submatrix of the matrix."))

;;; Test the submatrix of an upper triangular matrix
(deftest upper-triangular-submatrix (triangular-matrix)
  (let ((matrix (make-matrix 10 10 :matrix-type 'upper-triangular-matrix :initial-contents (upper-triangular-array)))
        (submat (make-matrix 10 10 :matrix-type 'dense-matrix :initial-contents (upper-triangular-array))))
    (assert-num= (upper-triangular-array) (submatrix matrix 0 0)) ;entire matrix
    (assert-num= (upper-triangular-array 3) (submatrix matrix 3 3)) ;start row and column to the end
    (assert-num= (upper-triangular-array 3 5) (submatrix matrix 3 3 :row-end 5 :column-end 5)) ;end row and column
    (assert-true (typep (submatrix matrix 1 2) 'dense-matrix)) ;submatrix is a dense matrix
    #+nil
    (assert-num= (submatrix submat 1 2)	(submatrix matrix 1 2)
      (array-error
       (linear-algebra::contents (submatrix submat 1 2))
       (linear-algebra::contents (submatrix matrix 1 2))))
    (assert-true (typep (submatrix matrix 1 1 :end-row 5) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :row-end 5) (submatrix matrix 1 1 :row-end 5))
    (assert-true (typep (submatrix matrix 1 1 :end-column 8) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :end-column 8) (submatrix matrix 1 1 :end-column 8))
    (assert-condition error (submatrix matrix 11 5));start row exceeds dimensions
    (assert-condition error (submatrix matrix 5 11)) ;start column exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-row 11)) ;end row exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-column 11)) ;end column exceeds dimensions
    (assert-condition error (submatrix matrix 7 7 :end-row 6)) ;start row exceeds end row
    (assert-condition error (submatrix matrix 7 7 :end-column 6)))) ;start column exceeds end column

;;; Test the submatrix of an lower triangular matrix
(deftest lower-triangular-submatrix (triangular-matrix)
  (let ((matrix (make-matrix 10 10 :matrix-type 'lower-triangular-matrix :initial-contents (lower-triangular-array)))
        (submat (make-matrix 10 10 :matrix-type 'dense-matrix :initial-contents (lower-triangular-array))))
    (assert-num= (lower-triangular-array) (submatrix matrix 0 0)) ;the entire matrix
    (assert-num= (lower-triangular-array 3) (submatrix matrix 3 3)) ;start row and column to the end
    (assert-num= (lower-triangular-array 3 5) (submatrix matrix 3 3 :row-end 5 :end-column 5)) ;end row and column

    ;; Submatrix is a dense matrix
    (assert-true (typep (submatrix matrix 2 1) 'dense-matrix))
    #+nil
    (assert-num= (submatrix submat 2 1)
	(submatrix matrix 2 1)
      (array-error
       (linear-algebra::contents
	(submatrix submat 2 1))
       (linear-algebra::contents
	(submatrix matrix 2 1))))
    (assert-true (typep (submatrix matrix 1 1 :end-row 5) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :end-row 5) (submatrix matrix 1 1 :end-row 5))
    (assert-true (typep (submatrix matrix 1 1 :end-column 8) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :end-column 8) (submatrix matrix 1 1 :end-column 8))

    (assert-condition error (submatrix matrix 11 5)) ;start row exceeds dimensions
    (assert-condition error (submatrix matrix 5 11)) ;start column exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-row 11)) ;end row exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-column 11)) ;end column exceeds dimensions
    (assert-condition error (submatrix matrix 7 7 :end-row 6)) ;start row exceeds end row
    (assert-condition error (submatrix matrix 7 7 :end-column 6)))) ;start column exceeds end column

;;; Set the submatrix of an upper triangular matrix
(deftest setf-upper-triangular-submatrix (triangular-matrix)

  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((1.0 1.0 1.0 0.0 0.0)
							 (0.0 1.0 1.0 0.0 0.0)
							 (0.0 0.0 1.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 0 0) (upper-triangular-matrix 0 3)))
    (assert-num= array-ul
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 0 0 :end-row 3 :end-column 3) (upper-triangular-matrix 0 10))))

  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 1.0 1.0 1.0)
						      (0.0 0.0 0.0 1.0 1.0)
						      (0.0 0.0 0.0 0.0 1.0)))
      (setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 2 2) (upper-triangular-matrix)))

  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 1.0 0.0)
							  (0.0 0.0 1.0 1.0 0.0)
							  (0.0 0.0 0.0 1.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 1 1) (upper-triangular-matrix 1 4)))
    (assert-num= array-mid
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 1 1 :end-row 4 :end-column 4) (upper-triangular-matrix 1))))

  ;; Above diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 0.0 1.0 1.0)
							  (0.0 0.0 0.0 0.0 1.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 0 2) (upper-triangular-matrix 0 3)))
    (assert-num= array-off
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 0 2 :end-row 3) (upper-triangular-matrix))))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 1 2) (submatrix (unit-matrix 3 3) 0 0 :end-row 2)))
    (assert-num= array-off
	(setf-submatrix 5 5 'upper-triangular-matrix (submatrix matrix 1 2 :end-row 3) (unit-matrix 3 3))))

  ;; Non upper-triangular subsets
  (assert-condition error (setf (submatrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) 0 1) (unit-matrix 5 3))))

;;; Set the submatrix of a lower triangular matrix
(deftest setf-lower-triangular-submatrix (triangular-matrix)

  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((1.0 0.0 0.0 0.0 0.0)
							 (1.0 1.0 0.0 0.0 0.0)
							 (1.0 1.0 1.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 0 0) (lower-triangular-matrix 0 3)))
    (assert-num= array-ul
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 0 0 :end-row 3 :end-column 3) (lower-triangular-matrix 0 10))))
  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 1.0 0.0 0.0)
						      (0.0 0.0 1.0 1.0 0.0)
						      (0.0 0.0 1.0 1.0 1.0)))
      (setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 2 2) (lower-triangular-matrix)))

  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 1.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 1 1) (lower-triangular-matrix 1 4)))
    (assert-num= array-mid
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 1 1 :end-row 4 :end-column 4) (lower-triangular-matrix 1))))

  ;; Below diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (1.0 0.0 0.0 0.0 0.0)
							  (1.0 1.0 0.0 0.0 0.0)
							  (1.0 1.0 1.0 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 2 0) (lower-triangular-matrix 0 3)))
    (assert-num= array-off
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 2 0 :end-column 3) (lower-triangular-matrix))))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 2 1) (submatrix (unit-matrix 3 3) 0 0 :end-column 2)))
    (assert-num= array-off
	(setf-submatrix 5 5 'lower-triangular-matrix (submatrix matrix 2 1 :end-column 3) (unit-matrix 3 3))))

  ;; Non lower-triangular subsets
  (assert-condition error (setf (submatrix (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix) 1 0) (unit-matrix 3 5))))


;; (defgeneric replace-matrix (matrix1 matrix2 &key
;; 					      start-row1 end-row1
;; 					      start-column1 end-column1
;; 					      start-row2 end-row2
;; 					      start-column2 end-column2)

;;; Replace all or part of an upper triangular matrix
(deftest upper-triangular-matrix-replace (triangular-matrix)

  ;; Replace the entire matrix
  (assert-num= (upper-triangular-matrix)
      (replace-matrix (zero-matrix 10 10 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)))

  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((1.0 1.0 1.0 0.0 0.0)
							 (0.0 1.0 1.0 0.0 0.0)
							 (0.0 0.0 1.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix 0 3)))
    (assert-num= array-ul
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:end-row1 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:end-row2 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:end-row1 3 :end-column2 3))
    (assert-num= array-ul
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:end-row2 3 :end-column2 3)))

  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 1.0 1.0 1.0)
						      (0.0 0.0 0.0 1.0 1.0)
						      (0.0 0.0 0.0 0.0 1.0)))
      (replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
		      :start-row1 2 :start-column1 2))

  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 1.0 0.0)
							  (0.0 0.0 1.0 1.0 0.0)
							  (0.0 0.0 0.0 1.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix 0 3)
			:start-row1 1 :start-column1 1))
    (assert-num= array-mid
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 1 :start-column1 1 :end-row1 4 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 1 :start-column1 1 :end-row2 3 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 1 :start-column1 1 :end-row1 4 :end-column2 3))
    (assert-num= array-mid
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 1 :start-column1 1 :end-row2 3 :end-column2 3)))

  ;; Above diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 0.0 1.0 1.0)
							  (0.0 0.0 0.0 0.0 1.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix 0 3)
			:start-row1 0 :start-column1 2))
    (assert-num= array-off
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 0 :start-column1 2 :end-row1 3))
    (assert-num= array-off
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (upper-triangular-matrix)
			:start-row1 0 :start-column1 2 :end-row2 3)))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 1.0 1.0 1.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (unit-matrix 3 3)
			:start-row1 1 :start-column1 2 :end-row2 2))
    (assert-num= array-off
	(replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (unit-matrix 10 10)
			:start-row1 1 :start-column1 2 :end-row1 3)))

  ;; Non upper triangular subsets
  (assert-condition error (replace-matrix (zero-matrix 5 5 :matrix-type 'upper-triangular-matrix) (unit-matrix 5 3)
					  :start-column1 1)))


;;; Replace all or part of a lower triangular matrix
(deftest lower-triangular-matrix-replace (triangular-matrix)

  ;; Replace the entire matrix
  (assert-num= (lower-triangular-matrix)
      (replace-matrix
       (zero-matrix 10 10 :matrix-type 'lower-triangular-matrix)
       (lower-triangular-matrix)))

  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((1.0 0.0 0.0 0.0 0.0)
							 (1.0 1.0 0.0 0.0 0.0)
							 (1.0 1.0 1.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix 0 3)))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :end-row1 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :end-row2 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :end-row1 3 :end-column2 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :end-row2 3 :end-column2 3)))

  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 1.0 0.0 0.0)
						      (0.0 0.0 1.0 1.0 0.0)
						      (0.0 0.0 1.0 1.0 1.0)))
      (replace-matrix
       (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
       (lower-triangular-matrix)
       :start-row1 2 :start-column1 2))

  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 1.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix 0 3)
	 :start-row1 1 :start-column1 1))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row1 4 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row2 3 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row1 4 :end-column2 3))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row2 3 :end-column2 3)))

  ;; Below diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (1.0 0.0 0.0 0.0 0.0)
							  (1.0 1.0 0.0 0.0 0.0)
							  (1.0 1.0 1.0 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix 0 3)
	 :start-row1 2 :start-column1 0))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 2 :start-column1 0
	 :end-column1 3))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (lower-triangular-matrix)
	 :start-row1 2 :start-column1 0
	 :end-column2 3)))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)
							  (0.0 1.0 1.0 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (unit-matrix 3 3)
	 :start-row1 2 :start-column1 1 :end-column2 2))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
	 (unit-matrix 10 10)
	 :start-row1 2 :start-column1 1 :end-column1 3)))

  ;; Non lower triangular subsets
  (assert-condition error (replace-matrix
			   (zero-matrix 5 5 :matrix-type 'lower-triangular-matrix)
			   (unit-matrix 5 3)
			   :start-row1 1)))

;;; Validate a range for an upper triangular matrix.
(deftest upper-triangular-matrix-validated-range (triangular-matrix)
  (test-matrix-validated-range 'upper-triangular-matrix 10 10))

;;; Validate a range for an lower triangular matrix.
(deftest lower-triangular-matrix-validated-range (triangular-matrix)
  (test-matrix-validated-range 'lower-triangular-matrix 10 10))

