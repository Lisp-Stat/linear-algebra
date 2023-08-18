;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite symmetric-matrix (core))

(defun symmetric-matrix (&optional (start 0) (end 10))
  (make-matrix (- end start) (- end start) :matrix-type 'symmetric-matrix :initial-contents (symmetric-array start end)))

(deftest make-symmetric-matrix (symmetric-matrix)
  ;; A default symmetric matrix
  (let ((matrix (make-matrix 10 10 :matrix-type 'symmetric-matrix)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-num= (make-array '(10 10) :initial-element 0)
	matrix))
  ;; Specify the symmetric matrix element type
  (let ((matrix (make-matrix 10 10 :matrix-type 'symmetric-matrix :element-type 'single-float)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-eq
	(array-element-type
	 (linear-algebra::contents matrix))
	(array-element-type
	 (make-array '(10 10) :element-type 'single-float)))
    (assert-num= (make-array '(10 10) :initial-element 0.0 :element-type 'single-float)	matrix))
  ;; Specify the symmetric matrix initial element
  (let ((matrix (make-matrix 10 10 :matrix-type 'symmetric-matrix :initial-element 1.0)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-num= (make-array '(10 10) :initial-element 1.0) matrix))
  ;; Specify the symmetric matrix contents - Nested list
  (let* ((data '((1.1 1.2 1.3 1.4)
		 (1.2 2.2 2.3 2.4)
		 (1.3 2.3 3.3 3.4)
		 (1.4 2.4 3.4 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))
  ;; Specify the symmetric matrix contents - Nested vector
  (let* ((data #(#(1.1 1.2 1.3 1.4)
		 #(1.2 2.2 2.3 2.4)
		 #(1.3 2.3 3.3 3.4)
		 #(1.4 2.4 3.4 4.4)))
         (matrix (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))
  ;; Specify the symmetric matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
						      (1.2 2.2 2.3 2.4)
						      (1.3 2.3 3.3 3.4)
						      (1.4 2.4 3.4 4.4))))
         (matrix (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'symmetric-matrix))
    (assert-num= data matrix))

  ;; Erroneous 2D array input data
  ;; I don't know why Thomas is testing error conditions, as they are raised by the testing utility function and not by make-matrix.
  (assert-condition error (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #3A(((1.1 1.2) (2.1 2.2))
												((3.1 3.2) (4.1 4.2))
												((5.1 5.2) (6.1 6.2)))))
  (assert-condition error (make-matrix 3 4 :matrix-type 'symmetric-matrix :initial-contents (symmetric-array 0 4)))
  (assert-condition error (make-matrix 4 3 :matrix-type 'symmetric-matrix :initial-contents (symmetric-array 0 4)))
  (assert-condition error (make-matrix 3 3 :element-type 'single-float :matrix-type 'symmetric-matrix
					   :initial-contents '((1.0 2.0 3.0) (4 5 6) (7 8 9))))
  (assert-condition error (make-matrix 3 3 :element-type 'single-float :matrix-type 'symmetric-matrix
					   :initial-contents  #(#(1.0 2.0 3.0) #(4 5 6) #(7 8 9))))
  (assert-condition error (make-matrix 3 3 :element-type 'single-float :matrix-type 'symmetric-matrix
					   :initial-contents #2A((1.0 2.0 3.0) (4 5 6) (7 8 9))))
  (assert-condition error (make-matrix 5 5 :matrix-type 'symmetric-matrix :initial-contents (coordinate-array 0 0 5 5)))
  ;; Specify initial element and initial contents
  (assert-condition error (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-element 1.1 :initial-contents (symmetric-array 0 4))))

;;; Test the symmetric matrix predicate
(deftest symmetric-matrix-predicate  (symmetric-matrix)
  (assert-true (symmetric-matrix-p (make-matrix 10 10 :matrix-type 'symmetric-matrix)))
  (assert-false (symmetric-matrix-p (make-array '(10 10)))))

;;; Test the symmetric matrix bounds
(deftest symmetric-matrix-in-bounds-p (symmetric-matrix)
  (test-matrix-in-bounds-p 'symmetric-matrix))

;;; Test the symmetric matrix element type
(deftest symmetric-matrix-element-type (symmetric-matrix)
  (test-matrix-element-type 'symmetric-matrix t nil))

;;; Test the symmetric matrix dimensions
(deftest symmetric-matrix-dimensions (symmetric-matrix)
  (test-matrix-dimensions 'symmetric-matrix 9 9))

;;; Test the symmetric matrix row dimension
(deftest symmetric-matrix-row-dimension (symmetric-matrix)
  (test-matrix-row-dimension 'symmetric-matrix 9 9))

;;; Test the symmetric matrix column dimension
(deftest symmetric-matrix-column-dimension (symmetric-matrix)
  (test-matrix-column-dimension 'symmetric-matrix 9 9))

;;; Reference symmetric matrix elements
(deftest symmetric-matrix-mref (symmetric-matrix)
  (let* ((initial-contents '((1.1 1.2 1.3 1.4 1.5)
			     (1.2 2.2 2.3 2.4 2.5)
			     (1.3 2.3 3.3 3.4 3.5)
			     (1.4 2.4 3.4 4.4 4.5)
			     (1.5 2.5 3.5 4.5 5.5)))
         (rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random-interior-index columns)
			(random-interior-index columns)))
		   ((/= i0 rowi) i0)))
         (data (make-array (list rows columns) :initial-contents  initial-contents))
         (matrix (make-matrix rows columns :matrix-type 'symmetric-matrix :initial-contents initial-contents)))
    (assert-num= (aref data 0 0) (mref matrix 0 0))
    (assert-num= (aref data 0 cend) (mref matrix 0 cend))
    (assert-num= (aref data rend 0) (mref matrix rend 0))
    (assert-num= (mref matrix 0 cend) (mref matrix rend 0))
    (assert-num= (aref data rend cend) (mref matrix rend cend))
    (assert-num= (aref data rowi coli) (mref matrix rowi coli))
    (assert-num= (mref matrix rowi coli) (mref matrix coli rowi))))

;;; Set symmetric matrix elements
(deftest symmetric-matrix-setf-mref (symmetric-matrix)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random-interior-index columns)
			(random-interior-index columns)))
		   ((/= i0 rowi) i0)))
         (matrix (make-matrix rows columns :matrix-type 'symmetric-matrix :initial-contents '((1.1 1.2 1.3 1.4 1.5)
											      (1.2 2.2 2.3 2.4 2.5)
											      (1.3 2.3 3.3 3.4 3.5)
											      (1.4 2.4 3.4 4.4 4.5)
											      (1.5 2.5 3.5 4.5 5.5)))))
    (destructuring-bind (val1 val2 val3 val4) (make-random-list 4 1.0)
      (setf (mref matrix 0 0)       val1)
      (setf (mref matrix 0 cend)    val2)
      (setf (mref matrix rend cend) val3)
      (setf (mref matrix rowi coli) val4)
      (assert-num= val1 (mref matrix 0 0))
      (assert-num= val2 (mref matrix 0 cend))
      (assert-num= val2 (mref matrix rend 0))
      (assert-num= val3 (mref matrix rend cend))
      (assert-num= val4 (mref matrix rowi coli))
      (assert-num= val4 (mref matrix coli rowi)))))

;;; Copy the symmetric matrix
(deftest copy-symmetric-matrix (symmetric-matrix)
  (let ((matrix (make-matrix 5 5 :matrix-type 'symmetric-matrix :initial-contents (symmetric-array 0 5))))
    (assert-true (symmetric-matrix-p (copy-matrix matrix)))
    (assert-false (eq matrix (copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (copy-matrix matrix))))
    (assert-num= matrix (copy-matrix matrix))))

;;; Test the submatrix of a symmetric matrix
(deftest symmetric-submatrix (symmetric-matrix)
  (let ((matrix (make-matrix 10 10 :matrix-type 'symmetric-matrix :initial-contents (symmetric-array)))
        (submat (make-matrix 10 10 :matrix-type 'dense-matrix :initial-contents (symmetric-array))))
    ;; The entire matrix
    (assert-num=
	(symmetric-array)
	(submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-num=
	(symmetric-array 3)
	(submatrix matrix 3 3))
    ;; End row and column
    (assert-num=
	(symmetric-array 3 5)
	(submatrix matrix 3 3 :end-row 5 :end-column 5))
    ;; Submatrix is a general matrix
    (assert-true
	(typep
	 (submatrix matrix 1 2)
	 'dense-matrix))
    (assert-num=
	(submatrix submat 1 2)
	(submatrix matrix 1 2))
    (assert-true
	(typep
	 (submatrix matrix 1 1 :end-row 5)
	 'dense-matrix))
    (assert-num=
	(submatrix submat 1 1 :end-row 5)
	(submatrix matrix 1 1 :end-row 5))
    (assert-true
	(typep
	 (submatrix matrix 1 1 :end-column 8)
	 'dense-matrix))
    (assert-num=
	(submatrix submat 1 1 :end-column 8)
	(submatrix matrix 1 1 :end-column 8))
    ;; Start row exceeds dimensions
    (assert-condition
	error (submatrix matrix 11 5))
    ;; Start column exceeds dimensions
    (assert-condition
	error (submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-condition
	error (submatrix matrix 5 5 :end-row 11))
    ;; End column exceeds dimensions
    (assert-condition
	error (submatrix matrix 5 5 :end-column 11))
    ;; Start row exceeds end row
    (assert-condition
	error (submatrix matrix 7 7 :end-row 6))
    ;; Start column exceeds end column
    (assert-condition
	error (submatrix matrix 7 7 :end-column 6))))

;;; Set the submatrix of a symmetric matrix
(deftest setf-symmetric-submatrix (symmetric-matrix)
  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((0.0 1.0 2.0 0.0 0.0)
							 (1.0 1.1 2.1 0.0 0.0)
							 (2.0 2.1 2.2 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 0 0)
	 (symmetric-matrix 0 3)))
    (assert-num= array-ul
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 0 0 :end-row 3 :end-column 3)
	 (symmetric-matrix))))
  ;; Lower right submatrix
  (assert-num=
      (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
					     (0.0 0.0 0.0 0.0 0.0)
					     (0.0 0.0 0.0 1.0 2.0)
					     (0.0 0.0 1.0 1.1 2.1)
					     (0.0 0.0 2.0 2.1 2.2)))
      (setf-submatrix 5 5 'symmetric-matrix (submatrix matrix 2 2) (symmetric-matrix)))
  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 1.1 2.1 3.1 0.0)
							  (0.0 2.1 2.2 3.2 0.0)
							  (0.0 3.1 3.2 3.3 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 1 1)
	 (symmetric-matrix 1 4)))
    (assert-num= array-mid
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 1 1 :end-row 4 :end-column 4)
	 (symmetric-matrix 1))))
  ;; Off diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 1.0 2.0)
							  (0.0 0.0 1.0 1.1 2.1)
							  (0.0 1.0 2.0 2.1 2.2)
							  (1.0 1.1 2.1 0.0 0.0)
							  (2.0 2.1 2.2 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 0 2)
	 (symmetric-matrix 0 3)))
    (assert-num= array-off
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 0 2 :end-row 3)
	 (symmetric-matrix))))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 1.0 2.0)
							  (0.0 0.0 1.0 1.1 2.1)
							  (0.0 1.0 1.1 0.0 0.0)
							  (0.0 2.0 2.1 0.0 0.0)))))
    (assert-num= array-off
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 1 2)
	 (submatrix
	  (symmetric-matrix 0 3) 0 0 :end-row 2)))
    (assert-num= array-off
	(setf-submatrix
	 5 5 'symmetric-matrix
	 (submatrix matrix 1 2 :end-row 3)
	 (symmetric-matrix))))
  ;; Asymmetric subsets
  (assert-condition error
      (setf
       (submatrix
	(zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	0 1)
       (unit-matrix 5 3))))

;;; Replace all or part of a symmetric matrix
(deftest symmetric-matrix-replace (symmetric-matrix)
  ;; Replace the entire matrix
  (assert-num= (symmetric-matrix)
      (replace-matrix
       (zero-matrix 10 10 :matrix-type 'symmetric-matrix)
       (symmetric-matrix)))
  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((0.0 1.0 2.0 0.0 0.0)
							 (1.0 1.1 2.1 0.0 0.0)
							 (2.0 2.1 2.2 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)
							 (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix 0 3)))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :end-row1 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :end-row2 3 :end-column1 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :end-row1 3 :end-column2 3))
    (assert-num= array-ul
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :end-row2 3 :end-column2 3)))
  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 0.0 0.0)
						      (0.0 0.0 0.0 1.0 2.0)
						      (0.0 0.0 1.0 1.1 2.1)
						      (0.0 0.0 2.0 2.1 2.2)))
      (replace-matrix (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
		      (symmetric-matrix)
		      :start-row1 2 :start-column1 2))
  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 1.0 2.0 0.0)
							  (0.0 1.0 1.1 2.1 0.0)
							  (0.0 2.0 2.1 2.2 0.0)
							  (0.0 0.0 0.0 0.0 0.0)))))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix 0 3)
	 :start-row1 1 :start-column1 1))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row1 4 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row2 3 :end-column1 4))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row1 4 :end-column2 3))
    (assert-num= array-mid
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 1 :start-column1 1
	 :end-row2 3 :end-column2 3)))
  ;; Off diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 1.0 2.0)
							  (0.0 0.0 1.0 1.1 2.1)
							  (0.0 1.0 2.0 2.1 2.2)
							  (1.0 1.1 2.1 0.0 0.0)
							  (2.0 2.1 2.2 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix 0 3)
	 :start-row1 0 :start-column1 2))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 0 :start-column1 2
	 :end-row1 3))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 0 :start-column1 2
	 :end-row2 3)))
  (let ((array-off (make-array '(5 5) :initial-contents '((0.0 0.0 0.0 0.0 0.0)
							  (0.0 0.0 0.0 1.0 2.0)
							  (0.0 0.0 1.0 1.1 2.1)
							  (0.0 1.0 1.1 0.0 0.0)
							  (0.0 2.0 2.1 0.0 0.0)))))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix 0 3)
	 :start-row1 1 :start-column1 2 :end-row2 2))
    (assert-num= array-off
	(replace-matrix
	 (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
	 (symmetric-matrix)
	 :start-row1 1 :start-column1 2 :end-row1 3)))
  ;; Asymmetric subsets
  (assert-condition error
      (replace-matrix
       (zero-matrix 5 5 :matrix-type 'symmetric-matrix)
       (unit-matrix 5 3)
       :start-column1 1)))

;;; Validate a range for a symmetric matrix.
(deftest symmetric-matrix-validated-range (symmetric-matrix)
  (test-matrix-validated-range
   'symmetric-matrix 10 10))

;;; Symmetric matrix fundamental operations

(deftest transpose-symmetric-matrix (symmetric-matrix)
  (let ((matrix (symmetric-matrix))
        (transpose (symmetric-array)))
    (assert-true
	(typep
	 (transpose matrix)
	 'symmetric-matrix))
    (assert-num=
	transpose (transpose matrix))))

(deftest ntranspose-symmetric-matrix (symmetric-matrix)
  (let ((matrix (symmetric-matrix))
        (transpose (symmetric-array)))
    (assert-eq matrix (ntranspose matrix))
    (assert-num= transpose matrix)))

(deftest permute-symmetric-matrix (symmetric-matrix)
  (let ((matrix (symmetric-matrix 0 5))
        (pmat (make-matrix 5 5 :matrix-type 'permutation-matrix :initial-contents '((0 0 1 0 0)
										    (0 0 0 0 1)
										    (1 0 0 0 0)
										    (0 1 0 0 0)
										    (0 0 0 1 0)))))
    (assert-num= #2A((2.0 3.0 0.0 4.0 1.0)
		     (2.1 3.1 1.0 4.1 1.1)
		     (2.2 3.2 2.0 4.2 2.1)
		     (3.2 3.3 3.0 4.3 3.1)
		     (4.2 4.3 4.0 4.4 4.1))
      (permute matrix pmat))
    (assert-num= #2A((2.0 2.1 2.2 3.2 4.2)
		     (4.0 4.1 4.2 4.3 4.4)
		     (0.0 1.0 2.0 3.0 4.0)
		     (1.0 1.1 2.1 3.1 4.1)
		     (3.0 3.1 3.2 3.3 4.3))
      (permute pmat matrix))))

(deftest scale-symmetric-matrix (symmetric-matrix)
  (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		   ( 3.6  6.6  6.9  7.2)
		   ( 3.9  6.9  9.9 10.2)
		   ( 4.2  7.2 10.2 13.2))
    (scale 3.0 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										     (1.2 2.2 2.3 2.4)
										     (1.3 2.3 3.3 3.4)
										     (1.4 2.4 3.4 4.4))))))

(deftest nscale-symmetric-matrix (symmetric-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										      (1.2 2.2 2.3 2.4)
										      (1.3 2.3 3.3 3.4)
										      (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix (nscale 3.0 matrix))
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      matrix)))

;;; FIXME : Add tests to cover addition/subtraction that results in a
;;; non-symmetric matrix

(deftest add-symmetric-matrix (symmetric-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents  #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    ;; No scalar
    (assert-num= #2A(( 2.2  2.4  2.6  2.8)
		     ( 2.4  4.4  4.6  4.8)
		     ( 2.6  4.6  6.6  6.8)
		     ( 2.8  4.8  6.8  8.8))
      (add matrix matrix))
    ;; Scalar1
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      (add matrix matrix :scalar1 2.0))
    ;; Scalar2
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      (add matrix matrix :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num= #2A(( 5.5  6.0  6.5  7.0)
		     ( 6.0 11.0 11.5 12.0)
		     ( 6.5 11.5 16.5 17.0)
		     ( 7.0 12.0 17.0 22.0))
      (add matrix matrix :scalar1 2.0 :scalar2 3.0))))

(deftest nadd-symmetric-matrix (symmetric-matrix)
  ;; No scalar
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
											   (1.2 2.2 2.3 2.4)
											   (1.3 2.3 3.3 3.4)
											   (1.4 2.4 3.4 4.4)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents '((1.1 1.2 1.3 1.4)
										     (1.2 2.2 2.3 2.4)
										     (1.3 2.3 3.3 3.4)
										     (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (nadd matrix1 matrix2))
    (assert-num= #2A((2.2 2.4 2.6 2.8)
		     (2.4 4.4 4.6 4.8)
		     (2.6 4.6 6.6 6.8)
		     (2.8 4.8 6.8 8.8))
      matrix1))
  ;; Scalar1
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
											   (1.2 2.2 2.3 2.4)
											   (1.3 2.3 3.3 3.4)
											   (1.4 2.4 3.4 4.4)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (nadd matrix1 matrix2 :scalar1 2.0))
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      matrix1))
  ;; Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
											   (1.2 2.2 2.3 2.4)
											   (1.3 2.3 3.3 3.4)
											   (1.4 2.4 3.4 4.4)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (nadd matrix1 matrix2 :scalar2 2.0))
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      matrix1))
  ;; Scalar1 & Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
											   (1.2 2.2 2.3 2.4)
											   (1.3 2.3 3.3 3.4)
											   (1.4 2.4 3.4 4.4)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1
	(nadd
	 matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num= #2A(( 5.5  6.0  6.5  7.0)
		     ( 6.0 11.0 11.5 12.0)
		     ( 6.5 11.5 16.5 17.0)
		     ( 7.0 12.0 17.0 22.0))
      matrix1)))

(deftest subtract-symmetric-matrix (symmetric-matrix)
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A(( 2.2  2.4  2.6  2.8)
										       ( 2.4  4.4  4.6  4.8)
										       ( 2.6  4.6  6.6  6.8)
										       ( 2.8  4.8  6.8  8.8))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    ;; No scalar
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      (subtract matrix1 matrix2))
    ;; Scalar1
    (assert-num= #2A(( 3.3  3.6  3.9  4.2)
		     ( 3.6  6.6  6.9  7.2)
		     ( 3.9  6.9  9.9 10.2)
		     ( 4.2  7.2 10.2 13.2))
      (subtract matrix1 matrix2 :scalar1 2.0))
    ;; Scalar2
    (assert-num= #2A((0.0 0.0 0.0 0.0)
		     (0.0 0.0 0.0 0.0)
		     (0.0 0.0 0.0 0.0)
		     (0.0 0.0 0.0 0.0))
      (subtract matrix1 matrix2 :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      (subtract matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))))

(deftest nsubtract-symmetric-matrix (symmetric-matrix)
  ;; No scalar
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '(( 2.2  2.4  2.6  2.8)
											   ( 2.4  4.4  4.6  4.8)
											   ( 2.6  4.6  6.6  6.8)
											   ( 2.8  4.8  6.8  8.8)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2))
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      matrix1
      :no-scalar))
  ;; Scalar1
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '((1.1 1.2 1.3 1.4)
											   (1.2 2.2 2.3 2.4)
											   (1.3 2.3 3.3 3.4)
											   (1.4 2.4 3.4 4.4)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1(nsubtract matrix1 matrix2 :scalar1 2.0))
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      matrix1
      :scalar1))
  ;; Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '(( 3.3  3.6  3.9  4.2)
											   ( 3.6  6.6  6.9  7.2)
											   ( 3.9  6.9  9.9 10.2)
											   ( 4.2  7.2 10.2 13.2)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2 :scalar2 2.0))
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      matrix1
      :scalar2))
  ;; Scalar1 & Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'symmetric-matrix
				  :initial-contents (make-array '(4 4) :initial-contents '(( 2.2  2.4  2.6  2.8)
											   ( 2.4  4.4  4.6  4.8)
											   ( 2.6  4.6  6.6  6.8)
											   ( 2.8  4.8  6.8  8.8)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3 1.4)
										       (1.2 2.2 2.3 2.4)
										       (1.3 2.3 3.3 3.4)
										       (1.4 2.4 3.4 4.4)))))
    (assert-eq matrix1
	(nsubtract matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num= #2A((1.1 1.2 1.3 1.4)
		     (1.2 2.2 2.3 2.4)
		     (1.3 2.3 3.3 3.4)
		     (1.4 2.4 3.4 4.4))
      matrix1
      :scalar1-&-scalar2)))

(deftest product-symmetric-matrix (symmetric-matrix)
  ;; Row vector - dense matrix
  (assert-true
      (typep
       (product
	(row-vector 1.0 2.0 3.0)
	(make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									      (1.2 2.2 2.3)
									      (1.3 2.3 3.3))))
       'row-vector))
  (assert-num= #(7.4 12.5 15.8)
      (product
       (row-vector 1.0 2.0 3.0)
       (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									     (1.2 2.2 2.3)
									     (1.3 2.3 3.3)))))
  (assert-num= #(15.54 26.25 33.18)
      (product
       (row-vector 1.0 2.0 3.0)
       (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									     (1.2 2.2 2.3)
									     (1.3 2.3 3.3)))
       2.1))
  (assert-condition error
      (product
       (row-vector 1.0 2.0 3.0 4.0 5.0 6.0)
       (make-matrix 3 3 :initial-element 1.0 :matrix-type 'symmetric-matrix)))
  ;; Dense matrix - column vector
  (assert-true
      (typep
       (product
	(make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									      (1.2 2.2 2.3)
									      (1.3 2.3 3.3)))
	(column-vector 1.0 2.0 3.0))
       'column-vector))
  (assert-num= #(7.4 12.5 15.8)
      (product
       (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									     (1.2 2.2 2.3)
									     (1.3 2.3 3.3)))
       (column-vector 1.0 2.0 3.0)))
  (assert-num= #(15.54 26.25 33.18)
      (product
       (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									     (1.2 2.2 2.3)
									     (1.3 2.3 3.3)))
       (column-vector 1.0 2.0 3.0)
       2.1))
  (assert-condition error
      (product
       (make-matrix 3 3 :initial-element 1.0 :matrix-type 'square-matrix)
       (column-vector 1.0 2.0 3.0 4.0 5.0 6.0)))
  ;; Dense matrix - matrix
  (assert-true
      (typep
       (product
	(make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									      (1.2 2.2 2.3)
									      (1.3 2.3 3.3)))
	(make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									      (1.2 2.2 2.3)
									      (1.3 2.3 3.3))))
       'symmetric-matrix))
  (assert-true
      (typep
       (product
	(make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									      (1.2 2.2 2.3)
									      (1.3 2.3 3.3)))
	(make-matrix 3 3 :matrix-type 'square-matrix :initial-contents #2A((1.0 1.0 1.0)
									   (2.0 2.0 2.0)
									   (3.0 3.0 3.0))))
       'square-matrix))
  (assert-num= #2A((4.34  6.95      8.48)
		   (6.95 11.57     14.209999)
		   (8.48 14.209999 17.869999))
    (product
     (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									   (1.2 2.2 2.3)
									   (1.3 2.3 3.3)))
     (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									   (1.2 2.2 2.3)
									   (1.3 2.3 3.3)))))
  (assert-num= #2A(( 9.114    14.594999 17.807999)
		   (14.594999 24.296999 29.840996)
		   (17.807999 29.840996 37.526997))
    (product
     (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									   (1.2 2.2 2.3)
									   (1.3 2.3 3.3)))
     (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2 1.3)
									   (1.2 2.2 2.3)
									   (1.3 2.3 3.3)))
     2.1)))

(deftest solve-symmetric-matrix (symmetric-matrix)
  (let ((vector2 (column-vector 2.0 1.0))
        (vector3 (column-vector 2.3 1.2 2.2))
        (matrix2 (make-matrix 2 2 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2) (1.2 2.2))))
        (matrix3 (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.15 1.26 1.37)
										       (1.26 2.23 2.31)
										       (1.37 2.31 3.31)))))
    ;; 2x2
    (assert-num= #(3.2653065 -1.3265308)
	(solve matrix2 vector2))
    (assert-num= #2A((1.1 1.2) (1.2 2.2)) matrix2)
    (assert-num= #(2.0 1.0)vector2)
    ;; 3x3
    (assert-num= #(3.5856622 -2.306286 0.79007966)
	(solve matrix3 vector3))
    (assert-num= #2A((1.15 1.26 1.37)
		     (1.26 2.23 2.31)
		     (1.37 2.31 3.31))
      matrix3)
    (assert-num= #(2.3 1.2 2.2) vector3)))

(deftest nsolve-symmetric-matrix (symmetric-matrix)
  (let ((vector2 (column-vector 2.0 1.0))
        (vector3 (column-vector 2.3 1.2 2.2))
        (matrix2 (make-matrix 2 2 :matrix-type 'symmetric-matrix :initial-contents #2A((1.1 1.2) (1.2 2.2))))
        (matrix3 (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents #2A((1.15 1.26 1.37)
										       (1.26 2.23 2.31)
										       (1.37 2.31 3.31)))))
    ;; 2x2
    (assert-num= #(3.2653065 -1.3265308)
	(nsolve matrix2 vector2))
    (assert-num= #2A((1.1 1.0909091) (1.0909091 0.8909091)) matrix2)
    (assert-num= #(3.2653065 -1.3265308) vector2)
    ;; 3x3
    (assert-num= #(3.5856622 -2.306286 0.79007966)
	(nsolve matrix3 vector3))
    (assert-num= #2A((1.15      1.0956522  1.1913043)
		     (1.0956522 0.84947825 0.9522979)
		     (1.1913043 0.9522979  0.90754557))
      matrix3)
    (assert-num= #(3.5856622 -2.306286 0.79007966) vector3)))

(deftest invert-symmetric-matrix (symmetric-matrix)
  ;; 2x2
  (let ((matrix (make-matrix 2 2 :matrix-type 'symmetric-matrix :initial-contents '((1.1 1.2) (1.2 2.2)))))
    (assert-num= #2A((2.2448979 -1.2244898) (-1.2244898 1.122449))
      (invert matrix))
    (assert-num= #2A((1.1 1.2) (1.2 2.2)) matrix))
  ;; 3x3
  (let ((matrix (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents '((1.15 1.26 1.37)
										    (1.26 2.23 2.31)
										    (1.37 2.31 3.31)))))
    (assert-num= #2A((2.3068395 -1.1345832 -0.16298579)
		     (-1.1345832 2.1764503 -1.0493114)
		     (-0.16298579 -1.0493114 1.101873))
      (invert matrix))
    (assert-num= #2A((1.15 1.26 1.37)
		     (1.26 2.23 2.31)
		     (1.37 2.31 3.31))
      matrix)))

(deftest ninvert-symmetric-matrix (symmetric-matrix)
  ;; 2x2
  (let ((matrix (make-matrix 2 2 :matrix-type 'symmetric-matrix :initial-contents '((1.1 1.2) (1.2 2.2)))))
    (assert-num= #2A((2.2448979 -1.2244898) (-1.2244898 1.122449))
      (ninvert matrix))
    (assert-num= #2A((1.1 1.0909091) (1.0909091 0.8909091)) matrix))
  ;; 3x3
  (let ((matrix (make-matrix 3 3 :matrix-type 'symmetric-matrix :initial-contents '((1.15 1.26 1.37)
										    (1.26 2.23 2.31)
										    (1.37 2.31 3.31)))))
    (assert-num= #2A((2.3068395 -1.1345832 -0.16298579)
		     (-1.1345832 2.1764503 -1.0493114)
		     (-0.16298579 -1.0493114 1.101873))
      (ninvert matrix))
    (assert-num= #2A((1.15      1.0956522  1.1913043)
		     (1.0956522 0.84947825 0.9522979)
		     (1.1913043 0.9522979  0.90754557))
      matrix)))
