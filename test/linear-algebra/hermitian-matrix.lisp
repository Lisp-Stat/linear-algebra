;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-test)

(defsuite hermitian-matrix (core))

(defun hermitian-matrix (&optional (start 0) (end 10))
  (make-matrix (- end start) (- end start) :matrix-type 'hermitian-matrix
					   :initial-contents (hermitian-array start end)))

(defun unit-hermitian-matrix (size)
  "Return a ROWSxCOLUMNS unit Hermitian matrix."
  (make-matrix size size :matrix-type 'hermitian-matrix :initial-contents
	       (let ((init (make-array (list size size))))
		 (dotimes (i0 size init)
		   (setf (aref init i0 i0) #C(1 0))
		   (dotimes (i1 i0)
		     (setf
		      (aref init i0 i1) #C(1 -1)
		      (aref init i1 i0) #C(1  1)))))))

(deftest make-hermitian-matrix (hermitian-matrix)
  ;; A default Hermitian matrix
  (let ((matrix (make-matrix 10 10 :matrix-type 'hermitian-matrix)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'hermitian-matrix))
    (assert-num= (make-array '(10 10) :initial-element 0) matrix))

  ;; Specify the Hermitian matrix element type
  (let* ((data '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0 3.0))
		 (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0 3.0))
		 (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0 0.0))))
         (matrix (make-matrix 3 3 :matrix-type 'hermitian-matrix :element-type '(complex single-float) :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'hermitian-matrix))
    (assert-eq (array-element-type (linear-algebra::contents matrix)) (array-element-type (make-array '(3 3) :element-type '(complex single-float) :initial-contents data)))
    (assert-num= (make-array '(3 3) :element-type '(complex single-float)
				    :initial-contents data) matrix))

  ;; Specify the Hermitian matrix contents - Nested list
  (let* ((data '((#C(1  0) #C(1  2) #C(1  3) #C(1 4))
		 (#C(1 -2) #C(2  0) #C(2  3) #C(2 4))
		 (#C(1 -3) #C(2 -3) #C(3  0) #C(3 4))
		 (#C(1 -4) #C(2 -4) #C(3 -4) #C(4 0))))
         (matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'hermitian-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))
  ;; Specify the Hermitian matrix contents - Nested vector
  (let* ((data #(#(#C(1  0) #C(1  2) #C(1  3) #C(1 4))
		 #(#C(1 -2) #C(2  0) #C(2  3) #C(2 4))
		 #(#C(1 -3) #C(2 -3) #C(3  0) #C(3 4))
		 #(#C(1 -4) #C(2 -4) #C(3 -4) #C(4 0))))
         (matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'hermitian-matrix))
    (assert-num= (make-array '(4 4) :initial-contents data) matrix))
  ;; Specify the Hermitian matrix contents - 2D array
  (let* ((data (make-array '(4 4) :initial-contents '((#C(1  0) #C(1  2) #C(1  3) #C(1 4))
						      (#C(1 -2) #C(2  0) #C(2  3) #C(2 4))
						      (#C(1 -3) #C(2 -3) #C(3  0) #C(3 4))
						      (#C(1 -4) #C(2 -4) #C(3 -4) #C(4 0)))))
         (matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix :initial-contents data)))
    (assert-true (matrixp matrix))
    (assert-true (typep matrix 'hermitian-matrix))
    (assert-num= data matrix))
  ;; Errors
  (assert-condition error (make-matrix 4 4 :matrix-type 'hermitian-matrix :initial-contents #C(1 2)))
  (assert-condition error (make-matrix 4 4 :matrix-type 'hermitian-matrix
					   :initial-contents #3A(((#C(1 0) #C(1 2)) (#C(2 1) #C(2 2)))
								 ((#C(3 1) #C(3 2)) (#C(4 1) #C(4 2)))
								 ((#C(5 1) #C(5 2)) (#C(6 1) #C(6 2))))))
  (assert-condition error (make-matrix 3 4 :matrix-type 'hermitian-matrix :initial-contents (hermitian-array 0 4)))
  (assert-condition error (make-matrix 4 3 :matrix-type 'hermitian-matrix :initial-contents (hermitian-array 0 4)))
  (assert-condition error (make-matrix 5 5 :matrix-type 'symmetric-matrix :initial-contents (coordinate-array 0 0 5 5)))
  ;; Specify initial element and initial contents
  (assert-condition error (make-matrix 4 4 :matrix-type 'hermitian-matrix :initial-element 1.1
					   :initial-contents (hermitian-array 0 4))))

;;; Test the hermitian matrix predicate
(deftest hermitian-matrix-predicate (hermitian-matrix)
  (assert-true (hermitian-matrix-p (make-matrix 10 10 :matrix-type 'hermitian-matrix
						      :initial-contents (hermitian-array))))
  (assert-false (hermitian-matrix-p (make-array '(10 10)))))

;;; Test the hermitian matrix bounds
(deftest hermitian-matrix-in-bounds-p (hermitian-matrix)
  (test-matrix-in-bounds-p 'hermitian-matrix (hermitian-array)))

;;; Test the hermitian matrix element type
(deftest hermitian-matrix-element-type (hermitian-matrix)
  (let ((numeric-types '(integer fixnum short-float single-float double-float long-float)))
    (dolist (ntype numeric-types)
      (assert-true (subtypep (matrix-element-type (make-matrix 2 2 :matrix-type 'hermitian-matrix
								   :element-type `(complex ,ntype)
								   :initial-contents (list
										      (list
										       (complex (coerce 1 ntype) (coerce  0 ntype))
										       (complex (coerce 1 ntype) (coerce  1 ntype)))
										      (list
										       (complex (coerce 1 ntype) (coerce -1 ntype))
										       (complex (coerce 1 ntype) (coerce  0 ntype))))))
			     (upgraded-array-element-type `(complex ,ntype)))))))

;;; Test the hermitian matrix dimensions
(deftest hermitian-matrix-dimensions (hermitian-matrix)
  (assert-equal (list 10 10) (matrix-dimensions  (make-matrix 10 10 :matrix-type 'hermitian-matrix
								    :initial-contents (hermitian-array)))))

;;; Test the hermitian matrix row dimension
(deftest hermitian-matrix-row-dimension (hermitian-matrix)
  (assert-eq 10 (matrix-row-dimension (make-matrix 10 10 :matrix-type 'hermitian-matrix
							 :initial-contents (hermitian-array)))))

;;; Test the hermitian matrix column dimension
(deftest hermitian-matrix-column-dimension (hermitian-matrix)
  (assert-eq 10 (matrix-column-dimension (make-matrix 10 10 :matrix-type 'hermitian-matrix
							    :initial-contents (hermitian-array)))))

;;; Reference hermitian matrix elements
(deftest hermitian-matrix-mref (hermitian-matrix)
  (let* ((initial-contents '((#C(1  0) #C(1  2) #C(1  3) #C(1  4) #C(1 5))
			     (#C(1 -2) #C(2  0) #C(2  3) #C(2  4) #C(2 5))
			     (#C(1 -3) #C(2 -3) #C(3  0) #C(3  4) #C(3 5))
			     (#C(1 -4) #C(2 -4) #C(3 -4) #C(4  0) #C(4 5))
			     (#C(1 -5) #C(2 -5) #C(3 -5) #C(4 -5) #C(5 0))))
         (rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random-interior-index columns)
			(random-interior-index columns)))
		   ((/= i0 rowi) i0)))
         (data (make-array (list rows columns) :initial-contents initial-contents))
         (matrix (make-matrix rows columns :matrix-type 'hermitian-matrix :initial-contents initial-contents)))
    (assert-num= (aref data 0 0) (mref matrix 0 0))
    (assert-num= (aref data 0 cend) (mref matrix 0 cend))
    (assert-num= (aref data rend 0) (mref matrix rend 0))
    (assert-num= (mref matrix 0 cend) (conjugate (mref matrix rend 0)))
    (assert-num= (aref data rend cend) (mref matrix rend cend))
    (assert-num= (aref data rowi coli) (mref matrix rowi coli))
    (assert-num= (mref matrix rowi coli) (conjugate (mref matrix coli rowi)))))

;;; Set hermitian matrix elements
(deftest hermitian-matrix-setf-mref (hermitian-matrix)
  (let* ((rows 5) (columns 5)
         (rend (1- rows)) (cend (1- columns))
         (rowi (random-interior-index rows))
         (coli (do ((i0 (random-interior-index columns)
			(random-interior-index columns)))
		   ((/= i0 rowi) i0)))
         (matrix (make-matrix rows columns :matrix-type 'hermitian-matrix
					   :initial-contents '((#C(1  0) #C(1  2) #C(1  3) #C(1  4) #C(1 5))
							       (#C(1 -2) #C(2  0) #C(2  3) #C(2  4) #C(2 5))
							       (#C(1 -3) #C(2 -3) #C(3  0) #C(3  4) #C(3 5))
							       (#C(1 -4) #C(2 -4) #C(3 -4) #C(4  0) #C(4 5))
							       (#C(1 -5) #C(2 -5) #C(3 -5) #C(4 -5) #C(5 0))))))
    (multiple-value-bind (val1 val2 val3 val4)
        (values #C(6 0) (complex-random #C(5 5))
		#C(7 0) (complex-random #C(5 5)))
      (setf (mref matrix 0 0)       val1)
      (setf (mref matrix 0 cend)    val2)
      (setf (mref matrix rend cend) val3)
      (setf (mref matrix rowi coli) val4)
      (assert-num= val1 (mref matrix 0 0))
      (assert-num= val2 (mref matrix 0 cend))
      (assert-num= val2 (conjugate (mref matrix rend 0)))
      (assert-num= val3 (mref matrix rend cend))
      (assert-num= val4 (mref matrix rowi coli))
      (assert-num= val4 (conjugate (mref matrix coli rowi))))))

;;; Copy the Hermitian matrix
(deftest copy-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 5 5 :matrix-type 'hermitian-matrix :initial-contents (hermitian-array 0 5))))
    (assert-true (hermitian-matrix-p (copy-matrix matrix)))
    (assert-false (eq matrix (copy-matrix matrix)))
    (assert-false (eq (linear-algebra::contents matrix) (linear-algebra::contents (copy-matrix matrix))))
    (assert-num= matrix (copy-matrix matrix))))

;;; Test the submatrix of a Hermitian matrix
(deftest hermitian-submatrix (hermitian-matrix)
  (let ((matrix (make-matrix 10 10 :matrix-type 'hermitian-matrix :initial-contents (hermitian-array)))
        (submat (make-matrix 10 10 :matrix-type 'dense-matrix :initial-contents (hermitian-array))))
    ;; The entire matrix
    (assert-num= (hermitian-array) (submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-num= (hermitian-array 3) (submatrix matrix 3 3))
    ;; End row and column
    (assert-num= (hermitian-array 3 5) (submatrix  matrix 3 3 :end-row 5 :end-column 5))
    ;; Submatrix is a general matrix
    (assert-true (typep (submatrix matrix 1 2) 'dense-matrix))
    (assert-num= (submatrix submat 1 2)	(submatrix matrix 1 2))
    (assert-true (typep (submatrix matrix 1 1 :end-row 5) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :end-row 5) (submatrix matrix 1 1 :end-row 5))
    (assert-true (typep (submatrix matrix 1 1 :end-column 8) 'dense-matrix))
    (assert-num= (submatrix submat 1 1 :end-column 8)(submatrix matrix 1 1 :end-column 8))
    ;; Start row exceeds dimensions
    (assert-condition error (submatrix matrix 11 5))
    ;; Start column exceeds dimensions
    (assert-condition error (submatrix matrix 5 11))
    ;; End row exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-row 11))
    ;; End column exceeds dimensions
    (assert-condition error (submatrix matrix 5 5 :end-column 11))
    ;; Start row exceeds end row
    (assert-condition error (submatrix matrix 7 7 :end-row 6))
    ;; Start column exceeds end column
    (assert-condition error (submatrix matrix 7 7 :end-column 6))))

;;; Set the submatrix of a Hermitian matrix
(deftest setf-hermitian-submatrix (hermitian-matrix)
  (macrolet ((setf-submatrix (size submatrix-form data-form)
               (let ((matrix (second submatrix-form)))
                 `(let ((,matrix (unit-hermitian-matrix ,size)))
                    (setf ,submatrix-form ,data-form)
                    ,matrix))))
    ;; Upper left submatrix
    (let ((array-ul (make-array '(5 5) :initial-contents '((#C(1  0) #C(2  1) #C(3  1) #C(1  1) #C(1 1))
							   (#C(2 -1) #C(2  0) #C(3  2) #C(1  1) #C(1 1))
							   (#C(3 -1) #C(3 -2) #C(3  0) #C(1  1) #C(1 1))
							   (#C(1 -1) #C(1 -1) #C(1 -1) #C(1  0) #C(1 1))
							   (#C(1 -1) #C(1 -1) #C(1 -1) #C(1 -1) #C(1 0))))))
      (assert-num= array-ul (setf-submatrix 5 (submatrix matrix 0 0) (hermitian-matrix 0 3)))
      (assert-num= array-ul (setf-submatrix 5 (submatrix matrix 0 0 :end-row 3 :end-column 3) (hermitian-matrix))))
    ;; Lower right submatrix
    (assert-num= (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 1))
							(#C(1 -1) #C(1  0) #C(1  1) #C(1  1) #C(1 1))
							(#C(1 -1) #C(1 -1) #C(1  0) #C(2  1) #C(3 1))
							(#C(1 -1) #C(1 -1) #C(2 -1) #C(2  0) #C(3 2))
							(#C(1 -1) #C(1 -1) #C(3 -1) #C(3 -2) #C(3 0))))
	(setf-submatrix 5 (submatrix matrix 2 2) (hermitian-matrix)))
    ;; Middle submatrix
    (let ((array-mid (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 1))
							    (#C(1 -1) #C(2  0) #C(3  2) #C(4  2) #C(1 1))
							    (#C(1 -1) #C(3 -2) #C(3  0) #C(4  3) #C(1 1))
							    (#C(1 -1) #C(4 -2) #C(4 -3) #C(4  0) #C(1 1))
							    (#C(1 -1) #C(1 -1) #C(1 -1) #C(1 -1) #C(1 0))))))
      (assert-num= array-mid (setf-submatrix 5 (submatrix matrix 1 1) (hermitian-matrix 1 4)))
      (assert-num= array-mid (setf-submatrix 5 (submatrix matrix 1 1 :end-row 4 :end-column 4) (hermitian-matrix 1))))
    ;; Off diagonal submatrix
    (let ((array-off (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 2))
							    (#C(1 -1) #C(1  0) #C(1  1) #C(2  1) #C(2 2))
							    (#C(1 -1) #C(1 -1) #C(1  0) #C(1  1) #C(1 1))
							    (#C(1 -1) #C(2 -1) #C(1 -1) #C(1  0) #C(1 1))
							    (#C(1 -2) #C(2 -2) #C(1 -1) #C(1 -1) #C(1 0)))))
          (submatrix (make-matrix 3 3 :initial-contents '((#C(1 1) #C(1 2) #C(1 3))
							  (#C(2 1) #C(2 2) #C(2 3))
							  (#C(3 1) #C(3 2) #C(3 3))))))
      (assert-num= array-off (setf-submatrix 5 (submatrix matrix 0 3) (submatrix submatrix 0 0 :end-row 2 :end-column 2)))
      (assert-num= array-off (setf-submatrix 5 (submatrix matrix 0 3 :end-row 2) submatrix)))
    (let ((array-off (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(2 1))
							    (#C(1 -1) #C(1  0) #C(1  1) #C(1  2) #C(2 2))
							    (#C(1 -1) #C(1 -1) #C(1  0) #C(1  1) #C(1 1))
							    (#C(1 -1) #C(1 -2) #C(1 -1) #C(1  0) #C(1 1))
							    (#C(2 -1) #C(2 -2) #C(1 -1) #C(1 -1) #C(1 0)))))
          (submatrix (make-matrix 3 3 :initial-contents '((#C(1 -1) #C(1 -2) #C(1 -3))
							  (#C(2 -1) #C(2 -2) #C(2 -3))
							  (#C(3 -1) #C(3 -2) #C(3 -3))))))
      (assert-num= array-off (setf-submatrix 5 (submatrix matrix 3 0 :end-column 2) submatrix))
      (assert-num= array-off (setf-submatrix 5 (submatrix matrix 3 0) (submatrix submatrix 0 0 :end-column 2)))))
  ;; Non-Hermitian subsets
  (assert-condition error (setf (submatrix (unit-hermitian-matrix 10) 0 1) (unit-matrix 5 3))))

;;; Replace all or part of a Hermitian matrix
(deftest hermitian-matrix-replace (hermitian-matrix)
  ;; Replace the entire matrix
  (assert-num= (hermitian-matrix) (replace-matrix (unit-hermitian-matrix 10) (hermitian-matrix)))
  ;; Upper left submatrix
  (let ((array-ul (make-array '(5 5) :initial-contents '((#C(1  0) #C(2  1) #C(3  1) #C(1  1) #C(1 1))
							 (#C(2 -1) #C(2  0) #C(3  2) #C(1  1) #C(1 1))
							 (#C(3 -1) #C(3 -2) #C(3  0) #C(1  1) #C(1 1))
							 (#C(1 -1) #C(1 -1) #C(1 -1) #C(1  0) #C(1 1))
							 (#C(1 -1) #C(1 -1) #C(1 -1) #C(1 -1) #C(1 0))))))
    (assert-num= array-ul (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 0 3)))
    (assert-num= array-ul (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix) :end-row1 3 :end-column1 3))
    (assert-num= array-ul (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix) :end-row2 3 :end-column1 3))
    (assert-num= array-ul (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix) :end-row1 3 :end-column2 3))
    (assert-num= array-ul (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix) :end-row2 3 :end-column2 3)))
  ;; Lower right submatrix
  (assert-num= (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 1))
						      (#C(1 -1) #C(1  0) #C(1  1) #C(1  1) #C(1 1))
						      (#C(1 -1) #C(1 -1) #C(1  0) #C(2  1) #C(3 1))
						      (#C(1 -1) #C(1 -1) #C(2 -1) #C(2  0) #C(3 2))
						      (#C(1 -1) #C(1 -1) #C(3 -1) #C(3 -2) #C(3 0))))
      (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix) :start-row1 2 :start-column1 2))
  ;; Middle submatrix
  (let ((array-mid (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 1))
							  (#C(1 -1) #C(2  0) #C(3  2) #C(4  2) #C(1 1))
							  (#C(1 -1) #C(3 -2) #C(3  0) #C(4  3) #C(1 1))
							  (#C(1 -1) #C(4 -2) #C(4 -3) #C(4  0) #C(1 1))
							  (#C(1 -1) #C(1 -1) #C(1 -1) #C(1 -1) #C(1 0))))))
    (assert-num= array-mid (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 1 4) :start-row1 1 :start-column1 1))
    (assert-num= array-mid (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 1 4) :start-row1 1 :start-column1 1 :end-row1 4 :end-column1 4))
    (assert-num= array-mid (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 1) :start-row1 1 :start-column1 1 :end-row2 3 :end-column1 4))
    (assert-num= array-mid (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 1) :start-row1 1 :start-column1 1 :end-row1 4 :end-column2 3))
    (assert-num= array-mid (replace-matrix (unit-hermitian-matrix 5) (hermitian-matrix 1) :start-row1 1 :start-column1 1 :end-row2 3 :end-column2 3)))
  ;; Off diagonal submatrix
  (let ((array-off (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 2))
							  (#C(1 -1) #C(1  0) #C(1  1) #C(2  1) #C(2 2))
							  (#C(1 -1) #C(1 -1) #C(1  0) #C(1  1) #C(1 1))
							  (#C(1 -1) #C(2 -1) #C(1 -1) #C(1  0) #C(1 1))
							  (#C(1 -2) #C(2 -2) #C(1 -1) #C(1 -1) #C(1 0)))))
        (submatrix1 (make-matrix 2 2 :initial-contents '((#C(1 1) #C(1 2))
							 (#C(2 1) #C(2 2)))))
        (submatrix2 (make-matrix 4 4 :initial-contents '((#C(1 1) #C(1 2) #C(1 3) #C(1 4))
							 (#C(2 1) #C(2 2) #C(2 3) #C(2 4))
							 (#C(3 1) #C(3 2) #C(3 3) #C(3 4))
							 (#C(4 1) #C(4 2) #C(4 3) #C(4 4))))))
    (assert-num= array-off (replace-matrix (unit-hermitian-matrix 5) submatrix1 :start-row1 0 :start-column1 3))
    (assert-num= array-off (replace-matrix (unit-hermitian-matrix 5) submatrix2 :start-row1 0 :start-column1 3 :end-row1 2))
    (assert-num= array-off (replace-matrix (unit-hermitian-matrix 5) submatrix2 :start-row1 0 :start-column1 3 :end-row2 2)))
  (let ((array-off (make-array '(5 5) :initial-contents '((#C(1  0) #C(1  1) #C(1  1) #C(1  1) #C(1 2))
							  (#C(1 -1) #C(1  0) #C(1  1) #C(2  1) #C(2 2))
							  (#C(1 -1) #C(1 -1) #C(1  0) #C(1  1) #C(1 1))
							  (#C(1 -1) #C(2 -1) #C(1 -1) #C(1  0) #C(1 1))
							  (#C(1 -2) #C(2 -2) #C(1 -1) #C(1 -1) #C(1 0)))))
        (submatrix (make-matrix 3 3 :initial-contents '((#C(1 1) #C(1 2) #C(1 3))
							(#C(2 1) #C(2 2) #C(2 3))
							(#C(3 1) #C(3 2) #C(3 3))))))
    (assert-num= array-off (replace-matrix (unit-hermitian-matrix 5) submatrix :start-column1 3 :end-row1 2))
    (assert-num= array-off (replace-matrix (unit-hermitian-matrix 5) submatrix :start-column1 3 :end-row2 2)))
  ;; Non-Hermitian subsets
  (assert-condition error (replace-matrix (unit-hermitian-matrix 5) (unit-matrix 5 3) :start-column1 1)))

;;; Validate a range for a hermitian matrix.
(deftest hermitian-matrix-validated-range (hermitian-matrix)
  (let ((matrix (unit-hermitian-matrix 10))
        (row1 (random 10))
        (row2 (random 10))
        (col1 (random 10))
        (col2 (random 10)))
    (assert-equal (values row1 col1 10 10) (matrix-validated-range matrix row1 col1))
    (assert-equal (values (min row1 row2) col1 (max row1 row2) 10)
	(matrix-validated-range matrix (min row1 row2) col1 (max row1 row2)))
    (assert-equal (values row1 (min col1 col2) 10 (max col1 col2))
	(matrix-validated-range matrix row1 (min col1 col2) nil (max col1 col2)))
    (assert-equal (values
		   (min row1 row2) (min col1 col2)
		   (max row1 row2) (max col1 col2))
	(matrix-validated-range
	 matrix (min row1 row2) (min col1 col2)
	 (max row1 row2) (max col1 col2)))
    (assert-condition error (matrix-validated-range matrix 11 col1))
    (assert-condition error (matrix-validated-range matrix row1 11))
    (assert-condition error (matrix-validated-range matrix 9 col1 1))
    (assert-condition error (matrix-validated-range matrix row1 9 10 1))
    (assert-condition error (matrix-validated-range matrix 9 9 1 1))))

(deftest norm-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 5 5 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1  0) #C(1  2) #C(1  3) #C(1  4) #C(1 5))
										    (#C(1 -2) #C(2  0) #C(2  3) #C(2  4) #C(2 5))
										    (#C(1 -3) #C(2 -3) #C(3  0) #C(3  4) #C(3 5))
										    (#C(1 -4) #C(2 -4) #C(3 -4) #C(4  0) #C(4 5))
										    (#C(1 -5) #C(2 -5) #C(3 -5) #C(4 -5) #C(5 0))))))
    (assert-num= 27.71826 (norm matrix))
    (assert-num= 27.71826 (norm matrix 1))
    (assert-num= 6.4031243 (norm matrix :max))
    (assert-num= 22.248597 (norm matrix :frobenius))
    (assert-num= 27.71826 (norm matrix :infinity))
    (assert-condition error (norm matrix :unknown))))

(deftest transpose-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										    (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										    (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										    (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0)))))
        (transpose #2A((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
		       (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
		       (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
		       (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0)))))
    (assert-true (typep (transpose matrix) 'hermitian-matrix))
    (assert-num= transpose (transpose matrix))))

(deftest ntranspose-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										    (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										    (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										    (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0)))))
        (transpose #2A((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
		       (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
		       (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
		       (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0)))))
    (assert-eq matrix (ntranspose matrix))
    (assert-num= transpose matrix)))

(deftest permute-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 5 5 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1  0) #C(1  2) #C(1  3) #C(1  4) #C(1 5))
										    (#C(1 -2) #C(2  0) #C(2  3) #C(2  4) #C(2 5))
										    (#C(1 -3) #C(2 -3) #C(3  0) #C(3  4) #C(3 5))
										    (#C(1 -4) #C(2 -4) #C(3 -4) #C(4  0) #C(4 5))
										    (#C(1 -5) #C(2 -5) #C(3 -5) #C(4 -5) #C(5 0)))))
        (pmat (make-matrix 5 5 :matrix-type 'permutation-matrix
			       :initial-contents '((0 0 1 0 0)
										    (0 0 0 0 1)
										    (1 0 0 0 0)
										    (0 1 0 0 0)
										    (0 0 0 1 0)))))
    (assert-true (typep (permute matrix pmat) 'square-matrix))
    (assert-num= #2A((#C(1  3) #C(1  4) #C(1  0) #C(1  5) #C(1  2))
		     (#C(2  3) #C(2  4) #C(1 -2) #C(2  5) #C(2  0))
		     (#C(3  0) #C(3  4) #C(1 -3) #C(3  5) #C(2 -3))
		     (#C(3 -4) #C(4  0) #C(1 -4) #C(4  5) #C(2 -4))
		     (#C(3 -5) #C(4 -5) #C(1 -5) #C(5  0) #C(2 -5)))
      (permute matrix pmat))
    (assert-true (typep (permute pmat matrix) 'square-matrix))
    (assert-num= #2A((#C(1 -3) #C(2 -3) #C(3  0) #C(3  4) #C(3  5))
		     (#C(1 -5) #C(2 -5) #C(3 -5) #C(4 -5) #C(5  0))
		     (#C(1  0) #C(1  2) #C(1  3) #C(1  4) #C(1  5))
		     (#C(1 -2) #C(2  0) #C(2  3) #C(2  4) #C(2  5))
		     (#C(1 -4) #C(2 -4) #C(3 -4) #C(4  0) #C(4  5)))
      (permute pmat matrix))))

(deftest scale-hermitian-matrix (hermitian-matrix)
  (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		   (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		   (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		   (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
    (scale 3.0 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				:initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										   (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										   (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										   (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0)))))))

(deftest nscale-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										    (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										    (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										    (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    (assert-eq matrix (nscale 3.0 matrix))
    (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		     (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		     (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		     (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
      matrix)))

(deftest add-hermitian-matrix (hermitian-matrix)
  (let ((matrix (make-matrix 4 4 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										    (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										    (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										    (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    ;; No scalar
    (assert-num= #2A((#C(2.0  0.0) #C(2.0  4.0) #C(2.0  6.0) #C(2.0 8.0))
		     (#C(2.0 -4.0) #C(4.0  0.0) #C(4.0  6.0) #C(4.0 8.0))
		     (#C(2.0 -6.0) #C(4.0 -6.0) #C(6.0  0.0) #C(6.0 8.0))
		     (#C(2.0 -8.0) #C(4.0 -8.0) #C(6.0 -8.0) #C(8.0 0.0)))
      (add matrix matrix))
    ;; Scalar1
    (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		     (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		     (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		     (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
      (add matrix matrix :scalar1 2.0))
    ;; Scalar2
    (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		     (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		     (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		     (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
      (add matrix matrix :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num= #2A((#C(5.0   0.0) #C( 5.0  10.0) #C( 5.0  15.0) #C( 5.0 20.0))
		     (#C(5.0 -10.0) #C(10.0   0.0) #C(10.0  15.0) #C(10.0 20.0))
		     (#C(5.0 -15.0) #C(10.0 -15.0) #C(15.0   0.0) #C(15.0 20.0))
		     (#C(5.0 -20.0) #C(10.0 -20.0) #C(15.0 -20.0) #C(20.0  0.0)))
      (add matrix matrix :scalar1 2.0 :scalar2 3.0))))

(deftest nadd-hermitian-matrix (hermitian-matrix)
  ;; No scalar
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
											   (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
											   (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
											   (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										     (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										     (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										     (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nadd matrix1 matrix2))
    (assert-num= #2A((#C(2.0  0.0) #C(2.0  4.0) #C(2.0  6.0) #C(2.0 8.0))
		     (#C(2.0 -4.0) #C(4.0  0.0) #C(4.0  6.0) #C(4.0 8.0))
		     (#C(2.0 -6.0) #C(4.0 -6.0) #C(6.0  0.0) #C(6.0 8.0))
		     (#C(2.0 -8.0) #C(4.0 -8.0) #C(6.0 -8.0) #C(8.0 0.0)))
      matrix1))
  ;; Scalar1
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
											   (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
											   (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
											   (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										     (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										     (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										     (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nadd matrix1 matrix2 :scalar1 2.0))
    (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		     (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		     (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		     (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
      matrix1))
  ;; Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
											   (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
											   (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
											   (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										     (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										     (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										     (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nadd matrix1 matrix2 :scalar2 2.0))
    (assert-num= #2A((#C(3.0   0.0) #C(3.0   6.0) #C(3.0   9.0) #C( 3.0 12.0))
		     (#C(3.0  -6.0) #C(6.0   0.0) #C(6.0   9.0) #C( 6.0 12.0))
		     (#C(3.0  -9.0) #C(6.0  -9.0) #C(9.0   0.0) #C( 9.0 12.0))
		     (#C(3.0 -12.0) #C(6.0 -12.0) #C(9.0 -12.0) #C(12.0  0.0)))
      matrix1))
  ;; Scalar1 & Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
											   (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
											   (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
											   (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents '((#C(1.0  0.0) #C(1.0  2.0) #C(1.0  3.0) #C(1.0 4.0))
										     (#C(1.0 -2.0) #C(2.0  0.0) #C(2.0  3.0) #C(2.0 4.0))
										     (#C(1.0 -3.0) #C(2.0 -3.0) #C(3.0  0.0) #C(3.0 4.0))
										     (#C(1.0 -4.0) #C(2.0 -4.0) #C(3.0 -4.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nadd matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num= #2A((#C(5.0   0.0) #C( 5.0  10.0) #C( 5.0  15.0) #C( 5.0 20.0))
		     (#C(5.0 -10.0) #C(10.0   0.0) #C(10.0  15.0) #C(10.0 20.0))
		     (#C(5.0 -15.0) #C(10.0 -15.0) #C(15.0   0.0) #C(15.0 20.0))
		     (#C(5.0 -20.0) #C(10.0 -20.0) #C(15.0 -20.0) #C(20.0  0.0)))
      matrix1)))

(deftest subtract-hermitian-matrix (hermitian-matrix)
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(2.0  0.0) #C(4.0  2.0) #C(6.0  2.0) #C(8.0 2.0))
							(#C(4.0 -2.0) #C(4.0  0.0) #C(6.0  4.0) #C(8.0 4.0))
							(#C(6.0 -2.0) #C(6.0 -4.0) #C(6.0  0.0) #C(8.0 6.0))
							(#C(8.0 -2.0) #C(8.0 -4.0) #C(8.0 -6.0) #C(8.0 0.0)))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
							(#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
							(#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
							(#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
    ;; No scalar
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      (subtract matrix1 matrix2))
    ;; Scalar1
    (assert-num= #2A((#C( 3.0  0.0) #C( 6.0  3.0) #C( 9.0  3.0) #C(12.0  3.0))
		     (#C( 6.0 -3.0) #C( 6.0  0.0) #C( 9.0  6.0) #C(12.0  6.0))
		     (#C( 9.0 -3.0) #C( 9.0 -6.0) #C( 9.0  0.0) #C(12.0  9.0))
		     (#C(12.0 -3.0) #C(12.0 -6.0) #C(12.0 -9.0) #C(12.0  0.0)))
      (subtract matrix1 matrix2 :scalar1 2.0))
    ;; Scalar2
    (assert-num= #2A((#C(0.0  0.0) #C(0.0  0.0) #C(0.0  0.0) #C(0.0 0.0))
		     (#C(0.0 -0.0) #C(0.0  0.0) #C(0.0  0.0) #C(0.0 0.0))
		     (#C(0.0 -0.0) #C(0.0 -0.0) #C(0.0  0.0) #C(0.0 0.0))
		     (#C(0.0 -0.0) #C(0.0 -0.0) #C(0.0 -0.0) #C(0.0 0.0)))
      (subtract matrix1 matrix2 :scalar2 2.0))
    ;; Scalar1 & Scalar2
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      (subtract matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))))

(deftest nsubtract-hermitian-matrix (hermitian-matrix)
  ;; No scalar
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(2.0  0.0) #C(4.0  2.0) #C(6.0  2.0) #C(8.0 2.0))
											   (#C(4.0 -2.0) #C(4.0  0.0) #C(6.0  4.0) #C(8.0 4.0))
											   (#C(6.0 -2.0) #C(6.0 -4.0) #C(6.0  0.0) #C(8.0 6.0))
											   (#C(8.0 -2.0) #C(8.0 -4.0) #C(8.0 -6.0) #C(8.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
							(#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
							(#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
							(#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2))
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      matrix1))
  ;; Scalar1
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
											   (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
											   (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
											   (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
        (matrix2 (make-matrix 4 4 :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
							(#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
							(#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
							(#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2 :scalar1 2.0))
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      matrix1))
  ;; Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C( 3.0  0.0) #C( 6.0  3.0) #C( 9.0  3.0) #C(12.0  3.0))
											   (#C( 6.0 -3.0) #C( 6.0  0.0) #C( 9.0  6.0) #C(12.0  6.0))
											   (#C( 9.0 -3.0) #C( 9.0 -6.0) #C( 9.0  0.0) #C(12.0  9.0))
											   (#C(12.0 -3.0) #C(12.0 -6.0) #C(12.0 -9.0) #C(12.0  0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
										       (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
										       (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
										       (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2 :scalar2 2.0))
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      matrix1))
  ;; Scalar1 & Scalar2
  (let ((matrix1 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents (make-array '(4 4)
								:initial-contents '((#C(2.0  0.0) #C(4.0  2.0) #C(6.0  2.0) #C(8.0 2.0))
											   (#C(4.0 -2.0) #C(4.0  0.0) #C(6.0  4.0) #C(8.0 4.0))
											   (#C(6.0 -2.0) #C(6.0 -4.0) #C(6.0  0.0) #C(8.0 6.0))
											   (#C(8.0 -2.0) #C(8.0 -4.0) #C(8.0 -6.0) #C(8.0 0.0))))))
        (matrix2 (make-matrix 4 4 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
										       (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
										       (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
										       (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0))))))
    (assert-eq matrix1 (nsubtract matrix1 matrix2 :scalar1 2.0 :scalar2 3.0))
    (assert-num= #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0) #C(4.0 1.0))
		     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0) #C(4.0 2.0))
		     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0) #C(4.0 3.0))
		     (#C(4.0 -1.0) #C(4.0 -2.0) #C(4.0 -3.0) #C(4.0 0.0)))
      matrix1)))

(deftest product-hermitian-matrix (hermitian-matrix)
  ;; Row vector - Hermitian matrix
  (assert-true (typep (product (row-vector 1.0 2.0 3.0)	(unit-hermitian-matrix 3)) 'row-vector))
  (assert-num= #(#C(14.0 -5.0) #C(15.0 -5.0) #C(18.0 5.0))
      (product (row-vector 1.0 2.0 3.0)
	       (make-matrix 3 3 :matrix-type 'hermitian-matrix
				:initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))))
  (assert-num= #(#C(29.399998 -10.5) #C(31.499999 -10.5) #C(37.8 10.5))
      (product (row-vector 1.0 2.0 3.0) (make-matrix 3 3 :matrix-type 'hermitian-matrix
							 :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
													      (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
													      (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	       2.1))
  (assert-condition error (product (row-vector 1.0 2.0 3.0 4.0 5.0 6.0) (unit-hermitian-matrix 3)))
  ;; Hermitian matrix - column vector
  (assert-true (typep (product (unit-hermitian-matrix 3) (column-vector 1.0 2.0 3.0)) 'column-vector))
  (assert-num= #(#C(14.0 5.0) #C(15.0 5.0) #C(18.0 -5.0))
      (product (make-matrix 3 3 :matrix-type 'hermitian-matrix
				:initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	       (column-vector 1.0 2.0 3.0)))
  (assert-num= #(#C(29.399998 10.5) #C(31.499999 10.5) #C(37.8 -10.5))
      (product (make-matrix 3 3 :matrix-type 'hermitian-matrix
				:initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										     (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										     (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	       (column-vector 1.0 2.0 3.0)
	       2.1))
  (assert-condition error (product (unit-hermitian-matrix 3) (column-vector 1.0 2.0 3.0 4.0 5.0 6.0)))
  ;; Hermitian matrix - matrix
  (assert-true (typep (product (unit-hermitian-matrix 3) (unit-hermitian-matrix 3)) 'hermitian-matrix))
  (assert-num= #2A((#C(16.0   0.0) #C(17.0  0.0) #C(16.0 11.0))
		   (#C(17.0   0.0) #C(22.0  0.0) #C(22.0  9.0))
		   (#C(16.0 -11.0) #C(22.0 -9.0) #C(32.0  0.0)))
    (product (make-matrix 3 3 :matrix-type 'hermitian-matrix
			      :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										   (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										   (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	     (make-matrix 3 3 :matrix-type 'hermitian-matrix
			      :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										   (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										   (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))))
  (assert-num= #2A((#C(33.6 0.0) #C(35.699997 0.0) #C(33.6 23.099999))
		   (#C(35.699997 0.0) #C(46.199997 0.0) #C(46.199997 18.9))
		   (#C(33.6 -23.099999) #C(46.199997 -18.9) #C(67.2 0.0)))
    (product (make-matrix 3 3 :matrix-type 'hermitian-matrix
			      :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										   (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										   (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	     (make-matrix 3 3 :matrix-type 'hermitian-matrix
			      :initial-contents #2A((#C(1.0  0.0) #C(2.0  1.0) #C(3.0  1.0))
										   (#C(2.0 -1.0) #C(2.0  0.0) #C(3.0  2.0))
										   (#C(3.0 -1.0) #C(3.0 -2.0) #C(3.0  0.0))))
	     2.1))
  (assert-condition error (product (unit-hermitian-matrix 3) (unit-hermitian-matrix 4))))

(deftest solve-hermitian-matrix (hermitian-matrix)
  (let ((vector2 (column-vector 2.0 1.0))
        (vector3 (column-vector 2.3 1.2 2.2))
        (matrix2 (make-matrix 2 2 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(2.0 0.0) #C(1.0 -2.0))
										       (#C(1.0 2.0) #C(3.0  0.0)))))
        (matrix3 (make-matrix 3 3 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
										       (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
										       (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0))))))
    ;; 2x2
    (assert-num= #(#C(5.0 2.0) #C(0.0 -4.0)) (solve matrix2 vector2))
    (assert-num= #2A((#C(2.0 0.0) #C(1.0 -2.0))
		     (#C(1.0 2.0) #C(3.0  0.0)))
      matrix2)
    (assert-num= #(2.0 1.0) vector2 (linear-algebra::contents vector2))
    ;; 3x3
    (assert-num= #(#C( 3.5175734   3.4673646)
		   #C( 3.3198433  -4.3366637)
		   #C(-0.78414906 -1.2595192))
	(solve matrix3 vector3))
    (assert-num= #2A((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
		     (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
		     (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0)))
      matrix3)
    (assert-num= #(2.3 1.2 2.2) vector3 (linear-algebra::contents vector3))))

(deftest nsolve-hermitian-matrix (hermitian-matrix)
  (let ((vector2 (column-vector 2.0 1.0))
        (vector3 (column-vector 2.3 1.2 2.2))
        (matrix2 (make-matrix 2 2 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(2.0 0.0) #C(1.0 -2.0))
										       (#C(1.0 2.0) #C(3.0  0.0)))))
        (matrix3 (make-matrix 3 3 :matrix-type 'hermitian-matrix
				  :initial-contents #2A((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
										       (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
										       (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0))))))
    ;; 2x2
    (assert-num= #(#C(5.0 2.0) #C(0.0 -4.0)) (nsolve matrix2 vector2))
    (assert-num= #2A((#C(2.0 0.0) #C(0.5 -1.0))
		     (#C(0.5 1.0) #C(0.5  0.0)))
      matrix2)
    (assert-num= #(#C(5.0 2.0) #C(0.0 -4.0)) vector2)
    ;; 3x3
    (assert-num= #(#C( 3.5175734   3.4673646)
		   #C( 3.3198433  -4.3366637)
		   #C(-0.78414906 -1.2595192))
	(nsolve matrix3 vector3))
    (assert-num= #2A((#C(3.31       0.0)       #C( 0.38066468 -0.6042296) #C( 0.4138973   -0.9063445))
		     (#C(0.38066468 0.6042296) #C( 0.54190326  0.0)       #C(-0.044656467 -2.1882146))
		     (#C(0.4138973  0.9063445) #C(-0.044656467 2.1882146) #C( 2.2680602   -3.7252904E-9)))
      matrix3)
    (assert-num= #(#C( 3.5175734   3.4673646)
		   #C( 3.3198433  -4.3366637)
		   #C(-0.78414906 -1.2595192))
	vector3)))

(deftest invert-hermitian-matrix (hermitian-matrix)
  ;; 2x2
  (let ((matrix (make-matrix 2 2 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(2.0 0.0) #C(1.0 -2.0))
										    (#C(1.0 2.0) #C(3.0  0.0))))))
    (assert-num= #2A((#C( 3.0 -0.0) #C(-1.0  2.0))
		     (#C(-1.0 -2.0) #C( 2.0 -0.0)))
      (invert matrix))
    (assert-num= #2A((#C(2.0 0.0) #C(1.0 -2.0))
		     (#C(1.0 2.0) #C(3.0  0.0)))
      matrix))
  ;; 3x3
  (let ((matrix (make-matrix 3 3 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
										    (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
										    (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0))))))
    (assert-num= #2A((#C( 2.602711    5.9604646E-8) #C(-0.64015717  2.808354)     #C(-0.7729426   0.04424542))
		     (#C(-0.64015717 -2.808354)     #C(3.9574066   -3.7252904E-9) #C( 0.019689279 0.96479553))
		     (#C(-0.7729426  -0.04424542)   #C(0.019689279 -0.96479553)   #C( 0.4409054  -7.241874E-10)))
      (invert matrix))
    (assert-num= #2A((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
		     (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
		     (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0)))
      matrix)))

(deftest ninvert-hermitian-matrix (hermitian-matrix)
  ;; 2x2
  (let ((matrix (make-matrix 2 2 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(2.0 0.0) #C(1.0 -2.0))
										    (#C(1.0 2.0) #C(3.0  0.0))))))
    (assert-num= #2A((#C( 3.0 -0.0) #C(-1.0  2.0))
		     (#C(-1.0 -2.0) #C( 2.0 -0.0)))
      (ninvert matrix))
    (assert-num= #2A((#C(2.0 0.0) #C(0.5 -1.0))
		     (#C(0.5 1.0) #C(0.5  0.0)))
      matrix))
  ;; 3x3
  (let ((matrix (make-matrix 3 3 :matrix-type 'hermitian-matrix
				 :initial-contents '((#C(3.31 0.0) #C(1.26 -2.0) #C(1.37 -3.0))
										    (#C(1.26 2.0) #C(2.23  0.0) #C(2.31 -1.5))
										    (#C(1.37 3.0) #C(2.31  1.5) #C(8.15  0.0))))))
    (assert-num= #2A((#C( 2.602711    5.9604646E-8) #C(-0.64015717  2.808354)     #C(-0.7729426   0.04424542))
		     (#C(-0.64015717 -2.808354)     #C(3.9574066   -3.7252904E-9) #C( 0.019689279 0.96479553))
		     (#C(-0.7729426  -0.04424542)   #C(0.019689279 -0.96479553)   #C( 0.4409054  -7.241874E-10)))
      (ninvert matrix))
    (assert-num= #2A((#C(3.31       0.0)       #C( 0.38066468 -0.6042296) #C( 0.4138973   -0.9063445))
		     (#C(0.38066468 0.6042296) #C( 0.54190326  0.0)       #C(-0.044656467 -2.1882146))
		     (#C(0.4138973  0.9063445) #C(-0.044656467 2.1882146) #C( 2.2680602   -3.7252904E-9)))
      matrix)))
