;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra)

(defclass hermitian-matrix (square-matrix)
  ()
  (:documentation "Hermitian matrix object."))

;;; Hermitian matrix interface operations

(defun hermitian-matrix-p (object)
  "Return true if object is a hermitian-matrix, NIL otherwise."
  (typep object 'hermitian-matrix))

(defun %initialize-hermitian-matrix-with-seq
       (matrix data dimensions element-type)
  "Initialize and validate a Hermitian matrix with a sequence."
  (let ((contents
         (setf
          (contents matrix)
          (make-array
           dimensions
           :element-type element-type
           :initial-contents data))))
    (dotimes (i0 (first dimensions) matrix)
      (if (zerop (imagpart (aref contents i0 i0)))
          (dotimes (i1 i0)
            (unless
                (complex-equal
                 (aref contents i0 i1)
                 (conjugate (aref contents i1 i0)))
              (error "The data is not Hermitian.")))
          (error "The data is not Hermitian.")))))

(defmethod initialize-matrix-contents
    ((matrix hermitian-matrix) (initial-element complex) initargs)
  "It is an error to initialize a Hermitian matrix with a complex
element."
  (error
   "The initial element for a ~A must be real." (type-of matrix)))

(defmethod initialize-matrix-contents
    ((matrix hermitian-matrix) (initial-contents list) initargs)
  "Initialize the Hermitian matrix with a nested sequence."
  (%initialize-hermitian-matrix-with-seq
   matrix initial-contents
   (getf initargs :dimensions)
   (getf initargs :element-type)))

(defmethod initialize-matrix-contents
    ((matrix hermitian-matrix) (initial-contents vector) initargs)
  "Initialize the Hermitian matrix with a nested sequence."
  (%initialize-hermitian-matrix-with-seq
   matrix initial-contents
   (getf initargs :dimensions)
   (getf initargs :element-type)))

(defmethod initialize-matrix-contents
    ((matrix hermitian-matrix) (initial-contents array) initargs)
  "Initialize the Hermitian matrix with a 2D array."
  (let* ((dimensions (getf initargs :dimensions))
         (contents
          (setf
           (contents matrix)
           (make-array
            dimensions
            :element-type (getf initargs :element-type)))))
    (dotimes (i0 (first dimensions) matrix)
      (if (zerop
           (imagpart
            (setf
             (aref contents i0 i0)
             (aref initial-contents i0 i0))))
          (dotimes (i1 i0)
            (unless
                (complex-equal
                 (setf
                  (aref contents i0 i1)
                  (aref initial-contents i0 i1))
                 (conjugate
                  (setf
                   (aref contents i1 i0)
                   (aref initial-contents i1 i0))))
              (error "The data is not Hermitian.")))
          (error "The data is not Hermitian.")))))

(defmethod (setf mref)
    ((data number) (matrix hermitian-matrix)
     (row integer) (column integer))
  "Set the element at row,column of matrix to data."
  (if (= row column)
      (unless
          (zerop
           (imagpart
            (setf (aref (contents matrix) row column) data)))
        (error "Diagonal Hermitian matrix elements must have a zero ~
                imaginary component."))
      (setf
       (aref (contents matrix) row column) data
       (aref (contents matrix) column row) (conjugate data))))

(defun %setf-hermitian-submatrix-on-diagonal
       (matrix data row numrows)
  (let ((mat (contents matrix))
        (dat (contents data)))
    (do ((di0 0   (1+ di0))
         (mi0 row (1+ mi0)))
        ((>= di0 numrows) data)         ; Return the data
      (setf (aref mat mi0 mi0) (aref dat di0 di0))
      (do ((di1 (1+ di0)      (1+ di1))
           (mi1 (+ 1 row di0) (1+ mi1)))
          ((>= di1 numrows))
        (setf
         (aref mat mi0 mi1) (aref dat di0 di1)
         (aref mat mi1 mi0) (aref dat di1 di0))))))

(defun %setf-hermitian-submatrix-off-diagonal
       (matrix data row column numrows numcols)
  (let ((mat (contents matrix))
        (dat (contents data)))
    (do ((di0 0   (1+ di0))
         (mi0 row (1+ mi0)))
        ((>= di0 numrows) data)         ; Return the data
      (do ((di1 0      (1+ di1))
           (mi1 column (1+ mi1)))
          ((>= di1 numcols))
        (setf
         (aref mat mi0 mi1) (aref dat di0 di1)
         (aref mat mi1 mi0) (conjugate (aref dat di0 di1)))))))

(defmethod submatrix
    ((matrix hermitian-matrix)
     (start-row integer) (start-column integer)
     &key end-row end-column)
  "Return a matrix created from the submatrix of matrix."
  (multiple-value-bind (start-row start-column end-row end-column)
      (matrix-validated-range
       matrix start-row start-column end-row end-column)
    (let* ((m-rows (- end-row start-row))
           (n-columns (- end-column start-column))
           (original (contents matrix))
           (contents
            (make-array
             (list m-rows n-columns)
             :element-type (matrix-element-type matrix))))
      (make-instance
       (cond
        ((and (= start-row start-column) (= end-row end-column))
         'hermitian-matrix)
        ((= m-rows n-columns)
         'square-matrix)
        (t 'dense-matrix))
       :contents
       (dotimes (row m-rows contents)
         (dotimes (column n-columns)
           (setf
            (aref contents row column)
            (aref original (+ start-row row)
                  (+ start-column column)))))))))

(defmethod (setf submatrix)
    ((data hermitian-matrix) (matrix hermitian-matrix)
     (start-row integer) (start-column integer)
     &key end-row end-column)
  "Set a submatrix of the matrix."
  (multiple-value-bind (start-row start-column end-row end-column)
      (matrix-validated-range
       matrix start-row start-column end-row end-column)
    (let ((m-rows
           (min
            (- end-row start-row)
            (matrix-row-dimension data)))
          (n-columns
           (min
            (- end-column start-column)
            (matrix-column-dimension data))))
      (cond
        ((and (= start-row start-column) (= m-rows n-columns))
         (%setf-hermitian-submatrix-on-diagonal
          matrix data start-row m-rows))
        ((or (< (+ start-row m-rows -1) start-column)
             (< (+ start-column n-columns -1) start-row))
         (%setf-hermitian-submatrix-off-diagonal
          matrix data start-row start-column m-rows n-columns))
        (t
         (error
          "Range(~D:~D,~D:~D) results in a non-Hermitian matrix."
          start-row end-row start-column end-column))))))

(defmethod (setf submatrix)
    ((data dense-matrix) (matrix hermitian-matrix)
     (start-row integer) (start-column integer)
     &key end-row end-column)
  "Set a submatrix of the matrix."
  (multiple-value-bind (start-row start-column end-row end-column)
      (matrix-validated-range
       matrix start-row start-column end-row end-column)
    (let ((m-rows
           (min
            (- end-row start-row)
            (matrix-row-dimension data)))
          (n-columns
           (min
            (- end-column start-column)
            (matrix-column-dimension data))))
      (if (or (< (+ start-row m-rows -1) start-column)
              (< (+ start-column n-columns -1) start-row))
          (%setf-hermitian-submatrix-off-diagonal
           matrix data start-row start-column m-rows n-columns)
          (error
           "Range(~D:~D,~D:~D) results in a non-Hermitian matrix."
           start-row end-row start-column end-column)))))

(defun %replace-hermitian-matrix-on-diagonal
       (matrix1 matrix2 row1 column1 row2 column2 numrows numcols)
  "Destructively replace a subset on the diagonal of matrix1 with
matrix2."
  (let ((contents1 (contents matrix1))
        (contents2 (contents matrix2)))
    (do ((   i0 0    (1+ i0))
         (m1-i0 row1 (1+ m1-i0))
         (m2-i0 row2 (1+ m2-i0)))
        ((>= i0 numrows) matrix1)       ; Return matrix1
      (setf (aref contents1 m1-i0 m1-i0) (aref contents2 m2-i0 m2-i0))
      (do ((   i1 (1+ i0)          (1+ i1))
           (m1-i1 (+ 1 column1 i0) (1+ m1-i1))
           (m2-i1 (+ 1 column2 i0) (1+ m2-i1)))
          ((>= i1 numcols))
        (setf
         (aref contents1 m1-i0 m1-i1) (aref contents2 m2-i0 m2-i1)
         (aref contents1 m1-i1 m1-i0) (aref contents2 m2-i1 m2-i0))))))

(defun %replace-hermitian-matrix-off-diagonal
       (matrix1 matrix2 row1 column1 row2 column2 numrows numcols)
  "Destructively replace a subset off the diagonal of matrix1 with
matrix2."
  (let ((contents1 (contents matrix1))
        (contents2 (contents matrix2)))
    (do ((   i0 0    (1+ i0))
         (m1-i0 row1 (1+ m1-i0))
         (m2-i0 row2 (1+ m2-i0)))
        ((>= i0 numrows) matrix1)       ; Return matrix1
      (do ((   i1 0       (1+ i1))
           (m1-i1 column1 (1+ m1-i1))
           (m2-i1 column2 (1+ m2-i1)))
          ((>= i1 numcols))
        (setf
         (aref contents1 m1-i0 m1-i1)
         (aref contents2 m2-i0 m2-i1)
         (aref contents1 m1-i1 m1-i0)
         (conjugate (aref contents2 m2-i0 m2-i1)))))))

(defmethod replace-matrix
    ((matrix1 hermitian-matrix) (matrix2 hermitian-matrix)
     &key (start-row1 0) end-row1
     (start-column1 0) end-column1
     (start-row2 0) end-row2
     (start-column2 0) end-column2)
  "Replace the elements of matrix1 with matrix2."
  (multiple-value-bind (start-row1 start-column1 end-row1 end-column1)
      (matrix-validated-range
       matrix1 start-row1 start-column1 end-row1 end-column1)
    (multiple-value-bind
        (start-row2 start-column2 end-row2 end-column2)
        (matrix-validated-range
         matrix2 start-row2 start-column2 end-row2 end-column2)
      (let ((m-rows
             (min
              (- end-row1 start-row1)
              (- end-row2 start-row2)))
            (n-columns
             (min
              (- end-column1 start-column1)
              (- end-column2 start-column2))))
        (cond
          ((and (= start-row1 start-column1)
                (= start-row2 start-column2)
                (= m-rows n-columns))
           (%replace-hermitian-matrix-on-diagonal
            matrix1 matrix2
            start-row1 start-column1
            start-row2 start-column2
            m-rows n-columns))
          ((or (< (+ start-row1 m-rows -1) start-column1)
               (< (+ start-column1 n-columns -1) start-row1))
           (%replace-hermitian-matrix-off-diagonal
            matrix1 matrix2
            start-row1 start-column1
            start-row2 start-column2
            m-rows n-columns))
          (t
           (error
            "Range(~D:~D,~D:~D) results in a non-Hermitian matrix."
            start-row1 (+ start-row1 m-rows -1)
            start-column1 (+ start-column1 n-columns -1))))))))

(defmethod replace-matrix
    ((matrix1 hermitian-matrix) (matrix2 dense-matrix)
     &key (start-row1 0) end-row1
     (start-column1 0) end-column1
     (start-row2 0) end-row2
     (start-column2 0) end-column2)
  "Replace the elements of matrix1 with matrix2."
  (multiple-value-bind (start-row1 start-column1 end-row1 end-column1)
      (matrix-validated-range
       matrix1 start-row1 start-column1 end-row1 end-column1)
    (multiple-value-bind
        (start-row2 start-column2 end-row2 end-column2)
        (matrix-validated-range
         matrix2 start-row2 start-column2 end-row2 end-column2)
      (let ((m-rows
             (min
              (- end-row1 start-row1)
              (- end-row2 start-row2)))
            (n-columns
             (min
              (- end-column1 start-column1)
              (- end-column2 start-column2))))
        (if (or (< (+ start-row1 m-rows -1) start-column1)
                (< (+ start-column1 n-columns -1) start-row1))
            (%replace-hermitian-matrix-off-diagonal
             matrix1 matrix2
             start-row1 start-column1
             start-row2 start-column2
             m-rows n-columns)
            (error
             "Range(~D:~D,~D:~D) results in a non-Hermitian matrix."
             start-row1 (+ start-row1 m-rows -1)
             start-column1 (+ start-column1 n-columns -1)))))))

(defmethod transpose ((matrix hermitian-matrix))
  "The transpose of a Hermitian matrix is itself."
  (copy-matrix matrix))

(defmethod ntranspose ((matrix hermitian-matrix))
  "The destructive transpose of a Hermitian matrix is itself."
  matrix)

(defmethod permute
    ((matrix hermitian-matrix) (permutation permutation-matrix))
  (if (every
       #'= (matrix-dimensions matrix) (matrix-dimensions permutation))
      (make-instance
       'square-matrix
       :contents
       (right-permute (contents matrix) (contents permutation)))
      (error
       "Dense matrix~A and permutation matrix~A sizes incompatible."
       (matrix-dimensions matrix)
       (matrix-dimensions permutation))))

(defmethod permute
    ((permutation permutation-matrix) (matrix hermitian-matrix))
  (if (every
       #'= (matrix-dimensions permutation) (matrix-dimensions matrix))
      (make-instance
       'square-matrix
       :contents
       (left-permute (contents permutation) (contents matrix)))
      (error
       "Permutation matrix~A and dense matrix~A sizes incompatible."
       (matrix-dimensions matrix)
       (matrix-dimensions permutation))))

(defmethod solve ((matrix hermitian-matrix) (vector column-vector))
  "Return the solution to the system of equations."
  (make-instance
   'column-vector
   :contents
   (hermitian-cholesky-solver
    (copy-array (contents matrix)) (copy-array (contents vector)))))

(defmethod nsolve ((matrix hermitian-matrix) (vector column-vector))
  "Return the solution to the system of equations."
  (setf
   (contents vector)
   (hermitian-cholesky-solver (contents matrix) (contents vector)))
  ;; Return the solution vector
  vector)

(defmethod invert ((matrix hermitian-matrix))
  "Return the invert of the hermitian matrix."
  (make-instance
   (class-of matrix)
   :contents
   (hermitian-cholesky-invert
    (copy-array (contents matrix)))))

(defmethod ninvert ((matrix hermitian-matrix))
  "Return the invert of the hermitian matrix."
  (make-instance
   (class-of matrix)
   :contents
   (hermitian-cholesky-invert
    (contents matrix))))
