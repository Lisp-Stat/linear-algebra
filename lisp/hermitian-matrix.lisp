#|

  Linear Algebra in Common Lisp

  Copyright (c) 2011-2014, Odonata Research LLC

  Permission is hereby granted, free  of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction,  including without limitation the rights
  to use, copy, modify,  merge,  publish,  distribute,  sublicense, and/or sell
  copies of the  Software,  and  to  permit  persons  to  whom  the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and  this  permission  notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED  "AS IS",  WITHOUT  WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT  NOT  LIMITED  TO  THE  WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE  AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT  HOLDERS  BE  LIABLE  FOR  ANY  CLAIM,  DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

|#

(in-package :linear-algebra)

(defclass hermitian-matrix (square-matrix)
  ()
  (:documentation
   "Hermitian matrix object."))

;;; Hermitian matrix interface operations

(defun hermitian-matrix-p (object)
  "Return true if object is a hermitian-matrix, NIL otherwise."
  (typep object 'hermitian-matrix))

(defun %initialize-hermitian-matrix-with-seq
       (matrix data rows columns element-type)
  "Initialize and validate a Hermitian matrix with a sequence."
  (let ((contents
         (setf
          (contents matrix)
          (make-array
           (list rows columns)
           :element-type element-type
           :initial-contents data))))
    (dotimes (i0 rows matrix)
      (if (zerop (imagpart (aref contents i0 i0)))
          (dotimes (i1 i0)
            (unless
                (complex-equal
                 (aref contents i0 i1)
                 (conjugate (aref contents i1 i0)))
              (error "The data is not Hermitian.")))
          (error "The data is not Hermitian.")))))

(defmethod initialize-matrix
    ((matrix hermitian-matrix) (data complex)
     (rows integer) (columns integer) element-type)
  "It is an error to initialize a Hermitian matrix with a complex
element."
  (declare (ignore data rows columns element-type))
  (error
   "The initial element for a ~A must be real." (type-of matrix)))

(defmethod initialize-matrix
    ((matrix hermitian-matrix) (data list)
     (rows integer) (columns integer) element-type)
  "Initialize the Hermitian matrix with a nested sequence."
  (%initialize-hermitian-matrix-with-seq
   matrix data rows columns element-type))

(defmethod initialize-matrix
    ((matrix hermitian-matrix) (data vector)
     (rows integer) (columns integer) element-type)
  "Initialize the Hermitian matrix with a nested sequence."
  (%initialize-hermitian-matrix-with-seq
   matrix data rows columns element-type))

(defmethod initialize-matrix
    ((matrix hermitian-matrix) (data array)
     (rows integer) (columns integer) element-type)
  "Initialize the Hermitian matrix with a 2D array."
  (let ((contents
         (setf
          (contents matrix)
          (make-array
           (list rows columns)
           :element-type element-type))))
    (dotimes (i0 rows matrix)
      (if (zerop
           (imagpart
            (setf (aref contents i0 i0) (aref data i0 i0))))
          (dotimes (i1 i0)
            (unless
                (complex-equal
                 (setf
                  (aref contents i0 i1)
                  (aref data i0 i1))
                 (conjugate
                  (setf
                   (aref contents i1 i0)
                   (aref data i1 i0))))
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
       (right-permute-array (contents matrix) (contents permutation)))
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
       (left-permute-array (contents permutation) (contents matrix)))
      (error
       "Permutation matrix~A and dense matrix~A sizes incompatible."
       (matrix-dimensions matrix)
       (matrix-dimensions permutation))))
