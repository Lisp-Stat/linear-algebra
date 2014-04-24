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

;;; Data vector classes

(defclass data-vector ()
  ((contents
    :type (array * (*))
    :initarg :contents
    :accessor contents))
  (:documentation
   "A data vector."))

(defclass row-vector (data-vector)
  ()
  (:documentation
   "A row vector."))

(defclass column-vector (data-vector)
  ()
  (:documentation
   "A column vector."))

;;; Data vector interface operations

(defmethod initialize-vector
    ((vector data-vector) (data number) (size integer) element-type)
  "Initialize a data vector with a value."
  (setf
   (contents vector)
   (make-array
    size :element-type element-type :initial-element data))
  ;; Return the data vector
  vector)

(defmethod initialize-vector
    ((vector data-vector) (data sequence) (size integer) element-type)
  "Initialize a data vector with a sequence."
  (setf
   (contents vector)
   (make-array
    size :element-type element-type :initial-contents data))
  ;; Return the data vector
  vector)

(defun row-vector (&rest numbers)
  "Create a row vector from the numbers."
  (make-vector
   (length numbers)
   :vector-type 'row-vector :initial-contents numbers))

(defun column-vector (&rest numbers)
  "Create a column vector from the numbers."
  (make-vector
   (length numbers)
   :vector-type 'column-vector :initial-contents numbers))

(defun row-vector-p (object)
  "Return true if object is a row-vector, NIL otherwise."
  (typep object 'row-vector))

(defun column-vector-p (object)
  "Return true if object is a column-vector, NIL otherwise."
  (typep object 'column-vector))

(defmethod vector-in-bounds-p ((vector data-vector) (index integer))
  "Return true if index does not exceed the dimensions of vector."
  (array-in-bounds-p (contents vector) index))

(defmethod vector-element-type ((vector data-vector))
  "Return the element type of vector."
  (array-element-type (contents vector)))

(defmethod vector-length ((vector data-vector))
  "Return the length of the vector."
  (length (contents vector)))

(defmethod vref ((vector data-vector) (index integer))
  "Return the element of vector at index."
  (aref (contents vector) index))

(defmethod (setf vref) ((data number) (vector data-vector) (index integer))
  "Set the element of vector at index to data."
  (setf (aref (contents vector) index) data))

(defmethod copy-vector ((vector data-vector))
  "Return a copy of the vector."
  (make-instance
   (class-of vector)
   :contents (copy-seq (contents vector))))

(defmethod subvector ((vector data-vector) start &optional end)
  "Return a new data vector that is a subset of vector."
  (make-instance
   (class-of vector)
   :contents (subseq (contents vector) start end)))

(defmethod (setf subvector)
    ((subvector data-vector) (vector data-vector) start &optional end)
  "Set the subvector of the data vector."
  (setf
   (subseq (contents vector) start end)
   (contents subvector))
  ;; Return the subvector
  subvector)

(defmethod replace-vector
    ((vector1 data-vector) (vector2 data-vector)
     &key (start1 0) end1 (start2 0) end2)
  "Destructively replace the elements of vector1 with vector2."
  (replace (contents vector1) (contents vector2)
           :start1 start1 :end1 end1 :start2 start2 :end2 end2)
  ;; Return vector1
  vector1)

;;; Data vector iteration operations
(defun %map-data-vector
       (result-type function first-vector &rest more-vectors)
  "Non-validating version of map-vector."
  (make-instance
   result-type
   :contents
   (apply #'map
          (class-of (contents first-vector))
          function
          (contents first-vector)
          (mapcar #'contents more-vectors))))

(defmethod map-vector :before
  (result-type
   (function function)
   (first-vector data-vector)
   &rest more-vectors)
  "Verify the arguments to map-vector."
  (declare (ignore function))
  (unless (subtypep result-type 'data-vector)
    (error "~A is not a subtype of DATA-VECTOR." result-type))
  (unless (every (lambda (x) (typep x 'data-vector)) more-vectors)
    (error "All vectors must be data vectors.")))

(defmethod map-vector
    (result-type
     (function function)
     (first-vector data-vector)
     &rest more-vectors)
  "Calls function on successive sets of data vectors."
  (apply #'%map-data-vector
         result-type
         function
         first-vector
         more-vectors))

(defun %map-into-data-vector (result-vector function &rest vectors)
  "Non-validating version of map-into-vector."
  (apply #'map-into
         (contents result-vector)
         function
         (mapcar #'contents vectors))
  ;; Return the result vector
  result-vector)

(defmethod map-into-vector :before
  ((result-vector data-vector) (function function) &rest vectors)
  "Verify the arguments to map-into-vector."
  (declare (ignore result-vector function))
  (unless (every (lambda (x) (typep x 'data-vector)) vectors)
    (error "All vectors must be data vectors.")))

(defmethod map-into-vector ((result-vector data-vector)
                            (function function) &rest vectors)
  "Destructively modifies the result vector with the result of
applying the function to each element of the vectors."
  (apply #'%map-into-data-vector
         result-vector
         function
         vectors))

;;; Data vector transformations

(defmethod apply-rotation :before
  ((vector1 data-vector) (vector2 data-vector) cc ss)
  "Verify the input to apply-rotation."
  (declare (ignore cc ss))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod apply-rotation
    ((vector1 data-vector) (vector2 data-vector) cc ss)
  "Return the plane rotations of vector1 and vector2 by cc and ss."
  (let ((rvec1
         (make-vector
          (vector-length vector1)
          :vector-type (class-of vector1)
          :element-type (vector-element-type vector1)))
        (rvec2
         (make-vector
          (vector-length vector2)
          :vector-type (class-of vector2)
          :element-type (vector-element-type vector2))))
    (dotimes (pos (vector-length vector1) (values rvec1 rvec2))
      (setf
       ;; Update result vector 1
       (vref rvec1 pos)
       (+ (* cc (vref vector1 pos))
          (* ss (vref vector2 pos)))
       ;; Update result vector 2
       (vref rvec2 pos)
       (+ (* -1 (conjugate ss) (vref vector1 pos))
          (* cc (vref vector2 pos)))))))

(defmethod napply-rotation :before
  ((vector1 data-vector) (vector2 data-vector) cc ss)
  "Verify the input to napply-rotation."
  (declare (ignore cc ss))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod napply-rotation
    ((vector1 data-vector) (vector2 data-vector) cc ss)
  "Return the plane rotations of vector1 and vector2 by cc and ss."
  (dotimes (pos (vector-length vector1) (values vector1 vector2))
    (psetf
     ;; Update result vector 1
     (vref vector1 pos)
     (+ (* cc (vref vector1 pos))
        (* ss (vref vector2 pos)))
     ;; Update result vector 2
     (vref vector2 pos)
     (+ (* -1 (conjugate ss) (vref vector1 pos))
        (* cc (vref vector2 pos))))))

;;; Data vector fundamental operations

(defmethod sumsq ((vector data-vector))
  "Return the scaling parameter and the sum of the squares of vector."
  (sumsq-vector (contents vector)))

(defmethod sump ((vector data-vector) (p number))
  "Return the scaling parameter and the sum of the P powers of vector."
  (sump-vector (contents vector) p))

(defmethod norm ((vector data-vector) &key (measure 1))
  "Return the p-norm of the vector."
  (norm-vector (contents vector) measure))

(defmethod transpose ((vector column-vector) &key conjugate)
  "Return a row vector."
  (make-instance
   'row-vector
   :contents
   (transpose (contents vector) :conjugate conjugate)))

(defmethod transpose ((vector row-vector) &key conjugate)
  "Return a column vector."
  (make-instance
   'column-vector
   :contents
   (transpose (contents vector) :conjugate conjugate)))

(defmethod ntranspose ((vector column-vector) &key conjugate)
  "Return a row vector destructively."
  (ntranspose (contents vector) :conjugate conjugate)
  (change-class vector 'row-vector))

(defmethod ntranspose ((vector row-vector) &key conjugate)
  "Return a column vector destructively."
  (ntranspose (contents vector) :conjugate conjugate)
  (change-class vector 'column-vector))

(defmethod permute :before
  ((vector row-vector) (matrix permutation-matrix))
  "Verify that the dimensions are compatible."
  (unless (= (vector-length vector) (matrix-row-dimension matrix))
    (error "Vector and permutation matrix sizes incompatible.")))

(defmethod permute ((vector row-vector) (matrix permutation-matrix))
  "Return the permutation of the row vector."
  (make-instance
   'row-vector
   :contents
   (right-permute-vector (contents vector) (contents matrix))))

(defmethod permute :before
  ((matrix permutation-matrix) (vector column-vector))
  "Verify that the dimensions are compatible."
  (unless (= (vector-length vector) (matrix-column-dimension matrix))
    (error "Vector and permutation matrix sizes incompatible.")))

(defmethod permute ((matrix permutation-matrix) (vector column-vector))
  "Return the permutation of the column vector."
  (make-instance
   'row-vector
   :contents
   (left-permute-vector (contents matrix) (contents vector))))

(defmethod scale ((scalar number) (vector data-vector))
  "Return the vector scaled by scalar."
  (make-instance
   (class-of vector)
   :contents
   (scale scalar (contents vector))))

(defmethod nscale ((scalar number) (vector data-vector))
  "Return the vector destructively scaled by scalar."
  (nscale scalar (contents vector))
  ;; Return the vector
  vector)

(defmethod add :before
  ((vector1 data-vector) (vector2 data-vector) &key scalar1 scalar2)
  "Verify that the dimensions are equal."
  (declare (ignore scalar1 scalar2))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod add
    ((vector1 column-vector) (vector2 column-vector)
     &key scalar1 scalar2)
  "Return the addition of scalar1*vector1 with scalar2*vector2."
  (make-instance
   (common-class-of vector1 vector2 'column-vector)
   :contents
   (add-vector
    (contents vector1) (contents vector2) scalar1 scalar2)))

(defmethod add
    ((vector1 row-vector) (vector2 row-vector)
     &key scalar1 scalar2)
  "Return the addition of scalar1*vector1 with scalar2*vector2."
  (make-instance
   (common-class-of vector1 vector2 'row-vector)
   :contents
   (add-vector
    (contents vector1) (contents vector2) scalar1 scalar2)))

(defmethod nadd :before
  ((vector1 data-vector) (vector2 data-vector)
   &key scalar1 scalar2)
  "Verify that the dimensions are equal."
  (declare (ignore scalar1 scalar2))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length in NADD-SCALED.")))

(defmethod nadd
    ((vector1 column-vector) (vector2 column-vector)
     &key scalar1 scalar2)
  "Return the addition of scalar2*vector2 to scalar1*vector1."
  (nadd-vector
   (contents vector1) (contents vector2) scalar1 scalar2)
  ;; Return vector1
  vector1)

(defmethod nadd
    ((vector1 row-vector) (vector2 row-vector)
     &key scalar1 scalar2)
  "Return the addition of scalar2*vector2 to scalar1*vector1."
  (nadd-vector
   (contents vector1) (contents vector2) scalar1 scalar2)
  ;; Return vector1
  vector1)

(defmethod subtract :before
  ((vector1 data-vector) (vector2 data-vector)
   &key scalar1 scalar2)
  "Verify that the dimensions are equal."
  (declare (ignore scalar1 scalar2))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod subtract
    ((vector1 column-vector) (vector2 column-vector)
     &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (make-instance
   (common-class-of vector1 vector2 'column-vector)
   :contents
   (subtract-vector
    (contents vector1) (contents vector2) scalar1 scalar2)))

(defmethod subtract
    ((vector1 row-vector) (vector2 row-vector)
     &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (make-instance
   (common-class-of vector1 vector2 'row-vector)
   :contents
   (subtract-vector
    (contents vector1) (contents vector2) scalar1 scalar2)))

(defmethod nsubtract :before
  ((vector1 data-vector) (vector2 data-vector)
   &key scalar1 scalar2)
  "Verify that the dimensions are equal."
  (declare (ignore scalar1 scalar2))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod nsubtract
    ((vector1 column-vector) (vector2 column-vector)
     &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (nsubtract-vector
   (contents vector1) (contents vector2) scalar1 scalar2)
  ;; Return vector1
  vector1)

(defmethod nsubtract
    ((vector1 row-vector) (vector2 row-vector)
     &key scalar1 scalar2)
  "Return the subraction of scalar2*vector2 from scalar1*vector1."
  (nsubtract-vector
   (contents vector1) (contents vector2) scalar1 scalar2)
  ;; Return vector1
  vector1)

(defmethod product :before
  ((vector1 row-vector) (vector2 column-vector)
   &key scalar conjugate)
  "Verify that the dimensions are equal."
  (declare (ignore scalar conjugate))
  (unless (= (vector-length vector1) (vector-length vector2))
    (error "VECTOR1 and VECTOR2 are not of equal length.")))

(defmethod product
    ((vector1 row-vector) (vector2 column-vector)
     &key scalar conjugate)
  "Return the dot product of vector1 and vector2."
  (inner-product-vector
   (contents vector1) (contents vector2) scalar conjugate))
