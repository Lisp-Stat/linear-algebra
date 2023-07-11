;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

  Rational tests and assertions for LISP-UNIT

  Copyright (c) 2009-2012, Thomas M. Hermann

  Permission is hereby granted, free of charge, to any person obtaining 
  a copy of this software and associated documentation files (the "Software"), 
  to deal in the Software without restriction, including without limitation 
  the rights to use, copy, modify, merge, publish, distribute, sublicense, 
  and/or sell copies of the Software, and to permit persons to whom the 
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included 
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
  OTHER DEALINGS IN THE SOFTWARE.

|#

(in-package :linear-algebra-test)

;;; Symbols exported from the rational extension

;;; Rational predicates and assertions

(defgeneric rational-equal (data1 data2)
  (:documentation
   "Return true if the rational data is equal."))

;;; (RATIONAL-EQUAL data1 data2) => boolean
(defmethod rational-equal ((data1 rational) (data2 rational))
  "Return true if the rational numbers are equal."
  (= data1 data2))

(defmethod rational-equal ((data1 complex) (data2 complex))
  "Return true if the complex parts are rational and equal."
  (if (and (typep data1 '(complex rational))
           (typep data2 '(complex rational)))
      (= data1 data2)
      (error "Rational equality is not applicable to complex values ~
              with floating point parts.")))

(defun %seq-rational-equal (seq1 seq2)
  "Return true if the sequences are equal length and at each position
the corresponding elements are equal."
  (or
   (and (null seq1) (null seq2))
   (and
    (= (length seq1) (length seq2))
    (every (lambda (d1 d2) (rational-equal d1 d2)) seq1 seq2))))

(defmethod rational-equal ((data1 list) (data2 list))
  "Return true if the lists are equal in length and element-wise
equal."
  (%seq-rational-equal data1 data2))

(defmethod rational-equal ((data1 list) (data2 vector))
  "Return true if the vector and the list are equal in length and
element-wise equal."
  (%seq-rational-equal data1 data2))

(defmethod rational-equal ((data1 vector) (data2 list))
  "Return true if the vector and the list are equal in length and
element-wise equal."
  (%seq-rational-equal data1 data2))

(defmethod rational-equal ((data1 vector) (data2 vector))
  "Return true if the vectors are equal in length and element-wise
equal."
  (%seq-rational-equal data1 data2))

(defmethod rational-equal ((data1 array) (data2 array))
  "Return true if the arrays are equal in dimension and element-wise
equal."
  (when (equal (array-dimensions data1)
               (array-dimensions data2))
    (%seq-rational-equal
     (make-array (array-total-size data1)
                 :element-type (array-element-type data1)
                 :displaced-to data1)
     (make-array (array-total-size data2)
                 :element-type (array-element-type data2)
                 :displaced-to data2))))

(defmethod rational-equal ((result1 sequence)
                           (result2 linear-algebra:data-vector))
  (rational-equal
   result1 (linear-algebra::contents result2)))

(defmethod rational-equal ((result1 linear-algebra:data-vector)
                           (result2 sequence))
  (rational-equal
   (linear-algebra::contents result1) result2))

(defmethod rational-equal ((result1 linear-algebra:data-vector)
                           (result2 linear-algebra:data-vector))
  (rational-equal (linear-algebra::contents result1)
                  (linear-algebra::contents result2)))

(defmethod rational-equal ((result1 list)
                           (result2 linear-algebra:dense-matrix))
  (let* ((contents (linear-algebra::contents result2))
         (rows (array-dimension contents 0))
         (columns (array-dimension contents 1))
         (data-row nil))
    (when (= rows (length result1))
      (dotimes (i0 rows t)
        (if (= columns
               (length (setf data-row (nth i0 result1))))
            (dotimes (i1 columns)
              (unless (rational-equal (elt data-row i1)
                                      (aref contents i0 i1))
                (return-from rational-equal)))
            (return-from rational-equal))))))

(defmethod rational-equal ((result1 vector)
                           (result2 linear-algebra:dense-matrix))
  (let* ((contents (linear-algebra::contents result2))
         (rows (array-dimension contents 0))
         (columns (array-dimension contents 1))
         (data-row nil))
    (when (= rows (length result1))
      (dotimes (i0 rows t)
        (if (= columns (length
                        (setf data-row (svref result1 i0))))
            (dotimes (i1 columns)
              (unless (rational-equal (elt data-row i1)
                                      (aref contents i0 i1))
                (return-from rational-equal)))
            (return-from rational-equal))))))

(defmethod rational-equal ((result1 array)
                           (result2 linear-algebra:dense-matrix))
  (rational-equal result1 (linear-algebra::contents result2)))

(defmethod rational-equal ((result1 linear-algebra:dense-matrix)
                           (result2 array))
  (rational-equal (linear-algebra::contents result1) result2))

(defmethod rational-equal ((result1 array)
                           (result2 linear-algebra:data-vector))
  (rational-equal result1 (linear-algebra::contents result2)))

(defmethod rational-equal ((result1 linear-algebra:data-vector)
                           (result2 array))
  (rational-equal (linear-algebra::contents result1) result2))

(defmethod rational-equal ((result1 linear-algebra:dense-matrix)
                           (result2 linear-algebra:dense-matrix))
  (rational-equal (linear-algebra::contents result1)
                  (linear-algebra::contents result2)))

(defmacro assert-rational-equal (expected form &rest extras)
  `(expand-assert 'equal-result ,form ,form ,expected ,extras :test #'rational-equal))
