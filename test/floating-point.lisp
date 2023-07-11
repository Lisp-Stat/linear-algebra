;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
#|

  Rational tests and assertions for linear algebra.

  Copyright (c) 2009-2012, Thomas M. Hermann
  Copyright (c) 2023 Symbolics Pte Ltd
  Copyright (c) 2023 Ten Factor Growth, LLC

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
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

;;; Symbols exported from the floating point extension

;;; TODO: put these in the package file
;;; Global variables
(export
 '(*measure* *epsilon* *significant-figures*))


;;; Floating point extensions

(defvar *measure* 1)

(defvar *epsilon* 1E-8
  "Set the error epsilon if the defaults are not acceptable.")

(defvar *significant-figures* 4
  "Default to 4 significant figures.")

(defgeneric default-epsilon (value)
  (:documentation
   "Return the default epsilon for the value.")
  (:method (value)
    (typecase value
      (array
       (or (loop for v across value
                 maximize (default-epsilon v))
           0))
      (list
       (or (loop for v in value
                 maximize (default-epsilon v))
           0))
      (short-float  (* 2S0 short-float-epsilon))
      (single-float (* 2F0 single-float-epsilon))
      (double-float (* 2D0 double-float-epsilon))
      (long-float   (* 2L0 long-float-epsilon))
      (number 0))))

(defgeneric relative-error (exact approximate)
  (:documentation
   "Return the relative-error between the 2 quantities."))

(defmethod relative-error ((exact float) (approximate float))
  "Return the error delta between the exact and approximate floating
point value."
  (%relative-error exact approximate))

(defmethod relative-error ((exact float) (approximate complex))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate float))
  "Return the relative error between the float and complex number."
  (%relative-error exact approximate))

(defmethod relative-error ((exact complex) (approximate complex))
  "Return the relative error of the complex numbers."
  (if (or (typep exact '(complex float))
          (typep approximate '(complex float)))
      (%relative-error exact approximate)
      (error "Relative error is only applicable to complex values with ~
              floating point parts.")))

(defgeneric float-equal (data1 data2 &optional epsilon)
  (:documentation
   "Return true if the floating point data is equal."))

(defmethod float-equal (x y &optional (epsilon *epsilon*))
  (declare (ignore epsilon))
  (cond ((and (null x) (null y)) t)
        ((or (null x) (null y)) nil)
        (t nil)))


(defmethod float-equal ((x number) (y number) &optional (epsilon *epsilon*))
  (or
   (and (zerop x) (zerop y))
   (< (%relative-error x y) epsilon)))

(defmethod float-equal ((x sequence) (y sequence) &optional (epsilon *epsilon*))
  (cond ((not (= (length x) (length y))) nil)
        (t 
         (every
          (lambda (d1 d2) (float-equal d1 d2 epsilon))
          x y))))

(defmethod float-equal ((x array) (y array) &optional (epsilon *epsilon*))
  (cond ((not (= (array-rank x) (array-rank y))) nil)
        (t
         (loop
           for index :below (array-total-size x)
           always (float-equal (row-major-aref x index)
                               (row-major-aref y index)
                               epsilon)))))

(defmethod float-equal ((result1 sequence)
                        (result2 linear-algebra:data-vector)
                        &optional (epsilon *epsilon*))
  (float-equal
   result1 (linear-algebra::contents result2) epsilon))


(defmethod float-equal ((result1 linear-algebra:data-vector)
                        (result2 linear-algebra:data-vector)
                        &optional (epsilon *epsilon*))
  (float-equal (linear-algebra::contents result1)
               (linear-algebra::contents result2)
               epsilon))

(defmethod float-equal ((result1 list)
                        (result2 linear-algebra:dense-matrix)
                        &optional (epsilon *epsilon*))
  (let* ((contents (linear-algebra::contents result2))
         (rows (array-dimension contents 0))
         (columns (array-dimension contents 1))
         (data-row nil))
    (when (= rows (length result1))
      (dotimes (i0 rows t)
        (if (= columns
               (length (setf data-row (nth i0 result1))))
            (dotimes (i1 columns)
              (unless (float-equal (elt data-row i1)
                                   (aref contents i0 i1)
                                   epsilon)
                (return-from float-equal)))
            (return-from float-equal))))))

(defmethod float-equal ((result1 vector)
                        (result2 linear-algebra:dense-matrix)
                        &optional (epsilon *epsilon*))
  (let* ((contents (linear-algebra::contents result2))
         (rows (array-dimension contents 0))
         (columns (array-dimension contents 1))
         (data-row nil))
    (when (= rows (length result1))
      (dotimes (i0 rows t)
        (if (= columns (lengthp
                        (setf data-row (svref result1 i0))))
            (dotimes (i1 columns)
              (unless (float-equal (elt data-row i1)
                                   (aref contents i0 i1)
                                   epsilon)
                (return-from float-equal)))
            (return-from float-equal))))))


(defmethod float-equal ((result1 array)
                        (result2 linear-algebra:dense-matrix)
                        &optional (epsilon *epsilon*))
  (float-equal
   result1 (linear-algebra::contents result2) epsilon))

(defmethod float-equal ((result1 linear-algebra:dense-matrix)
                        (result2 linear-algebra:dense-matrix)
                        &optional (epsilon *epsilon*))
  (float-equal (linear-algebra::contents result1)
               (linear-algebra::contents result2)
               epsilon))

#|
  (RELATIVE-ERROR x y) => float
  [NumAlgoC] : Definition 1.3, pg. 2
               modified with Definition 1.1, pg. 1
 
  The definition of relative error in this routine is modified from
  the Definition 1.3 in [NumAlgoC] for cases when either the exact
  or the approximate value equals zero. According to Definition 1.3,
  the relative error is identically equal to 1 in those cases. This
  function returns the absolute error in those cases. This is more
  useful for testing.
|#
(defun %relative-error (exact approximate)
  "Return the relative error of the numbers."
  (abs (if (or (zerop exact) (zerop approximate))
	   (- exact approximate)
	   (/ (- exact approximate) exact))))

(defmacro assert-float-equal (expected form &rest extras)
  `(expand-assert 'equal-result ,form ,form ,expected ,extras :test #'float-equal))

(defun %complex-float-random (limit &optional (state *random-state*))
  "Return a random complex float number."
  (complex
   (random (realpart limit) state)
   (random (imagpart limit) state)))

(defun %complex-rational-random (limit &optional (state *random-state*))
  "Return a random complex rational number."
  (let ((imaglimit (imagpart limit)))
    (if (< 1 imaglimit)
        (complex
         (random (realpart limit) state)
         ;; Ensure that the imaginary part is not zero.
         (do ((im (random imaglimit state)
                  (random imaglimit state)))
             ((< 0 im) im)))
        (error "Imaginary part must be greater than 1."))))

(defun complex-random (limit &optional (state *random-state*))
  "Return a random complex number. "
  (check-type limit complex)
  (if (typep limit '(complex rational))
      (%complex-rational-random limit state)
      (%complex-float-random limit state)))

(defun make-random-list (size &optional (limit 1.0))
  "Return a list of random numbers."
  (mapcar (if (complexp limit) #'complex-random #'random)
	  (make-list size :initial-element limit)))
