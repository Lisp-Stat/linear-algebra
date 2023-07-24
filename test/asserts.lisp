;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
#|

  Rational tests and assertions for LISP-UNIT

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
(in-package #:num-utils.num=)

(defmethod num= ((a linear-algebra:data-vector) (b linear-algebra:data-vector)
                 &optional (tolerance *num=-tolerance*))
  (and
       (equal (linear-algebra::vector-element-type a) (linear-algebra::vector-element-type b))
       (num= (linear-algebra::contents a) (linear-algebra::contents b) tolerance)))

(defmethod num= ((a linear-algebra:data-vector) (b vector)
                 &optional (tolerance *num=-tolerance*))
  (num= (linear-algebra::contents a) b tolerance))

(defmethod num= ((a vector) (b linear-algebra:data-vector)
                 &optional (tolerance *num=-tolerance*))
  (num= b a tolerance))

(defmethod num= ((a linear-algebra:data-vector) (b cons)
                 &optional (tolerance *num=-tolerance*))
  (num= (linear-algebra::contents a)
        (linear-algebra:make-vector (length b) :initial-contents b)
        tolerance))


(defmethod num= ((a cons) (b linear-algebra:data-vector)
                 &optional (tolerance *num=-tolerance*))
  (num= b a tolerance))

(defmethod num= ((a linear-algebra:dense-matrix) (b simple-array)
                 &optional (tolerance *num=-tolerance*))
  (num= (linear-algebra::contents a) b tolerance))

(defmethod num= ((a simple-array) (b linear-algebra:dense-matrix)
                 &optional (tolerance *num=-tolerance*))
  (num= b a tolerance))

(defmethod num= ((a linear-algebra:dense-matrix) (b linear-algebra:dense-matrix)
                 &optional (tolerance *num=-tolerance*))
  (num= (linear-algebra::contents a)
        (linear-algebra::contents b)
        tolerance))


(in-package :clunit)

(defmacro assert-num= (value expression &body forms)
  "Evaluates EXPRESSION  as an assertion,  an assertion passes  if (EQ
VALUE EXPRESSION) values non nil. FORMS  and their values are printed if
the test fails."
  (with-gensyms (result)
    (assertion-expander :result            result
                        :test              `(gen-test-form #'num-utils:num=
                                                           (multiple-value-list ,value)
                                                           ,result)
                        :result-expression `(multiple-value-list ,expression)
                        :report-expression `(eq ,value ,expression)
                        :expected          value
                        :forms             forms)))


(export '(assert-num=))


