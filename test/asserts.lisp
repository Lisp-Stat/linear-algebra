;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
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




(in-package #:clunit)

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
