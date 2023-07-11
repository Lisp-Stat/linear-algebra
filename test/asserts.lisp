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


(in-package :linear-algebra-test)


(defgeneric assert-passes? (type test expected actual)
  (:documentation "Return the result of the assertion.")
  (:method (type test expected actual)
    (ecase type
      ((equal-result failure-result)
       (and
        (<= (length expected) (length actual))
        ;; by putting expected in the second position we open up the ability
        ;; to use many more functions as tests (eg: typep)
        (every test actual expected)))
      (signal-result
       ;; These are lists of booleans
       (logically-equal (first expected) (first actual)))
      (error-result
       (or
        ;; todo: whats with eql?
        (eql (car actual) (car expected))
        (typep (car actual) (car expected))))
      (macro-result
       (%form-equal (first expected) (first actual)))
      (output-result
       (string=
        (string-trim '(#\newline #\return #\space) (car actual))
        (string-trim '(#\newline #\return #\space) (car expected)))))))

(defmacro expand-extras (extras)
  "Expand extra forms."
  `(lambda ()
     (list ,@(mapcan (lambda (form) (list `',form form)) extras))))

(defun internal-assert
    (type form code-thunk expected-thunk extras test &key full-form)
  "Perform the assertion and record the results."
  (let* ((actual (multiple-value-list (funcall code-thunk)))
         (expected (multiple-value-list (funcall expected-thunk)))
         (result (assert-passes? type test expected actual)))
    ;; Return the actual-values
    (apply #'values actual)))

(defmacro expand-assert (type form body expected extras
                         &key (test '#'eql)
                         full-form)
  "Expand the assertion to the internal format."
  `(internal-assert ,type ',form
    (lambda () ,body)
    (lambda () ,expected)
    (expand-extras ,extras)
    ,test
    :full-form (or ,full-form
                '(,type ,expected ,form))))

