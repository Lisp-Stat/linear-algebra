;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Unit Tests for Linear Algebra in Common Lisp

(uiop:define-package #:linear-algebra-test
  (:use #:cl #:clunit #:linear-algebra)
  (:import-from #:num-utils.arithmetic  #:sum #:seq-min #:seq-max #:square)
  (:import-from #:num-utils.elementwise #:eexpt #:eabs #:e/ #:esquare)
  (:import-from #:num-utils.matrix-shorthand #:vec)
  (:import-from #:num-utils #:num=))






