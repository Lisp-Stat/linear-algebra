;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:linear-algebra-kernel
  (:use #:cl)
  (:import-from #:num-utils #:num=
                *num=-tolerance*)
  ;; Utility functions
  (:export #:copy-array
           #:common-class-of
           #:common-array-element-type
           #:specific-array-element-type
           #:complex-equal
           #:number-equal)
  ;; Permutation
  (:export #:right-permute
           #:left-permute)
  ;; Unary operations
  (:export #:sumsq2 #:sumsq3
           #:sumsq #:sump
           #:sumsq-row
           #:sumsq-column
           #:norm-vector
           #:norm-array)
  ;; Binary operations
  (:export #:compatible-dimensions-p
           #:scaled-binary-op
           #:add-vector #:nadd-vector
           #:subtract-vector #:nsubtract-vector
           #:add-array #:nadd-array
           #:subtract-array #:nsubtract-array
           #:inner-product-vector
           #:product-vector-array
           #:product-array-vector
           #:product-array-array)
  ;; Rotations
  (:export #:givens-rotation
           #:jacobi-rotation
           #:householder-reflection)
  ;; Gauss algorithm
  (:export #:gauss-solver
           #:gauss-invert)
  ;; Cholesky
  (:export #:symmetric-cholesky-decomposition
           #:hermitian-cholesky-decomposition
           #:root-free-symmetric-cholesky-decomposition
           #:root-free-hermitian-cholesky-decomposition
           #:symmetric-cholesky-solver
           #:hermitian-cholesky-solver
           #:symmetric-cholesky-invert
           #:hermitian-cholesky-invert)
  ;; Conjugate gradient method
  (:export #:conjugate-gradient-solver)
  ;; Tridiagonal
  (:export #:tridiagonal-solver))
