;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:linear-algebra
  (:use #:cl)
  (:use-reexport #:linear-algebra-kernel)
  ;; Fundamental operations
  (:export #:norm
           #:transpose #:ntranspose
           #:permute
           #:scale #:nscale
           #:add #:nadd
           #:subtract #:nsubtract
           #:product
           #:solve #:nsolve
           #:invert #:ninvert)
  ;; Vector exports
  (:export #:initialize-vector
           #:make-vector
           #:vector-in-bounds-p
           #:vector-element-type
           #:vector-length
           #:vref
           #:copy-vector
           #:subvector
           #:replace-vector
           #:map-vector
           #:map-into-vector
           #:dovector
           #:apply-rotation
	   #:napply-rotation)
  ;; Matrix interface
  (:export #:matrix-object
           #:initialize-matrix
           #:make-matrix
           #:matrixp
           #:matrix-in-bounds-p
           #:matrix-element-type
           #:matrix-dimensions
           #:matrix-row-dimension
           #:matrix-column-dimension
           #:mref
           #:copy-matrix
           #:submatrix
           #:replace-matrix
           #:matrix-validated-range)
  ;; Identity matrix
  (:export #:identity-matrix
           #:identity-matrix-p)
  ;; Permutation matrix
  (:export #:permutation-matrix
           #:permutation-matrix-p)
  ;; Data vector exports
  (:export #:data-vector
           #:row-vector
           #:row-vector-p
           #:column-vector
           #:column-vector-p)
  ;; Dense matrix
  (:export #:dense-matrix
           #:dense-matrix-p)
  ;; Square matrix
  (:export #:square-matrix
           #:square-matrix-p)
  ;; Hermitian matrix
  (:export #:hermitian-matrix
           #:hermitian-matrix-p)
  ;; Symmetric matrix
  (:export #:symmetric-matrix
           #:symmetric-matrix-p))
