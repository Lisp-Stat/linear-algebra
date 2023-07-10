;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TEST -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; Copyright (c) 2023 Ten Factor Growth, LLC
;;; SPDX-License-identifier: MS-PL

(in-package :linear-algebra-test)

(defsuite gauss-kernel-test (linear-algebra-kernel-test))

(deftest initialize-pivot-selection-vector (gauss-kernel-test)
  (assert-rational-equal
   #(0 1 2 3 4 5)
   (linear-algebra-kernel::initialize-pivot-selection-vector 6)))

(deftest column-pivot-search (gauss-kernel-test)
  (assert-eq
   1 (linear-algebra-kernel::column-pivot-search
      #2A((1.1 1.4 1.3 1.2)
          (1.4 2.2 2.3 2.1)
          (1.3 2.3 3.3 3.2)
          (1.2 2.1 3.2 4.4)) 0))
  (assert-eq
   2 (linear-algebra-kernel::column-pivot-search
      #2A((1.1 1.4 1.3 1.2)
          (1.4 2.2 2.3 2.1)
          (1.3 2.3 3.3 3.2)
          (1.2 2.1 3.2 4.4)) 1))
  (assert-eq
   2 (linear-algebra-kernel::column-pivot-search
      #2A((1.1 1.4 1.3 1.2)
          (1.4 2.2 2.3 2.1)
          (1.3 2.3 3.3 3.2)
          (1.2 2.1 3.2 4.4)) 2))
  (assert-eq
   3 (linear-algebra-kernel::column-pivot-search
      #2A((1.1 1.4 1.3 1.2)
          (1.4 2.2 2.3 2.1)
          (1.3 2.3 3.3 3.2)
          (1.2 2.1 3.2 4.4)) 3))
  (assert-eq
   0 (linear-algebra-kernel::column-pivot-search
      #2A((2.2 1.1 1.1)
          (1.1 2.2 1.1)
          (1.1 1.1 2.2)) 0))
  (assert-eq
   1 (linear-algebra-kernel::column-pivot-search
      #2A((2.2 1.1 1.1)
          (1.1 2.2 1.1)
          (1.1 1.1 2.2)) 1))
  (assert-eq
   2 (linear-algebra-kernel::column-pivot-search
      #2A((2.2 1.1 1.1)
          (1.1 2.2 1.1)
          (1.1 1.1 2.2)) 2))
  (assert-eq
   2 (linear-algebra-kernel::column-pivot-search
      #2A((1.1 1.4  1.3 1.2)
          (1.4 2.2  2.3 2.1)
          (1.3 2.3 -3.3 3.2)
          (1.2 2.1  3.2 4.4)) 2)))

(deftest swap-rows (gauss-kernel-test)
  (assert-float-equal
   #2A((1.3 2.3 3.3 3.2)
       (1.4 2.2 2.3 2.1)
       (1.1 1.4 1.3 1.2)
       (1.2 2.1 3.2 4.4))
   (linear-algebra-kernel::swap-rows
    (make-array
     '(4 4) :initial-contents
     '((1.1 1.4 1.3 1.2)
       (1.4 2.2 2.3 2.1)
       (1.3 2.3 3.3 3.2)
       (1.2 2.1 3.2 4.4)))
    0 2)))

(deftest column-pivot (gauss-kernel-test)
  (let ((psv
         (linear-algebra-kernel::initialize-pivot-selection-vector 4))
        (array
         (make-array
          '(4 4) :initial-contents
          '((1.1 1.4 1.3 1.2)
            (1.4 2.2 2.3 2.1)
            (1.3 2.3 3.3 3.2)
            (1.2 2.1 3.2 4.4)))))
    ;;; Column 0
    (multiple-value-bind (array0 psv0)
        (linear-algebra-kernel::column-pivot
         array psv 0)
      (assert-eq array array0)
      (assert-eq psv psv0)
      (assert-rational-equal #(1 0 2 3) psv0)
      (assert-float-equal
       #2A((1.4 2.2 2.3 2.1)
           (0.7857143 -0.32857156 -0.507143 -0.44999993)
           (0.9285714  0.25714278 1.1642857  1.2500002)
           (0.8571429  0.21428538 1.2285714  2.6))
       array0))
    ;;; Column 1
    (multiple-value-bind (array1 psv1)
        (linear-algebra-kernel::column-pivot
         array psv 1)
      (assert-eq psv psv1)
      (assert-eq array array1)
      (assert-rational-equal #(1 0 2 3) psv1)
      (assert-float-equal
       #2A((1.4 2.2 2.3 2.1)
           (0.7857143 -0.32857156 -0.507143 -0.44999993)
           (0.9285714 -0.78260816  0.7673914 0.8978266)
           (0.8571429 -0.6521726   0.8978266 2.3065224))
       array1))
    ;;; Column 2
    (multiple-value-bind (array2 psv2)
        (linear-algebra-kernel::column-pivot
         array psv 2)
      (assert-eq psv psv2)
      (assert-eq array array2)
      (assert-rational-equal #(1 0 3 2) psv2)
      (assert-float-equal
       #2A((1.4 2.2 2.3 2.1)
           (0.7857143 -0.32857156 -0.507143  -0.44999993)
           (0.8571429 -0.6521726   0.8978266  2.3065224)
           (0.9285714 -0.78260816  0.8547211 -1.0736067))
       array2))))

(deftest gauss-factorization (gauss-kernel-test)
  (multiple-value-bind (array psv)
      (linear-algebra-kernel::gauss-factorization
       (make-array
        '(4 4) :initial-contents
        '((1.1 1.4 1.3 1.2)
          (1.4 2.2 2.3 2.1)
          (1.3 2.3 3.3 3.2)
          (1.2 2.1 3.2 4.4))))
    (assert-rational-equal #(1 0 3 2) psv)
    (assert-float-equal
     #2A((1.4        2.2         2.3        2.1)
         (0.7857143 -0.32857156 -0.507143  -0.44999993)
         (0.8571429 -0.6521726   0.8978266  2.3065224)
         (0.9285714 -0.78260816  0.8547211 -1.0736067))
     array)))

(deftest gauss-solver (gauss-kernel-test)n
  (let ((*epsilon* (* 64 single-float-epsilon)))
    ;; 2x2
    (assert-float-equal
     #(2.0 -1.0)
     (linear-algebra-kernel:gauss-solver
      (make-array
       '(2 2) :initial-contents
       '((1.1 1.2) (2.1 2.2)))
      (make-array 2 :initial-contents '(1.0 2.0))))
    ;; 3x3
    ;; Maxima : #(66.36628 -151.8314 85.6105)
    (assert-float-equal
     #(66.36775 -151.8342 85.6118)
     (linear-algebra-kernel:gauss-solver
      (make-array
       '(3 3) :initial-contents
       '((1.15 1.26 1.37) (2.14 2.23 2.31) (3.13 3.22 3.31)))
      (make-array 3 :initial-contents '(2.3 1.2 2.2))))))

(deftest gauss-invert (gauss-kernel-test)
  (assert-float-equal
   #2A((-22.000029 12.000016) (21.000027 -11.000015))
   (linear-algebra-kernel:gauss-invert
    (make-array
     '(2 2) :initial-contents
     '((1.1 1.2) (2.1 2.2)))))
  (assert-float-equal
   #2A((0.9272161 -0.04572601 -0.03333973)
       (-0.08021406 0.4631565 -0.029120658)
       (-0.07932379 -0.04061667 0.30898604))
   (linear-algebra-kernel:gauss-invert
    (make-array
     '(3 3) :initial-contents
     '((1.1 0.12 0.13)
       (0.21 2.2 0.23)
       (0.31 0.32 3.3)))))
  (assert-float-equal
   #2A((0.10003952 -5.862483e-4 -4.2409348e-4 -3.4301603e-4)
       (-0.0010267387 0.050018318 -3.748202e-4 -2.9333035e-4)
       (-0.001011414 -5.216503e-4 0.033345684 -2.7676846e-4)
       (-0.0010037516 -5.135755e-4 -3.5018355e-4 0.02500957))
   (linear-algebra-kernel:gauss-invert
    (make-array
     '(4 4) :initial-contents
     '((10.0 0.12 0.13 0.14)
       (0.21 20.0 0.23 0.24)
       (0.31 0.32 30.0 0.34)
       (0.41 0.42 0.43 40.0))))))
