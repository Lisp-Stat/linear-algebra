;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-KERNEL -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

(in-package #:linear-algebra-kernel)

(defun givens-rotation (f g)
  "Return c,s,r defined from the Givens rotation."
  (nu:check-types (f g) number)
  (cond ((zerop g) (values 1 0 f))
	((zerop f) (values 0 (signum (conjugate g)) (abs g)))
	(t (let ((abs-f (abs f))
		 (sqrtfg (sqrt (sum (esquare (eabs `#(,f ,g)))))))
	     (values (/ abs-f sqrtfg)
		     (/ (* (signum f) (conjugate g)) sqrtfg)
		     (* (signum f) sqrtfg))))))

(defun jacobi-rotation (x y z)
  "Return a, b, cos(theta) and sin(theta) terms from the Jacobi rotation."
  (let* ((yabs (abs y))
         (tau  (/ (- x z) 2.0 yabs))
         (tee  (/ (float-sign tau)
                  (+ (abs tau) (sqrt (+ 1.0 (expt tau 2))))))
         (cos-theta (/ (sqrt (+ 1.0 (expt tee 2))))) ; Invert sqrt
         (sin-theta (* cos-theta tee)))
    (values
     ;; a : first eigenvalue
     (+ (* cos-theta cos-theta x)
        (* 2.0 cos-theta sin-theta yabs)
        (* sin-theta sin-theta z))
     ;; b : second eigenvalue
     (+ (* sin-theta sin-theta x)
        (* -2.0 cos-theta sin-theta yabs)
        (* cos-theta cos-theta z))
     ;; Cosine theta
     cos-theta
     ;; Sine theta
     (* (conjugate (signum y)) sin-theta))))

(defun householder-reflection (alpha vector)
  "Return Beta, Tau and the Householder vector."
  (let* ((beta (- (float-sign (realpart alpha)
			      (sqrt (sum (esquare (eabs `#(,alpha ,(norm-vector vector 2)))))))))
         (tau  (- 1 (/ alpha beta))))
    (values beta tau (dotimes (index (length vector) vector)
		       (setf (aref vector index)
			     (/ (aref vector index) alpha))))))
