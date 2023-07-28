;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: LINEAR-ALGEBRA-TESTS -*-
;;; Copyright (c) 2011-2014, Odonata Research LLC
;;; Copyright (c) 2023 Symbolics Pte Ltd
;;; SPDX-License-identifier: MS-PL

;;; Convenience functions

(in-package #:linear-algebra-test)

(defun %complex-float-random (limit &optional (state *random-state*))
  "Return a random complex float number."
  (complex (random (realpart limit) state)
	   (random (imagpart limit) state)))

(defun %complex-rational-random (limit &optional (state *random-state*))
  "Return a random complex rational number."
  (let ((imaglimit (imagpart limit)))
    (if (< 1 imaglimit)
        (complex (random (realpart limit) state)
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
  (mapcar (if (complexp limit)
	      #'complex-random
	      #'random)
	  (make-list size :initial-element limit)))

;;; Perhaps merge these into num-utils

(defun random-interior-index (size)
  "Return an interior index that is guaranteed not to be an end point."
  (check-type size fixnum)
  (cond ((= 3 size) 1)
	((< 3 size)
	 (let ((end (1- size)))
	   (do ((index (random end) (random end)))
               ((< 0 index end) index))))
	(t (error "Invalid size : ~D." size))))

(defun zero-matrix (rows columns &key (matrix-type 'dense-matrix))
  "Return a ROWSxCOLUMNS zero matrix."
  (make-matrix rows columns :matrix-type matrix-type :initial-element 0))

(defun unit-matrix (rows columns &key (matrix-type 'dense-matrix))
  "Return a ROWSxCOLUMNS unit matrix."
  (make-matrix rows columns :matrix-type matrix-type :initial-element 1))

(defun identity-array (size)
  (let ((the-array (make-array (list size size) :initial-element 0.0)))
    (dotimes (i0 size the-array)
      (setf (aref the-array i0 i0) 1.0))))

(defun coordinate-array (&optional
                           (row 0)      (column 0)
                           (row-end 10) (column-end 10))
  "Return an array with the elements denoting the coordinate."
  (cond
    ((not (<= 0 row row-end 10)) (error "Invalid range of rows (~D:~D)." row row-end))
    ((not (<= 0 column column-end 10)) (error "Invalid range of columns (~D:~D)." column column-end))
    (t (let* ((rows (- row-end row))
              (columns (- column-end column))
              (the-array (make-array (list rows columns))))
	 (dotimes (i0 rows the-array)
           (dotimes (i1 columns)
             (setf (aref the-array i0 i1)
                   (+ row i0 (/ (+ column i1) 10.0)))))))))

(defun symmetric-array (&optional (start 0) (end 10))
  "Return a symmetric array with the element denoting the coordinate."
  (if (<= 0 start end 10)
      (let* ((size (- end start))
             (the-array (make-array (list size size)))
             (val nil))
        (dotimes (i0 size the-array)
          (setf (aref the-array i0 i0)
                (+ start i0 (/ (+ start i0) 10.0)))
          (dotimes (i1 i0)
            (setf val (+ start i0 (/ (+ start i1) 10.0))
                  (aref the-array i0 i1) val
                  (aref the-array i1 i0) val))))
      (error "Invalid range (~D:~D)." start end)))

(defun hermitian-array (&optional (start 0) (end 10))
  "Return a Hermitian array with the element denoting the coordinate."
  (if (<= 0 start end 10)
      (let* ((size (- end start))
             (the-array (make-array (list size size))))
        (dotimes (i0 size the-array)
          (setf (aref the-array i0 i0)
                (complex (+ 1 start i0) 0))
          (dotimes (i1 i0)
            (setf (aref the-array i0 i1)
                  (complex (+ 1 start i0) (- -1 start i1)))
            (setf (aref the-array i1 i0)
                  (complex (+ 1 start i0) (+ 1 start i1))))))
      (error "Invalid range (~D:~D)." start end)))

(defun upper-triangular-array (&optional (start 0) (end 10))
  "Return a 10x10 list with the element denoting the coordinate."
  (if (<= 0 start end 10)
      (let* ((size (- end start))
             (the-array (make-array (list size size) :element-type 'single-float :initial-element 0.0)))
        (dotimes (i1 size the-array)
          (setf (aref the-array i1 i1) 1.0)
          (dotimes (i0 i1)
            (setf (aref the-array i0 i1) 1.0))))
      (error "Invalid range (~D:~D)." start end)))

(defun lower-triangular-array (&optional (start 0) (end 10))
  "Return a 10x10 list with the element denoting the coordinate."
  (if (<= 0 start end 10)
      (let* ((size (- end start))
             (the-array (make-array (list size size) :element-type 'single-float :initial-element 0.0)))
        (dotimes (i0 size the-array)
          (setf (aref the-array i0 i0) 1.0)
          (dotimes (i1 i0)
            (setf (aref the-array i0 i1) 1.0))))
      (error "Invalid range (~D:~D)." start end)))

(defun random-permutation-vector (size &optional (counter -1))
  "Return a random permutation vector of SIZE."
  (let ((permutation (map-into (make-array size) (lambda () (incf counter)))))
    (do ((index 0 (1+ index))
         (swap (random size) (random size)))
        ((>= index size) permutation)
      (rotatef (svref permutation index) (svref permutation swap)))))

(defun random-permutation-array (size)
  "Return a random permutation matrix of SIZE."
  (let ((permutation (random-permutation-vector size))
        (the-array (make-array (list size size) :initial-element 0)))
    (dotimes (row size the-array)
      (setf (aref the-array row (svref permutation row)) 1))))

(defun permutations (list)
  "Return permutations of the list. [Erik Naggum]"
  (if (cdr list)
      (loop
	with rotation = list
	do (setq rotation (nconc (last rotation) (nbutlast rotation)))
	nconc
	(loop
          for list in (permutations (rest rotation))
          collect (cons (first rotation) (copy-list list)))
	until (eq rotation list))
      (list list)))

(defun cartesian-product (list1 list2)
  "Return a list of the Cartesian product of two lists."
  (mapcan (lambda (x) (mapcar (lambda (y) (list x y)) list2)) list1))

(defun nary-product (list1 list2 &rest more-lists)
  "Return a list of the n-ary Cartesian product of the lists."
  (if (null more-lists)
      (cartesian-product list1 list2)
      (mapcan
       (lambda (x)
         (mapcar (lambda (y) (push x y))
                 (apply #'nary-product list2
                        (car more-lists) (rest more-lists))))
       list1)))

