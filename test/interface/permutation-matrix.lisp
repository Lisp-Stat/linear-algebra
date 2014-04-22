#|

 Linear Algebra in Common Lisp Unit Tests

 Copyright (c) 2011, Thomas M. Hermann
 All rights reserved.

 Redistribution and  use  in  source  and  binary  forms, with or without
 modification, are permitted  provided  that the following conditions are
 met:

   o  Redistributions of  source  code  must  retain  the above copyright
      notice, this list of conditions and the following disclaimer.
   o  Redistributions in binary  form  must reproduce the above copyright
      notice, this list of  conditions  and  the  following disclaimer in
      the  documentation  and/or   other   materials  provided  with  the
      distribution.
   o  The names of the contributors may not be used to endorse or promote
      products derived from this software without  specific prior written
      permission.

 THIS SOFTWARE IS  PROVIDED  BY  THE  COPYRIGHT  HOLDERS AND CONTRIBUTORS
 "AS IS"  AND  ANY  EXPRESS  OR  IMPLIED  WARRANTIES, INCLUDING,  BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES  OF MERCHANTABILITY AND FITNESS FOR A
 PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 EXEMPLARY, OR  CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT LIMITED TO,
 PROCUREMENT OF  SUBSTITUTE  GOODS  OR  SERVICES;  LOSS  OF USE, DATA, OR
 PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER  CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER  IN  CONTRACT,  STRICT  LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR  OTHERWISE)  ARISING  IN  ANY  WAY  OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|#

(in-package :linear-algebra-test)

(define-test make-permutation-matrix
  (:tag :matrix :permutation-matrix :make-matrix)
  ;; A default permutation matrix
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix)))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(0 1 2 3 4)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - Nested list
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)
            (0 0 0 1 0)
            (0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - Nested vector
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          #(#(0 0 0 0 1)
            #(1 0 0 0 0)
            #(0 1 0 0 0)
            #(0 0 0 1 0)
            #(0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Specify the permutation matrix contents - 2D array
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          #2A((0 0 0 0 1)
              (1 0 0 0 0)
              (0 1 0 0 0)
              (0 0 0 1 0)
              (0 0 1 0 0)))))
    (assert-true (linear-algebra:matrixp matrix))
    (assert-true (typep matrix 'linear-algebra:permutation-matrix))
    (assert-rational-equal
     #(4 0 1 3 2)
     (linear-algebra::contents matrix)))
  ;; Erroneous 2D array input data
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 4
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    #3A(((1.1 1.2) (2.1 2.2))
        ((3.1 3.2) (4.1 4.2))
        ((5.1 5.2) (6.1 6.2)))))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 4
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    (random-permutation-array 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    4 3
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    (random-permutation-array 4)))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3 :element-type 'single-float
    :matrix-type 'linear-algebra:permutation-matrix))
  (assert-error
   'error
   (linear-algebra:make-matrix
    3 3
    :matrix-type 'linear-algebra:permutation-matrix
    :initial-contents
    #2A((0 1 0)
        (1 0 0)
        (1 0 1)))))

;;; Test the permutation matrix predicate
 (define-test permutation-matrix-predicate
   (:tag :matrix :permutation-matrix)
   (assert-true
    (linear-algebra:permutation-matrix-p
     (linear-algebra:make-matrix
      10 10 :matrix-type 'linear-algebra:permutation-matrix)))
   (assert-false
    (linear-algebra:permutation-matrix-p (make-array '(10 10)))))

;;; Test the permutation matrix bounds
(define-test permutation-matrix-in-bounds-p
  (:tag :matrix :permutation-matrix :matrix-in-bounds-p)
  (test-matrix-in-bounds-p 'linear-algebra:permutation-matrix))

;;; Test the permutation matrix element type
(define-test permutation-matrix-element-type
  (:tag :matrix :permutation-matrix :matrix-element-type)
  (assert-eq
   'fixnum
   (linear-algebra:matrix-element-type
    (linear-algebra:make-matrix
     5 5
     :matrix-type 'linear-algebra:permutation-matrix)))
  (dolist (element-type '(single-float double-float))
    (assert-error
     'error
     (linear-algebra:matrix-element-type
      (linear-algebra:make-matrix
       5 5
       :element-type element-type
       :matrix-type
       'linear-algebra:permutation-matrix)))))

;;; Test the permutation matrix dimensions
(define-test permutation-matrix-dimensions
  (:tag :matrix :permutation-matrix :matrix-dimensions)
  (test-matrix-dimensions 'linear-algebra:permutation-matrix 9 9))

;;; Test the permutation matrix row dimension
(define-test permutation-matrix-row-dimension
  (:tag :matrix :permutation-matrix :matrix-row-dimension)
  (test-matrix-row-dimension 'linear-algebra:permutation-matrix 9 9))

;;; Test the permutation matrix column dimension
(define-test permutation-matrix-column-dimension
  (:tag :matrix :permutation-matrix :matrix-column-dimension)
  (test-matrix-column-dimension
   'linear-algebra:permutation-matrix 9 9))

;;; Reference permutation matrix elements
(define-test permutation-matrix-mref
  (:tag :matrix :permutation-matrix :mref)
  (let ((pvec #(2 3 4 0 1))
        (matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 1 0 0)
            (0 0 0 1 0)
            (0 0 0 0 1)
            (1 0 0 0 0)
            (0 1 0 0 0)))))
    (do ((i0 0 (1+ i0)))
        ((>= i0 5))
      (do ((i1 0 (1+ i1)))
          ((>= i1 5))
        (if (= i1 (svref pvec i0))
            (assert-rational-equal
             1 (linear-algebra:mref matrix i0 i1))
            (assert-rational-equal
             0 (linear-algebra:mref matrix i0 i1)))))))

;;; Set permutation matrix elements
(define-test permutation-matrix-setf-mref
  (:tag :matrix :permutation-matrix :setf-mref)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          (random-permutation-array 5))))
    (dotimes (i0 5)
      (dotimes (i1 5)
        (setf (linear-algebra:mref matrix i0 i1) 1)
        (assert-true
         (= 5 (length
               (remove-duplicates
                (linear-algebra::contents matrix))))
         i0 i1 (linear-algebra::contents matrix))))))

;;; Copy the permutation matrix
(define-test copy-permutation-matrix
  (:tag :matrix :permutation-matrix :copy-matrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type 'linear-algebra:permutation-matrix
          :initial-contents
          (random-permutation-array 5))))
    (assert-true
     (linear-algebra:permutation-matrix-p
      (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq matrix (linear-algebra:copy-matrix matrix)))
    (assert-false
     (eq (linear-algebra::contents matrix)
         (linear-algebra::contents
          (linear-algebra:copy-matrix matrix))))
    (assert-rational-equal
     (linear-algebra::contents matrix)
     (linear-algebra::contents
      (linear-algebra:copy-matrix matrix)))))

;;; Test the submatrix of a permutation matrix
(define-test permutation-matrix-submatrix
  (:tag :matrix :permutation-matrix :submatrix)
  (let ((matrix
         (linear-algebra:make-matrix
          5 5
          :matrix-type
          'linear-algebra:permutation-matrix
          :initial-contents
          '((0 0 0 0 1)
            (0 0 0 1 0)
            (0 0 1 0 0)
            (1 0 0 0 0)
            (0 1 0 0 0)))))
    ;; The entire matrix
    (assert-rational-equal
     #2A((0 0 0 0 1)
         (0 0 0 1 0)
         (0 0 1 0 0)
         (1 0 0 0 0)
         (0 1 0 0 0))
     (linear-algebra:submatrix matrix 0 0))
    ;; Start row and column to the end
    (assert-rational-equal
     #2A((1 0 0) (0 0 0) (0 0 0))
     (linear-algebra:submatrix matrix 2 2))
    ;; End row and column
    (assert-rational-equal
     #2A((0 1) (1 0) (0 0))
     (linear-algebra:submatrix
      matrix 1 2 :end-row 4 :end-column 4)
     (linear-algebra::contents
      (linear-algebra:submatrix
       matrix 1 2 :end-row 4 :end-column 4)))
    ;; Start row exceeds dimensions
    (assert-error 'error (linear-algebra:submatrix matrix 6 5))
    ;; Start column exceeds dimensions
    (assert-error 'error (linear-algebra:submatrix matrix 5 6))
    ;; End row exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-row 6))
    ;; End column exceeds dimensions
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-column 6))
    ;; Start row exceeds end row
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-row 3))
    ;; Start column exceeds end column
    (assert-error
     'error (linear-algebra:submatrix matrix 5 5 :end-column 3))))

(define-test permutation-matrix-transpose
  (:tag :matrix :permutation-matrix :transpose)
  (loop
   for (permutation transpose) in (validated-permutation-transpose)
   do (assert-rational-equal
       transpose
       (linear-algebra::contents
        (linear-algebra:transpose
         (make-instance
          'linear-algebra:permutation-matrix
          :contents permutation)))
       permutation)))

;;; Validate a range for a permutation matrix.

(define-test permutation-matrix-validated-range
  (:tag :matrix :permutation-matrix :matrix-validate-range)
  (test-matrix-validated-range
   'linear-algebra:permutation-matrix 10 10))

;;; Validated transposition

(defun validated-permutation-transpose ()
  "Return a list of transposes of permutation vectors."
  (list
   (list (vector 4 3 2 1 0) (vector 4 3 2 1 0))
   (list (vector 4 3 2 0 1) (vector 3 4 2 1 0))
   (list (vector 4 3 1 0 2) (vector 3 2 4 1 0))
   (list (vector 4 3 1 2 0) (vector 4 2 3 1 0))
   (list (vector 4 3 0 2 1) (vector 2 4 3 1 0))
   (list (vector 4 3 0 1 2) (vector 2 3 4 1 0))
   (list (vector 4 2 1 0 3) (vector 3 2 1 4 0))
   (list (vector 4 2 1 3 0) (vector 4 2 1 3 0))
   (list (vector 4 2 0 3 1) (vector 2 4 1 3 0))
   (list (vector 4 2 0 1 3) (vector 2 3 1 4 0))
   (list (vector 4 2 3 1 0) (vector 4 3 1 2 0))
   (list (vector 4 2 3 0 1) (vector 3 4 1 2 0))
   (list (vector 4 1 0 3 2) (vector 2 1 4 3 0))
   (list (vector 4 1 0 2 3) (vector 2 1 3 4 0))
   (list (vector 4 1 3 2 0) (vector 4 1 3 2 0))
   (list (vector 4 1 3 0 2) (vector 3 1 4 2 0))
   (list (vector 4 1 2 0 3) (vector 3 1 2 4 0))
   (list (vector 4 1 2 3 0) (vector 4 1 2 3 0))
   (list (vector 4 0 3 2 1) (vector 1 4 3 2 0))
   (list (vector 4 0 3 1 2) (vector 1 3 4 2 0))
   (list (vector 4 0 2 1 3) (vector 1 3 2 4 0))
   (list (vector 4 0 2 3 1) (vector 1 4 2 3 0))
   (list (vector 4 0 1 3 2) (vector 1 2 4 3 0))
   (list (vector 4 0 1 2 3) (vector 1 2 3 4 0))
   (list (vector 3 2 1 0 4) (vector 3 2 1 0 4))
   (list (vector 3 2 1 4 0) (vector 4 2 1 0 3))
   (list (vector 3 2 0 4 1) (vector 2 4 1 0 3))
   (list (vector 3 2 0 1 4) (vector 2 3 1 0 4))
   (list (vector 3 2 4 1 0) (vector 4 3 1 0 2))
   (list (vector 3 2 4 0 1) (vector 3 4 1 0 2))
   (list (vector 3 1 0 4 2) (vector 2 1 4 0 3))
   (list (vector 3 1 0 2 4) (vector 2 1 3 0 4))
   (list (vector 3 1 4 2 0) (vector 4 1 3 0 2))
   (list (vector 3 1 4 0 2) (vector 3 1 4 0 2))
   (list (vector 3 1 2 0 4) (vector 3 1 2 0 4))
   (list (vector 3 1 2 4 0) (vector 4 1 2 0 3))
   (list (vector 3 0 4 2 1) (vector 1 4 3 0 2))
   (list (vector 3 0 4 1 2) (vector 1 3 4 0 2))
   (list (vector 3 0 2 1 4) (vector 1 3 2 0 4))
   (list (vector 3 0 2 4 1) (vector 1 4 2 0 3))
   (list (vector 3 0 1 4 2) (vector 1 2 4 0 3))
   (list (vector 3 0 1 2 4) (vector 1 2 3 0 4))
   (list (vector 3 4 2 1 0) (vector 4 3 2 0 1))
   (list (vector 3 4 2 0 1) (vector 3 4 2 0 1))
   (list (vector 3 4 1 0 2) (vector 3 2 4 0 1))
   (list (vector 3 4 1 2 0) (vector 4 2 3 0 1))
   (list (vector 3 4 0 2 1) (vector 2 4 3 0 1))
   (list (vector 3 4 0 1 2) (vector 2 3 4 0 1))
   (list (vector 2 1 0 4 3) (vector 2 1 0 4 3))
   (list (vector 2 1 0 3 4) (vector 2 1 0 3 4))
   (list (vector 2 1 4 3 0) (vector 4 1 0 3 2))
   (list (vector 2 1 4 0 3) (vector 3 1 0 4 2))
   (list (vector 2 1 3 0 4) (vector 3 1 0 2 4))
   (list (vector 2 1 3 4 0) (vector 4 1 0 2 3))
   (list (vector 2 0 4 3 1) (vector 1 4 0 3 2))
   (list (vector 2 0 4 1 3) (vector 1 3 0 4 2))
   (list (vector 2 0 3 1 4) (vector 1 3 0 2 4))
   (list (vector 2 0 3 4 1) (vector 1 4 0 2 3))
   (list (vector 2 0 1 4 3) (vector 1 2 0 4 3))
   (list (vector 2 0 1 3 4) (vector 1 2 0 3 4))
   (list (vector 2 4 3 1 0) (vector 4 3 0 2 1))
   (list (vector 2 4 3 0 1) (vector 3 4 0 2 1))
   (list (vector 2 4 1 0 3) (vector 3 2 0 4 1))
   (list (vector 2 4 1 3 0) (vector 4 2 0 3 1))
   (list (vector 2 4 0 3 1) (vector 2 4 0 3 1))
   (list (vector 2 4 0 1 3) (vector 2 3 0 4 1))
   (list (vector 2 3 1 0 4) (vector 3 2 0 1 4))
   (list (vector 2 3 1 4 0) (vector 4 2 0 1 3))
   (list (vector 2 3 0 4 1) (vector 2 4 0 1 3))
   (list (vector 2 3 0 1 4) (vector 2 3 0 1 4))
   (list (vector 2 3 4 1 0) (vector 4 3 0 1 2))
   (list (vector 2 3 4 0 1) (vector 3 4 0 1 2))
   (list (vector 1 0 4 3 2) (vector 1 0 4 3 2))
   (list (vector 1 0 4 2 3) (vector 1 0 3 4 2))
   (list (vector 1 0 3 2 4) (vector 1 0 3 2 4))
   (list (vector 1 0 3 4 2) (vector 1 0 4 2 3))
   (list (vector 1 0 2 4 3) (vector 1 0 2 4 3))
   (list (vector 1 0 2 3 4) (vector 1 0 2 3 4))
   (list (vector 1 4 3 2 0) (vector 4 0 3 2 1))
   (list (vector 1 4 3 0 2) (vector 3 0 4 2 1))
   (list (vector 1 4 2 0 3) (vector 3 0 2 4 1))
   (list (vector 1 4 2 3 0) (vector 4 0 2 3 1))
   (list (vector 1 4 0 3 2) (vector 2 0 4 3 1))
   (list (vector 1 4 0 2 3) (vector 2 0 3 4 1))
   (list (vector 1 3 2 0 4) (vector 3 0 2 1 4))
   (list (vector 1 3 2 4 0) (vector 4 0 2 1 3))
   (list (vector 1 3 0 4 2) (vector 2 0 4 1 3))
   (list (vector 1 3 0 2 4) (vector 2 0 3 1 4))
   (list (vector 1 3 4 2 0) (vector 4 0 3 1 2))
   (list (vector 1 3 4 0 2) (vector 3 0 4 1 2))
   (list (vector 1 2 0 4 3) (vector 2 0 1 4 3))
   (list (vector 1 2 0 3 4) (vector 2 0 1 3 4))
   (list (vector 1 2 4 3 0) (vector 4 0 1 3 2))
   (list (vector 1 2 4 0 3) (vector 3 0 1 4 2))
   (list (vector 1 2 3 0 4) (vector 3 0 1 2 4))
   (list (vector 1 2 3 4 0) (vector 4 0 1 2 3))
   (list (vector 0 4 3 2 1) (vector 0 4 3 2 1))
   (list (vector 0 4 3 1 2) (vector 0 3 4 2 1))
   (list (vector 0 4 2 1 3) (vector 0 3 2 4 1))
   (list (vector 0 4 2 3 1) (vector 0 4 2 3 1))
   (list (vector 0 4 1 3 2) (vector 0 2 4 3 1))
   (list (vector 0 4 1 2 3) (vector 0 2 3 4 1))
   (list (vector 0 3 2 1 4) (vector 0 3 2 1 4))
   (list (vector 0 3 2 4 1) (vector 0 4 2 1 3))
   (list (vector 0 3 1 4 2) (vector 0 2 4 1 3))
   (list (vector 0 3 1 2 4) (vector 0 2 3 1 4))
   (list (vector 0 3 4 2 1) (vector 0 4 3 1 2))
   (list (vector 0 3 4 1 2) (vector 0 3 4 1 2))
   (list (vector 0 2 1 4 3) (vector 0 2 1 4 3))
   (list (vector 0 2 1 3 4) (vector 0 2 1 3 4))
   (list (vector 0 2 4 3 1) (vector 0 4 1 3 2))
   (list (vector 0 2 4 1 3) (vector 0 3 1 4 2))
   (list (vector 0 2 3 1 4) (vector 0 3 1 2 4))
   (list (vector 0 2 3 4 1) (vector 0 4 1 2 3))
   (list (vector 0 1 4 3 2) (vector 0 1 4 3 2))
   (list (vector 0 1 4 2 3) (vector 0 1 3 4 2))
   (list (vector 0 1 3 2 4) (vector 0 1 3 2 4))
   (list (vector 0 1 3 4 2) (vector 0 1 4 2 3))
   (list (vector 0 1 2 4 3) (vector 0 1 2 4 3))
   (list (vector 0 1 2 3 4) (vector 0 1 2 3 4))))
