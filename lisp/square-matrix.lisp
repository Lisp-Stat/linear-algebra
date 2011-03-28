#|

 Linear Algebra in Common Lisp

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

(in-package :linear-algebra)

(defclass square-matrix (dense-matrix)
  ()
  (:documentation
   "Square matrix object."))

;;; Square matrix interface operations
(defun square-matrix-p (object)
  "Return true if OBJECT is a square matrix."
  (typep object 'square-matrix))

(defmethod initialize-matrix :before ((matrix square-matrix) data
                                      (rows integer) (columns integer)
                                      &optional element-type)
  "Verify that the number of rows and colums are equal."
  (declare (ignore matrix data element-type))
  (unless (= rows columns)
    (error "Number of rows must equal the number of columns.")))

(defmethod submatrix ((matrix square-matrix)
                      (row integer) (column integer)
                      &key row-end column-end)
  "Return a matrix created from the submatrix of matrix."
  (destructuring-bind (row column row-end column-end)
      (matrix-validated-range matrix row column row-end column-end)
    (let* ((numrows (- row-end row))
           (numcols (- column-end column))
           (original (contents matrix))
           (contents (make-array (list numrows numcols)
                                 :element-type
                                 (matrix-element-type matrix))))
      (make-instance
       (if (= numrows numcols) 'square-matrix 'dense-matrix)
       :contents
       (dotimes (i0 numrows contents)
         (dotimes (i1 numcols)
           (setf (aref contents i0 i1)
                 (aref original (+ row i0) (+ column i1)))))))))

