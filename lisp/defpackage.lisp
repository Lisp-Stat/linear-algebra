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

(in-package :cl-user)

(defpackage :linear-algebra
  (:use :common-lisp :floating-point)
  (:export
   ;; Fundamental operations
   :sumsq :sump
   :norm
   :transpose :ntranspose
   :permute :npermute
   :scale :nscale
   :add :nadd
   :subtract :nsubtract
   :product
   ;; Vector exports
   :vector-object
   :make-vector :initialize-vector
   :vector-object-p :vector-in-bounds-p
   :vector-element-type :vector-length
   :vref
   :copy-vector
   :subvector :replace-vector
   :map-vector :map-into-vector
   :dovector
   :givens-rotation
   :jacobi-rotation
   :householder-reflection
   :apply-rotation :napply-rotation
   ;; Matrix interface
   :matrix-object
   :make-matrix :initialize-matrix
   :matrixp :matrix-in-bounds-p
   :matrix-element-type
   :matrix-dimensions
   :matrix-row-dimension
   :matrix-column-dimension
   :mref
   :copy-matrix
   :submatrix :replace-matrix
   :matrix-validated-range
   ;; Identity matrix
   :identity-matrix
   :identity-matrix-p
   ;; Permutation matrix
   :permutation-matrix
   :permutation-matrix-p
   ;; Data vector exports
   :data-vector :row-vector :column-vector
   :column-vector-p :row-vector-p
   ;; Dense matrix
   :dense-matrix
   :dense-matrix-p
   ;; Square matrix
   :square-matrix
   :square-matrix-p
   ;; Hermitian matrix
   :hermitian-matrix
   :hermitian-matrix-p
   ;; Symmetric matrix
   :symmetric-matrix
   :symmetric-matrix-p
   ;; Triangular matrix
   :upper-triangular-matrix :lower-triangular-matrix
   :upper-triangular-matrix-p :lower-triangular-matrix-p))
