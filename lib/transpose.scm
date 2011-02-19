; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (transpose list)  ==>  list
;
; Transpose a matrix. A matrix is represented bt a nested list,
; where each inner list is a column of the matrix.
;
; Example:   (transpose '((1 2 3) (4 5 6)))  ==>  ((1 4) (2 5) (3 6))

(define (transpose x)
  (apply map list x))
