; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (sum integer_1 integer_2) ==> integer
;
; Compute the sum of a range of integers.
;
; Arguments: n - least integer in range
;            m - greatest integer in range
;
; Example:   (sum 2 5) ==> 14

(define (sum n m)
  (let ((x (+ 1 (- m n))))
    (+ (quotient (+ x (* x x))
                 2)
       (* x (- n 1)))))
