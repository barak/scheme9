; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (integer-sqrt integer) ==> integer
;
; Compute the integer part of the square root
; or a natural number.
;
; Arguments: square - integer
;
; Example:   (integer-sqrt 10) ==> 3

(define (integer-sqrt square)
  (letrec
    ((sqrt2 (lambda (x last)
       (cond ((= last x)
               x)
             ((= last (+ 1 x))
               (if (> (* x x) square) (- x 1) x))
             (else
               (sqrt2 (quotient
                         (+ x (quotient square x))
                         2)
                      x))))))
    (if (negative? square)
        (wrong "integer-sqrt: negative argument" square)
        (sqrt2 square 0))))
