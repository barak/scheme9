; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (factorial integer)  ==>  integer
;
; Compute the factorial of a number.
;
; Example:   (factorial 30)  ==>  265252859812191058636308480000000

(define (factorial n)
  (letrec
    ((r* (lambda (n m)
           (cond ((< m 2) n)
                 (else (let ((k (quotient m 2)))
                         (* (r* n k)
                            (r* (+ n k) (- m k)))))))))
    (if (negative? n)
        (wrong "factorial: negative operand" n)
        (r* 1 n))))
