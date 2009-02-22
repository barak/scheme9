; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (iota integer_1 integer_2) ==> list
;
; Create ranges of integers.
;
; Arguments: l - least integer in range
;            h - greatest integer in range
;
; Example:   (iota 17 21) ==> (17 18 19 20 21)

(define (iota l h)
  (letrec
    ((j (lambda (x r)
          (if (= x l)
              (cons l r)
              (j (- x 1) (cons x r))))))
    (if (> l h)
        (wrong "iota: bad range" (list l h)))
        (j h '())))
