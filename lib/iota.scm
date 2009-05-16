; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (iota integer1 integer2)  ==>  list
;
; Create a range of integers starting at INTEGER1 and ending at
; INTEGER2 (including both). Assume INTEGER1 <= INTEGER2.
;
; Example:   (iota 17 21)  ==>  (17 18 19 20 21)

(define (iota l h)
  (letrec
    ((j (lambda (x r)
          (if (= x l)
              (cons l r)
              (j (- x 1) (cons x r))))))
    (if (> l h)
        (wrong "iota: bad range" (list l h)))
        (j h '())))
