; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (filter proc^1 list) ==> list
;
; Extract elements from a list.
; The predicate P describes the property
; of the elements to be extracted.
;
; Arguments: p - predicate
;            a - source list
;
; Example:   (filter number? '(a 1 b 2 c 3)) ==> (1 2 3)

(define (filter p a)
  (letrec
    ((filter2
       (lambda (a r)
         (cond ((null? a) r)
               ((p (car a))
                 (filter2 (cdr a) (cons (car a) r)))
               (else (filter2 (cdr a) r))))))
    (filter2 (reverse a) '())))