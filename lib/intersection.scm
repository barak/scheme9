; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (intersection . list) ==> list
;
; Compute the intersection of a number of sets.
;
; Arguments: a* := sets
;
; Example:   (intersection '(v w x) '(w x y) '(x y z)) ==> (x)

(define (intersection . a*)
  (letrec
    ((intersection3 (lambda (a b r)
      (cond ((null? a)
              (reverse r))
            ((member (car a) b)
              (intersection3 (cdr a) b (cons (car a) r)))
            (else (intersection3 (cdr a) b r))))))
    (if (null? a*)
        a*
        (fold-left (lambda (a b) (intersection3 a b '()))
                   (car a*)
                   (cdr a*)))))
