; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (list->set list) ==> list
;
; Convert list to set.
; A set is a list containing only unique members.
;
; Arguments: a - list
;
; Example:   (list->set '(a b c b c)) ==> (a b c)

(define (list->set a)
  (letrec
    ((l->s
       (lambda (a r)
         (cond ((null? a)
                 (reverse r))
               ((member (car a) r)
                 (l->s (cdr a) r))
               (else (l->s (cdr a)
                           (cons (car a) r)))))))
    (l->s a '())))
