; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (remove proc^1 list) ==> list
;
; Remove elements from a list.
; The predicate P describes the property
; of the elements to be removed.
;
; Arguments: p - predicate
;            a - source list
;
; Example:   (remove number? '(a 1 b 2 c 3)) ==> (a b c)

(load-from-library "filter.scm")

(define (remove p a)
  (filter (lambda (x) (not (p x))) a))
