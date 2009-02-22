; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (union . list) ==> list
;
; Compute the union of a number of sets.
;
; Arguments: a* - sets
;
; Example:   (union '(v w x) '(w x y) '(x y z)) ==> (v w x y z)

(load-from-library "list-to-set.scm")

(define (union . a*)
  (list->set (apply append a*)))
