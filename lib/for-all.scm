; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (for-all proc . list) ==> form
;
; Test whether all members of a sequence of lists have a given property.
; The property is expressed using an N-ary predicate P. P is applied to
; a list consisting of the first member of each given list. If P
; returns truth, it is applied to a list consisting of the second
; member of each given list, etc. If P returns falsity for any set of
; members, FOR-ALL returns #F. If only one set of members is left to
; check, FOR-ALL returns the value of P applied to this final set. 
;
; Arguments: proc - predicate
;            a*   - lists
;
; Example:   (for-all < '(1 7) '(2 8) '(3 9)) ==> #t
;            ; because (< 1 2 3) and (< 7 8 9)

(define (for-all p . a*)
  (letrec
    ((car-of
       (lambda (a)
         (map car a)))
     (cdr-of
       (lambda (a)
         (map cdr a)))
     (any-null?
       (lambda (a)
         (and (memq #t (map null? a)) #t)))
     (forall*
       (lambda (a*)
         (cond ((any-null? a*) #t)
               ((any-null? (cdr-of a*))
                 (apply p (car-of a*)))
               (else (and (apply p (car-of a*))
                          (forall* (cdr-of a*))))))))
    (forall* a*)))
