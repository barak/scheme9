; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; See the LICENSE file of the S9fES package for terms of use
;
; (group-list integer list)  ==>  list
;
; Group the elements of LIST in tuples of INTEGER elements
; each (except for the last tuple, which may contain fewer
; elements). Return a list of the resulting tuples. The
; elements appear in the output list in the same order as
; in the input list.
;
; Example:   (group-list 2 '(1 2 3 4 5))  ==>  ((1 2) (3 4) (5))

(define (group-list n a)
  (let loop ((i   0)
             (in  a)
             (out '())
             (tmp '()))
    (cond ((null? in)
            (reverse! (cons (reverse! tmp) out)))
          ((= i n)
            (loop 0 in (cons (reverse! tmp) out) '()))
          (else
            (loop (+ 1 i) (cdr in) out (cons (car in) tmp))))))
