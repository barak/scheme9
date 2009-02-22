; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (quicksort proc^2 list) ==> list
;
; Sort lists using the quicksort algorithm.
; The predicate P describes the desired order.
;
; Arguments: p - predicate
;            a - list
;
; Example:   (quicksort < '(5 3 7 9 1)) ==> (1 3 5 7 9)

(load-from-library "partition.scm")

(define (quicksort p a)
  (letrec
    ((sort
       (lambda (a)
         (if (or (null? a)
                 (null? (cdr a)))
             a
             (let ((p* (partition (lambda (x) (p (car a) x))
                                  (cdr a))))
               (append (sort (cadr p*))
                       (list (car a))
                       (sort (car p*))))))))
    (sort a)))
