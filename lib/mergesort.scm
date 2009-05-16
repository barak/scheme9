; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (mergesort procedure^2 list)  ==>  list
;
; Sort lists using the mergesort algorithm. PROCEDURE^2 is a
; binary predicate that describes the desired order.
;
; Example:   (mergesort <= '(5 3 7 9 1))  ==>  (1 3 5 7 9)

(define (mergesort p a)
  (letrec
    ((split
       (lambda (a r1 r2)
         (cond ((or (null? a)
                    (null? (cdr a)))
                 (list (reverse r2) r1))
               (else (split (cddr a)
                            (cdr r1)
                            (cons (car r1) r2))))))
     (merge
       (lambda (a b r)
         (cond
           ((null? a)
             (if (null? b)
                 r
                 (merge a (cdr b) (cons (car b) r))))
           ((null? b)
             (merge (cdr a) b (cons (car a) r)))
           ((p (car a) (car b))
             (merge a (cdr b) (cons (car b) r)))
           (else (merge (cdr a) b (cons (car a) r))))))
     (sort
       (lambda (a)
         (cond
           ((or (null? a)
                (null? (cdr a)))
             a)
           (else (let ((p* (split a a '())))
                   (merge (reverse (sort (car p*)))
                          (reverse (sort (cadr p*)))
                          '())))))))
    (sort a)))
