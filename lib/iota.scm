; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (iota integer1 integer2)   ==>  list
; (iota* integer1 integer2)  ==>  list
;
; (load-from-library "iota.scm")
;
; Create a range of integers starting at INTEGER1 and ending at
; INTEGER2 (including both). Assume INTEGER1 <= INTEGER2.
;
; IOTA* differs from IOTA in that it exludes INTEGER2 from the
; generated range. Hence IOTA* can be used to create an empty
; range, which IOTA cannot do.
;
; Example:   (iota 17 21)   ==>  (17 18 19 20 21)
;            (iota 1 1)     ==>  (1)
;            (iota* 17 21)  ==>  (17 18 19 20)
;            (iota* 1 1)    ==>  ()

(define (iota* l h)
  (letrec
    ((j (lambda (x r)
          (if (< x l)
              r
              (j (- x 1) (cons x r))))))
    (if (> l h)
        (error "iota*: bad range" (list l h))
        (j (- h 1) '()))))

(define (iota l h)
    (if (> l h)
        (error "iota: bad range" (list l h))
        (iota* l (+ 1 h))))
