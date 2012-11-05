; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009-2012
; Placed in the Public Domain
;
; (iota integer)             ==>  list
; (iota* integer1 integer2)  ==>  list
;
; (load-from-library "iota.scm")
;
; IOTA creates a sequence of integers starting at 1 and ending
; at INTEGER (including both).
;
; IOTA* creates a sequence from INTEGER1 up to, but not including,
; INTEGER2. Hence IOTA* can be used to create empty sequences. The
; procedure assumes INTEGER1 <= INTEGER2.
;
; Example:   (iota 7)       ==>  (1 2 3 4 5 6 7)
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

(define (iota h) (iota* 1 (+ 1 h)))
