; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; signature: (zebra) ==> list
;
; Solve the zebra puzzle using AMK.
;
; Arguments: none
;
; (Example): (zebra)
;            ==> (((norwegian kools _.0 fox yellow)
;                  (ukrainian chesterfields tea horse blue)
;                  (englishman oldgolds milk snails red)
;                  (japanese parliaments coffee zebra green)
;                  (spaniard luckystrikes orangejuice dog ivory)))

(load-from-library "amk.scm")

(define (lefto x y l)
  (fresh (d)
    (any (all (caro l x)
              (cdro l d)
              (caro d y))
         (all (cdro l d)
              (lefto x y d)))))

(define (nexto x y l)
  (any (lefto x y l)
       (lefto y x l)))

(define (zebra)
  (fresh (h)
    (run* (h)
      (all (== h (list (list 'norwegian (_) (_) (_) (_))
                       (_)
                       (list (_) (_) 'milk (_) (_))
                       (_)
                       (_)))
           (memo (list 'englishman (_) (_) (_) 'red) h)
           (lefto (list (_) (_) (_) (_) 'green)
                  (list (_) (_) (_) (_) 'ivory) h)
           (nexto (list 'norwegian (_) (_) (_) (_))
                  (list (_) (_) (_) (_) 'blue) h)
           (memo (list (_) 'kools (_) (_) 'yellow) h)
           (memo (list 'spaniard (_) (_) 'dog (_)) h)
           (memo (list (_) (_) 'coffee (_) 'green) h)
           (memo (list 'ukrainian (_) 'tea (_) (_)) h)
           (memo (list (_) 'luckystrikes 'orangejuice (_) (_)) h)
           (memo (list 'japanese 'parliaments (_) (_) (_)) h)
           (memo (list (_) 'oldgolds (_) 'snails (_)) h)
           (nexto (list (_) (_) (_) 'horse (_))
                  (list (_) 'kools (_) (_) (_)) h)
           (nexto (list (_) (_) (_) 'fox (_))
                  (list (_) 'chesterfields (_) (_) (_)) h)
         ; (memo (list (_) (_) 'water (_) (_)) h)
           (memo (list (_) (_) (_) 'zebra (_)) h)))))
