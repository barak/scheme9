; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (replace form_1 form_2 pair) ==> pair
;
; Replace elements of a pair.
;
; Arguments: old  - element to replace
;            new  - element to insert in the place of OLD
;            form - pair in which elements will be replaced
;
; Example:   (replace '(x) '(y) '(lambda (x) y)) ==> (lambda (y) y)

(define (replace old new form)
  (cond ((equal? form old) new)
        ((pair? form)
          (cons (replace old new (car form))
                (replace old new (cdr form))))
        (else form)))
