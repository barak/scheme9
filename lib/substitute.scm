; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (substitute pair alist) ==> pair
;
; Substitute subforms of a given form.
; The association list ALIST contains the
; forms to be substituted as keys and the
; substitutes as values.
;
; arguments: form  - source form
;            alist - substitutions
;
; Example:   (substitute '(* (+ 5 7) 9) '(((+ 5 7) . 12))) ==> (* 12 9)

(define (substitute x a)
  (cond ((assoc x a) => cdr)
        ((pair? x)
          (cons (substitute (car x) a)
                (substitute (cdr x) a)))
        (else x)))
