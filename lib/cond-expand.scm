; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (cond-expand (symbol . body) ==> form
;
; A subset of SRFI-0 COND-EXPAND.
; Each argument of COND-EXPAND is a clause consisting of a
; leading "feature ID" (a symbol) and any number of trailing
; expressions (the body). It evaluates to the body of the
; first clause having a feature ID of SCHEME-9-FROM-EMPTY-SPACE,
; S9FES, or ELSE. The body is then evaluated by S9fES. When no
; suitable clause is found, an error is reported.
;
; Arguments: c* - clauses.
;
; Example:   (cond-expand (s9fes (cons 1 2))) ==> (1 . 2)
;            (cond-expand (foo (cons 1 2)) (else (+ 1 2))) ==> 3

(define-macro (cond-expand . c*)
  (letrec
    ((expand
       (lambda (c*)
         (cond ((null? c*)
                 (wrong "cond-expand: unfulfilled"))
               ((or (not (pair? c*))
                    (not (pair? (car c*))))
                 (wrong "cond-expand: syntax error" c*))
               ((or (eq? (caar c*) 's9fes)
                    (eq? (caar c*) 'scheme-9-from-empty-space)
                    (eq? (caar c*) 'else))
                 `(begin ,@(cdar c*)))
               (else (expand (cdr c*)))))))
    (expand c*)))
