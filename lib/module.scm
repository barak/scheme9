; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (module symbol definition ...)                 ==>  unspecific
; (import symbol (symbol-i ...) expression ...)  ==>  object
;
; Simple modules. Inside of a MODULE expression, DEFINE defines
; a local object and DEFINE* defines a public object. SYMBOL names
; the module itself.
;
; Expressions inside of IMPORT may use all SYMBOL-I's that are
; being imported from the module SYMBOL.
;
; Example:   (begin ; Note: BEGIN is only needed for automatic testing
;              (module math
;                (define* (fact x)
;                  (if (= 0 x) 1 (* x (fact (- x 1))))))
;              (import math (fact)
;                (fact 5)))                               ==> 120

(define-syntax module
  (syntax-rules (define define*)
    ((_ (m e))
       (letrec e (list . m)))
    ((_ (m e) (define (n . a*) . body) . defs)
       (module (m e) (define n (lambda a* . body)) . defs))
    ((_ (m e) (define n v) . defs)
       (module (m ((n v) . e)) . defs))
    ((_ (m e) (define* (n . a*) . body) . defs)
       (module (m e) (define* n (lambda a* . body)) . defs))
    ((_ (m e) (define* n v) . defs)
       (module (((cons 'n v) . m) ((n v) . e)) . defs))
    ((_ n . defs)
       (define n (module (() ()) . defs)))))

(define-syntax import
  (syntax-rules ()
    ((_ (n e ()) . body)
       (let e . body))
    ((_ (n e (i . r)) . body)
       (import (n ((i (cdr (assq 'i n))) . e) r) . body))
    ((_ n (i . r) . body)
       (import (n () (i . r)) . body))))
