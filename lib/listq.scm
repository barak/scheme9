; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; See the LICENSE file of the S9fES package for terms of use
;
; (listq object ...)  ==>  list
;
; LISTQ is like LIST, but quotes each OBJECT passed to it.
; Rationale: (list a b c)    evaluates a, b, and c
;            '(a b c)        makes the list immutable
;            `(a b c)        may be inefficient
;            (list 'a 'b 'c) is awkward
;
; Example:   (listq a (b c) d)  ==>  (a (b c) d)

(load-from-library "syntax-rules.scm")

(define-syntax listq
  (syntax-rules ()
    ((_ x ...)
      (list 'x ...))))
