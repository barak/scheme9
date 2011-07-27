; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; See the LICENSE file of the S9fES package for terms of use
;
; (false object ...)                   ==>  #f
; (id object)                          ==>  object
; (true object ...)                    ==>  #t
;
; (load-from-library "id.scm")
;
; ID returns the object passed to it.
;
; FALSE returns always #F and TRUE returns always #T, no matter which
; values are passed to them.
;
; Example:   (true)          ==>  #t
;            (false 1 2 3)   ==>  #f
;            (id 'whatever)  ==>  whatever

(define (id x)       x)
(define (true . x)  #t)
(define (false . x) #f)
