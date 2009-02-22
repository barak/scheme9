; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (implode list) ==> symbol
;
; Implode a list of single-character symbols into a symbol.
;
; Arguments: list - list to implode
;
; Example:   (implode '(b l a c k h o l e)) ==> blackhole

(define (implode x)
  (letrec
    ((sym->char
       (lambda (x)
         (let ((str (symbol->string x)))
           (if (= (string-length str) 1)
               (string-ref str 0)
               (wrong "bad symbol in implode" x))))))
    (string->symbol
      (list->string (map sym->char x)))))
