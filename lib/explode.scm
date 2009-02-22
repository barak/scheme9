; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (explode symbol) ==> list
;
; Explode a symbol into a list of single-character symbols.
;
; Arguments: x - symbol to explode
;
; Example:   (explode 'supernova) ==> (s u p e r n o v a)

(define (explode x)
  (map (lambda (x)
         (string->symbol (string x)))
       (string->list (symbol->string x))))
