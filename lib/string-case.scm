; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (string-upcase string)    ==>  string
; (string-downcase string)  ==>  string
;
; Return a fresh string containing the characters of STRING, but
; with the case of alphabetic characters converted. STRING-UPCASE
; converts characters to upper case, STRING-DOWNCASE to lower case.
;
; Example:   (string-upcase "Hello, World!")    ==>  "HELLO, WORLD!"
;            (string-downcase "Hello, World!")  ==>  "hello, world!"

(define (make-case-converter convert)
  (lambda (s)
    (let* ((k (string-length s))
           (new (make-string k)))
      (let upcase-chars ((i 0))
        (if (< i k)
            (begin (string-set! new i (convert (string-ref s i)))
                   (upcase-chars (+ 1 i)))
            new)))))

(define string-upcase   (make-case-converter char-upcase))
(define string-downcase (make-case-converter char-downcase))
