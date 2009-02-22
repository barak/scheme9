; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (read-line . port) ==> string
;
; Read a line from an input port.
;
; Arguments: port - port to read, default = current input port
;
; Example:   (with-input-from-file "lib/read-line.scm" read-line)
;              ==> "; Scheme 9 from Empty Space, Function Library"

(define (read-line . port)
  (letrec
    ((collect-chars
       (lambda (c s)
         (cond ((eof-object? c)
                 (if (null? s)
                     c
                     (list->string (reverse s))))
               ((char=? c #\newline)
                 (list->string (reverse s)))
               (else (collect-chars (apply read-char port)
                                    (cons c s)))))))
    (collect-chars (apply read-char port) '())))
