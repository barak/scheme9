; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (write-to-string expr) ==> string
;
; Write external representation to string.
; WRITE-TO-STRING is like WRITE but writes its output
; to a string instead of an output port.
;
; Arguments: x - form to write
;
; Example:   (write-to-string '(a 1 #\c #(v) #t "s" (a . d)))
;            ==> "(a 1 #\\c #(v) #t \"s\" (a . d))"

(define (write-to-string x)

  (define (stringify-improper-list a first)
    (cond
      ((pair? a)
        (cons (string-append (if first "" " ")
                             (write-to-string (car a)))
              (stringify-improper-list (cdr a) #f)))
      ((null? a) '())
      (else (list (string-append " . " (write-to-string a))))))

  (define (char->string c)
    (let ((v (char->integer c)))
      (cond ((= v 10) "#\\newline")
            ((= v 32) "#\\space")
            ((or (<= 0 v 31)
                 (> v 126))
              (string-append "#<unrepresentable character, code="
                             (number->string v)
                             ">"))
            (else (string-append "#\\" (string c))))))

  (cond ((eq? #t x) "#t")
        ((eq? #f x) "#f")
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((char? x) (char->string x))
        ((string? x) (string-append "\"" x "\""))
        ((null? x) "()")
        ((pair? x)
          (string-append
            "("
            (apply string-append
                   (stringify-improper-list x #t))
            ")"))
        ((vector? x)
          (string-append "#" (write-to-string (vector->list x))))
        ((procedure? x) "#<procedure>")
        ((eof-object? x) "#<eof>")
        (else "#<unspecific>")))
