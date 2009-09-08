; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (read-from-string string)  ==>  object
;
; Read a datum from a string. READ-FROM-STRING is like READ but it
; takes its input from a string instead of a port. It returns a pair
; containing the datum read in case of success. If there are any
; trailing characters after the extracted datum, the trailing string
; is placed in the cdr part of the returned pair. If the trailing
; string is empty, the cdr part is set to (). When an empty string
; or a string consisting of a comment exclusively is passed to
; READ-FROM-STRING, it returns (). In case of an error, a string
; explaining the cause of the error is returned.
;
; Example:   (read-from-string "  (this \"is\" #(a) (list)) ; comment")
;              ==>  ((this "is" #(a) (list)))
;
;            (read-from-string "  (this \"is\" #(a) (list))  more text")
;              ==>  ((this "is" #(a) (list)) . "  more text")
;
;            (read-from-string ")")
;              ==>  "unexpected closing parenthesis"

(define (read-from-string s)

  (define LPAREN #\()
  (define RPAREN #\))

  (define separator?
    (let ((separators
            (append (string->list "#'(),;`\"")
                    (list #\space
                          (integer->char 9)
                          (integer->char 10)
                          (integer->char 12)
                          (integer->char 13)))))
    (lambda (c)
      (and (memv c separators) #t))))

  (define (skip-blanks s)
    (cond ((null? s) '())
          ((char-whitespace? (car s))
             (skip-blanks (cdr s)))
          (else s)))

  (define (digit-value c)
    (- (char->integer c)
       (char->integer #\0)))

  (define (read-number s)
    (letrec
      ((read-number2
         (lambda (s n)
           (cond ((null? s)
                   (cons n s))
                 ((char-numeric? (car s))
                   (read-number2 (cdr s)
                                 (+ (* 10 n)
                                    (digit-value (car s)))))
                 (else (cons n s))))))
      (read-number2 s 0)))

  (define char-symbolic?
    (let ((symbol-chars (string->list "+-.*/<=>!?:$%_&~^")))
      (lambda (c)
        (or (char-alphabetic? c)
            (and (memv c symbol-chars) #t)))))

  (define (read-symbol s)
    (letrec
      ((rev-lst->sym
         (lambda (s)
           (string->symbol (list->string (reverse s)))))
       (read-symbol2
         (lambda (s sym)
           (cond ((null? s)
                   (cons (rev-lst->sym sym) s))
                 ((char-symbolic? (car s))
                   (read-symbol2 (cdr s) (cons (car s) sym)))
                 (else (cons (rev-lst->sym sym) s))))))
      (read-symbol2 s '())))

  (define (read-symbol-or-number s)
    (let ((s0 (car s))
          (s1 (if (pair? (cdr s)) (cadr s) #\x)))
      (if (char-numeric? s1)
          (if (char=? #\- s0)
              (let ((r (read-number (cdr s))))
                (cons (- (car r)) (cdr r)))
              (read-number (cdr s)))
          (read-symbol s))))

  (define (read-string s)
    (letrec
      ((rev-lst->str
         (lambda (s)
           (list->string (reverse s))))
       (read-string3
         (lambda (s t q)
           (cond ((null? s)
                   "unterminated string literal")
                 ((and (not q)
                       (char=? #\" (car s)))
                   (cons (rev-lst->str t) (cdr s)))
                 ((char=? #\\ (car s))
                   (read-string3 (cdr s)
                                 (cons (car s) t) #t))
                 (else
                   (read-string3 (cdr s)
                                 (cons (car s) t) #f))))))
      (read-string3 (cdr s) '() #f)))

  (define (read-character s)
    (cond ((null? (cddr s))
            "bad char literal")
          ((null? (cdddr s))
            (cons (caddr s) (cdddr s)))
          ((separator? (cadddr s))
            (cons (caddr s) (cdddr s)))
          (else (let ((r (read-symbol (cddr s))))
                  (case (car r)
                    ((space)   (cons #\space (cdr r)))
                    ((newline) (cons #\newline (cdr r)))
                    (else      "bad character name"))))))

  (define (read-dotted-cdr s lst)
    (let ((s (skip-blanks (cdr s))))
      (if (or (null? s)
              (char=? RPAREN (car s)))
          "missing cdr part in dotted pair"
          (let ((x (char-list->datum s)))
            (if (pair? x)
                (let ((s (skip-blanks (cdr x))))
                  (if (or (null? s)
                          (not (char=? RPAREN (car s))))
                      "missing closing parenthesis in dotted list"
                      (cons (append (reverse lst) (car x)) (cdr s))))
                x)))))

  (define (read-pair s)
    (letrec
      ((read-list
         (lambda (s lst)
           (let ((s (if (and (pair? s)
                             (char-whitespace? (car s)))
                        (skip-blanks (cdr s))
                        s)))
             (cond ((null? s)
                     "missing closing parenthesis")
                   ((char=? RPAREN (car s))
                     (cons (reverse lst) (cdr s)))
                   ((and (char=? #\. (car s))
                         (pair? (cdr s))
                         (separator? (cadr s)))
                     (read-dotted-cdr s lst))
                   (else (let ((x (char-list->datum s)))
                           (if (pair? x)
                               (read-list (cdr x)
                                          (cons (car x) lst))
                               "unexpected end of list"))))))))
      (read-list (cdr s) '())))

  (define (read-hash s)
    (let ((s1 (if (pair? (cdr s)) (cadr s) #\<)))
      (case s1
        ((#\t) (cons #t (cddr s)))
        ((#\f) (cons #f (cddr s)))
        ((#\\) (read-character s))
        ((#\() (let ((x (read-pair (cdr s))))
                 (if (pair? x)
                     (if (list? (car x))
                         (cons (list->vector (car x)) (cdr x))
                         "bad vector syntax")
                     x)))                      ; #\) balance parens
        (else  "bad # syntax"))))

  (define (read-quote s q)
    (let ((x (char-list->datum (cdr s))))
      (cond ((pair? x)
              (cons (list q (car x)) (cdr x)))
            ((null? x)
              (string-append "object expected after "
                             (symbol->string q)))
            (else x))))

  (define (char-list->datum s)
    (let ((s (skip-blanks s)))
      (cond ((null? s)
              '())
            ((char=? #\; (car s))
              '())
            ((char-symbolic? (car s))
              (read-symbol s))
            ((char-numeric? (car s))
              (read-number s))
            ((or (char=? #\+ (car s))
                 (char=? #\- (car s)))
              (read-symbol-or-number s))
            ((char=? #\" (car s))
              (read-string s))
            ((char=? #\# (car s))
              (read-hash s))
            ((char=? #\' (car s))
              (read-quote s 'quote))
            ((char=? #\` (car s))
              (read-quote s 'quasiquote))
            ((char=? #\, (car s))
              (if (and (not (null? (cdr s)))
                       (char=? #\@ (cadr s)))
                  (read-quote (cdr s) 'unquote-splicing)
                  (read-quote s 'unquote)))
            ((char=? LPAREN (car s))
              (read-pair s))
            ((char=? RPAREN (car s))
              "unexpected closing parenthesis")
            (else (error "can't parse this" (list->string s))))))

  (define (string->datum s)
    (char-list->datum (string->list s)))

  (let ((r (string->datum s)))
    (if (pair? r)
        (if (null? (cdr r))
            (list (car r))
            (let ((r2 (char-list->datum (cdr r))))
              (if (null? r2)
                  (list (car r))
                  (cons (car r) (list->string (cdr r))))))
        r)))
