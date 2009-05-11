; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (print-code string . show-matches) ==> unspecific
;
; Render Scheme code contained in a file in HTML with syntax
; highlighting and optional CSS paren matching.
; The CSS2 style sheet "scheme.css" contains the default style for
; syntax and expression highlighting. NOTE: This program handles
; only a subset of R5RS Scheme correctly. Caveat utilitor.
;
; Arguments: file         - program file to render
;            show-matches - when given and not #F, do CSS paren matching
;
; (Example): (print-code "print-code.scm") ==> unspecific

(define (print-code file . show-matches)

  (define *load-from-library* 0)

  (define LP #\()
  (define RP #\))

  (define (show-matches?)
    (and (not (null? show-matches))
         (car show-matches)))

  (define (Prolog) (if (show-matches?)
                       "<PRE class=scheme-hl><SPAN>"
                       "<PRE class=scheme><SPAN>"))

  (define (Epilog) "</SPAN></PRE>")

  (define Color-comment    "o")
  (define Color-paren      "p")
  (define Color-symbol     "s")
  (define Color-constant   "c")
  (define Color-std-proc   "r")
  (define Color-std-syntax "y")
  (define Color-ext-proc   "x")
  (define Color-ext-syntax "z")

  (define Color #f)
  (define Bold #f)

  (define (html-display s)
    (display
      (apply string-append
        (map (lambda (c)
               (case c
                     ((#\<) "&lt;")
                     ((#\>) "&gt;")
                     ((#\&) "&amp;")
                     (else  (string c))))
             (string->list s)))))

  (define (change-color quoted co bo thunk)
    (cond (quoted
            (thunk))
          ((and (equal? co Color) (eq? bo Bold))
            (thunk))
          (else
            (if Bold
                (begin (display "</B>")
                       (set! Bold #f)))
            (display "</SPAN>")
            (display "<SPAN class=")
            (display co)
            (display ">")
            (if (and bo (not Bold))
                (display "<B>"))
            (set! Color co)
            (set! Bold bo)
            (thunk))))

  (define (with-color quoted co thunk)
    (change-color quoted co #f thunk))

  (define (with-bold-color quoted co thunk)
    (change-color quoted co #t thunk))

  (define (symbolic? c)
    (let ((specials "!%&*+-./:<=>?@^_"))
      (or (char-alphabetic? c)
          (char-numeric? c)
          (and (memv c (string->list specials)) #t))))

  (define (print-paren c q)
    (cond ((show-matches?)
            (if Bold (begin (display "</B>") (set! Bold #f)))
            (display "</SPAN>")
            (if (char=? c LP) (display "<SPAN class=n>"))
            (display "<SPAN class=")
            (display (if q Color-constant Color-paren))
            (display ">")
            (set! Color #f)
            (display c)
            (if (char=? c RP) (display "</SPAN></SPAN><SPAN>")))
          (else
            (with-color q
                        Color-paren
                        (lambda () (display c)))))
        (read-char))

  (define (r5rs-syntax? s)
    (and (memq (string->symbol s)
               '(and begin case cond define define-syntax delay do
                 else if lambda let let* letrec quote quasiquote or
                 set! syntax-rules unquote unquote-splicing))
         #t))

  (define (s9fes-syntax? s)
    (and (memq (string->symbol s)
               '(define-macro))
         #t))

  (define (r5rs-procedure? s)
    (and (memq (string->symbol s)
               '(* + - < <= = > >= abs append apply assoc assq assv
                 boolean?  caaaar caaadr caaar caadar caaddr caadr
                 caar cadaar cadadr cadar caddar cadddr caddr cadr
                 call-with-input-file call-with-output-file car cdaaar
                 cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
                 cddar cdddar cddddr cdddr cddr cdr char->integer
                 char-alphabetic? char-ci<=? char-ci<? char-ci=?
                 char-ci>=? char-ci>? char-downcase char-lower-case?
                 char-numeric? char-upcase char-upper-case?
                 char-whitespace? char<=? char<? char=? char>=? char>?
                 char? close-input-port close-output-port cons
                 current-input-port current-output-port display
                 eof-object? eq? equal? eqv? even? expt for-each
                 force gcd input-port? integer->char integer? lcm
                 length list list->string list->vector list-ref
                 list-tail list? load make-string make-vector map
                 max member memq memv min modulo negative? newline
                 not null? number->string number? odd? open-input-file
                 open-output-file output-port? pair? peek-char port?
                 positive? procedure? quotient read read-char remainder
                 reverse set-car! set-cdr! sqrt string string->list
                 string->number string->symbol string-append
                 string-ci<=? string-ci<? string-ci=? string-ci>=?
                 string-ci>? string-copy string-fill! string-length
                 string-ref string-set! string<=? string<? string=?
                 string>=? string>? string? substring symbol->string
                 symbol? unquote unquote-splicing vector vector->list
                 vector-fill! vector-length vector-ref vector-set!
                 vector? with-input-from-file with-output-to-file
                 write write-char zero?))
         #t))

  (define (s9fes-procedure? s)
    (and (memq (string->symbol s)
               '(delete-file expand-quasiquote expand-macro file-exists?
                 fold-left fold-right gensym load-from-library locate-file
                 print set-input-port! set-output-port! stats symbols
                 trace void wrong))
         #t))

  (define (print-symbol-or-number c q)
    (letrec
      ((collect
         (lambda (c s)
           (cond ((symbolic? c)
                   (collect (read-char) (cons c s)))
                 (else (cons c (list->string (reverse s))))))))
      (let ((c/s (collect c '())))
        (cond ((string->number (cdr c/s))
                (with-color q
                            Color-constant
                            (lambda () (html-display (cdr c/s)))))
              ((r5rs-syntax? (cdr c/s))
                (with-bold-color q
                                 Color-std-syntax
                                 (lambda () (html-display (cdr c/s)))))
              ((r5rs-procedure? (cdr c/s))
                (with-color q
                            Color-std-proc
                            (lambda () (html-display (cdr c/s)))))
              ((s9fes-syntax? (cdr c/s))
                (with-bold-color q
                                 Color-ext-syntax
                                 (lambda () (html-display (cdr c/s)))))
              ((s9fes-procedure? (cdr c/s))
                (if (string=? "load-from-library" (cdr c/s))
                    (set! *load-from-library* 2))
                (with-color q
                            Color-ext-proc
                            (lambda () (html-display (cdr c/s)))))
              (else
                (with-color q
                            Color-symbol
                            (lambda () (html-display (cdr c/s))))))
        (car c/s))))

  (define (print-string c)
    (letrec
      ((collect
         (lambda (c s esc)
           (cond ((and (char=? c #\") (not esc))
                   (list->string (reverse (cons #\" s))))
                 (else (collect (read-char)
                                (cons c s)
                                (and (not esc) (char=? #\\ c))))))))
      (let* ((s  (collect c '() #t))
             (s2 (substring s 1 (- (string-length s) 1))))
        (if (= *load-from-library* 1)
            (with-color #f
                        Color-constant
                        (lambda ()
                          (html-display "\"")
                          (display "<A href=\"")
                          (display s2)
                          (display ".html")
                          (display "\">")
                          (html-display s2)
                          (display "</A>")
                          (html-display "\"")))
            (with-color #f
                        Color-constant
                        (lambda () (html-display s)))))
      (read-char)))

  (define (print-comment c)
    (letrec
      ((collect
         (lambda (c s)
           (cond ((char=? c #\newline)
                   (list->string (reverse s)))
                 (else (collect (read-char) (cons c s)))))))
      (with-color #f
                  Color-comment
                  (lambda ()
                    (html-display (collect c '()))))
      #\newline))

  (define (print-const s)
    (with-color #f
                Color-constant
                (lambda () (html-display s)))
    (read-char))

  (define (print-char c)
    (letrec
      ((collect
         (lambda (c s)
           (cond ((char-alphabetic? c)
                   (collect (read-char) (cons c s)))
                 (else (cons c (list->string (reverse s))))))))
      (with-color #f
                  Color-constant
                  (lambda ()
                    (display "#\\")
                    (let ((c/s (collect (read-char) (list c))))
                      (html-display (cdr c/s))
                      (car c/s))))))

  (define (print-hash-syntax c p)
    (let ((c (read-char)))
      (case c
            ((#\f) (cons p (print-const "#f")))
            ((#\t) (cons p (print-const "#t")))
            ((#\\) (cons p (print-char (read-char))))
            ((#\() (cons p (print-const "#(")))
            (else  (wrong "unknown # syntax" c)))))

  (define (print-quoted c p q type)
    (with-bold-color
      q
      Color-std-syntax
      (lambda ()
        (display (if (eq? type 'quote) #\' #\`))))
      (with-color #f
                  Color-constant
                  (lambda ()
                    (print-quoted-form (read-char)
                                       p
                                       (if (eq? q 'quasiquote)
                                           'qusiquote
                                           type)))))

  (define (print-unquoted p q)
    (with-bold-color
      (eq? q 'quote)
      Color-std-syntax
      (lambda ()
        (display #\,)
        (if (char=? (peek-char) #\@)
            (display (read-char)))))
    (if (not (eq? q 'quote))
        (let ((p/c (print-quoted-form (read-char) p #f)))
          (with-color #f
                      Color-constant
                      (lambda () p/c)))
          (cons p (read-char))))

  (define (print-object c p q)
    (cond ((char=? c LP)  (cons (+ 1 p) (print-paren LP q)))
          ((char=? c RP)  (cons (- p 1) (print-paren RP q)))
          ((symbolic? c)  (cons p (print-symbol-or-number c q)))
          ((char=? c #\") (cons p (print-string c)))
          ((char=? c #\;) (cons p (print-comment c)))
          ((char=? c #\#) (print-hash-syntax c p))
          ((char=? c #\') (print-quoted c p q 'quote))
          ((char=? c #\`) (print-quoted c p q 'quasiquote))
          ((char=? c #\,) (print-unquoted p q))
          (else           (wrong "unknown character class" c))))

  (define (skip-whitespace c)
    (cond ((eof-object? c) c)
          ((char-whitespace? c)
            (display c)
            (skip-whitespace (read-char)))
          (else c)))

  (define (print-quoted-list c p p0 type)
    (let ((c (skip-whitespace c)))
      (cond ((eof-object? c) (cons p c))
            (else (let ((p/c (print-object c p type)))
                    (if (<= (car p/c) p0)
                        p/c
                        (print-quoted-list (cdr p/c)
                                           (car p/c)
                                           p0
                                           type)))))))

  (define (print-quoted-form c p type)
    (let ((p/c (print-object c p type)))
      (if (= (car p/c) p)
          p/c
          (print-quoted-list (cdr p/c) (car p/c) p type))))

  (define (print-program c p q)
    (let ((c (skip-whitespace c)))
      (if (not (eof-object? c))
          (let ((p/c (print-object c p q)))
            (set! *load-from-library*
                  (if (zero? *load-from-library*)
                      0
                      (- *load-from-library* 1)))
            (print-program (cdr p/c) (car p/c) q)))))

  (with-input-from-file
    file
    (lambda ()
      (display (Prolog))
      (print-program (read-char) 0 #f)
      (display (Epilog))
      (newline))))
