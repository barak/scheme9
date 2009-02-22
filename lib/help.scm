; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (help) ==> unspecific
; (help symbol) ==> unspecific
;
; Display the synopsis of the given procedure or keyword.
; When SYMBOL is described in R4RS, produce its R4RS entry,
; otherwise display a S9FES-specific summary. When no symbol
; is passed to help, it explains itself.
;
; Arguments: sym - symbol to explain
;
; (Example): (help symbol?)  ==>  unspecific
;
; Output:    R4RS 6.4 (symbol? obj)
;
;            Returns #t if obj is a symbol, otherwise returns #f.
;
;            (symbol? 'foo)          ==>  #t
;            (symbol? (car '(a b)))  ==>  #t
;            (symbol? "bar")         ==>  #f
;            (symbol? 'nil)          ==>  #t
;            (symbol? '())           ==>  #f
;            (symbol? #f)            ==>  #f

(load-from-library "read-line.scm")
(load-from-library "string-contains.scm")
(load-from-library "remove.scm")
(load-from-library "mergesort.scm")

(define *lines-per-page* 20)

(define (procname->filename name)
  (let xlate ((in  (string->list name))
        (out ()))
    (cond ((null? in)
            (if (string=? name "-")
                "minus"
                (apply string-append (reverse out))))
          ((char=? #\+ (car in))
            (xlate (cdr in) (cons "plus" out)))
          ((char=? #\* (car in))
            (xlate (cdr in) (cons "star" out)))
          ((char=? #\? (car in))
            (xlate (cdr in) (cons "p" out)))
          ((char=? #\! (car in))
            (xlate (cdr in) (cons "b" out)))
          ((char=? #\: (car in))
            (xlate (cdr in) (cons "_" out)))
          ((char=? #\= (car in))
            (xlate (cdr in) (cons "eq" out)))
          ((and (char=? #\- (car in))
                (pair? (cdr in))
                (char=? #\> (cadr in)))
            (xlate (cddr in) (cons "-to-" out)))
          ((char=? #\< (car in))
            (if (and (pair? (cdr in))
                     (char=? #\= (cadr in)))
                (xlate (cddr in) (cons "le" out))
                (xlate (cdr in) (cons "lt" out))))
          ((char=? #\> (car in))
            (if (and (pair? (cdr in))
                     (char=? #\= (cadr in)))
                (xlate (cddr in) (cons "ge" out))
                (xlate (cdr in) (cons "gt" out))))
          (else (xlate (cdr in) (cons (string (car in)) out))))))

(define help
  (let ((procname->filename procname->filename))
    (lambda sym

      (define (more? tty)
        (display "----- more (enter q to quit) -----")
        (not (string-contains (read-line tty) "q")))

      (define (show-file file)
        (read-line)
        (newline)
        (let ((tty (current-input-port)))
          (with-input-from-file file
            (lambda ()
              (let print ((line (read-line))
                          (lno  1))
                (cond ((eof-object? line)
                    (newline))
                  ((and (not (zero? *lines-per-page*))
                            (= lno *lines-per-page*))
                        (if (more? tty)
                            (print line 0)))
                      (else
                        (display line)
                        (newline)
                        (print (read-line) (+ 1 lno)))))))))

      (let* ((name (cond ((null? sym)
                           "help")
                         ((symbol? (car sym))
                           (symbol->string (car sym)))
                         ((string? (car sym))
                           (car sym))
                         (else (wrong "help: expected string or symbol, got"
                                      (car sym)))))
             (name (procname->filename name)))
        (cond ((locate-file (string-append "help/" name))
                => show-file)
              (else
                (wrong "help: could not file help page" name)))))))

(define apropos
  (let ((procname->filename procname->filename))
    (lambda sym
      
      (let* ((name (cond ((null? sym)
                           "")
                         ((symbol? (car sym))
                           (symbol->string (car sym)))
                         ((string? (car sym))
                           (car sym))
                         (else (wrong
                                 "apropos: expected string or symbol, got"
                                 (car sym))))))
        (mergesort
          (lambda (a b)
            (string<=? (symbol->string a)
                       (symbol->string b)))
          (remove null?
                  (map (lambda (x)
                         (let ((s (symbol->string x)))
                           (if (and (string-contains s name)
                                    (locate-file
                                      (string-append
                                        "help/"
                                        (procname->filename s))))
                               x
                               '())))
                       (symbols))))))))
