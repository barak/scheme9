; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (pp form) ==> unspecific
; (pp #t form) ==> unspecific
; (pp #f form) ==> unspecific
; (pp-file string) ==> unspecific
;
; Pretty-print Scheme forms or files.
; When the first argument is #T, format FORM as code. When the first
; argument is #F, format FORM as data. When there is only one argument,
; pass it to PROGRAM? to figure out wehther it is code or data.
; NOTE: This program handles only a subset of R5RS Scheme correctly
; and removes all comments from its input program. Caveat utilitor.
;
; Arguments: a1   - form to pretty-print or #t/#f (PP)
;            a2   - form to pretty-print or () (PP)
;            file - file to pretty-print (PP-FILE)
;
; (Example): (pp '(let ((a 1) (b 2)) (cons a b))) ==> unspecific
;
; Output:   (let ((a 1)
;                 (b 2))
;             (cons a b))

(load-from-library "programp.scm")

; NOTE: In all pretty-printing functions,
;       X is the (sub)expression to print
;       N is the current indentation in characters
;       K is the current output column

(define (pretty-print a1 . a2)

  (define Right-margin 72)

  (define LP #\()
  (define RP #\))

  (define (atom? x)
    (and (not (pair? x))
         (not (null? x))
         (not (vector? x))))

  (define (atom-length x)
    (cond ((null? x) 2)
          ((number? x)
            (string-length (number->string x)))
          ((string? x)
            (+ 2 (string-length x)))
          ((char? x)
            (cond ((char=? x #\newline) 9)
                  ((char=? x #\space) 7)
                  (else 3)))
          ((boolean? x) 2)
          ((symbol? x)
            (string-length (symbol->string x)))
          ((vector? x)
            (list-length x))
          (else
            (wrong "unknown atom" x))))

  (define (quoted? x)
    (and (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (memq (car x) '(quote quasiquote unquote unquote-splicing))
         #t))

  (define (list-length x)
    (cond ((vector? x)
            (+ 1 (list-length (vector->list x))))
          ((not (pair? x))
            (atom-length x))
          ((quoted? x)
            (if (eq? (car x) 'unquote-splicing)
                (+ 2 (list-length (cadr x)))
                (+ 1 (list-length (cadr x)))))
          (else
            (+ 1 (list-length (car x))
                 (let ((k (list-length (cdr x))))
                   (if (atom? (cdr x)) (+ 4 k) k))))))

  (define (form-length x)
    (if (atom? x)
        (atom-length x)
        (list-length x)))

  (define (spaces n)
    (or (zero? n)
        (begin (display #\space)
               (spaces (- n 1)))))

  (define (pp-atom x n k)
    (if (vector? x)
        (pp-vector x n k)
        (begin (write x)
               (atom-length x))))

  (define (exceeds-margin? x k)
    (>= (+ k (form-length x))
        Right-margin))

  (define (linewrap x k)
    (cond ((zero? k) k)
          ((exceeds-margin? x k)
            (newline)
            0)
          (else k)))

  (define (indent k n)
    (if (not (zero? k))
        k
        (begin (spaces n)
               n)))

  (define (pp-quote x n k)
    (display #\')
    (pp-datum (cadr x) n (+ 1 k) #t))

  (define (pp-quasiquote x n k)
    (display #\`)
    (pp-expr (cadr x) n (+ 1 k) #t))

  (define (pp-unquote x n k)
    (display #\,)
    (pp-expr (cadr x) n (+ 1 k) #t))

  (define (pp-unquote-splicing x n k)
    (display ",@")
    (pp-expr (cadr x) n (+ 2 k) #t))

  (define (print-quotation x n k)
    (case (car x)
          ((quote)            (pp-quote x n k))
          ((quasiquote)       (pp-quasiquote x n k))
          ((unquote)          (pp-unquote x n k))
          ((unquote-splicing) (pp-unquote-splicing x n k))))

  (define (pp-members x n k)
    (cond ((null? x) k)
          ((not (pair? x))
            (display ". ")
            (+ 2 k (pp-atom x n k)))
          (else (let* ((k (pp-datum (car x) (+ 1 n) k #f))
                       (k (cond ((null? (cdr x)) k)
                                ((> k 0)
                                  (display #\space)
                                  (+ 1 k))
                                (else 0))))
                  (pp-members (cdr x) n k)))))

  (define (pp-datum x n k glue)
    (let* ((k (if glue k (linewrap x k)))
           (k (indent k n)))
        (cond ((not (pair? x))
                (+ k (pp-atom x n k)))
              ((quoted? x)
                (print-quotation x n k))
              (else
                (display LP)
                (let ((k (pp-members x k (+ 1 k))))
                  (display RP)
                  (+ 1 k))))))

  (define (left-depth a)
    (if (pair? a)
        (+ 1 (left-depth (car a)))
        0))

  (define (pp-vertical-args x n k glue)
    (letrec
      ((pp-args
         (lambda (x n k)
           (let ((n (pp-expr (car x) n k #t)))
             (cond ((null? (cdr x)) n)
                   (else (newline)
                         (indent 0 k)
                         (pp-args (cdr x) n k)))))))
      (indent k n)
      (display LP)
      (let ((k-new (pp-expr (car x) k (+ 1 k) #t))
            (k-arg (left-depth (car x))))
        (cond (glue (display #\space))
              (else (newline)
                    (indent 0 (+ k k-arg))))
        (let ((k (pp-args (cdr x) n (if glue (+ k-new k-arg) (+ k k-arg)))))
          (display RP)
          (+ 1 k)))))

  (define (any? p x)
    (letrec
      ((map-improper
         (lambda (f a)
           (letrec
             ((map-i
                (lambda (a r)
                  (cond ((null? a) (reverse r))
                        ((not (pair? a)) (append (reverse r)
                                                 (list (f a))))
                        (else (map-i (cdr a)
                                     (cons (f (car a)) r)))))))
             (map-i a '())))))
    (and (memq #t (map-improper p x)) #t)))

  (define (pp-app x n k glue)
    (if (exceeds-margin? x k)
        (let* ((len (form-length (car x)))
               (k-new (+ n len)))
          (if (and (> len 2)
                   (any? (lambda (x)
                           (exceeds-margin? x k-new))
                         (cdr x)))
              (pp-vertical-args x n (+ 1 k) #t)
              (pp-vertical-args x n k #t)))
        (pp-datum x n k glue)))

  (define (pp-body x n k glue)
    (cond ((null? x) n)
          (glue (pp-body (cdr x) (pp-expr (car x) n k #t) k #f))
          (else (newline)
                (indent 0 k)
                (pp-body (cdr x) (pp-expr (car x) k k #t) k #f))))

  (define (pp-lambda x n k)
    (display LP)
    (display "lambda ")
    (pp-expr (cadr x) (+ 2 k) (+ 8 k) #t)
    (let ((k (pp-body (cddr x) n (+ 2 k) #f)))
      (display RP)
      (+ 1 k)))

  (define (remove-default x)
    (let ((c (reverse x)))
      (if (or (eq? (caar c) 'else)
              (eq? (caar c) #t))
          (reverse (cdr c))
          x)))

  (define (pp-cond/case kw x n k)
    (letrec
      ((pp-indented-clause
         (lambda (x n k)
           (display LP)
           (pp-expr (caar x) n (+ 1 k) #t)
           (let ((k (pp-body (cdar x) n (+ 2 k) #f)))
             (display RP)
             (+ 1 k))))
       (pp-inline-clause
         (lambda (x n k)
           (display LP)
           (let ((k (pp-expr (caar x) n (+ 1 k) #t)))
             (display #\space)
             (let ((k (pp-body (cdar x) n (+ 1 k) #t)))
               (display RP)
               (+ 1 k)))))
       (pp-clause
         (lambda (x n k)
           (let ((k (indent k n)))
             (cond ((> (length (car x)) 2)
                     (pp-indented-clause x n k))
                   ((and (exceeds-margin? (car x) k)
                         (not (eq? (caar x) #t))
                         (not (eq? (caar x) 'else)))
                     (pp-indented-clause x n k))
                   (else (pp-inline-clause x n k))))))
       (print-clauses
         (lambda (pr x n k)
           (let ((k (pr x n (indent k n))))
             (cond ((null? (cdr x))
                     (display RP)
                     (+ 1 k))
                   (else (newline)
                         (print-clauses pr (cdr x) n 0))))))
      (multi-expr-clauses?
        (lambda (x)
          (any? (lambda (x)
                  (> (length x) 2))
                (remove-default x))))
      (long-clauses?
        (lambda (x k)
          (any? (lambda (x)
                  (exceeds-margin? x k))
                (remove-default x)))))
      (display LP)
      (display (if (eq? kw 'cond) "cond " "case "))
      (let ((printer (if (or (multi-expr-clauses? (cdr x))
                             (long-clauses? (cdr x) (+ k 6)))
                         pp-indented-clause
                         pp-inline-clause))
            (clauses (cond ((eq? kw 'cond)
                             (cdr x))
                           (else (pp-expr (cadr x) n (+ k 6) #t)
                                 (cddr x)))))
        (let ((base (cond ((eq? kw 'cond) (+ k 6))
                          (else (newline)
                                0))))
          (print-clauses printer clauses (+ k 6) base)))))

  (define (pp-do x n k)
    (letrec
      ((indent-inits
         (lambda (x n k first)
           (cond ((null? x) k)
                 (first (let ((k-new (pp-expr (car x) n k #t)))
                          (indent-inits (cdr x) k k #f)
                          k-new))
                 (else (newline)
                       (indent 0 k)
                       (let ((k-new (pp-expr (car x) k k #t)))
                         (indent-inits (cdr x) k k #f)
                         k-new))))))

      (display LP)
      (display "do ")
      (display LP)
      (indent-inits (cadr x) (+ n 5) (+ k 5) #t)
      (display RP)
      (newline)
      (indent 0 (+ 4 k))
      (pp-expr (caddr x) n k #t)
      (pp-body (cdddr x) n (+ 4 k) #f)
      (display RP)))

  (define (pp-if x n k)
    (cond ((exceeds-margin? x k)
            (display LP)
            (display "if ")
            (pp-expr (cadr x) (+ 4 n) (+ 4 k) #t)
            (newline)
            (let ((k (pp-expr (caddr x) (+ 4 n) 0 #f)))
              (cond
                ((null? (cdddr x))
                  (display RP)
                  (+ 1 k))
                (else (newline)
                      (let ((k (pp-expr (cadddr x) (+ 4 n) 0 #f)))
                        (display RP)
                        (+ 1 k))))))
          (else (pp-app x n k #t))))

  (define (pp-indented x n k prefix always-split)
    (let ((pl (+ 1 (string-length prefix))))
      (letrec
        ((indent-args
           (lambda (x n k glue)
             (let ((k (pp-expr (car x) n k glue)))
               (cond ((null? (cdr x))
                       (display RP)
                       (+ 1 k))
                     (else (newline)
                           (indent-args (cdr x) n 0 #f)))))))
        (cond ((or (and (> (length x) 1) (exceeds-margin? x k))
                   always-split)
                (display LP)
                (display prefix)
                (indent-args (cdr x) (+ k pl) (+ k pl) #t))
              (else (pp-app x (+ k pl) k #f))))))

  (define (pp-and x n k)
    (pp-indented x n k "and " #f))

  (define (pp-or x n k)
    (pp-indented x n k "or " #f))

  (define (pp-begin x n k)
    (pp-indented x n k "begin " #t))

  (define (pp-let-body x n k ind)
    (letrec
      ((lambda?
         (lambda (x)
           (and (pair? x) (eq? 'lambda (car x)))))
       (pp-let-procedure
         (lambda (x n k)
           (pp-expr (caar x) n (+ 1 k) #t)
           (newline)
           (let ((k (pp-expr (cadar x) (+ 2 n) 0 #t)))
             (display RP)
             (+ 2 k))))
       (pp-let-data
         (lambda (x n k)
           (let ((k (pp-expr (caar x) n (+ 1 k) #t)))
             (display #\space)
             (let ((k (pp-expr (cadar x) (+ 2 n) (+ 1 k) #t)))
               (display RP)
               (+ 2 k)))))
       (pp-assoc
         (lambda (x n k)
           (let ((k (indent k n)))
             (display LP)
             (cond ((lambda? (cadar x))
                     (pp-let-procedure x n k))
                   (else (pp-let-data x n k))))))
       (indent-bindings
         (lambda (x n k)
           (let ((k (pp-assoc x n k)))
             (cond ((null? (cdr x))
                     (display RP)
                     (+ 1 k))
                   (else (newline)
                         (indent-bindings (cdr x) n 0)))))))
      (indent-bindings (cadr x) (+ n ind) k)
      (let ((k (pp-body (cddr x) n (+ 2 n) #f)))
        (display RP)
        (+ 1 k))))

  (define (pp-let x n k)
    (display LP)
    (display "let ")
    (cond ((symbol? (cadr x))
            (let ((m (pp-atom (cadr x) n k)))
              (display #\space)
              (display LP)
              (pp-let-body (cdr x) k (+ 7 m k) (+ 7 m))))
          (else
            (display LP)
            (pp-let-body x k (+ 6 k) 6))))

  (define (pp-let* x n k)
    (display LP)
    (display "let* ")
    (display LP)
    (pp-let-body x k (+ 7 k) 7))

  (define (pp-letrec x n k)
    (display LP)
    (display "letrec ")
    (newline)
    (let ((k (indent 0 (+ k 2))))
      (display LP)
      (pp-let-body x n (+ 1 k) 3)))

  (define (pp-define x n k)
    (cond ((pair? (cadr x))
            (display LP)
            (display "define ")
            (pp-datum (cadr x) n k #t)
            (let ((k (pp-body (cddr x) n (+ 2 k) #f)))
              (display RP)
              (+ 1 k)))
          (else (pp-app x n k #f))))

  (define (pp-define-syntax x n k)
    (display LP)
    (display "define-syntax ")
    (pp-datum (cadr x) n k #t)
    (newline)
    (indent 0 (+ 2 k))
    (let ((k (pp-expr (caddr x) 0 (+ 2 k) #t)))
      (display RP)
      (+ 1 k)))

  (define (pp-syntax-rules x n k)
    (letrec
      ((pp-rules
         (lambda (x n k)
           (cond ((null? x) k)
                 (else (indent 0 k)
                       (display LP)
                       (pp-datum (caar x) n (+ 1 k) #t)
                       (newline)
                       (indent 0 (+ 3 k))
                       (let* ((n (pp-expr (cadar x) (+ 3 k) (+ 3 k) #t)))
                         (display RP)
                         (cond
                           ((null? (cdr x))
                             (+ 1 n))
                           (else (newline)
                                 (pp-rules (cdr x) 0 k)))))))))
      (display LP)
      (display "syntax-rules ")
      (pp-datum (cadr x) (+ 16 k) (+ 14 k) #t)
      (newline)
      (let ((k (pp-rules (cddr x) 0 (+ 2 k))))
        (display RP)
        (+ 1 k))))

  (define (pp-vector x n k)
    (display "'#")
    (display LP)
    (let ((k (pp-members (vector->list x) (+ 3 n) (+ 3 k))))
      (display RP)
      (+ 1 k)))

  (define (pp-expr x n k glue)
    (let* ((k (if glue k (linewrap x k)))
           (k (indent k n)))
      (cond ((not (pair? x)) (+ k (pp-atom x n k)))
            ((quoted? x) (print-quotation x n k))
            ((eq? (car x) 'lambda) (pp-lambda x n k))
            ((eq? (car x) 'cond) (pp-cond/case 'cond x n k))
            ((eq? (car x) 'case) (pp-cond/case 'case x n k))
            ((eq? (car x) 'do) (pp-do x n k))
            ((eq? (car x) 'if) (pp-if x n k))
            ((eq? (car x) 'and) (pp-and x n k))
            ((eq? (car x) 'or) (pp-or x n k))
            ((eq? (car x) 'let) (pp-let x n k))
            ((eq? (car x) 'let*) (pp-let* x n k))
            ((eq? (car x) 'letrec) (pp-letrec x n k))
            ((eq? (car x) 'begin) (pp-begin x n k))
            ((eq? (car x) 'define) (pp-define x n k))
            ((eq? (car x) 'define-syntax) (pp-define-syntax x n k))
            ((eq? (car x) 'syntax-rules) (pp-syntax-rules x n k))
            (else (pp-app x n k #t)))))

  (cond ((null? a2)
          (if (program? a1)
              (pp-expr a1 0 0 #f)
              (pp-datum a1 0 0 #f)))
        (a1
          (pp-expr (car a2) 0 0 #f))
        (else
          (pp-datum (car a2) 0 0 #t)))
  (newline))

(define pp pretty-print)

(define (pp-file file)
  (letrec
    ((pp*
       (lambda (x)
         (cond ((not (eof-object? x))
                 (pp #t x)
                 (let ((next (read)))
                   (if (not (eof-object? next))
                       (newline))
                   (pp* next)))))))
    (with-input-from-file
      file
      (lambda ()
        (pp* (read))))))
