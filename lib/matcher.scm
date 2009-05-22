; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (define-matcher symbol clause ...)                ==>  procedure
; (let-matcher symbol (clause ...) expression ...)  ==>  procedure
; (ml-match name program ...)                       ==>  procedure
;
; These constructs allow to write programs using pattern matching
; style. Each program is a set of clauses containing a pattern in
; the car part and a series of expressions in the cdr part. When
; the pattern matches the arguments passed to the program, the
; corresponding body is evaluated. Symbols used in the pattern
; are bound to the values matched by them in the scope of the body.
; The symbol _ matches any object without binding to it.
;
; A procedure resembling LENGTH may be written in this way using
; DEFINE-MATCHER:
;
; (define-matcher len
;   ((())      0)               ; The empty list has a length of 0.
;   (((_ . x)) (+ 1 (len x))))  ; A list whose car part does not
;                               ; matter and whose cdr part is X
;                               ; has a length of 1 plus the length
;                               ; of X.
;
; Here is the same program using ML-style syntax:
;
; (ml-match
;   len (())      = 0
;     : ((_ . x)) = (+ 1 (len x)))
;
; (It uses : instead of | to separate cases, because | is not
;  a valid Scheme symbol.)
;
; The following more elaborate example implements a subset of
; ML-MATCH by using pattern matching itself. The only difference
; to ML-MATCH is that it does not allow multiple expressions in
; bodies:
;
; (define-macro (simple-match name . clauses)
;   (let-match next-clause
;     (((out ())
;        (reverse out))
;      ((out (pattern '= expr ': . clauses))
;        (next-clause `((,pattern ,expr) ,@out) clauses))
;      ((out (pattern '= expr))
;        (next-clause `((,pattern ,expr) ,@out) ())))
;     `(define ,name
;        ,(apply match name (next-clause '() clauses)))))
;
; SIMPLE-MATCH works in the same way as ML-MATCH. Here is a
; function resembling APPEND using SIMPLE-MATCH:
;
; (simple-match
;   appnd (() x)      = x
;       : ((h . t) x) = (cons h (appnd t x)))
;
; Example:   (let-matcher fac
;              (((x)   (fac x 1))
;               ((0 r) r)
;               ((x r) (fac (- x 1) (* x r))))
;              (fac 10))
;                                          ==>  3628800
;            (begin
;              (ml-match
;                appnd (() x)      = x
;                    : ((h . t) x) = (cons h (appnd t x)))
;              (appnd '(a b c) '(d e f)))
;                                          ==>  (a b c d e f)


(define (match name . clauses)

  (define *accessors* '())

  (define *args* 'args)

  (define (make-accessor path)
    (if (null? path)
        '(lambda (x) x)
        (let ((a (string->symbol
                   (string-append
                     "c"
                     (apply string-append (map symbol->string path))
                     "r"))))
          (if (not (memq a *accessors*))
              (set! *accessors* (cons a *accessors*)))
          a)))

  (define (match-object type-pred eqv-pred accessor pattern)
    `((let ((tmp (,(make-accessor accessor) ,*args*)))
        (and (,type-pred tmp)
             (,eqv-pred tmp ,pattern)))))

  (define (make-matcher pattern accessor)
    (cond ((and (pair? pattern)
                (eq? 'quote (car pattern)))
             (match-object 'symbol? 'eq? accessor pattern))
          ((pair? pattern)
            (let loop ((p pattern)
                       (a accessor)
                       (r '()))
              (cond ((pair? p)
                      (loop (cdr p)
                            (cons 'd a)
                            (cons (append `((pair? (,(make-accessor a)
                                                      ,*args*)))
                                          (make-matcher (car p)
                                                        (cons 'a a)))
                                  r)))
                    ((null? p)
                      (apply append (reverse (cons (make-matcher p a) r))))
                    (else
                      (apply append (reverse (cons (make-matcher p a)
                                     r)))))))
          (else
            (cond ((symbol? pattern)
                    `())
                  ((null? pattern)
                    `((null? (,(make-accessor accessor) ,*args*))))
                  ((number? pattern)
                    (match-object 'number? '= accessor pattern))
                  ((boolean? pattern)
                    (match-object 'boolean? 'eq? accessor pattern))
                  ((char? pattern)
                    (match-object 'char? 'char=? accessor pattern))
                  ((string? pattern)
                    (match-object 'string? 'string=? accessor pattern))
                  ((vector? pattern)
                    (match-object 'vector? 'equal? accessor pattern))
                  (else (wrong "unknown object in pattern" pattern))))))

  (define (fetch-variables pattern accessor)
    (cond ((pair? pattern)
            (let loop ((p pattern)
                       (a accessor)
                       (r '()))
              (cond ((and (pair? p)
                          (eq? 'quote (car p)))
                      (apply append (reverse r)))
                    ((pair? p)
                      (loop (cdr p)
                            (cons 'd a)
                            (cons (fetch-variables (car p) (cons 'a a))
                                  r)))
                    ((null? p)
                      (apply append (reverse r)))
                    (else
                      (loop '() a (cons (fetch-variables p a)
                                        r))))))
          (else
            (if (and (symbol? pattern) (not (eq? '_ pattern)))
                `(,(cons pattern (make-accessor accessor)))
                `()))))

  (define (make-case pattern body)
    (let* ((matcher (make-matcher pattern '()))
           (env     (fetch-variables pattern '()))
           (vars    (map car env))
           (args    (map (lambda (x) `(,(cdr x) ,*args*))
                         env)))
      `((and ,@matcher)
               ((lambda ,vars ,@body) ,@args))))

  (define (gen-cad*r path)
    (let ((name (string->symbol
                  (apply string-append (map symbol->string
                                            (append '(c) path '(r))))))
          (body (let loop ((p path))
                  (if (null? p)
                      'x
                      (let ((op (if (eq? 'a (car p))
                                'car
                                'cdr)))
                        `(,op ,(loop (cdr p))))))))
      `(,name (lambda (x) ,body))))

  (define (def-accessor accessor)
    (let* ((acc-chars (symbol->string accessor))
           (k         (string-length acc-chars))
           (acc-path  (map (lambda (x)
                             (string->symbol (string x)))
                           (string->list (substring acc-chars 1 (- k 1))))))
      (if (> (length acc-path) 4)
          (list (gen-cad*r acc-path))
          '())))

  (set! *accessors* '())
  (set! *args* (gensym))
  (let* ((clauses   (map (lambda (x)
                           (make-case (car x) (cdr x)))
                         clauses))
         (acc-defs  (apply append (map def-accessor *accessors*))))
    `(lambda ,*args*
       (letrec (,@acc-defs)
         (cond ,@clauses
               (else (wrong "unmatched pattern" (cons ',name ,*args*))))))))

(define-macro (let-matcher name clauses . body)
  `(letrec ((,name ,(apply match name clauses))) ,@body))

(define-macro (define-matcher name . clauses)
  `(define ,name ,(apply match name clauses)))

(define-macro (ml-match name . clauses)
  (letrec
    ((body
       (lambda (in)
         (let loop ((in in)
                    (out '()))
           (if (or (null? in)
                   (eq? ': (car in)))
               (list (reverse out) in)
               (loop (cdr in) (cons (car in) out)))))))
    (let loop ((in clauses)
               (out '()))
      (cond ((null? in)
              `(define ,name ,(apply match name (reverse out))))
            ((eq? ': (car in))
              (loop (cdr in) out))
            ((and (pair? in)
                  (pair? (car in))
                  (pair? (cdr in))
                  (eq? '= (cadr in))
                  (pair? (cddr in)))
              (let ((next (body (cddr in))))
                (loop (cadr next)
                      (cons `(,(car in) ,@(car next)) out))))
              (else (wrong "ml-match: syntax error" in))))))
