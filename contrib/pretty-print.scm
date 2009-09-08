; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (pretty-print object)     ==>  unspecific
; (pretty-print #t object)  ==>  unspecific
; (pretty-print #f object)  ==>  unspecific
; (pp object)               ==>  unspecific
; (pp #t object)            ==>  unspecific
; (pp #f object)            ==>  unspecific
; (pp-file string)          ==>  unspecific
;
; Pretty-print Scheme forms or files. When the first argument of PP
; is #T, format OBJECT as code. When the first argument is #F, format
; OBJECT as data. When there is only one argument, pass it to PROGRAM?
; to figure out wehther it is code or data.
;
; PRETTY-PRINT is just a more verbose name for PP.
;
; PP-FILE pretty-prints all objects in the file STRING.
;
; NOTE: This program handles only a subset of R4RS Scheme correctly
; and removes all comments from its input program. Caveat utilitor.
;
; (Example): (pp '(let ((a 1) (b 2)) (cons a b)))  ==>  unspecific
;
;            Output:  (let ((a 1)
;                           (b 2))
;                       (cons a b))

(load-from-library "programp.scm")
(load-from-library "write-to-string.scm")

; If your Scheme does not support FLUID-LET or DEFINE-MACRO,
; there is an alternative implementation using SYNTAX-RULES in
; lib/fluid-let-sr.scm.

(load-from-library "fluid-let.scm")

(define Margin 72)
(define Offset 0)
(define Column 0)

(define *Really-print* #t)
(define *Max-Column* 0)

(define LP "(")
(define RP ")")
(define SP " ")

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))
       (not (vector? x))))

(define (object-length x)
  (string-length (write-to-string x)))

(define (exceeds-margin? x . opt-lead)
  (let ((lead (if (null? opt-lead) 0 (car opt-lead))))
  (>= (+ Column lead (string-length (write-to-string x)))
      Margin)))

(define (spaces n)
  (and *Really-print*
       (or (zero? n)
           (begin (write-char #\space)
                  (spaces (- n 1))))))

(define (linefeed)
  (if *Really-print*
      (write-char #\newline))
  (spaces Offset)
  (set! Column Offset))

(define (pr s)
  (if *Really-print*
      (display s))
  (set! Column (+ Column (string-length s)))
  (if (> Column *Max-Column*)
      (set! *Max-Column* Column)))

(define (pp-simple-form x)
  (let* ((s (write-to-string x))
         (k (string-length s)))
    (if (and (> (+ Column k) Margin)
             (> Column Offset))
        (linefeed))
    (pr s)))

(define (pp-datum x)
  (cond ((or (null? x)
             (symbol? x)
             (boolean? x)
             (char? x)
             (number? x)
             (string? x))
          (pp-simple-form x))
        ((vector? x)
          (display "#")
          (fluid-let ((Offset (+ 1 Offset)))
            (pp-pair(vector->list x))))
        ((pair? x)
          (pp-pair x))
        ((procedure? x)
          (pp-simple-form (string->symbol "#<PROCEDURE>")))
        (else
          (error "pp-datum: unknown type"))))

(define (pp-pair x)
  (pr LP)
  (fluid-let ((Offset (+ 1 Offset)))
    (let pp-members ((x x)
                     (s #f))
      (cond ((pair? x)
              (if s
                  (if (or (pair? (car x))
                          (vector? (car x)))
                      (linefeed)
                      (pr SP)))
              (pp-datum (car x))
              (pp-members (cdr x) #t))
            ((null? x)
              #f)
            (else
              (pr " . ")
              (pp-datum x)))))
    (pr RP))

(define (pp-quote x q)
  (pr q)
  (fluid-let ((Offset (+ Offset (string-length q))))
    (pp-datum (cadr x))))

(define (pp-body x)
  (cond ((not (null? x))
          (pp-form (car x))
          (if (not (null? (cdr x)))
              (linefeed))
          (pp-body (cdr x)))))

(define (pp-lambda x)
  (cond ((or (> (length x) 3)
             (exceeds-margin? x))
          (pr LP)
          (pr "lambda ")
          (fluid-let ((Offset (+ 2 Offset)))
            (pp-datum (cadr x))
            (linefeed)
            (pp-body (cddr x))
            (pr RP)))
        (else (pp-simple-form x))))

(define (fits-in-margin? formatter x)
  (fluid-let ((Column         Column)
              (Offset         Offset)
              (*Max-Column*   0)
              (*Really-print* #f))
    (formatter x)
    (< *Max-Column* Margin)))

(define (pp-inline-app x)
  (pr LP)
  (pp-simple-form (car x))
  (pr SP)
  (fluid-let ((Offset (+ 2 (object-length (car x)) Offset)))
    (pp-body (cdr x)))
  (pr RP))

(define (pp-indented-app x)
  (pr LP)
  (fluid-let ((Offset (+ 1 Offset)))
    (pp-form (car x)))
  (let ((indent (if (pair? (car x)) 1 2)))
    (fluid-let ((Offset (+ indent Offset)))
      (linefeed)
      (pp-body (cdr x)))
    (pr RP)))

(define (indented-style-preferred? x)
  (and (memq x '(call-with-current-continuation
                 call/cc
                 call-with-input-file
                 call-with-output-file
                 with-input-from-file
                 with-output-to-file))
       #t))

(define (pp-application x)
  (cond ((fits-in-margin? pp-simple-form x)
          (pp-simple-form x))
        ((indented-style-preferred? (car x))
          (pp-indented-app x))
        ((fits-in-margin? pp-inline-app x)
          (pp-inline-app x))
        (else
          (pp-indented-app x))))

(define (pp-cond/case what x)
  (letrec
    ((print-clauses
       (lambda (c*)
         (cond ((not (null? c*))
                 (pr LP)
                 (fluid-let ((Offset (+ 1 Offset)))
                   (if (eq? what 'cond)
                       (pp-form (caar c*))
                       (pp-datum (caar c*))))
                 (fluid-let ((Offset (+ 2 Offset)))
                   (linefeed)
                   (if (and (eq? 'cond what)
                            (pair? (cdar c*))
                            (eq? '=> (cadar c*)))
                       (fluid-let ((Offset (+ 3 Offset)))
                         (pr "=> ")
                         (pp-body (cddar c*)))
                       (pp-body (cdar c*))))
                 (pr RP)
                 (if (not (null? (cdr c*)))
                     (linefeed))
                 (print-clauses (cdr c*)))))))
    (pr LP)
    (pr (symbol->string what))
    (pr SP)
    (fluid-let ((Offset (+ (if (eq? what 'cond) 6 2)
                           Offset)))
      (if (eq? what 'case)
          (begin (pp-simple-form (cadr x))
                 (linefeed)))
      (let ((c* (if (eq? what 'cond)
                    (cdr x)
                    (cddr x))))
        (print-clauses c*)
        (pr RP)))))

(define (pp-do x)
  (letrec
    ((print-inits
       (lambda (x first)
         (cond ((null? x)
                 #f)
               (first
                 (pp-simple-form (car x))
                 (print-inits (cdr x) #f))
               (else
                 (linefeed)
                 (pp-simple-form (car x))
                 (print-inits (cdr x) #f)))))
       (init-part cadr)
       (test-part caddr)
       (do-body   cdddr))
    (pr LP)
    (pr "do ")
    (pr LP)
    (fluid-let ((Offset (+ 5 Offset)))
      (print-inits (init-part x) #t))
    (fluid-let ((Offset (+ 4 Offset)))
      (linefeed)
      (pr LP)
      (pp-form (car (test-part x)))
      (if (not (null? (cdr (test-part x))))
          (fluid-let ((Offset (+ 2 Offset)))
            (linefeed)
            (pp-body (cdr (test-part x)))))
      (pr RP)
      (linefeed)
      (pp-body (do-body x)))
    (pr RP)))

(define (pp-just-indent what x)
  (pr LP)
  (pr what)
  (pr SP)
  (fluid-let ((Offset (+ 2 (string-length what) Offset)))
    (let print ((x (cdr x)))
      (cond ((not (null? x))
              (pp-form (car x))
              (if (not (null? (cdr x)))
                  (linefeed))
              (print (cdr x)))))
    (pr RP)))

(define (pp-let-bindings b* rec)
  (pr LP)
  (fluid-let ((Offset (+ 1 Offset)))
    (let pp-bindings ((b* b*))
      (cond ((not (null? b*))
              (pr LP)
              (pp-simple-form (caar b*))
              (cond ((and rec (pair? (cadar b*)))
                      (fluid-let ((Offset (+ 2 Offset)))
                        (linefeed)
                        (pp-form (cadar b*))))
                    (else
                      (pr SP)
                      (fluid-let ((Offset (+ 2 (object-length (caar b*))
                                          Offset)))
                        (pp-form (cadar b*)))))
              (pr RP)
              (if (not (null? (cdr b*)))
                  (linefeed))
              (pp-bindings (cdr b*))))))
  (pr RP))

(define (pp-let x)
  (pr LP)
  (pr "let ")
  (let* ((named?   (symbol? (cadr x)))
         (bind     (if named? (caddr x) (cadr x)))
         (body     (if named? (cdddr x) (cddr x)))
         (name-len (if named?
                       (+ 1 (object-length (cadr x)))
                       0)))
    (fluid-let ((Offset (+ 5 name-len Offset)))
      (if named?
          (begin (pp-simple-form (cadr x))
                 (pr SP)))
      (pp-let-bindings bind #f))
    (fluid-let ((Offset (+ 2 Offset)))
      (linefeed)
      (pp-body body))
    (pr RP)))

(define (pp-let* x)
  (pr LP)
  (pr "let* ")
  (fluid-let ((Offset (+ 6 Offset)))
    (pp-let-bindings (cadr x) #f))
  (fluid-let ((Offset (+ 2 Offset)))
    (linefeed)
    (pp-body (cddr x)))
  (pr RP))

(define (pp-letrec x)
  (pr LP)
  (pr "letrec ")
  (fluid-let ((Offset (+ 2 Offset)))
    (linefeed)
    (pp-let-bindings (cadr x) #t))
  (fluid-let ((Offset (+ 2 Offset)))
    (linefeed)
    (pp-body (cddr x)))
  (pr RP))

(define (pp-fluid-let x)
  (pr LP)
  (pr "fluid-let ")
  (fluid-let ((Offset (+ 11 Offset)))
    (pp-let-bindings (cadr x) #f))
  (fluid-let ((Offset (+ 2 Offset)))
    (linefeed)
    (pp-body (cddr x)))
  (pr RP))

(define (pp-define-etc what x)
  (pr LP)
  (pr what)
  (pr SP)
  (pp-simple-form (cadr x))
  (fluid-let ((Offset (+ 2 Offset)))
    (if (or (pair? (cadr x))
            (exceeds-margin? x))
        (linefeed)
        (pr SP))
    (pp-body (cddr x)))
  (pr RP))

(define (pp-syntax-rules x)
  (letrec
    ((pp-rules
       (lambda (r*)
         (cond ((not (null? r*))
                 (pr LP)
                 (pp-datum (caar r*))
                 (fluid-let ((Offset (+ 2 Offset)))
                   (linefeed)
                   (pp-form (cadar r*))
                   (pr RP))
                 (if (not (null? (cdr r*)))
                   (linefeed))
                 (pp-rules (cdr r*)))))))
    (pr LP)
    (pr "syntax-rules ")
    (fluid-let ((Offset (+ 14 Offset)))
      (pp-datum (cadr x)))
    (fluid-let ((Offset (+ 2 Offset)))
      (linefeed)
      (pp-rules (cddr x))
      (pr RP))))

(define (pp-form x)
  (if (not (pair? x))
      (pp-datum x)
      (case (car x)
        ((quote)            (pp-quote x "'"))
        ((quasiquote)       (pp-quote x "`"))
        ((unquote)          (pp-quote x ","))
        ((unquote-splicing) (pp-quote x ",@"))
        ((lambda)           (pp-lambda x))
        ((cond)             (pp-cond/case 'cond x))
        ((case)             (pp-cond/case 'case x))
        ((do)               (pp-do x))
        ((if)               (pp-just-indent "if" x))
        ((and)              (pp-just-indent "and" x))
        ((or)               (pp-just-indent "or" x))
        ((let)              (pp-let x))
        ((let*)             (pp-let* x))
        ((letrec)           (pp-letrec x))
        ((fluid-let)        (pp-fluid-let x))
        ((begin)            (pp-just-indent "begin" x))
        ((define)           (pp-define-etc "define" x))
        ((define-macro)     (pp-define-etc "define-macro" x))
        ((define-syntax)    (pp-define-etc "define-syntax" x))
        ((syntax-rules)     (pp-syntax-rules x))
        (else               (pp-application x)))))

(define (pretty-print a1 . a2)
  (set! Column 0)
  (set! Offset 0)
  (cond ((null? a2)
          (if (program? a1)
              (pp-form a1)
              (pp-datum a1)))
        (a1
          (pp-form (car a2)))
        (else
          (pp-datum (car a2))))
  (linefeed))

(define pp pretty-print)

(define (pp-file file)
  (with-input-from-file
    file
    (lambda ()
      (let pp* ((x (read)))
        (cond ((not (eof-object? x))
                (pp #t x)
                (let ((next (read)))
                  (if (not (eof-object? next))
                      (linefeed))
                  (pp* next))))))))
