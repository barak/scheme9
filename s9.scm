;;
;; Scheme 9 from Empty Space
;; By Nils M Holm <nmh@t3x.org>, 2007,2008,2009
;;

;;----- Library -----

;; Some obvious procedures first

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (list . x) x)

(define (null? x) (eq? '() x))

;; Auxiliary definitions, will be redefined later

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

; There is no LET or LETREC yet, so

(define-macro (let bindings expr . exprs)
  ((lambda (split)
     ((lambda (tmp-split)
        (set! split tmp-split)
        (apply (lambda (vars args)
                 (append
                   (list (append
                           (list 'lambda)
                           (append (list vars)
                                   (append (list expr)
                                           exprs))))
                   args))
               (split bindings '() '())))
      (lambda (bind* vars args)
        (if (null? bind*)
            (list vars args)
            (split (cdr bind*)
                   (cons (caar bind*) vars)
                   (cons (cadr (car bind*)) args))))))
   #f))

(define (map-car f a)
  (let ((mapcar1 #f))
    (let ((tmp-mapcar1
            (lambda (a)
              (if (null? a)
                  '()
                   (cons (f (car a))
                         (mapcar1 (cdr a)))))))
    (set! mapcar1 tmp-mapcar1)
    (mapcar1 a))))

(define (map f a b)
  (let ((map2 #f))
    (let ((tmp-map2
            (lambda (a b)
              (if (null? a)
                  '()
                   (cons (f (car a) (car b))
                         (map2 (cdr a) (cdr b)))))))
    (set! map2 tmp-map2)
    (map2 a b))))

(define-macro (letrec bindings expr . exprs)
  (let ((make-temps #f)
        (append3 (lambda (a b c)
                   (append a (append b c)))))
    (let ((tmp-make-temps
            (lambda (b)
              (if (null? b)
                  '()
                  (cons (gensym)
                        (make-temps (cdr b)))))))
      (set! make-temps tmp-make-temps)
      (let ((tmps (make-temps bindings))
            (vars (map-car car bindings))
            (args (map-car cadr bindings)))
        (let ((undefineds (map-car (lambda (v) (list v #f))
                                   vars))
              (tmp-bindings (map list tmps args))
              (updates (map (lambda (v t) (list 'set! v t))
                            vars
                            tmps)))
          (list 'let
                undefineds
                (append3 '(let)
                         (list tmp-bindings)
                         (append3 updates
                                  (list expr)
                                  exprs))))))))

;; Library procedures

;; Booleans

(define (not x) (eq? #f x))

;; Type predicates

(define number? integer?)

(define (port? x)
  (or (input-port? x)
      (output-port? x)))

;; Equivalence predicates

(define (eqv? a b)
  (cond ((number? a)
          (and (number? b)
               (= a b)))
        ((char? a)
          (and (char? b)
               (char=? a b)))
        (else (eq? a b))))

(define (equal? a b)
  (cond ((eq? a b) #t)
        ((and (pair? a)
              (pair? b))
          (and (equal? (car a) (car b))
               (equal? (cdr a) (cdr b))))
        ((string? a)
          (and (string? b)
               (string=? a b)))
        ((vector? a)
           (and (vector? b)
                (equal? (vector->list a)
                        (vector->list b))))
        (else (eqv? a b))))

;; List procedures

(define (list? x)
  (letrec
    ((l? (lambda (x y)
           (cond ((eq? x y) #f)
                 ((null? x) #t)
                 ((pair? x)
                   (or (null? (cdr x))
                       (and (pair? (cdr x))
                            (l? (cddr x) (cdr y)))))
                 (else #f)))))
    (or (null? x)
        (and (pair? x)
             (l? (cdr x) x)))))

(define (assoc x a)
  (cond ((null? a) #f)
        ((equal? (caar a) x) (car a))
        (else (assoc x (cdr a)))))

(define (assq x a)
  (cond ((null? a) #f)
        ((eq? (caar a) x) (car a))
        (else (assq x (cdr a)))))

(define (assv x a)
  (cond ((null? a) #f)
        ((eqv? (caar a) x) (car a))
        (else (assv x (cdr a)))))

(define (member x a)
  (cond ((null? a) #f)
        ((equal? (car a) x) a)
        (else (member x (cdr a)))))

(define (memq x a)
  (cond ((null? a) #f)
        ((eq? (car a) x) a)
        (else (memq x (cdr a)))))

(define (memv x a)
  (cond ((null? a) #f)
        ((eqv? (car a) x) a)
        (else (memv x (cdr a)))))

(define (reverse a)
  (letrec
    ((reverse2
       (lambda (a b)
         (if (null? a)
             b
             (reverse2 (cdr a)
                       (cons (car a) b))))))
    (reverse2 a '())))

; Auxiliary functions for FOLD-LEFT, FOLD-RIGHT, MAP

(define (map-car f a)
  (letrec
    ((mapcar1
       (lambda (a r)
         (if (null? a)
             (reverse r)
             (mapcar1 (cdr a)
                      (cons (f (car a)) r))))))
    (mapcar1 a '())))

(define car-of
  (let ((map-car map-car))
    (lambda (a*)
      (map-car car a*))))

(define cdr-of
  (let ((map-car map-car))
    (lambda (a*)
      (map-car cdr a*))))

(define any-null?
  (let ((map-car map-car))
    (lambda (a*)
      (and (memq #t (map-car null? a*)) #t))))

(define fold-left
  (let ((car-of car-of)
        (cdr-of cdr-of
        (any-null? any-null?)))
    (lambda (f b . a*)
      (letrec
        ((fold
           (lambda (a* r)
             (if (any-null? a*)
                 r
                 (fold (cdr-of a*)
                       (apply f r (car-of a*)))))))
        (if (null? a*)
            (wrong "fold-left: too few arguments")
            (fold a* b))))))

(define fold-right
  (let ((car-of car-of)
        (cdr-of cdr-of
        (any-null? any-null?)))
    (lambda (f b . a*)
      (letrec
        ((foldr
           (lambda (a* r)
             (if (any-null? a*)
                 r
                 (foldr (cdr-of a*)
                        (apply f (append (car-of a*)
                                         (list r))))))))
        (if (null? a*)
            (wrong "fold-right: too few arguments")
            (foldr (map reverse a*) b))))))

(define append
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((append2
           (lambda (a b)
             (if (null? a)
                 b
                 (append2 (cdr a)
                          (cons (car a) b)))))
         (append-wrapper
           (lambda (a b)
             (if (null? b)
                 a
                 (append2 (reverse a) b)))))
        (fold-left append-wrapper '() a)))))

(define (length x)
  (letrec
    ((length2
       (lambda (x r)
         (if (null? x)
             r
             (length2 (cdr x)
                      (+ r 1))))))
    (length2 x 0)))

(define (list-tail x n)
  (cond ((zero? n) x)
        ((null? x) (wrong "list-tail: index out of range"))
        (else (list-tail (cdr x) (- n 1)))))

(define (list-ref x n)
  (car (list-tail x n)))

(define map
  (let ((car-of car-of)
        (cdr-of cdr-of
        (any-null? any-null?)))
    (lambda (f . a*)
      (letrec
        ((map2
           (lambda (a* r)
             (if (null? (car a*))
                 (reverse r)
                 (map2 (cdr-of a*)
                       (cons (apply f (car-of a*))
                             r))))))
        (if (null? a*)
            (wrong "map: too few arguments")
            (map2 a* '()))))))

(define (for-each f . a*)
  (if (null? a*)
      (wrong "for-each: too few arguments")
      (apply map f a*))
  (if #f #f))

;; Arithmetic procedures

(define (abs x) (if (< x 0) (- x) x))

(define (even? x) (zero? (remainder x 2)))

(define (expt x y)
  (letrec
    ((square
       (lambda (x) (* x x)))
     (expt2
       (lambda (x y)
         (cond ((zero? y) 1)
               ((even? y) (square (expt2 x (quotient y 2))))
               (else      (* x (square (expt2 x (quotient y 2)))))))))
    (if (negative? y)
        (wrong "expt: negative exponent" y)
        (expt2 x y))))

(define gcd
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((gcd2
           (lambda (a b)
             (cond ((zero? b) a)
                   ((zero? a) b)
                   ((< a b) (gcd2 a (remainder b a)))
                   (else (gcd2 b (remainder a b)))))))
        (fold-left gcd2 0 (map (lambda (x) (abs x))
                               a))))))

(define lcm
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((lcm2
           (lambda (a b)
             (let ((cd (gcd a b)))
               (* cd (* (quotient a cd)
                        (quotient b cd)))))))
        (fold-left lcm2 1 (map (lambda (x) (abs x))
                               a))))))

(define max
  (let ((fold-left fold-left))
    (lambda (a . b)
      (fold-left (lambda (a b)
                   (if (> a b) a b))
                 a b))))

(define min
  (let ((fold-left fold-left))
    (lambda (a . b)
      (fold-left (lambda (a b)
                   (if (< a b) a b))
                 a b))))

(define (modulo a b)
  (let ((rem (remainder a b)))
    (cond ((zero? rem) 0)
          ((eq? (negative? a) (negative? b)) rem)
          (else (+ b rem)))))

(define (negative? x) (< x 0))

(define (odd? x) (not (even? x)))

(define (positive? x) (> x 0))

(define (zero? x) (= 0 x))

;; String procedures

(define (number->string n . radix)
  (letrec
    ((digits
       (list->vector (string->list "0123456789abcdef")))
     (conv
       (lambda (n rdx res)
         (if (zero? n)
             res
             (conv (quotient n rdx)
                   rdx
                   (cons (vector-ref digits
                                     (remainder n rdx))
                         res)))))
     (get-radix
       (lambda ()
         (cond ((null? radix) 10)
               ((< 1 (car radix) 17) (car radix))
               (else (wrong "bad radix in number->string" radix))))))
    (let ((r (get-radix)))
      (cond ((zero? n)
              "0")
            ((negative? n)
              (list->string
                (cons #\- (conv (abs n) r '()))))
            (else (list->string (conv n r '())))))))

(define (string . x) (list->string x))

(define (string->number str . radix)
  (letrec
    ((digits
       (string->list "0123456789abcdef"))
     (value-of-digit
       (lambda (x)
         (letrec
           ((v (lambda (x d n)
             (cond ((null? d) 17)
                   ((char=? (car d) x) n)
                   (else (v x (cdr d) (+ n 1)))))))
           (v (char-downcase x) digits 0))))
     (conv3
       (lambda (lst res rdx)
         (if (null? lst)
             res
             (let ((dval (value-of-digit (car lst))))
               (and (< dval rdx)
                    (conv3 (cdr lst)
                           (+ (value-of-digit (car lst))
                              (* res rdx))
                           rdx))))))
     (conv
       (lambda (lst rdx)
         (if (null? lst)
             #f
             (conv3 lst 0 rdx))))
     (sconv
       (lambda (lst rdx)
         (cond ((null? lst) #f)
               ((char=? (car lst) #\+)
                 (conv (cdr lst) rdx))
               ((char=? (car lst) #\-)
                 (let ((r (conv (cdr lst) rdx)))
                   (if r (- r) #f)))
               (else (conv lst rdx)))))
     (get-radix
       (lambda ()
         (cond ((null? radix) 10)
               ((< 1 (car radix) 17) (car radix))
               (else (wrong "bad radix in string->number"
                            radix))))))
    (sconv (string->list str) (get-radix))))

;; Vector procedures

(define (vector . x) (list->vector x))

;; Input/output procedures

(define (newline . port)
  (apply display #\newline port))

(define (call-with-input-file file proc)
  (let ((f (open-input-file file)))
    (let ((r (proc f)))
      (close-input-port f)
      r)))

(define (call-with-output-file file proc)
  (let ((f (open-output-file file)))
    (let ((r (proc f)))
      (close-output-port f)
      r)))

(define with-input-from-file
  (let ((set-input-port! set-input-port!))
    (lambda (file thunk)
      (let ((outer-port (current-input-port))
            (new-port (open-input-file file)))
        (set-input-port! new-port)
        (let ((input (thunk)))
          (close-input-port new-port)
          (set-input-port! outer-port)
          input)))))

(define with-output-to-file
  (let ((set-output-port! set-output-port!))
    (lambda (file thunk)
      (let ((outer-port (current-output-port))
            (new-port (open-output-file file)))
        (set-output-port! new-port)
        (thunk)
        (close-output-port new-port)
        (set-output-port! outer-port)))))

;;----- Quasi-quote-expander -----

(define (expand-quasiquote form)
  (letrec
    ((does-splicing?
       (lambda (form)
         (if (not (pair? form))
             #f
             (or (and (pair? (car form))
                      (eq? 'unquote-splicing (caar form))
                      (pair? (cdar form)))
                 (does-splicing? (cdr form))))))
     (qq-list
       (lambda (form)
         (if (does-splicing? form)
             (cons 'append
                   (map (lambda (x)
                          (if (and (pair? x)
                                   (eq? 'unquote-splicing (car x))
                                   (pair? (cdr x)))
                              (cadr x)
                              (list 'list (expand-qq x))))
                        form))
             (cons 'list (map expand-qq form)))))
     (expand-qq
       (lambda (form)
         (cond ((vector? form)
                 (list 'list->vector
                       (qq-list (vector->list form))))
               ((not (pair? form))
                 (list 'quote form))
               ((and (eq? 'quasiquote (car form))
                     (pair? (cdr form)))
                 (expand-qq (cadr form)))
               ((and (eq? 'unquote (car form))
                     (pair? (cdr form)))
                 (cadr form))
               (else (qq-list form))))))
    (if (pair? (cdr form))
        (expand-qq (cadr form))
        form)))

;;----- Library -----

; Now that the QQ expander is here, define a
; clean version of LET (including named LET).
; Can't name it LET yet, because it uses LET.

(define-macro (%ext-let a1 a2 . a3)
  (letrec
    ((split
       (lambda (bind* vars args)
         (cond ((null? bind*)
                 (cons vars args))
               ((or (not (pair? bind*))
                    (not (pair? (car bind*)))
                    (not (symbol? (caar bind*)))
                    (not (pair? (cdar bind*)))
                    (not (null? (cddar bind*))))
                 (wrong "let: bad syntax" bind*))
               (else (split (cdr bind*)
                            (cons (caar bind*) vars)
                            (cons (cadar bind*) args)))))))
      (if (symbol? a1)
          (if (null? a3)
              (wrong "named let: missing body"
                     `(let ,a1 ,a2 ,@a3))
              (let ((va (split a2 '() '())))
                (let ((v (reverse (car va)))
                      (a (reverse (cdr va))))
                  `((letrec ((,a1 (lambda ,v ,@a3)))
                      ,a1) ,@a))))
          (let ((va (split a1 '() '())))
            (let ((v (car va))
                  (a (cdr va)))
              `((lambda ,v ,a2 ,@a3) ,@a))))))

(define-macro let %ext-let)

; Also define a clean version of LETREC.

(define-macro (%clean-letrec bindings expr . exprs)
  (letrec
    ((check
      (lambda (b)
        (cond ((null? b)
                #t)
              ((or (not (pair? b))
                   (not (pair? (car b)))
                   (not (symbol? (caar b)))
                   (not (pair? (cdar b)))
                   (not (null? (cddar b))))
                (wrong "letrec: bad syntax" b))
              (else (check (cdr b))))))
     (make-temps
       (lambda (b)
         (if (eq? '() b)
             '()
             (cons (gensym)
                   (make-temps (cdr b)))))))
    (check bindings)
    (let ((tmps (make-temps bindings))
          (vars (map car bindings))
          (args (map cadr bindings)))
      (let ((undefineds (map (lambda (v) (list v #f))
                             vars))
            (tmp-bindings (map (lambda (t a) (list t a))
                               tmps
                               args))
            (updates (map (lambda (v t) (list 'set! v t))
                          vars
                          tmps)))
        `(let ,undefineds
           (let ,tmp-bindings
             ,@updates
             ,expr ,@exprs))))))

(define-macro letrec %clean-letrec)

(define-macro (let* bindings expr . exprs)
  (letrec
    ((nest-let
       (lambda (b)
         (cond ((null? b)
                 (cons expr exprs))
               ((or (not (pair? b))
                    (not (pair? (car b)))
                    (not (symbol? (caar b)))
                    (not (pair? (cdar b)))
                    (not (null? (cddar b))))
                 (wrong "let*: bad syntax" b))
               ((null? (cdr b))
                 `(let ((,(caar b) ,(cadar b)))
                    ,@(nest-let (cdr b))))
               (else
                 `(let ((,(caar b) ,(cadar b)))
                    ,(nest-let (cdr b))))))))
    (if (null? bindings)
        `(let () ,expr ,@exprs)
        (nest-let bindings))))

(define-macro (case key . clauses)
  (letrec
    ((nest-if
       (lambda (k c*)
         (cond ((null? c*) '())
               ((or (not (pair? c*))
                    (not (pair? (car c*)))
                    (not (pair? (cdar c*))))
                 (wrong "case: bad syntax" c*))
               ((null? (cdr c*))
                 (if (eq? 'else (caar c*))
                     `((else ,@(cdar c*)))
                     `(((memv ,k ',(caar c*)) ,@(cdar c*)))))
               (else
                 `(((memv ,k ',(caar c*)) ,@(cdar c*))
                  ,@(nest-if k (cdr c*))))))))
    (let ((k (gensym)))
      `(let ((,k ,key))
         (cond ,@(nest-if k clauses))))))

(define-macro (do var-clauses test . body)
  (letrec
    ((split
       (lambda (clauses vars inits steps)
         (cond ((null? clauses)
                 (list vars inits steps))
               ((or (not (pair? clauses))
                    (not (pair? (car clauses)))
                    (not (symbol? (caar clauses)))
                    (not (pair? (cdar clauses))))
                 (wrong "do: bad syntax" clauses))
               (else
                 (split (cdr clauses)
                        (cons (caar clauses) vars)
                        (cons (cadar clauses) inits)
                        (if (null? (cddar clauses))
                            (cons (caar clauses) steps)
                            (cons (caddar clauses) steps))))))))
    (if (or (not (pair? test))
            (not (pair? (cdr test)))
            (not (null? (cddr test))))
        (wrong "do: bad syntax" test)
        (let ((loop (gensym))
              (var+init+step (split var-clauses '() '() '())))
          (let ((v (car   var+init+step))
                (i (cadr  var+init+step))
                (s (caddr var+init+step)))
            `(letrec
               ((,loop
                  (lambda ,v
                    (if ,(car test)
                        (begin ,@(cdr test))
                        (begin ,@body (,loop ,@s))))))
               (,loop ,@i)))))))

(define-macro (delay expr)
  `(let ((value #f))
     (lambda ()
       (if value
           (car value))
           (let ((x ,expr))
             (if value
                 (car value)
                 (begin (set! value (cons x '()))
                        (car value)))))))

(define (force x) (x))

;;----- Syntax-rules-transformer -----

; This expander rewrites DEFINE-SYNTAX to DEFINE-MACRO, e.g.:
;
; (define-syntax iff
;   (syntax-rules (then else)
;     ((_ p then c)        (or p c))
;     ((_ p then c else a) (if p c a))))
; =>
; (define-macro (iff . g52)
;   ((lambda (g52)
;      (cond
;        ((syntax-match g52 '(_ p then c) '(then else) '())
;          => (lambda (env)
;               (syntax-expand '(_ p then c) '(or p c) env)))
;        ((syntax-match g52 '(_ p then c else a) '(then else) '())
;          => (lambda (env)
;               (syntax-expand '(_ p then c else a) '(if p c a) env)))
;        (else (wrong "bad syntax for" 'iff))))
;    (cons 'iff g52)))

(define (make-ell-name name)
  (string->symbol
    (string-append "#..." (symbol->string name))))

; Match FORM against PATTERN.
; KEYWORDS contains the keywords of SYNTAX-RULES.
; When the given form matches the pattern, bind
; each variable of PATTERN to the corresponding
; part of the FORM, extend ENV by these bondings
; and return it.
; In case of a mismatch, return #F.
;
; NOTE: The ellipsis is an ordinary variable that
; binds to the CDR part of the currently matched
; part of FORM.
;
(define syntax-match
  (let ((make-ell-name make-ell-name))
    (lambda (form pattern keywords env)
      (cond
        ((pair? pattern)
          (cond
            ((and (symbol? (car pattern))
                  (pair? (cdr pattern))
                  (eq? '... (cadr pattern)))
              (cons (cons (make-ell-name (car pattern)) form) env))
            ((pair? form)
              (let ((e (syntax-match (car form) (car pattern) keywords env)))
                (and e (syntax-match (cdr form) (cdr pattern) keywords e))))
            (else #f)))
        ((memq pattern keywords)
          (if (eq? pattern form) env #f))
        ((symbol? pattern)
          (if (eq? '... pattern)
              (wrong "variable expected before ...")
              (cons (cons pattern form) env)))
        (else
          (if (equal? pattern form) env #f))))))

; Substitute variables of FORM by values of ENV.
; PATTERN is used to locate the variable to be
; substituted during ellipsis expansion. If
; PATTERN equals #F, no ellipsis expansion is
; performed.
;
(define syntax-expand
  (let ((make-ell-name make-ell-name))
    (lambda (pattern form env)
      (letrec
        ((ellipsis?
           (lambda (x)
             (and (pair? x)
                  (pair? (cdr x))
                  (eq? (cadr x) '...))))
         (subst-ellipsis
           (lambda (var tmpl ell-values env)
             (map (lambda (v)
                    (syntax-expand #f tmpl (cons (cons var v) env)))
                  ell-values)))
         (expand-ellipsis
           (lambda (form var ell-b env)
             (append (subst-ellipsis
                       var
                       (car form)
                       (cdr ell-b)
                       env)
                     (cddr form))))
         (car-or-false (lambda (x) (if (pair? x) (car x) #f)))
         (cdr-or-false (lambda (x) (if (pair? x) (cdr x) #f))))
        (cond
          ((not (pair? form))
            (cond ((assq form env) => cdr)
                  (else form)))
          ((ellipsis? form)
            (let* ((var (if pattern
                            (car pattern)
                            (car form)))
                   (ell (if var (make-ell-name var) var))
                   (ell-bindings (assq ell env)))
              (if ell-bindings
                  (if var
                      (expand-ellipsis form
                                       var
                                       ell-bindings
                                       env)
                      (append (list (syntax-expand #f (car form) env))
                              (cdr ell-bindings)
                              (cddr form)))
                  (wrong "unmatched ... in syntax-rules"))))
          ((pair? form)
            (cons (syntax-expand (car-or-false pattern)
                                 (car form)
                                 env)
                  (syntax-expand (cdr-or-false pattern)
                                 (cdr form)
                                 env)))
          (else (wrong "syntax-expand: this cannot happen")))))))

; Give a unique name to each variable that is bound in FORM.
; BOUND is a list of initially bound variables. This function
; also renames variables of LET, LET*, and LETREC, e.g.:
;
; (ALPHA-CONV '(LET ((X Y)) X) '()) => (LET ((##0 Y)) ##0)
;
; NOTE: Resulting unique names are not valid Scheme symbol
; names, but they are only used internally, so this does
; not matter.
;
(define (alpha-conv form bound)
  (letrec
    ((subst
       (lambda (x env)
         (cond ((assq x env) => cdr)
               (else x))))
     (map-improper
       (lambda (f a r)
         (cond ((null? a)
                 (reverse r))
               ((not (pair? a))
                 (append (reverse r) (f a)))
               (else (map-improper f (cdr a) (cons (f (car a)) r))))))
     (remove-bound
       (lambda (env bound)
         (cond ((null? env) '())
               ((memq (caar env) bound)
                 (remove-bound (cdr env) bound))
               (else (cons (car env)
                           (remove-bound (cdr env) bound))))))
     (conv
       (lambda (form env)
         (cond ((symbol? form)
                 (subst form env))
               ((not (pair? form))
                 form)
               ((and (eq? 'lambda (car form))
                     (pair? (cdr form))
                     (pair? (cddr form)))
                 (let ((e (map-improper (lambda (x)
                                          (cons x (gensym "##")))
                                        (cadr form)
                                        '())))
                   `(lambda ,@(conv (cdr form)
                                    (append (remove-bound e bound)
                                            env)))))
               ((and (or (eq? (car form) 'let)
                         (eq? (car form) 'letrec)
                         (eq? (car form) 'let*))
                     (pair? (cdr form))
                     (pair? (cadr form))
                     (pair? (cdadr form))
                     (pair? (cddr form)))
                 (let ((e (map-improper (lambda (x)
                                          (cons x (gensym "##")))
                                        (map (lambda (x)
                                               (if (pair? x) (car x) #f))
                                             (cadr form))
                                        '())))
                   `(,(car form) ,@(conv (cdr form)
                                         (append (remove-bound e bound)
                                                 env)))))
               (else (map-improper (lambda (x) (conv x env))
                                   form 
                                   '()))))))
    (conv form '())))

; Check the syntax of DEFINE-SYNTAX and rewrite it
; to an application of DEFINE-MACRO.
;
(define-macro define-syntax
  (let ((alpha-conv alpha-conv))
    (letrec
      ((flatten
         (lambda (x r)
           (cond ((null? x) r)
                 ((pair? x) (flatten (car x)
                                     (flatten (cdr x) r)))
                 (else (cons x r)))))
       (list-of?
         (lambda (p a)
           (or (null? a)
               (and (p (car a))
                    (list-of? p (cdr a))))))
       (keywords-ok?
         (lambda (x)
           (list-of? symbol? x)))
       (rules-ok?
         (lambda (x)
           (list-of? (lambda (x)
                       (and (pair? x)
                            (pair? (car x))
                            (pair? (cdr x))
                            (null? (cddr x))))
                     x)))
       (pattern caar)
       (template cadar)
       (rewrite-rules
         (lambda (app keywords rules-in rules-out)
           (if (null? rules-in)
               (reverse rules-out)
               (rewrite-rules
                 app
                 keywords
                 (cdr rules-in)
                 (let ((a (alpha-conv (template rules-in)
                                      (flatten (pattern rules-in)
                                               '()))))
                   (cons `((syntax-match ,app
                                         ',(pattern rules-in)
                                         ',keywords
                                         '())
                            => (lambda (env)
                                 (syntax-expand ',(pattern rules-in)
                                                ',a
                                                env)))
                         rules-out)))))))
      (lambda (name rules)
        (cond
          ((not (symbol? name))
            (wrong "define-syntax: expected symbol, got" name))
          ((or (not (pair? rules))
               (not (eq? 'syntax-rules (car rules))))
            (wrong "define-syntax: expected syntax-rules, got" rules))
          ((or (not (pair? (cdr rules)))
               (not (pair? (cddr rules))))
            (wrong "syntax-rules: too few arguments" rules))
          ((not (keywords-ok? (cadr rules)))
            (wrong "syntax-rules: bad keyword list" (cadr rules)))
          ((not (rules-ok? (cddr rules)))
            (wrong "syntax-rules: bad clause in rules" (cddr rules)))
          (else
            (let ((app (gensym))
                  (default `((else (wrong "bad syntax for" ',name)))))
              `(define-macro ,(cons name app)
                 (let ((,app (cons ',name ,app)))
                   (cond ,@(append (rewrite-rules app
                                                  (cadr rules)
                                                  (cddr rules)
                                                  '())
                                   default)))))))))))

;;----- Utilities -----

(define (print . x*)
  (letrec
    ((p (lambda (x* first)
          (cond ((not (null? x*))
                  (if (not first) (display #\space))
                  (write (car x*))
                  (p (cdr x*) #f))))))
    (p x* #t)
    (newline)))

(define (locate-file file)
  (letrec
    ((split
       (lambda (s)
         (let loop ((in  (string->list s))
                    (tmp '())
                    (out '()))
           (cond ((null? in)
                   (if (null? tmp)
                       out
                       (reverse (cons (list->string (reverse tmp))
                                out))))
                 ((char=? #\: (car in))
                   (loop (cdr in)
                         '()
                         (cons (list->string (reverse tmp))
                               out)))
                 (else
                   (loop (cdr in)
                         (cons (car in) tmp)
                         out)))))))
    (let loop ((path (split *library-path*)))
      (if (null? path)
          #f
          (let ((full-path (string-append (car path) "/" file)))
            (if (file-exists? full-path)
                full-path
                (loop (cdr path))))))))

(define (load-from-library file)
  (let ((full-path (locate-file file)))
       (if full-path
           (begin (if (not *loading*)
                      (begin (display "; loading from ")
                             (display full-path)
                             (newline)))
                  (load full-path))
           (wrong "cannot locate file" file))))
