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

(define (void) (if #f #f))

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
  (let ((append3
          (lambda (a b c)
            (append a (append b c))))
        (tmps (map-car (lambda (x) (gensym)) bindings))
        (vars (map-car car bindings))
        (args (map-car cadr bindings)))
    (let ((undefineds   (map-car (lambda (v) (list v #f))
                                 vars))
          (tmp-bindings (map list tmps args))
          (updates      (map (lambda (v t) (list 'set! v t))
                             vars
                             tmps)))
      (list 'let
            undefineds
            (append3 '(let)
                     (list tmp-bindings)
                     (append3 updates
                              (list expr)
                              exprs))))))

;; Library procedures

;; Booleans

(define (not x) (eq? #f x))

;; Type predicates

(define number? real?)

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
        (else
          (eq? a b))))

(define (equal? a b)
  (cond ((eq? a b)
          #t)
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
        (cdr-of cdr-of)
        (any-null? any-null?))
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
        (cdr-of cdr-of)
        (any-null? any-null?)
        (map-car map-car))
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
            (foldr (map-car reverse a*) b))))))

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
        (cdr-of cdr-of)
        (any-null? any-null?))
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
  (void))

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
        (/ (expt2 (exact->inexact x) y))
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
        (fold-left gcd2 0 (map abs a))))))

(define lcm
  (let ((fold-left fold-left))
    (lambda a
      (letrec
        ((lcm2
           (lambda (a b)
             (let ((cd (gcd a b)))
               (* cd (* (quotient a cd)
                        (quotient b cd)))))))
        (fold-left lcm2 1 (map abs a))))))

(define max
  (let ((fold-left fold-left))
    (lambda (a . b)
      (fold-left (lambda (a b)
                   (if (or (inexact? a) (inexact? b))
                       (exact->inexact (if (> a b) a b))
                       (if (> a b) a b)))
                 a
                 b))))

(define min
  (let ((fold-left fold-left))
    (lambda (a . b)
      (fold-left (lambda (a b)
                   (if (or (inexact? a) (inexact? b))
                       (exact->inexact (if (< a b) a b))
                       (if (< a b) a b)))
                 a
                 b))))

(define (modulo a b)
  (let ((rem (remainder a b)))
    (cond ((zero? rem) 0)
          ((eq? (negative? a) (negative? b)) rem)
          (else (+ b rem)))))

(define (negative? x) (< x 0))

(define (odd? x) (not (even? x)))

(define (positive? x) (> x 0))

(define (ceiling x) (- (floor (- x))))

(define (round x)
  (let ((x+ (+ 0.5 x)))
    (let ((rx (floor x+)))
      (if (and (odd? rx) (= x+ rx))
          (- rx 1)
          rx))))

(define (truncate x)
  ((if (< x 0) ceiling floor) x))

; used by EXP and SIN
(define (fact2 x r)
  (if (= x 0)
      r
      (fact2 (- x 1) (* x r))))

(define exp
  (let ((fact2 fact2))
    (lambda (x)
      (letrec
        ((e-series
           (lambda (x y r last)
             (if (= r last)
                 r
                 (e-series x
                           (+ 1 y)
                           (+ r (/ (expt x y)
                                   (fact2 y 1)))
                           r)))))
    (if (>= x 2.0)
        (let ((e^x/2 (+ 1 (/ x 2) (e-series (/ x 2) 2 0.0 1.0))))
          (* e^x/2 e^x/2))
        (+ 1 x (e-series x 2 0.0 1.0)))))))

(define (log x)
  (letrec
    ((l-series
       (lambda (x y r last lim)
         (cond ((and lim (zero? lim))
                 r)
               ((= r last)
                 (* 2 r))
               (else
                 (l-series x
                           (+ 2 y)
                           (+ r (/ (expt (/ (- x 1)
                                            (+ x 1))
                                         y)
                                   y))
                           r
                           (if lim (- lim 1) lim)))))))
    (if (< 0.1 x 5)
        (l-series x 1 0.0 1.0 #f)
        (let ((approx (l-series x 1 0.0 1.0 5)))
          (let ((a (/ x (exp approx))))
            (+ approx (log a)))))))

; auxilary definitions for SIN, COS, TAN
(define pi 3.141592653589793238462643383279502884197169399375105820974944)
(define pi/4  (/ pi 4))
(define pi/2  (/ pi 2))
(define 3pi/4 (+ pi/2 pi/4))
(define 3pi/2 (+ pi pi/2))
(define 5pi/4 (+ pi pi/4))
(define 7pi/4 (+ pi 3pi/4))
(define 2pi   (+ pi pi))

(define ->circle
  (let ((2pi 2pi))
    (lambda (x)
      (let ((x+ (abs x)))
        (let ((d (* 2pi (floor (/ x+ 2pi)))))
          (let ((x+ (- x+ d)))
            (if (negative? x)
                (- 2pi x+)
                x+)))))))

(define p-series 
  (let ((fact2 fact2))
    (lambda (x y r add last)
      (if (= r last)
          r
          (p-series x
                    (+ 2 y)
                    ((if add + -) r (/ (expt x y)
                                       (fact2 y 1)))
                    (not add)
                    r)))))

(define cos
  (let ((->circle ->circle)
        (p-series p-series)
        (pi       pi)
        (pi/2     pi/2)
        (3pi/2    3pi/2)
        (2pi      2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x)       (if (inexact? x)  #i1.0  1.0))
              ((= pi/2  x)       (if (inexact? x)  #i0.0  0.0))
              ((= pi    x)       (if (inexact? x) #i-1.0 -1.0))
              ((= 3pi/2 x)       (if (inexact? x)  #i0.0  0.0))
              ((<= 0    x pi/2)  (p-series    x         2 1.0 #f 0))
              ((<= pi/2 x pi)    (- (p-series (- pi x)  2 1.0 #f 0)))
              ((<= pi   x 3pi/2) (- (p-series (- x pi)  2 1.0 #f 0)))
              (else              (p-series    (- 2pi x) 2 1.0 #f 0)))))))

(define sin
  (let ((->circle ->circle)
        (p-series p-series)
        (pi       pi)
        (pi/2     pi/2)
        (3pi/2    3pi/2)
        (2pi      2pi))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((= 0     x) (if (inexact? x)  #i0.0  0.0))
              ((= pi/2  x) (if (inexact? x)  #i1.0  1.0))
              ((= pi    x) (if (inexact? x)  #i0.0  0.0))
              ((= 3pi/2 x) (if (inexact? x) #i-1.0 -1.0))
              (else (let ((z (cond ((<= 0    x  pi/2) x)
                                   ((<= pi/2 x  pi)   (- pi x))
                                   ((<= pi   x 3pi/2) (- x pi))
                                   (else              (- 2pi x)))))
                      (if (> x pi)
                          (- (p-series z 3 z #f 0))
                          (p-series z 3 z #f 0)))))))))

(define tan
  (let ((->circle ->circle)
        (pi       pi)
        (pi/4     pi/4)
        (3pi/4    3pi/4)
        (5pi/4    5pi/4)
        (7pi/4    7pi/4))
    (lambda (x)
      (let ((x (->circle x)))
        (cond ((or (= x 0)     (= x  pi))   (if (inexact? x)  #i0.0  0.0))
              ((or (= x  pi/4) (= x 5pi/4)) (if (inexact? x)  #i1.0  1.0))
              ((or (= x 3pi/4) (= x 7pi/4)) (if (inexact? x) #i-1.0 -1.0))
              (else                         (/ (sin x) (cos x))))))))

(define atan
  (let ((pi/2 pi/2))
    (letrec
      ((at-series
         (lambda (x y r last)
           (if (= r last)
               r
               (at-series x
                          (+ 1 y)
                          (+ r (* (/ (* (expt 2 (+ y y))
                                        (expt (fact2 y 1) 2))
                                     (fact2 (+ y y 1) 1))
                                  (/ (expt x (+ y y 1))
                                     (expt (+ 1 (* x x))
                                           (+ 1 y)))))
                          r)))))
      (lambda (x)
        (cond ((negative? x)
                (- (at-series (- x) 0 0 1)))
              ((> x 1)
                (- pi/2 (atan (/ x))))
              (else (at-series x 0 0 1)))))))

(define (asin x)
  (cond ((= 1 x)
          (* 2 (atan x)))
        ((negative? x)
          (- (asin (- x))))
        (else
          (atan (/ x (sqrt (- 1 (* x x))))))))

(define acos
  (let ((pi   pi)
        (pi/2 pi/2))
    (lambda (x)
      (cond ((= -1 x) pi)
            ((=  1 x) 0)
            (else (- pi/2 (asin x)))))))

(define (sqrt square)
  (letrec
    ((sqrt2 (lambda (x last)
       (if (= last x)
           x
           (sqrt2 (/ (+ x (/ square x)) 2)
                  x)))))
    (if (negative? square)
        (wrong "sqrt: negative argument" square)
        (sqrt2 square 0))))

(define (zero? x) (= 0 x))

;; String procedures

(define (string . x) (list->string x))

; Used by NUMBER->STRING and STRING->NUMBER
(define (number-of-digits n r)
    (if (zero? n)
        (if (zero? r) 1 r)
        (number-of-digits (quotient n 10) (+ 1 r))))

(define number->string
  (let ((number-of-digits number-of-digits))
    (lambda (n . radix)
      (letrec
        ((digits
           (list->vector
             (string->list "0123456789abcdefghijklmnopqrstuvwxyz")))
         (conv
           (lambda (n rdx res)
             (if (zero? n)
                 (if (null? res) '(#\0) res)
                 (conv (quotient n rdx)
                       rdx
                       (cons (vector-ref digits (remainder n rdx))
                             res)))))
         (conv-int
           (lambda (n rdx)
             (if (negative? n)
                 (list->string (cons #\- (conv (abs n) rdx '())))
                 (list->string (conv n rdx '())))))
         (conv-sci-real
           (lambda (m e)
             (let ((ms (conv-int m 10))
                   (es (conv-int e 10))
                   (i  (if (negative? m) 2 1)))
               (let ((k (string-length ms)))
                 (string-append (substring ms 0 i)
                                "."
                                (if (= k i) "0" (substring ms i k))
                                "e"
                                (if (<= 0 e) "+" "")
                                es)))))
         (zeroes
           (lambda (n)
             (letrec
               ((loop
                  (lambda (n z)
                    (if (positive? n)
                        (loop (- n 1) (cons #\0 z))
                        (list->string z)))))
               (loop n '()))))
         (conv-expanded-real
           (lambda (n offset)
             (let ((m (abs n)))
               (string-append (if (negative? n) "-" "")
                              (cond ((negative? offset) "0.")
                                    ((zero? offset)     "0")
                                    (else               ""))
                              (zeroes (- offset))
                              (let ((ms (conv-int m 10)))
                                (let ((k (string-length ms)))
                                  (if (<= 0 offset k)
                                    (string-append (substring ms 0 offset)
                                                   "."
                                                   (substring ms offset k))
                                    ms)))))))
         (conv-real
           (lambda (n)
             (let ((m (mantissa n))
                   (e (exponent n)))
               (let ((d (number-of-digits m 0)))
                 (if (< -4 (+ e d) 10)
                     (conv-expanded-real m (+ e d))
                     (conv-sci-real m (+ e d -1)))))))
         (get-radix
           (lambda ()
             (cond ((null? radix) 10)
                   ((<= 2 (car radix) 36) (car radix))
                   (else (wrong "number->string: invalid radix"
                                (car radix)))))))
        (let ((r (get-radix)))
          (cond ((not (or (integer? n) (= 10 r)))
                  (wrong "number->string: real number needs a radix of 10" n))
                ((integer? n)
                  (conv-int (inexact->exact n) r))
                (else
                  (conv-real n))))))))

(define string->number
  (let ((number-of-digits number-of-digits)
        (inexact #f))
    (lambda (str . radix)
      (letrec
        ((digits
           (string->list "0123456789abcdefghijklmnopqrstuvwxyz"))
         (value-of-digit
           (lambda (x)
             (letrec
               ((v (lambda (x d n)
                     (cond ((null? d) 36)
                           ((char=? (car d) x) n)
                           (else (v x (cdr d) (+ n 1)))))))
               (v (char-downcase x) digits 0))))
         (find-exponent-mark
           (lambda (c)
             (memv c '(#\d #\D #\e #\E #\f #\F #\l #\L #\s #\S))))
         (result  cons)
         (value   car)
         (rest    cdr)
         (FAIL    '(#f . #f))
         (failed? (lambda (res)
                    (eq? #f (cdr res))))
         (ok?     (lambda (res)
                    (not (eq? #f (cdr res)))))
         (conv3
           (lambda (lst val rdx)
             (if (null? lst)
                 (result val '())
                 (let ((dval (value-of-digit (car lst))))
                   (if (< dval rdx)
                       (conv3 (cdr lst)
                              (+ (value-of-digit (car lst))
                                 (* val rdx))
                              rdx)
                       (result val lst))))))
         (conv
           (lambda (lst rdx)
             (if (null? lst)
                 FAIL
                 (conv3 lst 0 rdx))))
         (conv-int
           (lambda (lst rdx)
             (cond ((null? lst)
                     FAIL)
                   ((char=? (car lst) #\+)
                     (conv (cdr lst) rdx))
                   ((char=? (car lst) #\-)
                     (let ((r (conv (cdr lst) rdx)))
                       (if (ok? r)
                           (result (- (value r)) (rest r))
                           FAIL)))
                   (else (conv lst rdx)))))
         (make-frag
           (lambda (x)
             (let ((d (number-of-digits x -1)))
               (- (/ x (expt 10.0 d)) 1.0))))
         (make-real
           (lambda (int frag expn)
             (let ((v (* (+ 0.0 (abs int) (make-frag frag))
                         (expt 10.0 expn))))
               (if (negative? int) (- v) v))))
         (conv-exponent
           (lambda (int frag lst)
             (if (null? lst)
                 FAIL
                 (let ((exp-part (conv-int lst 10)))
                   (if (failed? exp-part)
                       FAIL
                       (result (make-real int frag (value exp-part))
                               (rest exp-part)))))))
         (conv-decimals
           (lambda (int lst)
             (cond ((null? lst)
                     (result (+ 0.0 int) '()))
                   ((find-exponent-mark (car lst))
                     (conv-exponent int 10 (cdr lst)))
                   (else
                     (let ((frag-part (conv3 lst 1 10)))
                       (if (null? (rest frag-part))
                           (result (make-real int (value frag-part) 0)
                                   '())
                           (conv-exponent int
                                          (value frag-part)
                                          (cdr (rest frag-part)))))))))
         (radix-ten?
           (lambda (rdx)
             (if (not (= 10 rdx))
                 (if (null? radix)
                     #f
                     (wrong "string->number: real number needs a radix of 10"))
                 #t)))
         (mantissa-digits?
           (lambda (x)
             (cond ((null? x)                    #f)
                   ((char-numeric? (car x))      #t)
                   ((find-exponent-mark (car x)) #f)
                   (else (mantissa-digits? (cdr x))))))
         (conv-real
           (lambda (lst rdx)
             (let ((int-part (conv-int lst rdx)))
               (cond ((failed? int-part)
                       FAIL)
                     ((and (zero? (value int-part))
                           (not (mantissa-digits? lst)))
                       FAIL)
                     ((null? (rest int-part))
                       int-part)
                     ((find-exponent-mark (car (rest int-part)))
                       (if (radix-ten? rdx)
                           (conv-exponent (value int-part)
                                          10
                                          (cdr (rest int-part)))
                           FAIL))
                     ((char=? #\. (car (rest int-part)))
                       (if (radix-ten? rdx)
                           (conv-decimals (value int-part)
                                          (cdr (rest int-part)))
                           FAIL))
                     (else
                       int-part)))))
         (replace-inexact-digits!
           (lambda (a)
             (cond ((null? a) #f)
                   ((char=? #\# (car a))
                     (set-car! a #\5)
                     (set! inexact #t)
                     (replace-inexact-digits! (cdr a)))
                   (else
                     (replace-inexact-digits! (cdr a))))))
         (get-radix
           (lambda ()
             (cond ((null? radix) 10)
                   ((<= 2 (car radix) 36) (car radix))
                   (else (wrong "string->number: invalid radix"
                                (car radix)))))))
        (set! inexact #f)
        (let ((radix   (get-radix))
              (lst     (string->list str)))
          (if (and (> (string-length str) 1)
                   (char=? #\# (car lst)))
              (let ((mod (cadr lst)))
                (set! lst (cddr lst))
                (cond ((char=? mod #\e))
                      ((char=? mod #\d))
                      ((char=? mod #\i) (set! inexact #t))
                      ((char=? mod #\b) (set! radix 2))
                      ((char=? mod #\o) (set! radix 8))
                      ((char=? mod #\x) (set! radix 16))
                      (else             (set! lst '())))))
          (if (or (null? lst)
                  (memv (car lst) '(#\+ #\- #\.))
                  (char-numeric? (car lst)))
              (replace-inexact-digits! lst))
          (let ((r (cond ((null? lst)
                           FAIL)
                         ((char=? #\- (car lst))
                           (conv-real (cdr lst) radix))
                         (else
                           (conv-real lst radix)))))
            (if (null? (rest r))
                (let ((v (if (char=? #\- (car lst))
                             (- (value r))
                             (value r))))
                  (if inexact
                      (exact->inexact v)
                      v))
                #f)))))))

;; Vector procedures

(define (vector . x) (list->vector x))

;; Input/output procedures

(define (newline . port)
  (apply write-char #\newline port))

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
    ((improper-or-splicing?
       (lambda (form)
         (cond ((null? form)
                 #f)
               ((not (pair? form))
                 #t)
               ((and (pair? (cdr form))
                     (null? (cddr form))
                     (eq? 'unquote (car form)))
                 #t)
               (else
                 (or (and (pair? (car form))
                          (eq? 'unquote-splicing (caar form))
                          (pair? (cdar form)))
                     (improper-or-splicing? (cdr form)))))))
     (map-unquote
       (lambda (x)
         (cond ((null? x)
                 '())
               ((not (pair? x))
                 (list (list 'quote x)))
               ((and (pair? (cdr x))
                     (null? (cddr x))
                     (eq? 'unquote (car x)))
                 (list (cadr x)))
               ((and (pair? (car x))
                     (eq? 'unquote-splicing (caar x))
                     (pair? (cdar x)))
                 (cons (cadar x)
                       (map-unquote (cdr x))))
               (else
                 (cons (list 'list (expand-qq (car x)))
                       (map-unquote (cdr x)))))))
     (qq-list
       (lambda (form)
         (if (improper-or-splicing? form)
             (cons 'append (map-unquote form))
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
                  (wrong "nested quasiquote is not supported" form))
               ((and (eq? 'unquote (car form))
                     (pair? (cdr form)))
                 (cadr form))
               (else
                 (qq-list form))))))
    (if (pair? (cdr form))
        (expand-qq (cadr form))
        form)))

;;----- Library -----

; LET/LET*/LETREC helper
(define (check-bindings b who)
  (cond ((null? b)
          #t)
        ((or (not (pair? b))
             (not (pair? (car b)))
             (not (symbol? (caar b)))
             (not (pair? (cdar b)))
             (not (null? (cddar b))))
          (wrong (string-append who ": invalid syntax") b))
        (else
          (check-bindings (cdr b) who))))

; Now that the QQ expander is here, define a
; clean version of LET (including named LET).
; Can't name it LET yet, because it uses LET.

(define-macro %ext-let
  (let ((check-bindings check-bindings))
    (lambda (a1 a2 . a3)
      (letrec
        ((split
           (lambda (bind* vars args)
             (if (null? bind*)
                 (cons vars args)
                 (split (cdr bind*)
                        (cons (caar bind*) vars)
                        (cons (cadar bind*) args))))))
        (if (symbol? a1)
            (if (null? a3)
                (wrong "named let: missing body"
                       `(let ,a1 ,a2 ,@a3))
                (begin (check-bindings a2 "let")
                       (let ((va (split a2 '() '())))
                         (let ((v (reverse (car va)))
                               (a (reverse (cdr va))))
                           `((letrec ((,a1 (lambda ,v ,@a3)))
                               ,a1) ,@a)))))
            (begin (check-bindings a1 "let")
                   (let ((va (split a1 '() '())))
                     (let ((v (car va))
                           (a (cdr va)))
                       `((lambda ,v ,a2 ,@a3) ,@a)))))))))

(define-macro let %ext-let)

; Also define a clean version of LETREC.

(define-macro %clean-letrec
  (let ((check-bindings check-bindings))
    (lambda (bindings expr . exprs)
      (check-bindings bindings "letrec")
      (let ((tmps (map (lambda (x) (gensym)) bindings))
            (vars (map car bindings))
            (args (map cadr bindings)))
        (let ((undefineds   (map (lambda (v) (list v #f))
                                 vars))
              (tmp-bindings (map (lambda (t a) (list t a))
                                 tmps
                                 args))
              (updates      (map (lambda (v t) (list 'set! v t))
                                 vars
                                 tmps)))
          `(let ,undefineds
             (let ,tmp-bindings
               ,@updates
               ,expr
               ,@exprs)))))))

(define-macro letrec %clean-letrec)

(define-macro let*
  (let ((check-bindings check-bindings))
    (lambda (bindings expr . exprs)
      (letrec
        ((nest-let
           (lambda (b)
             (cond ((null? b)
                     (cons expr exprs))
                   ((null? (cdr b))
                     `(let ((,(caar b) ,(cadar b)))
                        ,@(nest-let (cdr b))))
                   (else
                     `(let ((,(caar b) ,(cadar b)))
                        ,(nest-let (cdr b))))))))
        (check-bindings bindings "let*")
        (if (null? bindings)
            `(let () ,expr ,@exprs)
            (nest-let bindings))))))

(define-macro (case key . clauses)
  (letrec
    ((gen-clauses
       (lambda (k c*)
         (cond ((null? c*) '())
               ((or (not (pair? c*))
                    (not (pair? (car c*)))
                    (not (pair? (cdar c*))))
                 (wrong "case: invalid syntax" c*))
               ((null? (cdr c*))
                 (if (eq? 'else (caar c*))
                     `((else ,@(cdar c*)))
                     `(((memv ,k ',(caar c*)) ,@(cdar c*)))))
               (else
                 `(((memv ,k ',(caar c*)) ,@(cdar c*))
                     ,@(gen-clauses k (cdr c*))))))))
    (let ((k (gensym)))
      `(let ((,k ,key))
         (cond ,@(gen-clauses k clauses))))))

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
                 (wrong "do: invalid syntax" clauses))
               (else
                 (split (cdr clauses)
                        (cons (caar clauses) vars)
                        (cons (cadar clauses) inits)
                        (if (null? (cddar clauses))
                            (cons (caar clauses) steps)
                            (cons (caddar clauses) steps))))))))
    (if (or (not (pair? test))
            (not (list? (cdr test))))
        (wrong "do: invalid syntax" test)
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
           (car value)
           (let ((x ,expr))
             (if value
                 (car value)
                 (begin (set! value (cons x '()))
                        (car value))))))))

(define (force x) (x))

;;----- Syntax-rules-transformer -----

; This expander rewrites DEFINE-SYNTAX to DEFINE-MACRO, e.g.:
;
; (define-syntax iff
;   (syntax-rules (then else)
;     ((_ p then c)        (or p c))
;     ((_ p then c else a) (if p c a))))
; ==>
; (define-macro (iff . g283)
;   ((lambda (syntax-expand syntax-match g283)
;      (cond ((syntax-match g283 '(_ p then c) '(then else) '())
;              => (lambda (env)
;                   (syntax-expand '(_ p then c) '(or p c) env)))
;            ((syntax-match g283 '(_ p then c else a) '(then else) '())
;              => (lambda (env)
;                   (syntax-expand '(_ p then c else a) '(if p c a) env)))
;            (else
;              (wrong "invalid syntax" g283))))
;    #<PROCEDURE syntax-expand>
;    #<PROCEDURE syntax-match>
;    (cons 'iff g283)))

; Match FORM against PATTERN.
; KEYWORDS contains the keywords of SYNTAX-RULES.
; When the given form matches the pattern, bind
; each variable of PATTERN to the corresponding
; part of the FORM, extend ENV by these bondings
; and return it.
; In case of a mismatch, return #F.
;
; NOTE: The ellipsis is an ordinary variable, but
; it binds to an environment rather than a form.
;
(define (syntax-match form pattern keywords env)
  (letrec
    ((match
       (lambda (form pattern keywords env)
         (cond
           ((pair? pattern)
             (cond
               ((and (pair? (cdr pattern))
                     (eq? '... (cadr pattern)))
                 (let ((e* (map (lambda (x)
                                  (match x (car pattern) keywords '()))
                                form)))
                   (cons (cons '... e*) env)))
               ((pair? form)
                 (let ((e (match (car form) (car pattern) keywords env)))
                   (and e (match (cdr form) (cdr pattern) keywords e))))
               (else #f)))
           ((memq pattern keywords)
             (if (eq? pattern form) env #f))
           ((symbol? pattern)
             (cons (cons pattern form) env))
           (else
             (if (equal? pattern form) env #f))))))
    (let ((e (match form pattern keywords env)))
      (if e (reverse e) e))))

; Give a unique name to each variable that is bound in FORM.
; BOUND is a list of initially bound variables. This function
; also renames variables of LET, LET*, and LETREC, e.g.:
;
; (ALPHA-CONV '(LET ((X Y)) X) '()) => (LET ((G0 Y)) G0)
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
               ((and (eq? 'quote (car form))
                     (pair? (cdr form))
                     (null? (cddr form)))
                 form)
               ((and (eq? 'lambda (car form))
                     (pair? (cdr form))
                     (pair? (cddr form)))
                 (let ((e (map-improper (lambda (x)
                                          (cons x (gensym)))
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
                     (pair? (caadr form))
                     (pair? (cddr form)))
                 (let ((e (map-improper (lambda (x)
                                          (cons x (gensym)))
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

; Substitute variables of FORM by values of ENV.
;
(define (syntax-expand bound tmpl env)
  (let ((alpha-conv alpha-conv))
    (letrec
      ((expand
         (lambda (tmpl env)
           (cond
             ((not (pair? tmpl))
               (cond ((assq tmpl env) => cdr)
                     (else tmpl)))
             ((and (pair? tmpl)
                   (pair? (cdr tmpl))
                   (eq? (cadr tmpl) '...))
               (let ((eenv (assq '... env)))
                 (if (not eenv)
                     (wrong
                       "syntax-rules: template without matching ... in pattern"
                       tmpl)
                     (begin (set-car! eenv '(#f))
                            (map (lambda (x)
                                   (expand (car tmpl) x))
                                 (cdr eenv))))))
             (else (cons (expand (car tmpl) env)
                         (expand (cdr tmpl) env)))))))
      (alpha-conv (expand tmpl env) bound))))

; Check the syntax of DEFINE-SYNTAX and rewrite it
; to an application of DEFINE-MACRO.
;
(define-macro define-syntax
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
               (cons `((syntax-match ,app
                                     ',(pattern rules-in)
                                     ',keywords
                                     '())
                        => (lambda (env)
                             (syntax-expand ',(flatten (pattern rules-in) '())
                                            ',(template rules-in)
                                            env)))
                     rules-out))))))
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
          (wrong "syntax-rules: malformed keyword list" (cadr rules)))
        ((not (rules-ok? (cddr rules)))
          (wrong "syntax-rules: invalid clause in rules" (cddr rules)))
        (else
          (let* ((app (gensym))
                 (default `((else (wrong "invalid syntax" ,app)))))
            `(define-macro ,(cons name app)
               (let ((,app (cons ',name ,app))
                     (syntax-match ,syntax-match)
                     (syntax-expand ,syntax-expand))
                 (cond ,@(append (rewrite-rules app
                                                (cadr rules)
                                                (cddr rules)
                                                '())
                                 default))))))))))

;;----- Utilities -----

(define (print . x*)
  (letrec
    ((p (lambda (x* first)
          (cond ((not (null? x*))
                  (if (not first) (write-char #\space))
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
  (let ((full-path (locate-file file))
        (do-load (lambda (file)
                   (begin (if (not *loading*)
                              (begin (display "; loading from ")
                                     (display file)
                                     (newline)))
                          (load file)))))
    (if full-path
        (do-load full-path)
        (let ((full-path (locate-file (string-append file ".scm"))))
          (if full-path
              (do-load full-path)
              (wrong "cannot locate file" file))))))
