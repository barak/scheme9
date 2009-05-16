; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (run* (variable) query)  ==>  list
; (run* () query)          ==>  list
;
; Run the given AMK (Another Micro Kanren) query and return its
; result, if any. See the book "Logic Programming in Scheme"
; (http://www.t3x.org/nmh/book-pdfs/) for an introduction to AMK.
; If a variable is given, return all values for that variable
; that satisfy the query.
;
; Example:   (run* (vq) (appendo vq (_) '(a b c)))
;              ==>  (() (a) (a b) (a b c))

; ----- Core -----

(define (fail x) '())

(define (succeed x) (list x))

(define failed? null?)

(define (var x) (cons '? x))

(define (_) (var '_))

(define (var? x)
  (and (pair? x)
       (eq? (car x) '?)))

(define empty-s '())

(define _bottom_ (var 'bottom))

(define (atom? x) (not (pair? x)))

(define (ext-s x v s) (cons (cons x v) s))

(define (walk x s)
  (if (not (var? x))
      x
      (let ((v (assq x s)))
        (if v
            (walk (cdr v) s)
            x))))

(define (unify x y s)
  (let ((x (walk x s))
        (y (walk y s)))
    (cond ((eqv? x y) s)
          ((var? x) (ext-s x y s))
          ((var? y) (ext-s y x s))
          ((or (atom? x) (atom? y)) #f)
          (else (let ((s (unify (car x) (car y) s)))
                  (and s (unify (cdr x) (cdr y) s)))))))

(define (== x y)
  (lambda (s)
    (let ((s2 (unify x y s)))
      (if s2
          (succeed s2)
          (fail s)))))

(define (any* . g*)
  (lambda (s)
    (letrec
      ((try
         (lambda g*
           (if (null? g*)
               (fail s)
               (append ((car g*) s)
                       (apply try (cdr g*)))))))
      (apply try g*))))

(define-syntax any
  (syntax-rules ()
    ((_) fail)
    ((_ g ...)
       (any* (lambda (s) (g s)) ...))))

(define (all . g*)
  (lambda (s)
    (letrec
      ((try
         (lambda (g* s*)
           (if (null? g*)
               s*
               (try (cdr g*)
                    (apply append
                           (map (car g*) s*)))))))
      (try g* (succeed s)))))

(define (one* . g*)
  (lambda (s)
    (letrec
      ((try
         (lambda g*
           (if (null? g*)
               (fail s)
               (let ((out ((car g*) s)))
                 (if (failed? out)
                     (apply try (cdr g*))
                     out))))))
      (apply try g*))))

(define-syntax one
  (syntax-rules ()
    ((_) fail)
    ((_ g ...)
       (one* (lambda (s) (g s)) ...))))

(define (neg g)
  (lambda (s)
    (let ((out (g s)))
      (if (failed? out)
          (succeed s)
          (fail s)))))

(define (choice x lst)
  (if (null? lst)
      fail
      (any (== x (car lst))
           (choice x (cdr lst)))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g)
       (let () g))
    ((_ (v ...) g)
       (let ((v (var 'v)) ...) g))))

(define (occurs? x y s)
  (let ((v (walk y s)))
    (cond ((var? y) (eq? x y))
          ((var? v) (eq? x v))
          ((atom? v) #f)
          (else (or (occurs? x (car v) s)
                    (occurs? x (cdr v) s))))))

(define (circular? x s)
  (let ((v (walk x s)))
    (if (eq? x v)
        #f
        (occurs? x (walk x s) s))))

(define (walk* x s)
  (letrec
    ((w* (lambda (x s)
           (let ((x (walk x s)))
             (cond ((var? x) x)
                   ((atom? x) x)
                   (else (cons (w* (car x) s)
                               (w* (cdr x) s))))))))
    (cond ((circular? x s) _bottom_)
          ((eq? x (walk x s)) empty-s)
          (else (w* x s)))))

(define (preserve-bottom s)
  (if (occurs? _bottom_ s s)
      '()
      s))

(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))

(define (reify v)
  (letrec
    ((reify-s
       (lambda (v s)
         (let ((v (walk v s)))
           (cond ((var? v)
                   (ext-s v (reify-name (length s)) s))
                 ((atom? v) s)
                 (else (reify-s (cdr v)
                                (reify-s (car v) s))))))))
    (reify-s v empty-s)))

(define (run x g)
  (preserve-bottom
    (map (lambda (s)
           (walk* x (append s (reify (walk* x s)))))
         (g empty-s))))

(define-syntax run*
  (syntax-rules ()
    ((_ () goal) (run #f goal))
    ((_ (v) goal) (run v goal))))

; ----- Tools -----

(define vp (var 'p))
(define vq (var 'q))

(define (conso a d p) (== (cons a d) p))

(define (caro p a) (conso a (_) p))

(define (cdro p d) (conso (_) d p))

(define (pairo p) (conso (_) (_) p))

(define (eqo x y) (== x y))

(define (nullo a) (eqo a '()))

(define (memo x l)
  (fresh (d)
    (any (caro l x)
         (all (cdro l d)
              (memo x d)))))

(define (appendo x y r)
  (any (all (== x '())
            (== y r))
       (fresh (hd tl app)
         (all (conso hd tl x)
              (conso hd app r)
              (appendo tl y app)))))

(define (memqo x l r)
  (fresh (d)
    (any (all (caro l x)
              (== l r))
         (all (cdro l d)
              (memqo x d r)))))

(define (rmemqo x l r)
  (fresh (d)
    (any (all (cdro l d)
              (rmemqo x d r))
         (all (caro l x)
              (== l r)))))

(define (reverseo l r) (rmemqo r l))
