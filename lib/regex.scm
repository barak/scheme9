; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (re-comp string)        ==>  list
; (re-match list string)  ==>  string | #f
;
; Compile and match regular expressions.
;
; RE-COMP compiles a regular expression (RE) and returns it.
; Compiled REs (CREs) are represented by lists.
;
; RE-MATCH matches a compiled RE against a string. When
; (part of) the string matches the CRE, it returns the
; matching part. When the CRE does not match the string,
; it returns #F.
;
; The following RE patterns are evaluated:
; .          match any character
; [char...]  match character class (may contain ranges of the form c1-c2)
; ^          match beginning of line
; $          match end of line
; *          match zero or more instances of the preceding pattern
; +          match one or more instances of the preceding pattern
; ?          match the preceding pattern optionally
;
; Example:   (re-match (re-comp "^a[b-y]+z$") "abz")    ==>  "abz"
;            (re-match (re-comp "^a[b-y]+z$") "abbbz")  ==>  "abbbz"
;            (re-match (re-comp "^a[b-y]+z$") "az")     ==>  #f

(define (make-range c0 cn cls)
    (if (> c0 cn)
        cls
        (make-range (+ 1 c0)
                    cn
                    (cons (integer->char c0) cls))))

(define compile-class
  (let ((make-range make-range))
    (lambda (in out cls first)
      (cond
        ((null? in) #f)
        ((char=? #\] (car in))
          (list (cdr in) (cons (reverse cls) out)))
        ((and first (char=? #\^ (car in)))
          (compile-class (cdr in) out '(#\]) #f))
        ((and (not first)
              (not (null? (cdr cls)))
              (char=? #\- (car in))
              (pair? (cdr in))
              (not (char=? #\] (cadr in))))
          (let ((c0 (char->integer (car cls)))
                (cn (char->integer (cadr in))))
            (if (< c0 cn)
                (compile-class (cddr in)
                               out
                               (make-range c0 cn (cdr cls)) #f)
                (compile-class (cdr in)
                               out
                               (cons #\- cls) #f))))
        (else
          (compile-class (cdr in)
                         out
                         (cons (car in) cls) #f))))))

(define re-comp
  (let ((compile-class compile-class))
    (lambda (re)
      (letrec
         ((compile
           (lambda (in out)
             (cond
               ((not in) #f)
               ((null? in) (reverse out))
               (else (case (car in)
                       ((#\\)
                         (if (pair? (cdr in))
                             (compile (cddr in)
                                      (cons (cadr in) out))
                             #f))
                       ((#\^ #\$ #\.)
                         (compile (cdr in)
                                  (cons (list (car in)) out)))
                       ((#\* #\?)
                         (compile (cdr in)
                                  (if (null? out)
                                      (cons (car in) out)
                                      (cons (list (car in) (car out))
                                            (cdr out)))))
                       ((#\+)
                         (compile (cdr in)
                                  (if (null? out)
                                      (cons (car in) out)
                                      (cons (list #\* (car out)) out))))
                       ((#\[)
                         (apply compile
                                (compile-class (cdr in) out '(#\[) #t)))
                       (else
                         (compile (cdr in)
                                  (cons (car in) out)))))))))
        (compile (string->list re) '())))))

(define (match-char p c)
  (cond ((char? p)
          (if (char=? #\. p)
              #t
              (char=? p c)))
        ((char=? #\[ (car p))
          (and (memv c (cdr p)) #t))
        ((char=? #\] (car p))
          (not (memv c (cdr p))))
        (else #f)))

(define make-choices
  (let ((match-char match-char))
    (lambda (cre s m)
      (cond
        ((or (null? s)
             (not (match-char (cadar cre) (car s))))
          (list (list s m)))
        (else (cons (list s m)
                    (make-choices cre (cdr s) (cons (car s) m))))))))

(define match-cre
  (let ((match-char match-char))
    (lambda (cre s m)
      (cond
        ((null? cre)
          (list->string (reverse m)))
        ((null? s)
          (cond ((equal? cre '((#\$)))
                  (match-cre '() '() m))
                ((and (pair? (car cre))
                      (char=? #\* (caar cre))
                      (null? (cdr cre)))
                  '())
                (else #f)))
        ((pair? (car cre))
          (cond
            ((char=? #\* (caar cre))
              (match-star cre s m))
            ((char=? #\? (caar cre))
              (if (match-char (cadar cre) (car s))
                  (match-cre (cdr cre) (cdr s) (cons (car s) m))
                  (match-cre (cdr cre) s m)))
            ((match-char (car cre) (car s))
              (match-cre (cdr cre) (cdr s) (cons (car s) m)))
            (else #f)))
        ((char=? (car cre) (car s))
          (match-cre (cdr cre) (cdr s) (cons (car s) m)))
        (else #f)))))

(define match-star
  (let ((match-cre match-cre))
    (lambda (cre s m)
      (letrec
        ((try-choices
           (lambda (c*)
             (if (null? c*)
                 #f
                 (let ((r (match-cre (cdr cre) (caar c*) (cadar c*))))
                   (if r
                       (list->string (append (reverse m) (string->list r)))
                       (try-choices (cdr c*))))))))
        (try-choices (reverse (make-choices cre s '())))))))

(define try-matches
  (let ((match-cre match-cre))
    (lambda (cre s)
      (cond ((null? s) (match-cre cre s '()))
        (else (let ((r (match-cre cre s '())))
                (if (or (not r) (string=? "" r))
                    (try-matches cre (cdr s))
                    r)))))))

(define re-match
  (let ((match-cre match-cre)
        (try-matches try-matches))
    (lambda (cre s)
      (if (and (pair? cre) (equal? '(#\^) (car cre)))
          (match-cre (cdr cre) (string->list s) '())
          (try-matches cre (string->list s))))))
