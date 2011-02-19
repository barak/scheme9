; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009,2010
; Placed in the Public Domain
;
; (draw-tree object)  ==>  unspecific
;
; Print a tree structure resembling a Scheme datum. Each cons
; cell is represented by [o|o] with lines leading to their car
; and cdr parts. Conses with a cdr value of () are represented
; by [o|/].
;
; (Example): (draw-tree '((a) (b . c) (d e)))  ==>  unspecific
;
;            Output:  [o|o]---[o|o]---[o|/]
;                      |       |       |
;                     [o|/]    |      [o|o]---[o|/]
;                      |       |       |       |
;                      a       |       d       e
;                              |
;                             [o|o]--- c
;                              |
;                              b

(define (draw-tree n)

  (define *nothing* (cons 'N '()))

  (define *visited* (cons 'V '()))

  (define (empty? x) (eq? x *nothing*))

  (define (visited? x) (eq? (car x) *visited*))

  (define (mark-visited x) (cons *visited* x))

  (define (members-of x) (cdr x))

  (define (done? x)
    (and (pair? x)
         (visited? x)
         (null? (cdr x))))

  (define (void) (if #f #f))

  (define (draw-fixed-string s)
    (let* ((b (make-string 8 #\space))
           (k (string-length s))
           (s (if (> k 7) (substring s 0 7) s))
           (s (if (< k 3) (string-append " " s) s))
           (k (string-length s)))
      (display (string-append s (substring b 0 (- 8 k))))))

  (define (draw-atom n)
    (cond ((null? n)
            (draw-fixed-string "()"))
          ((symbol? n)
            (draw-fixed-string (symbol->string n)))
          ((number? n)
            (draw-fixed-string (number->string n)))
          ((string? n)
            (draw-fixed-string (string-append "\"" n "\"")))
          ((char? n)
            (draw-fixed-string (string-append "#\\" (string n))))
          ((eq? n #t)
            (draw-fixed-string "#t"))
          ((eq? n #f)
            (draw-fixed-string "#f"))
          (else
            (error "draw-atom: unknown type" n))))

  (define (draw-conses n)
    (letrec
      ((d-conses
         (lambda (n)
           (cond ((not (pair? n))
                   (draw-atom n))
                 ((null? (cdr n))
                   (display "[o|/]"))
                 (else
                   (display "[o|o]---")
                   (d-conses (cdr n)))))))
      (d-conses n)
      n))

  (define (draw-bars n)
    (letrec
      ((d-bars
         (lambda (n)
           (cond ((not (pair? n))
                   (void))
                 ((empty? (car n))
                   (draw-fixed-string "")
                   (d-bars (cdr n)))
                 ((and (pair? (car n))
                       (visited? (car n)))
                   (d-bars (cdar n))
                   (d-bars (cdr n)))
                 (else
                   (draw-fixed-string "|")
                   (d-bars (cdr n)))))))
      (d-bars (members-of n))))

  (define (skip-empty n)
    (letrec
      ((skip2
         (lambda (n)
           (cond ((not (pair? n))
                   n)
                 ((or (empty? (car n))
                      (done? (car n)))
                   (skip2 (cdr n)))
                 (else
                   n)))))
      (skip2 n)))

  (define (remove-trailing-nothing n)
    (reverse! (skip-empty (reverse n))))

  (define (all-vertical? n)
    (or (not (pair? n))
             (and (null? (cdr n))
                  (all-vertical? (car n)))))

  (define (draw-members n)
    (letrec
      ((d-members
         (lambda (n r)
           (cond ((not (pair? n))
                   (reverse! r))
                 ((empty? (car n))
                   (draw-fixed-string "")
                   (d-members (cdr n)
                              (cons *nothing* r)))
                 ((not (pair? (car n)))
                   (draw-atom (car n))
                   (d-members (cdr n)
                              (cons *nothing* r)))
                 ((null? (cdr n))
                   (d-members (cdr n)
                              (cons (draw-final (car n)) r)))
                 ((all-vertical? (car n))
                   (draw-fixed-string "[o|/]")
                   (d-members (cdr n)
                              (cons (caar n) r)))
                 (else
                   (draw-fixed-string "|")
                   (d-members (cdr n)
                              (cons (car n) r)))))))
      (mark-visited
        (remove-trailing-nothing
          (d-members (members-of n) '())))))

  (define (draw-final n)
    (cond ((not (pair? n))
            (draw-atom n)
            *nothing*)
          ((visited? n)
            (draw-members n))
          (else
            (mark-visited (draw-conses n)))))

  (letrec
    ((d-tree
       (lambda (n)
         (cond ((done? n)
                 (void))
               (else
                 (newline)
                 (draw-bars n)
                 (newline)
                 (d-tree (draw-members n)))))))
    (if (not (pair? n))
        (draw-atom n)
        (d-tree (mark-visited (draw-conses n))))
    (newline)))

(define dt draw-tree)
