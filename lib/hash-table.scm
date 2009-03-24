; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (make-hash-table)      ==> hash-table
; (make-hash-table size) ==> hash-table
;
; (hash-table-ref hash-table key)        ==> value | #f
; (hash-table-set! hash-table key value) ==> unspecific
; (hash-table->list hash-table)          ==> list
;
; Explode a symbol into a list of single-character symbols.
;
; Arguments: size - size of hash table, must be prime
;            h    - hash table
;            k    - key, may be any Scheme object
;            v    - value
;
; Example:   (let ((h (make-hash-table 11)))
;              (hash-table-set! h "key" 'value)
;              (hash-table-ref  h "key"))
;                                          ==>  ("key" . value)

(define (make-hash-table . size)
  (if (and (pair? size)
           (not (null? (cdr size))))
      (wrong "make-hash-table: too many arguments" size))
  (let* ((size (if (null? size) 997 (car size)))
         (h (make-vector (+ 1 size) '())))
    (vector-set! h 0 size)
    h))

(define (hash x k)
  (letrec
    ((string->hash
       (lambda (s k)
         (let ((k (string-length s)))
           (let loop ((h 0)
                      (i 0))
             (if (>= i k)
                 h
                 (loop (remainder
                         (+ (* 8 h) (char->integer (string-ref s i)))
                         k)
                       (+ 1 i)))))))
     (improper-length
       (lambda (x)
         (let loop ((x x)
                    (k 0))
           (if (pair? x)
               (loop (cdr x) (+ 1 k))
               k)))))
    (cond ((symbol? x) (string->hash (symbol->string x) k))
          ((string? x) (string->hash x k))
          ((number? x) (remainder x k))
          ((char? x)   (remainder (char->integer x) k))
          ((pair? x)   (remainder (improper-length x) k))
          ((vector? x) (remainder (vector-length x) k))
          (else        (- k 1)))))

(define hash-table-ref
  (let ((hash hash))
    (lambda (h k)
      (let ((i (+ 1 (hash k (vector-ref h 0)))))
        (assoc k (vector-ref h i))))))

(define hash-table-set!
  (let ((hash hash))
    (lambda (h k v)
    (let ((i (+ 1 (hash k (vector-ref h 0)))))
      (cond ((assoc k (vector-ref h i))
              => (lambda (x)
                   (set-cdr! x v)))
            (else (vector-set! h i (cons (cons k v)
                                         (vector-ref h i)))))))))

(define (hash-table->list h)
  (apply append (cdr (vector->list h))))
