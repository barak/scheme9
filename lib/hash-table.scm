; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (make-hash-table)                               ==>  hash-table
; (make-hash-table integer)                       ==>  hash-table
; (hash-table-ref hash-table object)              ==>  value | #f
; (hash-table-set! hash-table object-k object-v)  ==>  unspecific
; (hash-table->list hash-table)                   ==>  list
;
; MAKE-HASH-TABLE creates a fresh hash table. When a size is
; passed to it, the given number of slots will be created in
; the table, otherwise a built-in default is used.
;
; HASH-TABLE-REF retrieves a (KEY . VALUE) pair from the hash
; table using the given OBJECT as a key. When no pair with the
; given key exists, it returns #F.
;
; HASH-TABLE-SET! stores the value OBJECT-V under the key
; OBJECT-K in the given hash-table.
;
; HASH-TABLE->LIST returns a list containing all pairs of
; the given hash table in no specific order.
;
; Example:   (let ((h (make-hash-table 11)))
;              (hash-table-set! h "key" 'value)
;              (hash-table-ref  h "key"))
;                                          ==>  ("key" . value)

(load-from-library "count.scm")

(define (make-hash-table . size)
  (if (and (pair? size)
           (not (null? (cdr size))))
      (wrong "make-hash-table: too many arguments" size))
  (let ((size (if (null? size)
                  997
                  (car size))))
    (make-vector size '())))

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
                       (+ 1 i))))))))
    (cond ((symbol? x) (string->hash (symbol->string x) k))
          ((string? x) (string->hash x k))
          ((number? x) (remainder x k))
          ((char? x)   (remainder (char->integer x) k))
          ((pair? x)   (remainder (count x) k))
          ((vector? x) (remainder (count (vector->list x)) k))
          (else        (- k 1)))))

(define hash-table-ref
  (let ((hash hash))
    (lambda (h k)
      (let ((i (hash k (vector-length h))))
        (assoc k (vector-ref h i))))))

(define hash-table-set!
  (let ((hash hash))
    (lambda (h k v)
    (let ((i (hash k (vector-length h))))
      (cond ((assoc k (vector-ref h i))
              => (lambda (x)
                   (set-cdr! x v)))
            (else (vector-set! h i (cons (cons k v)
                                         (vector-ref h i)))))))))

(define (hash-table->list h)
  (apply append (vector->list h)))
