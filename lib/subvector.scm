; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (subvector vector integer1 integer2)  ==>  vector
; (vector-copy vector)                  ==>  vector
;
; SUBVECTOR returns a fresh vector formed from the members of VECTOR
; beginning with index INTEGER1 (inclusive) and ending with index
; INTEGER2 (exclusive).
;
; (Vector-copy v) is shorthand for (subvector v 0 (vector-length v)).
;
; Example:   (subvector '#(a b c d e) 2 4)  ==>  #(c d)
;            (subvector '#(a b c d e) 2 2)  ==>  #()
;            (vector-copy '#(a b c))        ==>  #(a b c)

(define (subvector v p0 pn)
  (let ((k (vector-length v)))
    (cond ((<= 0 p0 pn k)
            (let ((n (make-vector (- pn p0))))
              (do ((i p0 (+ 1 i))
                   (j 0  (+ 1 j)))
                  ((= i pn)
                    n)
                (vector-set! n j (vector-ref v i)))))
          (else
            (error "subvector: bad range" (list p0 pn))))))

(define (vector-copy v)
  (subvector v 0 (vector-length v)))
