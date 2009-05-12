; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; Bitwise logic operations:
;
; (bitwise-... integer integer ...) ==> integer
;
; The following operations are implemented:
;
; VAL1             0   0   1   1
; VAL2             0   1   0   1  Operation
; ----------------------------------------------------------------
; bitwise-clear    0   0   0   0  set to 0
; bitwise-and      0   0   0   1  and
; bitwise-and-c2   0   0   1   0  and VAL1 with complement of VAL2
; bitwise-1        0   0   1   1  VAL1
; bitwise-and-c1   0   1   0   0  and complement of VAL1 with VAL2
; bitwise-2        0   1   0   1  VAL2
; bitwise-xor      0   1   1   0  exclusive or
; bitwise-or       0   1   1   1  or
; bitwise-or-not   1   0   0   0  not-or
; bitwise-xor-not  1   0   0   1  not-xor (equivalence)
; bitwise-c2       1   0   1   0  complement of VAL2
; bitwise-or-c2    1   0   1   1  or VAL1 with complement of VAL2
; bitwise-c1       1   1   0   0  complement of VAL1
; bitwise-or-c1    1   1   0   1  or complement of VAL1 with VAL2
; bitwise-and-not  1   1   1   0  not-and
; bitwise-set      1   1   1   1  set to 1
;
; BITWISE-SHIFT-LEFT shifts its first argument to the left by
; N bits where N is the value of the second argument.
; BITWISE-SHIFT-RIGHT shifts its first argument to the right.
;
; Multiple arguments associate to the left, i.e.: (BITWISE-op a b c)
; equals (BITWISE-op (BITWISE-op a b) c) for all of the above
; operations.
;
; Arguments: a, b - integer arguments
;            c    - optional integer arguments
;
; Example:   (bitwise-clear   #b1010 #b1100)  ==>  #b0000
;            (bitwise-or-not  #b1010 #b1100)  ==>  #b0001
;            (bitwise-and-c2  #b1010 #b1100)  ==>  #b0010
;            (bitwise-c2      #b1010 #b1100)  ==>  #b0011
;            (bitwise-and-c1  #b1010 #b1100)  ==>  #b0100
;            (bitwise-c1      #b1010 #b1100)  ==>  #b0101
;            (bitwise-xor     #b1010 #b1100)  ==>  #b0110
;            (bitwise-and-not #b1010 #b1100)  ==>  #b0111
;            (bitwise-and     #b1010 #b1100)  ==>  #b1000
;            (bitwise-xor-not #b1010 #b1100)  ==>  #b1001
;            (bitwise-1       #b1010 #b1100)  ==>  #b1010
;            (bitwise-or-c2   #b1010 #b1100)  ==>  #b1011
;            (bitwise-2       #b1010 #b1100)  ==>  #b1100
;            (bitwise-or-c1   #b1010 #b1100)  ==>  #b1101
;            (bitwise-or      #b1010 #b1100)  ==>  #b1110
;            (bitwise-set     #b1010 #b1100)  ==>  #b1111
;            (bitwise-shift-left 1 10)        ==>  1024
;            (bitwise-shift-right 10 1)       ==>  5

(define (make-variadic f)
  (lambda (a b . c)
    (fold-left f a (cons b c))))

(define (bitwise-op r0 r1 r2 r3)
  (make-variadic
    (lambda (x y)
      (let loop ((r 0)
                 (n 1)
                 (a x)
                 (b y))
        (let ((not-a (even? a))
              (not-b (even? b)))
          (let ((r* (if not-a (if not-b (+ r (* n r0))
                                        (+ r (* n r1)))
                              (if not-b (+ r (* n r2))
                                        (+ r (* n r3))))))
            (if (and (zero? a)
                     (zero? b))
                (if (and (zero? x)
                         (zero? y))
                    r*
                    r)
                (loop r* (+ n n) (quotient a 2) (quotient b 2)))))))))

(define bitwise-clear   (bitwise-op 0 0 0 0))
(define bitwise-and     (bitwise-op 0 0 0 1))
(define bitwise-and-c2  (bitwise-op 0 0 1 0))
(define bitwise-1       (bitwise-op 0 0 1 1))
(define bitwise-and-c1  (bitwise-op 0 1 0 0))
(define bitwise-2       (bitwise-op 0 1 0 1))
(define bitwise-xor     (bitwise-op 0 1 1 0))
(define bitwise-or      (bitwise-op 0 1 1 1))
(define bitwise-or-not  (bitwise-op 1 0 0 0))
(define bitwise-xor-not (bitwise-op 1 0 0 1))
(define bitwise-c2      (bitwise-op 1 0 1 0))
(define bitwise-or-c2   (bitwise-op 1 0 1 1))
(define bitwise-c1      (bitwise-op 1 1 0 0))
(define bitwise-or-c1   (bitwise-op 1 1 0 1))
(define bitwise-and-not (bitwise-op 1 1 1 0))
(define bitwise-set     (bitwise-op 1 1 1 1))

(define bitwise-shift-left
  (make-variadic
    (lambda (a b)
      (* a (expt 2 b)))))

(define bitwise-shift-right
  (make-variadic
    (lambda (a b)
      (quotient a (expt 2 b)))))
