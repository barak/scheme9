; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; S9fES Statistics Package.
;
; *pi*  ==>  3.1415...
; *e*   ==>  2.7182...
;
; The famous constants.
;
; (id form)  ==>  form
;
; Identity function. Useful in SUM and PROD.
; E.g.: (id 1)  ==>  1
;
; (floor-int real)  ==>  integer
; (round-int real)  ==>  integer
;
; Round a real number and return the closest exact integer.
; E.g.: (floor-int 3.5)  ==>  3
;       (round-int 3.5)  ==>  4
;
; (round-at integer real)  ==>  real
;
; Round REAL to INTEGER fractional digits.
; E.g.: (round-at 3 *pi*)  ==>  3.142
;
; (size set)  ==>  integer
;
; Return the size (number of items) of a data set.
; E.g.: (size '(1 5 19))  ==>  3
;
; (nth set integer)  ==>  item
;
; Return the INTEGER'th item of a data set.
; The first item is at position 0.
; E.g.: (nth '(1 2 3) 1)  ==>  2
;
; (sort set)  ==>  set
;
; Sort the items of a data set (numerically ascending).
; E.g.: (sort '(2 3 1))  ==>  (1 2 3)
;
; (iota integer1 integer2)  ==>  list
;
; Return a list containing the numbers from integer1 to integer2.
; E.g.: (iota 1 4)  ==>  (1 2 3 4)
;
; (sqr real)  ==>  real
;
; Square a number.
; E.g.: (sqr 5)  ==> 25
;
; (sign real)  ==>  -1 | 0 | +1
;
; Return the smallest integer with the same sign as REAL.
; E.g.: (sign -1.4142)  ==>  -1
;
; (! real)  ==>  real
; 
; Compute real!.
; E.g.: (! 5)  ==>  120
;
; (mean set)  ==> real
;
; Compute the arithmetic mean of a data set.
; E.g.: (mean '(1 2 3 4))  ==>  2.5
;
; (maxval set)  ==>  real
; (minval set)  ==>  real
;
; Return the maximum/minimum value of a set.
; E.g.: (maxval '(2 1 4 3))  ==>  4
;       (minval '(2 1 4 3))  ==>  1
;
; (range set)  ==>  real
;
; Compute the range of a data set (MAXVAL - MINVAL).
; E.g.: (range '(2 1 4 3))  ==>  3
;
; (pctile integer set)  ==>  real
; E.g.: (pctile 30 '(2 1 4 3))  ==>  2
;
; Return the INTEGER'th percentile of SET. Assert 0<=INTEGER<=100.
; When INTEGER falls between two values, return their MEAN.
;
; (qtile integer set)  ==>  real
;
; Return the INTEGER'th quartile of SET. Assert 0<=INTEGER<=4.
; The n'th QTILE is the 4n'th PCTILE.
; E.g.: (qtile 3 '(2 1 4 3))  ==>  3.5
;
; (median set)  ==>  real
;
; Return the median of SET. The median of a set is its 2nd QTILE.
; E.g.: (median '(1 2 3 4))  ==>  2.5
;
; (freq set)  ==>  ((item . integer) ...)
;
; Return the frequency of each item of SET.
; E.g.: (freq '(5 7 5 9 7 5))  ==>  ((7 . 2) (9 . 1) (5 . 3))
;
; (modes set)  ==>  list
;
; Return the modes (the most frequently occurring items) of SET.
; E.g.: (modes '(1 2 3 3 4 4))  ==>  (3 4)
;
; (sum procedure integer1 integer2)           ==>  real
; (sum procedure integer1 integer2 integer3)  ==>  real
; (sum* procedure set ...)                    ==>  real
;
; Compute the sum of PROCEDURE(i) over INTEGER1<=i<INTEGER2.
; When INTEGER3 is given, compute PROCEDURE(i*INTEGER3), i.e.
; increment i by INTEGER3 after each step.
; SUM* computes the sum over the elements of the given SETs,
; using PROCEDURE to combine members of sets.
; E.g.: (sum id 1 5)          ==>  10
;       (sum* id '(1 2 3 4))  ==>  10
;
; (prod procedure integer1 integer2)           ==>  real
; (prod procedure integer1 integer2 integer3)  ==>  real
; (prod* procedure set ...)                    ==>  real
;
; Compute the product of PROCEDURE(i) over INTEGER1<=i<INTEGER2.
; When INTEGER3 is given, compute PROCEDURE(i*INTEGER3), i.e.
; increment i by INTEGER3 after each step.
; PROD* computes the product over the elements of the given SETs
; and uses PROCEDURE to combine members of sets.
; E.g.: (prod id 1 5)          ==>  24
;       (prod* id '(1 2 3 4))  ==>  120
;
; (slope set)  ==>  real
;
; Compute the slope of the linear regression of SET. When the linear
; regression line is defined by the formula Y=XB+A, this function
; returns the coefficient B.
; E.g.: (slope '(1 3 5 7 9))  ==>  2.0
;
; (y-int set)  ==>  real
;
; Compute the y-intercept of the linear regression of SET. When the
; linear regression line is defined by the formula Y=XB+A, this
; function returns the coefficient A.
; E.g.: (y-int '(1 3 5 7 9))  ==>  -1.0
;
; (stddev-slope set)  ==>  real
;
; Compute the standard deviation of the points around the regression line.
; E.g.: (stddev-slope '(1 2 5 8 9))  ==>  0.632455532033675865
;
; (stderr-slope set)  ==>  real
;
; Compute the standard error of the points around the regression line.
; E.g.: (stderr-slope '(1 2 5 8 9))  ==>  0.199999999999999999
;
; (var set)      ==>  real
; (var-pop set)  ==>  real
;
; Compute the variance of SET. VAR calculates the variance of
; a sample and VAR-POP the variance of a population.
; E.g.: (var '(1 2 5 8 9))      ==>  12.5
;       (var-pop '(1 2 5 8 9))  ==>  10.0
;
; (stddev set)      ==>  real
; (stddev-pop set)  ==>  real
;
; Compute the standard deviation of SET. STDDEV calculates the standard
; deviation of a sample and STDDEV-POP the standard deviation of a
; population.
; E.g: (stddev '(1 2 5 8 9))      ==>  3.53553390593273762
;      (stddev-pop '(1 2 5 8 9))  ==>  3.16227766016837933
;
; (corr set)  ==>  real
;
; Compute the correlation of the data points in SET. The return value
; Y will be in the range -1..1, where Y>0 means the the the X and Y
; axis are correlated, Y<0 means that they are negatively correlated
; and Y=0 means that they are not correlated at all. Values closer to
; zero denote weaker correlation. When no X values are given,
; (iota 1 (size set)) is used.
; E.g.: (corr '((1 . 1) (2 . 2) (3 . 3)))  ==>  1.0
;       (corr '(9 8 5 2 1))                ==>  -0.983869910099907469
;
; (zscores set)      ==>  set
; (zscores-pop set)  ==>  set
;
; Convert a set of raw scores to a set of z-scores. Z-scores measure
; the distance from the mean of the set in standard deviations, so a
; value of -1 would indicate the the corresponding raw score is one
; standard deviation below the mean. ZSCORES-POP computes the z-scores
; of a population and ZSCORES the z-scores of a sample.
; E.g.: (zscores '(1 2 3))      ==>  (-1.0 0.0 1.0)
; E.g.: (zscores-pop '(1 2 3))  ==>  (-1.224... 0.0 1.224...)
;
; (bindist integer1 integer2 real)   ==>  real
; (bindist* integer1 integer2 real)  ==>  list
; (bindist+ integer1 integer2 real)  ==>  real
;
; LET k=INTEGER1 be the expected number of successful trials, n=INTEGER2
; be the number of total independent trials, and p=REAL the probability
; of a trial to succeed.
; BINDIST(k,n,p) computes the probability of K out of N trials to succeed.
; BINDIST*(k,n,p) computes the probablilities of 0..K out of N trials to
; succeed and returns them in a list.
; BINDIST+(k,n,p) returns the cumulative probability of 0..K out of N
; trials to succeed, i.e. it returns 1.0 if k=n.
; BINDIST(3,5,1/6) would compute the probability of getting three times
; the same face when tossing a dice five times (when the face is chosen
; beforehand).
; E.g.: (bindist 3 5 (/ 6))   ==>  0.0321502057613168719
;       (bindist* 3 5 (/ 6))  ==>  (0.401... 0.401... 0.160... 0.032...)
;       (bindist+ 3 5 (/ 6))  ==>  0.996656378600823041
;
; (stddist real)   ==>  real
; (stddist+ real)  ==>  real
;
; STDDIST implements the probability density function of the standard
; normal distribution, i.e. STDDIST(x) is the probability of a data
; point of a sample to be found at X. X is given in z-scores.
; STDDIST+ implements the cumulative distribution function of the
; standard normal distribution, i.e. STDDIST+(X) returns the area
; under the standard normal curve from -INF to X.
; NOTE: y=STDDIST(x) drops to zero at mean(x)-4 and mean(x)+4 and
; y=STDDIST+(x) drops to zero at mean(x)-4 and rises to 1 at mean(x)+4.
; This is because these function are implemented using lookup tables.
; E.g.: (stddist 0)   ==>  0.4
;       (stddist+ 0)  ==>  0.5
;
; (normdist real1 real2 real3)   ==>  real
; (normdist+ real1 real2 real3)  ==>  real
;
; NORMDIST implements the probability density function of a normal
; distribution with x=REAL1, mean=REAL2, and standard deviation=REAL3.
; NORMDIST+ implements the cumulative distribution function.
; NORMDIST(x,0,1) = STDDIST(x) and NORMDIST+(x,0,1) = STDDIST+(x).
; See also: STDDIST, STTDIST+.
; E.g.: (normdist 2 2 0.5)   ==>  0.8
;       (normdist+ 2 2 0.5)  ==>  1.0
;
; ----------------------------------------------------------------
;
; Automatic testing
;
; Example:  (id 1)                             ==>  1
;           (floor-int 3.5)                    ==>  3
;           (round-int 3.5)                    ==>  4
;           (round-at 3 *pi*)                  ==>  3.142
;           (size '(1 5 19))                   ==>  3
;           (nth '(1 2 3) 1)                   ==>  2
;           (sort '(2 3 1))                    ==>  (1 2 3)
;           (iota 1 4)                         ==>  (1 2 3 4)
;           (sqr 5)                            ==> 25
;           (sign -1.4142)                     ==>  -1
;           (! 5)                              ==>  120
;           (mean '(1 2 3 4))                  ==>  2.5
;           (maxval '(2 1 4 3))                ==>  4
;           (minval '(2 1 4 3))                ==>  1
;           (range '(2 1 4 3))                 ==>  3
;           (pctile 30 '(2 1 4 3))             ==>  2
;           (qtile 3 '(2 1 4 3))               ==>  3.5
;           (median '(1 2 3 4))                ==>  2.5
;           (freq '(5 7 5 9 7 5))              ==>  ((7 . 2) (9 . 1) (5 . 3))
;           (modes '(1 2 3 3 4 4))             ==>  (3 4)
;           (sum id 1 5)                       ==>  10
;           (sum* id '(1 2 3 4))               ==>  10
;           (prod id 1 5)                      ==>  24
;           (prod* id '(1 2 3 4))              ==>  24
;           (slope '(1 3 5 7 9))               ==>  2.0
;           (y-int '(1 3 5 7 9))               ==>  -1.0
;           (var '(1 2 5 8 9))                 ==>  12.5
;           (var-pop '(1 2 5 8 9))             ==>  10.0
;           (stddev-slope '(1 2 3 4 5))        ==>  0.0
;           (stderr-slope '(1 2 3 4 5))        ==>  0.0
;           (corr '((1 . 1) (2 . 2) (3 . 3)))  ==>  1.0
;           (zscores '(1 2 3))                 ==>  (-1.0 0.0 1.0)
;           (bindist 1 2 (/ 2))                ==>  0.5
;           (stddist 0)                        ==>  0.4
;           (stddist+ 0)                       ==>  0.5
;           (normdist 2 2 0.5)                 ==>  0.8
;           (normdist 2 2 0.5)                 ==>  0.8
;           (normdist+ 2 2 0.5)                ==>  1.0

(load-from-library "mergesort.scm")
(load-from-library "hash-table.scm")
(load-from-library "factorial.scm")
(load-from-library "iota.scm")

(define *pi*         #f)
(define *e*          #f)
(define id           #f)
(define floor-int    #f)
(define round-int    #f)
(define round-at     #f)
(define size         #f)
(define nth          #f)
(define sort         #f)
(define sqr          #f)
(define sign         #f)
(define mean         #f)
(define maxval       #f)
(define minval       #f)
(define range        #f)
(define pctile       #f)
(define qtile        #f)
(define median       #f)
(define freq         #f)
(define modes        #f)
(define sum          #f)
(define sum*         #f)
(define prod         #f)
(define prod*        #f)
(define slope        #f)
(define y-int        #f)
(define stddev-slope #f)
(define stderr-slope #f)
(define var          #f)
(define var-pop      #f)
(define stddev       #f)
(define stddev-pop   #f)
(define corr         #f)
(define zscores      #f)
(define zscores-pop  #f)
(define bindist      #f)
(define bindist*     #f)
(define bindist+     #f)
(define stddist      #f)
(define stddist+     #f)
(define normdist     #f)
(define normdist+    #f)
(define !            #f)

(define (init-stat)

  (define pi 3.141592653589793238462643383279502884197169399375105820974944)
  (define e  2.718281828459045235360287471352662497757247093699959574966967)

  (define (sc:id x) x)

  (define (sc:floor-int x) (inexact->exact (floor x)))

  (define (sc:round-int x) (inexact->exact (round x)))

  (define (sc:round-at k x)
    (let ((k (expt 10 k)))
      (/ (round (* x k)) k)))

  (define sc:size length)

  (define sc:nth list-ref)

  (define (sc:sort values)
    (mergesort < values))

  (define (sc:sqr x) (* x x))

  (define (sc:sign x)
    (cond ((zero? x)      0)
          ((positive? x)  1)
          (else          -1)))

  (define (->values x)
    (if (pair? (car x))
        (map cdr x)
        x))

  (define (->set x)
    (if (pair? (car x))
        x
        (map cons (iota 1 (size x)) x)))

  (define (sc:mean values)
    (let ((values (->values values)))
      (/ (apply + values) (sc:size values))))

  (define (sc:maxval x) (apply max x))

  (define (sc:minval x) (apply min x))

  (define (sc:range values)
    (let ((values (->values values)))
      (- (sc:maxval values)
         (sc:minval values))))

  (define (sc:pctile n values)
    (let ((values (->values values)))
      (cond ((not (<= 1 n 100))
              (wrong "pctile: first argument N must be 1 <= N <= 100"))
            ((or (null? values)
                 (null? (cdr values)))
              (wrong "pctile: too few values" values))
            (else
              (let ((vs (sc:sort values))
                    (i  (/ (* (sc:size values) n) 100)))
                (cond ((= n 100)
                        (car (reverse vs)))
                      ((= i (floor i))
                        (let ((t (list-tail vs (- (sc:floor-int i) 1))))
                          (/ (+ (car t) (cadr t)) 2)))
                      (else (list-ref vs (sc:floor-int i)))))))))

  (define (sc:qtile n values)
    (if (not (<= 1 n 4))
        (wrong "qtile: first argument N must be 1 <= N <= 4")
        (sc:pctile (* n 25) values)))

  (define (sc:median values)
    (sc:pctile 50 values))

  (define (sc:freq values)
    (let* ((values (->values values))
           (h  (make-hash-table (length values))))
      (let loop ((v values))
        (if (null? v)
            (hash-table->list h)
            (let* ((n (hash-table-ref h (car v)))
                   (n (if n (cdr n) 0)))
              (hash-table-set! h (car v) (+ 1 n))
              (loop (cdr v)))))))

  (define (sc:modes values)
    (let ((vf (sc:freq values)))
      (let loop ((v   vf)
                 (max 0))
        (cond ((null? v)
                (let loop ((v vf)
                           (m '()))
                  (cond ((null? v)
                          (reverse m))
                        ((= (cdar v) max)
                          (loop (cdr v) (cons (caar v) m)))
                        (else
                          (loop (cdr v) m)))))
              ((> (cdar v) max)
                (loop (cdr v) (cdar v)))
              (else
                (loop (cdr v) max))))))

  (define (acc fa fx base start lim . step)
    (let ((i0  (min start lim))
          (iN  (max start lim))
          (inc (if (null? step) 1 (car step))))
      (let loop ((i i0)
                 (r base))
        (if (>= i iN)
            r
            (loop (+ i inc) (fa r (fx i)))))))

  (define (sc:sum f start lim . step)
    (apply acc + f 0 start lim step))

  (define (sc:sum* f . values)
    (apply + (apply map f values)))

  (define (sc:prod f start lim . step)
      (apply acc * f 1 start lim step))

  (define (sc:prod* f . values)
    (apply * (apply map f values)))

  (define (sc:slope set)
    (let* ((set     (->set set))
           (xvalues (map car set))
           (yvalues (map cdr set))
           (x-mean  (sc:mean xvalues))
           (y-mean  (sc:mean yvalues))
           (n       (sc:size set)))
     (/ (sc:sum* (lambda (xi yi)
                   (* (- xi x-mean)
                      (- yi y-mean)))
                 xvalues
                 yvalues)
        (sc:sum* (lambda (xi)
                   (sc:sqr (- xi x-mean)))
                 xvalues))))

  (define (sc:y-int set)
    (let* ((set    (->set set))
           (x-mean (sc:mean (map car set)))
           (y-mean (sc:mean (map cdr set))))
      (- y-mean (* (sc:slope set) x-mean))))

  (define (sc:stddev-slope set)
    (let* ((set     (->set set))
           (xvalues (map car set))
           (yvalues (map cdr set))
           (x-mean  (sc:mean xvalues))
           (y-mean  (sc:mean yvalues))
           (b       (sc:slope set))
           (n       (sc:size set)))
      (sqrt (/ (- (sc:sum* (lambda (yi)
                             (sc:sqr (- yi y-mean)))
                           yvalues)
                  (* b (sc:sum* (lambda (xi yi)
                                  (* (- xi x-mean)
                                     (- yi y-mean)))
                                xvalues
                                yvalues)))
               (- n 1)))))

  (define (sc:stderr-slope set)
    (let* ((set     (->set set))
           (xvalues (map car set))
           (x-mean  (sc:mean xvalues))
           (n       (sc:size set)))
      (/ (stddev-slope set)
         (sqrt (sc:sum* (lambda (xi)
                          (sc:sqr (- xi x-mean)))
                        xvalues)))))

  (define (var* set dec)
    (let ((values (->values set))
          (n      (sc:size set)))
      (/ (- (sc:sum* sc:sqr values)
            (/ (sc:sqr (sc:sum* sc:id values))
               n))
         (+ n dec))))

  (define (sc:var set) (var* set -1))

  (define (sc:var-pop set) (var* set 0))

  (define (sc:stddev set) (sqrt (sc:var set)))

  (define (sc:stddev-pop set) (sqrt (sc:var-pop set)))

  (define (sc:corr set)
    (let* ((set     (->set set))
           (xvalues (map car set))
           (yvalues (map cdr set))
           (n       (sc:size set))
           (S_xy    (- (sc:sum* * xvalues yvalues)
                       (/ (* (sc:sum* sc:id xvalues)
                             (sc:sum* sc:id yvalues))
                          n)))
           (S_xx    (- (sc:sum* sc:sqr xvalues)
                       (/ (sc:sqr (sc:sum* sc:id xvalues))
                          n)))
           (S_yy    (- (sc:sum* sc:sqr yvalues)
                       (/ (sc:sqr (sc:sum* sc:id yvalues))
                          n))))
      (/ S_xy (sqrt (* S_xx S_yy)))))

  (define (sc:zscores values)
    (let ((values (->values values)))
      (let ((M (sc:mean values))
            (S (sc:stddev values)))
        (map (lambda (x) (/ (- x M) S))
             values))))

  (define (sc:zscores-pop values)
    (let ((values (->values values)))
      (let ((u (sc:mean values))
            (s (sc:stddev-pop values)))
        (map (lambda (x) (/ (- x u) s))
             values))))

  (define (norm M S values)
    (let ((values (sc:zscores (->values values))))
      (map (lambda (z) (+ (* z S) M))
           values)))

  (define (choose n k)
    (if (< 0 k n)
        (quotient (factorial n)
                  (* (factorial k)
                     (factorial (- n k))))
        1))

  (define (sc:bindist k n p)
    (* (choose n k)
       (expt p k)
       (expt (- 1 p) (- n k))))

  (define (sc:bindist* k n p)
    (map (lambda (k) (sc:bindist k n p))
         (iota 0 k)))

  (define (sc:bindist+ k n p)
    (min 1.0 (apply + (map (lambda (k) (sc:bindist k n p))
                             (iota 0 k)))))

  ; (define (sc:stddist x)
  ;   (* (/ (sqrt (* 2 pi)))
  ;      (exp (- (/ (sc:sqr x) 2)))))

  (define pdf
    '#(0.00014 0.00014 0.00015 0.00016 0.00016 0.00017 0.00018 0.00018 
       0.00019 0.00020 0.00021 0.00021 0.00022 0.00023 0.00024 0.00025
       0.00026 0.00027 0.00028 0.00029 0.00030 0.00031 0.00033 0.00034
       0.00035 0.00037 0.00038 0.00039 0.00041 0.00042 0.00044 0.00046
       0.00047 0.00049 0.00051 0.00053 0.00055 0.00057 0.00059 0.00061
       0.00063 0.00066 0.00068 0.00071 0.00073 0.00076 0.00079 0.00081
       0.00084 0.00087 0.00090 0.00094 0.00097 0.00100 0.00104 0.00107
       0.00111 0.00115 0.00119 0.00123 0.00127 0.00132 0.00136 0.00141
       0.00146 0.00151 0.00156 0.00161 0.00167 0.00172 0.00178 0.00184
       0.00190 0.00196 0.00203 0.00210 0.00216 0.00224 0.00231 0.00238
       0.00246 0.00254 0.00262 0.00271 0.00279 0.00288 0.00298 0.00307
       0.00317 0.00327 0.00337 0.00348 0.00358 0.00370 0.00381 0.00393
       0.00405 0.00417 0.00430 0.00443 0.00457 0.00470 0.00485 0.00499
       0.00514 0.00530 0.00545 0.00562 0.00578 0.00595 0.00613 0.00631
       0.00649 0.00668 0.00687 0.00707 0.00727 0.00748 0.00770 0.00792
       0.00814 0.00837 0.00861 0.00885 0.00909 0.00935 0.00961 0.00987
       0.01014 0.01042 0.01071 0.01100 0.01130 0.01160 0.01191 0.01223
       0.01256 0.01289 0.01323 0.01358 0.01394 0.01431 0.01468 0.01506
       0.01545 0.01585 0.01625 0.01667 0.01709 0.01753 0.01797 0.01842
       0.01888 0.01936 0.01984 0.02033 0.02083 0.02134 0.02186 0.02239
       0.02294 0.02349 0.02406 0.02463 0.02522 0.02582 0.02643 0.02705
       0.02768 0.02833 0.02898 0.02965 0.03034 0.03103 0.03174 0.03246
       0.03319 0.03394 0.03470 0.03547 0.03626 0.03706 0.03788 0.03871
       0.03955 0.04041 0.04128 0.04217 0.04307 0.04398 0.04491 0.04586
       0.04682 0.04780 0.04879 0.04980 0.05082 0.05186 0.05292 0.05399
       0.05508 0.05618 0.05730 0.05844 0.05959 0.06077 0.06195 0.06316
       0.06438 0.06562 0.06687 0.06814 0.06943 0.07074 0.07206 0.07341
       0.07477 0.07614 0.07754 0.07895 0.08038 0.08183 0.08329 0.08478
       0.08628 0.08780 0.08933 0.09089 0.09246 0.09405 0.09566 0.09728
       0.09893 0.10059 0.10226 0.10396 0.10567 0.10741 0.10915 0.11092
       0.11270 0.11450 0.11632 0.11816 0.12001 0.12188 0.12376 0.12566
       0.12758 0.12952 0.13147 0.13344 0.13542 0.13742 0.13943 0.14146
       0.14350 0.14556 0.14764 0.14973 0.15183 0.15395 0.15608 0.15822
       0.16038 0.16256 0.16474 0.16694 0.16915 0.17137 0.17360 0.17585
       0.17810 0.18037 0.18265 0.18494 0.18724 0.18954 0.19186 0.19419
       0.19652 0.19886 0.20121 0.20357 0.20594 0.20831 0.21069 0.21307
       0.21546 0.21785 0.22025 0.22265 0.22506 0.22747 0.22988 0.23230
       0.23471 0.23713 0.23955 0.24197 0.24439 0.24681 0.24923 0.25164
       0.25406 0.25647 0.25888 0.26129 0.26369 0.26609 0.26848 0.27086
       0.27324 0.27562 0.27798 0.28034 0.28269 0.28504 0.28737 0.28969
       0.29200 0.29431 0.29659 0.29887 0.30114 0.30339 0.30563 0.30785
       0.31006 0.31225 0.31443 0.31659 0.31874 0.32086 0.32297 0.32506
       0.32713 0.32918 0.33121 0.33322 0.33521 0.33718 0.33912 0.34105
       0.34294 0.34482 0.34667 0.34849 0.35029 0.35207 0.35381 0.35553
       0.35723 0.35889 0.36053 0.36213 0.36371 0.36526 0.36678 0.36827
       0.36973 0.37115 0.37255 0.37391 0.37524 0.37654 0.37780 0.37903
       0.38023 0.38139 0.38251 0.38361 0.38466 0.38568 0.38667 0.38762
       0.38853 0.38940 0.39024 0.39104 0.39181 0.39253 0.39322 0.39387
       0.39448 0.39505 0.39559 0.39608 0.39654 0.39695 0.39733 0.39767
       0.39797 0.39822 0.39844 0.39862 0.39876 0.39886 0.39892 0.39894))

  (define (sc:stddist x)
    (let ((pdf (lambda (x)
                 (vector-ref pdf (sc:round-int (* x 100))))))
      (cond ((zero? x)  0.4)
            ((<= x -4)  0)
            ((< x   0)  (pdf (+ x 4)))
            ((>= x +4)  0)
            (else       (pdf (- 4 x))))))

  (define cdf
    '#(0.00000 0.00399 0.00798 0.01197 0.01595 0.01994 0.02392 0.02790 0.03188 
       0.03586 0.03983 0.04380 0.04776 0.05172 0.05567 0.05962 0.06356 0.06749 
       0.07142 0.07535 0.07926 0.08317 0.08706 0.09095 0.09483 0.09871 0.10257
       0.10642 0.11026 0.11409 0.11791 0.12172 0.12552 0.12930 0.13307 0.13683
       0.14058 0.14431 0.14803 0.15173 0.15542 0.15910 0.16276 0.16640 0.17003
       0.17364 0.17724 0.18082 0.18439 0.18793 0.19146 0.19497 0.19847 0.20194
       0.20540 0.20884 0.21226 0.21566 0.21904 0.22240 0.22575 0.22907 0.23237
       0.23565 0.23891 0.24215 0.24537 0.24857 0.25175 0.25490 0.25804 0.26115
       0.26424 0.26730 0.27035 0.27337 0.27637 0.27935 0.28230 0.28524 0.28814
       0.29103 0.29389 0.29673 0.29955 0.30234 0.30511 0.30785 0.31057 0.31327
       0.31594 0.31859 0.32121 0.32381 0.32639 0.32894 0.33147 0.33398 0.33646
       0.33891 0.34134 0.34375 0.34614 0.34849 0.35083 0.35314 0.35543 0.35769
       0.35993 0.36214 0.36433 0.36650 0.36864 0.37076 0.37286 0.37493 0.37698
       0.37900 0.38100 0.38298 0.38493 0.38686 0.38877 0.39065 0.39251 0.39435
       0.39617 0.39796 0.39973 0.40147 0.40320 0.40490 0.40658 0.40824 0.40988
       0.41149 0.41308 0.41466 0.41621 0.41774 0.41924 0.42073 0.42220 0.42364
       0.42507 0.42647 0.42785 0.42922 0.43056 0.43189 0.43319 0.43448 0.43574
       0.43699 0.43822 0.43943 0.44062 0.44179 0.44295 0.44408 0.44520 0.44630
       0.44738 0.44845 0.44950 0.45053 0.45154 0.45254 0.45352 0.45449 0.45543
       0.45637 0.45728 0.45818 0.45907 0.45994 0.46080 0.46164 0.46246 0.46327
       0.46407 0.46485 0.46562 0.46638 0.46712 0.46784 0.46856 0.46926 0.46995
       0.47062 0.47128 0.47193 0.47257 0.47320 0.47381 0.47441 0.47500 0.47558
       0.47615 0.47670 0.47725 0.47778 0.47831 0.47882 0.47932 0.47982 0.48030
       0.48077 0.48124 0.48169 0.48214 0.48257 0.48300 0.48341 0.48382 0.48422
       0.48461 0.48500 0.48537 0.48574 0.48610 0.48645 0.48679 0.48713 0.48745
       0.48778 0.48809 0.48840 0.48870 0.48899 0.48928 0.48956 0.48983 0.49010
       0.49036 0.49061 0.49086 0.49111 0.49134 0.49158 0.49180 0.49202 0.49224
       0.49245 0.49266 0.49286 0.49305 0.49324 0.49343 0.49361 0.49379 0.49396 
       0.49413 0.49430 0.49446 0.49461 0.49477 0.49492 0.49506 0.49520 0.49534 
       0.49547 0.49560 0.49573 0.49585 0.49598 0.49609 0.49621 0.49632 0.49643 
       0.49653 0.49664 0.49674 0.49683 0.49693 0.49702 0.49711 0.49720 0.49728 
       0.49736 0.49744 0.49752 0.49760 0.49767 0.49774 0.49781 0.49788 0.49795 
       0.49801 0.49807 0.49813 0.49819 0.49825 0.49831 0.49836 0.49841 0.49846
       0.49851 0.49856 0.49861 0.49865 0.49869 0.49874 0.49878 0.49882 0.49886
       0.49889 0.49893 0.49896 0.49900 0.49903 0.49906 0.49910 0.49913 0.49916
       0.49918 0.49921 0.49924 0.49926 0.49929 0.49931 0.49934 0.49936 0.49938
       0.49940 0.49942 0.49944 0.49946 0.49948 0.49950 0.49952 0.49953 0.49955
       0.49957 0.49958 0.49960 0.49961 0.49962 0.49964 0.49965 0.49966 0.49968
       0.49969 0.49970 0.49971 0.49972 0.49973 0.49974 0.49975 0.49976 0.49977
       0.49978 0.49978 0.49979 0.49980 0.49981 0.49981 0.49982 0.49983 0.49983
       0.49984 0.49985 0.49985 0.49986 0.49986 0.49987 0.49987 0.49988 0.49988
       0.49989 0.49989 0.49990 0.49990 0.49990 0.49991 0.49991 0.49992 0.49992
       0.49992 0.49992 0.49993 0.49993 0.49993 0.49994 0.49994 0.49994 0.49994
       0.49995 0.49995 0.49995 0.49995 0.49995 0.49996 0.49996 0.49996 0.49996
       0.49996 0.49996 0.49997 0.49997 0.49997 0.49997 0.49997 0.49997 0.49997
       0.49997 0.49998 0.49998 0.49998 0.49998))

  (define (sc:stddist+ x)
    (let ((cdf (lambda (x)
                 (vector-ref cdf (sc:round-int (* x 100))))))
      (cond ((zero? x) 0.5)
            ((< x -4)  0)
            ((< x  0)  (- 0.5 (cdf (- x))))
            ((> x +4)  1)
            (else      (+ 0.5 (cdf x))))))

  ; (define (sc:normdist x m sd)
  ;   (* (/ (* sd (sqrt (* 2 pi))))
  ;      (exp (- (/ (sc:sqr (- x m))
  ;                 (* 2 (sc:sqr sd)))))))

  (define (sc:normdist x m sd)
    (/ (sc:stddist (/ (- x m) sd)) sd))

  (define (sc:normdist+ x m sd)
    (/ (sc:stddist+ (/ (- x m) sd)) sd))

  (set! *pi*         pi)
  (set! *e*          e)
  (set! id           sc:id)
  (set! floor-int    sc:floor-int)
  (set! round-int    sc:round-int)
  (set! round-at     sc:round-at)
  (set! size         sc:size)
  (set! nth          sc:nth)
  (set! sort         sc:sort)
  (set! sqr          sc:sqr)
  (set! sign         sc:sign)
  (set! mean         sc:mean)
  (set! maxval       sc:maxval)
  (set! minval       sc:minval)
  (set! range        sc:range)
  (set! pctile       sc:pctile)
  (set! qtile        sc:qtile)
  (set! median       sc:median)
  (set! freq         sc:freq)
  (set! modes        sc:modes)
  (set! sum          sc:sum)
  (set! sum*         sc:sum*)
  (set! prod         sc:prod)
  (set! prod*        sc:prod*)
  (set! slope        sc:slope)
  (set! y-int        sc:y-int)
  (set! stddev-slope sc:stddev-slope)
  (set! stderr-slope sc:stderr-slope)
  (set! var          sc:var)
  (set! var-pop      sc:var-pop)
  (set! stddev       sc:stddev)
  (set! stddev-pop   sc:stddev-pop)
  (set! corr         sc:corr)
  (set! zscores      sc:zscores)
  (set! zscores-pop  sc:zscores-pop)
  (set! bindist      sc:bindist)
  (set! bindist*     sc:bindist*)
  (set! bindist+     sc:bindist+)
  (set! stddist      sc:stddist)
  (set! stddist+     sc:stddist+)
  (set! normdist     sc:normdist)
  (set! normdist+    sc:normdist+)
  (set! !            factorial))

(init-stat)
