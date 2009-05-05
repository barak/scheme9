; Fast STRING-FIND procedure
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; Based on "A Very Fast Substring Search Algorithm", Daniel M. Sunday,
; CACM v33, #8, August 1990
; and the SUBSTRING-SEARCH-MAKER procedure by Ken Dickey (Aug 1990).
;
; (string-find pattern text)     ==>  offset | #f
; (string-ci-find pattern text)  ==>  offset | #f
;
; Attempt to find the string PATTERN in the string TEXT. Return
; the offset of the first occurrence of PATTERN in TEXT or #F, if
; TEXT does not contain PATTERN. STRING-CI-FIND does the same,
; but ignores case.
;
; Example:   (string-find "test" "This is a test string")     ==>  10
;            (string-find "TEST" "This is a test string")     ==>  #f
;            (string-ci-find "TEST" "This is a test string")  ==>  10

(define (make-string-find ignore-case)
  (lambda (pattern text)
    (letrec
      ((charset-len 256)
       (make-shift-table
         (lambda (pattern k)
           (let* ((shift-table (make-vector charset-len (+ 1 k)))
                  (max (- k 1)))
             (let loop ((i 0))
               (vector-set! shift-table 
                            (char->integer (string-ref pattern i))
                            (- k i))
               (if (< i max)
                   (loop (+ 1 i))
                   shift-table)))))
       (lookup-offset
         (lambda (tbl n)
           (vector-ref tbl (char->integer (string-ref text n)))))
       (char-equal?
         (if ignore-case char-ci=? char=?)))
      (let ((kp (string-length pattern))
            (kt (string-length text)))
        (let ((shift-table (make-shift-table pattern kp))
              (p_max (- kp 1))
              (t_max (- kt 1)))
          (let find ((i 0))
            (if (> (+ kp i) kt)
                #f
                (let compare ((pi 0)
                              (ti i))
                  (cond
                    ((> pi p_max)
                      #f)
                    ((char-equal? (string-ref pattern pi)
                                  (string-ref text ti))
                      (if (= pi p_max)
                          i
                          (compare (+ pi 1) (+ ti 1))))
                    ((> (+ kp i) t_max)
                      #f)
                    (else
                      (find (+ i (lookup-offset shift-table
                                                (+ i kp))))))))))))))

(define string-find    (make-string-find #f))
(define string-ci-find (make-string-find #t))
