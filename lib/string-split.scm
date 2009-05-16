; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (string-split char string)  ==>  list
;
; Split a string into substrings. CHAR is interpreted as a separator.
; Return a list containing all coherent sequences of non-separating
; characters contained in the given string.
;
; Example:   (string-split #\space " to be  or  not to be ")
;              ==>  ("to" "be" "or" "not" "to" "be")

(define (string-split c s)
  (letrec
    ((skip-separators
       (lambda (i k)
         (cond ((= i k) i)
               ((char=? (string-ref s i) c)
                 (skip-separators (+ i 1) k))
               (else i))))
     (split
       (lambda (i k tmp res)
         (cond ((= i k)
                 (if (string=? "" tmp)
                     res
                     (cons tmp res)))
               ((char=? (string-ref s i) c)
                 (split (skip-separators i k)
                        k
                        ""
                        (cons tmp res)))
               (else (split (+ 1 i)
                            k
                            (string-append
                              tmp
                              (string (string-ref s i)))
                            res))))))
    (let ((k (string-length s)))
      (reverse (split (skip-separators 0 k) k "" '())))))
