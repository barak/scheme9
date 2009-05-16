; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (string-contains string1 string2)     ==>  string | #f
; (string-ci-contains string1 string2)  ==>  string | #f
;
; Find the first occurrence of a small string STRING1 in a large
; string STRING2. Return the first substring of STRING2 beginning
; with STRING1. When STRING2 does not contain STRING1, return #F.
; STRING-CI-CONTAINS performs the same function, but ignores case.
;
; Example:   (string-contains "gemeinsam" "ein")     ==>  "einsam"
;            (string-contains "democracy" "people")  ==>  #f

(define (make-string-contains p?)
  (lambda (s u)
    (let ((ks (string-length s))
          (ku (string-length u)))
      (let locate ((i  0))
        (cond ((> (+ i ku) ks) #f)
              ((p? u (substring s i (+ i ku)))
                (substring s i ks))
              (else (locate (+ 1 i))))))))

(define string-contains
  (make-string-contains string=?))

(define string-ci-contains
  (make-string-contains string-ci=?))
