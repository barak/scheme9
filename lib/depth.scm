; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (depth list) ==> integer
;
; Compute the depth of a list.
;
; Arguments: a - list
;
; Example:   (depth '(a (b (c d (e))))) ==> 4

(define (depth a)
  (if (pair? a)
      (+ 1 (apply max (map depth a)))
      0))
