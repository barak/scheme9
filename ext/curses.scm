; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (curs:attron integer)   ==>  unspecific
; (curs:attroff integer)  ==>  unspecific
; (curs:standout)         ==>  unspecific
; (curs:standend)         ==>  unspecific
; (curs:box int1 int2 int3 int4)  ==>  unspecific
;
; An interface to some curses routines.

(require-extension curses)

(load-from-library "bitwise-ops.scm")

(define curs:attr-normal    (curs:get-magic-value "A_NORMAL"))
(define curs:attr-standout  (curs:get-magic-value "A_STANDOUT"))
(define curs:attr-underline (curs:get-magic-value "A_UNDERLINE"))
(define curs:attr-bold      (curs:get-magic-value "A_BOLD"))

(define curs:key-backspace (curs:get-magic-value "KEY_BACKSPACE"))
(define curs:key-up        (curs:get-magic-value "KEY_UP"))
(define curs:key-down      (curs:get-magic-value "KEY_DOWN"))
(define curs:key-left      (curs:get-magic-value "KEY_LEFT"))
(define curs:key-right     (curs:get-magic-value "KEY_RIGHT"))
(define curs:key-home      (curs:get-magic-value "KEY_HOME"))
(define curs:key-ppage     (curs:get-magic-value "KEY_PPAGE"))
(define curs:key-npage     (curs:get-magic-value "KEY_NPAGE"))

(define old-attrset curs:attrset)
(define curs:attributes 0)

(define curs:attrset
  (let ((old-attrset old-attrset))
    (lambda (attr)
      (old-attrset attr)
      (set! curs:attributes attr))))

(define (curs:attron attr)
  (curs:attrset (bitwise-or attr curs:attributes)))

(define (curs:attroff attr)
  (curs:attrset (bitwise-and-c2 curs:attributes attr)))

(define (curs:standout)
  (curs:attrset curs:attr-standout))

(define (curs:standend)
  (curs:attrset curs:attr-normal))

(define (curses:curses) #t)
