#! /usr/local/bin/s9 -f

; An arse(1) command for invoking the ARSE editor.
; by Nils M Holm, 2010
; See the LICENSE file of the S9fES package for terms of use
;
; Usage: arse [-r] [file ...]

(load "contrib/arse.scm")
;(load-from-library "arse.scm")
(load-from-library "parse-optionsb.scm")

(define show-help  (option #\h #f))
(define read-only  (option #\r #f))
(define options    `(,read-only
                     ,show-help))

(define (usage)
  (display "Usage: arse [-r] [file ...]")
  (newline))

(let* ((files (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
             ,usage
             ""
             "ARSE is a Recursive Scheme Editor"
             ""
             "-r  read-only mode"
             ""))
      (sys:exit 0)))
  (let ((options (string-append
                   (if (opt-val read-only) "r" ""))))
    (if (null? files)
        (arse #f options)
        (arse (car files) options))))
