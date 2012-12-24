#! /usr/local/bin/s9e-core -f

; A command for invoking the S9 Editor.
; by Nils M Holm, 2010,2012
; Placed in the Public Domain
;
; Usage: s9e [-r] [file]

; When not using an S9E heap image, uncomment one of the below:

;(load "./contrib/s9e.scm")
;(load-from-library "s9e.scm")
;(load-from-library "parse-optionsb.scm")

(define show-help  (option #\h #f))
(define read-only  (option #\r #f))
(define options    `(,read-only
                     ,show-help))

(define (usage)
  (display "Usage: s9e [-r] [file]")
  (newline))

(let* ((files (parse-options! (sys:command-line) options usage)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
             ,usage
             ""
             "S9E is the Scheme 9 Editor"
             ""
             "-r  read-only mode"
             ""))
      (sys:exit 0)))
  (let ((options (apply append
                        (list (if (opt-val read-only) '(read-only) '())))))
    (if (null? files)
        (apply s9e #f options)
        (apply s9e (car files) options))))
