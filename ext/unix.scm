; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; An interface to some Unix system services.
;
; (chdir string) ==> boolean
; (chmod string integer) ==> boolean
; (chmod string string)  ==> boolean
; (chown string integer1|string1|#f integer2|string2|#f) ==> boolean
; (command-line) ==> list of string
; (errno) ==> integer
; (exit integer) ==>
; (exit)         ==>
; (flush output-port) ==> boolean
; (getcwd) ==> string
; (getenv string) ==> string | #f
; (getgid) ==> integer
; (getgrnam string) ==> alist | #f
; (getgrgid integer) ==> alist | #f
; (getpwent string) ==> list
; (getpwnam string) ==> alist | #f
; (getpwuid integer) ==> alist | #f
; (getuid) ==> integer
; (group-name string|integer) ==> string | #f
; (group-gid string|integer) ==> string | #f
; (link string1 string2) ==> boolean
; (lock string) ==> boolean
; (mkdir string integer) ==> boolean
; (format-time string time) ==> string | #f
; (readdir string) ==> string | #f
; (readlink string) ==> string | #f
; (rmdir string) ==> boolean
; (spawn string) ==> (input-port output-port)
; (stat string) ==> alist | #f
; (stat-name string) ==> string | #f
; (stat-size string) ==> integer | #f
; (stat-uid string) ==> integer | #f
; (stat-gid string) ==> integer | #f
; (stat-mode string) ==> integer | #f
; (stat-ctime string) ==> integer | #f
; (stat-atime string) ==> integer | #f
; (stat-mtime string) ==> integer | #f
; (stat-dev string) ==> integer | #f
; (stat-ino string) ==> integer | #f
; (stat-nlink  string) ==> integer | #f
; (symlink string1 string2) ==> boolean
; (system string) ==> boolean
; (time) ==> integer
; (time->unix-time time) ==> integer | #f
; (unix-time->time integer) ==> time
; (unlink string) ==> boolean
; (unlock string) ==> unspecific
; (user-name string|integer) ==> string | #f
; (user-uid string|integer) ==> integer | #f
; (user-gid string|integer) ==> integer | #f
; (user-gecos string|integer) ==> string | #f
; (user-home string|integer) ==> string | #f
; (user-shell string|integer) ==> string | #f
; (utimes string) ==> boolean
;
; (Example): (getcwd) ==> "/u/home/nmh"

(if (not (memq 'unix *extensions*))
    (wrong "The unix system interface requires the \"unix\" extension"))

(load-from-library "bitwise-ops.scm")
(load-from-library "for-all.scm")

(define chdir        unix:chdir)
(define chown        unix:chown)
(define command-line unix:command-line)
(define errno        unix:errno)
(define flush        unix:flush)
(define getcwd       unix:getcwd)
(define getenv       unix:getenv)
(define getgid       unix:getgid)
(define getgrnam     unix:getgrnam)
(define getgrgid     unix:getgrgid)
(define getpwent     unix:getpwent)
(define getpwnam     unix:getpwnam)
(define getpwuid     unix:getpwuid)
(define getuid       unix:getuid)
(define link         unix:link)
(define lock         unix:lock)
(define mkdir        unix:mkdir)
(define readdir      unix:readdir)
(define readlink     unix:readlink)
(define rmdir        unix:rmdir)
(define spawn        unix:spawn)
(define stat         unix:stat)
(define symlink      unix:symlink)
(define system       unix:system)
(define time         unix:time)
(define unlink       unix:unlink)
(define unlock       unix:unlock)
(define utimes       unix:utimes)

(define (stat-accessor tag)
  (lambda (file)
    (let ((s (stat file)))
      (if s
          (cdr (assq tag s))
          #f))))

(define stat-name  (stat-accessor 'name))
(define stat-size  (stat-accessor 'size))
(define stat-uid   (stat-accessor 'uid))
(define stat-gid   (stat-accessor 'gid))
(define stat-mode  (stat-accessor 'mode))
(define stat-ctime (stat-accessor 'ctime))
(define stat-atime (stat-accessor 'atime))
(define stat-mtime (stat-accessor 'mtime))
(define stat-dev   (stat-accessor 'dev))
(define stat-ino   (stat-accessor 'ino))
(define stat-nlink (stat-accessor 'nlink))

; Mode may be
; - an integer representing a decimal mode, e.g. 511
; - a string representing an octal mode, e.g. "0755"
; - a string of the form "[ugoa]+[+-][rwx]+", e.g. "a+rwx"
(define (chmod mode file)
  (letrec
    ((old-mode (stat-mode file))
     (str->mode
       (lambda (s)
         (letrec
           ((op #f)
            (make-mode
              (lambda (u g o m)
                ((if (char=? op #\+)
                     bitwise-or
                     bitwise-and-c2)
                 old-mode
                 (bitwise-or (if u (bitwise-shift-left m 6) 0)
                             (if g (bitwise-shift-left m 3) 0)
                             (if o m 0))))))
           (let loop ((l (string->list s))
                      (u #f)
                      (g #f)
                      (o #f)
                      (m 0)
                      (b #f))
             (if (null? l)
                 (if op (make-mode u g o m) #f)
                 (case (car l)
                   ((#\u) (if b #f (loop (cdr l) #t g  o  m #f)))
                   ((#\g) (if b #f (loop (cdr l) u  #t o  m #f)))
                   ((#\o) (if b #f (loop (cdr l) u  g  #t m #f)))
                   ((#\a) (if b #f (loop (cdr l) #t #t #t m #f)))
                   ((#\r) (if b (loop (cdr l) u g o (bitwise-or m 4) #t) #f))
                   ((#\w) (if b (loop (cdr l) u g o (bitwise-or m 2) #t) #f))
                   ((#\x) (if b (loop (cdr l) u g o (bitwise-or m 1) #t) #f))
                   ((#\+ #\-) (if b #f (begin (set! op (car l))
                                              (loop (cdr l) u g o m #t))))
                   (else  #f))))))))
    (if (not old-mode)
        (wrong "chmod: no such file" file))
    (let ((mode (cond ((integer? mode)
                        mode)
                      ((string? mode)
                        (if (and (> (string-length mode) 0)
                                 (char<=? #\0 (string-ref mode 0) #\9))
                            (string->number mode 8)
                            (let ((m (str->mode mode)))
                              (if m m (wrong "chmod: bad mode" mode)))))
                      (else (wrong "chmod: bad mode" mode)))))
      (if (not (<= 0 mode 511))
          (wrong "chmod: bad mode" mode))
      (unix:chmod file mode))))

(define (exit . code)
  (cond ((null? code)
          (unix:exit 0))
        ((and (null? (cdr code))
              (integer? (car code))
              (<= 0 (car code) 127))
          (unix:exit (car code)))
        (else (wrong "exit: bad argument(s)" code))))

(define (user-accessor tag)
  (lambda (x)
    (let ((u (if (string? x)
                 (unix:getpwnam x)
                 (unix:getpwuid x))))
      (if u
          (cdr (assq tag u))
          #f))))

(define user-name  (user-accessor 'name))
(define user-uid   (user-accessor 'uid))
(define user-gid   (user-accessor 'gid))
(define user-gecos (user-accessor 'gecos))
(define user-home  (user-accessor 'home))
(define user-shell (user-accessor 'shell))

(define (group-accessor tag)
  (lambda (x)
    (let ((u (if (string? x)
                 (unix:getgrnam x)
                 (unix:getgrgid x))))
      (if u
          (cdr (assq tag u))
          #f))))

(define group-name (group-accessor 'name))
(define group-gid  (group-accessor 'gid))

(define (chown user group file)
  (let* ((s (stat file))
         (u (if s
                (cdr (assq 'uid s))
                (wrong "chown: cannot stat" file)))
         (g (cdr (assq 'gid s)))
         (new-u (cond ((integer? user)
                        user)
                      ((string? user)
                        (let ((u (user-uid user)))
                          (if u u (wrong "chown: no such user" user))))
                      ((not user)
                        u)
                      (else (wrong "chown: bad user name/ID" user))))
         (new-g (cond ((integer? group)
                        group)
                      ((string? group)
                        (let ((g (group-gid group)))
                          (if g g (wrong "chown: no such group" group))))
                      ((not group)
                        g)
                      (else (wrong "chown: bad group name/ID" group)))))
    (unix:chown file new-u new-g)))

(define (leap-year? x)
  (or (zero? (remainder x 400))
      (and (zero? (remainder x 4))
           (not (zero? (remainder x 100))))))

(define (unix-time->time n)
  (let* ((leap-year? leap-year?)
         (days/mon  '#(31 28 31 30 31 30 31 31 30 31 30 31))
         (sec/hour  (* 60 60))
         (sec/day   (* 24 sec/hour))
         (wday      (remainder (+ 3 (quotient n sec/day)) 7))
         (year+rest (let loop ((t n)
                               (y 1970))
                      (let ((s (* sec/day (if (leap-year? y) 366 365))))
                        (if (> s t)
                            (list y t)
                            (loop (- t s) (+ 1 y))))))
         (year      (car year+rest))
         (n         (cadr year+rest))
         (mon+rest  (begin
                      (if (leap-year? year)
                          (vector-set! days/mon 1 29))
                      (let loop ((t n)
                                 (m 0))
                        (let ((nt (- t (* sec/day
                                          (vector-ref days/mon m)))))
                          (if (negative? nt)
                              (list m t)
                              (loop nt (+ 1 m)))))))
         (month     (+ 1 (car mon+rest)))
         (n         (cadr mon+rest))
         (day       (+ 1 (quotient n sec/day)))
         (n         (remainder n sec/day))
         (hour      (quotient n sec/hour))
         (n         (remainder n sec/hour))
         (min       (quotient n 60))
         (sec       (remainder n 60)))
    (list wday year month day hour min sec)))

(define (proper-time? t)
  (let ((days/mon  '#(31 28 31 30 31 30 31 31 30 31 30 31)))
    (if (and (list? t)
             (= (length t) 7)
             (number? (cadr t))
             (leap-year? (cadr t)))
        (vector-set! days/mon 1 29))
    (and (list? t)
         (= (length t) 7)
         (for-all number? t)
         (<=    0 (list-ref t 0)    6)
         (<=    1 (list-ref t 2)   12)
         (<=    1 (list-ref t 3) (vector-ref
                                       days/mon
                                       (- (list-ref t 2) 1)))
         (<=    0 (list-ref t 4)   23)
         (<=    0 (list-ref t 5)   59)
         (<=    0 (list-ref t 6)   59))))

(define (time->unix-time t)
  (let* ((leap-year? leap-year?)
         (proper-time? proper-time?)
         (days/mon  '#(31 28 31 30 31 30 31 31 30 31 30 31))
         (sec/hour  (* 60 60))
         (sec/day   (* 24 sec/hour))
         (sec/year  (* 365 sec/day))
         (leap-years-until
           (lambda (x)
             (+ (quotient x 4)
                (- (quotient x 100))
                (quotient x 400)))))
    (if (and (list? t)
             (= (length t) 7)
             (number? (cadr t))
             (leap-year? (cadr t)))
        (vector-set! days/mon 1 29))
    (if (not (proper-time? t))
        #f
        (let ((leap-days (- (leap-years-until (list-ref t 1))
                            (leap-years-until 1970)
                            1)))
          (+ (* sec/year (- (list-ref t 1) 1970))
             (* sec/day leap-days)
             (let loop ((d 0)
                        (m 1))
               (if (< m (list-ref t 2))
                   (loop (+ d (vector-ref days/mon (- m 1)))
                         (+ 1 m))
                   (* d sec/day)))
             (* sec/day (- (list-ref t 3) 1))
             (* sec/hour (list-ref t 4))
             (* 60 (list-ref t 5))
             (list-ref t 6))))))

(define (format-time format time)
  (let ((proper-time? proper-time?)
        (wdays '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
        (months '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
        (zeros (make-string 10 #\0))
        (in-range? (lambda (n0 nn a)
                     (and (not (null? a))
                          (char<=? n0 (car a) nn))))
        (next (lambda (a)
                (if (null? a) a (cdr a)))))
    (if (not (proper-time? time))
        #f
        (let loop ((f (string->list format))
                   (s '()))
          (cond ((null? f)
                  (apply string-append (reverse s)))
                ((char=? (car f) #\~)
                  (let* ((k     (if (in-range? #\0 #\9 (cdr f))
                                    (- (char->integer (cadr f))
                                       (char->integer #\0))
                                    0))
                         (f     (if (in-range? #\0 #\9 (cdr f))
                                    (next f)
                                    f))
                         (colon (in-range? #\: #\: (cdr f)))
                         (f     (if (in-range? #\: #\: (cdr f))
                                    (next f)
                                    f))
                         (at    (in-range? #\@ #\@ (cdr f)))
                         (f     (if (in-range? #\@ #\@ (cdr f))
                                    (next f)
                                    f))
                         (type  (cond ((null? (cdr f)) #f)
                                      ((memv (cadr f)
                                             '(#\w #\y #\m #\d #\h #\s #\~))
                                        (cadr f))
                                      (else #f)))
                         (f     (next f)))
                  (if (not type)
                      #f
                      (let*
                        ((fmt (case type
                                ((#\w) (vector-ref wdays (list-ref time 0)))
                                ((#\y) (number->string (list-ref time 1)))
                                ((#\m) (cond (colon
                                               (number->string
                                                 (list-ref time 2)))
                                             (at
                                               (vector-ref months
                                                           (list-ref time 2)))
                                             (else
                                               (number->string
                                                 (list-ref time 5)))))
                                ((#\d) (number->string (list-ref time 3)))
                                ((#\h) (number->string (list-ref time 4)))
                                ((#\s) (number->string (list-ref time 6)))
                                (else  (string type))))
                         (fmt (let ((n (string-length fmt)))
                                (if (> k n)
                                    (string-append
                                      (substring zeros 0 (- k n))
                                      fmt)
                                    fmt))))
                    (loop (next f)
                          (cons fmt s))))))
                (else
                  (loop (cdr f)
                        (cons (string (car f)) s))))))))

(define (unix:unix) #t)
