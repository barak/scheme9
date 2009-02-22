; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2009
; See the LICENSE file of the S9fES package for terms of use
;
; (chdir string) ==> unspecific
; (chmod string integer) ==> unspecific
; (chown string integer1 integer2) ==> unspecific
; (command-line) ==> list of string
; (exit integer) ==>
; (flush output-port) ==> unspecific
; (getcwd) ==> string
; (getenv string) ==> string
; (getgid) ==> integer
; (getpwnam string) ==> alist
; (getpwuid integer) ==> alist
; (getuid) ==> integer
; (link string1 string2) ==> unspecific
; (lock string) ==> boolean
; (mkdir string integer) ==> unspecific
; (rmdir string) ==> unspecific
; (spawn string) ==> (input-port output-port)
; (stat string) ==> alist
; (symlink string1 string2) ==> unspecific
; (system string) ==> boolean
; (unlock string) ==> unspecific
; (utimes string) ==> 
;
; An interface to some Unix system services.
; 
; Arguments: see help pages
;
; (Example): (getcwd) ==> "/u/home/nmh"

(if (not (memq 'unix *extensions*))
    (wrong "The system functions require the 'unix' extension"))

(define chdir        unix:chdir)
(define chmod        unix:chmod)
(define chown        unix:chown)
(define command-line unix:command-line)
(define exit         unix:exit)
(define flush        unix:flush)
(define getcwd       unix:getcwd)
(define getenv       unix:getenv)
(define getgid       unix:getgid)
(define getpwnam     unix:getpwnam)
(define getpwuid     unix:getpwuid)
(define getuid       unix:getuid)
(define link         unix:link)
(define lock         unix:lock)
(define mkdir        unix:mkdir)
(define rmdir        unix:rmdir)
(define spawn        unix:spawn)
(define stat         unix:stat)
(define symlink      unix:symlink)
(define system       unix:system)
(define unlock       unix:unlock)
(define utimes       unix:utimes)
