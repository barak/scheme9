; Scheme 9 from Empty Space, Function Library
; By Nils M Holm, 2010
; Placed in the Public Domain
;
; (get-line integer1 integer2 string1 string2)  ==>  string | #f
;
; GET-LINE edits a single line of text interactively.
;
; INTEGER1 (y) and INTEGER2 (x) specify the coordinates of the
; visual editing buffer on the screen. STRING1 is the initial
; content of the buffer, and STRING2 is a prompt that will be
; displayed in front of the buffer. The length of the buffer
; is unlimited; its visual representation extends to the end
; of the row on the screen. GET-LINE returns a new string with
; the edited content or #F when editing is aborted.
;
; GET-LINE renders the initial content and places the cursor
; at the end of the buffer. Characters typed will be inserted
; into the buffer at cursor position. In addition, GET-LINE
; accepts the following editing commands ([^A] = [control]+[A]):
;
;       [^A]        go to beginning of buffer.
;       [^E]        go to end of buffer.
;       [^B]        move back one character (also [Left]).
;       [^D]        delete character under cursor.
;       [^F]        move forward one character (also [Right]).
;       [ESC]       end editing, return string (also [Enter]).
;       [Backspace] delete character to the left.
;       [^U]        delete all characters in buffer.
;       [^C]        Abort editing, return #F (also [^G]).
;
; (Example): (begin (curs:initscr)
;                   (curs:raw)
;                   (curs:noecho)
;                   (curs:nonl)
;                   (get-line 0 0 "" "Enter text here: "))

(require-extension curses)

(define (get-line y x buf prompt)
  (let* ((lim  256)
         (cols (- (curs:cols) x))
         (rk   0)
         (s    buf)
         (o    (string-length prompt))
         (i    (string-length s))
         (z    i)
         (t    0))
    (curs:move y x)
    (curs:clrtoeol)
    (curs:standout)
    (curs:addstr prompt)
    (curs:standend)
    (let loop ()
      (if (> (- i t) (- cols o 2))
          (set! t (- i (- cols o 2))))
      (if (< i t)
          (set! t i))
      (curs:mvaddstr y o (substring s t (+ t (min (- z t)
                                                  (- cols o 2)))))
      (curs:clrtoeol)
      (curs:move y (+ o (- i t)))
      (let ((k (curs:getch)))
        (cond ((or (= k 27)
                   (= k 13))
                (curs:move y x)
                (curs:clrtoeol)
                s)
              ((and (<= 32 k 126)
                    (< z (- lim 1)))
                (set! s (string-append (substring s 0 i)
                                       (string (integer->char k))
                                       (substring s i z)))
                (set! i (+ 1 i))
                (set! z (+ 1 z))
                (loop))
              ((= k curs:key-backspace)
                (cond ((zero? i)
                        (cond ((zero? z)
                                (curs:move y x)
                                (curs:clrtoeol)
                                #f)
                              (else
                                (curs:beep)
                                (loop))))
                      (else
                        (set! i (- i 1))
                        (set! s (string-append (substring s 0 i)
                                               (substring s (+ 1 i) z)))
                        (set! z (- z 1))
                        (loop))))
              ((= k 4)
                (cond ((>= i z)
                        (curs:beep)
                        (loop))
                      (else
                        (set! s (string-append (substring s 0 i)
                                               (substring s (+ 1 i) z)))
                        (set! z (- z 1))
                        (loop))))
              ((= k 1)
                (set! i 0)
                (loop))
              ((= k 5)
                (set! i z)
                (loop))
              ((or (= k 3)
                   (= k 7))
                #f)
              ((= k 21)
                (set! i 0)
                (set! z 0)
                (set! s "")
                (loop))
              ((and (< i z)
                    (or (= k curs:key-right)
                        (= k 6)))
                (set! i (+ 1 i))
                (loop))
              ((and (positive? i)
                    (or (= k curs:key-left)
                        (= k 2)))
                (set! i (- i 1))
                (loop))
              (else
                (curs:beep)
                (loop)))))))
