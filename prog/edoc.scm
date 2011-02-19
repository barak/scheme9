#! /usr/local/bin/s9 -f

#|edoc code|#

; edoc -- embedded documentation processor
; By Nils M Holm, 2010
; Placed in the Public Domain

#|edoc------------------------------------------------------------------
\1{EDOC}
\2{An Embedded Documentation Processor}

EDOC is a text processor that renders Scheme and C programs with
embedded documentation in EDOC format to HTML. This file is an EDOC
document and the program implementing EDOC at the same time. When
run in a Scheme system, it processes EDOC documents, and when passed
to EDOC it results in the documentation for the program, including
its own syntax-highlighted source code.

To convert this file to HTML, run
\b{edoc.scm -l scheme -o edoc.html edoc.scm}

\3{EDOC Format}
Embedded documentation is enclosed by a line starting with the string
"#|edoc" (in Scheme) or "/*edoc" (in C). Its ends with a line ending in
"code|#" (Scheme) or "code*/" (C). Lines of the forms

\b{
"#|edoc ... code|#"
"/*edoc ... code*/"
}

(without the quotes) are ignored completely, by may be used to set the
language type at the beginning of a file. Lines of the form

\b{
"#|edoc reset code|#"
"/*edoc reset code*/"
}

are also ignored but in addition they reset the language type, so
they can be used to compile bilingual files.

A trailing backslash may be used to concatenate subsequent lines:

\b{
foo\\
bar
}

parses as \k{foobar}.

Embedded documentation may contain the following tags. \v{Mode}s may
contain \v{attributes}, but not vice versa. Neither \v{mode}s nor
\v{attribute}s may be nested. The following \v{mode}s exist:

\b{
\\q{edoc-text\=}              quoted text        <BLOCKQUOTE>
\\b{edoc-text\=}              block text         <PRE>
\\u{edoc-text\=}              unsorted list      <UL>
\\o{edoc-text\=}              ordered list       <OL>
\\0{edoc-text file-name\=}    new part           <H1>
\\1{edoc-text\=}              new chapter        <H1>
\\2{edoc-text\=}              new section        <H3>
\\3{edoc-text\=}              new subsection     <H3>
\\i{file}                     image              <IMG src="file">
}

These \v{attribute}s exist:

\b{
\\a{text\=}         anchor               <A name=text></A>
\\r{text url\=}     reference            <A href=url>text</A>
\\s{text\=}         small                <SMALL>
\\v{text\=}         variable             <VAR>
\\k{text\=}         keyword              <CODE>
\\e{text\=}         emphasis             <EM>
\\E{text\=}         strong emphasis      <STRONG>
\\h{text\=}         highlight            <B>
\\l{text\=}         list element         <LI>
\\x{text name\=}    index entry          <A name=name>text</A>
\\X{text name\=}    code index entry     <A name=name><CODE>text</CODE></A>
\\_{text\=}         subscript            <SUB>
\\^{text\=}         superscript          <SUP>
\\\\               literal backslash    literal
\\=c              literal character    literal
}

Setting the \v{url} in \\r or the \v{name} in \\x and \\X to \v{*}
duplicates the preceding text, e.g.: \\x{foo *\=} equals \\x{foo foo\=}.

The CSS2 style sheets "scheme.css" and "ccode.css" contain the
default styles for syntax highlighting. The "edoc.css" style
sheet may be used to define other markup.

\e{Note}: This program handles only subsets of R4RS Scheme and C89
correctly. Caveat utilitor!

\3{Synopsis}

\b{
edoc [iswL] [-b file] [-l lang] [-o file] [-t text] [-x name] [file ...]

Render programs with embedded edoc sections in HTML

-b file  make headings link back to 'file'
-i       generate index file (INDEX)
-l lang  source language is lang (scheme, ccode)
-o file  write output to the given file
-s       strip edoc sections, output raw code
-t text  content of the HTML TITLE tag
-w       overwrite output files (default: keep)
-x name  extract section with given file name
-L       generate Lout output (experimental!)
}
------------------------------------------------------------------code|#

(load-from-library "when.scm")
(load-from-library "setters.scm")
(load-from-library "displaystar.scm")
(load-from-library "scm2html.scm")
(load-from-library "c2html.scm")
(load-from-library "htmlify-char.scm")
(load-from-library "loutify-char.scm")
(load-from-library "string-scan.scm")
(load-from-library "string-translate.scm")
(load-from-library "parse-optionsb.scm")
(load-from-library "read-line.scm")
(load-from-library "append-to-output-file.scm")
(load-from-library "standard-error.scm")
(load-from-library "hash-table.scm")

(define *language*   #f)
(define *title*      #f)
(define *file-name*  #f)
(define *extracting* #f)

(define *output-port* (current-output-port))
(define *to-file*     #f)
(define *ndx-file*    #f)

(define *line-no* 0)

(define *index-tags* (make-hash-table))

(define show-help   (option #\h #f))
(define backlink    (option #\b 'string #f))
(define make-index  (option #\i #f))
(define language    (option #\l 'string #f))
(define output-file (option #\o 'string #f))
(define strip-doc   (option #\s #f))
(define title       (option #\t 'string ""))
(define extract     (option #\x 'string #f))
(define lout        (option #\L #f))
(define overwrite   (option #\w #f))
(define options     `(,show-help
                      ,backlink
                      ,make-index
                      ,language
                      ,output-file
                      ,strip-doc
                      ,title
                      ,extract
                      ,overwrite
                      ,lout))

(define (edoc-error msg . arg)
  (with-output-to-stderr
    (lambda ()
      (display* "edoc: "
                *line-no*
                ": error: "
                msg
                (if (null? arg) "" ": ")
                (if (null? arg) "" (car arg))
                #\newline)
      (sys:exit 1))))

(define (set-language! x)
  (if (and *language*
           (not (eq? x *language*)))
      (edoc-error "conflicting language specification" x))
  (set! *language* x))

(define (pr . items)
  (for-each (lambda (x)
              (display x *output-port*))
            items))

(define (close-file)
  (if *to-file*
      (close-output-port *output-port*)))

(define (next-output-file name)
  (close-file)
  (if (and (opt-val overwrite)
           (file-exists? name))
      (delete-file name))
  (set! *output-port* (open-output-file name))
  (set! *to-file* name))

(define (html-prolog)
  (for-each
    pr
    `("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
                                                        #\newline
       "  \"http://www.w3.org/TR/html4/loose.dtd\">"    #\newline
       "<HTML>"                                         #\newline
       "<HEAD>"                                         #\newline
       "<TITLE>"
       ,(if *file-name* *file-name* "")
       ,(if (and *title* *file-name*) " : " "")
       ,(if *title* *title* "")
       "</TITLE>"                                       #\newline
       "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"scheme.css\">"
       #\newline
       "<LINK rel=\"stylesheet\" type=\"text/css\" href=\"ccode.css\">"
       #\newline
       "<LINK rel=\"stylesheet\" type=\"text/css\""
       " href=\"edoc.css\">"                            #\newline
       "</HEAD>"                                        #\newline
       "<BODY>"                                         #\newline
       #\newline)))

(define (lout-prolog)
  (for-each
    pr
    `("@Include { S9Book }"                #\newline
      "@Use { @BookSetup"                  #\newline
      "    @DocumentHeight { 27.9c}"       #\newline
      "    @DocumentWidth { 21c}"          #\newline
      "    @TopMargin { 1.5c }"            #\newline
      "    @BottomMargin { 0.5c }"         #\newline
      "    @InnerMargin{ 2.5c }"           #\newline
      "    @OuterMargin { 1.5c }"          #\newline
      "    @Spacing { 1.25fx }"            #\newline
      "    @SkipPageNos { 1 2 3 }"         #\newline
      "    @BaseFont { Times Roman 11p }"  #\newline
      "}"                                  #\newline
      "@Document"                          #\newline
      "//"                                 #\newline
      "@Text @Begin"                       #\newline
      #\newline)))

(define (prolog lang)
  (cond
    ((opt-val strip-doc)
      (pr (if (eq? lang 'scheme)
              "; DO NOT EDIT THIS FILE!"
              "/* DO NOT EDIT THIS FILE!")
          (if *file-name*
              (string-append " EDIT \"" *file-name* "\" INSTEAD.")
              "")
          (if (eq? lang 'ccode) " */" "")
          #\newline))
    ((and (opt-val extract)
          (not *extracting*)))
    ((opt-val lout)
      (lout-prolog))
    (else
      (html-prolog))))

(define (html-epilog)
  (for-each
    pr
    '(""         #\newline
      "</BODY>"  #\newline
      "</HTML>"  #\newline)))

(define (lout-epilog)
  (for-each
    pr
    '("@End @Text"  #\newline
      #\newline)))

(define (epilog)
  (if (and (not (opt-val strip-doc))
           (or (not (opt-val extract))
               *extracting*))
      (if (opt-val lout)
          (lout-epilog)
          (html-epilog)))
  (if (opt-val extract)
      (if *extracting*
          (sys:exit)
          (set! *extracting* #t))))

(define mode-text    'text)
(define mode-quote   'quote)    ; \q
(define mode-block   'block)    ; \b
(define mode-ulist   'ulist)    ; \u
(define mode-olist   'olist)    ; \o
(define mode-hd0     'hd0)      ; \0
(define mode-hd1     'hd1)      ; \1
(define mode-hd2     'hd2)      ; \2
(define mode-hd3     'hd3)      ; \3
(define mode-image   'image)    ; \i
(define attr-literal 'literal)  ; \=
(define attr-anchor  'anchor)   ; \a
(define attr-ref     'ref)      ; \r
(define attr-var     'var)      ; \v
(define attr-keyword 'keyword)  ; \k
(define attr-emph    'emph)     ; \e
(define attr-strong  'strong)   ; \E
(define attr-highlt  'highlt)   ; \h
(define attr-elem    'element)  ; \l
(define attr-index   'index)    ; \x
(define attr-cindex  'cindex)   ; \X
(define attr-small   'small)    ; \s
(define attr-sub     'sub)      ; \_
(define attr-super   'super)    ; \^

(define mode #f)
(define attr #f)

(define (html-reset-attr!)
  (cond ((eq? attr attr-elem)    (pr "</LI>"))
        ((eq? attr attr-emph)    (pr "</EM>"))
        ((eq? attr attr-strong)  (pr "</STRONG>"))
        ((eq? attr attr-highlt)  (pr "</B>"))
        ((eq? attr attr-keyword) (pr "</CODE>"))
        ((eq? attr attr-anchor)  (pr "\"></A>"))
        ((eq? attr attr-ref)     (pr "</A>"))
        ((eq? attr attr-var)     (pr "</VAR>"))
        ((eq? attr attr-small)   (pr "</SMALL>"))
        ((eq? attr attr-sub)     (pr "</SUB>"))
        ((eq? attr attr-super)   (pr "</SUP>"))
        ((eq? attr attr-index)   (pr "</A>"))
        ((eq? attr attr-cindex)  (pr "</CODE></A>")))
  (set! attr #f))

(define (lout-reset-attr!)
  (cond ((eq? attr attr-elem)    (pr "} @Br"))
        ((eq? attr attr-emph)    (pr "}}"))
        ((eq? attr attr-strong)  (pr "}}"))
        ((eq? attr attr-highlt)  (pr "}}"))
        ((eq? attr attr-keyword) (pr "}}"))
        ((eq? attr attr-anchor)  (pr "}}"))
        ((eq? attr attr-ref)     (pr "}"))
        ((eq? attr attr-var)     (pr "}}"))
        ((eq? attr attr-small)   (pr "}}"))
        ((eq? attr attr-sub)     (pr "}}"))
        ((eq? attr attr-super)   (pr "}}"))
        ((eq? attr attr-index)   (pr "}}"))
        ((eq? attr attr-cindex)  (pr "}}}")))
  (set! attr #f))

(define (reset-attr!)
  (if (opt-val lout)
      (lout-reset-attr!)
      (html-reset-attr!)))

(define (html-set-attr! x)
  (cond ((eq? x attr-elem)    (pr "<LI>"))
        ((eq? x attr-emph)    (pr "<EM>"))
        ((eq? x attr-strong)  (pr "<STRONG>"))
        ((eq? x attr-highlt)  (pr "<B>"))
        ((eq? x attr-keyword) (pr "<CODE>"))
        ((eq? x attr-anchor)  (pr "<A name=\""))
        ((eq? x attr-ref)     (pr "<A href=\""))
        ((eq? x attr-var)     (pr "<VAR>"))
        ((eq? x attr-small)   (pr "<SMALL>"))
        ((eq? x attr-sub)     (pr "<SUB>"))
        ((eq? x attr-super)   (pr "<SUP>"))
        ((eq? x attr-index)   (pr "<A name=\""))
        ((eq? x attr-cindex)  (pr "<A name=\""))))

(define (lout-set-attr! x)
  (cond ((eq? x attr-elem)    (pr "{@Dash "))
        ((eq? x attr-emph)    (pr "{@I{"))
        ((eq? x attr-strong)  (pr "{@B{"))
        ((eq? x attr-highlt)  (pr "{@B{"))
        ((eq? x attr-keyword) (pr "{@T{"))
        ((eq? x attr-anchor)  (pr "{@PageMark{"))
        ((eq? x attr-ref)     (pr "{"))
        ((eq? x attr-var)     (pr "{@I{"))
        ((eq? x attr-small)   (pr "{@Small{"))
        ((eq? x attr-sub)     (pr "{@Sub{"))
        ((eq? x attr-super)   (pr "{@Sup{"))
        ((eq? x attr-index)   (pr "{}{@X{\""))
        ((eq? x attr-cindex)  (pr "{}{@X{\""))))

(define (set-attr! x)
  (if (not mode)
      (set-mode! mode-text))
  (reset-attr!)
  (if (opt-val lout)
      (lout-set-attr! x)
      (html-set-attr! x))
  (set! attr x))

(define (html-reset-mode!)
  (cond ((eq? mode mode-block) (pr "</PRE>"))
        ((eq? mode mode-olist) (pr "</OL>"))
        ((eq? mode mode-ulist) (pr "</UL>"))
        ((eq? mode mode-hd0)   (pr "</H1>"))
        ((eq? mode mode-hd1)   (pr "</H1>"))
        ((eq? mode mode-hd2)   (pr "</H2>"))
        ((eq? mode mode-hd3)   (pr "</H3>"))
        ((eq? mode mode-image) (pr ".png\">"))
        ((eq? mode mode-quote) (pr "</BLOCKQUOTE>"))
        ((eq? mode mode-text)  (pr "</P>"))))

(define (lout-reset-mode!)
  (cond ((eq? mode mode-block) (pr "}//" #\newline))
        ((eq? mode mode-olist) (pr "}//" #\newline))
        ((eq? mode mode-ulist) (pr "}//" #\newline))
        ((eq? mode mode-hd0)   (pr "}//" #\newline))
        ((eq? mode mode-hd1)   (pr "}//" #\newline))
        ((eq? mode mode-hd2)   (pr "}//" #\newline))
        ((eq? mode mode-hd3)   (pr "}//" #\newline))
        ((eq? mode mode-image)   (pr ".eps}}//"))
        ((eq? mode mode-quote) (pr "}//" #\newline))
        ((eq? mode mode-text)  (pr "}//" #\newline))))

(define (reset-mode!)
  (if (and (or (eq? mode mode-hd0)
               (eq? mode mode-hd1))
           (opt-val backlink))
      (pr "</A>"))
  (if (opt-val lout)
      (lout-reset-mode!)
      (html-reset-mode!))
  (set! mode #f))

(define (html-set-mode! x)
  (cond ((eq? x mode-block) (pr "<PRE>"))
        ((eq? x mode-ulist) (pr "<UL>"))
        ((eq? x mode-olist) (pr "<OL>"))
        ((eq? x mode-hd0)   (pr ""))
        ((eq? x mode-hd1)   (pr "<H1>"))
        ((eq? x mode-hd2)   (pr "<H2>"))
        ((eq? x mode-hd3)   (pr "<H3>"))
        ((eq? x mode-image) (pr "<IMG src=\""))
        ((eq? x mode-quote) (pr "<BLOCKQUOTE>"))
        ((eq? x mode-text)  (pr "<P>"))))

(define (lout-set-mode! x)
  (cond ((eq? x mode-block) (pr "@Pre{"))
        ((eq? x mode-ulist) (pr "@Pa{"))
        ((eq? x mode-olist) (pr "@Pa{"))
        ((eq? x mode-hd0)   (pr "// @Sp // @Chapter{"))
        ((eq? x mode-hd1)   (pr "// @Sp // @Chapter{"))
        ((eq? x mode-hd2)   (pr "// @Pa // @Section{"))
        ((eq? x mode-hd3)   (pr "// @Pa // @SubSection{"))
        ((eq? x mode-image) (pr "//1f {@IncludeGraphic{"))
        ((eq? x mode-quote) (pr "@Pa @I{"))
        ((eq? x mode-text)  (pr "@Pa{"))))

(define (set-mode! x)
  (reset-mode!)
  (if (opt-val lout)
      (lout-set-mode! x)
      (html-set-mode! x))
  (if (and (eq? x mode-hd1)
           (opt-val backlink))
      (pr "<A href=\"" (opt-val backlink) "\">"))
  (set! mode x))

(define (do-break nnl)
  (if (eq? mode mode-block)
      (do ((i 0 (+ 1 i)))
          ((= i nnl))
        (pr #\newline))
      (begin (set-mode! #f)
             (pr #\newline))))

(define (escape-char c)
  (if (opt-val lout)
      (loutify-char c)
      (htmlify-char c)))

(define (loutify-spaces s)
  (let ((k (string-length s))
        (make-spaces
          (lambda (n r)
            (if (positive? n)
                (cons (string-append "{&" (number->string n) "s}")
                      r)
                r))))
    (let loop ((i 0)
               (r '())
               (n 0))
      (cond ((= i k)
              (apply string-append (reverse! (make-spaces n r))))
            ((char=? #\space (string-ref s i))
              (loop (+ 1 i) r (+ 1 n)))
            ((positive? n)
              (loop i
                    (make-spaces n r)
                    0))
            (else
              (loop (+ 1 i) (cons (substring s i (+ 1 i)) r) 0))))))

(define (list->escaped-string a)
  (let ((s (apply string-append (map escape-char a))))
    (if (and (opt-val lout)
             (eq? mode mode-block))
        (loutify-spaces s)
        s)))

(define *command*  #f)
(define *output*   '())
(define *nested*   0)
(define *printref* #f)

(define (flush . keep)
  (pr (list->escaped-string (reverse! *output*)))
  (set! *output* '()))

(define (extract-ref! require-blank)
  (let loop ((o *output*)
             (u '()))
    (cond ((null? o)
            (if require-blank
                (edoc-error "missing blank in \\r{} or \\0{}"))
                (begin (set! *output* '())
                       (list->string u)))
          ((char=? (car o) #\space)
            (set! *output* (cdr o))
            (list->escaped-string u))
          (else
            (loop (cdr o) (cons (car o) u))))))

(define (command-text)
  (let loop ((o *output*)
             (u '()))
    (cond ((null? o)
            (list->string u))
          (else
            (loop (cdr o) (cons (car o) u))))))

(define (close-ndx)
  (if *ndx-file*
      (begin (close-output-port *ndx-file*)
             (set! *ndx-file* #f))))

(define (write-ndx file tag text ctag)
  (let ((TAB (integer->char 9)))
    (if (opt-val make-index)
        (begin
          (if (not *ndx-file*)
              (set! *ndx-file* (append-to-output-file "INDEX")))
          (display* *ndx-file*
                    (if *to-file* *to-file* "")
                    TAB
                    tag
                    TAB
                    (car ctag)
                    text
                    (cadr ctag)
                    #\newline)))))

(define (unique-tag tag)
  (let ((id (cond ((hash-table-ref *index-tags* tag)
                    => (lambda (v)
                         (let ((v (+ 1 (car v))))
                           (hash-table-set! *index-tags* tag v)
                           v)))
                  (else
                    (hash-table-set! *index-tags* tag 0)
                    0))))
    (string-append tag ":" (number->string id))))

(define (make-print-ref s)
  (if (and (> (string-length s) 7)
           (string-ci=? "http:" (substring s 0 5)))
      (string-append " [" s "]")
      (let* ((k (string-scan #\# s))
             (s (if k
                    (substring s (+ 2 k) (string-length s))
                    s)))
        (string-append " [page @PageOf{\"" s "\"}]"))))

(define (htmlname s)
  (string-translate s "#<>&" "_LGA"))

(define (render s nnl)
  (if (> nnl 0)
      (do-break nnl))
  (let loop ((s (string->list s)))
    (cond ((negative? *nested*)
            (edoc-error "unmatched closing brace"))
          ((> *nested* 2)
            (edoc-error "too many nested commands"))
          ((null? s)
            (flush)
            (reset-attr!))
          ((and (not *command*)
                (char=? #\\ (car s)))
            (set! *command* #t)
            (loop (cdr s)))
          ((char=? #\} (car s))
            (dec! *nested*)
            (cond ((eq? attr attr-ref)
                    (set! *printref* #f)
                    (if (opt-val lout)
                        (set! *printref* (make-print-ref (extract-ref! #t)))
                        (begin (pr (extract-ref! #t))
                               (pr "\">"))))
                  ((or (eq? attr attr-cindex)
                       (eq? attr attr-index))
                    (let* ((sym  (extract-ref! #f))
                           (star (string=? sym "*"))
                           (ctag (if (eq? attr attr-cindex)
                                     (if (opt-val lout)
                                         '("@T{" "}")
                                         '("<CODE>" "</CODE>"))
                                     '("" "")))
                           (text  (command-text))
                           (text  (if (string=? text "")
                                      sym
                                      text))
                           (u-sym (unique-tag (if star text sym))))
                      (write-ndx *to-file* (htmlname u-sym) text ctag)
                      (if (opt-val lout)
                          (pr (htmlname u-sym) "\"}{" (car ctag))
                          (pr (htmlname u-sym) "\">" (car ctag)))
                      (if star
                          (flush))))
                  ((eq? mode mode-hd0)
                    (cond ((opt-val lout)
                            (extract-ref! #t))
                          (else
                            (close-ndx)
                            (epilog)
                            (next-output-file (extract-ref! #t))
                            (prolog *language*)
                            (pr "<H1>")
                            (if (opt-val backlink)
                                (pr "<A href=\""
                                    (opt-val backlink)
                                    "\">"))))))
            (flush)
            (if *printref*
                (begin (pr *printref*)
                       (set! *printref* #f)))
            (if attr
                (reset-attr!)
                (reset-mode!))
            (loop (cdr s)))
          (*command*
            (flush)
            (set! *command* #f)
            (cond ((char=? #\\ (car s))
                    (pr (escape-char #\\))
                    (loop (cdr s)))
                  ((pair? (cdr s))
                    (inc! *nested*)
                    (let ((k (car s)))
                      (case k
                            ((#\b) (set-mode! mode-block))
                            ((#\e) (set-attr! attr-emph))
                            ((#\E) (set-attr! attr-strong))
                            ((#\h) (set-attr! attr-highlt))
                            ((#\k) (set-attr! attr-keyword))
                            ((#\l) (set-attr! attr-elem))
                            ((#\o) (set-mode! mode-olist))
                            ((#\q) (set-mode! mode-quote))
                            ((#\a) (set-attr! attr-anchor))
                            ((#\r) (set-attr! attr-ref))
                            ((#\s) (set-attr! attr-small))
                            ((#\u) (set-mode! mode-ulist))
                            ((#\v) (set-attr! attr-var))
                            ((#\x) (set-attr! attr-index))
                            ((#\X) (set-attr! attr-cindex))
                            ((#\_) (set-attr! attr-sub))
                            ((#\^) (set-attr! attr-super))
                            ((#\0) (set-mode! mode-hd0))
                            ((#\1) (set-mode! mode-hd1))
                            ((#\2) (set-mode! mode-hd2))
                            ((#\3) (set-mode! mode-hd3))
                            ((#\i) (set-mode! mode-image))
                            ((#\=) (dec! *nested*)
                                   (pr (escape-char (cadr s))))
                            (else  (edoc-error "unknown command" k)))
                      (loop (cddr s))))
                  (else
                    (loop (cdr s)))))
          (else
            (if (not mode)
                (set-mode! mode-text))
            (push! (car s) *output*)
            (loop (cdr s))))))

(define (edoc-start? s)
  (and (< 5 (string-length s))
       (or (string=? "#|edoc" (substring s 0 6))
           (string=? "/*edoc" (substring s 0 6)))))

(define (edoc-end? s)
  (let ((k (string-length s))
        (d (if (eq? *language* 'scheme)
               "code|#"
               "code*/")))
    (and (> k 5)
         (string=? d (substring s (- k 6) k)))))

(define *read-buffer* '())

(define (buffered-read-line)
  (if (null? *read-buffer*)
      (begin (inc! *line-no*)
             (read-line))
      (pop! *read-buffer*)))

(define (edoc)
  (let loop ((line (buffered-read-line))
             (nnl  0))
    (cond ((or (eof-object? line)
               (edoc-end? line))
            (reset-mode!)
            (if (not (zero? *nested*))
                (edoc-error "missing closing brace at end of edoc section"))
            (set! *nested* 0)
            (if (not (opt-val strip-doc))
                (pr #\newline)))
          ((string=? "" line)
            (loop (buffered-read-line) (+ 1 nnl)))
          ((opt-val strip-doc)
            (loop (buffered-read-line) 0))
          ((let ((k (string-length line)))
             (and (positive? k)
                  (char=? #\\ (string-ref line (- k 1)))
                  (not (and (> k 1)
                            (char=? #\\ (string-ref line (- k 2)))))))
             (let ((k   (string-length line))
                   (new (buffered-read-line)))
               (if (string? new)
                   (loop (string-append (substring line 0 (- k 1)) new) 0)
                   (loop (substring line 0 (- k 1)) 0))))
          (else
            (render line nnl)
            (pr #\newline)
            (loop (buffered-read-line) 0)))))

; The -x option is an ugly hack. It expects the
; \0 command naming the section to start at the
; beginning of a fresh line and requires it to
; be placed directly under an /*edoc or #|edoc
; tag.
;
(define (skip-to-section tag)
  (let ((kt (string-length tag)))
    (let loop ((prev ""))
      (let* ((line (read-line))
             (kl   (if (eof-object? line)
                       0
                       (string-length line))))
        (inc! *line-no*)
        (cond ((eof-object? line)
                (edoc-error "no such section" tag))
              ((and (>= kl (+ 4 kt))
                    (string=? "\\0{" (substring line 0 3))
                    (string=? tag (substring line (- kl kt 1) (- kl 1))))
                (set! *read-buffer*
                      (list prev line)))
              (else
                (loop line)))))))

(define (code)
  (if (opt-val extract)
      (skip-to-section (opt-val extract)))
  (let ((gen (if (eq? *language* 'scheme)
                 scm2html
                 c2html)))
    (let ((out   #f)
          (attr  '(#f #f #f 0 ()))
          (cont  #f)
          (init  #t))
      (let loop ((nnl 0))
        (let ((line (buffered-read-line)))
          (cond ((eof-object? line)
                  (pr (gen 'terminate: attr 'lout-mode: (opt-val lout))
                      #\newline)
                  (set! attr '(#f #f #f 0 ()))
                  (if cont
                      (begin (if (not (opt-val strip-doc))
                                 (if (opt-val lout)
                                     (pr "}" #\newline)
                                     (pr "</PRE>" #\newline)))
                             (close-ndx)
                             (epilog))))
                ((string=? "" line)
                  (loop (+ 1 nnl)))
                ((edoc-start? line)
                  (set-language! (if (char=? #\# (string-ref line 0))
                                     'scheme
                                     'ccode))
                  (cond ((edoc-end? line)
                          (if (string-ci=? "reset" (substring line 7 12))
                              (set! *language* #f))
                          (loop 0))
                        (else
                          (pr (gen 'terminate: attr
                                   'lout-mode: (opt-val lout)))
                          (set! attr '(#f #f #f 0 ()))
                          (if (and cont (not (opt-val strip-doc)))
                              (if (opt-val lout)
                                  (pr "}//" #\newline)
                                  (pr "</PRE>" #\newline)))
                          (set! cont #f)
                          (if init
                              (prolog *language*))
                          (set! init #f)
                          (edoc)
                          (loop 0))))
                (else
                  (if init
                      (prolog *language*))
                  (set! init #f)
                  (when (not cont)
                        (cond ((opt-val strip-doc))
                              ((opt-val lout)
                                (pr "@Code{"))
                              (else
                                (pr "<PRE class="
                                    *language*
                                    ">")))
                        (set! cont #t))
                  (cond ((opt-val strip-doc)
                          #f)
                        ((eq? *language* 'scheme)
                          (set! out (scm2html 'mark-s9-procs: #t
                                              'mark-s9-extns: #t
                                              'input-string: line
                                              'initial-style: attr
                                              'lout-mode: (opt-val lout))))
                        ((eq? *language* 'ccode)
                          (set! out (c2html 'input-string: line
                                            'initial-style: attr
                                            'lout-mode: (opt-val lout))))
                        (else
                          (edoc-error "cannot figure out source language")))
                  (do ((i 0 (+ 1 i)))
                        ((= i nnl))
                    (pr #\newline))
                  (cond (out
                          (set! attr (car out))
                          (pr (cadr out) #\newline))
                        (else
                          (pr line #\newline)))
                  (loop 0))))))))

(define (usage)
  (display "Usage: edoc [-iswL] [-b file] [-l lang] [-o file] [-t title]")
  (display " [-x name]")
  (newline)
  (display "            [file ...]")
  (newline))

(let ((files (parse-options! (sys:command-line) options usage)))
  (if (opt-val language)
      (set! *language* (string->symbol (opt-val language))))
  (if (opt-val output-file)
      (next-output-file (opt-val output-file)))
  (if (opt-val title)
      (set! *title* (opt-val title)))
  (cond ((opt-val show-help)
          (display-usage
            `(""
              ,usage
              ""
              "Render programs with embedded edoc sections in HTML."
              ""
              "-b file  make headings links back to 'file'"
              "-i       generate index file (INDEX)"
              "-l lang  source language is lang (scheme, ccode)"
              "-o file  write output to the given file"
              "-s       strip edoc sections, output raw code"
              "-t text  content of the HTML TITLE tag"
              "-w       overwrite output files (default: keep)"
              "-x name  extract section with given file name"
              "-L       generate Lout output (experimental!)"
              ""))
          (sys:exit 0))
        ((null? files)
          (code))
        (else
          (for-each (lambda (file)
                      (with-input-from-file
                        file
                        (lambda ()
                          (set! *file-name* file)
                          (code))))
                    files))))
