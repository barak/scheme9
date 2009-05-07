; Sample PROLOG program
; By Nils M Holm, 1998-2009
; See the LICENSE file of the S9fES package for terms of use
;
; To load this program, run
;
; (load-from-library "prolog.scm")
; (load-from-library "prolog-sample.scm")
;
; Then try some queries like
;
; (? (female ?who))           ; Which females are there?
; (? (_ eric ?who))           ; Which relatives does Eric have?
; (? (?relation cathy ?who))  ; Cathy is related to whom in which way?

(new-database!)      ; set up fresh database

(! (female cathy))   ; add some facts
(! (female denise))
(! (female fanny))
(! (male anthony))
(! (male bertram))
(! (male eric))

(! (parent bertram eric))
(! (parent cathy eric))
(! (parent anthony cathy))
(! (parent eric denise))
(! (parent anthony fanny))

(! (wife cathy bertram))

(:- (husband ?a ?b) (wife ?b ?a))  ; define some predicates

(:- (mother ?a ?b) (female ?a)
                   (parent ?a ?b))

(:- (father ?a ?b) (male ?a)
                   (parent ?a ?b))

(:- (child ?a ?b) (parent ?b ?a))

(:- (descendant ?a ?b) (child ?a ?b))

(:- (descendant ?a ?b) (child ?a ?x)
                       (descendant ?x ?b))