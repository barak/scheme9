# mkfile for Plan 9
# By Nils M Holm, 2008

all:V:	s9 s9.image

s9:	s9.8
	8l -o s9 s9.8

s9.8:	s9.c s9.h
	8c -Dplan9 s9.c

s9.image:	s9 s9.scm
	./s9 -d s9.image
