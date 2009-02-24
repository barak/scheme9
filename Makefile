# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# gcc
CC=	gcc
CFLAGS=	-g -Wall -ansi -pedantic -O

# vanilla cc
#CC=	cc
#CFLAGS=	-O

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
datadir=$(prefix)/share
libdir=$(exec_prefix)/lib
mandir=$(prefix)/man

# BITS_PER_WORD_64	use 64-bit bignum arithmetics
# NO_SIGNALS		disable POSIX signal handlers
# IMAGEFILE		default image file name
# LIBRARY		default library source file
# DEFAULT_LIBRARY_PATH	default search path for LOCATE-FILE

DEFS=-DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(libdir)/s9fes:$(datadir)/s9fes\""

EXTINI=	unix_init()
EXTOBJ=	unix.o
EXTDEF=	-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\""

all:	s9 s9.image s9e s9e.image

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o $@ s9.c

s9.image:	s9 s9.scm
	rm -f $@ && ./s9 -i -d $@

s9e:	s9e.o $(EXTOBJ)
	$(CC) $(CFLAGS) -o $@ s9e.o $(EXTOBJ)

s9e.scm:	s9.scm
	ln -s s9.scm $@

s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="$(EXTINI)" $(EXTDEF) \
		-o $@ -c s9.c

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) -I . -o unix.o -c ext/unix.c

s9e.image:	s9e s9e.scm
	rm -f $@ && ./s9e -i -d $@

test:	s9 s9.image
	./s9 -if test.scm

libtest:	s9 s9.image
	sh libtest.sh

# old version of install(1) may need -C
#C=-C

install: install-s9 install-s9e

install-s9:	s9 s9.scm s9.image s9.1
	install -d -m 0755 $(DESTDIR)$(bindir)
	install -d -m 0755 $(DESTDIR)$(libdir)/s9fes
	install -d -m 0755 $(DESTDIR)$(datadir)/s9fes
	install -d -m 0755 $(DESTDIR)$(datadir)/s9fes/help
	install $C -m 0755 s9 $(DESTDIR)$(bindir)/
	install $C -m 0644 s9.scm $(DESTDIR)$(datadir)/s9fes/
	install $C -m 0644 s9.image $(DESTDIR)$(libdir)/s9fes/
	install $C -m 0644 lib/* $(DESTDIR)$(datadir)/s9fes/
	install $C -m 0644 help/* $(DESTDIR)$(datadir)/s9fes/help/

install-s9e:	install-s9 s9e s9e.scm s9e.image
	install -d -m 0755 $(DESTDIR)$(bindir)
	install -d -m 0755 $(DESTDIR)$(libdir)/s9fes
	install -d -m 0755 $(DESTDIR)$(datadir)/s9fes
	install -d -m 0755 $(DESTDIR)$(datadir)/s9fes/help
	install $C -m 0755 s9e $(DESTDIR)$(bindir)/
	install $C -m 0644 s9e.scm $(DESTDIR)$(datadir)/s9fes/
	install $C -m 0644 s9e.image $(DESTDIR)$(libdir)/s9fes/
	install $C -m 0644 ext/*.scm $(DESTDIR)$(datadir)/s9fes/

s9.1: s9.1.in
	sed -e "s,@LIBDIR@,$(libdir)/s9fes," < $? \
	 | sed -e "s,@DATADIR@,$(datadir)/s9fes," > $@

clean:
	rm -f s9 s9e s9e.scm s9.image s9e.image s9.1.gz ext/ext-*.h ext/ext.h \
		s9.1 \
		__testfile__ *.o *.core core s9.7.tgz s9fes.tgz __tmp[12]__

# --- end of distribution Makefile ---
