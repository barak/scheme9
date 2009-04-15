# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# Change at least this line:
#PREFIX= /u

# Override default compiler and flags
#CC=	gcc
CFLAGS=	-g -Wall -ansi -pedantic -O

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
datadir=$(prefix)/share
libdir=$(exec_prefix)/lib
mandir=$(prefix)/man

BINDIR=$(bindir)
LIBDIR=$(libdir)/s9fes
DATADIR=$(datadir)/s9fes
MANDIR=$(mandir)/man1

# BITS_PER_WORD_64	use 64-bit bignum arithmetics
# NO_SIGNALS		disable POSIX signal handlers
# IMAGEFILE		default image file name
# LIBRARY		default library source file
# DEFAULT_LIBRARY_PATH	default search path for LOCATE-FILE

DEFS=   -DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(LIBDIR):$(DATADIR):$(LIBDIR)/contrib\""

EXTINI=	unix_init()
EXTOBJ=	unix.o
EXTDEF=	-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\""

all:	s9 s9.image s9.1 all-s9e

all-s9e:	s9e s9e.image s9e.1

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o $@ s9.c

s9.image:	s9 s9.scm
	rm -f $@ && ./s9 -n -d $@

s9e:	s9e.o $(EXTOBJ)
	$(CC) $(CFLAGS) -o $@ $^

s9e.scm:	s9.scm
	ln -s s9.scm $@

s9e.o:	s9.h
s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="$(EXTINI)" $(EXTDEF) \
		-o $@ -c $<

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) -I . -c $<

s9e.image:	s9e s9e.scm ext/system.scm
	rm -f $@ && env S9FES_LIBRARY_PATH=.:./lib \
			./s9e -n -f ext/system.scm -d $@

%.1: %.1.in
	sed -e "s,@LIBDIR@,$(LIBDIR)," < $< \
	 | sed -e "s,@DATADIR@,$(DATADIR)," > $@

%.gz: %
	gzip -9 <$< >$@

lint:
	gcc -g -Wall -ansi -pedantic -O s9.c

test:	s9 s9.image
	./s9 -nf test.scm

libtest:	s9 s9.image
	sh libtest.sh

# old version of install(1) may need -C
#C=-C

install: install-s9 install-s9e

install-s9:	s9 s9.scm s9.image s9.1
	install -d -m 0755 $(DESTDIR)$(BINDIR)
	install -d -m 0755 $(DESTDIR)$(LIBDIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)/help
	install -d -m 0755 $(DESTDIR)$(LIBDIR)/contrib
	install $C -m 0755 s9 $(DESTDIR)$(BINDIR)/
	install $C -m 0644 s9.scm $(DESTDIR)$(DATADIR)/
	install $C -m 0644 s9.image $(DESTDIR)$(LIBDIR)/
	install $C -m 0644 lib/* $(DESTDIR)$(DATADIR)/
	install $C -m 0644 contrib/* $(DESTDIR)$(LIBDIR)/contrib/
	install $C -m 0644 help/* $(DESTDIR)$(DATADIR)/help/

install-s9e:	install-s9 s9e s9e.scm s9e.image
	install -d -m 0755 $(DESTDIR)$(BINDIR)
	install -d -m 0755 $(DESTDIR)$(LIBDIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)/help
	install $C -m 0755 s9e $(DESTDIR)$(BINDIR)/
	install $C -m 0644 s9e.scm $(DESTDIR)$(DATADIR)/
	install $C -m 0644 s9e.image $(DESTDIR)$(LIBDIR)/
	install $C -m 0644 ext/*.scm $(DESTDIR)$(DATADIR)/

clean:
	rm -f s9 s9e s9e.scm s9.image s9e.image s9.1.gz s9e.1.gz s9.1.txt \
		s9e.1.txt s9.1 s9e.1 \
		*.o *.core core s9.9.tgz s9fes.tgz __tmp[12]__ __testfile__

# --- end of distribution Makefile ---
