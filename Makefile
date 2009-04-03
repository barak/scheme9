# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# Change at least this line:
PREFIX= /u

# Override default compiler and flags
#CC=	gcc
#CFLAGS=	-g -Wall -ansi -pedantic -O

BINDIR=	$(PREFIX)/bin
LIBDIR=	$(PREFIX)/share/s9fes
MANDIR=	$(PREFIX)/man/man1

# BITS_PER_WORD_64	use 64-bit bignum arithmetics
# NO_SIGNALS		disable POSIX signal handlers
# IMAGEFILE		default image file name
# LIBRARY		default library source file
# DEFAULT_LIBRARY_PATH	default search path for LOCATE-FILE

DEFS=	-DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(LIBDIR):$(LIBDIR)/contrib\""

EXTINI=	unix_init()
EXTOBJ=	unix.o
EXTDEF=	-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\""

all:	s9 s9.image s9.1.gz # all-s9e

all-s9e:	s9e s9e.image s9e.1.gz

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o $@ s9.c

s9.image:	s9 s9.scm
	rm -f $@ && ./s9 -n -d $@

s9e:	s9e.o $(EXTOBJ)
	$(CC) $(CFLAGS) -o $@ s9e.o $(EXTOBJ)

s9e.scm:	s9.scm
	ln -s s9.scm $@

s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="$(EXTINI)" $(EXTDEF) \
		-o $@ -c s9.c

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) -I . -o unix.o -c ext/unix.c

s9e.image:	s9e s9e.scm ext/system.scm
	rm -f $@ && ./s9e -n -f ext/system.scm -d $@

s9.1.gz:	s9.1
	sed -e "s,@LIBDIR@,$(LIBDIR)," <$? |gzip -9 >$@

s9e.1.gz:	s9e.1
	sed -e "s,@LIBDIR@,$(LIBDIR)," <$? |gzip -9 >$@

lint:
	gcc -g -Wall -ansi -pedantic -O s9.c

test:	s9 s9.image
	./s9 -nf test.scm

libtest:	s9 s9.image
	sh libtest.sh

# old version of install(1) may need -C
#C=-C
install:	s9 s9.scm s9.image s9.1.gz
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(LIBDIR)/contrib
	install -d -m 0755 $(LIBDIR)/help
	install $C -m 0755 s9 $(BINDIR)
	strip $(BINDIR)/s9
	install $C -m 0644 s9.scm $(LIBDIR)
	install $C -m 0644 s9.image $(LIBDIR)
	install $C -m 0644 lib/* $(LIBDIR)
	install $C -m 0644 contrib/* $(LIBDIR)/contrib
	install $C -m 0644 s9.1.gz $(MANDIR)
	install $C -m 0644 help/* $(LIBDIR)/help

install-s9e:	install s9e s9e.scm s9e.image s9e.1.gz
	install $C -m 0755 s9e $(BINDIR)
	strip $(BINDIR)/s9e
	install $C -m 0644 s9e.scm $(LIBDIR)/s9e.scm
	install $C -m 0644 s9e.image $(LIBDIR)
	install $C -m 0644 ext/*.scm $(LIBDIR)
	install $C -m 0644 s9e.1.gz $(MANDIR)

clean:
	rm -f s9 s9e s9e.scm s9.image s9e.image s9.1.gz s9e.1.gz s9.1.txt \
		*.o *.core core s9.9.tgz s9fes.tgz __tmp[12]__ __testfile__

# --- end of distribution Makefile ---
