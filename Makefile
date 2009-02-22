# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# gcc
CC=	gcc
CFLAGS=	-g -Wall -ansi -pedantic -O

# vanilla cc
#CC=	cc
#CFLAGS=	-O

# Change at least these:
BINDIR=	/u/bin
LIBDIR=	/u/share/s9fes
MANDIR=	/u/man/man1

# BITS_PER_WORD_64	use 64-bit bignum arithmetics
# NO_SIGNALS		disable POSIX signal handlers
# IMAGEFILE		default image file name
# LIBRARY		default library source file
# DEFAULT_LIBRARY_PATH	default search path for LOCATE-FILE

DEFS=-DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(LIBDIR)\""

EXTINI=	unix_init()
EXTOBJ=	unix.o
EXTDEF=	-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\""

all:	s9 s9.image # s9e s9e.image

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o s9 s9.c

s9.image:	s9 s9.scm
	rm -f s9.image && ./s9 -i -d s9.image

s9e:	s9e.o $(EXTOBJ)
	$(CC) $(CFLAGS) -o s9e s9e.o $(EXTOBJ)

s9e.scm:	s9.scm
	ln -s s9.scm s9e.scm

s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="$(EXTINI)" $(EXTDEF) \
		-o s9e.o -c s9.c

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) -I . -o unix.o -c ext/unix.c

s9e.image:	s9e s9e.scm
	rm -f s9e.image && ./s9e -i -d s9e.image

test:	s9 s9.image
	./s9 -if test.scm

libtest:	s9 s9.image
	sh libtest.sh

# old version of install(1) may need -C
#C=-C
install:	s9 s9.scm s9.image
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(LIBDIR)/help
	install $C -m 0755 s9 $(BINDIR)
	strip $(BINDIR)/s9
	install $C -m 0644 s9.scm $(LIBDIR)
	install $C -m 0644 s9.image $(LIBDIR)
	install $C -m 0644 lib/* $(LIBDIR)
	sed -e "s,@LIBDIR@,$(LIBDIR)," <s9.1 |gzip -9 >s9.1.gz
	install $C -m 0644 s9.1.gz $(MANDIR)
	install $C -m 0644 help/* $(LIBDIR)/help

install-s9e:	install s9e s9e.scm s9e.image
	install $C -m 0755 s9e $(BINDIR)
	strip $(BINDIR)/s9e
	install $C -m 0644 s9e.scm $(LIBDIR)/s9e.scm
	install $C -m 0644 s9e.image $(LIBDIR)
	install $C -m 0644 ext/*.scm $(LIBDIR)
	install $C -m 0644 s9e.1 $(MANDIR)
	gzip -9f $(MANDIR)/s9e.1

clean:
	rm -f s9 s9e s9e.scm s9.image s9e.image s9.1.gz ext/ext-*.h ext/ext.h \
		__testfile__ *.o *.core core s9.7.tgz s9fes.tgz __tmp[12]__

# --- end of distribution Makefile ---
