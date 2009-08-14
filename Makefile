# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# Change at least this line:
PREFIX= /u

VERSION= 20090811

# Extras to be added to the heap image
EXTRA_STUFF=	-f contrib/help.scm \
		-f contrib/pretty-print.scm

# Set up environment to be used during the build process
BUILD_ENV=	env S9FES_LIBRARY_PATH=.:lib:contrib

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

# Which OS are we using (unix or plan9)?
OSDEF=	-Dunix

# For the S9fES Scientific Calculator; uncomment, if needed
X11BASE=	/usr/X11R6

DEFS=	$(OSDEF) \
	-DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(LIBDIR):$(LIBDIR)/contrib\""

all:	s9 s9.image s9.1.gz # all-s9e

all-s9e:	s9e s9e.image s9e.1.gz

all-s9sc:	s9sc s9sc.image sys6x12.vf s9sc.1.gz

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o s9 s9.c

s9.image:	s9 s9.scm
	rm -f s9.image && $(BUILD_ENV) ./s9 -n $(EXTRA_STUFF) -d s9.image

s9.1.gz:	s9.1
	sed -e "s,@LIBDIR@,$(LIBDIR)," <s9.1 |gzip -9 >s9.1.gz

s9e:	s9e.o unix.o
	$(CC) $(CFLAGS) -o s9e s9e.o unix.o

s9e.scm:	s9.scm
	ln -s s9.scm s9e.scm

s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="unix_init()" \
		-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\"" \
		-o s9e.o -c s9.c

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) $(OSDEF) -I . -o unix.o -c ext/unix.c

s9e.image:	s9e s9e.scm ext/unix.scm
	rm -f s9e.image && \
	$(BUILD_ENV) ./s9e -n -f ext/unix.scm $(EXTRA_STUFF) -d s9e.image

s9e.1.gz:	s9e.1
	sed -e "s,@LIBDIR@,$(LIBDIR)," <s9e.1 |gzip -9 >s9e.1.gz

s9sc:	s9sc.o sc.o
	$(CC) $(CFLAGS) -o s9sc s9sc.o sc.o -L $(X11BASE)/lib -lX11

s9sc.scm:	s9.scm
	ln -s s9.scm s9sc.scm

s9sc.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="sc_init()" \
		-DIMAGEFILE="\"s9sc.image\"" -DLIBRARY="\"s9sc.scm\"" \
		-o s9sc.o -c s9.c

sc.o:	ext/sc.c
	$(CC) $(CFLAGS) -I $(X11BASE)/include -I . -o sc.o -c ext/sc.c

sys6x12.vf:	ext/sys6x12.vfd mkvfont
	./mkvfont 6 12 ext/sys6x12.vfd sys6x12.vf

mkvfont:	ext/mkvfont.c
	$(CC) $(CFLAGS) -o mkvfont ext/mkvfont.c

s9sc.image:	s9sc s9sc.scm ext/sc.scm
	rm -f s9sc.image && \
	$(BUILD_ENV) ./s9sc -n -f ext/sc.scm $(EXTRA_STUFF) -d s9sc.image

s9sc.1.gz:	s9sc.1
	gzip -9c <s9sc.1 >s9sc.1.gz

lint:
	gcc -g -Wall -ansi -pedantic s9.c && rm a.out

test:	s9 s9.image
	$(BUILD_ENV) ./s9 -nf test.scm

libtest:	s9 s9.image
	$(BUILD_ENV) sh libtest.sh

# old version of install(1) may need -c
#C=-c
install:	s9 s9.scm s9.image s9.1.gz
	install -d -m 0755 $(BINDIR)
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(LIBDIR)/contrib
	install -d -m 0755 $(LIBDIR)/help
	install -d -m 0755 $(MANDIR)
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
	install $C -m 0644 s9e.scm $(LIBDIR)
	install $C -m 0644 s9e.image $(LIBDIR)
	install $C -m 0644 ext/unix.scm $(LIBDIR)
	install $C -m 0644 s9e.1.gz $(MANDIR)

install-s9sc:	install s9sc s9sc.scm s9sc.image sys6x12.vf s9sc.1.gz
	install $C -m 0755 s9sc $(BINDIR)
	strip $(BINDIR)/s9sc
	install $C -m 0644 s9sc.scm $(LIBDIR)
	install $C -m 0644 sys6x12.vf $(LIBDIR)
	install $C -m 0644 s9sc.image $(LIBDIR)
	install $C -m 0644 ext/sc.scm $(LIBDIR)
	install $C -m 0644 s9sc.1.gz $(MANDIR)

deinstall:
	rm $(LIBDIR)/contrib/* && rmdir $(LIBDIR)/contrib
	rm $(LIBDIR)/help/* && rmdir $(LIBDIR)/help
	rm $(LIBDIR)/* && rmdir $(LIBDIR)
	rm $(BINDIR)/s9
	rm $(BINDIR)/s9e
	-rmdir $(BINDIR)
	rm $(MANDIR)/s9.1.gz
	-rmdir $(MANDIR)

clean:
	rm -f s9 s9.image s9.1.gz s9.s.txt s9e s9e.scm s9e.image s9e.1.gz \
		s9sc s9sc.scm s9sc.image s9sc.1.gz mkvfont sys6x12.vf \
		*.o *.core core s9.B.tgz s9fes-$(VERSION).tar.gz __tmp[12]__ \
		__testfile__ rpp CHANGES.html LICENSE.html README.html \
		s9.1.html s9.exe s9e.exe

# --- end of distribution Makefile ---
