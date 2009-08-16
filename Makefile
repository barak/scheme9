# Scheme 9 from Empty Space Makefile.
# Nils M Holm, 2007,2008,2009

# Change at least this line:
#PREFIX= /u

VERSION= 20090814

# Extras to be added to the heap image
EXTRA_STUFF=	-f contrib/help.scm \
		-f contrib/pretty-print.scm

# Set up environment to be used during the build process
BUILD_ENV=	env S9FES_LIBRARY_PATH=.:lib:contrib

# Override default compiler and flags
#CC=	gcc
CFLAGS=	-g -Wall -ansi -pedantic -O2

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

# Which OS are we using (unix or plan9)?
OSDEF=	-Dunix

# For the S9fES Scientific Calculator; uncomment, if needed
X11BASE=	/usr/X11R6

DEFS=	$(OSDEF) \
	-DDEFAULT_LIBRARY_PATH="\"$(LIBDIR):$(LIBDIR)/contrib:$(DATADIR):~/s9fes\""

EXTINI=	unix_init()
EXTOBJ=	unix.o
EXTDEF=	-DIMAGEFILE="\"s9e.image\"" -DLIBRARY="\"s9e.scm\""

all:	all-s9
all:	all-s9e
all:    all-s9sc

all-s9:	s9 s9.image
all-s9:	s9.1

all-s9e:	s9e s9e.image
all-s9e:	s9e.1

all-s9sc:	s9sc s9sc.image
all-s9sc:	s9sc.1
all-s9sc:	sys6x12.vf

s9:	s9.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -o s9 s9.c

s9.image:	s9 s9.scm contrib/help.scm contrib/pretty-print.scm
	rm -f s9.image && $(BUILD_ENV) ./s9 -n $(EXTRA_STUFF) -d s9.image

s9e:	s9e.o $(EXTOBJ)
	$(CC) $(CFLAGS) -o s9e s9e.o $(EXTOBJ)

s9e.scm:
	ln -sf s9.scm s9e.scm

s9e.o:	s9.h
s9e.o:	s9.c
	$(CC) $(CFLAGS) $(DEFS) -I . -DEXTENSIONS="$(EXTINI)" $(EXTDEF) \
		-o s9e.o -c s9.c

unix.o:	ext/unix.c
	$(CC) $(CFLAGS) $(OSDEF) -I . -o unix.o -c ext/unix.c

s9e.image:	s9e s9e.scm ext/unix.scm
	rm -f s9e.image && \
	$(BUILD_ENV) ./s9e -n -f ext/unix.scm $(EXTRA_STUFF) -d s9e.image

%.1: %.1.in
	sed -e "s,@LIBDIR@,$(LIBDIR),g" < $@.in \
	 | sed -e "s,@DATADIR@,$(DATADIR),g" > $@

%.gz: %
	gzip -9 < $* > $@

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

lint:
	gcc -g -Wall -ansi -pedantic s9.c && rm a.out

test:	s9 s9.image
	$(BUILD_ENV) ./s9 -nf test.scm

libtest:	s9 s9.image
	$(BUILD_ENV) sh libtest.sh

# old version of install(1) may need -c
#C=-c

install: install-s9
install: install-s9e
install: install-s9sc

install-s9:	s9 s9.scm s9.image
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

install-s9e:	s9e s9e.scm s9e.image
	install -d -m 0755 $(DESTDIR)$(BINDIR)
	install -d -m 0755 $(DESTDIR)$(LIBDIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)
	install $C -m 0755 s9e $(DESTDIR)$(BINDIR)/
	install $C -m 0644 s9e.scm $(DESTDIR)$(DATADIR)/
	install $C -m 0644 s9e.image $(DESTDIR)$(LIBDIR)/
	install $C -m 0644 ext/*.scm $(DESTDIR)$(DATADIR)/

install-s9sc:	s9sc s9sc.scm s9sc.image sys6x12.vf
	install -d -m 0755 $(DESTDIR)$(BINDIR)
	install -d -m 0755 $(DESTDIR)$(LIBDIR)
	install -d -m 0755 $(DESTDIR)$(DATADIR)
	install $C -m 0755 s9sc $(DESTDIR)$(BINDIR)
	install $C -m 0644 s9sc.scm $(DESTDIR)$(DATADIR)
	install $C -m 0644 sys6x12.vf $(DESTDIR)$(LIBDIR)
	install $C -m 0644 s9sc.image $(DESTDIR)$(LIBDIR)
	install $C -m 0644 ext/sc.scm $(DESTDIR)$(DATADIR)

deinstall:
	rm $(DESTDIR)$(LIBDIR)/contrib/* && rmdir $(DESTDIR)$(LIBDIR)/contrib
	rm $(DESTDIR)$(LIBDIR)/help/* && rmdir $(DESTDIR)$(LIBDIR)/help
	rm $(DESTDIR)$(LIBDIR)/* && rmdir $(DESTDIR)$(LIBDIR)
	rm $(DESTDIR)$(BINDIR)/s9
	rm $(DESTDIR)$(BINDIR)/s9e
	-rmdir $(DESTDIR)$(BINDIR)

clean:
	rm -f s9.1     s9e.1     s9sc.1
	rm -f s9.1.gz  s9e.1.gz  s9sc.1.gz
	rm -f s9.1.txt s9e.1.txt s9sc.1.txt
	rm -f s9 s9e s9sc
	rm -f s9.image  s9e.image s9sc.image
	rm -f s9.s.txt
	rm -f s9e.scm s9sc.scm
	rm -f mkvfont sys6x12.vf \
		*.o *.core core s9.B.tgz s9fes-$(VERSION).tar.gz __tmp[12]__ \
		__testfile__ rpp CHANGES.html LICENSE.html README.html \
		s9.1.html s9.exe s9e.exe s9sc.exe

# --- end of distribution Makefile ---
