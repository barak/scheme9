# Scheme 9 from Empty Space
# Makefile (obviously)
# By Nils M Holm, 2007-2012
# Placed in the Public Domain.

# Change at least this line:
PREFIX= /u

# Uncomment these to include the Unix extensions
EXTRA_SCM+=	-l ext/unix.scm
EXTRA_OBJS+=	unix.o
EXTRA_INIT+=	sys_init();
EXTRA_LIBS+=

# Uncomment these to include the Curses extensions
EXTRA_SCM+=	-l ext/curses.scm
EXTRA_OBJS+=	curses.o
EXTRA_INIT+=	curs_init();
EXTRA_LIBS+=	-lcurses

# Uncomment this and define BIG_REAL below to include
# real number arithmetics
EXTRA_SCM+=	-l s9-real.scm

# Set up environment to be used during the build process
BUILD_ENV=	env S9FES_LIBRARY_PATH=.:lib:ext:contrib

# Override default compiler and flags
CC=	gcc
CFLAGS=	-g -Wall -ansi -pedantic -O2

# Where to install the stuff
BINDIR=	$(PREFIX)/bin
LIBDIR=	$(PREFIX)/share/s9fes
MANDIR=	$(PREFIX)/man/man1

# Which OS are we using (unix or plan9)?
OSDEF=	-Dunix

# Options to be added to $(DEFS)
#	-DBITS_PER_WORD_64	# use 64-bit bignum arithmetics
#	-DNO_SIGNALS		# disable POSIX signal handlers
#	-DLIBRARY="\"file\""	# default library source file
#	-DDEFAULT_LIBRARY_PATH="\"dir:...\""
#				# default search path for LOCATE-FILE
#	-DNETWORK		# include socket code in the Unix extension
#	-DCURSES_RESET		# automatically run CURS:ENDWIN on the REPL
#				# (*requires* the Curses extension)
#	-DBIG_REAL		# Enable big real arithmetics
#				# (requires "s9-real.scm" EXTRA_SCM, above)

DEFS=	$(OSDEF) \
	-DDEFAULT_LIBRARY_PATH="\".:~/s9fes:$(LIBDIR)\"" \
	-DEXTENSIONS="$(EXTRA_INIT)" \
	-DNETWORK -DCURSES_RESET \
	-DBIG_REAL

all:	s9 s9.image s9.1.gz s9.1.txt lib/syntax-rules.scm \
		lib/matcher.scm

s9:	s9.o s9.h $(EXTRA_OBJS)
	$(CC) -o s9 s9.o $(EXTRA_OBJS) $(EXTRA_LIBS)

s9.o:	s9.c s9-real.c s9.h
	$(CC) -o s9.o $(CFLAGS) $(DEFS) -c s9.c


s9.image:	s9 s9.scm s9-real.scm ext/unix.scm ext/curses.scm config.scm
	rm -f s9.image && \
		$(BUILD_ENV) ./s9 -n $(EXTRA_SCM) -l config.scm -d s9.image

s9.1.gz:	s9.1
	sed -e "s,@LIBDIR@,$(LIBDIR)," <s9.1 |gzip -9 >s9.1.gz

unix.o:	ext/unix.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o unix.o -c ext/unix.c

curses.o:	ext/curses.c s9.h
	$(CC) $(CFLAGS) $(DEFS) -I . -o curses.o -c ext/curses.c

lint:
	gcc -g -Wall -ansi -pedantic s9.c && rm a.out

test:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/test.scm

libtest:	s9 test.image
	$(BUILD_ENV) sh util/libtest.sh

systest:	s9 s9.image
	$(BUILD_ENV) ./s9 -f util/systest.scm

srtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/srtest.scm

realtest:	s9 test.image
	$(BUILD_ENV) ./s9 -i test -f util/realtest.scm

test.image:	s9 s9.scm s9-real.scm
	$(BUILD_ENV) ./s9 -n $(EXTRA_SCM) -d test.image

tests:
	make test
	make realtest
	make srtest
	make libtest
	make systest

install:	install-s9 install-util

# old version of install(1) may need -c
#C=-c
install-s9:	s9 s9.scm s9.image s9.1.gz
	install -d -m 0755 $(BINDIR)
	install -d -m 0755 $(LIBDIR)
	install -d -m 0755 $(LIBDIR)/help
	install -d -m 0755 $(MANDIR)
	install $C -m 0755 s9 $(BINDIR)
	strip $(BINDIR)/s9
	install $C -m 0644 s9.scm $(LIBDIR)
	install $C -m 0644 s9.image $(LIBDIR)
	install $C -m 0644 lib/* $(LIBDIR)
	install $C -m 0644 ext/*.scm $(LIBDIR)
	install $C -m 0644 contrib/* $(LIBDIR)
	install $C -m 0644 s9.1.gz $(MANDIR)
	install $C -m 0644 help/* $(LIBDIR)/help
	install $C -m 0755 util/make-help-links $(LIBDIR)/help
	(cd $(LIBDIR)/help && ./make-help-links && rm make-help-links)

install-util:
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/s9help.scm >$(BINDIR)/s9help
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/s9resolve.scm >$(BINDIR)/s9resolve
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/scm2html1.scm >$(BINDIR)/scm2html
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/scmpp.scm >$(BINDIR)/scmpp
	-chmod +x $(BINDIR)/s9help	\
		  $(BINDIR)/s9resolve	\
		  $(BINDIR)/scm2html	\
		  $(BINDIR)/scmpp

install-progs:
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/advgen.scm >$(BINDIR)/advgen
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/c2html1.scm >$(BINDIR)/c2html
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/cols.scm >$(BINDIR)/cols
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/dupes.scm >$(BINDIR)/dupes
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/edoc.scm >$(BINDIR)/edoc
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/htmlify.scm >$(BINDIR)/htmlify
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/s9hts.scm >$(BINDIR)/s9hts
	sed -e "s|^#! /usr/local|#! $(PREFIX)|"	\
		<prog/soccat.scm >$(BINDIR)/soccat
	-chmod +x $(BINDIR)/advgen	\
		  $(BINDIR)/c2html	\
		  $(BINDIR)/cols	\
		  $(BINDIR)/dupes	\
		  $(BINDIR)/edoc	\
		  $(BINDIR)/htmlify	\
		  $(BINDIR)/s9hts	\
		  $(BINDIR)/soccat

deinstall:
	rm -f $(LIBDIR)/help/* && rmdir $(LIBDIR)/help
	rm -f $(LIBDIR)/* && rmdir $(LIBDIR)
	rm -f $(BINDIR)/s9
	-rmdir $(BINDIR)
	-rmdir $(MANDIR)

deinstall-util:
	rm -f $(BINDIR)/s9help		\
	      $(BINDIR)/s9resolve	\
	      $(BINDIR)/scm2html	\
	      $(BINDIR)/scmpp

deinstall-progs:
	rm -f $(BINDIR)/advgen		\
	      $(BINDIR)/c2html		\
	      $(BINDIR)/cols		\
	      $(BINDIR)/dupes		\
	      $(BINDIR)/edoc		\
	      $(BINDIR)/htmlify		\
	      $(BINDIR)/s9hts		\
	      $(BINDIR)/soccat

tabs:
	@find . -name \*.scm -exec grep -l "	" {} \;

cd:
	s9 -f util/check-descr.scm

clean:
	rm -f s9 s9.image test.image s9.1.gz *.o *.core \
		CATEGORIES.html core s9fes.tgz __testfile__ 

new-version:
	vi edoc/s9.c.edoc CHANGES
	make s9.c

update-library:
	vi util/make-docs
	util/make-docs
	vi util/make-help-links \
		util/descriptions \
		util/categories.html
	clear
	@echo "Now copy the new help pages from help-new to help"
	@echo "and run (cd help; ../util/make-help-links)."

s9.1.txt:	s9.1
	cc -o rpp util/rpp.c
	nroff s9.1 | ./rpp -a >s9.1.txt
	rm -f rpp

docs:	lib ext contrib
	util/make-docs

webdump:
	util/make-html

advdump:	prog/advgen.scm prog/adventure.adv prog/adventure.intro \
		prog/adventure.imprint
	 prog/advgen.scm -rv \
		-P terminal:session \
		-e prog/adventure.imprint \
		-i prog/adventure.intro \
		-t "The Quest for S9fES" \
		-y s9.css \
		prog/adventure.adv
	cp MASCOT.png util/s9.css advdump

csums:
	txsum -u <_checksums >_checksums.new
	mv _checksums.new _checksums

mksums:	clean
	find . -type f | grep -v _checksums | txsum -m >_checksums

stripped-arc:	clean s9.1.txt
	mv Makefile Makefile.ORIG
