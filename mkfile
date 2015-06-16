# mkfile for Plan 9
# By Nils M Holm, 2008; Ray Lai, 2014

</$objtype/mkfile

TARG=		s9
OFILES=		s9.$O
CLEANFILES=	s9.image test.image
CFLAGS=		$CFLAGS -Dplan9

# Include real number arithmetics

CFLAGS=		$CFLAGS -DREALNUM
EXTRA_SCM=	-l s9-real.scm
HFILES=		s9-real.c

all:V: s9 s9.image

tests:V: test realtest srtest libtest

s9:	$O.out
	cp $prereq $target

s9.image:	s9 s9.scm s9-real.scm config.scm
	./s9 -i - -n $EXTRA_SCM -l config.scm -d $target

libtest:V: s9 test.image
	ape/psh util/$target.sh

%test:V: s9 test.image util/%test.scm
	./s9 -i test -f util/$target.scm

test.image: s9 s9.scm s9-real.scm
	./s9 -i - -n $EXTRA_SCM -d $target

</sys/src/cmd/mkone
