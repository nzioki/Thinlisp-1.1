all : TAGS docs/tl-manual.pdf lisp bin

TAGS : tlt/lisp/*.lisp tl/lisp/*.lisp tl/c/*.c lecho/lisp/*.lisp lecho/c/*.c makefile
	find tl tlt lecho \( -name '*.[ch]' -o -name '*.lisp' \) -print | etags -

docs/tl-manual.pdf : docs/tl-manual.texinfo
	( cd docs ; make -k )

lisp :
	echo "(load \"boot\")" | lisp -batch

bin :
	( cd tl/bin ; make -kf makefile-linux )
	( cd lecho/bin ; make -kf makefile-linux )

opt :
	( cd tl/opt ; make -kf makefile-linux OPT=1 )
	( cd lecho/opt ; make -kf makefile-linux OPT=1 )

clean :
	if [ -f TAGS ] ; then rm TAGS ; fi
	( cd docs ; make clean )
	if [ -d tlt/dev ] ; then rm -f tlt/dev ; fi
	if [ -d tlt/macro ] ; then rm -f tlt/macro ; fi
	if [ -d tl/dev ] ; then rm -f tl/dev ; fi
	if [ -d tl/macro ] ; then rm -f tl/macro ; fi
	if [ -d tl/bin ] ; then rm -f tl/bin ; fi
	if [ -d tl/opt ] ; then rm -f tl/opt ; fi
	if [ -d lecho/dev ] ; then rm -f lecho/dev ; fi
	if [ -d lecho/macro ] ; then rm -f lecho/macro ; fi
	if [ -d lecho/bin ] ; then rm -f lecho/bin ; fi
	if [ -d lecho/opt ] ; then rm -f lecho/opt ; fi
