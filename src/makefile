all : TAGS docs/tl-manual.pdf lisp bin

distribution : tarballs

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
	if [ -d tlt/dev ] ; then rm -rf tlt/dev ; fi
	if [ -d tlt/macro ] ; then rm -rf tlt/macro ; fi
	if [ -d tl/dev ] ; then rm -rf tl/dev ; fi
	if [ -d tl/macro ] ; then rm -rf tl/macro ; fi
	if [ -d tl/bin ] ; then rm -rf tl/bin ; fi
	if [ -d tl/opt ] ; then rm -rf tl/opt ; fi
	if [ -d lecho/dev ] ; then rm -rf lecho/dev ; fi
	if [ -d lecho/macro ] ; then rm -rf lecho/macro ; fi
	if [ -d lecho/bin ] ; then rm -rf lecho/bin ; fi
	if [ -d lecho/opt ] ; then rm -rf lecho/opt ; fi
	if [ -d dist ] ; then rm -rf dist ; fi
	( cd docs ; make clean )

tarballs : 
	if [ -d dist ] ; then rm -rf dist ; fi
	if [ -d src ] ; then rm -rf src ; fi
	mkdir dist
	cvs -d `cat CVS/Root` export src
	cp src/README src/INSTALL src/LICENSE src/VERSION dist
	( cd src ; tar czvf ../dist/thinlisp-`head -1 VERSION`.tgz * )
	( cd src ; zip ../dist/thinlisp-`head -1 VERSION`.zip * )
	rm -rf src
