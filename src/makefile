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
	rm -rf {tlt,tl,lecho}/{dev,macro,bin,opt} dist src TAGS
	rm -rf {tl,lecho}/c/*.tlt
	( cd docs ; make clean )

tarballs : 
	if [ -d dist ] ; then rm -rf dist ; fi
	if [ ! -d src ] ; then echo Must export an SRC before making distribution ; exit 1 ; fi
	mkdir dist
	cp src/README src/INSTALL src/LICENSE src/VERSION dist
	( cd src ; tar czvf ../dist/thinlisp-`head -1 VERSION`.tgz * )
	( cd src ; zip -r ../dist/thinlisp-`head -1 VERSION`.zip * )
	( cd src ; bzip2 -cz `find * -type f -print` > ../dist/thinlisp-`head -1 VERSION`.bz2 )
