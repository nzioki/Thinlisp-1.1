# Makefile for ThinLisp, requires the GNU varient of make.

MF = makefile-`uname -s | tr 'A-Z' 'a-z'`

ifdef THREAD
THREADARG = THREAD=1
else
THREADARG =
endif

all : TAGS docs/tl-manual.pdf lisp bin

distribution : tarballs

TAGS : tlt/lisp/*.lisp tl/lisp/*.lisp tl/c/*.c lecho/lisp/*.lisp lecho/c/*.c makefile
	find tl tlt lecho \( -name '*.[ch]' -o -name '*.lisp' \) -print | etags -

docs/tl-manual.pdf : docs/tl-manual.texinfo
	cd docs && $(MAKE) -k

lisp :
	./bootstrap

bin :
	cd tl/bin && $(MAKE) -kf $(MF)
	cd lecho/bin && $(MAKE) -kf $(MF)

opt :
	cd tl/opt && $(MAKE) -kf $(MF) OPT=1
	cd lecho/opt && $(MAKE) -kf $(MF) OPT=1

clean :
	csh -f -c 'rm -rf {tlt,tl,lecho}/{dev,macro,bin,opt} dist src TAGS'
	rm -rf lecho/c/*.tlt tl/c/*.tlt
	cd docs && $(MAKE) clean

lisp-clean :
	csh -f -c 'rm -rf {tlt,tl,lecho}/{dev,macro,bin,opt} dist src'
	rm -rf lecho/c/*.tlt tl/c/*.tlt

tarballs : 
	if [ -d dist ] ; then rm -rf dist ; fi
	if [ ! -d src ] ; then echo Must export an SRC before making distribution ; exit 1 ; fi
	mkdir dist
	cp src/README src/INSTALL src/docs/LICENSE src/VERSION dist
	cd src && tar czvf ../dist/thinlisp-`head -1 VERSION`.tgz *
	cd src && zip -r ../dist/thinlisp-`head -1 VERSION`.zip *
	cd src && bzip2 -cz `find * -type f -print` > ../dist/thinlisp-`head -1 VERSION`.bz2
