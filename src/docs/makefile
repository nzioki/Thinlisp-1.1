all : tl-manual.pdf tl-manual.dvi

tl-manual.dvi : tl-manual.texinfo makefile
	texi2dvi tl-manual.texinfo

tl-manual.pdf : tl-manual.texinfo makefile
	texi2pdf tl-manual.texinfo
