all : tl-manual.info tl-manual.dvi tl-manual.html tl-manual.pdf

clean : 
	rm -f tl-manual.aux tl-manual.cp tl-manual.cps tl-manual.dvi tl-manual.fn \
   tl-manual.ky tl-manual.log tl-manual.pdf tl-manual.pg tl-manual.toc \
   tl-manual.tp tl-manual.vr tl-manual.inf* tl-manual.html \
   tl-manual_foot.html tl-manual.ps

tl-manual.info : tl-manual.texinfo makefile
	makeinfo tl-manual.texinfo

tl-manual.dvi : tl-manual.texinfo makefile
	texi2dvi tl-manual.texinfo

tl-manual.html : tl-manual.texinfo makefile
	texi2html tl-manual.texinfo

tl-manual.ps : tl-manual.dvi makefile
	dvips -o tl-manual.ps tl-manual.dvi 

tl-manual.pdf : tl-manual.ps makefile
	ps2pdf tl-manual.ps tl-manual.pdf
