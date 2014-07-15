REPOS=/var/www/html/R/
SRCREPOS=src/contrib
WINREPOS=bin/windows/contrib/3.1
DOCREPOS=doc/packages
ARCHIVE=./dists
INSTALL=install
INSTALL_DIR=./library

RM = rm -f
CP = cp
TOUCH = touch
REXE = R --vanilla
RCMD = $(REXE) CMD
RSCRIPT = Rscript --vanilla

PDFLATEX = pdflatex
BIBTEX = bibtex
MAKEIDX = makeindex

default:

.PHONY:

%.dist %.manual %.vignettes: export R_QPDF=qpdf
%.dist %.manual %.vignettes: export R_GSCMD=gs
%.dist %.manual %.vignettes: export GS_QUALITY=ebook
%.dist %.manual %.vignettes: export R_HOME=$(shell $(REXE) RHOME)
%.check %.crancheck: export _R_CHECK_ALL_NON_ISO_C_=TRUE
%.check %.crancheck: export _R_CHECK_WALL_FORTRAN_=TRUE
%.check %.xcheck: export POMP_FULL_TESTS=yes
%.vignettes %.data: export R_LIBS=$(CURDIR)/library
%.bin %.check %.qcheck %.qqcheck %.xcheck %.crancheck %.upload %.publish %.clean %.install %.win: PKG = $*_$(shell perl -ne 'print $$1 if /Version:\s+(\d+\.\d+-\d+)/;' $*/DESCRIPTION)

%.vignettes: %.install
	cd $*/vignettes; make 

%.data: %.install
	cd $*/inst/data-R; make

%.win: %.dist
	ncftpput -v win-builder.r-project.org R-release $(PKG).tar.gz
	$(TOUCH) $@

%.upload: %.dist
	ncftpput -v cran.r-project.org incoming $(PKG).tar.gz
	$(TOUCH) $@

%.crandist: %.cransrc
	$(RCMD) build --resave-data --compact-vignettes --md5 cran/$*
	$(TOUCH) $@

%.crancheck: %.crandist
	mkdir -p check
	$(RCMD) check --as-cran --library=check $(PKG).tar.gz
	$(TOUCH) $@

%.cransrc:
	$(RM) -r cran/$*
	mkdir -p cran/$*
	(cd $*;	git archive --format=tar master . ) | (cd cran/$*; tar -xvf - )
	$(RM) -r cran/$*/tests 
	$(TOUCH) $@

%.xcheck: %.dist
	$(RCMD) check --use-gct --use-valgrind --timings --library=check $(PKG).tar.gz
	$(TOUCH) $@

%.check: %.dist
	mkdir -p check
	$(RCMD) check --library=check $(PKG).tar.gz
	$(TOUCH) $@

%.qcheck: %.dist
	mkdir -p check
	$(RCMD) check --library=check --no-vignettes --no-tests $(PKG).tar.gz
	$(TOUCH) $@

%.qqcheck: %.dist
	mkdir -p check
	$(RCMD) check --library=check --no-codoc --no-examples --no-vignettes --no-manual --no-tests $(PKG).tar.gz
	$(TOUCH) $@

%.manual:
	$(RM) $*.pdf
	$(RCMD) Rd2pdf --no-preview --pdf $*
	$(RSCRIPT) -e "tools::compactPDF(\"$*.pdf\")";
	$(TOUCH) $@

%.included_manual: %.manual
	$(CP) $*.pdf $*/inst/doc/manual.pdf
	$(TOUCH) $@

%.dist: %.manual
	-$(RCMD) Rdconv -t txt $*/inst/NEWS.Rd -o $*/inst/NEWS
	$(RCMD) build --force --no-manual --resave-data --compact-vignettes=both $*
	$(TOUCH) $@

%.bin: %.dist
	$(RCMD) build --force --binary $(PKG).tar.gz
	$(TOUCH) $@

%.clean: clean
	$(RM) $*.dist $*.check $*.qcheck $*.qqcheck $*.crancheck $*.manual $*.cransrc
	$(RM) -r $*.Rcheck check/$* cran/$*
	$(RM) $*/src/*.o $*/src/*.so $*/src/symbols.rds $*/vignettes/Rplots.*
	$(RM) $(PKG).tar.gz $(PKG).zip $*.pdf

%.distclean: %.clean
	$(RM) $*.win $*.upload $*.bin $*.changelog $*.crandist $*.included_manual

%.install: %.dist
	mkdir -p $(INSTALL_DIR)
	$(RCMD) INSTALL --library=$(INSTALL_DIR) $(PKG).tar.gz
	$(TOUCH) $@

%.remove:
	$(RCMD) REMOVE --library=$(INSTALL_DIR) $*
	$(RM) $*.install

%.publish: %.dist %.manual
	mkdir -m0755 -p $(REPOS)/$(WINREPOS)
	mkdir -m0755 -p $(REPOS)/$(SRCREPOS)
	mkdir -m0755 -p $(REPOS)/$(DOCREPOS)
	(cd $(REPOS)/$(WINREPOS); $(RM) $*_*.zip)
	(cd $(REPOS)/$(SRCREPOS); $(RM) $*_*.tar.gz)
	(cd $(REPOS)/$(DOCREPOS); $(RM) $*.pdf)
	-$(INSTALL) -m0644 $(PKG).zip $(REPOS)/$(WINREPOS)
	-$(INSTALL) -m0644 $(PKG).tar.gz $(REPOS)/$(SRCREPOS)
	-$(INSTALL) -m0644 $*.pdf $(REPOS)/$(DOCREPOS)
	$(RSCRIPT) pkgindex.R winrepos="\"$(REPOS)/$(WINREPOS)\"" srcrepos="\"$(REPOS)/$(SRCREPOS)\""
	-$(INSTALL) -m0644 $(PKG).zip $(ARCHIVE)
	-$(INSTALL) -m0644 $(PKG).tar.gz $(ARCHIVE)
	-$(INSTALL) -m0644 $*.pdf $(ARCHIVE)
	$(RSCRIPT) pkgindex.R

%.tex: %.Rnw
	$(RCMD) Sweave $*

%.R: %.Rnw
	$(RCMD) Stangle $*

%.pdf: %.tex
	$(PDFLATEX) $*
	-$(BIBTEX) $*
	$(PDFLATEX) $*
	$(PDFLATEX) $*

%.bbl: %.tex
	-$(PDFLATEX) $*
	$(BIBTEX) $*

%.idx: %.tex
	-$(PDFLATEX) $*

%.ind: %.idx
	$(MAKEIDX) $*

clean:
	$(RM) *.o *.so *.tex *.log *.aux *.out *.nav *.snm *.toc *-???.pdf Rplots.ps Rplots.pdf

.SECONDARY:
