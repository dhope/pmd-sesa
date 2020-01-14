################################################################################
# From : https://gist.github.com/halpo/1405945
# See also :https://gist.github.com/cboettig/1591001 for more examples
# Description of File:
# Makefile for knitr compiling
# 
################################################################################
all:pdf  # default rule DO NOT EDIT
################################################################################
MAINFILE  := test
RNWFILES  := 
RFILES    := 
TEXFILES  := 
CACHEDIR  := cache
FIGUREDIR := figures
LATEXMK_FLAGS := 
##### Explicit Dependencies #####
################################################################################
RNWTEX = $(RNWFILES:.Rnw=.tex)
ROUTFILES = $(RFILES:.R=.Rout)
RDATAFILES= $(RFILES:.R=.Rdata)
MAINTEX = $(MAINFILE:=.tex)
MAINPDF = $(MAINFILE:=.pdf)
ALLTEX = $(MAINTEX) $(RNWTEX) $(TEXFILES)
PDFLT = pdflatex -synctex=1 -interaction=nonstopmode -shell-escape 
CHAPTERNAME = HopeLankSmithPaquetYdenberg_SESA
#lualatex

# Dependencies
$(RNWTEX): $(RDATAFILES)
$(MAINTEX): $(RNWTEX) $(TEXFILES)
$(MAINPDF): $(MAINTEX) $(ALLTEX) 

.PHONY:pdf tex clean clearcache cleanall
pdf: $(MAINPDF)
tex: $(RDATAFILES) $(ALLTEX) 

$(CACHEDIR):
	mkdir $(CACHEDIR)
	
$(FIGUREDIR):
	mkdir $(FIGUREDIR)

copyfiles:
	cp ../ThesisChapter/* .

%.tex: %.Rnw
	Rscript \
	  -e "library(knitr)" \
	  -e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
	  -e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
	  -e "knitr::knit('$<','$@')"


%.R:%.Rnw
	Rscript -e "Sweave('$^', driver=Rtangle())"

%.Rout:%.R
	R CMD BATCH "$^" "$@"

%.bib: %.tex
	if [ ! -e $*.aux ];\
	then\
		$(PDFLT) $*.tex;\
	fi;
	bibtex $*.aux

%.pdf: %.tex %.bib
	$(PDFLT) $*
	$(PDFLT) $*
	# $(PDFLT) $<
	#latexmk -pdf  $<
	
clean:
	-latexmk -c -quiet $(MAINFILE).tex
	-rm -f $(MAINTEX) $(RNWTEX)
	-rm -rf $(FIGUREDIR)
	-rm *tikzDictionary
	-rm $(MAINPDF)
	
clearcache:
	-rm -rf cache

cleanall: clean clearcache



buildms: 
	Rscript \
	  -e "library(knitr)" \
	  -e "library(tidyverse)" \
	  -e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
	  -e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
	  -e "knitr::knit('$(CHAPTERNAME).Rnw','$(CHAPTERNAME).tex')"
	$(PDFLT) $(CHAPTERNAME)

	bibtex $(CHAPTERNAME).aux
	$(PDFLT) $(CHAPTERNAME)
	# removeandbackup