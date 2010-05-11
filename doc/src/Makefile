# Makefile
# a simple LaTeX Makefile
#

TEX-MAIN = main
PDF-OUT = ../rgp

all: pdf clean

pdf:
	@echo "PDF: making paper pdf document..." 
	@pdflatex $(TEX-MAIN).tex
	#@bibtex $(TEX-MAIN)
	@pdflatex $(TEX-MAIN).tex
	@pdflatex $(TEX-MAIN).tex
	@mv $(TEX-MAIN).pdf $(PDF-OUT).pdf
	@echo "PDF: DONE."

clean:
	@echo "CLEAN: cleaning temporary files..."
	@rm -f *.aux
	@rm -f *.log
	@rm -f *.out
	@rm -f *.toc
	@rm -f *.dvi
	@rm -f *.lof
	@rm -f *.blg
	@rm -f *.bbl
	@rm -f *~
	@rm -rf auto	
	@echo "CLEAN: DONE."

show:
	open $(PDF-OUT).pdf

force_look:
	true
