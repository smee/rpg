# Makefile
# make build script for RGP
#

.SILENT:
.PHONEY: usage help install test check clean distclean package

usage:
	echo "Makefile for the RGP genetic programming package for R."
	echo "Usage: make TARGET with TARGET being:"
	echo ""
	echo "  check     - Run R CMD check on the package."
	echo "  clean     - Clean up package cruft."
	echo "  distclean - Clean up and remove all generated artifacts."
	echo "  help      - Show this message."
	echo "  install   - Install the package, writing the output into install.log."
	echo "  macros    - Generate code from m4 macros in codegen/ into skel/."
	echo "  package   - Build source package of last commit."
	echo "  roxygen   - Roxygenize skel/ into pkg/."
	echo "  shlibs    - Build shared libraries of C-based components (for interactive testing)."
	echo "  test      - Install package and run unit tests."
	echo "  vignette  - Build the package vignette."

help: usage

install: clean roxygen
	echo "Installing package..."
	R CMD INSTALL --no-multiarch pkg > install.log 2>&1 || cat install.log
	echo "DONE."

test: install
	echo "Running unit tests..."
	Rscript pkg/inst/unittests/runner.r
	echo "DONE."

check: clean roxygen
	echo "Running R CMD check..."
	R CMD check pkg && rm -fR pkg.Rcheck
	echo "DONE."

macros:
	echo "Generating code from m4 macros..."
	m4 codegen/evaluate_language_expression.m4 > playground/evaluate_language_expression.h
	echo "DONE."

shlibs: macros
	echo "Buiding shared libraries of C-based components..."
	R CMD SHLIB playground/evolution.c playground/selection.c playground/mutate_function.c playground/create_expr_tree.c playground/eval_vectorized.c playground/population.c
	echo "DONE."

roxygen:
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1 || cat roxygen.log
	#./roxygen-fixup >> roxygen.log 2>&1
	echo "DONE."

clean:
	echo "Cleaning up..."
	rm -fR pkg
	rm -fR skel.Rcheck rgp.Rcheck
	rm -fR skel/src/*.o skel/src/*.so skel/R/*~
	rm -fR playground/*.o playground/*.so playground/*~
	rm -fR .RData .Rhistory build.log install.log roxygen.log
	rm -f rgp_introduction.aux rgp_introduction.log rgp_introduction.out rgp_introduction.tex texput.log
	rm -f Sweave.sty
	echo "DONE."

distclean: clean
	echo "Removing all generated artifacts..."
	rm -f rgp_*.tar.gz 
	rm -f playground/evaluate_language_expression.h
	rm -f rgp_introduction.pdf
	echo "DONE."

package: clean roxygen
	echo "Building package..."
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	rm -fR pkg
	echo "DONE."

vignette:
	echo "Building package vignette..."
	R CMD sweave --encoding=utf8 skel/vignettes/rgp_introduction.Rnw 
	cp skel/vignettes/Sweave.sty .
	pdflatex rgp_introduction.tex
	pdflatex rgp_introduction.tex
	echo "DONE."

# EOF

