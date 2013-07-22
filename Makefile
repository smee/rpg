# Makefile
# make build script for RGP
#

.SILENT:
.PHONEY: usage help install test check clean distclean package

usage:
	echo "Makefile for the RGP genetic programming package for R."
	echo "Usage: make TARGET with TARGET being:"
	echo ""
	echo "  check     - Run CRAN check on the package."
	echo "  clean     - Clean up package cruft."
	echo "  distclean - Clean up and remove all generated artifacts."
	echo "  help      - Show this message."
	echo "  install   - Install the package, writing the output into install.log."
	echo "  macros    - Generate code from m4 macros in codegen/ into skel/."
	echo "  package   - Build source package of last commit."
	echo "  roxygen   - Roxygenize skel/ into pkg/."
	echo "  shlibs    - Build shared libraries of C-based components (for interactive testing)."
	echo "  test      - Install package and run unit tests."

help: usage

install: clean roxygen
	echo "Installing package..."
	"$(R_HOME)/bin/R" CMD INSTALL --no-multiarch pkg > install.log 2>&1 || cat install.log
	echo "DONE."

test: install
	echo "Running unit tests..."
	"$(R_HOME)/bin/Rscript" pkg/inst/unittests/runner.r
	echo "DONE."

check: clean roxygen
	echo "Running CRAN check..."
	"$(R_HOME)/bin/R" CMD check --as-cran pkg && rm -fR pkg.Rcheck
	echo "DONE."

macros:
	echo "Generating code from m4 macros..."
	m4 codegen/evaluate_language_expression.m4 > skel/src/evaluate_language_expression.h
	echo "DONE."

# TODO production C-code now lives in skel/src
shlibs: macros
	echo "Buiding shared libraries of C-based components..."
	"$(R_HOME)/bin/R" CMD SHLIB playground/evolution.c playground/selection.c playground/mutate_function.c playground/create_expr_tree.c playground/population.c
	echo "DONE."

roxygen: macros
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1 || cat roxygen.log
	echo "DONE."

clean:
	echo "Cleaning up..."
	rm -fR pkg
	rm -fR skel.Rcheck rgp.Rcheck
	rm -fR skel/src/*.o skel/src/*.so skel/R/*~
	rm -fR playground/*.o playground/*.so playground/*~
	rm -fR .RData .Rhistory build.log install.log roxygen.log
	echo "DONE."

distclean: clean
	echo "Removing all generated artifacts..."
	rm -f rgp_*.tar.gz 
	rm -f skel/src/evaluate_language_expression.h
	echo "DONE."

package: clean roxygen
	echo "Building package..."
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	"$(R_HOME)/bin/R" CMD build pkg > build.log 2>&1
	rm -fR pkg
	echo "DONE."

# EOF

