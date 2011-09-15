# Makefile
# make build script for RGP
#

.SILENT:
.PHONEY: usage help install test check clean package

usage:
	echo "Most important targets:"
	echo ""
	echo " install  - Install the package, writing the output into install.log."
	echo " check    - Run R CMD check on the package."
	echo " test     - Install package and run unit tests."
	echo " help     - Show ALL available targets."

help: usage
	echo " clean    - Clean up package cruft."
	echo " package  - Build source package of last commit."
	echo " m4       - Generate code from m4 macros in codegen/ into skel/."
	echo " roxygen  - Roxygenize skel/ into pkg/."

install: clean roxygen
	echo "Installing package..."
	R CMD INSTALL --no-multiarch pkg > install.log 2>&1
	echo "DONE."

test: install
	echo "Running unit tests..."
	Rscript pkg/inst/unittests/runner.r
	echo "DONE."

check: clean roxygen
	echo "Running R CMD check..."
	R CMD check pkg && rm -fR pkg.Rcheck
	echo "DONE."

m4: skel
	echo "Generating code from m4 macros..."
	m4 codegen/evaluate_language_expression.m4 > skel/src/playground/evaluate_language_expression.h
	echo "DONE."

roxygen: skel
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1
	./roxygen-fixup >> roxygen.log 2>&1
	echo "DONE."

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel/R/*~ skel.Rcheck
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log
	echo "DONE."

package: clean roxygen
	echo "Building package..."
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	rm -fR pkg
	echo "DONE."
