# Makefile
# make build script for RGP
#

.SILENT:
.PHONEY: usage help install test check clean distclean package

usage:
	echo "Most important targets:"
	echo ""
	echo " install   - Install the package, writing the output into install.log."
	echo " check     - Run R CMD check on the package."
	echo " test      - Install package and run unit tests."
	echo " help      - Show ALL available targets."

help: usage
	echo " clean     - Clean up package cruft."
	echo " distclean - Clean up and remove all generated artifacts."
	echo " package   - Build source package of last commit."
	echo " codegen   - Generate code from m4 macros in codegen/ into skel/."
	echo " roxygen   - Roxygenize skel/ into pkg/."

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

macros:
	echo "Generating code from m4 macros..."
	m4 codegen/evaluate_language_expression.m4 > playground/evaluate_language_expression.h
	echo "DONE."

roxygen:
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1
	./roxygen-fixup >> roxygen.log 2>&1
	echo "DONE."

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel/R/*~ skel.Rcheck rgp.Rcheck
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log
	echo "DONE."

distclean: clean
	echo "Removing all generated artifacts..."
	rm -f rgp_*.tar.gz 
	rm -f playground/evaluate_language_expression.h
	echo "DONE."

package: clean roxygen
	echo "Building package..."
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	rm -fR pkg
	echo "DONE."

# EOF

