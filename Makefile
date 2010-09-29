.SILENT:
.PHONEY: usage help install test check clean package

usage:
	echo "Most important targets:"
	echo ""
	echo " install  - Install the package, writing the output into install.log"
	echo " test     - Install package and run unit tests"
	echo " check    - Run R CMD check on the package"
	echo " help     - Show ALL available targets"

help: usage
	echo " clean    - Clean up package cruft"
	echo " package  - Build source package of last commit"
	echo " roxygen  - Roxygenize skel/ into pkg/"

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
	-git stash save -q
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	-git stash pop -q
	rm -f pkg/ChangeLog
	echo "DONE."
