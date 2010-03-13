.SILENT:
.PHONEY: usage help install test check clean package

usage:
	echo "Available targets:"
	echo ""
	echo " install  - install the package, writing the output into install.log"
	echo " test     - install package and run unit tests"
	echo " check    - run R CMD check on the package"
	echo " help     - shows all available targets"

help: usage
	echo " clean    - clean up package cruft"
	echo " package  - build source package of last commit"
	echo " pkg      - roxygenize skel/ into pkg/"

install: clean pkg
	echo "Installing package..."
	R CMD INSTALL --no-multiarch pkg > install.log 2>&1

test: install
	echo "Running unit tests..."
	Rscript pkg/inst/unittests/runner.r

check: clean pkg
	echo "Running R CMD check..."
	R CMD check pkg && rm -fR pkg.Rcheck

pkg: skel
	echo "Roxygenizing package..."
	./roxygenize > roxygen.log 2>&1

clean:
	echo "Cleaning up..."
	rm -fR skel/src/*.o skel/src/*.so skel.Rcheck
	rm -fR pkg
	rm -fR .RData .Rhistory build.log install.log roxygen.log

package: clean pkg
	echo "Building package..."
	-git stash save -q
	echo "Date: $(date +%Y-%m-%d)" >> pkg/DESCRIPTION
	git log --no-merges -M --date=iso --format=medium pkg/ > pkg/ChangeLog
	R CMD build pkg > build.log 2>&1
	-git stash pop -q
	rm -f pkg/ChangeLog
