RGP is a simple yet flexible Genetic Programming system for the R environment. The system implements
classical untyped tree-based genetic programming as well as more advanced variants including, for
example, strongly typed genetic programming and Pareto genetic programming.

See http://rsymbolic.org for details.

This package contains the following objects:
.
├── .git                 git repository structure
│   └── [...]
├── .gitignore           list of files that git shouldn't track
├── Makefile             the make script for building, packaging and testing RGP
├── README.txt           (this file)
├── bump-version         script to set the RGP version tag
├── codegen              m4 sources for generated code
│   └── [...]
├── doc                  RGP documentation
│   └── [...]
├── examples             RGP examples
│   └── [...]
├── playground           experimental RGP components
│   └── [...]
├── results              results of RGP test experiments
│   └── [...]
├── roxygen-fixup        script to work around a roxygen bug
├── roxygenize           script for generating documentation from R sources
└── skel                 the R CRAN package skeleton
    ├── DESCRIPTION      metadata for the RGP CRAN package
    ├── R                R sources
    ├── demo             R demos
    ├── inst             other RGP components (e.g. unit tests) 
    ├── src              C sources 
    └── vignettes        package vignette "A Friendly Introduction to RGP"
