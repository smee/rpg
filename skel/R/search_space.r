## search_space.r
##   - Functions for defining the search space for symbolic regression
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Functions for defining the search space for symbolic regression
##'
##' The GP search space is defined by a set of functions, a set of input variables,
##' a set of constant constructor functions, and some rules how these functions,
##' input variables, and constants may be combined to form valid symbolic expressions.
##' The function set is simply given as a set of strings naming functions in the
##' global environment. Input variables are also given as strings.
##' Combination rules are implemented by a type system and defined by assigning sTypes
##' to functions, input variables, and constant constructors.
##'
##' Function sets and input variable sets are S3 classes containing the following
##' fields:
##' \code{$all} contains a list of all functions, or input variables, or constant
##' factories.
##' \code{$byRange} contains a table of all input variables, or functions, or
##' constant factories, indexed by the string label of their sTypes for input
##' variables, or by the string label of their range sTypes for functions, or by
##' the string label of their range sTypes for constant factories. This field
##' exists mainly for quickly finding a function, input variable, or constant
##' factory that matches a given type.
##' 
##' Multiple function sets, or multiple input variable sets, or multiple constant
##' factory sets can be combined using the \code{\link{c}} function.
##' \code{functionSet} creates a function set.
##' \code{inputVariableSet} creates an input variable set.
##' \code{constantFactorySet} creates a constant factory set.
##' \code{functionSetFromList}, code{inputVariableSetFromList}, and
##' \code{constantConstructorSetFromList } are variants of the above functions
##' that take a list as their only parameter instead of arbitrary many parameters.
##'
##' @param ... Names of functions or input variables given as strings.
##' @return A function set or input variable set.
##'
##' @examples{
##' # creating an untyped search space description...
##' functionSet("+", "-", "*", "/", "expt", "log", "sin", "cos", "tan")
##' inputVariableSet("x", "y")
##' constantFactorySet(function() runif(1, -1, 1))
##' }
##' @rdname searchSpaceDefinition
##' @export

functionSet <- function(..., list=NULL) {
  ll <- if (missing(list)) list(...) else c(list, ...)
  
  funcset <- list()
  class(funcset) <- c("functionSet", "list")
  ## funcset$all <- Map(function(o) as.name(o) %::% sType(o), list) # convert to names, keeping sTypes
  funcset$all <- lapply(ll, function(o) as.name(o) %::% sType(o))
  funcset$byRange <- sortByRange(funcset$all)
  funcset
}

##' @rdname searchSpaceDefinition
##' @export
inputVariableSet <- function(...) inputVariableSetFromList(list(...))

##' @rdname searchSpaceDefinition
##' @export
constantFactorySet <- function(...) constantFactorySetFromList(list(...))

##' @rdname searchSpaceDefinition
##' @export
inputVariableSetFromList <- function(l) {
  inset <- list()
  inset$all <- Map(function(o) as.name(o) %::% sType(o), l) # convert to names, keeping sTypes
  inset$byRange <- sortByRange(inset$all)
  class(inset) <- c("inputVariableSet", "list")
  inset
}

##' @rdname searchSpaceDefinition
##' @export
constantFactorySetFromList <- function(l) {
  constfacset <- list()
  constfacset$all <- l
  constfacset$byRange <- sortByRange(constfacset$all)
  class(constfacset) <- c("constantFactorySet", "list")
  constfacset
}

##' @rdname searchSpaceDefinition
##' @export
c.functionSet <- function(..., recursive = FALSE) {
  fSets <- list(...)
  combinedFsets <- list()
  for (fSet in fSets) combinedFsets <- append(fSet$all, combinedFsets)
  functionSetFromList(combinedFsets)
}

##' @rdname searchSpaceDefinition
##' @export
c.inputVariableSet <- function(..., recursive = FALSE) {
  iSets <- list(...)
  combinedIsets <- list()
  for (iSet in iSets) combinedIsets <- append(iSet$all, combinedIsets)
  inputVariableSetFromList(combinedIsets)
}

##' @rdname searchSpaceDefinition
##' @export
c.constantFactorySet <- function(..., recursive = FALSE) {
  cSets <- list(...)
  combinedCsets <- list()
  for (cSet in cSets) combinedCsets <- append(cSet$all, combinedCsets)
  constantFactorySetFromList(combinedCsets)
}

##' Tabulate a list of functions or input variables by their range sTypes
##'
##' @param x A list of functions or input variables to sort by range sType.
##' @return A table of the objects keyed by their range types.
sortByRange <- function(x) {
  byRangeTable <- list()
  for (o in x) {
    if (hasStype(o)) {
      oStype <- sType(o)
      oStypeRange <- if (is(oStype, "sFunctionType")) oStype$range else oStype
      if (is.null(byRangeTable[[oStypeRange$string]])) byRangeTable[[oStypeRange$string]] <- list()
      byRangeTable[[oStypeRange$string]] <- append(byRangeTable[[oStypeRange$string]], list(o))
    }
  }
  byRangeTable
}
