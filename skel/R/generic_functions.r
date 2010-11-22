## generic_functions.r
##   - A generic function framework with predicate dispatch for R
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include function_utils.r
NA

##' Generic functions with predicate dispatch
##'
##' TODO
generic <- function(defaultMethod = no.default.method) {
  generic <- new.function()
  formals(generic) <- formals(defaultMethod)
  body(generic) <- body(function(...) {
    print(paste("methods:", attr(generic, "methods"))) # TODO
  })
  class(generic) <- c("generic", "function")
  attr(generic, "methods") <- list(list(true.predicate, defaultMethod))
  generic
}

no.default.method <- function(...)
  stop("no applicable method for arguments (", paste(..., sep = ","), ")")

add.method <- function(generic, predicate, methodFunction) {
  if (!inherits(generic, "generic")) stop("add.method: first argument must be a generic")
  if (!inherits(generic, "predicate")) stop("add.method: second argument must be a predicate")
  if (!inherits(generic, "function")) stop("add.method: third argument must be a function")
  # TODO add method precedence inference via the predicate implication relation
  attr(generic, "methods") <- c(list(predicate, methodFunction), attr(generic, "methods"))
  generic
}

list.methods <- NULL

dispatch.generic <- function(generic, ...) {
  NULL
}

predicate <- function(predicateFunction, predicateClass = NA) {
  if (!inherits(predicateFunction, "function")) stop("predicate: first argument must be a function")
  predicate <- predicateFunction
  if (is.na(predicateClass))
    class(predicate) <- c("predicate", "function")
  else
    class(predicate) <- c(predicateClass, "predicate", "function")
  predicate
}

true.predicate <- predicate(function(...) TRUE, "true.predicate")
false.predicate <- predicate(function(...) FALSE, "false.predicate")

# TODO add and.predicate, or.predicate, not.predicate
