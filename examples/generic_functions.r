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
##' TODO add possibility to add methods directly in the generic definition
generic <- function(defaultMethodFunction = no.default.method) {
  genericFunction <- new.function()
  formals(genericFunction) <- formals(defaultMethodFunction)
  body(genericFunction) <- body(function(...) {
    methods <- attr(sys.function(), "methods")
    for (method in methods) {
	  if (eval(body(method$predicateFunction))) { # method predicate is true
		return(eval(body(method$methodFunction)))
	  }
	}
	stop("generic: no matching method") # this should never happen
  })
  class(genericFunction) <- c("generic", "function")
  defaultMethod <- list(predicateFunction = function(...) TRUE, methodFunction = defaultMethodFunction)
  attr(genericFunction, "methods") <- list(defaultMethod)
  genericFunction
}

no.default.method <- function(...)
  stop("no applicable method for arguments (", paste(..., sep = ","), ")")

add.method <- function(generic, predicate, methodFunction) {
  if (!inherits(generic, "generic")) stop("add.method: first argument must be a generic")
  if (!inherits(methodFunction, "function")) stop("add.method: third argument must be a function")
  #if (formals(generic) != formals(methodFunction)) stop("add.method: formal parameters of methodFunction must match the formal parameters of the generic")
  predicateExpression <- substitute(predicate)
  predicateFunction <- new.function()
  formals(predicateFunction) <- formals(generic)
  body(predicateFunction) <- predicateExpression
  # TODO add method precedence inference via the predicate implication relation
  # TODO bug?: this leaves the attributes of generic unchanged, because generic is a copy!!!
  method <- list(predicateFunction = predicateFunction, methodFunction = methodFunction)
  attr(generic, "methods") <- c(list(method), # second "list" because c() flattens its arguments
                                attr(generic, "methods"))
  generic
}

list.methods <- NULL

dispatch.generic <- function(generic, ...) {
  NULL
}
