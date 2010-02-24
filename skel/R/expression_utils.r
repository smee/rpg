## expression_utils.r
##   - Utility functions for R expressions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Common higher-order functions for transforming R expressions
##'
##' \code{MapExpressionNodes} transforms an expressionen by walking its tree,
##' replacing every node in the tree with the result of applying a function
##' \code{f}. It is a variant of \code{\link{Map}} for expression trees.
##' TODO
##'
##' @param f The function to apply.
##' @param expr The expression to transformed.
##'
##' @rdname expressionTransformation
##' @export
MapExpressionNodes <- function(f, expr) {
  if (is.call(expr)) {
    oldfunc <- expr[[1]]
    newfunc <- f(oldfunc)
    newcall <- as.call(append(newfunc, Map(function(e) MapExpressionNodes(f, e), rest(expr))))
  } else {
    f(expr)
  }
}
