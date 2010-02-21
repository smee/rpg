## mutation.r
##   - Functions for mutating GP individuals
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Random mutation of functions and expressions
##'
##' \code{mutateFunc} mutates a function \eqn{f} by recursively replacing inner functions in
##'   \eqn{f} with probability \code{mutatefuncprob}.
##' \code{mutateSubtree} mutates a function by recursively replacing inner nodes with
##'   newly grown subtrees of maximum depth \code{maxsubtreedepth}.
##' \code{mutateConst} mutates a function by perturbing each constant \eqn{c} with probability
##'   \code{mutateconstprob} by setting \eqn{c := c + rnorm(1)}.
##'
##' @param func The function to mutate randomly.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param mutatefuncprob The probability of trying to replace an inner function at each node.
##' @param mutatesubtreeprob The probability of replacing a subtree with a newly grown subtree
##'   at each node.
##' @param maxsubtreedepth The maximum depth of newly grown subtrees.
##' @param mutateconstprob The probability of mutating a constant by adding \code{rnorm(1)} to it.
##' @return The randomly mutated function.
##'
##' @rdname expressionMutation
##' @export
mutateFunc <- function(func, funcset, mutatefuncprob = 0.1) {
  mutatefuncexpr <- function(expr, funcset, mutatefuncprob) {
    if (is.call(expr)) {
      oldfunc <- expr[[1]]
      newfunccandidate <- if (runif(1) <= mutatefuncprob) randelt(funcset$all) else oldfunc
      newfunc <- if(arity(newfunccandidate) == arity(oldfunc)) newfunccandidate else oldfunc
      as.call(append(newfunc, Map(function(e) mutatefuncexpr(e, funcset, mutatefuncprob), rest(expr))))
    } else expr
  }
  mutant <- new.function()
  formals(mutant) <- formals(func)
  body(mutant) <- mutatefuncexpr(body(func), funcset, mutatefuncprob)
  mutant
}

##' @rdname expressionMutation
##' @export
mutateSubtree <- function(func, funcset, inset, mutatesubtreeprob = 0.1, maxsubtreedepth = 5) {
  mutatesubtreeexpr <- function(expr, funcset, inset, mutatesubtreeprob, maxsubtreedepth) {
    if (runif(1) <= mutatesubtreeprob) { # replace current node with new random subtree
      randexprGrow(funcset, inset, maxdepth = maxsubtreedepth)
    } else if (is.call(expr)) {
      as.call(append(expr[[1]],
                     Map(function(e) mutatesubtreeexpr(e, funcset, inset, mutatesubtreeprob, maxsubtreedepth),
                         rest(expr))))
    } else expr
  }
  mutant <- new.function()
  formals(mutant) <- formals(func)
  body(mutant) <- mutatesubtreeexpr(body(func), funcset, inset, mutatesubtreeprob, maxsubtreedepth)
  mutant
}

##' @rdname expressionMutation
##' @export
mutateConst <- function(func, mutateconstprob = 0.1) {
  mutateconstexpr <- function(expr, mutateconstprob) {
    if (is.call(expr)) {
      as.call(append(expr[[1]], Map(function(e) mutateconstexpr(e, mutateconstprob), rest(expr))))
    } else if (runif(1) <= mutateconstprob && is.numeric(expr)) {
      expr + rnorm(1)
    } else expr
  }
  mutant <- new.function()
  formals(mutant) <- formals(func)
  body(mutant) <- mutateconstexpr(body(func), mutateconstprob)
  mutant
}
