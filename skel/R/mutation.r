## mutation.r
##   - Functions for mutating GP individuals
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include stypes.r
##' @include breeding.r
NA

##' Random mutation of functions and expressions
##'
##' \code{mutateFunc} mutates a function \eqn{f} by recursively replacing inner functions in
##'   \eqn{f} with probability \code{mutatefuncprob}.
##' \code{mutateSubtree} mutates a function by recursively replacing inner nodes with
##'   newly grown subtrees of maximum depth \code{maxsubtreedepth}.
##' \code{mutateNumericConst} mutates a function by perturbing each numeric constant \eqn{c}
##'   with probability \code{mutateconstprob} by setting \eqn{c := c + rnorm(1)}.
##' \code{mutateFuncTyped}, \code{mutateSubtreeTyped}, and \code{mutateNumericConstTyped} are
##' variants of the above functions that respect sType tags and only create well-typed results.
##'
##' @param func The function to mutate randomly.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param mutatefuncprob The probability of trying to replace an inner function at each node.
##' @param mutatesubtreeprob The probability of replacing a subtree with a newly grown subtree
##'   at each node.
##' @param maxsubtreedepth The maximum depth of newly grown subtrees.
##' @param mutateconstprob The probability of mutating a constant by adding \code{rnorm(1)} to it.
##' @param breedingFitness A breeding function. See the documentation for
##'   \code{\link{geneticProgramming}} for details.
##' @param breedingTries The number of breeding steps.
##' @return The randomly mutated function.
##'
##' @rdname expressionMutation
##' @export
mutateFunc <- function(func, funcset, mutatefuncprob = 0.01,
                       breedingFitness = function(individual) TRUE,
                       breedingTries = 50) {
  mutatefuncexpr <- function(expr, funcset, mutatefuncprob) {
    if (is.call(expr)) {
      oldfunc <- expr[[1]]
      newfunccandidate <- if (runif(1) <= mutatefuncprob)
        toName(randelt(funcset$all, prob = attr(funcset$all, "probabilityWeight")))
      else oldfunc
      newfunc <- if(arity(newfunccandidate) == arity(oldfunc)) newfunccandidate else oldfunc
      as.call(append(newfunc, Map(function(e) mutatefuncexpr(e, funcset, mutatefuncprob), rest(expr))))
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutatefuncexpr(body(func), funcset, mutatefuncprob)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}

##' @rdname expressionMutation
##' @export
mutateSubtree <- function(func, funcset, inset, conset, mutatesubtreeprob = 0.1, maxsubtreedepth = 5,
                          breedingFitness = function(individual) TRUE,
                          breedingTries = 50) {
  mutatesubtreeexpr <- function(expr, funcset, inset, conset, mutatesubtreeprob, maxsubtreedepth) {
    if (runif(1) <= mutatesubtreeprob) { # replace current node with new random subtree
      randexprGrow(funcset, inset, conset, maxdepth = maxsubtreedepth)
    } else if (is.call(expr)) {
      as.call(append(expr[[1]],
                     Map(function(e) mutatesubtreeexpr(e, funcset, inset, conset,
                                                       mutatesubtreeprob, maxsubtreedepth),
                         rest(expr))))
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutatesubtreeexpr(body(func), funcset, inset, conset, mutatesubtreeprob, maxsubtreedepth)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}

##' @rdname expressionMutation
##' @export
mutateNumericConst <- function(func, mutateconstprob = 0.1,
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 50) {
  mutateconstexpr <- function(expr, mutateconstprob) {
    if (is.call(expr)) {
      as.call(append(expr[[1]], Map(function(e) mutateconstexpr(e, mutateconstprob), rest(expr))))
    } else if (runif(1) <= mutateconstprob && is.numeric(expr)) {
      expr + rnorm(1)
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutateconstexpr(body(func), mutateconstprob)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}

##' @rdname expressionMutation
##' @export
mutateFuncTyped <- function(func, funcset, mutatefuncprob = 0.01,
                            breedingFitness = function(individual) TRUE,
                            breedingTries = 50) {
  mutatefuncexprTyped <- function(expr, funcset, mutatefuncprob) {
    if (is.call(expr)) { # only look at calls, this mutation ignores terminal nodes...
      oldfunc <- expr[[1]]
      oldfuncType <- sType(oldfunc)
      oldfuncRangeType <- rangeTypeOfType(oldfuncType)
      ## Select a candidate for a new function of matching range type. This can of course result
      ## in a candidate function with a different domain type. If this happens the mutation is
      ## simply aborted, because searching again for a matching function would cost too much time...
      newfunccandidate <- if (runif(1) <= mutatefuncprob)
        toName(randelt(funcset$byRange[[oldfuncRangeType$string]],
                       prob = attr(funcset$byRange[[oldfuncRangeType$string]], "probabilityWeight")))
      else oldfunc
      newfunccandidateType <- sType(newfunccandidate)
      newfunc <- if(identical(newfunccandidateType, oldfuncType)) newfunccandidate else oldfunc
      newcall <- as.call(append(newfunc, Map(function(e) mutatefuncexprTyped(e, funcset, mutatefuncprob), rest(expr))))
      newcall %::% sType(expr) # tag the mutated expression with the correct type
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutatefuncexprTyped(body(func), funcset, mutatefuncprob)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}

##' @rdname expressionMutation
##' @export
mutateSubtreeTyped <- function(func, funcset, inset, conset, mutatesubtreeprob = 0.1, maxsubtreedepth = 5,
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 50) {
  mutatesubtreeexprTyped <- function(expr, funcset, inset, conset, mutatesubtreeprob, maxsubtreedepth) {
    if (runif(1) <= mutatesubtreeprob) { # replace current node with new random subtree of correct type
      type <- rangeTypeOfType(sType(expr))
      randexprTypedGrow(type, funcset, inset, conset, maxdepth = maxsubtreedepth)
    } else if (is.call(expr)) {
      mutatedExpr <-
        as.call(append(expr[[1]],
                       Map(function(e) mutatesubtreeexprTyped(e, funcset, inset, conset,
                                                              mutatesubtreeprob, maxsubtreedepth),
                           rest(expr))))
      mutatedExpr %::% sType(expr) # tag the mutated expression with the correct type
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutatesubtreeexprTyped(body(func), funcset, inset, conset, mutatesubtreeprob, maxsubtreedepth)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}

##' @rdname expressionMutation
##' @export
mutateNumericConstTyped <- function(func, mutateconstprob = 0.1,
                                    breedingFitness = function(individual) TRUE,
                                    breedingTries = 50) {
  mutateconstexprTyped <- function(expr, mutateconstprob) {
    if (is.call(expr)) {
      mutatedExpr <- as.call(append(expr[[1]], Map(function(e) mutateconstexprTyped(e, mutateconstprob), rest(expr))))
      mutatedExpr %::% sType(expr)
    } else if (runif(1) <= mutateconstprob && is.numeric(expr)) {
      mutatedExpr <- expr + rnorm(1)
      mutatedExpr %::% sType(expr)
    } else expr
  }
  doMutation <- function() {
    mutant <- new.function()
    formals(mutant) <- formals(func)
    body(mutant) <- mutateconstexprTyped(body(func), mutateconstprob)
    mutant
  }
  breed(doMutation, breedingFitness, breedingTries)
}
