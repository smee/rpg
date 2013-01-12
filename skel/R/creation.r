## creation.R
##   - Functions for creating new GP individuals (individual initialization)
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Creates an R expression by random growth
##'
##' Creates a random R expression by randomly growing its tree. In each step of growth,
##' with probability \code{subtreeprob}, an operator is chosen from the function set \code{funcset}.
##' The operands are then generated by recursive calls. If no subtree is generated, a constant will
##' be generated with probability \code{constprob}. If no constant is generated, an input variable
##' will be chosen randomly. The depth of the resulting expression trees can be bounded by the
##' \code{maxdepth} parameter.
##' \code{randexprFull} creates a random full expression tree of depth \code{maxdepth}. The algorithm
##' is the same as \code{randexprGrow}, with the exception that the probability of generating
##' a subtree is fixed to 1  until the desired tree depth \code{maxdepth} is reached.
##'
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxdepth The maximum expression tree depth.
##' @param constprob The probability of generating a constant in a step of growth, if no subtree
##'   is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
##'   will be generated. Defaults to \code{0.2}.
##' @param subtreeprob The probability of generating a subtree in a step of growth.
##' @param curdepth (internal) The depth of the random expression currently generated, used internally
##'   in recursive calls.
##' @return A new R expression generated by random growth.
##' @rdname randomExpressionCreation
##' @export
randexprGrow <- function(funcset, inset, conset,
                         maxdepth = 8,
                         constprob = 0.2, subtreeprob = 0.5,
                         curdepth = 1) {
  constprob <- if (is.empty(conset$all)) 0.0 else constprob
  if (runif(1) <= subtreeprob && curdepth < maxdepth) { # maybe create subtree if maximum depth not reached
    randfuncindex <- sample.int(n = length(funcset$all), size = 1, prob = attr(funcset$all, "probabilityWeight"))
    funcname <- toName(funcset$all[[randfuncindex]])
    funcarity <- funcset$arities[randfuncindex]
    as.call(append(funcname,
                   lapply(1:funcarity, function(i) randexprGrow(funcset, inset, conset, maxdepth,
                                                                constprob, subtreeprob, curdepth + 1))))
  } else { # create terminal
    if (runif(1) <= constprob) { # create constant
      constfactory <- randelt(conset$all, prob = attr(conset$all, "probabilityWeight"))
      constfactory()
    } else { # create input variable
      toName(randelt(inset$all, prob = attr(inset$all, "probabilityWeight")))
    }
  }
}

##' @rdname randomExpressionCreation
##' @export
randexprFull <- function(funcset, inset, conset,
                         maxdepth = 8,
                         constprob = 0.2) {
  randexprGrow(funcset, inset, conset, maxdepth, constprob, 1.0)
}

##' Creates an R function with a random expression as its body
##'
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxdepth The maximum expression tree depth.
##' @param exprfactory The function to use for randomly creating the function's body.
##' @param constprob The probability of generating a constant in a step of growth, if no subtree
##'   is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
##'   will be generated. Defaults to \code{0.2}.
##' @param breedingFitness A breeding function. See the documentation for
##'   \code{\link{geneticProgramming}} for details.
##' @param breedingTries The number of breeding steps.
##' @return A randomly generated R function.
##' @rdname randomFunctionCreation
##' @export
randfunc <- function(funcset, inset, conset, maxdepth = 8,
                     constprob = 0.2, exprfactory = randexprGrow,
                     breedingFitness = function(individual) TRUE,
                     breedingTries = 50) {
  funcFactory <- function() {
    newf <- new.function(envir = funcset$envir)
    formals(newf) <- new.alist(inset$allFormals)
    body(newf) <- exprfactory(funcset, inset, conset, maxdepth, constprob = constprob)
    newf
  }
  breed(funcFactory, breedingFitness, breedingTries)
}

##' @rdname randomFunctionCreation
##' @export
randfuncRampedHalfAndHalf <- function(funcset, inset, conset, maxdepth = 8, constprob = 0.2,
                                      breedingFitness = function(individual) TRUE,
                                      breedingTries = 50) {
  if (runif(1) > 0.5)
    randfunc(funcset, inset, conset, maxdepth, exprfactory = randexprFull, constprob = constprob,
             breedingFitness = breedingFitness, breedingTries = breedingTries)
  else
    randfunc(funcset, inset, conset, maxdepth, exprfactory = randexprGrow, constprob = constprob,
             breedingFitness = breedingFitness, breedingTries = breedingTries)
}

##' Creates an R expression by random growth respecting type constraints
##'
##' Creates a random R expression by randomly growing its tree. In each step of growth,
##' with probability \code{subtreeprob}, an operator is chosen from the function set \code{funcset}.
##' The operands are then generated by recursive calls. If no function of matching range type exists,
##' a terminal (constant or input variable) will be generated instead. If no subtree is generated, a
##' constant will be generated with probability \code{constprob}. If no constant is generated, an input
##' variable will be chosen randomly. The depth of the resulting expression trees can be bounded by the
##' \code{maxdepth} parameter.
##' In contrast to \code{randexprGrow}, this function respects sType tags of functions, input variables,
##' and constant factories. Only well-typed expressions are created.
##' \code{randexprTypedFull} creates a random full expression tree of depth \code{maxdepth},
##' respecting type constraints.
##' All nodes in the created expressions will be tagged with their sTypes.
##'
##' @param type The (range) type the created expression should have.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxdepth The maximum expression tree depth.
##' @param constprob The probability of generating a constant in a step of growth, if no subtree
##'   is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
##'   will be generated. Defaults to \code{0.2}.
##' @param subtreeprob The probability of generating a subtree in a step of growth.
##' @param curdepth (internal) The depth of the random expression currently generated, used internally
##'   in recursive calls.
##' @return A new R expression generated by random growth.
##' @rdname randomExpressionCreationTyped
##' @export
randexprTypedGrow <- function(type, funcset, inset, conset,
                              maxdepth = 8,
                              constprob = 0.2, subtreeprob = 0.5,
                              curdepth = 1) {
  if (is.null(type)) stop("randexprTypedGrow: Type must not be NULL.")
  constprob <- if (is.empty(conset$all)) 0.0 else constprob
  typeString <- type$string
  insetTypes <- Map(sType, inset$all)
  if (curdepth < maxdepth && runif(1) <= subtreeprob) { # maybe create subtree of correct type if maximum depth is not reached
    funcname <- toName(randelt(funcset$byRange[[typeString]], prob = attr(funcset$byRange[[typeString]], "probabilityWeight")))
    if (!is.null(funcname)) { # a function of correct range type has been found...
      functype <- sType(funcname)
      funcdomaintypes <- functype$domain
      newSubtree <-
        as.call(append(funcname,
                       Map(function(domaintype) randexprTypedGrow(domaintype, funcset, inset, conset, maxdepth,
                                                                  constprob, subtreeprob, curdepth + 1),
                           funcdomaintypes)))
    ## the type of the generated subtree is a function type with the input variable types as domain types...
    newSubtreeType <- insetTypes %->% type
    return(newSubtree %::% newSubtreeType)
    }
  }
  # else, create a terminal node of correct type
  return(randterminalTyped(typeString, inset, conset, constprob) %::% type)
}

##' @rdname randomExpressionCreationTyped
##' @export
randexprTypedFull <- function(type, funcset, inset, conset,
                              maxdepth = 8,
                              constprob = 0.2) {
  randexprTypedGrow(type, funcset, inset, conset, maxdepth, constprob, 1.0)
}

##' Creates a well-typed R function with a random expression as its body
##'
##' @param type The range type of the random function to create.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxdepth The maximum expression tree depth.
##' @param constprob The probability of generating a constant in a step of growth, if no subtree
##'   is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
##'   will be generated. Defaults to \code{0.2}.
##' @param exprfactory The function to use for randomly creating the function's body.
##' @param breedingFitness A breeding function. See the documentation for
##'   \code{\link{geneticProgramming}} for details.
##' @param breedingTries The number of breeding steps.
##' @return A randomly generated well-typed R function.
##' @rdname randomFunctionCreationTyped
##' @export
randfuncTyped <- function(type, funcset, inset, conset, maxdepth = 8,
                          constprob = 0.2, exprfactory = randexprTypedGrow,
                          breedingFitness = function(individual) TRUE,
                          breedingTries = 50) {
  funcFactory <- function() {
    newf <- new.function(envir = funcset$envir)
    formals(newf) <- new.alist(inset$allFormals)
    body(newf) <- exprfactory(type, funcset, inset, conset, maxdepth, constprob = constprob)
    newf
  }
  breed(funcFactory, breedingFitness, breedingTries)
}

##' @rdname randomFunctionCreationTyped
##' @export
randfuncTypedRampedHalfAndHalf <- function(type, funcset, inset, conset, maxdepth = 8, constprob = 0.2,
                                           breedingFitness = function(individual) TRUE,
                                           breedingTries = 50) {
  if (runif(1) > 0.5)
    randfuncTyped(type, funcset, inset, conset, maxdepth, exprfactory = randexprTypedFull, constprob = constprob,
                  breedingFitness = breedingFitness, breedingTries = breedingTries)
  else
    randfuncTyped(type, funcset, inset, conset, maxdepth, exprfactory = randexprTypedGrow, constprob = constprob,
                  breedingFitness = breedingFitness, breedingTries = breedingTries)
}

##' Create a random terminal node
##'
##' @param typeString The string label of the type of the random terminal node to create.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param constprob The probability of creating a constant versus an input variable.
##' @return A random terminal node, i.e. an input variable or a constant.
randterminalTyped <- function(typeString, inset, conset, constprob) {
  if (runif(1) <= constprob) { # create constant of correct type
    constfactory <- randelt(conset$byRange[[typeString]], prob = attr(conset$byRange[[typeString]], "probabilityWeight"))
    if (is.null(constfactory)) stop("randterminalTyped: Could not find a constant factory for type ", typeString, ".")
    constfactory()
  } else { # create input variable of correct type
    invar <- toName(randelt(inset$byRange[[typeString]], prob = attr(inset$byRange[[typeString]], "probabilityWeight")))
    if (is.null(invar)) { # there are no input variables of the requested type, try to create a contant instead
      constfactory <- randelt(conset$byRange[[typeString]], prob = attr(conset$byRange[[typeString]], "probabilityWeight"))
      if (is.null(constfactory)) stop("randterminalTyped: Could not find an input variable or constant factory for type ", typeString, ".")
      constfactory()
    } else {
      invar
    }
  }
}
