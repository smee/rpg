## fast_symbolic_regression.R
##   - Tools for fast (subtree-based) symbolic regression using the R formula interface
##
## RGP - a GP system for R
## 2010-2013 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Fast symbolic regression via untyped standard genetic programming
##'
##' Perform fast subtree-based symbolic regression via untyped genetic programming.
##' The regression task is specified as a \code{\link{formula}}. Only simple formulas
##' without interactions are supported. The result of the symbolic regression run is a
##' symbolic regression model containing an untyped GP population of model
##' functions.
##'
##' @param formula A \code{\link{formula}} describing the regression task. Only
##'   simple formulas of the form \code{response ~ variable1 + ... + variableN}
##'   are supported at this point in time.
##' @param data A \code{\link{data.frame}} containing training data for the
##'   symbolic regression run. The variables in \code{formula} must match
##'   column names in this data frame.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{makeStepsStopCondition} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param individualSizeLimit Individuals with a number of tree nodes that
##'   exceeds this size limit will get a fitness of \code{Inf}.
##' @param functionSet The function set.
##' @param constantInterval A vector containg the minimum and maximum for generated
##'   constants, defaults to \code{c(-10.0, 10.0)}
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param errorMeasure A function to use as an error measure, defaults to SMSE.
##' @param progressMonitor A function of signature
##'   \code{function(population, fitnessValues, fitnessFunction, stepNumber, evaluationNumber,
##'   bestFitness, timeElapsed)} to be called with each evolution step.
##' @param ndsParentSelectionProbability The probability to use non-dominated-sorting-based
##'   parent selection in each generation versus random parent selection.
##' @param verbose Whether to print progress messages.
##' @return An symbolic regression model that contains an untyped GP population.
##'
##' @seealso \code{\link{predict.symbolicRegressionModel}}, \code{\link{geneticProgramming}}
##' @export
fastSymbolicRegression <- function(formula,
                                   data,
                                   functionSet = mathFunctionSet,
                                   constantInterval = c(-10.0, 10.0),
                                   errorMeasure = smse,
                                   stopCondition = makeTimeStopCondition(5),
                                   population = NULL,
                                   populationSize = 100,
                                   individualSizeLimit = 128,
                                   mutationFunction = NULL,
                                   crossoverFunction = NULL,
                                   ndsParentSelectionProbability = 0.0,
                                   verbose = TRUE,
                                   progressMonitor = NULL) {
  # extract fitness cases from formula and data and generate fitness function...
  cleanedData <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else {
    data
  }
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueYs <- eval(responseVariable, envir = data)
  xsList <- lapply(explanatoryVariables, eval, envir = data)
  xs <- unlist(xsList)
  naSafeErrorErrorMeasure <- function(y1, y2) {
    err <- errorMeasure(y1, y2)
    if (is.na(err)) Inf else err
  }
  fitnessFunction <- function(ind) {
    indSubtreeYs <- .Call("eval_vectorized_R", ind, xs, TRUE)
    lapply(indSubtreeYs, function(ys) naSafeErrorErrorMeasure(trueYs, ys))
  }

  # create input variable set from formula...
  variableNames <- attr(terms(formula(formula)), "term.labels")
  inVarSet <- inputVariableSet(list=as.list(variableNames))

  # initialize population if not given as a parameter and calculate initial objective values...
  population <- if (is.null(population)) {
    populationFactory(populationSize, functionSet, inVarSet, 8, constantInterval[1], constantInterval[2])
  } else {
    population
  }
  objectiveValues <- evalPopulation(population, fitnessFunction)

  # initialze progress counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  bestFitness <- NA #min(fitnessValues) # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  # generate result compatible with predict.symbolicRegressionModel...
  symbolicRegressionModel <- NA # TODO
  browser() # TODO
  stop("not implemented") # TODO

  class(symbolicRegressionModel) <- c("symbolicRegressionModel", class(symbolicRegressionModel))
  symbolicRegressionModel  
}


## Tool functions...
populationFactory <- function(populationSize, funSet, inVarSet, maxfuncdepth, constMin, constMax) { 
  Map(function(i) makeClosure(.Call("initialize_expression_grow_R",
                                    as.list(funSet$nameStrings),
                                    as.integer(funSet$arities),
                                    as.list(inVarSet$nameStrings),
                                    constMin, constMax,
                                    1.0, 0.2,
                                    as.integer(maxfuncdepth)),
                                    as.list(inVarSet$nameStrings)), 1:populationSize)
}

evalPopulation <- function(population, fitnessFunction, ageValues = integer(length(population))) {
  fitnesses <- mapply(fitnessFunction, population)
  complexities <- mapply(function(f) .Call("func_visitation_length_R", f, TRUE), population)
  do.call(cbind, mapply(function(subtreeFitnesses, subtreeComplexities, i)
                          mapply(function(subtreeFitness, subtreeComplexity, j)
                                   c(i, j, subtreeFitness, subtreeComplexity, ageValues[i]),
                                 subtreeFitnesses, subtreeComplexities, seq_along(subtreeFitnesses)),
                        fitnesses, complexities, seq_along(fitnesses)))
}

extractIndexedSubtreesFromPopulation <- function(population, evalResults) {
  populationIndices <- evalResults[1,]
  subtreeIndices <- evalResults[2,]
  mapply(function(i, j) {
    subtree <- population[[i]]
    body(subtree) <- .Call("get_sexp_subtree_R", body(subtree), as.integer(j - 1))
    subtree
  },
  evalResults[1,], evalResults[2,])
}
