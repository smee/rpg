## evolution.r
##   - Functions defining typical evolution main loops,
##     some typical GP function- and constant sets
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##


##' Symbolic regression via untyped Genetic Programming
##'
##' Perform symbolic regression via untyped Genetic Programming. The regression
##' task is specified as a \code{\link{formula}}. Only the simple formulas
##' without interactions are supported at this time. The result of the symbolic
##' regression run is an untyped GP population of model functions.
##'
##' @param formula A \code{\link{formula}} describing the regression task. Only
##'   simple formulas of the form \code{response ~ variable1 + ... + variableN}
##'   are supported at this point in time.
##' @param data A \code{\link{data.frame}} containing training data for the
##'   symbolic regression run. The variables in \code{formula} must match
##'   column names in this data frame.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{evolutionStopConditions} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param individualSizeLimit Individuals with a number of tree nodes that
##'   exceeds this size limit will get a fitness of \code{Inf}.
##' @param functionSet The function set.
##' @param constantSet The set of constant factory functions.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param progressMonitor A function of signature
##'   \code{function(population, stepNumber, timeElapsed)} to be called
##'   with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return
##'
##' @export
symbolicRegression <- function(formula, data,
                               stopCondition = makeStepsStopCondition(1000),
                               population = NULL,
                               populationSize = 500,
                               individualSizeLimit = 64,
                               functionSet = mathFunctionSet,
                               constantSet = numericConstantSet,
                               crossoverFunction = crossover,
                               mutationFunction = NULL,
                               progressMonitor = NULL,
                               verbose = TRUE) {
  progmon <-
    if (is.null(progressMonitor) && verbose)
      function(pop, stepNumber, timeElapsed)
        if (stepNumber %% 100 == 0) message(sprintf("evolution step %i, time elapsed: %f seconds", stepNumber, timeElapsed))
    else if (is.null(progressMonitor))
      function(pop, stepNumber, timeElapsed) NULL # verbose == FALSE, do not show progress
    else
      progressMonitor
  inputVariables <- do.call(inputVariableSet, as.list(as.character(attr(terms(formula), "variables")[-2])[-1]))
  mutatefunc <-
    if (is.null(mutationFunction))
      function(ind) mutateSubtree(mutateNumericConst(ind),
                                  functionSet, inputVariables, constantSet, mutatesubtreeprob = 0.01)
    else
      mutationFunction
  fitnessFunction <- makeRegressionFitnessFunction(formula, data, errormeasure = rmse, indsizelimit = individualSizeLimit)
  pop <-
    if (is.null(population))
      makePopulation(populationSize, functionSet, inputVariables, constantSet)
    else
      population
  stepNumber <- 1
  startTime <- proc.time()["elapsed"]
  timeElapsed <- 0

  if (verbose) message("STARTING symbolic regression evolution run...")
  while (!stopCondition(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)) {
    pop <- tournamentselectionstep(pop, fitnessFunction, functionSet, inputVariables, constantSet,
                                   crossoverfunc = crossoverFunction, mutatefunc = mutatefunc)
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- 1 + stepNumber
    progmon(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)
  }
  if (verbose) message("Symbolic regression evolution run FINISHED.")
  
  pop
}

##' Evolution stop conditions
##'
##' Evolution stop conditions are predicates (functions that return a single logical value)
##' of the signature \code{function(population, stepNumber, timeElapsed)}. They are used
##' to decide when to finish a GP evolution run. Stop conditions must be members of the
##' S3 class \code{c("stopCondition", "function")}. They can be combined using the generic
##' concatenate function \code{\link{c}}, whereas a combined stop condition is fulfilled
##' if any of the constituent stop conditions is fulfilled.
##'
##' \code{makeStepsStopCondition} creates a stop condition that is fulfilled if the number
##' of evolution steps exceeds a given limit.
##' \code{makeTimeStopCondition} creates a stop condition that is fulfilled if the run time
##' (in seconds) of an evolution run exceeds a given limit.
##'
##' @param stepLimit The maximum number of evolution steps for \code{makeStepsStopCondition}.
##' @param timeLimit The maximum runtime in seconds for \code{makeTimeStopCondition}.
##'
##' @rdname evolutionStopConditions
##' @export
makeStepsStopCondition <- function(stepLimit) {
  stopCondition <- function(pop, stepNumber, timeElapsed) stepNumber >= stepLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export
makeTimeStopCondition <- function(timeLimit) {
  stopCondition <- function(pop, stepNumber, timeElapsed) timeElapsed >= timeLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export
c.stopCondition <- function(..., recursive = FALSE) {
  stopConditions <- list(...)
  stopCondition <- function(pop, stepNumber, timeElapsed) {
    results <- rep(FALSE, length(stopConditions))
    idx <- 1
    for (cond in stopConditions) {
      results[idx] <- cond(pop, stepNumber, timeElapsed)
      idx <- 1 + idx
    }
    any(results)
  }
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' Default function- and constant factory sets for Genetic Programming
##'
##' \code{arithmeticFunctionSet} is an untyped function set containing the functions
##' "+", "-", "*", and "/".
##' \code{expLogFunctionSet} is an untyped function set containing the functions
##' "sqrt", "exp", and "ln".
##' \code{trigonometricFunctionSet} is an untyped function set containing the functions
##' "sin", "cos", and "tan".
##' \code{mathFunctionSet} is an untyped function set containing all the above functions.
##'
##' \code{numericConstantSet} is an untyped constant factory set containing a single
##' constant factory that creates numeric constants via calls to \code{runif(1, -1, 1)}.
##'
##' @rdname defaultGPFunctionAndConstantSets
##' @export
arithmeticFunctionSet <- functionSet("+", "-", "*", "/")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
expLogFunctionSet <- functionSet("sqrt", "exp", "ln")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
trigonometricFunctionSet <- functionSet("sin", "cos", "tan")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
mathFunctionSet <- c(arithmeticFunctionSet, expLogFunctionSet, trigonometricFunctionSet)

##' @rdname defaultGPFunctionAndConstantSets
##' @export
numericConstantSet <- constantFactorySet(function() runif(1, -1, 1))
