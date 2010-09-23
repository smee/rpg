## evolution.r
##   - Functions defining typical evolution main loops,
##     some typical GP function- and constant sets
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include search_space.r
##' @include time_utils.r
NA

##' Standard genetic programming
##'
##' Perform a standard genetic programming run. The required argument \code{fitnessFunction}
##' must be supplied with an objective function that assigns a numerical fitness
##' value to an R function. Fitness values are minimized, i.e. smaller values mean
##' higher/better fitness. If a multi-objective \code{selectionFunction} is
##' used, \code{fitnessFunction} return a numerical vector of fitness values.
##' The result of the genetic programming run is a genetic programming result object
##' containing a GP population of R functions. \code{summary.geneticProgrammingResult}
##' can be used to create summary views of a GP result object.
##'
##' @param fitnessFunction In case of a single-objective selection function,
##'   \code{fitnessFunction} must be a single function that assigns a
##'   numerical fitness value to a GP individual represented as a R function.
##'   Smaller fitness values mean higher/better fitness. If a multi-objective
##'   selection function is used, \code{fitnessFunction} must return a numerical
##'   vector of fitness values.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{makeStepsStopCondition} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param functionSet The function set.
##' @param inputVariables The input variable set.
##' @param constantSet The set of constant factory functions.
##' @param selectionFunction The selection function to use. Defaults to
##'   tournament selection with tournament size \code{populationSize / 2}. See
##'   \link{makeTournamentSelection} for details.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param progressMonitor A function of signature
##'   \code{function(population, stepNumber, evaluationNumber, timeElapsed)} to be called
##'   with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return A genetic programming result object that contains a GP population in the
##'   field \code{population}, as well as metadata describing the run parameters.
##'
##' @seealso \code{\link{summary.geneticProgrammingResult}}, \code{\link{symbolicRegression}}
##' @export
geneticProgramming <- function(fitnessFunction,
                               stopCondition = makeTimeStopCondition(5),
                               population = NULL,
                               populationSize = 100,
                               functionSet = mathFunctionSet,
                               inputVariables = inputVariableSet("x"),
                               constantSet = numericConstantSet,
                               selectionFunction = makeTournamentSelection(tournamentSize = ceiling(populationSize / 2)),
                               crossoverFunction = crossover,
                               mutationFunction = NULL,
                               progressMonitor = NULL,
                               verbose = TRUE) {
  ## Provide default parameters and initialize GP run...
  logmsg <- function(msg, ...) {
    if (verbose)
      message(sprintf(msg, ...))
  }
  progmon <-
    if (verbose) {
      function(pop, evaluationNumber, stepNumber, timeElapsed) {
        if (!is.null(progressMonitor))
          progressMonitor(pop, evaluationNumber, stepNumber, timeElapsed)
        if (stepNumber %% 100 == 0)
          logmsg("evolution step %i, fitness evaluations: %i, time elapsed: %s",
                 stepNumber, evaluationNumber, formatSeconds(timeElapsed))
      }
    } else if (is.null(progressMonitor)) {
      function(pop, stepNumber, evaluationNumber, timeElapsed) NULL # verbose == FALSE, do not show progress
    } else
      progressMonitor
  mutatefunc <-
    if (is.null(mutationFunction)) {
      function(ind) mutateSubtree(mutateNumericConst(ind),
                                  functionSet, inputVariables, constantSet, mutatesubtreeprob = 0.01)
    } else
      mutationFunction
  pop <-
    if (is.null(population))
      makePopulation(populationSize, functionSet, inputVariables, constantSet)
    else
      population
  stepNumber <- 1
  evaluationNumber <- 0
  startTime <- proc.time()["elapsed"]
  timeElapsed <- 0

  ## Execute GP run...
  logmsg("STARTING standard genetic programming evolution run...")
  while (!stopCondition(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)) {
    # Select two sets of individuals and divide each into winners and losers...
    selA <- selectionFunction(pop, fitnessFunction); selB <- selectionFunction(pop, fitnessFunction)
    winnersA <- selA$selected[, 1]; winnersB <- selB$selected[, 1]
    losersA <- selA$discarded[, 1]; losersB <- selB$discarded[, 1]
    losers <- c(losersA, losersB)
    # Create winner children...
    winnerChildren <- Map(function(winnerA, winnerB)
                            mutatefunc(crossoverFunction(pop[[winnerA]], pop[[winnerB]])),
                          winnersA, winnersB)
    # Replace losers by winner children (cycling the list of winner children if too short)...
    suppressWarnings(pop[losers] <- winnerChildren)
    
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- 1 + stepNumber
    evaluationNumber <- selA$numberOfFitnessEvaluations + selB$numberOfFitnessEvaluations + evaluationNumber
    progmon(pop = pop, stepNumber = stepNumber, evaluationNumber = evaluationNumber, timeElapsed = timeElapsed)
  }
  logmsg("Standard genetic programming evolution run FINISHED after %i evolution steps, %i fitness evaluations and %s.",
         stepNumber, evaluationNumber, formatSeconds(timeElapsed))

  ## Return GP run result...
  structure(list(fitnessFunction = fitnessFunction,
                 stopCondition = stopCondition,
                 timeElapsed = timeElapsed,
                 stepNumber = stepNumber,
                 evaluationNumber = evaluationNumber,
                 population = pop,
                 functionSet = functionSet,
                 constantSet = constantSet,
                 crossoverFunction = crossoverFunction,
                 mutationFunction = mutatefunc), class = "geneticProgrammingResult")
}

##' Summary reports of genetic programming run result objects
##'
##' Create a summary report of a genetic programming result object as returned by
##' \code{\link{geneticProgramming}} or \code{\link{symbolicRegression}}, for
##' example.
##'
##' @param object The genetic programming run result object to report on.
##' @param reportFitness Whether to report the fitness values of each individual
##'   in the result population. Note that calculating fitness values may take
##'   a long time. Defaults to \code{TRUE}.
##' @param orderByFitness Whether the report of the result population should be
##'   ordered by fitness. This does not have an effect if \code{reportFitness}
##'   is set to \code{FALSE}. Defaults to \code{TRUE}.
##' @param ... Ignored in this summary function.
##'
##' @seealso \code{\link{geneticProgramming}}, \code{\link{symbolicRegression}}
##' @export
summary.geneticProgrammingResult <- function(object, reportFitness = TRUE, orderByFitness = TRUE, ...) {
  individualFunctions <- object$population
  individualFunctionsAsStrings <- Map(function(f) Reduce(function(a, b) paste(a, b, sep=""),
                                                         deparse(f)),
                                      individualFunctions)
  report <- cbind(1:length(individualFunctions), individualFunctions, individualFunctionsAsStrings)
  colnames(report) <- c("Individual Index", "Individual Function", "(as String)")
  if (reportFitness) {
    fitnessList <- lapply(individualFunctions, object$fitnessFunction)
    fitnessesLength <- length(fitnessList)
    fitnessDimemsion <- length(fitnessList[[1]])
    flatFitnesses <- Reduce(c, fitnessList)
    fitnessMatrix <- matrix(as.list(flatFitnesses), ncol = fitnessDimemsion)
    if (is.null(colnames(fitnessMatrix))) colnames(fitnessMatrix) <- paste("Fitness", 1:fitnessDimemsion)
    report <- cbind(report, fitnessMatrix)
    if (orderByFitness) {
      rawFitnessMatrix <- matrix(flatFitnesses, ncol = fitnessDimemsion)
      report <- report[do.call(order, split(rawFitnessMatrix, col(rawFitnessMatrix))),]
    }
  }
  report
}

##' Symbolic regression via untyped genetic programming
##'
##' Perform symbolic regression via untyped genetic programming. The regression
##' task is specified as a \code{\link{formula}}. Only the simple formulas
##' without interactions are supported at this time. The result of the symbolic
##' regression run is a symbolic regression model containing an untyped GP
##' population of model functions.
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
##' @param penalizeGenotypeConstantIndividuals Individuals that do not contain
##'   any input variables will get a fitness of \code{Inf}.
##' @param functionSet The function set.
##' @param constantSet The set of constant factory functions.
##' @param selectionFunction The selection function to use. Defaults to
##'   tournament selection with tournament size \code{populationSize / 2}. See
##'   \link{makeTournamentSelection} for details.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param progressMonitor A function of signature
##'   \code{function(population, stepNumber, evaluationNumber, timeElapsed)} to be called
##'   with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return An symbolic regression model that contains an untyped GP population.
##'
##' @seealso \code{\link{predict.symbolicRegressionModel}}, \code{\link{geneticProgramming}}
##' @export
symbolicRegression <- function(formula, data,
                               stopCondition = makeStepsStopCondition(1000),
                               population = NULL,
                               populationSize = 100,
                               individualSizeLimit = 64,
                               penalizeGenotypeConstantIndividuals = FALSE,
                               functionSet = mathFunctionSet,
                               constantSet = numericConstantSet,
                               selectionFunction = makeTournamentSelection(tournamentSize = ceiling(populationSize / 2)),
                               crossoverFunction = crossover,
                               mutationFunction = NULL,
                               progressMonitor = NULL,
                               verbose = TRUE) {
  ## Match variables in formula to those in data or parent.frame() and
  ## return them in a new data frame. This also expands any '.'
  ## arguments in the formula.  
  mf <- model.frame(formula, data)
  ## Extract list of terms (rhs of ~) in expanded formula
  variableNames <- attr(terms(formula(mf)), "term.labels")
  ## Create inputVariableSet
  inVarSet <- inputVariableSet(list=as.list(variableNames))
  fitFunc <- makeRegressionFitnessFunction(formula(mf), mf, errormeasure = rmse,
                                           penalizeGenotypeConstantIndividuals = penalizeGenotypeConstantIndividuals,
                                           indsizelimit = individualSizeLimit)
  gpModel <- geneticProgramming(fitFunc, stopCondition, population, populationSize,
                                functionSet, inVarSet, constantSet, selectionFunction,
                                crossoverFunction, mutationFunction,
                                progressMonitor, verbose)
  
  structure(append(gpModel, list(formula = formula(mf))),
                   class = c("symbolicRegressionModel", "geneticProgrammingResult"))
}

##' Predict method for symbolic regression models
##'
##' Predict values via a model function from a population of model functions
##' generated by symbolic regression.
##'
##' @param object A model created by \code{\link{symbolicRegression}}.
##' @param newdata A \code{\link{data.frame}} containing input data for the
##'   symbolic regression model. The variables in \code{object$formula} must match
##'   column names in this data frame.
##' @param model The numeric index of the model function in \code{object$population}
##'   to use for prediction or \code{"BEST"} to use the model function with the best
##'   training fitness.
##' @param detailed Whether to add metadata to the prediction object returned.
##' @param ... Ignored in this \code{predict} method.
##' @return A vector of predicted values or, if \code{detailed} is \code{TRUE}, a
##'   list of the following elements:
##'   \code{model} the model used in this prediction
##'   \code{response} a matrix of predicted versus respone values
##'   \code{RMSE} the RMSE between the real and predicted response
##'
##' @export
predict.symbolicRegressionModel <- function(object, newdata, model = "BEST", detailed = FALSE, ...) {
  ind <- if (model == "BEST") {
    trainingFitnessSortedPopulation <- sortBy(object$population, object$fitnessFunction)
    trainingFitnessSortedPopulation[[1]]
  } else object$population[[model]]
  data <- if (any(is.na(newdata))) {
    dataWithoutNAs <- na.omit(newdata)
    warning(sprintf("removed %i data rows containing NA values", length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else newdata
  
  formulaVars <- as.list(attr(terms(object$formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  attach(data)
  trueResponse <- eval(responseVariable)
  explanatories <- lapply(explanatoryVariables, eval)
  detach(data)
  ysind <- do.call(ind, explanatories) # vectorized evaluation
  errorind <- rmse(trueResponse, ysind)
  
  if (detailed) {
    predictedVersusReal <- cbind(ysind, trueResponse)
    colnames(predictedVersusReal) <- c("predicted", "real")
    list(model = ind, response = predictedVersusReal, RMSE = errorind)
  } else ysind
}

##' Evolution stop conditions
##'
##' Evolution stop conditions are predicates (functions that return a single logical value)
##' of the signature \code{function(population, stepNumber, evaluationNumber, timeElapsed)}.
##' They are used to decide when to finish a GP evolution run. Stop conditions must be members
##' of the S3 class \code{c("stopCondition", "function")}. They can be combined using the
##' generic \emph{and} (\code{|}), \emph{or} (\code{|}) and \emph{not} (\code{!}) functions.
##'
##' \code{makeStepsStopCondition} creates a stop condition that is fulfilled if the number
##' of evolution steps exceeds a given limit.
##' \code{makeEvaluationsStopCondition} creates a stop condition that is fulfilled if the
##' number of fitness function evaluations exceeds a given limit.
##' \code{makeTimeStopCondition} creates a stop condition that is fulfilled if the run time
##' (in seconds) of an evolution run exceeds a given limit.
##'
##' @param stepLimit The maximum number of evolution steps for \code{makeStepsStopCondition}.
##' @param evaluationLimit The maximum number of fitness function evaluations for
##'   \code{makeEvaluationsStopCondition}
##' @param timeLimit The maximum runtime in seconds for \code{makeTimeStopCondition}.
##' @param e1 A stop condition.
##' @param e2 A stop condition.
##'
##' @rdname evolutionStopConditions
##' @export
makeStepsStopCondition <- function(stepLimit) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed) stepNumber >= stepLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export
makeEvaluationsStopCondition <- function(evaluationLimit) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed) evaluationNumber >= evaluationLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export
makeTimeStopCondition <- function(timeLimit) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed) timeElapsed >= timeLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `&.stopCondition`
`&.stopCondition` <- function(e1, e2) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed)
    e1(pop, stepNumber, evaluationNumber, timeElapsed) && e2(pop, stepNumber, evaluationNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `|.stopCondition`
`|.stopCondition` <- function(e1, e2) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed)
    e1(pop, stepNumber, evaluationNumber, timeElapsed) || e2(pop, stepNumber, evaluationNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `!.stopCondition`
`!.stopCondition` <- function(e1) {
  stopCondition <- function(pop, stepNumber, evaluationNumber, timeElapsed)
    !e1(pop, stepNumber, evaluationNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' Some simple arithmetic and logic functions for use in GP expressions
##'
##' \code{safeDivide} a division operator that returns 0 if the divisor is 0.
##' \code{safeLn} a natural logarithm operator that return 0 if its argument is less
##'   then 0.
##' \code{ln} is the natural logarithm.
##' \code{positive} returns true if its argument is greater then 0.
##' \code{ifPositive} returns its second argument if its first argument is positive,
##'   otherwise its third argument.
##' \code{ifThenElse} returns its second argument if its first argument is \code{TRUE},
##'   otherwise its third argument.
##'
##' @param a A numeric value.
##' @param b A numeric value.
##' @param x A numeric value.
##' @param thenbranch The element to return when \code{x} is \code{TRUE}.
##' @param elsebranch The element to return when \code{x} is \code{FALSE}.
##'
##' @rdname safeGPfunctions
##' @export
safeDivide <- function(a, b) ifelse(b == 0, b, a / b)

##' @rdname safeGPfunctions
##' @export
safeSqroot <- function(a) sqrt(ifelse(a < 0, 0, a))

##' @rdname safeGPfunctions
##' @export
safeLn <- function(a) log(ifelse(a < 0, 0, a))

##' @rdname safeGPfunctions
##' @export
ln <- function(a) log(a)

##' @rdname safeGPfunctions
##' @export
positive <- function(x) x > 0

##' @rdname safeGPfunctions
##' @export
ifPositive <- function(x, thenbranch, elsebranch) ifelse(x > 0, thenbranch, elsebranch)

##' @rdname safeGPfunctions
##' @export
ifThenElse <- function(x, thenbranch, elsebranch) ifelse(x, thenbranch, elsebranch)

##' Default function- and constant factory sets for Genetic Programming
##'
##' \code{arithmeticFunctionSet} is an untyped function set containing the functions
##' "+", "-", "*", and "/".
##' \code{expLogFunctionSet} is an untyped function set containing the functions
##' "sqrt", "exp", and "ln".
##' \code{trigonometricFunctionSet} is an untyped function set containing the functions
##' "sin", "cos", and "tan".
##' \code{mathFunctionSet} is an untyped function set containing all of the above functions.
##'
##' \code{numericConstantSet} is an untyped constant factory set containing a single
##' constant factory that creates numeric constants via calls to \code{runif(1, -1, 1)}.
##'
##' \code{typedArithmeticFuncset} is a typed function set containing the functions
##' "+", "-", "*", and "/".
##' \code{typedExpLogFuncset} is a typed function set containing the functions
##' "sqrt", "exp", and "ln".
##' \code{typedTrigonometricFuncset} is a typed function set containing the functions
##' "sin", "cos", and "tan".
##' \code{typedMathFuncset} is a typed function set containing all of the typed functions above.
##'
##' \code{typedLogicalFuncset} is a typed function set containing the functions
##' "<", ">", "==", "ifThenElse", "&", "|", and "!".
##' \code{typedMathLogicalFuncset} is a typed function set containing all functions of
##' \code{typedMathFuncset} and \code{typedLogicalFuncset}.
##'
##' \code{typedHigherOrderVectorFuncset} is a typed function set containing the functions
##' "sapply" and "mean".
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

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedArithmeticFuncset <- functionSet("+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")))

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedExpLogFuncset <- functionSet("sqrt" %::% (list(st("numeric")) %->% st("numeric")),
                                  "exp" %::% (list(st("numeric")) %->% st("numeric")),
                                  "ln" %::% (list(st("numeric")) %->% st("numeric")))

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedTrigonometricFuncset <- functionSet("sin" %::% (list(st("numeric")) %->% st("numeric")),
                                         "cos" %::% (list(st("numeric")) %->% st("numeric")),
                                         "tan" %::% (list(st("numeric")) %->% st("numeric")))

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedMathFuncset <- c(typedArithmeticFuncset, typedExpLogFuncset, typedTrigonometricFuncset)

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedLogicalFuncset <- functionSet("<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   "==" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   "ifThenElse" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")),
                                   "&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                   "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                   "!" %::% (list(st("logical")) %->% st("logical")))

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedMathLogicalFuncset <- c(typedMathFuncset, typedLogicalFuncset)

##' @rdname defaultGPFunctionAndConstantSets
##' @export
typedHigherOrderVectorFuncset <- functionSet("sapply" %::% (list(st("numeric"), list(st("numeric")) %->% st("numeric")) %->% st("numeric")),
                                             "mean" %::% (list(st("numeric")) %->% st("numeric")))

