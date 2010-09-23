## niching.r
##   - Functions defining evolution main loops for cluster-based niching GP
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include evolution.r
##' @include list_utils.r
NA

##' Cluster-based multi-niche genetic programming
##'
##' TODO one run, multiple parallel passes
##'
##' @param fitnessFunction TODO
##'
##' @seealso \code{\link{geneticProgramming}}, \code{\link{summary.geneticProgrammingResult}}, \code{\link{symbolicRegression}}
##' @export
multiNicheGeneticProgramming <- function(fitnessFunction,
                                         runStopCondition = makeTimeStopCondition(25),
                                         passStopCondition = makeTimeStopCondition(5),
                                         numberOfNiches = 2,
                                         clusterFunction = groupListConsecutive,
                                         joinFunction = function(niches) Reduce(c, niches),
                                         population = NULL,
                                         populationSize = 500,
                                         functionSet = mathFunctionSet,
                                         inputVariables = inputVariableSet("x"),
                                         constantSet = numericConstantSet,
                                         selectionFunction = makeTournamentSelection(),
                                         crossoverFunction = crossover,
                                         mutationFunction = NULL,
                                         progressMonitor = NULL,
                                         verbose = TRUE,
                                         clusterApply = sfClusterApplyLB,
                                         clusterLibrary = sfLibrary,
                                         clusterExport = sfExport,
                                         clusterRemove = sfRemove) {
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
  popClass <- class(pop)
  stepNumber <- 1
  evaluationNumber <- 0
  startTime <- proc.time()["elapsed"]
  timeElapsed <- 0
  clusterLibrary(rgp) # load RGP library on cluster

  ## Execute multi-niche GP run...
  niches <- clusterFunction(pop, numberOfNiches) # cluster population into niches
  for (i in 1:numberOfNiches) class(niches[[i]]) <- popClass # niches should be of class "gp population"
  # TODO ...
  logmsg("STARTING multi-niche genetic programming evolution run...")
  while (!runStopCondition(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)) {
    logmsg("multi-niche pass with %i niches, evolution steps %i, fitness evaluations: %i, time elapsed: %s",
           numberOfNiches, stepNumber, evaluationNumber, formatSeconds(timeElapsed))
    passResults <- clusterApply(niches,
                                function(niche) geneticProgramming(fitnessFunction,
                                                                   stopCondition = passStopCondition,
                                                                   population = niche,
                                                                   functionSet = functionSet,
                                                                   inputVariables = inputVariables,
                                                                   constantSet = constantSet,
                                                                   selectionFunction = selectionFunction,
                                                                   crossoverFunction = crossoverFunction,
                                                                   mutationFunction = mutatefunc,
                                                                   progressMonitor = function(pop, stepNumber, evaluationNumber, timeElapsed) NULL,
                                                                   verbose = FALSE))
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- stepNumber + Reduce(`+`, Map(function(passResult) passResult$stepNumber, passResults))
    evaluationNumber <- evaluationNumber + Reduce(`+`, Map(function(passResult) passResult$evaluationNumber, passResults))
    passResultNiches <- Map(function(passResult) passResult$population, passResults)
    pop <- joinFunction(passResultNiches) # join niches again after each pass
    class(pop) <- popClass # pop should be of class "gp population"
  }
  logmsg("Multi-niche genetic programming evolution run FINISHED after %i evolution steps, %i fitness evaluations and %s.",
         stepNumber, evaluationNumber, formatSeconds(timeElapsed))

  ## Return GP run result...
  structure(list(fitnessFunction = fitnessFunction,
                 stopCondition = runStopCondition,
                 timeElapsed = timeElapsed,
                 stepNumber = stepNumber,
                 evaluationNumber = evaluationNumber,
                 population = pop,
                 functionSet = functionSet,
                 constantSet = constantSet,
                 crossoverFunction = crossoverFunction,
                 mutationFunction = mutatefunc), class = "geneticProgrammingResult")
}
