# visual_sr_demo.R
# simple visual example of untyped RGP symbolic regression runs
# 2011 Oiver Flasch
#

require("twiddler")
require("rgl")
require("rgp")


# define test functions...
#
defineTestFunction <- function(f, dim = 1) structure(list(f = f, dim = dim), class = "testFunction")

Salutowicz1d <- defineTestFunction(function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1), 1)


# main symbolic regression driver function for twiddler...
#
twiddleSymbolicRegression <- function(testFunctionName = "Salutowicz1d",
                                      maxTime = 15) {
  fitnessCases <- data.frame(x1 = 1:100 * 0.1, y = Salutowicz1d$f(1:100 * 0.1))

  #searchHeuristic <- makeExploitativeSteadyStateSearchHeuristic(selectionFunction = makeTournamentSelection(tournamentSize = 10))
  #searchHeuristic <- makeTinyGpSearchHeuristic()
  #searchHeuristic <- makeCommaEvolutionStrategySearchHeuristic(mu = 25)
  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = 20, newIndividualsPerGeneration = 2,
                                                                     enableComplexityCriterion = TRUE,
                                                                     enableAgeCriterion = TRUE)

  funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt") 
  inVarSet <- inputVariableSet("x1")
  constSet <- numericConstantSet

  mutationFunction1 <- function(ind) {
    mutateChangeDeleteInsert(ind, funSet, inVarSet, constSet,
                             strength = 1, subtreeDepth = 2, constprob = 0.2, iterations = 1,
                             changeProbability = 1/3, deleteProbability = 1/3, insertProbability = 1/3)
  }

  mutationFunction2 <- function(ind) {
    subtreeMutantBody <- .Call("mutate_subtrees_R", body(ind), 0.33, 0.75,
                               funSet$all, as.integer(funSet$arities),
                               inVarSet$all, -1, 1, 1.0, 0.5, 2L)
    #print("--1"); print(subtreeMutantBody)
    functionMutantBody <- .Call("mutate_functions_R", subtreeMutantBody, 0.33,
                                funSet$all, as.integer(funSet$arities))
    #print("--2"); print(functionMutantBody)
    constantMutantBody <- .Call("mutate_constants_normal_R", functionMutantBody, 0.33, 0, 1)
    #print("--3"); print(constantMutantBody)
    mutant <- .Call("make_closure", constantMutantBody, inVarSet$all)
    mutant
  }

  crossoverFunction1 <- function(func1, func2, crossoverprob = 1,
                                 breedingFitness = function(individual) TRUE,
                                 breedingTries = 1) {
    childBody <- .Call("crossover_single_point_R", body(func1), body(func2))[[1]]
    child <- .Call("make_closure", childBody, inVarSet$all)
    child
  }

  statistics <- NULL 
  startTime1 <- Sys.time()

  pMon <- function(pop, fitnessValues, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
    if (stepNumber %% 10 == 0) {
      message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                      stepNumber, evaluationNumber, bestFitness, timeElapsed))
    }
    if (stepNumber %% 50 == 0) {
      bestIndividual <- pop[order(fitnessValues)][[1]] 
      rescaledBestIndividual <- rescaleIndividual(bestIndividual, fitnessCases$y)
      message("current best individual (not rescaled):")
      message(sprintf(" %s", deparse(bestIndividual)))
      plotFunctions(list(Salutowicz1d$f, rescaledBestIndividual, bestIndividual), from = 0, to = 10, steps = 100,
                    main = "Current Best Individual vs. True Function",
                    sub = sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                                  stepNumber, evaluationNumber, bestFitness, timeElapsed))
    }
    if (stepNumber %% 100 == 0) {
      timeTaken <- as.numeric(Sys.time() - startTime1, units = "secs")
      startTime1 <<- Sys.time()
      popSizes <- as.numeric(Map(funcSize, pop))
      sizeStatistics <- summary(popSizes)
      statisticsRow <- c(sizeStatistics, Time = timeTaken)
      statistics <<- rbind(statistics, statisticsRow)
      message("population function size statistics: min: ", min(popSizes),
              " median: ", median(popSizes), " mean: ", mean(popSizes),
              " max: ", max(popSizes),
              " time: ", timeTaken)
    }
  }

  sr <- symbolicRegression(y ~ x1, data = fitnessCases,
                           functionSet = funSet,
                           #errorMeasure = mae,
                           errorMeasure = smse,
                           #stopCondition = makeStepsStopCondition(250),
                           stopCondition = makeTimeStopCondition(maxTime * 60),
                           populationSize = 200,
                           individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                           #mutationFunction = mutationFunction1,
                           mutationFunction = mutationFunction2,
                           crossoverFunction = crossoverFunction1,
                           searchHeuristic = searchHeuristic,
                           verbose = TRUE,
                           progressMonitor = pMon)

  #quartz()
  #old.par <- par(mfcol = c(2, 1))
  #plot(statistics[,"Mean"], type = "b", main = "Mean Individual Size", xlab = "step * 100", ylab = "Mean Ind. Size (Nodes")
  #plot(statistics[,"Time"], type = "b", main = "Compute Time", xlab = "step * 100", ylab = "Compute Time (Secs)")
  #par(old.par)
  #message("type ls() to view active bindings, the result population has been saved to the variable sr1")

  return(sr)
}

# Tool functions...
#
rescaleIndividual <- function(ind, trueY) {
  indY <- ind(1:100 * 0.1)
  b = cov(trueY, indY) / var(indY)
  a = mean(trueY) - b * mean(indY)
  function(x1) a + b * ind(x1)
}

# main entry point
#
twiddle(twiddleSymbolicRegression(testFunctionName, maxTime), eval = FALSE,
        testFunctionName = combo("Salutowicz1d", "TODO"),
        maxTime = knob(lim = c(0.1, 60), res = 0.1))

# EOF
