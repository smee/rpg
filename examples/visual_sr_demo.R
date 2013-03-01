#!/usr/bin/env Rscript
#
# visual_sr_demo.R
# simple visual example of untyped RGP symbolic regression runs
# 2011 Oiver Flasch
#

require("twiddler")
require("rgl")
require("rgp")


# define test functions...
#
defineTestFunction <- function(f, domainInterval = c(0, 1), samples = 100)
  structure(list(f = f, domainInterval = domainInterval, samples = samples), class = "testFunction")

Salutowicz1d <- defineTestFunction(function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1), c(0, 10))
UnwrappedBall1d <- defineTestFunction(function(x) 10/((x - 3)*(x - 3) + 5), c(-10, 10))


# main symbolic regression driver function for twiddler...
#
twiddleSymbolicRegression <- function(enableAgeCriterion = TRUE,
                                      enableComplexityCriterion = FALSE,
                                      functionSetString = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")',
                                      lambda = 20,
                                      maxTimeMinutes = 15,
                                      newIndividualsPerGeneration = 2,
                                      populationSize = 100,
                                      randomSeed = 1,
                                      testFunctionName = "Salutowicz1d") {
  set.seed(randomSeed)

  testFunction <- switch(testFunctionName,
                         "Salutowicz1d" = Salutowicz1d,
                         "UnwrappedBall1d" = UnwrappedBall1d,
                         stop("twiddleSymbolicRegression: unkown test function name: ", testFunctionName))
  domainInterval <- testFunction$domainInterval
  testFunctionSamplePoints <- seq(from = domainInterval[1], to = domainInterval[2],
                                  length.out = testFunction$samples)
  fitnessCases <- data.frame(x1 = testFunctionSamplePoints, y = testFunction$f(testFunctionSamplePoints))

  #searchHeuristic <- makeTinyGpSearchHeuristic()
  #searchHeuristic <- makeCommaEvolutionStrategySearchHeuristic(mu = 25)
  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = lambda,
                                                                     newIndividualsPerGeneration = newIndividualsPerGeneration,
                                                                     enableComplexityCriterion = enableComplexityCriterion,
                                                                     enableAgeCriterion = enableAgeCriterion)

  funSet <- do.call(functionSet, as.list(eval(parse(text = functionSetString))))
  inVarSet <- inputVariableSet("x1")
  constSet <- numericConstantSet

  mutationFunction <- function(ind) {
    subtreeMutantBody <- mutateSubtreeFast(body(ind), funSet, inVarSet, -1, 1, 0.33, 0.75, 1.0, 0.5, 2) 
    #print("--1"); print(subtreeMutantBody)
    functionMutantBody <- mutateFuncFast(subtreeMutantBody, funSet, mutatefuncprob = 0.33)
    #print("--2"); print(functionMutantBody)
    constantMutantBody <- mutateNumericConstFast(functionMutantBody, mutateconstprob = 0.33, mu = 0, sigma = 1)
    #print("--3"); print(constantMutantBody)
    mutant <- makeClosure(constantMutantBody, inVarSet$all, envir = funSet$envir)
    mutant
  }

  crossoverFunction <- function(func1, func2, crossoverprob = 1,
                                 breedingFitness = function(individual) TRUE,
                                 breedingTries = 1) {
    childBody <- crossoverexprFast(body(func1), body(func2))
    child <- makeClosure(childBody, inVarSet$all, envir = funSet$envir)
    child
  }

  sampleFunction <- function(f, from, to, steps) {
    xs <- seq(from, to, length = steps)
    ys <- as.vector(Map(f, xs), mode = "numeric")
    ys
  }
  testFunctionRange <- range(sampleFunction(testFunction$f, from = domainInterval[1], to = domainInterval[2], steps = 100))

  statistics <- NULL 
  startTime1 <- Sys.time()

  pMon <- function(pop, fitnessValues, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
    if (stepNumber %% 10 == 0) {
      message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                      stepNumber, evaluationNumber, bestFitness, timeElapsed))
    }
    if (stepNumber %% 50 == 0) {
      bestIndividual <- pop[order(fitnessValues)][[1]] 
      rescaledBestIndividual <- rescaleIndividual(bestIndividual, fitnessCases$y, domainInterval)
      message("current best individual (not rescaled):")
      message(sprintf(" %s", deparse(bestIndividual)))
      plotFunctions(list(testFunction$f, rescaledBestIndividual, bestIndividual), from = domainInterval[1], to = domainInterval[2], steps = 100,
                    ylim = testFunctionRange,
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
                           stopCondition = makeTimeStopCondition(maxTimeMinutes * 60),
                           populationSize = populationSize,
                           individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                           mutationFunction = mutationFunction,
                           crossoverFunction = crossoverFunction,
                           searchHeuristic = searchHeuristic,
                           envir = environment(), # TODO
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
rescaleIndividual <- function(ind, trueY, domainInterval, samples = 100) {
  indX <- seq(from = domainInterval[1], to = domainInterval[2], length.out = samples)
  indY <- ind(indX)
  b = cov(trueY, indY) / var(indY)
  a = mean(trueY) - b * mean(indY)
  function(x1) a + b * ind(x1)
}

startVisualSr <- function() {
  twiddle(twiddleSymbolicRegression(enableAgeCriterion, enableComplexityCriterion, functionSetString, lambda, maxTimeMinutes, newIndividualsPerGeneration, populationSize, randomSeed, testFunctionName), eval = FALSE,
          testFunctionName = combo("Salutowicz1d", "UnwrappedBall1d"),
          populationSize = knob(lim = c(1, 1000), default = 100, res = 1),
          lambda = knob(lim = c(1, 100), default = 20, res = 1),
          newIndividualsPerGeneration = knob(lim = c(1, 100), default = 2, res = 1),
          enableAgeCriterion = toggle(default = TRUE),
          enableComplexityCriterion = toggle(default = FALSE),
          functionSetString = entry(default = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
          randomSeed = knob(lim = c(1, 1000), res = 1),
          maxTimeMinutes = knob(lim = c(0.1, 240), res = 0.1))
}

# main entry point
#
startVisualSr()

# EOF
