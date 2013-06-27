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
DampedOscillator1d <- defineTestFunction(function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi), c(0, 10))


# main symbolic regression driver function for twiddler...
#
twiddleSymbolicRegression <- function(enableAgeCriterion = TRUE,
                                      enableComplexityCriterion = FALSE,
                                      functionSetString = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")',
                                      lambda = 20,
                                      crossoverProbability = 0.9,
                                      maxTimeMinutes = 15,
                                      newIndividualsPerGeneration = 2,
                                      populationSize = 100,
                                      subSamplingShare = 1.0,
                                      randomSeed = 1,
                                      plotFront = TRUE,
                                      plotProgress = TRUE,
                                      testFunctionName = "Salutowicz1d") {
  set.seed(randomSeed)

  testFunction <- switch(testFunctionName,
                         "DampedOscillator1d" = DampedOscillator1d,
                         "Salutowicz1d" = Salutowicz1d,
                         "UnwrappedBall1d" = UnwrappedBall1d,
                         stop("twiddleSymbolicRegression: unkown test function name: ", testFunctionName))
  domainInterval <- testFunction$domainInterval
  testFunctionSamplePoints <- seq(from = domainInterval[1], to = domainInterval[2],
                                  length.out = testFunction$samples)
  fitnessCases <- data.frame(x1 = testFunctionSamplePoints, y = testFunction$f(testFunctionSamplePoints))

  funSet <- do.call(functionSet, as.list(eval(parse(text = functionSetString))))
  inVarSet <- inputVariableSet("x1")
  constSet <- numericConstantSet

  populationFactory <- function(populationSize, funSet, inVarSet, maxfuncdepth, constMin, constMax) { 
    Map(function(i) makeClosure(.Call("initialize_expression_grow_R",
                                      as.list(funSet$nameStrings),
                                      as.integer(funSet$arities),
                                      as.list(inVarSet$nameStrings),
                                      constMin, constMax,
                                      0.8, 0.2,
                                      as.integer(maxfuncdepth)),
                                as.list(inVarSet$nameStrings)), 1:populationSize)
  }

  #newIndividualFactory <- function(n, functionSet, inputVariables, constantSet,
  #                                 maxfuncdepth, extinctionPrevention,
  #                                 breedingFitness, breedingTries) {
  #  makePopulation(n, functionSet, inputVariables, maxfuncdepth, -1.0, 1.0)
  #}
 
  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = lambda,
                                                                     crossoverProbability = crossoverProbability,
                                                                     newIndividualsPerGeneration = newIndividualsPerGeneration,
                                                                     enableComplexityCriterion = enableComplexityCriterion,
                                                                     enableAgeCriterion = enableAgeCriterion)
  
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

  if (length(dev.list()) == 0) {
    dev.new() # create device for phenotype plot 
    if (plotFront) dev.new() # create device for Pareto plot 
    if (plotProgress) dev.new() # create device for progress plot
  }

  statistics <- NULL 
  startTime1 <- Sys.time()
  fitnessHistory <- c()
  complexityHistory <- c()
  ageHistory <- c()
  dominatedHypervolumeHistory <- c()

  pMon <- function(pop, objectiveVectors, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed, indicesToRemove) {
    fitnessValues <- objectiveVectors$fitnessValues
    if (plotFront) {
      oldDev <- dev.cur()
      dev.set(3)
      plotParetoFront(objectiveVectors$poolFitnessValues, objectiveVectors$poolComplexityValues, objectiveVectors$poolAgeValues,
                      indicesToRemove, main = "Selection Pool Pareto Plot",
                      xlab = "Fitness (SRMSE)", ylab = "Complexity (Visitation Length)")
      dev.set(oldDev)
    }
    if (stepNumber %% 10 == 0) {
      message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                      stepNumber, evaluationNumber, bestFitness, timeElapsed))
      if (plotProgress) {
        points <- rbind(objectiveVectors$fitnessValues, objectiveVectors$complexityValues, objectiveVectors$ageValues)
        finitePoints <- points[, !apply(is.infinite(points), 2, any)]
        bestFitnessIndex <- which.min(objectiveVectors$fitnessValues)
        fitnessHistory <<- c(fitnessHistory, log(objectiveVectors$fitnessValues[bestFitnessIndex]))
        complexityHistory <<- c(complexityHistory, objectiveVectors$complexityValues[bestFitnessIndex])
        ageHistory <<- c(ageHistory, objectiveVectors$ageValues[bestFitnessIndex])
        dominatedHypervolumeHistory <<- c(dominatedHypervolumeHistory, dominated_hypervolume(finitePoints))
        generations <- 1:length(fitnessHistory) * 10
        oldDev <- dev.cur()
        dev.set(4)
        oldPar <- par(no.readonly = TRUE)
        layout(matrix(1:4, 4, 1, byrow = TRUE))
        plot(generations, fitnessHistory, type = "l",
             main = "Fittest Individual Fitness", xlab = "Generation", ylab = "Fitness (log SRMSE)")
        plot(generations, complexityHistory, type = "l", col = "red",
             main = "Fittest Individual Complexity", xlab = "Generation", ylab = "Complexity (Visitation Length)")
        plot(generations, ageHistory, type = "l", col = "green",
             main = "Fittest Individual Age", xlab = "Generation", ylab = "Age (Generations)")
        plot(generations, dominatedHypervolumeHistory, type = "l", col = "gray",
             main = "Dominated Hypervolume", xlab = "Generation", ylab = "Hypervolume")
        par(oldPar)
        dev.set(oldDev)
      }
    }
    if (stepNumber %% 50 == 0) {
      bestIndividual <- pop[order(fitnessValues)][[1]] 
      rescaledBestIndividual <- rescaleIndividual(bestIndividual, fitnessCases$y, domainInterval)
      message("current best individual (not rescaled):")
      message(sprintf(" %s", deparse(bestIndividual)))
      dev.set(2)
      plotFunctions(list(testFunction$f, rescaledBestIndividual, bestIndividual), from = domainInterval[1], to = domainInterval[2], steps = 100,
                    ylim = testFunctionRange,
                    main = "Current Best Solution vs. True Function",
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

  population <- populationFactory(populationSize, funSet, inVarSet, 8, -10.0, 10.0)

  sr <- symbolicRegression(y ~ x1, data = fitnessCases,
                           functionSet = funSet,
                           #errorMeasure = mae,
                           errorMeasure = smse,
                           #stopCondition = makeStepsStopCondition(250),
                           stopCondition = makeTimeStopCondition(maxTimeMinutes * 60),
                           population = population,
                           populationSize = populationSize,
                           individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                           subSamplingShare = subSamplingShare,
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
  twiddle(twiddleSymbolicRegression(enableAgeCriterion, enableComplexityCriterion, functionSetString, lambda, crossoverProbability, maxTimeMinutes, newIndividualsPerGeneration, populationSize, subSamplingShare, randomSeed, plotFront, plotProgress, testFunctionName), eval = FALSE,
          testFunctionName = combo("Salutowicz1d", "UnwrappedBall1d", "DampedOscillator1d"),
          plotFront = toggle(default = TRUE),
          plotProgress = toggle(default = TRUE),
          populationSize = knob(lim = c(1, 1000), default = 100, res = 1),
          lambda = knob(lim = c(1, 100), default = 20, res = 1),
          crossoverProbability = knob(lim = c(0.0, 1.0), default = 0.9, res = .01),
          newIndividualsPerGeneration = knob(lim = c(1, 100), default = 2, res = 1),
          enableAgeCriterion = toggle(default = TRUE),
          enableComplexityCriterion = toggle(default = FALSE),
          functionSetString = entry(default = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
          subSamplingShare = knob(lim = c(0.01, 1.0), default = 1.0, res = 0.01),
          randomSeed = knob(lim = c(1, 1000), res = 1),
          maxTimeMinutes = knob(lim = c(0.1, 30), res = 0.1))
}

# main entry point
#
startVisualSr()

# EOF
