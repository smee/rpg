## gui.R
##   - RGP GUIs 
##
## RGP - a GP system for R
## 2010-13 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Symbolic Regression GUI 
##'
##' Shows the GUI for (untyped) symbolic regression. This GUI is limited to functions of one
##' variable (1D function) as of now. See the documentation on \code{symbolicRegression}
##' on how to perform symbolic regression for higher dimensional data.
##' This function returns the symbolic regression result object after the user closes
##' the GUI window.
##'
##' @return A symbolic regression result object.
##' @export
symbolicRegressionGui <- function() {
  # init variables to make CRAN happy
  enableAgeCriterion <- NULL; enableComplexityCriterion <- NULL
  functionSetString <- NULL
  lambda <- NULL; crossoverProbability <- NULL; maxTimeMinutes <- NULL
  newIndividualsPerGeneration <- NULL; populationSize <- NULL
  subSamplingShare <- NULL; randomSeed <- NULL
  plotFront <- NULL; plotProgress <- NULL
  ndsParentSelectionProbability <- NULL; ndsSelectionFunctionName <- NULL
  targetFunctionName <- NULL; csvFileName <- NULL
  twiddle(twiddleSymbolicRegression(enableAgeCriterion, enableComplexityCriterion,
                                    functionSetString, errorMeasureName,
                                    lambda, crossoverProbability, maxTimeMinutes,
                                    newIndividualsPerGeneration, populationSize, subSamplingShare,
                                    randomSeed, plotFront, plotProgress,
                                    ndsParentSelectionProbability, ndsSelectionFunctionName,
                                    targetFunctionName, csvFileName),
          auto = FALSE, eval = FALSE, label = "RGP Symbolic Regression GUI",
          targetFunctionName = combo("Salutowicz 1d", "Unwrapped Ball 1d", "Damped Oscillator 1d", "CSV File"),
          csvFileName = filer(),
          plotFront = toggle(default = TRUE),
          plotProgress = toggle(default = TRUE),
          populationSize = knob(lim = c(1, 1000), default = 100, res = 1),
          lambda = knob(lim = c(1, 100), default = 20, res = 1),
          crossoverProbability = knob(lim = c(0.0, 1.0), default = 0.5, res = .01),
          newIndividualsPerGeneration = knob(lim = c(1, 100), default = 20, res = 1),
          enableAgeCriterion = toggle(default = TRUE),
          enableComplexityCriterion = toggle(default = TRUE),
          ndsParentSelectionProbability = knob(lim = c(0.0, 1.0), default = 1.0, res = .01),
          ndsSelectionFunctionName = combo("Crowding Distance", "Hypervolume"),
          functionSetString = entry(default = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
          errorMeasureName = combo("SMSE", "SSSE", "RMSE", "SSE", "MAE"),
          subSamplingShare = knob(lim = c(0.01, 1.0), default = 1.0, res = 0.01),
          randomSeed = knob(lim = c(1, 1000), res = 1),
          maxTimeMinutes = knob(lim = c(0.1, 60), res = 0.5, default = 15.0))
}

# Test target functions...
#
defineTargetFunction <- function(f, domainInterval = c(0, 1), dim = 1, samples = 100)
  structure(list(f = f, domainInterval = domainInterval, dim = dim, samples = samples), class = "targetFunction")

Salutowicz1d <- defineTargetFunction(function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1), c(0, 10))
UnwrappedBall1d <- defineTargetFunction(function(x) 10/((x - 3)*(x - 3) + 5), c(-10, 10))
DampedOscillator1d <- defineTargetFunction(function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi), c(0, 10))

# Symbolic regression driver function for twiddler...
#
twiddleSymbolicRegression <- function(enableAgeCriterion = TRUE,
                                      enableComplexityCriterion = TRUE,
                                      functionSetString = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")',
                                      errorMeasureName = "SMSE",
                                      lambda = 20,
                                      crossoverProbability = 0.5,
                                      maxTimeMinutes = 15,
                                      newIndividualsPerGeneration = 20,
                                      populationSize = 100,
                                      subSamplingShare = 1.0,
                                      randomSeed = 1,
                                      plotFront = TRUE,
                                      plotProgress = TRUE,
                                      ndsParentSelectionProbability = 1.0,
                                      ndsSelectionFunctionName = "Crowding Distance",
                                      targetFunctionName = "Salutowicz 1d",
                                      csvFileName = "") {
  system(sprintf("afplay -t %d ~/repos/rgp/examples/data/gp_music.mp4", as.integer(maxTimeMinutes * 60)), wait = FALSE) # TODO playing music may lead to better GP results

  set.seed(randomSeed)

  targetFunction <- switch(targetFunctionName,
                         "Damped Oscillator 1d" = DampedOscillator1d,
                         "Salutowicz 1d" = Salutowicz1d,
                         "Unwrapped Ball 1d" = UnwrappedBall1d,
                         "CSV File" = makeCsvFileTargetFunction(csvFileName),
                         stop("twiddleSymbolicRegression: unkown test function name: ", targetFunctionName))
  errorMeasure  <- switch(errorMeasureName,
                          "SMSE" = smse,
                          "SSSE" = ssse,
                          "RMSE" = rmse,
                          "SSE" = sse,
                          "MAE" = mae,
                          stop("twiddleSymbolicRegression: unkown error measure name: ", errorMeasureName))
  ndsSelectionFunction <- switch(ndsSelectionFunctionName,
                                 "Crowding Distance" = nds_cd_selection,
                                 "Hypervolume" = nds_hv_selection,
                                 stop("twiddleSymbolicRegression: unkown NDS selection function name: ", ndsSelectionFunctionName))
  domainInterval <- targetFunction$domainInterval
  targetFunctionSamplePoints <- seq(from = domainInterval[1], to = domainInterval[2],
                                  length.out = targetFunction$samples)
  fitnessCases <- data.frame(x1 = targetFunctionSamplePoints, y = targetFunction$f(targetFunctionSamplePoints))

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

  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = lambda,
                                                                     crossoverProbability = crossoverProbability,
                                                                     newIndividualsPerGeneration = newIndividualsPerGeneration,
                                                                     enableComplexityCriterion = enableComplexityCriterion,
                                                                     enableAgeCriterion = enableAgeCriterion,
                                                                     ndsParentSelectionProbability = ndsParentSelectionProbability,
                                                                     ndsSelectionFunction = ndsSelectionFunction)
  
  sampleFunction <- function(f, from, to, steps) {
    xs <- seq(from, to, length = steps)
    ys <- as.vector(Map(f, xs), mode = "numeric")
    ys
  }

  targetFunctionRange <- range(sampleFunction(targetFunction$f, from = domainInterval[1], to = domainInterval[2], steps = targetFunction$samples))

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
  lastBestFitness <- Inf

  progressMonitor <- function(pop, objectiveVectors, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed, indicesToRemove) {
    fitnessValues <- objectiveVectors$fitnessValues
    if (plotFront) {
      oldDev <- dev.cur()
      dev.set(3)
      plotParetoFront(objectiveVectors$poolFitnessValues, objectiveVectors$poolComplexityValues, objectiveVectors$poolAgeValues,
                      indicesToRemove, main = sprintf("Selection Pool Pareto Plot (%d Individuals)", length(objectiveVectors$poolFitnessValues)),
                      xlab = "Fitness", ylab = "Complexity (Visitation Length)")
      dev.set(oldDev)
    }
    if (bestFitness < lastBestFitness) {
      alarm() # beep when a new best individual is found
      lastBestFitness <<- bestFitness
      bestIndividual <- pop[order(fitnessValues)][[1]] 
      if (errorMeasureName == "SMSE" || errorMeasureName == "SSSE") {
        rescaledBestIndividual <- rescaleIndividual(bestIndividual, fitnessCases$y, domainInterval, targetFunction$samples)
        message("NEW best individual (not rescaled):")
        message(sprintf(" %s", deparse(bestIndividual)))
        dev.set(2)
        plotFunctions(list(targetFunction$f, rescaledBestIndividual, bestIndividual),
                      from = domainInterval[1], to = domainInterval[2], steps = targetFunction$samples,
                      ylim = targetFunctionRange,
                      main = "Current Best Solution vs. True Function",
                      sub = sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                                    stepNumber, evaluationNumber, bestFitness, timeElapsed))
      } else {
        message("NEW best individual:")
        message(sprintf(" %s", deparse(bestIndividual)))
        dev.set(2)
        plotFunctions(list(targetFunction$f, bestIndividual),
                      from = domainInterval[1], to = domainInterval[2], steps = targetFunction$samples,
                      ylim = targetFunctionRange,
                      main = "Current Best Solution vs. True Function",
                      sub = sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                                    stepNumber, evaluationNumber, bestFitness, timeElapsed))
      }
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
             main = "Fittest Individual Fitness", xlab = "Generation", ylab = "log Fitness")
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
                           errorMeasure = errorMeasure,
                           #stopCondition = makeStepsStopCondition(250),
                           stopCondition = makeTimeStopCondition(maxTimeMinutes * 60),
                           population = population,
                           populationSize = populationSize,
                           individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                           subSamplingShare = subSamplingShare,
                           searchHeuristic = searchHeuristic,
                           envir = environment(), # TODO
                           verbose = TRUE,
                           progressMonitor = progressMonitor)
  return(sr)
}

# Tool functions...
#
makeDataFrameTargetFunction <- function(dataFrame) {
  stopifnot(ncol(dataFrame) == 2)
  defineTargetFunction(f = approxfun(x = dataFrame[[1]], y = dataFrame[[2]], rule = 1),
                       domainInterval = as.numeric(range(dataFrame[[1]])),
                       dim = 1,
                       samples = nrow(dataFrame))
}

makeCsvFileTargetFunction <- function(csvFileName) makeDataFrameTargetFunction(read.csv(csvFileName))
rescaleIndividual <- function(ind, trueY, domainInterval, samples = 100) {
  indX <- seq(from = domainInterval[1], to = domainInterval[2], length.out = samples)
  indY <- ind(indX)
  b = cov(trueY, indY) / var(indY)
  a = mean(trueY) - b * mean(indY)
  function(x1) a + b * ind(x1)
}

