#!/usr/bin/env Rscript
#
# initial_population_objective_space_statistics.R 
# visual example for calculating initial population objective space statistics 
# 2013 Oiver Flasch
#

require("twiddler")
require("scales")
require("rgp")


# define test functions...
#
defineTargetFunction <- function(f, domainInterval = c(0, 1), dim = 1, samples = 100)
  structure(list(f = f, domainInterval = domainInterval, dim = dim, samples = samples), class = "targetFunction")

makeDataFrameTargetFunction <- function(dataFrame) {
  stopifnot(ncol(dataFrame) == 2)
  defineTargetFunction(f = approxfun(x = dataFrame[[1]], y = dataFrame[[2]], rule = 1),
                       domainInterval = as.numeric(range(dataFrame[[1]])),
                       dim = 1,
                       samples = nrow(dataFrame))
}

makeCsvFileTargetFunction <- function(csvFileName)
  makeDataFrameTargetFunction(read.csv(csvFileName))

Salutowicz1d <- defineTargetFunction(function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1), c(0, 10))
UnwrappedBall1d <- defineTargetFunction(function(x) 10/((x - 3)*(x - 3) + 5), c(-10, 10))
DampedOscillator1d <- defineTargetFunction(function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi), c(0, 10))

 
# main initial population objective space statistics driver function for twiddler...
#
twiddleInitialPopulationObjectiveSpaceStatistics <- function(targetFunctionName = "Salutowicz 1d",
                                                             csvFileName = "",
                                                             functionSetString = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")',
                                                             populationSize = 100,
                                                             randomSeed = 1,
                                                             removeInfFitnessValues = FALSE,
                                                             plotFront = TRUE,
                                                             plotHistograms = TRUE) {
  set.seed(randomSeed)

  targetFunction <- switch(targetFunctionName,
                         "Damped Oscillator 1d" = DampedOscillator1d,
                         "Salutowicz 1d" = Salutowicz1d,
                         "Unwrapped Ball 1d" = UnwrappedBall1d,
                         "CSV File" = makeCsvFileTargetFunction(csvFileName),
                         stop("twiddleSymbolicRegression: unkown test function name: ", targetFunctionName))
  domainInterval <- targetFunction$domainInterval
  targetFunctionSamplePoints <- seq(from = domainInterval[1], to = domainInterval[2],
                                  length.out = targetFunction$samples)
  fitnessCases <- data.frame(x1 = targetFunctionSamplePoints, y = targetFunction$f(targetFunctionSamplePoints))
  fitnessFunction <- makeRegressionFitnessFunction(y ~ x1, fitnessCases, environment(),
                                                   errorMeasure = smse,
                                                   indsizelimit = NA,
                                                   penalizeGenotypeConstantIndividuals = FALSE,
                                                   subSamplingShare = 1.0)

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

  if (length(dev.list()) == 0) {
    if (plotFront) dev.new() # create device for Pareto plot 
    if (plotHistograms) dev.new() # create device for progress plot
  }
  
  message("creating population of ", populationSize, " individuals...")
  population <- populationFactory(populationSize, funSet, inVarSet, 8, -10.0, 10.0)
  message("DONE.")
  message("calculating population fitness values...")
  populationFitnessValues <- as.numeric(sapply(population, fitnessFunction))
  populationFitnessValuesNA <- populationFitnessValues
  populationFitnessValuesNA[populationFitnessValuesNA == Inf] <- NA # replace Inf with NA
  numberOfInfFitnessValues <- length(populationFitnessValuesNA[is.na(populationFitnessValuesNA)])
  message("DONE.")
  message("calculating population complexity values...")
  populationComplexityValues <- as.numeric(sapply(population, funcVisitationLength))
  message("DONE.")

  if (plotFront) {
    oldDev <- dev.cur()
    dev.set(2)
    x <- populationFitnessValuesNA
    if (!removeInfFitnessValues) {
      x[is.na(x)] <- max(x, na.rm = TRUE) # replace NA values with max of non-NA values
      y <- populationComplexityValues
      col <- ifelse(is.na(populationFitnessValuesNA), "red", "black")
    } else {
      x <- na.omit(x) # remove NA values
      y <- populationComplexityValues[!is.na(populationFitnessValuesNA)]
      col <- "black"
    }
    plot(x = x, y = y, 
         main = "Population Pareto Plot", pch = 20, col = alpha(col, 0.1),
         xlab = "Fitness (SRMSE)", ylab = "Complexity (Visitation Length)")
    legend("topleft",
           c(paste("Fitness = +Inf (", numberOfInfFitnessValues / length(populationFitnessValues) * 100, " %)", sep = "")),
           pch = 20, col = c("red"))
    dev.set(oldDev)
  }
  if (plotHistograms) {
    oldDev <- dev.cur()
    dev.set(3)
    oldPar <- par(no.readonly = TRUE)
    par(mfrow = c(2, 1))
    hist(populationFitnessValues, xlab = "Fitness (SRMSE)", main = "Histogram of Population Fitness Values")
    hist(populationComplexityValues, xlab = "Complexity (Visitation Length)", main = "Histogram of Population Complexity Values")
#TODO
    par(oldPar)
    dev.set(oldDev)
  }

  return (NULL)
}

initialPopulationObjectiveSpaceStatisticsGui <- function() {
  twiddle(twiddleInitialPopulationObjectiveSpaceStatistics(targetFunctionName, csvFileName, functionSetString, populationSize, randomSeed, removeInfFitnessValues, plotFront, plotHistograms), eval = FALSE, label = "RGP Initial Population Objective Space Statistics",
          targetFunctionName = combo("Salutowicz 1d", "Unwrapped Ball 1d", "Damped Oscillator 1d", "CSV File"),
          csvFileName = filer(),
          plotFront = toggle(default = TRUE),
          plotHistograms = toggle(default = TRUE),
          removeInfFitnessValues = toggle(default = FALSE),
          populationSize = knob(lim = c(1, 10000), default = 1000, res = 100),
          functionSetString = entry(default = 'c("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")'),
          randomSeed = knob(lim = c(1, 1000), res = 1))
}

# Main entry point...
#
initialPopulationObjectiveSpaceStatisticsGui()

# EOF
