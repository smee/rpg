## rgpGmogpScreening.R
## 2013 Oliver Flasch
##

require("FrF2")
require("parallel")
require("rgp")


getPBSArrayJobID <- function()
  if ("" == Sys.getenv("PBS_ARRAYID")) {
    warning("getPBSArrayJobID: environment variable PBS_ARRAYID not set, returning ID 1")
    1
  } else {
    as.numeric(Sys.getenv("PBS_ARRAYID"))
  }

rescaleIndividual <- function(ind, srDataFrame, independentVariables, dependentVariable) {
  indX <- srDataFrame[, independentVariables]
  indY <- if (is.data.frame(indX)) apply(indX, 1, function(x) do.call(ind, as.list(x))) else ind(indX)
  trueY <- srDataFrame[, dependentVariable]
  indY <- if (length(indY) == 1) rep(indY, length(trueY)) else indY
  b = cov(trueY, indY) / var(indY)
  a = mean(trueY) - b * mean(indY)
  rescaledInd <- function(...) a + b * ind(...)
  return (rescaledInd)
}

buildingBlocksFromNumber <- function(buildingBlockSetNumber = 1L) {
  switch(buildingBlockSetNumber,
         list("+", "-", "*", "/"),
         list("+", "-", "*", "sin", "exp"),
         list("+", "-", "*", "/", "sin", "exp", "log", "sqrt"),
         list("+", "-", "*", "/", "sin", "cos", "tan", "exp", "log", "sqrt"),
         stop("buildingBlocksFromNumber: unkown building block set number: ", buildingBlockSetNumber))
}

errorMeasureFromNumber <- function(errorMeasureNumber) {
  switch(errorMeasureNumber,
         smse,
         rmse,
         sse,
         mae,
         stop("errorMeasureFromNumber: unkown error measure number: ", errorMeasureFromNumber))
}

#nds_hv_selection_fixed <- function(points, n, ...) {
#  selected <- integer() 
#  currentPoints <- points
#  for (i in 1:n) {
#    currentSelection <- nds_hv_selection(currentPoints, n = 1)
#    print("***") # TODO
#    print(ncol(currentPoints)) # TODO
#    print(currentSelection) # TODO
#    print("---") # TODO
#    selected <- c(selected, currentSelection)
#    currentPoints <- currentPoints[, -currentSelection] 
#  }
#  return (selected)
#}

#selectionFunctionFromNumber <- function(selectionFunctionNumber) {
#  switch(selectionFunctionNumber,
#         nds_cd_selection,
#         nds_hv_selection_fixed,
#         stop("selectionFunctionFromNumber: unkown selection function number: ", selectionFunctionNumber))
#}


startRgpGmogpExperiment <- function(problemParameters = list(data = NULL,
                                                             enableComplexityCriterion = TRUE,
                                                             symbolicRegressionFormula = NULL),
                                    algorithmParameters = list(buildingBlockSetNumber = 4L,        # Factor A RoI: {1L, ..., 4L}
                                                               constantMutationProbability = 0.0,  # Factor B RoI: [0, 1]
                                                               crossoverProbability = 0.5,         # Factor C RoI: [0, 1]
                                                               enableAgeCriterion = TRUE,          # Factor D RoI: |B
                                                               errorMeasureNumber = 1L,            # Factor E RoI: {1L, ..., 4L}
                                                               functionMutationProbability = 0.0,  # Factor F RoI: [0, 1]
                                                               lambdaRel = 1.0,                    # Factor G RoI: [0, 1]
                                                               mu = 100L,                          # Factor H RoI: {8L, ..., 256L}
                                                               nuRel = 0.5,                        # Factor J RoI: [0, 1]
                                                               parentSelectionProbability = 1.0,   # Factor K RoI: [0, 1]
                                                               #selectionFunctionNumber = 1L,       # Factor L RoI: {1L, 2L}
                                                               subtreeMutationProbability = 1.0),  # Factor M RoI: [0, 1]
                                    experimentParameters = list(evaluations = 10e6L, # ten million fitness evaluations
                                                                populationSnapshots = 10L,
                                                                randomSeed = 1,
                                                                rdsOutputFileName = paste("rgpGMOGPresult", experimentParameters$randomSeed, sep = ""))) {
  # check parameters for problems...
  if (is.null(problemParameters$data)) stop("startRgpGmogpExperiment: No valid input data.")
  if (is.null(problemParameters$symbolicRegressionFormula)) stop("startRgpGmogpExperiment: No valid symbolic regression formula.")

  # transform relative to absolute parameters...
  algorithmParameters$buildingBlocks <- buildingBlocksFromNumber(round(algorithmParameters$buildingBlockSetNumber))
  algorithmParameters$lambda <- max(2, ceiling(algorithmParameters$lambdaRel * algorithmParameters$mu / 2))
  algorithmParameters$nu <- ceiling(algorithmParameters$nuRel * algorithmParameters$mu)

  # extract symbolic regression formula and training data...
  srFormula <- as.formula(problemParameters$symbolicRegressionFormula)
  srDataFrame <- problemParameters$data$training

  # build building block sets...
  funSet <- do.call(functionSet, algorithmParameters$buildingBlocks)
  independentVariables <-  attr(terms(srFormula), "term.labels")
  inVarSet <- do.call(inputVariableSet, as.list(independentVariables))
  constSet <- numericConstantSet

  mutationFunction <- if (algorithmParameters$subtreeMutationProbability == 1 && algorithmParameters$functionMutationProbability == 0 && algorithmParameters$constantMutationProbability == 0) {
    function(ind) {
      subtreeMutantBody <- mutateSubtreeFast(body(ind), funSet, inVarSet, -10.0, 10.0, insertprob = 0.5, deleteprob = 0.5, subtreeprob = 1.0, constprob = 0.5, maxsubtreedepth = 8)
      makeClosure(subtreeMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else if (algorithmParameters$subtreeMutationProbability == 0 && algorithmParameters$functionMutationProbability == 1 && algorithmParameters$constantMutationProbability == 0) {
    function(ind) {
      functionMutantBody <- mutateFuncFast(body(ind), funSet, mutatefuncprob = 0.1)
      makeClosure(functionMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else if (algorithmParameters$subtreeMutationProbability == 0 && algorithmParameters$functionMutationProbability == 0 && algorithmParameters$constantMutationProbability == 1) {
    function(ind) {
      constantMutantBody <- mutateNumericConstFast(body(ind), mutateconstprob = 0.1, mu = 0.0, sigma = 1.0)
      makeClosure(constantMutantBody, inVarSet$all, envir = funSet$envir)
    }
  } else {
    function(ind) {
      mutantBody <- body(ind)
      weightSum <- algorithmParameters$subtreeMutationProbability + algorithmParameters$functionMutationProbability + algorithmParameters$constantMutationProbability
      rouletteWheelPosition <- runif(1, min = 0, max = weightSum)
      if (0 == weightSum) {
        return (ind)
      } else if (rouletteWheelPosition < algorithmParameters$subtreeMutationProbability) {
        mutantBody <- mutateSubtreeFast(mutantBody, funSet, inVarSet, -10.0, 10.0, insertprob = 0.5, deleteprob = 0.5, subtreeprob = 1.0, constprob = 0.5, maxsubtreedepth = 8)
      } else if (rouletteWheelPosition < algorithmParameters$subtreeMutationProbability + algorithmParameters$functionMutationProbability) {
        mutantBody <- mutateFuncFast(mutantBody, funSet, mutatefuncprob = 0.1)
      } else if (rouletteWheelPosition <= weightSum) {
        mutantBody <- mutateNumericConstFast(mutantBody, mutateconstprob = 0.1, mu = 0.0, sigma = 1.0)
      }
      makeClosure(mutantBody, inVarSet$all, envir = funSet$envir)
    }
  }

  populationFactory <- function(mu, funSet, inVarSet, maxfuncdepth, constMin, constMax) { 
    Map(function(i) makeClosure(.Call("initialize_expression_grow_R",
                                      as.list(funSet$nameStrings),
                                      as.integer(funSet$arities),
                                      as.list(inVarSet$nameStrings),
                                      constMin, constMax,
                                      0.8, 0.2,
                                      as.integer(maxfuncdepth)),
                                as.list(inVarSet$nameStrings)), 1:mu)
  }

  errorMeasure  <- errorMeasureFromNumber(round(algorithmParameters$errorMeasureNumber))

  #ndsSelectionFunction <- selectionFunctionFromNumber(round(algorithmParameters$selectionFunctionNumber))
  ndsSelectionFunction <- nds_cd_selection

  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = algorithmParameters$lambda,
                                                                     crossoverProbability = algorithmParameters$crossoverProbability,
                                                                     newIndividualsPerGeneration = algorithmParameters$nu,
                                                                     enableComplexityCriterion = problemParameters$enableComplexityCriterion,
                                                                     enableAgeCriterion = algorithmParameters$enableAgeCriterion,
                                                                     ndsParentSelectionProbability = algorithmParameters$parentSelectionProbability,
                                                                     ndsSelectionFunction = ndsSelectionFunction)

  snapshotInterval <- experimentParameters$evaluations / experimentParameters$populationSnapshots
  nextSnapshotAt <- snapshotInterval
  populationHistory <- list() 

  progressMonitor <- function(pop, objectiveVectors, fitnessFunction,
                              stepNumber, evaluationNumber, bestFitness, timeElapsed, indicesToRemove) {
    cat(".") # TODO debug output
    if (evaluationNumber >= nextSnapshotAt) {
      # save a snapshop of the current population
      populationHistory <<- c(list(list(stepNumber = stepNumber, timeElapsed = timeElapsed, population = pop, objectiveVectors = objectiveVectors)),
                              populationHistory)
      nextSnapshotAt <<- nextSnapshotAt + snapshotInterval
    }
  }

  # set random seed
  set.seed(experimentParameters$randomSeed)
  
  # initialize population...
  message("startRgpGmogpExperiment: INITIALIZING population")
  population <- populationFactory(algorithmParameters$mu, funSet, inVarSet, 8, -10.0, 10.0)

  # do genetic programming run...
  message("startRgpGmogpExperiment: STARTING GP run")
  sr <- suppressWarnings(symbolicRegression(srFormula,
                                            data = srDataFrame,
                                            functionSet = funSet,
                                            errorMeasure = errorMeasure,
                                            stopCondition = makeEvaluationsStopCondition(experimentParameters$evaluations),
                                            population = population,
                                            populationSize = algorithmParameters$mu,
                                            individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                                            searchHeuristic = searchHeuristic,
                                            mutationFunction = mutationFunction,
                                            envir = environment(),
                                            verbose = TRUE,
                                            progressMonitor = progressMonitor))
  message("startRgpGmogpExperiment: GP run done")

  # build result object...
  result <- list(symbolicRegressionResult = sr,
                 populationHistory = populationHistory,
                 problemParameters = problemParameters,
                 algorithmParameters = algorithmParameters,
                 experimentParameters = experimentParameters)

  # save result to file...
  message("startRgpGmogpExperiment: saved result to file '", experimentParameters$rdsOutputFileName, "'")
  saveRDS(result, file = experimentParameters$rdsOutputFileName)
  
  # return result 
  return (result)
}


#test target functions from http://symbolicregression.com ...
kotanchek <- function(x1, x2 ,A = -1, B = -2.5, C = 3.2) (exp (-1*(x1 + A)*(x1 + A))) / ((x2 + B)*(x2 + B) + 3.2) 
salustowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
salustowicz2d <- function(x1,x2) exp(-1*x1)*x1*x1*x1*(x2-5)*sin(x1)*cos(x1)*(sin(x1)*sin(x1)*cos(x1)-1)
unwrappedBall1d <- function(x) 10/((x - 3)*(x - 3) + 5)
rationalPolynom3d <- function(x1,x2,x3= 2) (30*(x1 - 1)*(x3 -1)) / ((x1 -10)*x2*x2)
sineCosine2d<- function(x1,x2) 6*sin(x1)*cos(x2)
ripple2d <- function(x1,x2) (x1-3)*(x2-3) + 2*sin((x1-4)*(x2-4))
ratPol2d <- function(x1,x2) ((x1-3)*(x1-3)*(x1-3)*(x1-3) + (x2-3)*(x2-3)*(x2-3) -x2 + 3) / ((x2-2)*(x2-2)*(x2-2)*(x2-2)+ 10)

salustowicz1dData <- tabulateFunction(salustowicz1d, x = seq(1, 10, length.out = 100))
salustowicz1dTrainingDataIndices <- sample(1:nrow(salustowicz1dData), size = 0.5 * nrow(salustowicz1dData), replace = FALSE)
salustowicz1dTrainingData <- salustowicz1dData[salustowicz1dTrainingDataIndices, ]
salustowicz1dValidationData <- salustowicz1dData[-salustowicz1dTrainingDataIndices, ]

rgpGmogpScreening <- function(seed = 1,
                              trainingData = salustowicz1dTrainingData,
                              repeats = 10,
                              evaluations = 1e6L,
                              experimentIndex = 1,
                              experimentName = "unnamed") {
  rgpGmogpScreeningDesign <- pb(
    nruns = 12,
    n12.taguchi = FALSE,
    nfactors = 11, # nruns - 1
    ncenter = 6, 
    replications = repeats, 
    repeat.only = TRUE,
    randomize = TRUE ,
    seed = seed ,
    factor.names = list( 
      buildingBlockSetNumber = c(1, 4),
      constantMutationProbability =c (0, 1),
      crossoverProbability = c(0, 1),
      enableAgeCriterion = c(0, 1),
      errorMeasureNumber = c(1, 4),
      functionMutationProbability = c(0, 1),
      lambdaRel = c(0, 1),
      mu = c(8, 256),
      nuRel = c(0, 1),
      parentSelectionProbability = c(0, 1),
      #selectionFunctionNumber = c(1, 2),
      subtreeMutationProbability = c(0, 1)))

  message("\n*** rgpGmogpScreening run started.")
  message("Using DoE with ", nrow(rgpGmogpScreeningDesign), " experiments:")
  print(rgpGmogpScreeningDesign)

  # get experiment according to PBS array job ID...
  experiment <- rgpGmogpScreeningDesign[experimentIndex, ]
  experimentRdsFileName <- paste(experimentName, "_", experimentIndex, ".RDS", sep = "")

  # run experiment and save results
  message("running rgpGmogpScreening experiment #", experimentIndex, ":")
  print(experiment)
  experimentResult <- startRgpGmogpExperiment(
    problemParameters = list(data = list(training = trainingData),
                             enableComplexityCriterion = TRUE,
                             symbolicRegressionFormula = y ~ x1),
    algorithmParameters = list(buildingBlockSetNumber = experiment$buildingBlockSetNumber,
                               constantMutationProbability = experiment$constantMutationProbability,
                               crossoverProbability = experiment$crossoverProbability,
                               enableAgeCriterion = experiment$enableAgeCriterion,
                               errorMeasureNumber = experiment$errorMeasureNumber,
                               functionMutationProbability = experiment$functionMutationProbability,
                               lambdaRel = experiment$lambdaRel,
                               mu = experiment$mu,
                               nuRel = experiment$nuRel,
                               parentSelectionProbability = experiment$parentSelectionProbability,
                               #selectionFunctionNumber = experiment$selectionFunctionNumber,
                               subtreeMutationProbability = experiment$subtreeMutationProbability),
    experimentParameters = list(evaluations = evaluations,
                                populationSnapshots = 10L,
                                randomSeed = seed + experimentIndex,
                                rdsOutputFileName = experimentRdsFileName))

  message("rgpGmogpScreening run DONE.")
  return (experimentResult)
}


# run experiment...
arrayJobID <- getPBSArrayJobID()
rgpGmogpScreening(seed = arrayJobID,
                  experimentName = "salustowicz1d",
                  experimentIndex = arrayJobID,
                  trainingData = salustowicz1dTrainingData,
                  repeats = 10,
                  evaluations = 10000L)

