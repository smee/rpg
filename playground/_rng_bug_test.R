## spotAlgStartRgpGMOGP.R
## 2013 Oliver Flasch
##

require("rgp")


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
         list("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt"),
         # TODO
         stop("buildingBlocksFromNumber: unkown building block set number: ", buildingBlockSetNumber))
}

errorMeasureFromName <- function(errorMeasureName) {
  switch(errorMeasureName,
         "SMSE" = smse,
         "SSSE" = ssse,
         "RMSE" = rmse,
         "SSE" = sse,
         "MAE" = mae,
         stop("errorMeasureFromName: unkown error measure name: ", errorMeasureName))
}

startRgpGMOGPExperiment <- function(problemParameters = list(data = NULL,
                                                             enableComplexityCriterion = TRUE,
                                                             symbolicRegressionFormula = NULL),
                                    algorithmParameters = list(buildingBlockSetNumber = 1L,
                                                               constantMutationProbability = 0.0,
                                                               crossoverProbability = 0.5,
                                                               enableAgeCriterion = TRUE,
                                                               errorMeasure = "SMSE",
                                                               functionMutationProbability = 0.0,
                                                               lambdaRel = 1.0,
                                                               mu = 100L,
                                                               nuRel = 0.5,
                                                               parentSelectionProbability = 1.0,
                                                               selectionFunction = "Crowding Distance",
                                                               subtreeMutationProbability = 1.0),
                                    experimentParameters = list(evaluations = 10e6L, # ten million fitness evaluations
                                                                populationSnapshots = 10L,
                                                                randomSeed = 1,
                                                                rdsOutputFileName = paste("rgpGMOGPresult", experimentParameters$randomSeed, sep = ""))) {
  # check parameters for problems...
  if (is.null(problemParameters$data)) stop("startRgpGMOGPExperiment: No valid input data.")
  if (is.null(problemParameters$symbolicRegressionFormula)) stop("startRgpGMOGPExperiment: No valid symbolic regression formula.")

  # transform relative to absolute parameters...
  algorithmParameters$buildingBlocks <- buildingBlocksFromNumber(algorithmParameters$buildingBlockSetNumber)
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

  mutationFunction <- function(ind) { # TODO
    #mutateSubtree(ind, funSet, inVarSet, numericConstantSet) # TODO
    #mutateFunc(ind, funSet)
    ind
  } # TODO

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

  errorMeasure  <- errorMeasureFromName(algorithmParameters$errorMeasure)

  ndsSelectionFunction <- switch(algorithmParameters$selectionFunction,
                                 "Crowding Distance" = nds_cd_selection,
                                 "Hypervolume" = nds_hv_selection,
                                 stop("startRgpGMOGPExperiment: unkown NDS selection function name: ", algorithmParameters$selectionFunction))

  searchHeuristic <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = algorithmParameters$lambda,
                                                                     crossoverProbability = algorithmParameters$crossoverProbability,
                                                                     newIndividualsPerGeneration = algorithmParameters$nu,
                                                                     enableComplexityCriterion = problemParameters$enableComplexityCriterion,
                                                                     enableAgeCriterion = algorithmParameters$enableAgeCriterion,
                                                                     ndsParentSelectionProbability = algorithmParameters$parentSelectionProbability,
                                                                     ndsSelectionFunction = ndsSelectionFunction)

  lastBestFitness <- Inf

  progressMonitor <- function(pop, objectiveVectors, fitnessFunction,
                              stepNumber, evaluationNumber, bestFitness, timeElapsed, indicesToRemove) {
    if (bestFitness != lastBestFitness) {
      message("XXXXX Strange: best fitness changed in evaluation ", evaluationNumber)
      lastBestFitness <<- bestFitness
    }

    if (evaluationNumber %% 10 == 0) {
      #print(bestFitness) # TODO
      #print(indicesToRemove) # TODO
      #print(pop) # TODO
    }
  }

  # set random seed
  message("startRgpGMOGPExperiment: SETTING random seed ", experimentParameters$randomSeed)
  set.seed(experimentParameters$randomSeed)
  
  # initialize population...
  message("startRgpGMOGPExperiment: INITIALIZING population")
  population <- populationFactory(algorithmParameters$mu, funSet, inVarSet, 8, -10.0, 10.0)

  # do genetic programming run...
  message("startRgpGMOGPExperiment: STARTING GP run")
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
                                            crossoverFunction = function(a, b, ...) a, # TODO 
                                            envir = environment(),
                                            verbose = FALSE,
                                            progressMonitor = progressMonitor))
  message("startRgpGMOGPExperiment: GP run done")

  # build result object...
  result <- list(symbolicRegressionResult = sr,
                 problemParameters = problemParameters,
                 algorithmParameters = algorithmParameters,
                 experimentParameters = experimentParameters)

  # save result to file...
  message("startRgpGMOGPExperiment: saved result to file '", experimentParameters$rdsOutputFileName, "'")
  saveRDS(result, file = experimentParameters$rdsOutputFileName)
  
  # return result 
  return (result)
}

# test code...
kotanchek <- function(x1, x2 ,A = -1, B = -2.5, C = 3.2) (exp (-1*(x1 + A)*(x1 + A))) / ((x2 + B)*(x2 + B) + 3.2) 
salustowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
salustowicz2d <- function(x1,x2) exp(-1*x1)*x1*x1*x1*(x2-5)*sin(x1)*cos(x1)*(sin(x1)*sin(x1)*cos(x1)-1)
unwrappedBall1d <- function(x) 10/((x - 3)*(x - 3) + 5)
rationalPolynom3d <- function(x1,x2,x3= 2) (30*(x1 - 1)*(x3 -1)) / ((x1 -10)*x2*x2)
sineCosine2d<- function(x1,x2) 6*sin(x1)*cos(x2)
ripple2d <- function(x1,x2) (x1-3)*(x2-3) + 2*sin((x1-4)*(x2-4))
ratPol2d <- function(x1,x2) ((x1-3)*(x1-3)*(x1-3)*(x1-3) + (x2-3)*(x2-3)*(x2-3) -x2 + 3) / ((x2-2)*(x2-2)*(x2-2)*(x2-2)+ 10)

result1 <- startRgpGMOGPExperiment(problemParameters = list(data = list(training = tabulateFunction(salustowicz1d, x = seq(1, 10, length.out = 100))),
                                                            enableComplexityCriterion = FALSE, # TRUE, TODO
                                                            symbolicRegressionFormula = y ~ x1),
                                  algorithmParameters = list(buildingBlockSetNumber = 1L,
                                                             constantMutationProbability = 0.0,
                                                             crossoverProbability = 0.0, # 0.5, TODO
                                                             enableAgeCriterion = FALSE, # TRUE, TODO
                                                             errorMeasure = "MAE", # "SMSE", TODO
                                                             functionMutationProbability = 0.0,
                                                             lambdaRel = 1.0,
                                                             mu = 8L, # 100L, TODO
                                                             nuRel = 0.0, # 0.5 TODO
                                                             parentSelectionProbability = 0.0, # 1.0, TODO
                                                             selectionFunction = "Crowding Distance",
                                                             subtreeMutationProbability = 1.0),
                                  experimentParameters = list(evaluations = 100000L,
                                                              populationSnapshots = 10L,
                                                              randomSeed = 1,
                                                              rdsOutputFileName = "salustowicz1dGMOGPresult_1.RDS"))

# eof

