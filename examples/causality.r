# causality.r
# Experiments that show the lack of causality in standard GP variation operators.
# 2010 Oliver Flasch
#

require(rgp)


makeRandomIndividual <- function(functions = arithmeticFunctionSet,
                                 inputVariables = inputVariableSet("x"),
                                 constantFactories = numericConstantSet,
                                 maxDepth = 5, expressionFactory = randexprFull)
  randfunc(functions, inputVariables, constantFactories, maxDepth, expressionFactory)

univariateRmse <- function(f1, f2, xs = seq(1, 10, by = 0.1)) {
  ys1 <- Vectorize(f1)(xs)
  ys2 <- Vectorize(f2)(xs)
  rmse(ys1, ys2)
}

sampleIndividualDistances <- function(n = 100,
                                      individualFactory = makeRandomIndividual,
                                      mutationFunction = function(ind) makeRandomIndividual(),
                                      distanceMeasure = univariateRmse) {
  rlen <- 0
  r <- c()
  while (rlen < n) {
    ind1 <- individualFactory()
    ind2 <- mutationFunction(ind1)
    d <- distanceMeasure(ind1, ind2)
    if (!is.nan(d) && !is.infinite(d)) { # filter infinite and nan distances
      r <- c(r, d)
      rlen <- rlen + 1
    }
  }
  r
}

individualDistancesExperiment <- function(n = 100, maxDepth = 5) {
  indFac <- function() makeRandomIndividual(maxDepth = maxDepth)
  dsRandomInds <- sampleIndividualDistances(n = n, individualFactory = indFac)
  dsMutateConstants <- sampleIndividualDistances(n = n, individualFactory = indFac,
                                                 mutationFunction = function(ind) mutateNumericConst(ind, mutateconstprob=0.1))
  dsMutateFunctions <- sampleIndividualDistances(n = n, individualFactory = indFac,
                                                 mutationFunction = function(ind) mutateFunc(ind, arithmeticFunctionSet, mutatefuncprob=0.1))
  dsMutateSubtress <- sampleIndividualDistances(n = n, individualFactory = indFac,
                                                mutationFunction = function(ind) mutateSubtree(ind, arithmeticFunctionSet, inputVariableSet("x"), numericConstantSet))
  allDs <- data.frame(list(dsRandomInds = dsRandomInds, dsMutateConstants = dsMutateConstants, dsMutateFunctions = dsMutateFunctions, dsMutateSubtress = dsMutateSubtress))
  allDs
}

# Experiment runs...
#system.time(resultsDepth8 <- individualDistancesExperiment(500, maxDepth=8))
#system.time(resultsDepth6 <- individualDistancesExperiment(500, maxDepth=6))
#system.time(resultsDepth4 <- individualDistancesExperiment(500, maxDepth=4))
#system.time(resultsDepth2 <- individualDistancesExperiment(500, maxDepth=2))
