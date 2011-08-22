## c_eval_test.r
##   - Functions for testing the C-based expression evalutors
##
## RGP - a GP system for R
## 2010-2011 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

require("rgp")
require("microbenchmark")
ompEnabled <- FALSE
cEnabled <- FALSE
if (file.exists("eval_vectorized_omp.so")) {
  dyn.load("eval_vectorized_omp.so")
  ompEnabled <- TRUE
} else if (file.exists("eval_vectorized.so")) {
  dyn.load("eval_vectorized.so")
  cEnabled <- TRUE
}
message("ompEnabled: ", ompEnabled, " cEnabled: ", cEnabled)


makeCRegressionFitnessFunction <- function(formula, data) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  explanatories <- lapply(explanatoryVariables, eval, envir=data)
  explanatoryVector <- Reduce(c, explanatories)
  function(ind) {
    ## TODO evalLoopRmse is not yet available
    .Call("evalLoopRmse", ind, names(formals(ind)), explanatoryVector, trueResponse)
  }
}

makeCVectorizedRegressionFitnessFunction <- function(formula, data) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  explanatories <- lapply(explanatoryVariables, eval, envir=data)
  explanatoryVector <- Reduce(c, explanatories)
  function(ind) {
    .Call("evalVectorizedRmse", ind, explanatoryVector, trueResponse)
  }
}

makeCVectorizedOmpRegressionFitnessFunction <- function(formula, data) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  explanatories <- lapply(explanatoryVariables, eval, envir=data)
  explanatoryVector <- Reduce(c, explanatories)
  function(ind) {
    .Call("evalVectorizedOmpRmse", ind, explanatoryVector, trueResponse)
  }
}

makeLoopRegressionFitnessFunction <- function(formula, data) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  numberOfFitnessCases <- length(trueResponse)
  explanatories <- lapply(explanatoryVariables, eval, envir=data)
  function(ind) {
    ysind <- numeric(numberOfFitnessCases)
    for (i in 1:numberOfFitnessCases) {
      ysind[[i]] <- ind(explanatories[[1]][i], explanatories[[2]][i]) # TODO hard coded arity
    }
    rmse(trueResponse, ysind)
  }
}

makeTestData <- function(testDataSize)
  data.frame(x1 = 1:testDataSize, x2 = 2 * (1:testDataSize),
             y = 0.3 * sin(1:testDataSize) + cos(0.1 * (1:testDataSize)) + 0.1 * (1:testDataSize) - 2)

benchmarkFitnessFunction <- function(fitnessFunctionFactory = makeRegressionFitnessFunction,
                                     fitnessCases = 256, times = 100L,
                                     ind = function(x1, x2) 0.2 * cos(x1 + 0.1) + sin(0.2 * x2) + 0.05 * x1 - 1.5,
                                     testData = makeTestData(fitnessCases),
                                     fitnessFunction = fitnessFunctionFactory(y ~ x1 + x2, testData)) {
  microbenchmark(fitnessFunction(ind), times = times)$time
}

benchmarkFitnessFunctions <- function(fitnessCases = 256, times = 100L,
                                      ind = function(x1, x2) 0.2 * cos(x1 + 0.1) + sin(0.2 * x2) + 0.05 * x1 - 1.5) {
  testData <- makeTestData(fitnessCases)
  rLoopRegressionFitnessFunction <- makeLoopRegressionFitnessFunction(y ~ x1 + x2, testData)
  rVectorizedRegressionFitnessFunction <- makeRegressionFitnessFunction(y ~ x1 + x2, testData)
  cVectorizedRegressionFitnessFunction <- makeCVectorizedRegressionFitnessFunction(y ~ x1 + x2, testData)
  cVectorizedOmpRegressionFitnessFunction <- makeCVectorizedOmpRegressionFitnessFunction(y ~ x1 + x2, testData)
  if (ompEnabled) {
    microbenchmark(rVectorizedRegressionFitnessFunction(ind),
                   cVectorizedOmpRegressionFitnessFunction(ind),
                   times = times)
  } else if (cEnabled) {
    microbenchmark(rVectorizedRegressionFitnessFunction(ind),
                   cVectorizedRegressionFitnessFunction(ind),
                   times = times)
  } else {
    microbenchmark(rVectorizedRegressionFitnessFunction(ind),
                   times = times)
  }
}
