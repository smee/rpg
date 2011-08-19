## c_eval_test.r
##   - Functions for testing the C-based expression evalutors
##
## RGP - a GP system for R
## 2010-2011 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

require("rgp")
dyn.load("c_eval_vectorized.so")


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
    ##ysind <- do.call(ind, explanatories) # vectorized fitness-case evaluation
    ##rmse(trueResponse, ysind)
    .Call("CevalWrapper", ind, names(formals(ind)), explanatoryVector, trueResponse)
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
    ##ysind <- do.call(ind, explanatories) # vectorized fitness-case evaluation
    ##rmse(trueResponse, ysind)
    .Call("CevalVectorizedWrapper", ind, names(formals(ind)), explanatoryVector, trueResponse)
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
