# svmKernelEvolution.r
# Evolution of SVM Kernels for the KernLab SVM implementation
# 2010 Oliver Flasch
#

library(rgp)


# Function-, input-, and constant sets for SVM kernels...
# ---
vectorDimension <- 3
`%+%` <- function(x, y) x + y
`%-%` <- function(x, y) x - y
`%s*%` <- function(x, y) x * y
`%*s%` <- function(x, y) x * y
`%s^%` <- function(x, y) x ^ y
`%v^%` <- function(x, y) x ^ y

typedSvmKernelConstantSet <- constantFactorySet((function() t(runif(1, -1, 1))) %::% (list() %->% st("numeric")),
                                                (function() runif(vectorDimension, -1, 1)) %::% (list() %->% st("numericVector")))
typedSvmKernelInputVariableSet <- inputVariableSet("x" %::% st("numericVector"),
                                                   "y" %::% st("numericVector"))
typedSvmKernelFunctionSet <- functionSet("exp" %::% (list(st("numeric")) %->% st("numeric")),
                                         "+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                         "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                         "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                         "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                         "%+%" %::% (list(st("numericVector"), st("numericVector")) %->% st("numericVector")),
                                         "%-%" %::% (list(st("numericVector"), st("numericVector")) %->% st("numericVector")),
                                         "%*%" %::% (list(st("numericVector"), st("numericVector")) %->% st("numeric")),
                                         "%s*%" %::% (list(st("numeric"), st("numericVector")) %->% st("numericVector")),
                                         "%*s%" %::% (list(st("numericVector"), st("numeric")) %->% st("numericVector")),
                                         "%s^%" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                         "%v^%" %::% (list(st("numericVector"), st("numeric")) %->% st("numericVector")))


# Evolution wrapper functions...
# ---
sizeFitnessFunction <- function(f) 0 - funcSize(f) # grow large functions for testing
vectorizedSizeFitnessFunction <- function(fs) sapply(fs, sizeFitnessFunction)

evolveSvmKernels <- function(fitnessFunction, stopCondition = makeTimeStopCondition(5))
  typedGeneticProgramming(fitnessFunction, st("numeric"),
                          functionSet = typedSvmKernelFunctionSet,
                          inputVariables = typedSvmKernelInputVariableSet,
                          constantSet = typedSvmKernelConstantSet,
                          population = makeTypedPopulation(100, st("numeric"),
                            typedSvmKernelFunctionSet, typedSvmKernelInputVariableSet, typedSvmKernelConstantSet,
                            maxfuncdepth = 3, constprob = 0.1),
                          selectionFunction = makeTournamentSelection(tournamentSize = 20, vectorizedFitness = TRUE),
                          stopCondition = stopCondition)
