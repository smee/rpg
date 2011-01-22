# breeding.r
# examples for using breeding functions to force constraints on GP individuals
# 2011 Oliver Flasch
#

require(rgp)


# Function-, input-, and constant sets for SVM kernels...
# ---
constantVectorDimension <- 1 # the dimension of vector-valued kernel constants
`%+%` <- function(x, y) x + y
`%-%` <- function(x, y) x - y
`%s*%` <- function(x, y) x * y
`%*s%` <- function(x, y) x * y
`%s^%` <- function(x, y) x ^ y
`%v^%` <- function(x, y) x ^ y

typedSvmKernelConstantSet <- constantFactorySet((function() t(runif(1, -1, 1))) %::% (list() %->% st("numeric")),
                                                (function() runif(constantVectorDimension, -1, 1)) %::% (list() %->% st("numericVector")))
typedSvmKernelInputVariableSet <- inputVariableSet(quote((x %+% y) %^% 2) %::% st("numericVector"),
                                                   quote(x %*% y) %::% st("numericVector"))
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


# Fitness functions...
# ---
sizeFitnessFunction <- function(f) 0 - funcSize(f) # grow large functions for testing


# Evolution wrappers and breeding demo...
# ---

breed <- function(ind) {
  message("Breeding...")
  if (runif(1) > 0.5) { message("   ...TRUE."); TRUE } else { message("   ...FALSE."); FALSE } 
}

evolveSvmKernels <- function(fitnessFunction, stopCondition = makeTimeStopCondition(5),
                             breedingFitness = breed, breedingTries = 50)
  typedGeneticProgramming(fitnessFunction, st("numeric"),
                          functionSet = typedSvmKernelFunctionSet,
                          inputVariables = typedSvmKernelInputVariableSet,
                          constantSet = typedSvmKernelConstantSet,
                          population = makeTypedPopulation(100, st("numeric"),
                            typedSvmKernelFunctionSet, typedSvmKernelInputVariableSet, typedSvmKernelConstantSet,
                            maxfuncdepth = 3, constprob = 0.1),
                          stopCondition = stopCondition,
                          breedingFitness = breedingFitness, breedingTries = breedingTries)

# evolveSvmKernels(sizeFitnessFunction)



