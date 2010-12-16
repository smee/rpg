# typedGeneticProgramming.r
# based on the first tutorial found at TODO
# 2010 Oliver Flasch
#

library(rgp)


# Some general typed function- and constant sets...
# ---
typedLogicalConstantSet <- constantFactorySet((function() runif(1) > .5) %::% (list() %->% st("logical")))

typedNumericLogicalConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                     (function() runif(1) > .5) %::% (list() %->% st("logical")))

typedArithmeticFunctionSet <- functionSet("+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                          "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")))

typedExpLogFunctionSet <- functionSet("sqrt" %::% (list(st("numeric")) %->% st("numeric")),
                                      "exp" %::% (list(st("numeric")) %->% st("numeric")),
                                      "ln" %::% (list(st("numeric")) %->% st("numeric")))

typedTrigonometricFunctionSet <- functionSet("sin" %::% (list(st("numeric")) %->% st("numeric")),
                                             "cos" %::% (list(st("numeric")) %->% st("numeric")),
                                             "tan" %::% (list(st("numeric")) %->% st("numeric")))

typedMathFunctionSet <- c(typedArithmeticFunctionSet, typedExpLogFunctionSet, typedTrigonometricFunctionSet)

typedBooleanFunctionSet <- functionSet("&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "!" %::% (list(st("logical")) %->% st("logical")))

typedLogicalFunctionSet <- c(typedBooleanFunctionSet,
                             functionSet("<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         "==" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                         "ifThenElse" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric"))))

typedMathLogicalFunctionSet <- c(typedMathFunctionSet, typedLogicalFunctionSet)

typedHigherOrderVectorFunctionSet <- functionSet("sapply" %::% (list(st("numeric"), list(st("numeric")) %->% st("numeric")) %->% st("numeric")),
                                                 "mean" %::% (list(st("numeric")) %->% st("numeric")))


# Function- and constant sets for functions returning pairs of numbers...
# ---
numericPair <- function(a, b) list(a, b)

typedNumericPairConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                  ((function() numericPair(runif(1, -1, 1), runif(1, -1, 1))) %::% (list() %->% st("numericPair"))))
typedNumericPairFunctionSet <- functionSet("numericPair" %::% (list(st("numeric"), st("numeric")) %->% st("numericPair")))
typedMathPairFunctionSet <- c(typedMathFunctionSet, typedNumericPairFunctionSet)
typedNumericInputVariableSet <- inputVariableSet("x" %::% st("numeric"))
typedBooleanInputVariableSet <- inputVariableSet("a" %::% st("logical"), "b" %::% st("logical"), "c" %::% st("logical"))


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


# Fitness functions...
# ---
sizeFitnessFunction <- function(f) 0 - funcSize(f) # grow large functions for testing

# integerToBoolean
# convert a scalar positive integer (or zero) to its binary representation as list of logicals
integerToLogicals <- function(i, width = floor(log(base = 2, i) + 1)) {
  k <- i
  result <- list()
  l <- 0
  while (k != 0) {
    bit <- as.logical(k %% 2)
    result <- c(bit, result)
    k <- k %/% 2
    l <- l + 1
  }
  if (width - l < 0) stop("integerToLogicals: width to small")
  c(replicate(width - l, FALSE), result) # prefix with zeros
}

# booleanFunctionVector
# given a boolean function f, returns the boolean vector of result values of f
booleanFunctionAsList <- function(f) {
  fArity <- length(formals(f))
  inputs <- Map(function(i) integerToLogicals(i, width = fArity), 0:(2 ^ fArity - 1))
  Map(function(input) do.call(f, input), inputs)
}

# numberOfDifferentBits
# given two lists of booleans of equal length, returns the number of differing bits
numberOfDifferentBits <- function(a, b) {
  Reduce(`+`, Map(function(ai, bi) if (ai != bi) 1 else 0, a, b))
}

# makeBooleanFitnessFunction:
# given a boolean target function, returns a fitness function that returns the number of
# different places in the output of a given boolean function and the target function
makeBooleanFitnessFunction <- function(targetFunction) {
  targetFunctionList <- booleanFunctionAsList(targetFunction)
  function(f) {
    fList <- booleanFunctionAsList(f)
    numberOfDifferentBits(fList, targetFunctionList)
  }
}


# Evolution wrapper functions...
# ---
evolveFeaturePairs <- function(fitnessFunction, stopCondition = makeTimeStopCondition(5))
  typedGeneticProgramming(fitnessFunction, st("numericPair"),
                          functionSet = typedMathPairFunctionSet,
                          inputVariables = typedNumericInputVariableSet,
                          constantSet = typedNumericPairConstantSet,
                          stopCondition = stopCondition)

evolveBooleanFunction <- function(fitnessFunction, stopCondition = makeTimeStopCondition(30))
  typedGeneticProgramming(fitnessFunction, st("logical"),
			  functionSet = typedBooleanFunctionSet,
			  inputVariables = typedBooleanInputVariableSet,
			  constantSet = typedLogicalConstantSet,
			  stopCondition = stopCondition)

evolveSvmKernels <- function(fitnessFunction, stopCondition = makeTimeStopCondition(5))
  typedGeneticProgramming(fitnessFunction, st("numeric"),
                          functionSet = typedSvmKernelFunctionSet,
                          inputVariables = typedSvmKernelInputVariableSet,
                          constantSet = typedSvmKernelConstantSet,
                          population = makeTypedPopulation(100, st("numeric"),
                            typedSvmKernelFunctionSet, typedSvmKernelInputVariableSet, typedSvmKernelConstantSet,
                            maxfuncdepth = 3, constprob = 0.1),
                          stopCondition = stopCondition)

# Test - Function
parity <- function(x) {
  numberOfOnes <- sum(sapply(x, function(bit) if (bit) 1 else 0))
  numberOfOnes %% 2 != 0 
}

# Wrapper

parity3 <- function(x1,x2,x3) parity(c(x1,x2,x3))

# Fitness function

parityFitnessFunction <- makeBooleanFitnessFunction(parity3)

