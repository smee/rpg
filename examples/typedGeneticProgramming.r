# typedGeneticProgramming.r
# based on the first tutorial found at TODO
# 2010 Oliver Flasch
#

library(rgp)
source("../skel/R/time_utils.r") # TODO
source("../skel/R/evolution.r") # TODO


# Some general typed function- and constant sets...
# ---
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

typedLogicalFunctionSet <- functionSet("<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                       ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                       "==" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                       "ifThenElse" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")),
                                       "&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                       "!" %::% (list(st("logical")) %->% st("logical")))

typedMathLogicalFunctionSet <- c(typedMathFunctionSet, typedLogicalFunctionSet)

typedHigherOrderVectorFunctionSet <- functionSet("sapply" %::% (list(st("numeric"), list(st("numeric")) %->% st("numeric")) %->% st("numeric")),
                                                 "mean" %::% (list(st("numeric")) %->% st("numeric")))


# Function- and constant sets for functions returning pairs of numbers...
# ---
typedNumericPairConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                  ((function() numericPair(runif(1, -1, 1), runif(1, -1, 1))) %::% (list() %->% st("numericPair"))))
typedNumericPairFunctionSet <- functionSet("numericPair" %::% (list(st("numeric"), st("numeric")) %->% st("numericPair")))
typedMathPairFunctionSet <- c(typedMathFunctionSet, typedNumericPairFunctionSet)
typedNumericInputVariableSet <- inputVariableSet("x" %::% st("numeric"))


# Function-, input-, and constant sets for SVM kernels...
# ---
vectorDimension <- 3
`%+%` <- function(x, y) x + y
`%-%` <- function(x, y) x - y

typedVectorScalarConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                   (function() runif(vectorDimension, -1, 1)) %::% (list() %->% st("numericVector")))
typedVectorInputVariableSet <- inputVariableSet("x" %::% st("numericVector"))
typedVectorScalarFunctionSet <- functionSet("%+%" %::% (list(st("numericVector"), st("numericVector")) %->% st("numericVector")),
                                            "%-%" %::% (list(st("numericVector"), st("numericVector")) %->% st("numericVector")))


# Test runs...
# ---
fitnessFunction1 <- function(f) 0 - funcSize(f) # grow large functions

numericPair <- function(a, b) list(a, b)

#pop1 <- typedGeneticProgramming(fitnessFunction1, st("numericPair"),
#                                functionSet = typedMathPairFunctionSet, constantSet = typedNumericPairConstantSet,
#                                stopCondition = makeTimeStopCondition(240))
