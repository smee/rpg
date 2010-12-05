# typedGeneticProgramming.r
# based on the first tutorial found at TODO
# 2010 Oliver Flasch
#

library(rgp)
source("../skel/R/time_utils.r") # TODO
source("../skel/R/evolution.r") # TODO


# function- and constant sets for functions returning pairs of numbers
typedNumericPairFunctionSet <- functionSet("numericPair" %::% (list(st("numeric"), st("numeric")) %->% st("numericPair")))
typedMathPairFunctionSet <- c(typedMathFuncset, typedNumericPairFunctionSet)

typedNumericPairConstantSet <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                  ((function() numericPair(runif(1, -1, 1), runif(1, -1, 1))) %::% (list() %->% st("numericPair"))))

# test run
fitnessFunction1 <- function(f) 0 - funcSize(f) # grow large functions

numericPair <- function(a, b) list(a, b)

pop1 <- typedGeneticProgramming(fitnessFunction1, st("numericPair"),
                                functionSet = typedMathPairFunctionSet, constantSet = typedNumericPairConstantSet,
                                stopCondition = makeTimeStopCondition(240))
