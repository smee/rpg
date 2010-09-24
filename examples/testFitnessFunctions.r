# testFitnessFunction.r
# Fitness functions for regression tests...
# 2010 Oliver Flasch
#

sumfit <- function(f) { s <- abs(sum(Vectorize(f)(1:10))); if (is.nan(s) || is.infinite(s)) Inf else s }

maxsizefit <- function(f) 1 / funcSize(f)

minsizefit <- function(f) funcSize(f)
