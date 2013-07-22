#!/usr/bin/env Rscript

library("microbenchmark")
library("rgp")

# test code...

set.seed(3)
popSize <- 1000
maxDepth <- 12
xs <- 1:1000

funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")
#funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "sqrt") # log is handled by the fallback evaluator as .Primitive("log") has arity 2
inVarSet <- inputVariableSet("x1")
constSet <- numericConstantSet

populationFactory <- function(populationSize, funSet, inVarSet, maxfuncdepth, constMin, constMax) { 
  Map(function(i) makeClosure(.Call("initialize_expression_grow_R",
                                    as.list(funSet$nameStrings),
                                    as.integer(funSet$arities),
                                    as.list(inVarSet$nameStrings),
                                    constMin, constMax,
                                    0.8, 0.2,
                                    as.integer(maxfuncdepth)),
                              as.list(inVarSet$nameStrings)), 1:populationSize)
}

message("\n---- random population")
p1 <- populationFactory(popSize, funSet, inVarSet, maxDepth, -1, 1)
print(p1)

message("\n---- eval_vectorized_R with intermediate results on population")
gctorture(on = TRUE)
res1 <- Map(function(f) {
  res <- .Call("eval_vectorized_R", f, xs, TRUE)
  print(res)
  res
}, p1)
#print(res1)

#.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE)
#microbenchmark(.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE))

