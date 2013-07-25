#!/usr/bin/env Rscript

library("microbenchmark")
library("rgp")

# test code...
set.seed(3)
maxDepth <- 12 
xs1 <- 1:100
xs2 <- 1:100

funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")
#funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "sqrt") # log is handled by the fallback evaluator as .Primitive("log") has arity 2
inVarSet <- inputVariableSet("x1", "x2")
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

f1 <- populationFactory(1, funSet, inVarSet, maxDepth, -1, 1)[[1]]
#f1 <- function(x, y) x * 3.14 + log(x + y)
arg1 <- (1:100) + 0.25
arg2 <- (1:100) - 0.5

message("---- f1")
print(f1)

message("\n---- funcVisitationLength")
funcVisitationLength(f1, intermediateResults = FALSE)
microbenchmark(funcVisitationLength(f1, intermediateResults = FALSE))

message("\n---- funcVisitationLength with intermediate results")
funcVisitationLength(f1, intermediateResults = TRUE)
microbenchmark(funcVisitationLength(f1, intermediateResults = TRUE))

message("\n---- func_visitation_length_R")
.Call("func_visitation_length_R", f1, FALSE)
microbenchmark(.Call("func_visitation_length_R", f1, FALSE))

message("\n---- func_visitation_length_R with intermediate results")
#gctorture(on = TRUE)
as.integer(.Call("func_visitation_length_R", f1, TRUE))
microbenchmark(.Call("func_visitation_length_R", f1, TRUE))

