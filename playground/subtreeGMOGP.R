#!/usr/bin/env Rscript

library("microbenchmark")
library("rgp")


# Test target functions...
#
defineTargetFunction <- function(f, domainInterval = c(0, 1), dim = 1, samples = 100)
  structure(list(f = f, domainInterval = domainInterval, dim = dim, samples = samples), class = "targetFunction")
  
Salutowicz1d <- defineTargetFunction(function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1), c(0, 10))
UnwrappedBall1d <- defineTargetFunction(function(x) 10/((x - 3)*(x - 3) + 5), c(-10, 10))
DampedOscillator1d <- defineTargetFunction(function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi), c(0, 10))

# Test code...
#
set.seed(3)
maxDepth <- 12 
xs1 <- 1:100
xs2 <- 1:100

funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")
#funSet <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "sqrt") # log is handled by the fallback evaluator as .Primitive("log") has arity 2
#inVarSet <- inputVariableSet("x1", "x2")
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

p1 <- populationFactory(50, funSet, inVarSet, maxDepth, -1, 1)

f1 <- populationFactory(1, funSet, inVarSet, maxDepth, -1, 1)[[1]]
#f1 <- function(x, y) x * 3.14 + log(x + y)
arg1 <- (1:100) + 0.25
#arg2 <- (1:100) - 0.5

message("---- f1")
print(f1)

message("\n---- eval_vectorized_R with intermediate results")
#.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE)
#microbenchmark(.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE))
microbenchmark(.Call("eval_vectorized_R", f1, arg1, TRUE))

message("\n---- func_visitation_length_R with intermediate results")
#gctorture(on = TRUE)
#as.integer(.Call("func_visitation_length_R", f1, TRUE))
microbenchmark(.Call("func_visitation_length_R", f1, TRUE))

message("\n---- eval_vectorized_R on population with intermediate results")
#microbenchmark(results <- Map(function(f) .Call("eval_vectorized_R", f, c(arg1, arg2), TRUE), p1))
microbenchmark(results <- Map(function(f) .Call("eval_vectorized_R", f, arg1, TRUE), p1))

message("\n---- func_visitation_length_R on population with intermediate results")
microbenchmark(complexities <- Map(function(f) .Call("func_visitation_length_R", f, TRUE), p1))

makeSmseFitnessFunction <- function(targetFunction) {
  xs <- seq(from = targetFunction$domainInterval[1], to = targetFunction$domainInterval[2], length.out = targetFunction$samples)
  trueYs <- targetFunction$f(xs)
  function(ind) mapply(function(ys) smse(trueYs, ys), .Call("eval_vectorized_R", ind, xs, TRUE)) # vector of SMSE of each subtree of ind
}

evalPopulation <- function(population, fitnessFunction = makeSmseFitnessFunction(Salutowicz1d)) { # TODO
  fitnesses <- mapply(fitnessFunction, population)
  complexities <- mapply(function(f) .Call("func_visitation_length_R", f, TRUE), population)
  do.call(cbind, mapply(function(subtreeFitnesses, subtreeComplexities, i)
                          mapply(function(subtreeFitness, subtreeComplexity, j)
                                   c(i, j, subtreeFitness, subtreeComplexity),
                                 subtreeFitnesses, subtreeComplexities, seq_along(subtreeFitnesses)),
                        fitnesses, complexities, seq_along(fitnesses)))
}

#do.call(cbind, mapply(function(subtreeResults, subtreeComplexity, i) mapply(function(subtreeResult, subtreeComplexity, j) list(i, j, mean(subtreeResult), subtreeComplexity), subtreeResults, subtreeComplexity, seq_along(subtreeResults)), results, complexities, seq_along(results)))
#print(results)
#unlist(Map(function(result, i) Map(function(subtree, j) list(subtree, i, j), result, seq_along(result)), results, seq_along(results)), recursive=FALSE)
#t(sapply(unlist(Map(function(result, i) Map(function(subtree, j) list(median(subtree), i, j), result, seq_along(result)), results, seq_along(results)), recursive=FALSE), rbind))
#do.call(cbind, mapply(function(subtreeResults, subtreeComplexity, i) mapply(function(subtreeResult, subtreeComplexity, j) list(i, j, mean(subtreeResult), subtreeComplexity), subtreeResults, subtreeComplexity, seq_along(subtreeResults)), results, complexities, seq_along(results)))
#.Call("get_sexp_subtree_R", body(p1[[30]]), 86L)

