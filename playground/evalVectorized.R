#!/usr/bin/env Rscript

library("microbenchmark")
library("rgp")

deepDoCall <- function(f, args, envir = environment()) {
  results <- list()
  deepDoCallRecursive <- function(f, args, envir = environment()) {
    fBody <- body(f)
    if (is.call(fBody)) {
      operator <- fBody[[1]]
      newArgs <- Map(function(arg) { body(f) <- arg; deepDoCallRecursive(f, args, envir) }, fBody[-1])
      res <- do.call(as.character(operator), newArgs, envir = envir)
      results <<- c(results, list(res))
      res
    } else if (is.symbol(fBody)) {
      body(f) <- fBody
      res <- do.call(f, args, envir = envir)
      results <<- c(results, list(res))
      res
    } else {
      res <- fBody
      results <<- c(results, list(res))
      res
    }
  }
  deepDoCallRecursive(f, args, envir = envir)
  results
}

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

message("\n---- deepDoCall")
deepDoCall(f1, list(arg1, arg2))
microbenchmark(deepDoCall(f1, list(arg1, arg2)))

message("\n---- do.call")
do.call(f1, list(arg1, arg2))
microbenchmark(do.call(f1, list(arg1, arg2)))

message("\n---- eval_vectorized_R")
.Call("eval_vectorized_R", f1, c(arg1, arg2), FALSE)
microbenchmark(.Call("eval_vectorized_R", f1, c(arg1, arg2), FALSE))

message("\n---- eval_vectorized_R with intermediate results")
.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE)
microbenchmark(.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE))

