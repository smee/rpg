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
f1 <- function(x, y) x * 3.14 + log(x + y)
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

