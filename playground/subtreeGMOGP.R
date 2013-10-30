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
maxDepth <- 8 
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
                                    1.0, 0.2, # 1.0 subtree probability => initialize_expression_full_R
                                    as.integer(maxfuncdepth)),
                              as.list(inVarSet$nameStrings)), 1:populationSize)
}

p1 <- populationFactory(50, funSet, inVarSet, maxDepth, -1, 1)

f1 <- populationFactory(1, funSet, inVarSet, maxDepth, -1, 1)[[1]]
#f1 <- function(x, y) x * 3.14 + log(x + y)
arg1 <- (1:100) + 0.25
arg1Long <- (1:10000) + 0.25
#arg2 <- (1:100) - 0.5

message("---- f1")
print(f1)

message("\n---- eval_vectorized_R with intermediate results")
#.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE)
#microbenchmark(.Call("eval_vectorized_R", f1, c(arg1, arg2), TRUE))
timeSamples <- 20 
timesTaken1 <- mapply(function(i) {
  subArg1 <- arg1Long[1:(length(arg1Long) / timeSamples * i)]
  timeTaken <- microbenchmark(.Call("eval_vectorized_R", f1, subArg1, TRUE), times = 1000L, warmup = 10L)
  timeTaken$time / 1000
}, 1:timeSamples)
boxplot(timesTaken1, main = "eval_vectorized_R Performance (1000 Samples)", xlab = "Input Vector Length * 500", ylab = "Compute Time (ms)")

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

extractIndexedSubtreesFromPopulation <- function(population, evalResults) {
  populationIndices <- evalResults[1,]
  subtreeIndices <- evalResults[2,]
  mapply(function(i, j) {
    subtree <- population[[i]]
    body(subtree) <- .Call("get_sexp_subtree_R", body(subtree), as.integer(j - 1))
    subtree
  },
  evalResults[1,], evalResults[2,])
}

#do.call(cbind, mapply(function(subtreeResults, subtreeComplexity, i) mapply(function(subtreeResult, subtreeComplexity, j) list(i, j, mean(subtreeResult), subtreeComplexity), subtreeResults, subtreeComplexity, seq_along(subtreeResults)), results, complexities, seq_along(results)))
#print(results)
#unlist(Map(function(result, i) Map(function(subtree, j) list(subtree, i, j), result, seq_along(result)), results, seq_along(results)), recursive=FALSE)
#t(sapply(unlist(Map(function(result, i) Map(function(subtree, j) list(median(subtree), i, j), result, seq_along(result)), results, seq_along(results)), recursive=FALSE), rbind))
#do.call(cbind, mapply(function(subtreeResults, subtreeComplexity, i) mapply(function(subtreeResult, subtreeComplexity, j) list(i, j, mean(subtreeResult), subtreeComplexity), subtreeResults, subtreeComplexity, seq_along(subtreeResults)), results, complexities, seq_along(results)))
#.Call("get_sexp_subtree_R", body(p1[[30]]), 86L)
#r1 <- evalPopulation(p1)
#plot(x=log(r1[3,], base=10), y=log(r1[4,], base=10), col="#00000044", xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r1)))
#plot(x=log(r1[3,], base=10), y=log(r1[4,], base=10), col="#00000044", xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r1))); r2 <- r1[3:4, -nds_cd_selection(r1[3:4,], ncol(r1)-7000)];  points(x=log(r2[1,],base=10),y=log(r2[2,],base=10), pch=0, col="green")
#r3 <- unique(r1[3:4,], MARGIN=2); plot(x=log(r3[1,], base=10), y=log(r3[2,], base=10), col=1, xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r3))); r4 <- r3[, -nds_cd_selection(r3, ncol(r3)-10)];  points(x=log(r4[1,],base=10),y=log(r4[2,],base=10), pch=0, col="green")
#r3 <- unique(r1[3:4,], MARGIN=2); plot(x=log(r3[1,], base=10), y=log(r3[2,], base=10), col=1, xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r3))); r4 <- r3[, nds_rank(r3) <= 6];  points(x=log(r4[1,],base=10),y=log(r4[2,],base=10), pch=0, col="green")
#r3 <- unique(r1[3:4,], MARGIN=2); r3 <- r3[,!is.na(r3[1,])]; pal1 <- colorRampPalette(c("green", "cyan", "yellow", "orange", "red", "blue", "black"), bias=4)(length(unique(nds_rank(r3)))); plot(x=log(r3[1,], base=10), y=log(r3[2,], base=10), col=pal1[nds_rank(r3)], xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r3)))
#r3 <- unique(r1[3:4,], MARGIN=2); r3[,is.na(r3[1,])] <- Inf; pal1 <- colorRampPalette(c("green", "cyan", "yellow", "orange", "red", "blue", "black"), bias=4)(length(unique(nds_rank(r3)))); plot(x=log(r3[1,], base=10), y=log(r3[2,], base=10), col=pal1[nds_rank(r3)], xlab="fitness (log10 SMSE)", ylab="complexity (log10 visitation length)", main=sprintf("Objective Space Plot for %d Subtrees", ncol(r3)))

