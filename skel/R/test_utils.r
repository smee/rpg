## test_utils.r
##   - Utility functions for testing and benchmarking RGP
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Measure the number of fitness evaluations per second performed by RGP.
##'
##' Measure the number of fitness evaluations per second performed by
##' \code{\link{geneticProgramming}}. A number of \code{samples} experiments
##' are performed.
##'
##' @param fitnessFunction The fitness function to pass to the call to
##' \code{\link{geneticProgramming}}.
##' @param samples The number of indpendent measurements to perform.
##' @param ... Options as passed to the call to \code{\link{geneticProgramming}}.
##' @return The number of fitness evaluations per second performed by RGP.
rgpBenchmark <- function(fitnessFunction = function(ind) 0, samples = 1, ...) {
  as.numeric(lapply(1:samples, function(i) {
    fitnessEvaluations <- 0
    gpm <- geneticProgramming(function(ind) { fitnessEvaluations <<- fitnessEvaluations + 1; fitnessFunction(ind) },
                              stopCondition = makeTimeStopCondition(10), ...)
    fitnessEvaluations / 10
  }))
}
