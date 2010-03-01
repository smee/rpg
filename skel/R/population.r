## population.r
##   - Functions for handling GP populations
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Classes for populations of individuals represented as functions
##'
##' \code{makePopulation} creates a population of untyped individuals, whereas
##' \code{makeTypedPopulation} creates a population of typed individuals.
##' \code{print.population} prints the population.
##' \code{summary.population} returns a summary view of a population.
##'
##' @param size The population size in number of individuals.
##' @param type The (range) type of the individual functions to create.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxfuncdepth The maximum depth of the functions of the new population.
##' @param funcfactory A factory for creating the functions of the new population.
##' @param object The population to summarize or print.

##' @return A new population of functions.
##'
##' @rdname populationCreation
##' @export
makePopulation <- function(size, funcset, inset, conset,
                           maxfuncdepth = 8,
                           funcfactory = function() randfunc(funcset, inset, conset, maxfuncdepth)) {
  pop <- tabulateList(function(i) funcfactory(), size)
  class(pop) <- c("untypedPopulation", "population", "list")
  pop
}

##' @rdname populationCreation
##' @export
makeTypedPopulation <- function(size, type, funcset, inset, conset,
                                maxfuncdepth = 8,
                                funcfactory = function() randfuncTyped(type, funcset, inset, conset, maxfuncdepth)) {
  pop <- makePopulation(size, funcset, inset, conset, maxfuncdepth, funcfactory)
  class(pop) <- c("typedPopulation", "population", "list")
  pop
}

##' @rdname populationCreation
##' @export
print.population <- function(x, ...) {
  print.default(x, ...) # TODO
}

##' @rdname populationCreation
##' @export
summary.population <- function(object, ...) {
  # TODO
  value <- list(class(object)[1], length(object))
  names(value) <- c("Class", "Size")
  class(value) <- "table"
  value
}

##' Fitness/Complexity plot for populations
##'
##' Plots the fitness against the complexity of each individual in a population.
##'
##' @param pop A population to plot.
##' @param fitnessFunction The function to calculate an individual's fitness with.
##' @param complexityFunction The function to calculate an individual's complexity with.
##' @param ... Additional parameters for the underlying call to \code{\link{plot}}.
plotPopulationFitnessComplexity <- function(pop, fitnessFunction, complexityFunction = funcVisitationLength, ...) {
  popFit <- as.vector(lapply(pop, fitnessFunction), mode = "numeric")
  popCpx <- as.vector(lapply(pop, complexityFunction), mode = "numeric")
  points <- cbind(popFit, popCpx)
  colnames(points) <- c("Fitness", "Complexity")
  plot(points, ...)
}

##' Calculate the fitness value of each individual in a population
##'
##' @param pop A population of functions.
##' @param fitnessfunc The fitness function.
##' @return A list of fitness function values in the same order as \code{pop}.
##' @export
popfitness <- function(pop, fitnessfunc) sapply(pop, fitnessfunc)
