## population.r
##   - Functions for handling GP populations
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Creates new populations of individuals represented as functions
##'
##' \code{makePopulation} creates a population of untyped individuals, whereas
##' \code{makeTypedPopulation} creates a population of typed individuals.
##'
##' @param size The population size in number of individuals.
##' @param type The (range) type of the individual functions to create.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param conset The set of constant factories.
##' @param maxfuncdepth The maximum depth of the functions of the new population.
##' @param funcfactory A factory for creating the functions of the new population.
##' @return A new population of functions.
##'
##' @rdname populationCreation
##' @export
makePopulation <- function(size, funcset, inset, conset,
                           maxfuncdepth = 8,
                           funcfactory = function() randfunc(funcset, inset, conset, maxfuncdepth))
  tabulateList(function(i) funcfactory(), size)

##' @rdname populationCreation
##' @export
makeTypedPopulation <- function(size, type, funcset, inset, conset,
                                maxfuncdepth = 8,
                                funcfactory = function() randfuncTyped(type, funcset, inset, conset, maxfuncdepth))
  makePopulation(size, funcset, inset, conset, maxfuncdepth, funcfactory)

##' Calculate the fitness value of each individual in a population
##'
##' @param pop A population of functions.
##' @param fitnessfunc The fitness function.
##' @return A list of fitness function values in the same order as \code{pop}.
##' @export
popfitness <- function(pop, fitnessfunc) sapply(pop, fitnessfunc)
