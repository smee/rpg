## selection.r
##   - GP selection strategies
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' GP selection functions
##'
##' A GP selection function determines which individuals in a population should
##' survive, i.e. are selected for variation or cloning, and which individuals
##' of a population should be replaced. Single-objective selection functions base
##' their selection decision only on a single fitness function, whereas
##' multi-objective selection functions support multiple fitness functions.
##' Every selection function takes a population and a (list of) fitness
##' functions as required arguments. It returns a list of two tables \code{selected}
##' and \code{discarded}, with columns \code{index} and \code{fitness} each.
##' The first table contains the population indices of the individuals selected as
##' survivors, the second table contains the population indices of the individuals
##' that should be discarded and replaced. This definition simplifies the implementation
##' of \emph{steady-state} evolutionary strategies where most of the individuals of a
##' population are unchanged in each selection step. In a GP context, these strategies
##' are often more efficient than generational strategies. 
##'
##' \code{makeTournamentSelection} returns a classic single-objective tournament selection
##'   function.
##' \code{makeParetoTournamentSelection} returns a multi-objective tournament selection
##'   function that selects individuals on the pareto front.
##' \code{makeComplexityTournamentSelection} returns a multi-objective selection function that
##'   implements the common case of dual-objective tournament selection with high solution
##'   quality as the first objective and low solution complexity as the second objective.
##'
##' @param population The population to select from. All indices returned refer to
##'   this population.
##' @param fitnessFunction Either a single function or a list of functions to be used for
##'   measuring the performance of an individual. If a list of functions is given to a
##'   single-objective selection function, only the first function will be used.
##' @param complexityMeasure The function used to measure the complexity of an individual.
##' @param tournamentSize The number of individuals to randomly select to form a
##'   tournament, defaults to 10 in the single-objective case, 30 in the multi-objective case.
##' @param selectionSize The number of individuals to return as selected.
##' @param tournamentDeterminism The propability \emph{p} for selecting the best individual
##'   in a tournament, must be in the interval (0.0, 1.0]. The best individual is selected
##'   with propability \emph{p}, the second best individual is selected with propability
##'   \emph{p * (1 - p)}, the third best individual ist selected with propability
##'   \emph{p * (1 - p)^2}, and so on. Note that setting \code{tournamentDeterminism}
##'   to \code{1.0} (the default) yields determistic behavior.
##' @return A selection function.
##'
##' @rdname selectionFunctions
##' @export
makeTournamentSelection <- function(tournamentSize = 10,
                                    selectionSize = ceiling(tournamentSize / 2),
                                    tournamentDeterminism = 1.0)
  function(population, fitnessFunction) {
    if (length(fitnessFunction) > 1) {
      warning("Multiple fitness function used with single-objective selection, additional objectives are discared.")
      fitnessFunction <- fitnessFunction[[1]]
    }
    poolIdxs <- sample(length(population), tournamentSize)
    poolFitness <- sapply(population[poolIdxs], fitnessFunction)
    idxFitTable <- cbind(poolIdxs, poolFitness)
    colnames(idxFitTable) <- c("index", "fitness")
    # Sort by (single-criterial) fitness...
    sortedIdxFitTable <- idxFitTable[order(idxFitTable[,"fitness"]),]
    # ...then shuffle the ranking depending on tournamentDeterminism:
    shuffledSortedIdxFitTable <- sortedIdxFitTable[nondeterministicRanking(tournamentSize),]
    # The first selectionSize individuals are selected, the rest are discarded:
    list(selected = shuffledSortedIdxFitTable[1:selectionSize,],
         discarded = shuffledSortedIdxFitTable[-(1:selectionSize),])
  }

##' @rdname selectionFunctions
##' @export
makeParetoTournamentSelection <- function(tournamentSize = 30, tournamentDeterminism = 1.0)
  function(population, fitnessFunction) {
    if (!is.list(fitnessFunction)) fitnessFunction <- list(fitnessFunction)
    cat("BAM!") # TODO
    NA # TODO
  }

##' @rdname selectionFunctions
##' @export
makeComplexityTournamentSelection <- function(complexityMeasure = funcVisitationLength,
                                              tournamentSize = 10, tournamentDeterminism = 1.0) {
  selectionFunction <- makeMultiObjectiveTournamentSelection(tournamentSize, tournamentDeterminism)
  function(population, fitnessFunction)
    selectionFunction(population, append(fitnessFunction, list(complexityMeasure)))
}

##' Create a nondeterministic ranking
##'
##' Create a permutation of the sequence \code{s} = \code{1:l} representing a ranking.
##' If \code{p} = 1, the ranking will be completely deterministic, i.e. equal to
##' \code{1:l}. If \code{p} = 0, the ranking will be completely random. If
##' 0 < \code{p} < 1, the places in the ranking will be determined by iterative
##' weighted sampling without replacement from the sequence \code{s} := \code{1:l}.
##' At each step of this iterated weighted sampling, the first remaining element of
##' \code{s} will be selected with probability \code{p}, the second element with
##' probability \code{p * (1 - p)}, the third element with probability
##' \code{p * (1 - p) ^ 2}, and so forth.
##'
##' @param l The numer of elements in the ranking.
##' @param p The "degree of determinism" of the ranking to create.
##' @return A ranking permutation of the values \code{1:l}.
nondeterministicRanking <- function(l, p = 1) {
  if (p == 0)
    sample(1:l, replace = FALSE) # completely random ranking
  else if (p == 1)
    1:l # completely deterministic ranking
  else {
    # The following more straight-forward code suffers from numeric instability for l >~ 50:
    # pmf <- function(rdet, rl)
    #   if (rl <= 1) rdet else c(rdet, sapply(1:(rl - 1), function(i) rdet * (1 - rdet) ^ i))
    # sample(1:l, replace = FALSE, prob = pmf(p, l)) # TODO numeric bug for l >> 10!
    # This is why we do it the stupid way instead:
    indexPmf <- function(rdet, l) {
      randomPool <- runif(l); randomPool[l] <- -Inf # at least the last index is selected
      which(randomPool <= rdet)[1]
    }
    urn <- 1:l
    ranking <- c()
    for (i in 1:l) {
      sampledIndex <- indexPmf(p, l - i + 1) # Pull an index from the urn...
      ranking <- c(ranking, urn[sampledIndex]) # ...and prepend it to the ranking,...
      urn <- urn[-sampledIndex] # ...then remove it from the urn.
    }
    ranking
  }
}
