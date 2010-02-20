## selection.r
##   - GP selection strategies
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Tournament selection
##'
##' Randomly select two pairs of individuals from \code{pop}. Compare the fitness function
##' values of these, yielding two winners and two losers. Replace both losers in \code{pop} pop
##' by mutated crossover children of the winners.
##'
##' @param pop The population to perform a tournament selection step on.
##' @param fitfunc The fitness function.
##' @param funcset The function set.
##' @param inset The set of input variables.
##' @param crossoverfunc The crossover function.
##' @param mutatefunc The mutation function.
##' @return The population after performing a tournament.
##' @export
tournamentselectionstep <- function(pop, fitfunc, funcset, inset,
                                    crossoverfunc = crossover,
                                    mutatefunc = function(ind) mutateSubtree(mutateConst(ind),
                                      funcset, inset, mutatesubtreeprob = 0.01)) {
  # perform two tournaments...
  idxs <- sample(length(pop), 4)
  if (fitfunc(pop[[idxs[1]]]) < fitfunc(pop[[idxs[2]]])) {
  	winneridx1 <- idxs[1]; loseridx1 <- idxs[2]
  } else {
  	winneridx1 <- idxs[2]; loseridx1 <- idxs[1]
  }
  if (fitfunc(pop[[idxs[3]]]) < fitfunc(pop[[idxs[4]]])) {
  	winneridx2 <- idxs[3]; loseridx2 <- idxs[4]
  } else {
  	winneridx2 <- idxs[4]; loseridx2 <- idxs[3]
  }
  # do crossover...
  winnerchild1 <- crossover(pop[[winneridx1]], pop[[winneridx2]])
  winnerchild2 <- crossover(pop[[winneridx2]], pop[[winneridx1]])
  # replace losers with mutated winner children...
  pop[[loseridx1]] <- mutatefunc(winnerchild1)
  pop[[loseridx2]] <- mutatefunc(winnerchild2)
  pop
}
