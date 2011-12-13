## meta_heuristics.r
##   - Functions defining meta heuristics (i.e. algorithmic frameworks) 
##     for Genetic Programming 
##
## RGP - a GP system for R
## 2011 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Meta-heuristics for GP search
##'
##' The meta-heuristic, i.e. the concrete GP search algorithm, is a modular component of RGP. RGP
##' already provides a set of meta-heuristics for GP search, including the following:
##'
##' \code{makeExploitativeSteadyStateMetaHeuristic} creates a exploitative steady state
##' meta-heuristic for use in RGP. This meta-heuristic was the only option in early versions of
##' RGP and is provided mainly for reasons of backward-compatiblity. TODO describe this heuristic
##'
##' \code{makeTinyGpMetaHeuristic} creates an RGP meta-heuristic that mimics the search heuristic
##' implemented in Riccardo Poli's TinyGP system. TODO describe this heuristic
##'
##' @rdname metaHeuristics 
##' @export
makeExploitativeSteadyStateMetaHeuristic <- function()
function(logFunction, stopCondition, pop, fitnessFunction, selectionFunction,
         mutationFunction, crossoverFunction,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  ## Initialize statistic counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  archiveList <- list() # the archive of all individuals selected in this run, only used if archive == TRUE
  archiveIndexOf <- function(archive, individual)
    Position(function(a) identical(body(a$individual), body(individual)), archive)
  bestFitness <- Inf # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  ## Execute GP run...
  logFunction("STARTING genetic programming evolution run (exploitative steady state meta-heuristic) ...")
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
    # Select two sets of individuals and divide each into winners and losers...
    selA <- selectionFunction(pop, fitnessFunction); selB <- selectionFunction(pop, fitnessFunction)
    if (archive) { # add the evaluated individuals to the archive...
      evaluatedIndices <- c(selA$selected[, 1], selB$selected[, 1], selA$discarded[, 1], selB$discarded[, 1])
      evaluatedFitnesses <- c(selA$selected[, 2], selB$selected[, 2], selA$discarded[, 2], selB$discarded[, 2])
      for (i in 1:length(evaluatedIndices))
        archiveList[[length(archiveList) + 1]] <- list(individual = pop[[evaluatedIndices[i]]],
                                                       fitness = evaluatedFitnesses[i])
    }
    winnersA <- selA$selected[, 1]; winnersB <- selB$selected[, 1]
    bestFitness <- min(c(bestFitness, selA$selected[, 2], selB$selected[, 2]))
    losersA <- selA$discarded[, 1]; losersB <- selB$discarded[, 1]
    losers <- c(losersA, losersB)
    # Create winner children through crossover and mutation...
    makeWinnerChildren <- function(winnersA, winnersB)
                            Map(function(winnerA, winnerB)
                                  mutationFunction(crossoverFunction(pop[[winnerA]], pop[[winnerB]],
                                                                     breedingFitness = breedingFitness,
                                                                     breedingTries = breedingTries)),
                                winnersA, winnersB)
    winnerChildrenA <- makeWinnerChildren(winnersA, winnersB) 
    winnerChildrenB <- makeWinnerChildren(winnersA, winnersB) 
    winnerChildren <- c(winnerChildrenA, winnerChildrenB)
    # Replace losers with winner children...
    if (extinctionPrevention) {
      numberOfLosers <- length(losers)
      winnerChildrenAndLosers <- c(winnerChildren, pop[losers])
      uniqueWinnerChildrenAndLosers <- unique(winnerChildrenAndLosers) # unique() does not change the order of it's argument
      numberOfUniqueWinnerChildrenAndLosers <- length(uniqueWinnerChildrenAndLosers)
      if (numberOfUniqueWinnerChildrenAndLosers < numberOfLosers) { # not enough unique individuals...
        numberMissing <- numberOfLosers - numberOfUniqueWinnerChildrenAndLosers
        warning(sprintf("geneticProgramming: not enough unique individuals for extinction prevention (%d individuals missing)", numberMissing))
        # we have to fill up with duplicates...
        uniqueWinnerChildrenAndLosers <- c(uniqueWinnerChildrenAndLosers, winnerChildrenAndLosers[1:numberMissing])
      }
      uniqueChildren <- uniqueWinnerChildrenAndLosers[1:numberOfLosers] # fill up duplicated winner children with losers
      pop[losers] <- uniqueChildren
    } else {
      pop[losers] <- winnerChildren
    }
    # Apply restart strategy...
    if (restartCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                         evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
      restartResult <- restartStrategy(fitnessFunction, pop, populationSize, functionSet, inputVariables, constantSet)
      pop <- restartResult[[1]]
      elite <- joinElites(restartResult[[2]], elite, eliteSize, fitnessFunction)
      logFunction("restarted run")
    }
    
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- 1 + stepNumber
    evaluationNumber <- selA$numberOfFitnessEvaluations + selB$numberOfFitnessEvaluations + evaluationNumber
    progressMonitor(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                    evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)
  }
  elite <- joinElites(pop, elite, eliteSize, fitnessFunction) # insert pop into elite at end of run
  logFunction("Genetic programming evolution run FINISHED after %i evolution steps, %i fitness evaluations and %s.",
         stepNumber, evaluationNumber, formatSeconds(timeElapsed))

  ## Return result list...
  list(timeElapsed = timeElapsed,
       stepNumber = stepNumber,
       evaluationNumber = evaluationNumber,
       bestFitness = bestFitness,
       population = pop,
       elite = elite,
       archiveList = archiveList)
}

##' @rdname metaHeuristics 
##' @export
makeTinyGpMetaHeuristic <- function()
function(logFunction, stopCondition, pop, fitnessFunction, selectionFunction,
         mutationFunction, crossoverFunction,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  ## Initialize statistic counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  archiveList <- list() # the archive of all individuals selected in this run, only used if archive == TRUE
  archiveIndexOf <- function(archive, individual)
    Position(function(a) identical(body(a$individual), body(individual)), archive)
  bestFitness <- Inf # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  ## Execute GP run...
  logFunction("STARTING genetic programming evolution run (TinyGP meta-heuristic) ...")
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
    stop() # TODO
  }

  ## Return result list...
  list(timeElapsed = timeElapsed,
       stepNumber = stepNumber,
       evaluationNumber = evaluationNumber,
       bestFitness = bestFitness,
       population = pop,
       elite = elite,
       archiveList = archiveList)
}
