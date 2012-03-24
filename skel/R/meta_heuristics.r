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
##' implemented in Riccardo Poli's TinyGP system. TODO describe this heuristic, cite TinyGP
##'
##' \code{makeCommaEvolutionStrategyMetaHeuristic} creates a RGP meta-heuristic that implements a
##' (mu, lambda) Evolution Strategy. The lambda parameter is fixed to the population size.
##' TODO description based on Luke09a
##'
##' \code{makeAgeFitnessComplexityParetoGpMetaHeuristic} creates a RGP meta-heuristic that implements
##' an evolutionary multi objective optimization algorithm (EMOA) that selects on three criteria:
##' Individual age, individual fitness, and individual complexity. TODO description
##'
##' @param selectionFunction The selection function to use in meta-heuristics that support
##'   different selection functions. Defaults to tournament selection. See
##'   \link{makeTournamentSelection} for details.
##' @param crossoverProbability The crossover probability for meta-heuristics that support
##'   this setting (i.e. TinyGP). Defaults to \code{0.9}.
##  @param tournamentSize The tournament size for meta-heuristics that support this setting
##'   (i.e. TinyGP). Defaults to \code{2}.
##' @param mu The number of surviving parents for the Evolution Strategy meta-heuristic. Note that
##'   with \code{makeCommaEvolutionStrategyMetaHeuristic}, lambda is fixed to the population size,
##'   i.e. \code{length(pop)}.
##' @param lambda The number of children to create in each generation.
##' @param enableComplexityCriterion Whether to enable the complexity criterion in multi-criterial
##'   search heuristics.
##' @param enableAgeCriterion Whether to enable the age criterion in multi-criterial search heuristics.
##' @param complexityMeasure The complexity measure, a function of signature \code{function(ind, fitness)}
##'   returning a single numeric value.
##' @param \code{newIndividualsPerGeneration} The number of new individuals per generation to
##'   insert into the population.
##' @param \code{newIndividualsMaxDepth} The maximum depth of new individuals inserted into the
##'   population.
##'
##' @rdname metaHeuristics 
##' @export
makeExploitativeSteadyStateMetaHeuristic <- function(selectionFunction = makeTournamentSelection())
function(logFunction, stopCondition, pop, fitnessFunction,
         mutationFunction, crossoverFunction,
         functionSet, inputVariables, constantSet,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  logFunction("STARTING genetic programming evolution run (exploitative steady state meta-heuristic) ...")
  
  fitnessValues <- sapply(pop, fitnessFunction)

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
    fitnessValues[c(winnersA, winnersB)] <- c(selA$selected[, 2], selB$selected[, 2])
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
    progressMonitor(pop = pop, fitnessValues = fitnessValues, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
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
       archiveList = archiveList,
       metaHeuristicResults = list())
}

##' @rdname metaHeuristics 
##' @export
makeTinyGpMetaHeuristic <- function(crossoverProbability = 0.9, tournamentSize = 2)
function(logFunction, stopCondition, pop, fitnessFunction,
         mutationFunction, crossoverFunction,
         functionSet, inputVariables, constantSet,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  logFunction("STARTING genetic programming evolution run (TinyGP meta-heuristic) ...")
  
  ## Tool functions...
  randomIndex <- function(maxIndex) as.integer(runif(1, min = 1, max = maxIndex + 1)) 
  tournament <- function(fitnessValues, popSize, tournamentSize) {
    bestIndex <- randomIndex(popSize) 
    bestFitness <- Inf
    for (i in 1:tournamentSize) {
      competitorIndex <- randomIndex(popSize)
      if (fitnessValues[competitorIndex] < bestFitness) {
        bestFitness <- fitnessValues[competitorIndex]
        bestIndex <- competitorIndex
      }
    }
    bestIndex
  }
  negativeTournament <- function(fitnessValues, popSize, tournamentSize) {
    worstIndex <- randomIndex(popSize) 
    worstFitness <- -Inf
    for (i in 1:tournamentSize) {
      competitorIndex <- randomIndex(popSize)
      if (fitnessValues[competitorIndex] > worstFitness) {
        worstFitness <- fitnessValues[competitorIndex]
        worstIndex <- competitorIndex
      }
    }
    worstIndex
  }

  ## Global variables...
  popSize <- length(pop)
  fitnessValues <- sapply(pop, fitnessFunction)

  ## Initialize statistic counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  archiveList <- list() # the archive of all individuals selected in this run, only used if archive == TRUE
  archiveIndexOf <- function(archive, individual)
    Position(function(a) identical(body(a$individual), body(individual)), archive)
  bestFitness <- min(fitnessValues) # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  ## Execute GP run...
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
    child <- NULL
    if (runif(1) < crossoverProbability) {
      # create child via crossover
      motherIndex <- tournament(fitnessValues, popSize, tournamentSize)
      fatherIndex <- tournament(fitnessValues, popSize, tournamentSize)
      child <- crossoverFunction(pop[[motherIndex]], pop[[fatherIndex]],
                                 breedingFitness = breedingFitness,
                                 breedingTries = breedingTries)
    } else {
      # create child via mutation
      parentIndex <- tournament(fitnessValues, popSize, tournamentSize)
      child <- mutationFunction(pop[[parentIndex]])
    }

    childFitness <- fitnessFunction(child)
    bestFitness <- if (childFitness < bestFitness) childFitness else bestFitness

    offspringIndex <- negativeTournament(fitnessValues, popSize, tournamentSize)
    fitnessValues[offspringIndex] <- childFitness
    pop[[offspringIndex]] <- child

    if (archive) {
      archiveList[[length(archiveList) + 1]] <- list(individual = child,
                                                     fitness = childFitness)
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
    evaluationNumber <- 1 + evaluationNumber
    progressMonitor(pop = pop, fitnessValues = fitnessValues, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
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
       archiveList = archiveList,
       metaHeuristicResults = list())
}

##' @rdname metaHeuristics 
##' @export
makeCommaEvolutionStrategyMetaHeuristic <- function(mu = 1)
function(logFunction, stopCondition, pop, fitnessFunction,
         mutationFunction, crossoverFunction,
         functionSet, inputVariables, constantSet,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  logFunction("STARTING genetic programming evolution run (Evolution Strategy meta-heuristic) ...")
  
  ## Tool functions...
  truncationSelect <- function(mu, fitnessValues) order(fitnessValues)[1:mu]

  ## Global variables...
  lambda <- length(pop)
  if(mu > lambda) stop("makeCommaEvolutionStrategyMetaHeuristic: mu must be less or equal to the population size")
  childrenPerParent <- ceiling(lambda / mu)
  fitnessValues <- sapply(pop, fitnessFunction)

  ## Initialize statistic counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  archiveList <- list() # the archive of all individuals selected in this run, only used if archive == TRUE
  archiveIndexOf <- function(archive, individual)
    Position(function(a) identical(body(a$individual), body(individual)), archive)
  bestFitness <- min(fitnessValues) # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  ## Execute GP run...
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {
    parentIndices <- truncationSelect(mu, fitnessValues)
    nextGeneration <- list()
    for (i in 1:mu) {
      nextGeneration <- c(nextGeneration,
                          replicate(childrenPerParent, mutationFunction(pop[[i]])))
    }
    elite <- joinElites(pop, elite, eliteSize, fitnessFunction) # insert pop into elite
    pop <- nextGeneration[1:lambda] # replace entire population with next generation
    fitnessValues <- sapply(pop, fitnessFunction)
    bestFitness <- if (min(fitnessValues) < bestFitness) min(fitnessValues) else bestFitness
 
    if (archive) {
      archiveList[[length(archiveList) + 1]] <- list(individual = child,
                                                       fitness = childFitness)
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
    evaluationNumber <- lambda + evaluationNumber
    progressMonitor(pop = pop, fitnessValues = fitnessValues, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
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
       archiveList = archiveList,
       metaHeuristicResults = list())
}

##' @rdname metaHeuristics 
##' @export
makeAgeFitnessComplexityParetoGpMetaHeuristic <- function(lambda = 20,
                                                          crossoverProbability = 0.9,
                                                          enableComplexityCriterion = TRUE,
                                                          enableAgeCriterion = TRUE,
                                                          complexityMesaure = function(ind, fitness) funcVisitationLength(ind),
                                                          ageMergeFunction = max,
                                                          newIndividualsPerGeneration = 1,
                                                          newIndividualsMaxDepth = 8,
                                                          plotFront = TRUE)
function(logFunction, stopCondition, pop, fitnessFunction,
         mutationFunction, crossoverFunction,
         functionSet, inputVariables, constantSet,
         archive, extinctionPrevention,
         elite, eliteSize,
         restartCondition, restartStrategy,
         breedingFitness, breedingTries,
         progressMonitor) {
  ## Tool functions...
  plotParetoFront <- function(x, y, ranks, ages,
                              xlab = "X", ylab = "Y",
                              maxAge = 50) {
    ageColorScale <- colorRamp(c("#00FF00", "#006600","#0000FF", "#000000"))
    rankOneXs <- x[ranks == 1]; rankOneYs <- y[ranks == 1]; rankOneAges <- ages[ranks == 1]
    rankOneAgeColors <- rgb(ageColorScale(rankOneAges / maxAge), maxColorValue = 255)
    plot(rankOneXs, rankOneYs,
         xlab = xlab, ylab = ylab,
         col = rankOneAgeColors, pch = 1, main = "Pareto Plot")
    points(x[ranks > 1], y[ranks > 1], col = "gray", pch = 4)
  }

  logFunction("STARTING genetic programming evolution run (Age/Fitness/Complexity Pareto GP  meta-heuristic) ...")

  ## Initialize run-global variables...
  mu <- length(pop)
  if (mu < 2 * lambda) stop("makeAgeFitnessComplexityParetoGpMetaHeuristic: condition mu < 2 * lambda must be fulfilled")
  fitnessValues <- as.numeric(sapply(pop, fitnessFunction))
  complexityValues <- as.numeric(Map(complexityMesaure, pop, fitnessValues))
  ageValues <- integer(mu) # initialize ages with zeros

  ## Initialize statistic counters...
  stepNumber <- 1
  evaluationNumber <- 0
  timeElapsed <- 0
  archiveList <- list() # the archive of all individuals selected in this run, only used if archive == TRUE
  archiveIndexOf <- function(archive, individual)
    Position(function(a) identical(body(a$individual), body(individual)), archive)
  bestFitness <- min(fitnessValues) # best fitness value seen in this run, if multi-criterial, only the first component counts
  startTime <- proc.time()["elapsed"]

  ## Execute GP run...
  while (!stopCondition(pop = pop, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                        evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)) {

    # Sample (without replacement) 2 * lambda parent inviduals...
    # TODO add an option for pareto-tournaments here
    parentIndices <- sample(1:mu, 2 * lambda, replace = FALSE)
    motherIndices <- parentIndices[1:lambda]
    fatherIndices <- parentIndices[(lambda + 1):(2 * lambda)]

    # Create individuals...
    children <- Map(function(motherIndex, fatherIndex) {
                      if (runif(1) < crossoverProbability) {
                        # create child via crossover
                        crossoverFunction(pop[[motherIndex]], pop[[fatherIndex]],
                                          breedingFitness = breedingFitness,
                                          breedingTries = breedingTries)
                      } else {
                        # create child via mutation
                        mutationFunction(pop[[motherIndex]])
                      }
                    },
                    motherIndices, fatherIndices)

    # Evaluate children individuals...
    childrenFitnessValues <- as.numeric(sapply(children, fitnessFunction))
    childrenComplexityValues <- as.numeric(Map(complexityMesaure, children, childrenFitnessValues))
    childrenAgeValues <- 1 + as.integer(Map(ageMergeFunction, ageValues[motherIndices], ageValues[fatherIndices]))

    # Create and evaluate new individuals...
    newIndividuals <- makePopulation(newIndividualsPerGeneration, functionSet, inputVariables, constantSet,
                                     maxfuncdepth = newIndividualsMaxDepth,
                                     extinctionPrevention = extinctionPrevention,
                                     breedingFitness = breedingFitness, breedingTries = breedingTries)
    newIndividualsFitnessValues <- as.numeric(sapply(newIndividuals, fitnessFunction))
    newIndividualsComplexityValues <- as.numeric(Map(complexityMesaure, newIndividuals, newIndividualsFitnessValues))
    newIndividualsAgeValues <- integer(newIndividualsPerGeneration) # initialize ages with zeros

    # Create the pool of individuals to select the next generation from...
    pool <- c(pop, children, newIndividuals)
    poolFitnessValues <- c(fitnessValues, childrenFitnessValues, newIndividualsFitnessValues)
    poolComplexityValues <- c(complexityValues, childrenComplexityValues, newIndividualsComplexityValues) 
    poolAgeValues <- c(ageValues, childrenAgeValues, newIndividualsAgeValues)

    # Sort the pool via the non-domination relation and select individuals for removal...
    poolPoints <- if (enableAgeCriterion & enableComplexityCriterion)
      rbind(poolFitnessValues, poolComplexityValues, poolAgeValues)
    else if (enableComplexityCriterion)
      rbind(poolFitnessValues, poolComplexityValues)
    else if (enableAgeCriterion)
      rbind(poolFitnessValues, poolAgeValues)
    else
      rbind(poolFitnessValues)
    if (plotFront) {
      poolNdsRanks <- nds_rank(poolPoints)
      plotParetoFront(poolFitnessValues, poolComplexityValues, poolNdsRanks, poolAgeValues,
                      xlab = "Fitness", ylab = "Complexity")
    }
    poolIndicesToRemove <- if (!enableComplexityCriterion & !enableAgeCriterion) {
      # single-criterial case
      order(poolPoints, decreasing = TRUE)[1:(lambda + newIndividualsPerGeneration)]
    } else {
      # multi-criteral case
      nds_cd_selection(poolPoints, lambda + newIndividualsPerGeneration)
    }

    # Replace current population with next generation...
    pop <- pool[-poolIndicesToRemove]
    fitnessValues <- poolFitnessValues[-poolIndicesToRemove]
    complexityValues <- poolComplexityValues[-poolIndicesToRemove]
    ageValues <- poolAgeValues[-poolIndicesToRemove]
    if (min(fitnessValues) < bestFitness) bestFitness <- min(fitnessValues) # update best fitness

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
    evaluationNumber <- lambda + newIndividualsPerGeneration + evaluationNumber
    progressMonitor(pop = pop, fitnessValues = fitnessValues, fitnessFunction = fitnessFunction, stepNumber = stepNumber,
                    evaluationNumber = evaluationNumber, bestFitness = bestFitness, timeElapsed = timeElapsed)
  }
 
  elite <- joinElites(pop, elite, eliteSize, fitnessFunction) # insert pop into elite at end of run
  bestFitness <- min(fitnessValues)
  logFunction("Genetic programming evolution run FINISHED after %i evolution steps, %i fitness evaluations and %s.",
              stepNumber, evaluationNumber, formatSeconds(timeElapsed))

  ## Return result list...
  list(timeElapsed = timeElapsed,
       stepNumber = stepNumber,
       evaluationNumber = evaluationNumber,
       bestFitness = bestFitness,
       population = pop,
       elite = elite,
       archiveList = archiveList,
       metaHeuristicResults = list(fitnessValues = fitnessValues,
                                   complexityValues = complexityValues,
                                   ageValues = ageValues))
}
