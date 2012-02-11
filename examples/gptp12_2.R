# gptp_12_2.R
# experiments for GPTP-12 (setup 2)
# 2012 Oiver Flasch
#

require("rgp")


#gctorture(on = TRUE) # DEBUG
set.seed(1) # controlled random seed (the evoke the air of reproducability)

#Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
#df1 <- data.frame(x1 = 1:100*0.1, y = Salutowicz1d(1:100 * 0.1))
x1 <- seq(from = 0, to = 6.2, length.out = 63)
df1 <- data.frame(x1 = x1, y = sin(x1)) # single sine period

#metaHeuristic1 <- makeExploitativeSteadyStateMetaHeuristic(selectionFunction = makeTournamentSelection(tournamentSize = 10))
metaHeuristic1 <- makeTinyGpMetaHeuristic(crossoverProbability = 0.9, tournamentSize = 2)
#metaHeuristic1 <- makeCommaEvolutionStrategyMetaHeuristic(mu = 25)
#metaHeuristic1 <- makeAgeFitnessComplexityParetoGpMetaHeuristic(lambda = 20, newIndividualsPerGeneration = 2)

#functionSet1 <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt")
functionSet1 <- functionSet("+", "-", "*", "/")
inputVariableSet1 <- inputVariableSet("x1")
constantFactorySet1 <- numericConstantSet # TinyGP change min and max random number to [-5, 5]

mutationFunction1 <- function(ind) {
  mutateChangeDeleteInsert(ind, functionSet1, inputVariableSet1, constantFactorySet1,
                           strength = 1, subtreeDepth = 2, constprob = 0.2, iterations = 1,
                           changeProbability = 1/3, deleteProbability = 1/3, insertProbability = 1/3)
}

mutationFunction2 <- function(ind) {
  #mutantBody <- body(ind) # DEBUG
  #print("-----------------------------------------------------------------") # DEBUG
  #print(mutantBody) # DEBUG
  mutantBody <- .Call("mutate_subtrees_R", body(ind), 0.33, 0.75,
                      functionSet1$all, as.integer(functionSet1$arities),
                      inputVariableSet1$all, -1, 1, 1.0, 0.5, 2L)
  #print("--1"); print(mutantBody)
  mutantBody <- .Call("mutate_functions_R", mutantBody, 1.0, # 0.33,
                      functionSet1$all, as.integer(functionSet1$arities)) # okay
  #print("--2"); print(mutantBody)
  mutantBody <- .Call("mutate_constants_normal_R", mutantBody, 0.33, 0, 1) # okay
  #print("--3"); print(mutantBody)
  #print("====>") # DEBUG
  #print(mutantBody) # DEBUG
  mutant <- .Call("make_closure", mutantBody, inputVariableSet1$all)
  mutant
  #ind # DEBUG 
}

crossoverFunction1 <- function(func1, func2, crossoverprob = 1,
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 1) {
  #print("-----------------------------------------------------------------")
  #print(body(func1)) # DEBUG 
  #print("XXXXX")
  #print(body(func2)) # DEBUG 
  childBody <- .Call("crossover_single_point_R", body(func1), body(func2))[[1]]
  child <- .Call("make_closure", childBody, inputVariableSet1$all)
  #print("====>")
  #print(childBody) # DEBUG 
  
  # DEBUG 
  #child <- new.function()
  #formals(child) <- formals(func1)
  #body(child) <- body(func1)
  child
  #func1 # DEBUG
}

statistics1 <- NULL 
startTime1 <- Sys.time()

progressMonitor1 <- function(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
  if (stepNumber %% 1000 == 0) {
    message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                    stepNumber, evaluationNumber, bestFitness, timeElapsed))
  }
  if (stepNumber %% 10000 == 0) {
    timeTaken <- as.numeric(Sys.time() - startTime1, units = "secs")
    startTime1 <<- Sys.time()
    popSizes <- as.numeric(Map(funcSize, pop))
    sizeStatistics <- summary(popSizes)
    statisticsRow <- c(sizeStatistics, Time = timeTaken)
    statistics1 <<- rbind(statistics1, statisticsRow)
    message("population size statistics: min: ", min(popSizes),
            " median: ", median(popSizes), " mean: ", mean(popSizes),
            " max: ", max(popSizes),
            " time: ", timeTaken)
  }
}

sr1 <- symbolicRegression(y ~ x1, data = df1,
                          functionSet = functionSet1,
                          errorMeasure = mae,
                          #stopCondition = makeTimeStopCondition(15 * 60),
                          stopCondition = makeEvaluationsStopCondition(1e+06), # 1M evaluations
                          #populationSize = 300,
                          populationSize = 4,
                          #populationSize = 5000, # 5K individuals
                          individualSizeLimit = 128, # individuals with more than 64 nodes (inner and leafs) get fitness Inf
                          #mutationFunction = mutationFunction1,
                          mutationFunction = mutationFunction2,
                          crossoverFunction = crossoverFunction1,
                          metaHeuristic = metaHeuristic1,
                          verbose = FALSE,
                          progressMonitor = progressMonitor1)

quartz()
old.par <- par(mfcol = c(2, 1))
plot(statistics1[,"Mean"], type = "b", main = "Mean Individual Size", xlab = "step * 100", ylab = "Mean Ind. Size (Nodes")
plot(statistics1[,"Time"], type = "b", main = "Compute Time", xlab = "step * 100", ylab = "Compute Time (Secs)")
par(old.par)
message("type ls() to view active bindings, the result population has been saved to the variable sr1")
