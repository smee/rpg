# sr_parameters.R
# simple example for parameterizing untyped RGP symbolic regression runs
# 2011 Oiver Flasch
#

require("rgp")


Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
df1 <- data.frame(x1 = 1:100*0.1, y = Salutowicz1d(1:100 * 0.1))

#searchHeuristic1 <- makeTinyGpSearchHeuristic()
#searchHeuristic1 <- makeCommaEvolutionStrategySearchHeuristic(mu = 25)
searchHeuristic1 <- makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = 20, newIndividualsPerGeneration = 2)

functionSet1 <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt") 
inputVariableSet1 <- inputVariableSet("x1")
constantFactorySet1 <- numericConstantSet

mutationFunction1 <- function(ind) {
  mutateChangeDeleteInsert(ind, functionSet1, inputVariableSet1, constantFactorySet1,
                           strength = 1, subtreeDepth = 2, constprob = 0.2, iterations = 1,
                           changeProbability = 1/3, deleteProbability = 1/3, insertProbability = 1/3)
}

mutationFunction2 <- function(ind) {
  subtreeMutantBody <- .Call("mutate_subtrees_R", body(ind), 0.33, 0.75,
                             functionSet1$all, as.integer(functionSet1$arities),
                             inputVariableSet1$all, -1, 1, 1.0, 0.5, 2L)
  #print("--1"); print(subtreeMutantBody)
  functionMutantBody <- .Call("mutate_functions_R", subtreeMutantBody, 0.33,
                              functionSet1$all, as.integer(functionSet1$arities))
  #print("--2"); print(functionMutantBody)
  constantMutantBody <- .Call("mutate_constants_normal_R", functionMutantBody, 0.33, 0, 1)
  #print("--3"); print(constantMutantBody)
  mutant <- .Call("make_closure", constantMutantBody, inputVariableSet1$all)
  mutant
}

crossoverFunction1 <- function(func1, func2, crossoverprob = 1,
                               breedingFitness = function(individual) TRUE,
                               breedingTries = 1) {
  childBody <- .Call("crossover_single_point_R", body(func1), body(func2))[[1]]
  child <- .Call("make_closure", childBody, inputVariableSet1$all)
  child
}

statistics1 <- NULL 
startTime1 <- Sys.time()

progressMonitor1 <- function(pop, fitnessValues, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
  if (stepNumber %% 10 == 0) {
    message(sprintf("evolution step %i, fitness evaluations: %i, best fitness: %f, time elapsed: %f",
                    stepNumber, evaluationNumber, bestFitness, timeElapsed))
  }
  if (stepNumber %% 100 == 0) {
    timeTaken <- as.numeric(Sys.time() - startTime1, units = "secs")
    startTime1 <<- Sys.time()
    popSizes <- as.numeric(Map(funcSize, pop))
    sizeStatistics <- summary(popSizes)
    statisticsRow <- c(sizeStatistics, Time = timeTaken)
    statistics1 <<- rbind(statistics1, statisticsRow)
    message("population function size statistics: min: ", min(popSizes),
            " median: ", median(popSizes), " mean: ", mean(popSizes),
            " max: ", max(popSizes),
            " time: ", timeTaken)
  }
}

sr1 <- symbolicRegression(y ~ x1, data = df1,
                          functionSet = functionSet1,
                          errorMeasure = mae,
                          #stopCondition = makeStepsStopCondition(250),
                          stopCondition = makeTimeStopCondition(15 * 60),
                          populationSize = 200,
                          individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                          #mutationFunction = mutationFunction1,
                          mutationFunction = mutationFunction2,
                          crossoverFunction = crossoverFunction1,
                          searchHeuristic = searchHeuristic1,
                          verbose = TRUE,
                          progressMonitor = progressMonitor1)

quartz()
old.par <- par(mfcol = c(2, 1))
plot(statistics1[,"Mean"], type = "b", main = "Mean Individual Size", xlab = "step * 100", ylab = "Mean Ind. Size (Nodes")
plot(statistics1[,"Time"], type = "b", main = "Compute Time", xlab = "step * 100", ylab = "Compute Time (Secs)")
par(old.par)
message("type ls() to view active bindings, the result population has been saved to the variable sr1")
