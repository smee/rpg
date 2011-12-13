# sr_parameters.R
# simple example for parameterizing untyped RGP symbolic regression runs
# 2011 Oiver Flasch
#

require("rgp")


Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
df1 <- data.frame(x1 = 1:100*0.1, y=Salutowicz1d(1:100*0.1))

functionSet1 <- functionSet("+", "-", "*", "/", "sin", "cos", "exp", "log", "sqrt") 
inputVariableSet1 <- inputVariableSet("x1")
constantFactorySet1 <- numericConstantSet

mutationFunction1 <- function(ind) {
  mutateChangeDeleteInsert(ind, functionSet1, inputVariableSet1, constantFactorySet1,
                           strength = 1, subtreeDepth = 2, constprob = 0.2, iterations = 1,
                           changeProbability = 1/3, deleteProbability = 1/3, insertProbability = 1/3)
}

statistics1 <- NULL 
startTime1 <- Sys.time()

progressMonitor1 <- function(pop, fitnessFunction, stepNumber, evaluationNumber, bestFitness, timeElapsed) {
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
    message("population size statistics: min: ", min(popSizes),
            " median: ", median(popSizes), " mean: ", mean(popSizes),
            " max: ", max(popSizes),
            " time: ", timeTaken)
  }
}

sr1 <- symbolicRegression(y ~ x1, data = df1,
                          functionSet = functionSet1,
                          stopCondition = makeStepsStopCondition(5000), # makeTimeStopCondition(seconds)
                          populationSize = 200,
                          individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                          selectionFunction = makeTournamentSelection(tournamentSize = 10),
                          mutationFunction = mutationFunction1,
                          verbose = FALSE,
                          progressMonitor = progressMonitor1)

old.par <- par(mfcol = c(2, 1))
plot(statistics1[,"Mean"], type = "b", main = "Mean Individual Size", xlab = "step * 100", ylab = "Mean Ind. Size (Nodes")
plot(statistics1[,"Time"], type = "b", main = "Compute Time", xlab = "step * 100", ylab = "Compute Time (Secs)")
par(old.par)
message("type ls() to view active bindings, the result population has been saved to the variable sr1")
