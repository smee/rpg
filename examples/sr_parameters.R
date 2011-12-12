# sr_parameters.R
# simple example for parameterizing untyped RGP symbolic regression runs
# 2011 Oiver Flasch
#

require("rgp")


Salutowicz1d <- function(x) exp(-1*x)*x*x*x*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
df1 <- data.frame(x1 = 1:100*0.1, y=Salutowicz1d(1:100*0.1))

functionSet1 <- functionSet("+", "-")
inputVariableSet1 <- inputVariableSet("x1")
constantFactorySet1 <- numericConstantSet

mutationFunction1 <- function(ind) {
  mutateChangeDeleteInsert(ind, functionSet1, inputVariableSet1, constantFactorySet1,
                           strength = 1, subtreeDepth = 2, constprob = 0.2, iterations = 1,
                           changeProbability = 1/3, deleteProbability = 1/3, insertProbability = 1/3)
}

sr1 <- symbolicRegression(y ~ x1, data = df1,
                          functionSet = functionSet1,
                          stopCondition = makeStepsStopCondition(500), # makeTimeStopCondition(seconds)
                          populationSize = 200,
                          individualSizeLimit = 128, # individuals with more than 128 nodes (inner and leafs) get fitness Inf
                          selectionFunction = makeTournamentSelection(tournamentSize = 10),
                          mutationFunction = mutationFunction1)

message("type ls() to view active bindings, the result population has been saved to the variable sr1")
