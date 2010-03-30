## rgpSpot.r
## Some tools and hints for using GP with SPOT on the AppStorm data
## 2010 Oliver Flasch
##


require(rgp)


## Tools...
preprocessStorm <- function(df, lambda = 0.1) {
  dfPreprocessed <- df
  cutoff <- 0.1
  kernLen <- -log(cutoff) / lambda
  kern <- rev(exp(-lambda * 0:(kernLen + 1)))
  dfPreprocessed$LeakyRain <-
    convolve(dfPreprocessed$Rainfall, kern, type="o")[1:nrow(dfPreprocessed)]
  dfPreprocessed
}

plotStorm <- function(df) {
  oldpar <- par(mfrow=c(2,1))
  plot(df$Rainfall, type="l", col="blue", ylab="Rainfall", xlab="")
  if ("LeakyRain" %in% names(df))
    lines(df$LeakyRain, col="dodgerblue")
  legend("topright", legend = c("Pluvio", "Leaky Rain"),
         lt = 1, col = c("blue", "dodgerblue"), bty = "n")
  plot(df$FillLevel, type="l", col="darkblue", ylab="Fill Level", xlab="")
  par(oldpar)
}

inputVariablesFromFormula <- function(formula, data) {
  mf <- model.frame(formula, data)
  variableNames <- attr(terms(formula(mf)), "term.labels")
  inputVariableSet(list=as.list(variableNames))
}

## Data loading and partitioning...
load("~/data/appStorm.rda")
trainingData <- appStorm[5000:10000,]
testData <- appStorm[10000:30000,]
testData2 <- appStorm[25000:30000,]

## SPOT-wrapper for simple symbolic regression, mainly to reveal "hidden" GP parameters
## and for hiding parameters that are should not be tuned by SPO
simpleStormSymbolRegression <- function(seed, trainingData, testData,
                                        verbose = FALSE,
                                        stopCondition = makeTimeStopCondition(30),
                                        ## the remaining parameters can be tuned by SPO
                                        embeddingDimension = 0, # ROI: [0, 50]
                                        populationSize = 500, # ROI: [tournamentSize, 1000]
                                        individualSizeLimit = 64, # ROI: [16, 128]
                                        minNumericConstant = -1, # ROI: [-100, 0]
                                        maxNumericConstant = 1, # ROI: [0.001, 100]
                                        tournamentSize = 2, # ROI: [2, populationSize]
                                        tournamentDeterminism = 1.0, # ROI: [0.0, 1.0]
                                        crossoverProbability = 0.1, # ROI: [0.0, 1.0]
                                        subtreeMutationProbability = 0.1, # ROI: [0.0, 1.0]
                                        constantMutationProbability = 0.01) { # ROI: [0.0, 1.0]
  ## Set random seed...
  set.seed(seed)
  ## Preprocess data...
  preprocessedTrainingData <- embedDataFrame(trainingData, "Rainfall", embeddingDimension)
  preprocessedTrainingData[,"Date"] <- NULL # delete Date column
  preprocessedTestData <- embedDataFrame(testData, "Rainfall", embeddingDimension)
  preprocessedTestData[,"Date"] <- NULL # delete Date column
  ## Parameterize symbolic regression...
  f <- FillLevel ~ . # use FillLevel as target and every other variable as a predictor
  functionSet <- mathFunctionSet
  constantSet <- constantFactorySet(function() runif(1, minNumericConstant, maxNumericConstant))
  selectionFunction <- function(population, fitnessFunction)
    tournamentSelection(population, fitnessFunction, tournamentSize, tournamentDeterminism)
  crossoverFunction <- function(ind1, ind2) crossover(ind1, ind2, crossoverprob = crossoverProbability)
  mutationFunction <- function(ind)
    mutateSubtree(mutateNumericConst(ind, mutateconstprob = constantMutationProbability),
                  functionSet, inputVariablesFromFormula(f, preprocessedTrainingData), constantSet,
                  mutatesubtreeprob = subtreeMutationProbability)
  srModel <- symbolicRegression(f, preprocessedTrainingData,
                                stopCondition = stopCondition,
                                populationSize = populationSize,
                                individualSizeLimit = individualSizeLimit,
                                penalizeGenotypeConstantIndividuals = FALSE,
                                functionSet = functionSet,
                                constantSet = constantSet,
                                selectionFunction = selectionFunction,
                                crossoverFunction = crossoverFunction,
                                mutationFunction = mutationFunction,
                                verbose = verbose)
  ## Calculate prediction RMSE on test data...
  detailedPrediction <- predict(srModel, newdata = preprocessedTestData, detailed = TRUE)
  detailedPrediction$RMSE
}
