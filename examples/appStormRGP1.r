## appStormRGP1.r
## First symbolic regression experiments on the AppStorm data, no embedding
## 2010 Oliver Flasch
##

## Data loading and partitioning...
load("~/data/appStorm.rda")
trainingData <- appStorm[, 5000:10000]
testData <- appStrom[, 10000:30000]
testData2 <- appStrom[, 25000:30000]

## Evolution (time budget: 4 hrs)
srModel <- symbolicRegression(FillLevel ~ Rainfall, trainingData,
                              stopCondition = makeTimeStopCondition(4 * 60 * 60))

## Result analysis...
trainingFitnessFunction <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, trainingData)
summary(popfitness(srModel$population, trainingFitnessFunction)) # RMSE on training data

testFitnessFunction <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, testData)
summary(popfitness(srModel$population, testFitnessFunction)) # RMSE on testing data
