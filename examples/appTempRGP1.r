## appTempRGP1.r
## First symbolic regression experiments on the AppTemperature data, no embedding
## 2010 Oliver Flasch
##

## Data loading and partitioning (1/6 training, 5/6 test)...
load("~/data/appTemperatureStreet1Pool5_clean.rda")
trainingData <- subDataFrame(appTemperatureStreet1Pool5, size = 1/6, pos = "START")
testData <- subDataFrame(appTemperatureStreet1Pool5, size = 5/6, pos = "END")

## Evolution (time budget: 4 hrs)
population <- symbolicRegression(NH4N ~ Temperature + pH + Conductivity, trainingData,
                                 stopCondition = makeTimeStopCondition(4 * 60 * 60))

## Result analysis...
trainingFitnessFunction <- makeRegressionFitnessFunction(NH4N ~ Temperature + pH + Conductivity, trainingData)
summary(popfitness(population, trainingFitnessFunction)) # RMSE on training data

testFitnessFunction <- makeRegressionFitnessFunction(NH4N ~ Temperature + pH + Conductivity, testData)
summary(popfitness(population, testFitnessFunction)) # RMSE on testing data
sortBy(population, testFitnessFunction)[[1]] # best individual on training data
