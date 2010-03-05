## appTempRGP2.r
## First symbolic regression experiments on the AppTemperature data, embedding of dimension 3
## 2010 Oliver Flasch
##

## Data loading, partitioning (1/6 training, 5/6 test), and embedding (dimension 3)...
load("~/data/appTemperatureStreet1Pool5_clean.rda")
trainingData <- subDataFrame(appTemperatureStreet1Pool5, size = 1/6, pos = "START")
testData <- subDataFrame(appTemperatureStreet1Pool5, size = 5/6, pos = "END")
trainingDataE3 <- embedDataFrame(trainingData, c("Temperature", "pH", "Conductivity"), 3)
testDataE3 <- embedDataFrame(testData, c("Temperature", "pH", "Conductivity"), 3)

## Evolution (time budget: 4 hrs)
population <- symbolicRegression(NH4N ~ Temperature + pH + Conductivity, trainingDataE3,
                                 stopCondition = makeTimeStopCondition(4 * 60 * 60))

## Result analysis...
trainingFitnessFunction <- makeRegressionFitnessFunction(NH4N ~ Temperature + pH + Conductivity, trainingDataE3)
summary(popfitness(population, trainingFitnessFunction)) # RMSE on training data

testFitnessFunction <- makeRegressionFitnessFunction(NH4N ~ Temperature + pH + Conductivity, testDataE3)
summary(popfitness(population, testFitnessFunction)) # RMSE on testing data
sortBy(population, testFitnessFunction)[[1]] # best individual on training data
