## appStormRGP1.r
## First symbolic regression experiments on the AppStorm data, no embedding
## 2010 Oliver Flasch
##

## Tools...
preprocessStorm <- function(df, lambda = 0.1) {
  dfPreprocessed <- df
  kern <- rev(exp(-lambda * 0:20))
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

## Data loading and partitioning...
load("~/data/appStorm.rda")
trainingData <- appStorm[5000:10000,]
testData <- appStorm[10000:30000,]
testData2 <- appStorm[25000:30000,]

## Evolution (time budget: 4 hrs)
srModel <- symbolicRegression(FillLevel ~ Rainfall, trainingData,
                              stopCondition = makeTimeStopCondition(4 * 60 * 60))

## Result analysis...
trainingFitnessFunction <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, trainingData)
summary(popfitness(srModel$population, trainingFitnessFunction)) # RMSE on training data

testFitnessFunction <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, testData)
summary(popfitness(srModel$population, testFitnessFunction)) # RMSE on testing data
