## snowfallTest.r
## A template showing how to use Snowfall for distributing RGP runs on a cluster
## 2010 Oliver Flasch
##

require(snowfall)
source("rgp.r")

## Load, partition, and pre-process datasets...
load("~/data/appStorm.rda")
trainingDataE0 <- appStorm[5000:10000,] # E0 means "no embedding"
testDataE0 <- appStorm[10000:30000,]
trainingDataE3 <- embedDataFrame(trainingDataE0, cols = "Rainfall", dimension = 3)
testDataE3 <- embedDataFrame(testDataE0, cols = "Rainfall", dimension = 3)

## Define the worker function to disribute on the cluster...
sfWorker <- function(seed) {
  set.seed(seed)
  symbolicRegression(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3, trainingData,
                     stopCondition = makeTimeStopCondition(4 * 60 * 60)) # 4 hours
}

## Start runs on the cluster, collect and save results...
srModels <- sfLapply(1:64, sfWorker) # 64 runs
save(srModels, file="~/appStormRGP2.rda")
