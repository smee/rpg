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
trainingDataE10 <- embedDataFrame(trainingDataE0, cols = "Rainfall", dimension = 10)
testDataE10 <- embedDataFrame(testDataE0, cols = "Rainfall", dimension = 10)
trainingDataE20 <- embedDataFrame(trainingDataE0, cols = "Rainfall", dimension = 20)
testDataE20 <- embedDataFrame(testDataE0, cols = "Rainfall", dimension = 20)
trainingDataE50 <- embedDataFrame(trainingDataE0, cols = "Rainfall", dimension = 50)
testDataE50 <- embedDataFrame(testDataE0, cols = "Rainfall", dimension = 50)

## Define the worker function to disribute on the cluster...
sfWorker <- function(seed) {
  set.seed(seed)
  symbolicRegression(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3, trainingDataE3,
                     stopCondition = makeTimeStopCondition(4 * 60 * 60)) # 4 hours
}

## Initialize the cluster, start the runs on the cluster, collect and save results...
sfInit(parallel = TRUE, socketHosts = c(rep("maanbs02", 16), rep("maanbs03", 16)))
sfExportAll() # export all variables in the local global environment to the remote cluster nodes
srModels <- sfLapply(1:64, sfWorker) # 64 runs
save(srModels, file = "~/appStormRGP2.rda")
sfStop()

## Analyze and plot the results...
trainingFitnessFunctionE0 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, trainingDataE0)
testFitnessFunctionE0 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall, testDataE0)
trainingFitnessFunctionE3 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3, trainingDataE3)
testFitnessFunctionE3 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3, testDataE3)
trainingFitnessFunctionE10 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10, trainingDataE10)
testFitnessFunctionE10 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10, testDataE10)
trainingFitnessFunctionE20 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10 + Rainfall.P11 + Rainfall.P12 + Rainfall.P13 + Rainfall.P14 + Rainfall.P15 + Rainfall.P16 + Rainfall.P17 + Rainfall.P18 + Rainfall.P19 + Rainfall.P20, trainingDataE20)
testFitnessFunctionE20 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10 + Rainfall.P11 + Rainfall.P12 + Rainfall.P13 + Rainfall.P14 + Rainfall.P15 + Rainfall.P16 + Rainfall.P17 + Rainfall.P18 + Rainfall.P19 + Rainfall.P20, testDataE20)
trainingFitnessFunctionE50 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10 + Rainfall.P11 + Rainfall.P12 + Rainfall.P13 + Rainfall.P14 + Rainfall.P15 + Rainfall.P16 + Rainfall.P17 + Rainfall.P18 + Rainfall.P19 + Rainfall.P20 + Rainfall.P21 + Rainfall.P22 + Rainfall.P23 + Rainfall.P24 + Rainfall.P25 + Rainfall.P26 + Rainfall.P27 + Rainfall.P28 + Rainfall.P29 + Rainfall.P30 + Rainfall.P31 + Rainfall.P32 + Rainfall.P33 + Rainfall.P34 + Rainfall.P35 + Rainfall.P36 + Rainfall.P37 + Rainfall.P38 + Rainfall.P39 + Rainfall.P40 + Rainfall.P41 + Rainfall.P42 + Rainfall.P43 + Rainfall.P44 + Rainfall.P45 + Rainfall.P46 + Rainfall.P47 + Rainfall.P48 + Rainfall.P49 + Rainfall.P50, trainingDataE50)
testFitnessFunctionE50 <- makeRegressionFitnessFunction(FillLevel ~ Rainfall + Rainfall.P1 + Rainfall.P2 + Rainfall.P3 + Rainfall.P4 + Rainfall.P5 + Rainfall.P6 + Rainfall.P7 + Rainfall.P8 + Rainfall.P9 + Rainfall.P10 + Rainfall.P11 + Rainfall.P12 + Rainfall.P13 + Rainfall.P14 + Rainfall.P15 + Rainfall.P16 + Rainfall.P17 + Rainfall.P18 + Rainfall.P19 + Rainfall.P20 + Rainfall.P21 + Rainfall.P22 + Rainfall.P23 + Rainfall.P24 + Rainfall.P25 + Rainfall.P26 + Rainfall.P27 + Rainfall.P28 + Rainfall.P29 + Rainfall.P30 + Rainfall.P31 + Rainfall.P32 + Rainfall.P33 + Rainfall.P34 + Rainfall.P35 + Rainfall.P36 + Rainfall.P37 + Rainfall.P38 + Rainfall.P39 + Rainfall.P40 + Rainfall.P41 + Rainfall.P42 + Rainfall.P43 + Rainfall.P44 + Rainfall.P45 + Rainfall.P46 + Rainfall.P47 + Rainfall.P48 + Rainfall.P49 + Rainfall.P50, testDataE50)

e0resTrain <- sapply(srModels, function(srModel) min(popfitness(srModel$population, trainingFitnessFunctionE0)));
e0resTest <- sapply(srModels, function(m) predict(m, newdata = testDataE0, detailed = TRUE)$RMSE):
e3resTrain <- sapply(srModels, function(srModel) min(popfitness(srModel$population, trainingFitnessFunctionE3)));
e3resTest <- sapply(srModels, function(m) predict(m, newdata = testDataE3, detailed = TRUE)$RMSE);
e10resTrain <- sapply(srModels, function(srModel) min(popfitness(srModel$population, trainingFitnessFunctionE10)));
e10resTest <- sapply(srModels, function(m) predict(m, newdata = testDataE10, detailed = TRUE)$RMSE);
e20resTrain <- sapply(srModels, function(srModel) min(popfitness(srModel$population, trainingFitnessFunctionE20)));
e20resTest <- sapply(srModels, function(m) predict(m, newdata = testDataE20, detailed = TRUE)$RMSE);
e50resTrain <- sapply(srModels, function(srModel) min(popfitness(srModel$population, trainingFitnessFunctionE50)));
e50resTest <- sapply(srModels, function(m) predict(m, newdata = testDataE50, detailed = TRUE)$RMSE)

summary(e0resTrain)
summary(e0resTest)
summary(e3resTrain)
summary(e3resTest)
summary(e10resTrain)
summary(e10resTest)
summary(e50resTrain)
summary(e50resTest)

require(tikzDevice)
tikz("~/repos/ofdiss/src/figures/results/boxplot_ConventionalGP_AppNH4N_MinRMSE.tex", width=4.5,height=3.5)
par(mfcol=c(1,2))
boxplot(minTrainingFitnessE0, minTrainingFitnessE3, xlab="Embedding Dim.", ylab="RMSE", names=c(0, 3), main="Training")
boxplot(minTestFitnessE0, minTestFitnessE3, xlab="Embedding Dim.", ylab="RMSE", names=c(0, 3), main="Test")
dev.off()
tikz("~/repos/ofdiss/src/figures/results/boxplot_ConventionalGP_AppStorm_MinRMSE.tex", width=4.5,height=3.5)
par(mfcol=c(1,2))
boxplot(e0resTrain, e3resTrain, e10resTrain, e50resTrain, xlab="Embedding Dim.", ylab="RMSE", names=c(0, 3, 10, 50), main="Training")
boxplot(e0resTest, e3resTest, e10resTest, e50resTest, xlab="Embedding Dim.", ylab="RMSE", names=c(0, 3, 10, 50), main="Test")
dev.off()
