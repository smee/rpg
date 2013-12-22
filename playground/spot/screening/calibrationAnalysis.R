# calibrationAnalysis.R
#

r1 <- readRDS("salustowicz1d_calibration.RDS")
matplot(x = r1$fitnessHistories[,1], y = r1$fitnessHistories[,-1], type = "l", xlab = "# Fitness Evaluations", ylab = "Training Fitness (SMSE)")
