# calibrationAnalysis.R
#

r1 <- readRDS("salustowicz1d_calibration.RDS")

pdf("gmogp_calibration_fitness_histories.pdf", family = "Palatino", width = 8 ,height = 8)
matplot(x = r1$fitnessHistories[,1], y = r1$fitnessHistories[,-1], type = "l", xlab = "# Fitness Evaluations", ylab = "Training Fitness (SMSE)")
dev.off()

