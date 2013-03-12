#!/usr/bin/env Rscript

require(twiddler)
require(SPOT)
 
 
## Create design points
samples <- 20
x <- cbind(runif(samples) * 15 - 5, runif(samples) * 15)

## Compute observations at design points (for Branin function)
y <- as.matrix(apply(x, 1, spotBraninFunction))

## Create model with default settings
fit <- forrBuilder(x, y)

## Print model parameters
print(fit)
originalFit <- fit
 
twiddleForrFit <- function(deltaTheta1, deltaTheta2, plot3d = FALSE) {
  fit$Theta <- c(originalFit$Theta[1] + deltaTheta1, originalFit$Theta[2] + deltaTheta2)
  fit$dmodeltheta <- 10^fit$Theta

  testProblem <- function(x) {
    return(forrRegPredictor(x, fit, pred.all = FALSE)$f)
  }

  if (plot3d) {
    spotSurf3d(testProblem, c(-5,0), c(10,15))
  } else {
    spotSurfContour(testProblem, c(-5,0), c(10,15))
  }
}
 
twiddle(twiddleForrFit(deltaTheta1, deltaTheta2, plot3d),
  eval = TRUE, 
  deltaTheta1 = knob(c(-1, 1), res = 0.01, default=0),
  deltaTheta2 = knob(c(-1, 1), res = 0.01, default=0),#	c = knob(c(-1, 1), res = 0.01, default=0),
  plot3d = toggle(label = "3D plot")
)

