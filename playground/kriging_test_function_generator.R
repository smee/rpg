#!/usr/bin/env Rscript

require(twiddler)
require(SPOT)
 
 
## Create design points
x = cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y = as.matrix(apply(x,1,spotBraninFunction))
## Create model with default settings
fit = forrBuilder(x,y)
## Print model parameters
print(fit)
fitsave <- fit
 
plotFn <- function(theta1,theta2) {
  fit$Theta <- c(fitsave$Theta[1]+theta1,fitsave$Theta[2]+theta2)
  fit$dmodeltheta <- 10^fit$Theta
  fn <- function(xx) {
    return(forrRegPredictor(xx,fit,F)$f)
  }

  spotSurfContour(fn,c(-5,0),c(10,15))
}
 
twiddle(plotFn(a,b), eval = TRUE, 
  a = knob(c(-1, 1), res = 0.01, default=0),
  b = knob(c(-1, 1), res = 0.01, default=0)#,	c = knob(c(-1, 1), res = 0.01, default=0)
)
