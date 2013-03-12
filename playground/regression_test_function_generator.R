#!/usr/bin/env Rscript
#
# regression_test_function_generator.R
# Randomized regression test functions
# 2013 SPOTSeven group
#

require("rgp")
require("SPOT")
require("soobench")
require("twiddler")
 

lhdSampleSoobenchFunction <- function(f = branin_function()) {
  x <- latinHypercubeDesign(length(lower_bounds(f)), lowerBounds = lower_bounds(f), upperBounds = upper_bounds(f))
  y <- as.matrix(apply(x, 1, f))
  return (list(x = x, y = y))
}

makeForresterTestFunctionGenerator <- function(x, y) {
  originalFit <- forrBuilder(x, y)
  forrTestFunctionGenerator <- function(thetaSd = 1.0) {
    newFit <- originalFit
    newFit$Theta <- newFit$Theta + rnorm(length(newFit$Theta), sd = thetaSd)
    newFit$dmodeltheta <- 10 ^ newFit$Theta
    forrTestProblem <- function(x) forrRegPredictor(x, newFit, pred.all = FALSE)$f
    return  (forrTestProblem)
  }
  return (forrTestFunctionGenerator)
}


# demo code

baseFunction <- branin_function()
#baseFunction <- rastrigin_function(2)
baseFunctionSamples <- lhdSampleSoobenchFunction(baseFunction)
testFunctionGenerator <- makeForresterTestFunctionGenerator(baseFunctionSamples$x, baseFunctionSamples$y)

twiddleTestFunctionGenerator <- function(thetaSd = 0.01, plot3d = FALSE) {
  if (plot3d) {
    spotSurf3d(testFunctionGenerator(thetaSd = thetaSd), lower_bounds(baseFunction), upper_bounds(baseFunction))
  } else {
    spotSurfContour(testFunctionGenerator(thetaSd = thetaSd), lower_bounds(baseFunction), upper_bounds(baseFunction))
  }
}
 
twiddle(twiddleTestFunctionGenerator(thetaSd, plot3d), eval = FALSE, 
  thetaSd = knob(c(0.0, 1.0), res = 0.001, default = 0.01),
  plot3d = toggle(label = "3D plot"))

