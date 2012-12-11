# rbf_interpolation.R
#

require("rgl")
require("rgp")
require("cmaes")
require("twiddler")

rbfm <- function(points,
                 values = points[, ncol(points)],
                 kernel = makeGaussRbfKernel(1.2),
                 distMethod = "euclidean", p = 2,
                 optimizeMethod = "none",
                 nrbf = FALSE,
                 verbose = FALSE) {
  if (verbose) message("rbfm: RBF model fit started...") 
  dim <- ncol(points)
  n <- nrow(points)
  distances <- as.matrix(dist(points, method = distMethod, p = p))
  rbf <- apply(distances, 2, kernel)
  rhs <- numeric(n)
  rhs <- if (nrbf) sum(rbf) * values else values
  w <- solve(rbf, rhs)
  if (verbose) message("rbfm: RBF model fit done. Result:", result) 
 
  result <- structure(list(points = points,
                           values = values,
                           kernel = kernel,
                           distMethod = distMethod,
                           p = p,
                           nrbf = nrbf,
                           rbf = rbf,
                           rhs = rhs,
                           w = w,
                           interpolator = NULL),
                      class = c("rbfModel", "list"))

  # results$interpolator is a closure over result, so that changes to result can affect it...
  result$interpolator <- function(x) {
    # alternative 1: distance calculation via R's native dist function (complexity O(n^2))...
    xDistances <- as.matrix(dist(rbind(x, result$points), method = result$distMethod, p = result$p))
    fVal <- result$kernel(xDistances[1,])[-1]
    # alternative 2: distance caculcation via interpreted code (complexity O(n), but slower in practive)...
    #xDistances <- apply(points, 1, function(point) as.numeric(dist(rbind(x, point), method = distMethod, p = p)))
    #fVal <- kernel(xDistances)
    # ... .
    sumW <- sum(result$w * fVal)
    if (result$nrbf) sumW / sum(fVal) else sumW 
  }

  return (result)
}

makeMultiquadraticRbfKernel <- function(r0) function(r) sqrt(r^2 + r0^2) 
makeInverseMultiquadraticRbfKernel <- function(r0) function(r) (r^2 + r0^2)^-0.5 
makeThinPlateRbfKernel <- function(r0) function(r) r^2 * log(r/r0)
makeGaussianRbfKernel <- function(r0) function(r) exp(-0.5 * r^2 / r0^2)

# test code
plotFunction <- function(f, samplesPerDimension = 64, ...) {
  xs <- seq(0, 1.0, length = samplesPerDimension)
  ys <- xs 
  zs <- outer(xs, ys, Vectorize(function(x, y) f(c(x, y))))
  persp3d(z = zs, xlab = "x1", ylab = "x2", zlab = "y", ...)
}

testRbf <- function(trueFunction, kernel, lhdSize = NA, optimizeMethod = "none") {
  x1 <- if (is.na(lhdSize)) latinHypercubeDesign(2) else latinHypercubeDesign(2, size = lhdSize)
  y1 <- apply(x1, 1, trueFunction)
  rbf1 <- rbfm(x1, values = y1, kernel = kernel, optimizeMethod = optimizeMethod)$interpolator
  plotFunction(trueFunction, col = "blue", alpha = 1/3,
               main = "RBF Interpolation of 2D Functions", sub = "true (blue) versus interpolated (red)")
  plotFunction(rbf1, add = TRUE, col = "red")
}

twiddleTestRbf <- function(f, centers, k, r0, optimizeMethod) {
  trueFunction <- switch(f,
                         "sum" = function(x) x[1] + x[2],
                         "product" = function(x) x[1] * x[2],
                         "sphere" = sphereFunction,
                         "six hump" = sixHumpFunction,
                         "rosenbrock" = rosenbrockFunction,
                         "rastrigin" = rastriginFunction,
                         "mexican hat" = mexicanHatFunction,
                         "branin" = braninFunction)
  kernel <- switch(k,
                   "inverse multiquadratic" = makeInverseMultiquadraticRbfKernel(r0),
                   "multiquadratic" = makeMultiquadraticRbfKernel(r0),
                   "thin plate" = makeThinPlateRbfKernel(r0),
                   "gaussian" = makeGaussianRbfKernel(r0))
  testRbf(trueFunction, kernel, lhdSize = centers, optimizeMethod = optimizeMethod)
}

sphereFunction <- function (x) {
	y <- sum(10*(x-0.5)^2)
	return(y)
}
sixHumpFunction <- function (x) {
	x1 <- (x[1] - 0.5) * 5 
	x2 <- (x[2] - 0.5) * 3	
	y<-(4-2.1*x1^2+x1^4/3)*x1^2+x1*x2+(-4+4*x2^2)*x2^2	
	return(y)
}
rosenbrockFunction <- function (x) {
  x1 <- x[1] * 10
  x2 <- x[2] * 10
  y <- ((1-x1)^2)+(100*((x2-(x1^2))^2))
  return(y)
}
rastriginFunction <- function (x) {  
  x <- (x - 0.5) * 2 
  y<-sum(((x^2) - (cos(x*pi*2)*10))) + 10*length(x)
  return(y)
}
mexicanHatFunction <- function (x) {	
	x1 <- (x[1] - 0.5) * 20 
	x2 <- (x[2] - 0.5) * 20 
	distance <- sqrt(x1^2 + x2^2)
	if (distance == 0) # stetig ergaenzen
		{y<-1}
	else
		{y<- sin(distance) / distance}
	return(y)       
}
braninFunction <- function (x) {	
	x1 <- x[1] * 15 - 5 
	x2 <- x[2] * 15 
	y <- (x2 - 5.1/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
    return(y)	
}

twiddle(twiddleTestRbf(f, centers, kernel, r0, optimizeMethod), 
        optimizeMethod = combo("none", "TODO"),
        r0 = knob(lim = c(0.01, 10.0), res = 0.01, default = 1.2),
        kernel = combo("inverse multiquadratic", "multiquadratic", "thin plate", "gaussian"),
        centers = knob(lim = c(2, 64), res = 1, default = 22),
        f = combo("product", "sum", "sphere", "six hump", "rosenbrock", "rastrigin", "mexican hat", "branin"),
        eval = FALSE)
