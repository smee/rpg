# rbf_interpolation.R
#

require("rgl")
require("rgp")
require("twiddler")

rbfInterpolator <- function(points,
                            values = points[, ncol(points)],
                            kernel = makeGaussRbfKernel(1.2),
                            distMethod = "euclidean", p = 2,
                            nrbf = FALSE) {
  dim <- ncol(points)
  n <- nrow(points)
  distances <- as.matrix(dist(points, method = distMethod, p = p))
  rbf <- apply(distances, 2, kernel)
  rhs <- numeric(n)
  rhs <- if (nrbf) sum(rbf) * values else values
  w <- solve(rbf, rhs)

  function(x) {
    xDistances <- as.matrix(dist(rbind(x, points), method = distMethod, p = p))
    fVal <- kernel(xDistances[1,])[-1]
    sumW <- sum(w * fVal)
    if (nrbf) sumW / sum(fval) else sumW 
  }
}

makeGaussRbfKernel <- function(epsilon) function(r) exp(-(epsilon * r) ^ 2)


# test code
plotFunction <- function(f, ...) {
  X <- seq(0, 1.0, length = 64)
  Y <- X
  Z <- outer(X, Y, Vectorize(function(x, y) f(c(x, y))))
  persp3d(z = Z, xlab = "x1", ylab = "x2", zlab = "y", ...)
}

testRbf <- function(trueFunction, lhdSize = NA) {
  x1 <- if (is.na(lhdSize)) latinHypercubeDesign(2) else latinHypercubeDesign(2, size = lhdSize)
  y1 <- apply(x1, 1, trueFunction)
  rbf1 <- rbfInterpolator(x1, y1)
  plotFunction(trueFunction, col = "blue", alpha = 1/3,
               main = "RBF Interpolation of 2D Functions", sub = "true (blue) versus interpolated (red)")
  plotFunction(rbf1, add = TRUE, col = "red")
}

twiddleTestRbf <- function(f, centers) {
  trueFunction <- switch(f,
                         "sum" = function(x) x[1] + x[2],
                         "product" = function(x) x[1] * x[2],
                         "sphere" = sphereFunction,
                         "six hump" = sixHumpFunction,
                         "rosenbrock" = rosenbrockFunction,
                         "rastrigin" = rastriginFunction,
                         "mexican hat" = mexicanHatFunction,
                         "branin" = braninFunction)
  testRbf(trueFunction, centers)
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
	x1 <- (x[1] - 0.5) * 10
	x2 <- (x[2] - 0.5) * 10
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

twiddle(twiddleTestRbf(f, centers), centers = knob(lim = c(2, 64), res = 1, default = 22),
                                    f = combo("product", "sum", "sphere", "six hump", "rosenbrock", "rastrigin", "mexican hat", "branin"))
