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
  X <- seq(0, 1.0, length= 32)
  Y <- X
  Z <- outer(X, Y, Vectorize(function(x, y) f(c(x, y))))
  persp3d(z = Z, xlab = "x1", ylab = "x2", zlab = "y", aspect = "iso", ...)
}

testRbf <- function(trueFunction, lhdSize = NA) {
  x1 <- if (is.na(lhdSize)) latinHypercubeDesign(2) else latinHypercubeDesign(2, size = lhdSize)
  y1 <- apply(x1, 1, trueFunction)
  rbf1 <- rbfInterpolator(x1, y1)
  plotFunction(trueFunction, col = "blue", alpha = 1/3,
               main = "RBF Interpolation of 2D Functions", sub = "true (blue) versus interpolated (red)")
  plotFunction(rbf1, add = TRUE, col = "red")
}

twiddle(testRbf(function(x) x[1] * x[2], centers), centers = knob(lim = c(2, 64), res = 1))
