# experimentDesigns.R
# demos for the experiment design generators in RGP
# 2012 Oliver Flasch
#

require("rgp")
require("rgl")
require("twiddler")


plotGrid2d <- function(size, ...) {
  D <- gridDesign(2, points = seq(from = 0.0, to = 1.0, length.out = size))
  plot(x = D[,1], y = D[,2], main = sprintf("grid design (size = %d)", size), ...)
}

plotGrid3d <- function(size, ...) {
  D <- gridDesign(3, points = seq(from = 0.0, to = 1.0, length.out = size))
  plot3d(x = D[,1], y = D[,2], z = D[,3], main = sprintf("grid design (size = %d)", size), ...)
}

plotLhd2d <- function(size, retries, ...) {
  D <- latinHypercubeDesign(2, size = size, retries = retries)
  # color points by distance to closest neighbour
  minDists <- as.numeric(apply(as.matrix(dist(D)), 1, function(x) min(x[x > 0])))
  maxDist <- sqrt(2)
  colorFunction <- colorRamp(c("red", "blue", "green"))
  rgbColors <- colorFunction(minDists / maxDist) 
  colors <- rgb(rgbColors[, 1], rgbColors[, 2], rgbColors[, 3], maxColorValue = 255)

  plot(x = D[,1], y = D[,2], col = colors, main = sprintf("latin hypercube design (size = %d, min dist = %g)", size, min(dist(D))), ...)
}

plotLhd3d <- function(size, retries, ...) {
  D <- latinHypercubeDesign(3, size = size, retries = retries)
  # color points by distance to closest neighbour
  minDists <- as.numeric(apply(as.matrix(dist(D)), 1, function(x) min(x[x > 0])))
  maxDist <- sqrt(3)
  colorFunction <- colorRamp(c("red", "blue", "green"))
  rgbColors <- colorFunction(minDists / maxDist) 
  colors <- rgb(rgbColors[, 1], rgbColors[, 2], rgbColors[, 3], maxColorValue = 255)

  plot3d(x = D[,1], y = D[,2], z = D[,3], col = colors, main = sprintf("latin hypercube design (size = %d, min dist = %g)", size, min(dist(D))), ...)
}

twiddleLhd2d <- function()
  twiddle(plotLhd2d(size, retries),
          size = knob(lim = c(2, 1000), res = 1),
          retries = knob(lim = c(0, 1000), res = 10))

twiddleLhd3d <- function()
  twiddle(plotLhd3d(size, retries),
          size = knob(lim = c(2, 1000), res = 1),
          retries = knob(lim = c(0, 1000), res = 10))

twiddleGrid2d <- function()
  twiddle(plotGrid2d(size), size = knob(lim = c(2, 100), res = 1), eval = FALSE)

twiddleGrid3d <- function()
  twiddle(plotGrid3d(size), size = knob(lim = c(2, 100), res = 1), eval = FALSE)

message("use twiddleLhd2d, twiddeLhd3d(), twilldeGrid2d(), or twiddleGrid3d() to test RGP's experiment design generators")
