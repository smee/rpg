## data_utils.r
##   - Utility functions for R matrices and data frames
##
## RGP - a GP system for R
## 2010-2012 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Patrick Koch, Olaf Mersmann and Joerg Stork
## Latin Hypercube Sampling Code by Chrisitian Lasarczyk
## released under the GPL v2
##

##' Embed columns in a data frame
##'
##' Embeds the columns named \code{cols} in the data frame \code{x} into a space of dimension
##' \code{dimension}.
##'
##' @param x The data frame containing the columns to embed.
##' @param cols A vector a list of the names of the columns to embed.
##' @param dimension The additional dimensions to generate when embedding.
##' @return The data frame, augmented with embedded columns, shortended by \code{dimension} rows.
##'
##' @export
embedDataFrame <- function(x, cols = NULL, dimension = 1) {
  if (dimension == 0)
    return(x)
  d <- dimension + 1
  if (d <= 1 || d >= nrow(x))
    stop("dimension must be greater than 1 and smaller than nrow(x)")
  columns <- if (missing(cols)) names(x) else cols

  embeddings <- lapply(columns, function(column) {
                                  embedded <- as.matrix(embed(x[[column]], d)[,-1])
                                  colnames(embedded) <- paste(column, ".P", 1:ncol(embedded), sep = "")
                                  embedded
                                })
  data.frame(list(x[-1:-(d-1),], embeddings))
}

##' Select a continuous subframe of a data frame
##'
##' Return a continuous subframe of the data frame \code{x} containing \code{size} * \code{nrow(x)}
##' rows from the start, center, or end.
##'
##' @param x The data frame to get a subframe from.
##' @param size The size ratio of the subframe. Must be between 0 and 1.
##' @param pos The position to take the subframe from. Must be \code{"START"}, \code{"CENTER"},
##'   or \code{"END"}.
##' @return A subframe of \code{x}.
##'
##' @export
subDataFrame <- function(x, size = 1.0, pos = "START") {
  if (size < 0 || size > 1) stop("size must be between 0 and 1")
  if (!(pos %in% c("START", "CENTER", "END"))) stop("pos must be either START, CENTER, or END")
  numberOfRows <- nrow(x)
  numberOfSelectedRows <- size * numberOfRows
  if (numberOfSelectedRows == 0) return(NULL)
  startIndex <-
    if ("START" == pos) 1
    else if ("CENTER" == pos) 1 + (numberOfRows - numberOfSelectedRows) %/% 2
    else if ("END" == pos) 1 + numberOfRows - numberOfSelectedRows
    else stop()
  endIndex <- startIndex + numberOfSelectedRows - 1
  x[startIndex:endIndex,]
}

##' Create a regular grid design matrix
##'
##' Returns a n = length(points)**dimension times m = dimension matrix containing
##' the coordinates of sample points from a hypervolume of the given dimension.
##' Points are sampled in a grid defined by the vector points.
##'
##' @param dimension The number of columns in the design matrix to create.
##' @param points A vector of points to sample at in each dimension.
##' @return The regular grid design matrix.
##'
##' @export
gridDesign <- function(dimension, points = seq(from = 0.0, to = 1.0, length.out = 10)) {
  s = length(points)
  D <- matrix(0.0, nrow = s ** dimension, ncol = dimension)

  for (i in 1:nrow(D)) {
    for (j in 1:ncol(D)) {
      index <- (ceiling(i / s ** (j - 1)) - 1) %% s + 1
      D[i, j] <- points[index] 
    }
  }

  return (D)
}

##' Create a normalized design matrix
##'
##' Produces a normalized design and calculates the minimal distance 
##' if required. Returns a design is a matrix with \code{dim} columns and
##' \code{size} rows.
##' 
##' @param dimension Dimension of the problem (will be no. of columns of the result matrix).
##' @param size Number of points with that dimension needed. (will be no. of rows of the result matrix).
##' @param calcMinDistance Indicates whether a minimal distance should be calculated.
##' @return List \code{L} consists of a matrix and nd (if required) a minimal distance.
##'
##' @export
normalizedDesign <- function(dimension, size, calcMinDistance = FALSE) {
	step <- 1 / size
	D <- replicate(dimension, sample(0:(size - 1), size) * step + runif(size) * step)

	if (calcMinDistance) {
		minDistance <- min(dist(D))
	} else {
		minDistance <- NA
  }

	list(design = D,  minDistance = minDistance)
}

##' Create a latin hypercube design (LHD)
##'
##' Produces a LHD matrix with \code{dimension} columns and \code{size}
##' rows.
##'
##' @param dimension Dimension of the problem (will be no. of columns of the result matrix).
##' @param size Number of design points, defaults to \code{max(11 * dimension,
##'   1 + 3 * dimension + dimension * (dimension - 1) / 2 + 1)}.
##' @param bounds Interval (2-element numeric vector) giving the upper and lower bounds
##'   for sampling, defaults to \code{c(0.0, 1.0)}.
##' @param retries Number of retries, which is the number of trials to find a design
##'   with the lowest distance, default is \code{2 * dimension}.
##' @return A LHD matrix. 
##'
##' @export
latinHypercubeDesign <- function(dimension,
                                 size = max(11 * dimension, 1 + 3 * dimension + dimension * (dimension - 1) / 2 + 1),
                                 bounds = c(0.0, 1.0),
                                 retries = 2 * dimension) {
	# min distance does not have to be calculated if there is only one try 
	bestNormalizedD <- normalizedDesign(dimension, size, calcMinDistance = retries > 0)
	if (retries > 0) {
		for (i in 1:retries) {
			candidateD <- normalizedDesign(dimension, size, calcMinDistance = TRUE)
			## maximize minimal distance
			if (candidateD$minDistance > bestNormalizedD$minDistance) {
				bestNormalizedD <- candidateD 
      }
		}
	}

  # scale normalized matrix to bounds
  lowerBound <- bounds[1]
  upperBound <- bounds[2]
  D <- lowerBound + bestNormalizedD$design * (upperBound - lowerBound)

	return (D)
}
