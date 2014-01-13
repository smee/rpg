## pareto_utils.R
##   - Utility functions for Pareto fronts 
##
## RGP - a GP system for R
## 2010-2014 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Patrick Koch, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Find the knee of a two dimensional pareto front 
##'
##' Given a matrix \code{m} of two rows and n columns, representing solutions of a
##' two-dimensional optimization problem, returns the column index of the point with
##' minimum euclidean distance to the utopia point. The utopia point is the point
##' consisting of the row minima of \code{m}.
##'
##' @param m A matrix of two rows and n columns, representing the solutions of a
##'   two-dimensional optimization problem.
##' @return The knee point index, i.e. the column index in m of the point of minimum
##'   euclidean distance to the utopia point.
##' @examples
##' m1 <- matrix(runif(200), ncol = 100)
##' plot(t(m1))
##' points(t(m1[,nds_rank(m1) == 1]), col = "red", pch = 16)
##' pKnee <- m1[, paretoFrontKneeIndex(m1)]
##' points(t(pKnee), col = "green4", pch = 16)
##'
##' @export
paretoFrontKneeIndex <- function(m) {
  pUtopia <- c(min(m[1, ]), min(m[2, ]))
  which.min(as.matrix(dist(t(cbind(pUtopia, m)), method = "euclidean"))[-1, 1])
}
