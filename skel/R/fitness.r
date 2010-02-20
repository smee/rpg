## fitness.r
##   - Standard GP fitness functions and tools for creating GP fitness functions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Mean squared error (MSE)
##'
##' @param xs A numeric vector or list.
##' @param ys A numeric vector or list.
##' @return The MSE between \code{xs} and \code{ys}.
##' @export
mse <- function(xs, ys) crossprod(xs - ys)

##' Root mean squared error (RMSE)
##'
##' @param xs A numeric vector or list.
##' @param ys A numeric vector or list.
##' @return The RMSE between \code{xs} and \code{ys}.
##' @export
rmse <- function(xs, ys) sqrt(mse(xs, ys))

##' Create a fitness function from a function of one variable
##'
##' Creates a fitness function that calculates an error measure with respect to an arbitrary reference
##' function of one variable on the sequence of fitness cases \code{seq(from, to, length = steps)}.
##' When an \code{indsizelimit} is given, individuals exceeding this limit will receive a fitness of \code{Inf}.
##'
##' @param func The reference function.
##' @param from The start of the sequence of fitness cases.
##' @param to The end of the sequence of fitness cases.
##' @param steps The number of steps in the sequence of fitness cases.
##' @param errormeasure A function to use as an error measure.
##' @param indsizelimit Individuals exceeding this size limit will get a fitness of \code{Inf}.
##' @return A fitness function based on the reference function \code{func}.
##' @export
fitfuncfromfunc <- function(func, from = -1, to = 1, steps = 128, errormeasure = rmse, indsizelimit = NA) {
  xs <- seq(from, to, length = steps)
  ystarget <- func(xs)
  function(ind) {
    ysind <- ind(xs) # vectorized fitness-case evaluation
  	errorind <- errormeasure(ystarget, ysind)
  	if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
  	  Inf # ind size limit exceeded
  	else if (is.na(errorind) || is.nan(errorind))
  	  Inf # error value is NA or NaN
  	else errorind
  }
}
