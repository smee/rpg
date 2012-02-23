## fitness.r
##   - Standard GP fitness functions and tools for creating GP fitness functions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Mean absolute error (MAE)
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The MAE between \code{x} and \code{y}.
##' @export
mae <- function(x, y) mean(abs(x - y))

##' Mean squared error (MSE)
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The MSE between \code{x} and \code{y}.
##' @export
mse <- function(x, y) mean((x - y)^2)

##' Root mean squared error (RMSE)
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The RMSE between \code{x} and \code{y}.
##' @export
rmse <- function(x, y) sqrt(mse(x, y))

##' Normalize a vector into the interval [0, 1]
##'
##' @param x The vector to normalize, so that each element lies in the
##'   interval [0, 1].
##' @return The normalized vector.
##' @export
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

##' Scaled mean squared error (SMSE)
##'
##' Calculates the MSE between vectors after normalizing them into the
##' interval [0, 1].
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The SMSE between \code{x} and \code{y}.
smse <- function(x, y) mse(normalize(x), normalize(y))

##' Symbolic squared error function (SE)
##'
##' Given to functions \code{f} and \code{g}, returns a function whose body
##' is the symbolic representation of the squared error between \code{f} and
##' \code{g}, i.e. \code{function(x) (f(x) - g(x))^2}.
##'
##' @param f An R function.
##' @param g An R function with the same formal arguments as \code{f}.
##' @return A function representing the squared error between \code{f} and \code{g}. 
##' @export
seSymbolicFunction <- function(f, g) {
  newf <- new.function()
  formals(newf) <- formals(f)
  body(newf) <- call("^", call("-", body(f), body(g)), 2)
  newf
}

##' Symbolic squared error (SE)
##'
##' Given to functions \code{f} and \code{g}, returns the area the squared
##' differences between \code{f} and \code{g} in the integration limits
##' \code{lower} and \code{upper}.
##'
##' @param f An R function.
##' @param g An R function with the same formal arguments as \code{f}.
##' @param lower The lower limit of integraion.
##' @param upper The upper limit of integraion.
##' @param subdivisions The maximum number of subintervals for numeric integration. 
##' @return The area of the squared differences between \code{f} and \code{g}, or
##'   \code{Inf} if integration is not possible in the limits given. 
##' @export
seSymbolic <- function(f, g, lower, upper, subdivisions = 100) {
  seFunction <- seSymbolicFunction(f, g)
  tryCatch(integrate(seFunction, lower = lower, upper = upper, subdivisions = subdivisions)$value,
           error = function(error) Inf)
           #error = function(error) { print(error); Inf }) # DEBUG
}

##' Create a fitness function based on symbolic squared error (SE) 
##'
##' Creates a fitness function that calculates the squared error of
##' an individual with respect to a reference function \code{func}.
##' When an \code{indsizelimit} is given, individuals exceeding this
##' limit will receive a fitness of \code{Inf}.
##'
##' @param func The reference function.
##' @param lower The lower limit of integraion.
##' @param upper The upper limit of integraion.
##' @param subdivisions The maximum number of subintervals for numeric integration. 
##' @param indsizelimit Individuals exceeding this size limit will get
##'   a fitness of \code{Inf}.
##' @return A fitness function based on the reference function \code{func}.
##' @export
makeSeSymbolicFitnessFunction <- function(func, lower, upper, subdivisions = 100,
                                          indsizelimit = NA) {
  function(ind) {
    errorind <- seSymbolic(ind, func,
                           lower = lower, upper = upper,
                           subdivisions = subdivisions)
  	if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
  	  Inf # ind size limit exceeded
  	else errorind
  }
}

##' Create a fitness function from a function of one variable
##'
##' Creates a fitness function that calculates an error measure with
##' respect to an arbitrary reference function of one variable on the
##' sequence of fitness cases \code{seq(from, to, length = steps)}.
##' When an \code{indsizelimit} is given, individuals exceeding this
##' limit will receive a fitness of \code{Inf}.
##'
##' @param func The reference function.
##' @param from The start of the sequence of fitness cases.
##' @param to The end of the sequence of fitness cases.
##' @param steps The number of steps in the sequence of fitness cases.
##' @param errorMeasure A function to use as an error measure, defaults to RMSE.
##' @param indsizelimit Individuals exceeding this size limit will get
##'   a fitness of \code{Inf}.
##' @return A fitness function based on the reference function \code{func}.
##' @export
makeFunctionFitnessFunction <- function(func, from = -1, to = 1, steps = 128,
                                        errorMeasure = rmse, indsizelimit = NA) {
  xs <- seq(from, to, length = steps)
  ystarget <- func(xs)
  function(ind) {
    ysind <- ind(xs) # vectorized fitness-case evaluation
  	errorind <- errorMeasure(ystarget, ysind)
  	if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
  	  Inf # ind size limit exceeded
  	else if (is.na(errorind) || is.nan(errorind))
  	  Inf # error value is NA or NaN
  	else errorind
  }
}
