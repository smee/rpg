# rgpTesting.r
# Tests and examples for RGP
# 2010 Oliver Flasch, released under the GPL v2
#


source("../skel/R/list_utils.r")
source("../skel/R/function_utils.r")
source("../skel/R/plot_utils.r")
source("../skel/R/stypes.r")
source("../skel/R/search_space.r")
source("../skel/R/creation.r")
source("../skel/R/population.r")
source("../skel/R/fitness.r")
source("../skel/R/complexity.r")
source("../skel/R/selection.r")
source("../skel/R/mutation.r")
source("../skel/R/recombination.r")

# testing...
# ---

safeDivide <- function(a, b) ifelse(b == 0, b, a / b) # TODO
safeSqroot <- function(a) sqrt(ifelse(a < 0, 0, a))
safeLn <- function(a) log(ifelse(a < 0, 0, a))
ln <- function(a) log(a)
ifPositive <- function(x, thenbranch, elsebranch) ifelse(x > 0, thenbranch, elsebranch)

arithmeticFuncset <- functionSet("+", "-", "*", "/")
expLogFuncset <- functionSet("sqrt", "exp", "ln")
trigonometricFuncset <- functionSet("sin", "cos", "tan")
mathFuncset <- c(arithmeticFuncset, expLogFuncset, trigonometricFuncset)

onedimInset <- inputVariableSet("x")

simpleevostep <- function(pop, fitnessfunc, funcset, inset)
  tournamentselectionstep(pop, fitnessfunc, funcset, inset)

simpleevo <- function(pop, fitnessfunc, steps = 1,
                      funcset = mathFuncset, inset = onedimInset, printfreq = NA) {
  for (step in 1:steps) {
    if (!is.na(printfreq) && (step %% printfreq) == 0)
      cat(sprintf("step %i of %i (%g %%)\n", step, steps, (step / steps) * 100))
    pop <- simpleevostep(pop, fitnessfunc, funcset, inset)
  }
  pop
}

## some simple fitness functions for testing...
sinusfitness <- fitfuncfromfunc(sin, -pi, pi, steps = 256, indsizelimit = 32)
f1 <- function(x) sin(x*0.2*cos(x))
f1fitness <- fitfuncfromfunc(f1, -2*pi, 2*pi, steps = 1024, indsizelimit = 32)
new.dampedOscillator <- function(m = 1, R = 1, x0 = 1, omega = pi, phi0 = pi) {
  delta <- R / 2 * m
  function(t) x0 * exp(-delta * t) * sin(omega * t + phi0)
}
do1 <- new.dampedOscillator()
do1fitness <- fitfuncfromfunc(do1, 0, 10, steps = 512, indsizelimit = 16)


## simple evolution of the sinus function in interval [-pi,pi]...
#pop1 <- new.population(500, arithmeticFuncset, onedimInset)
#pop1 <- simpleevo(pop1, sinusfitness, funcset = arithmeticFuncset, inset = onedimInset, steps = 10000, printfreq = 100); summary(popfitness(pop1, sinusfitness))
#system.time(pop1 <- simpleevo(pop1, sinusfitness, funcset = arithmeticFuncset, inset = onedimInset, steps = 10000, printfreq = 100)); summary(popfitness(pop1, sinusfitness))
#pop1sorted <- sortBy(pop1, sinusfitness)
#plotFunctions(list(sortBy(pop1, sinusfitness)[[1]], sin), -pi, pi, 1024)
#plot.new(); text(0.5, 0.5, indToPlotmathExpr(sortBy(pop1, sinusfitness)[[1]]))

## evolution of a simple damped oscillator (e.g. a pendulum)
#pop2 <- new.population(500, c(arithmeticFuncset, trigonometricFuncset), onedimInset)
#pop2 <- simpleevo(pop2, do1fitness, funcset = c(arithmeticFuncset, trigonometricFuncset), inset = onedimInset, steps = 100000, printfreq = 100); summary(popfitness(pop2, do1fitness))
#plotFunctions(list(sortBy(pop2, do1fitness)[[1]], do1), 0, 10, 1024)
#plot.new(); text(0.5, 0.5, indToPlotmathExpr(sortBy(pop2, do1fitness)[[1]]))
