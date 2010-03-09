# rgpTesting.r
# Tests and examples for RGP
# 2010 Oliver Flasch, released under the GPL v2
#


source("rgp.r")

# testing...
# ---

## some simple functions for GP use...
safeDivide <- function(a, b) ifelse(b == 0, b, a / b) # TODO
safeSqroot <- function(a) sqrt(ifelse(a < 0, 0, a))
safeLn <- function(a) log(ifelse(a < 0, 0, a))
ln <- function(a) log(a)
positive <- function(x) x > 0
ifPositive <- function(x, thenbranch, elsebranch) ifelse(x > 0, thenbranch, elsebranch)
ifThenElse <- function(x, thenbranch, elsebranch) ifelse(x, thenbranch, elsebranch)

## untyped and typed function sets...
arithmeticFuncset <- functionSet("+", "-", "*", "/")
expLogFuncset <- functionSet("sqrt", "exp", "ln")
trigonometricFuncset <- functionSet("sin", "cos", "tan")
mathFuncset <- c(arithmeticFuncset, expLogFuncset, trigonometricFuncset)

typedArithmeticFuncset <- functionSet("+" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "-" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "*" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")),
                                      "/" %::% (list(st("numeric"), st("numeric")) %->% st("numeric")))
typedExpLogFuncset <- functionSet("sqrt" %::% (list(st("numeric")) %->% st("numeric")),
                                  "exp" %::% (list(st("numeric")) %->% st("numeric")),
                                  "ln" %::% (list(st("numeric")) %->% st("numeric")))
typedTrigonometricFuncset <- functionSet("sin" %::% (list(st("numeric")) %->% st("numeric")),
                                         "cos" %::% (list(st("numeric")) %->% st("numeric")),
                                         "tan" %::% (list(st("numeric")) %->% st("numeric")))
typedMathFuncset <- c(typedArithmeticFuncset, typedExpLogFuncset, typedTrigonometricFuncset)

typedLogicalFuncset <- functionSet("<" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   ">" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   "==" %::% (list(st("numeric"), st("numeric")) %->% st("logical")),
                                   "ifThenElse" %::% (list(st("logical"), st("numeric"), st("numeric")) %->% st("numeric")),
                                   "&" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                   "|" %::% (list(st("logical"), st("logical")) %->% st("logical")),
                                   "!" %::% (list(st("logical")) %->% st("logical")))

typedMathLogicalFuncset <- c(typedMathFuncset, typedLogicalFuncset)

## untyped and typed input variable sets...
onedimInset <- inputVariableSet("x")

typedOnedimInset <- inputVariableSet("x" %::% st("numeric"))

## untyped and typed constant factory sets...
numericConset <- constantFactorySet(function() runif(1, -1, 1))

typedNumericLogicalConset <- constantFactorySet((function() runif(1, -1, 1)) %::% (list() %->% st("numeric")),
                                                (function() runif(1) > .5) %::% (list() %->% st("logical")))

## simple evolution main loop...
simpleevo <- function(pop, fitnessfunc, steps = 1,
                      funcset = mathFuncset, inset = onedimInset, conset = numericConset,
                      crossoverfunc = crossover,
                      mutatefunc = function(ind) mutateSubtree(mutateNumericConst(ind),
                        funcset, inset, conset, mutatesubtreeprob = 0.01),
                      printfreq = NA) {
  for (step in 1:steps) {
    if (!is.na(printfreq) && (step %% printfreq) == 0)
      cat(sprintf("step %i of %i (%g %%)\n", step, steps, (step / steps) * 100))
    pop <- tournamentselectionstep(pop, fitnessfunc, funcset, inset, conset,
                                   crossoverfunc = crossoverfunc,
                                   mutatefunc = mutatefunc)
  }
  pop
}

simpleevoTyped <- function(pop, fitnessfunc, steps = 1,
                      funcset = typedMathLogicalFuncset, inset = typedOnedimInset, conset = typedNumericLogicalConset,
                      crossoverfunc = crossoverTyped,
                      mutatefunc = function(ind)
                           mutateSubtreeTyped(mutateFuncTyped(mutateNumericConstTyped(ind),
                                                              funcset, mutatefuncprob = 0.01),
                                              funcset, inset, conset, mutatesubtreeprob = 0.01),
                                              
                      printfreq = NA)
  simpleevo(pop, fitnessfunc, steps, funcset, inset, conset, crossoverfunc, mutatefunc, printfreq)

## some simple fitness functions for testing...
sinusfitness <- makeFunctionFitnessFunction(sin, -pi, pi, steps = 256, indsizelimit = 32)

f1 <- function(x) sin(x*0.2*cos(x))
f1fitness <- makeFunctionFitnessFunction(f1, -2*pi, 2*pi, steps = 1024, indsizelimit = 32)

new.dampedOscillator <- function(m = 1, R = 1, x0 = 1, omega = pi, phi0 = pi) {
  delta <- R / 2 * m
  function(t) x0 * exp(-delta * t) * sin(omega * t + phi0)
}
do1 <- new.dampedOscillator()
do1fitness <- makeFunctionFitnessFunction(do1, 0, 10, steps = 512, indsizelimit = 16)

squarewave <- function(x) ifelse(x %% 1 >= 0.5, 1, -1)
squarewavefitness <- makeFunctionFitnessFunction(squarewave, 0, 3, steps = 512, indsizelimit = 16)

## simple evolution of the sinus function in the interval [-pi,pi]...
#pop1 <- makePopulation(500, arithmeticFuncset, onedimInset, numericConset)
#pop1 <- simpleevo(pop1, sinusfitness, funcset = arithmeticFuncset, inset = onedimInset, conset = numericConset, steps = 10000, printfreq = 100); summary(popfitness(pop1, sinusfitness))
#system.time(pop1 <- simpleevo(pop1, sinusfitness, funcset = arithmeticFuncset, inset = onedimInset, conset = numericConset, steps = 10000, printfreq = 100)); summary(popfitness(pop1, sinusfitness))
#plotFunctions(list(sortBy(pop1, sinusfitness)[[1]], sin), -pi, pi, 1024)
#plot.new(); text(0.5, 0.5, indToPlotmathExpr(sortBy(pop1, sinusfitness)[[1]]))

## evolution of a simple damped oscillator (e.g. a pendulum)
#pop2 <- makePopulation(500, c(arithmeticFuncset, trigonometricFuncset), onedimInset, numericConset)
#pop2 <- simpleevo(pop2, do1fitness, funcset = c(arithmeticFuncset, trigonometricFuncset), inset = onedimInset, conset = numericConset, steps = 100000, printfreq = 100); summary(popfitness(pop2, do1fitness))
#plotFunctions(list(sortBy(pop2, do1fitness)[[1]], do1), 0, 10, 1024)
#plot.new(); text(0.5, 0.5, indToPlotmathExpr(sortBy(pop2, do1fitness)[[1]]))

## evolution of the sinus function in the interval [-pi,pi] with typed GP using arithmetic and logic operators...
#pop3 <- makeTypedPopulation(500, st("numeric"), c(typedArithmeticFuncset, typedLogicalFuncset), typedOnedimInset, typedNumericLogicalConset)
#pop3 <- simpleevoTyped(pop3, sinusfitness, funcset = c(typedArithmeticFuncset, typedLogicalFuncset), inset = typedOnedimInset, conset = typedNumericLogicalConset, steps = 5000, printfreq = 100); summary(popfitness(pop3, sinusfitness))
#plotFunctions(list(sortBy(pop3, sinusfitness)[[1]], sin), -pi, pi, 1024)

## evolution of a fourier series approximation of a square wave function
#pop4 <- makePopulation(500, c(arithmeticFuncset, functionSet("sin")), onedimInset, numericConset)
#pop4 <- simpleevo(pop4, squarewavefitness, funcset = c(arithmeticFuncset, functionSet("sin")), inset = onedimInset, conset = numericConset, steps = 10000, printfreq = 100); summary(popfitness(pop4, squarewavefitness))
#plotFunctions(list(sortBy(pop4, squarewavefitness)[[1]], squarewave), 0, 3, 1024)

## a simple test of  symbolic regression using genetic programming
#df1 <- data.frame(list(y = 1:100, x1 = 1:100, x2 = 101:200, x3 = 201:300, x4 = 301:400)); df1$y <- df1$x1 * df1$x2 + df1$x3
#rff1 <- makeRegressionFitnessFunction(y ~ x1 + x2 + x3 + x4, df1)
#pop5 <- symbolicRegression(y ~ x1 + x2 + x3 + x4, df1, stopCondition = makeTimeStopCondition(120), functionSet = arithmeticFunctionSet); summary(popfitness(pop5, rff1))
#sortBy(pop5, rff1)[[1]]
