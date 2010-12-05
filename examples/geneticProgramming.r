# geneticProgramming.r
# based on the first tutorial found at http://rsymbolic.org/wiki/rgp/Getting_Started
# 2010 Oliver Flasch
#

library(rgp)


functionSet1 <- functionSet("+", "*", "-", "/", "exp", "log")
inputVariableSet1 <- inputVariableSet("x")
constantFactorySet1 <- constantFactorySet(function() rnorm(1))

interval1 <- seq(from=2*-pi, to=2*pi, by=0.1)
fitnessFunction1 <- function(f) rmse(Vectorize(f)(interval1), sin(interval1))

gpResult1 <- geneticProgramming(functionSet=functionSet1, inputVariables=inputVariableSet1, constantSet=constantFactorySet1, fitnessFunction=fitnessFunction1, stopCondition=makeTimeStopCondition(5*60))

best1 <- gpResult1$population[[which.min(sapply(gpResult1$population, fitnessFunction1))]]
plot(sin(interval1), type="l", col="red"); lines(best1(interval1), col="black")
