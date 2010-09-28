require(rgp)

## specify experiment parameters
evaluationsPerRun <- 100000
numberOfRuns <- 10

## define test functions and test data
salustowicz1d <- function(x) exp(-x)*x^3*sin(x)*cos(x)*(sin(x)*sin(x)*cos(x)-1)
unwrappedBall1d <- function(x) 10 / ((x - 3)^2 + 5)
dampedOscillator1d <- function(x) 1.5 * exp(-0.5 * x) * sin(pi * x + pi)

dfSalustowicz1d <- data.frame(x=seq(from=0, to=12, length.out=512), y=salustowicz1d(seq(from=0, to=12, length.out=512)))
dfUnwrappedBall1d <- data.frame(x=seq(from=-2, to=8, length.out=512), y=unwrappedBall1d(seq(from=-2, to=8, length.out=512)))
dfDampedOscillator1d <- data.frame(x=seq(from=1, to=4*pi, length.out=512), y=dampedOscillator1d(seq(from=1, to=4*pi, length.out=512)))

## define cluster worker functions
makeSgpWorker <- function(data) {
  function(i)
    symbolicRegression(y ~ x, data, stopCondition = makeEvaluationsStopCondition(evaluationsPerRun), restartCondition = makeFitnessDistributionRestartCondition())
}

## initialize the compute cluster
sfInit(cpus = 2, parallel = TRUE)
sfLibrary(rgp)
sfExport("evaluationsPerRun")
sfExport("dfSalustowicz1d")
sfExport("dfUnwrappedBall1d")
sfExport("dfDampedOscillator1d")

## create baseline results with symbolic regression by standard GP
print("starting baseline runs...")
sgpResultsSalustowicz1d <- sfClusterApplyLB(1:numberOfRuns, makeSgpWorker(dfSalustowicz1d))
print("1/3 done")
sgpResultsUnwrappedBall1d <- sfClusterApplyLB(1:numberOfRuns, makeSgpWorker(dfUnwrappedBall1d))
print("2/3 done")
sgpResultsDampedOscillator1d <- sfClusterApplyLB(1:numberOfRuns, makeSgpWorker(dfDampedOscillator1d))
print("DONE.")

#srr1 <- multiNicheSymbolicRegression(y~x, df2, stopCondition=makeTimeStopCondition(10*60), passStopCondition=makeTimeStopCondition(30), individualSizeLimit=64, restartCondition=makeFitnessStagnationRestartCondition(), numberOfNiches=4)

## stop the compute cluster
sfStop()

#pdf(width=7.5, height=2.5, file="~/Desktop/sr_test_functions.pdf"); par(mfrow=c(1,3), mar=c(3.1,2.1,2.1,1.1)); plot(DampedOscillator1D, type="l", main="1: Damped Oscillator 1D", xlab="", ylab=""); plot(UnwrappedBall1D, type="l", main="2: Unwrapped Ball 1D", xlab="", ylab=""); plot(Salustowicz1D, type="l", main="3: Salustowicz 1D", xlab="", ylab=""); dev.off()
